###############################################################################
# Mesocarnivore detection rates per camera x year (July-November window)
#
# Inputs
#   Cameratrap_table.csv                           StudySite, species, datetime, count
#   Camera_operation_dates_2016-2023_Long_format.csv   Camera, Start, End, Notes
#
# Output
#   One row per camera x year x species with:
#     detections      number of detections (Jul 1 - Nov 30) while camera was operating
#     operation_days  number of days the camera operated within Jul 1 - Nov 30
#     detection_rate  detections / operation_days
###############################################################################

library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(purrr)
library(stringr)
library(ggplot2)

# ---- 0. Settings ------------------------------------------------------------
trap_file <- "2026-sandbox/data/cameratrap_table.csv"
ops_file  <- "2026-sandbox/data/Camera_operation_dates_2016-2023_Long_format.csv"
out_file  <- "2026-sandbox/mesocarnivore_detection_rates_JulNov.csv"

season_months   <- 7:11   # July-November (whole months, so Jul 1 - Nov 30)

# 0 = count every record as a detection.
# Set to e.g. 30 to collapse records of the same species at the same camera that
# are < 30 min apart into a single independent event.
independence_min <- 0

# TRUE = lump all mongoose species (and unidentified mongoose) into one "mongoose" group
pool_mongooses <- TRUE

# ---- 1. Species lookup (EDIT ME) --------------------------------------------
# Left: species label as it appears in the data after lower-casing and replacing
# spaces/hyphens with "_". Right: the standardised species name to report.
# Anything not listed here is NOT treated as a mesocarnivore and is dropped.
# Add/remove rows to change which species are included.
species_lookup <- tribble(
  ~raw,                              ~species,
  "genet",                           "genet",
  "civet",                           "civet",
  "civeet",                          "civet",                  # typo in data
  "honey_badger",                    "honey_badger",
  "honeybadger",                     "honey_badger",
  "serval",                          "serval",
  "caracal",                         "caracal",
  "jackal",                          "jackal",
  "marsh_mongoose",                  "marsh_mongoose",
  "mongoose_marsh",                  "marsh_mongoose",
  "marshmongoose",                   "marsh_mongoose",
  "banded_mongoose",                 "banded_mongoose",
  "mongoose_banded",                 "banded_mongoose",
  "bandedmongoose",                  "banded_mongoose",
  "slender_mongoose",                "slender_mongoose",
  "mongoose_slender",                "slender_mongoose",
  "slendermongoose",                 "slender_mongoose",
  "white_tailed_mongoose",           "white_tailed_mongoose",
  "mongoose_white_tailed",           "white_tailed_mongoose",
  "whitetailedmongoose",             "white_tailed_mongoose",
  "bushy_tailed_mongoose",           "bushy_tailed_mongoose",
  "mongoose_bushy_tailed",           "bushy_tailed_mongoose",
  "mongoose_dwarf",                  "dwarf_mongoose",
  # "Large grey" and "Egyptian" mongoose are the same species (Herpestes ichneumon)
  "egyptian_mongoose",               "egyptian_mongoose",
  "egyptianmongoose",                "egyptian_mongoose",
  "mongoose_large_grey",             "egyptian_mongoose",
  "largegreymongoose",               "egyptian_mongoose",
  "egyptian_or_large_grey_mongoose", "egyptian_mongoose",
  # Not identified to species
  "mongoose",                        "mongoose_unidentified"
)

if (pool_mongooses) {
  species_lookup <- species_lookup %>%
    mutate(species = if_else(str_detect(species, "mongoose"), "mongoose", species))
}

# ---- 2. Read data -----------------------------------------------------------
trap <- read_csv(trap_file, col_types = cols(.default = col_character()),
                 show_col_types = FALSE)
ops  <- read_csv(ops_file,  col_types = cols(.default = col_character()),
                 show_col_types = FALSE)

# Excel's row limit is 1,048,576 -- a CSV with exactly this many rows has
# almost certainly been cut off when it was exported.
if (nrow(trap) >= 1048575) {
  warning("Cameratrap_table.csv has ", nrow(trap), " rows, which is Excel's row ",
          "limit. The file is probably truncated; re-export the full table.")
}

# ---- 3. Camera operation days -----------------------------------------------
# Expand each Start-End interval into individual days (both ends inclusive),
# then keep distinct camera-days so overlapping / touching intervals
# (e.g. one deployment ending the day the next one starts) are not double counted.
ops_days <- ops %>%
  transmute(camera = str_trim(Camera),
            start  = ymd(Start),
            end    = ymd(End)) %>%
  filter(!is.na(start), !is.na(end), end >= start) %>%
  mutate(date = map2(start, end, ~ seq(.x, .y, by = "day"))) %>%
  unnest(date) %>%
  distinct(camera, date) %>%
  mutate(year = year(date), month = month(date)) %>%
  filter(month %in% season_months)

op_summary <- ops_days %>%
  count(camera, year, name = "operation_days")

# ---- 4. Mesocarnivore detections --------------------------------------------
# Datetimes are a mix of "YYYY-MM-DD H:MM" and date-only strings.
det <- trap %>%
  mutate(camera   = str_trim(StudySite),
         raw      = str_replace_all(str_to_lower(str_trim(species)), "[\\s-]+", "_"),
         datetime = parse_date_time(datetime, orders = c("Ymd HMS", "Ymd HM", "Ymd"),
                                    tz = "UTC", quiet = TRUE),
         date     = as_date(datetime),
         count    = suppressWarnings(as.integer(count))) %>%
  select(-species) %>%                                 # raw label; replaced by lookup
  inner_join(species_lookup, by = "raw") %>%          # keeps mesocarnivores only
  filter(!is.na(date))

# Optional: collapse records into independent events
if (independence_min > 0) {
  det <- det %>%
    arrange(camera, species, datetime) %>%
    group_by(camera, species) %>%
    filter(is.na(lag(datetime)) |
             as.numeric(difftime(datetime, lag(datetime), units = "mins")) > independence_min) %>%
    ungroup()
}

# Keep only detections made on a day the camera was actually operating
# (inside the Jul-Nov window). This also drops detections that fall outside the
# operation dates, e.g. pre-2016 records or set-up/maintenance visits.
n_before <- nrow(det)
det_in <- det %>%
  semi_join(ops_days, by = c("camera", "date"))
message(n_before - nrow(det_in), " of ", n_before,
        " mesocarnivore records were outside Jul-Nov operation days and were dropped.")

det_summary <- det_in %>%
  mutate(year = year(date)) %>%
  group_by(camera, year, species) %>%
  summarise(detections = sum(count, na.rm = TRUE), .groups = "drop")

# ---- 5. Combine, fill zeros, compute rate ------------------------------------
# Every camera-year that operated gets a row for every mesocarnivore species
# recorded in the dataset, with 0 detections where none occurred.
result <- op_summary %>%
  crossing(species = sort(unique(det$species))) %>%
  left_join(det_summary, by = c("camera", "year", "species")) %>%
  mutate(detections     = replace_na(detections, 0L),
         detection_rate = detections / operation_days) %>%
  arrange(species, camera, year) %>%
  select(camera, year, species, detections, operation_days, detection_rate)

write_csv(result, out_file)
print(result, n = 20)

# ---- 6. Optional quick summaries ---------------------------------------------
# Detections / operation days pooled across cameras, per species and year:
pooled <- result %>%
  group_by(species, year) %>%
  summarise(detections = sum(detections), .groups = "drop") %>%
  left_join(op_summary %>% group_by(year) %>%
              summarise(operation_days = sum(operation_days), .groups = "drop"),
            by = "year") %>%
  mutate(detection_rate = detections / operation_days)
print(pooled, n = 20)

# ---- 7. Plot: detection rate over time, one facet per species ----------------
plot_file       <- "mesocarnivore_detection_rates_JulNov.png"
min_total_det   <- 20     # only plot species with at least this many detections overall
min_effort_days <- 500    # years with fewer pooled operation days get an open symbol
show_cameras    <- FALSE  # TRUE = also draw each camera's rate as faint grey points

plot_species <- pooled %>%
  group_by(species) %>%
  summarise(total = sum(detections), .groups = "drop") %>%
  filter(total >= min_total_det) %>%
  pull(species)

pooled_plot <- pooled %>%
  filter(species %in% plot_species) |> 
  filter(year <= 2022) |> 
  filter(species != "serval")

p <- ggplot(pooled_plot, aes(x = year, y = detection_rate))

p <- p +
  geom_vline(xintercept = 2019, colour = "firebrick", linetype = "dashed", linewidth = 0.7) +
  geom_line(colour = "black", linewidth = 0.8) +
  geom_point(colour = "black", size = 2.4, stroke = 1) +
  facet_wrap(~ species, scales = "free_y",
             labeller = labeller(species = function(x) str_to_sentence(str_replace_all(x, "_", " ")))) +
  scale_x_continuous(breaks = sort(unique(pooled_plot$year))) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.08))) +
  labs(x = NULL, y = "Detection rate") +
  theme_classic(base_size = 11) +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill = "grey92"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank())

print(p)
ggsave(plot_file, p, width = 11, height = 7, dpi = 300)
