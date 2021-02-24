library(tidyverse)

# data re-created in Excel based on downloaded sheets from supplementary material
data <- read.csv("data/data-to-recreate-stalmansfig3.csv")

head(data)

ggplot(data, aes(x = Period, y = Mean, group=1)) +
  geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0) +
  stat_summary(fun.y=sum, geom="line", col = "darkred", size = .75) +
  geom_point(stat='summary', fun.y=sum, size = 3) +
  facet_wrap(~ Species, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.text.x = element_text(size = 12),
        axis.title=element_text(size=14)) +
  labs(y = "Relative Density")

ggsave("figures/stalmans-fig3-recreate.pdf", useDingbats = FALSE)  
