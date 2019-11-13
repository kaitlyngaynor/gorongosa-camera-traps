# Gorongosa Camera Trap Analysis

### 1. Data cleaning scripts

**01-timeshift**: Change date and time metadata associated with image files; to do before generating record table in camtrapR
* Need a spreadsheet with the time corrections for all camera folders that must be edited

**02-create-record-table-year1** and **02a-create-record-table-year2**: Generate record table from raw photos that have been sorted into folders by species (used in many of the scripts below); generate species-specific detection matrices for later use in Occupancy_matrix_wrangling.Rmd

**03-remove-duplicate-records**: written by Lynn, gets record table into format with one row per burst (to account for differences in sorting method between seasons)
* Input and output are both record tables

**04-sunrisesunset-season.R**: assigns time of day and season to each record, based on date and time
* Script written for elephant activity pattern paper
* Input
    * Record table from camtrapR
    * Information on daily sunrise and sunset time, which I downloaded from a NOAA (?) spreadsheet - sunrise_sunset.csv
* Compares the time to sunrise/sunset on that date to assign day or night
    * Uses mutate function rather than a for loop, hmm! Could be worth bringing into future code
* Output
    * Record table with columns for time period and season (ex. Record_table_all_1hr_timeperiods.csv)

**05-recordtable-all-to-15min.R** and **05a-recordtable-all-to-10min.R**: Cleans master record table, eliminating records that are outside of camera operation and reducing to “independent” records 
* Input
    * Complete record table with all detections (generated by camtrapR, columns for elapsed time) (Gaynor_ALLrecords_cleaned_June16_to_June17.csv)
    * Camera metadata with columns with dates for Start, End, Problem1_From, Problem1_To (optional) (cam_metadata_all.csv)
* Output
    * Record table that only includes detections separated by a given interval (here, I did 15 min, but could be easily changed), during period of camera operation (Gaynor_15min_records_June2016_to_June2017.csv)
    * Can be brought into RAI_calculation.R to calculate RAI
    
**06-make-master-occupancy-matrix.R**: Create a master occupancy matrix for later analysis, using record tables and camtrapR functions.
    
**07-gorongosa-gis.RMd**: Compile and clean spatial vector and raster layers, and extract values at camera locations.
* Input
    * A number of GIS layers
    * Spreadsheet with camera latitude and longitude (Camera_coordinates.csv)
* Crop, change resolution/extent, convert vector to raster, create raster stack
* Calculate distance to features
* Calculate neighborhood raster values
* Output
    * Spreadsheet with extracted raster values at camera locations (cam_metadata_102518.csv)

**08-occupancy_matrix_wrangling.Rmd**: Generate summarized occupancy records for different time periods in the format necessary for analysis using Lindsey’s code
* Input
    * Master file with a row for each camera-species, and column for each date, with 0, 1, and NA for non-detection, detection, and inoperability. I believe I created this file by compiling individual species matrices from camtrapR but don’t have the code (All_species_by_date_062316_090417.csv)
* Subsets by dates of interest
* Sum “1” columns to calculate total number of sampling periods with detections, spread data so there is one column per species and one row per camera
* Count “0” and “1” columns to calculate total number of nights that each camera was operational
* Output
    * Detection counts for each species at each camera (ex. Detections_7_1_16_9_30_16.csv)
    * Camera operation for each camera (ex. Camoperation_7_1_16_9_30_16.csv)

**08a-occupancy-matrix-wrangling-monthly-loop.R**: Does the same as above, but allows the creation of multiple matrices for multiple periods using a for loop

SIF_DataGrab.R: reformats detection and operation matrices for pairwise species interaction factor modeling (from Lindsey)
* Input
    * Detection and operation matrices (as generated by Occupancy_matrix_wrangling.Rmd)
* Output
    * Spreadsheet of species pair co-occurrence data ready for SIF analysis using CoOcc_Code.R (ex. Dry.peak.SppPairs.csv)


### 2. Data exploration scripts

**01-covariate-correlations.R**: calculates correlations among variables
* Input
    * Camera metadata file (cam_metadata_master.csv)
* Determines Pearson’s r coefficients and p-values for relationships between each of covariates
* Worth revisiting!
* Output - none (but could export the correlation matrix)

**02-normalization-comparison.R**: Was curious to see how similar the metadata values were at each camera site if the covariate rasters were standardized before extraction, versus standardized among the 60 values at the camera sites Lindsey suggested they should be similar, or the camera locations don't accurately represent the study area.

**03-aerial-count-exploration.R**: Basic exploration of aerial count data, to determine percentage/biomass of ungulates on floodplain. (Doesn't actually integrate camera trap data; that's another project)


### 3. RAI data analysis scripts

**01-rai-calculation.R**: Calculates RAI for each species at each camera from the record table for a given time period.
* Input
    * Cleaned record table (generated by Recordtable_all_to_15min.R) with only independent records (Gaynor_15min_records_June2016_to_June2017.csv)
    * Camera operation dates for time period of interest, created by Occupancy_matrix_wrangling.Rmd (ex. Camoperation_7_1_16_9_30_16.csv) - column that has the number of trap-nights for each camera in that season
* Removes species that are not to be included in analysis (incl. Ghost)
* Subsets record table to time period of interest (after you input start and end dates)
* Calculates number of records of each species at each camera
* Calculates RAI based on number of records and operation date
* Currently written for dry, wet3mo, wet4mo - will need to edit if 
* Output
    * Table for each season, with columns for species, camera, operation, count, and RAI (ex. RAI_peakdry.csv)

**01a-rai-calculation-10min.R**: Does the same thing as above, but with 10 min interval rather than 15

**01b-rai-calculation-monthly-for-loop.R**: Does the same thing as above, but with a loop to allow for calculation of RAI every month

**02-rai-analysis.Rmd**: Variety of analyses related to RAI, in progress
* Input
    * RAI data files from RAI_calculation.R (ex. RAI_peakdry.csv)
* Variety of analyses on RAI
* In progress...

### 4. Occupancy data analysis scripts

**01-occupancy-models.Rmd**: Lindsey’s occupancy models
* Input
    * Detection histories for each species at each camera, plus camera operation days for each camera (from Occupancy_matrix_wrangling.Rmd)
    * Camera locations (2018camloc.csv) - tbh not sure what this ends up being used for?
    * Species information (2018spp.csv) - for grouping species for analysis
    * Camera metadata (cam_metadata_master.csv) - for pulling out occupancy and detection covariates
* Runs multi-species hierarchical occupancy models with specified covariates for occupancy and detection
* Output - various files with occupancy, detection means and CIs for different groups/species, modeled species richness, etc.

**01a-occupancy-models-aerial-period.Rmd**: Run occupancy models for the 2016 aerial count. For Integrative Zoology / Animal Conservation manuscript

**01b-occupancy-models-combinedyearseason.Rmd**: Run occupancy models for both years and seasons in a single model, where year and season are included as predictors (technically not an appropriate approach)

**02-occupancy-covariate-comparison.R**: for exploring the effects of individual covariates on overall occupancy (Lindsey’s suggested method for Bayesian "model selection")
* Input
    * D and G spreadsheets (intermediate outputs from Occupancy.R
* Essentially does what Occupancy.R does, but with just one detection covariate, and only exports community-level hyperparameters
* Output
    * Community-level hyperparameter - coefficient corresponding to effect of the predictor variable on occupancy (mean, SD, upper and lower CIs)
 
**02-occupancy-covariate-comparison-v2.R**: for exploring the effects of individual covariates on overall occupancy (Lindsey’s suggested method for Bayesian "model selection")
* Input
    * Individual spreadsheets for the community coefficient model output (outputs from Occupancy_covariate_comparison.R

**03-occupancy-results-exploration.Rmd**: Exploring the results of the occupancy models


### 5. Temporal data analysis scripts

**01-temporal_overlap.R**: Calculates pairwise temporal (24-hr) overlap between all pairs of species, using overlap package.
* Input - record table
* Output - long data matrix with pairwise overlap values for all 703 species pairs, with columns for overlap in dry, wet3mo, and wet4mo seasons

### 6. Species Interaction Factor analysis scripts

**01-format-data-for-sif.R**: Gets data into correct format

**02-run-sif-model.R**: Runs species co-occurrence models. Code from Lindsey Rich.
* Input
    * Species pair detection information - from SIF_DataGrab.R (ex. Dry.peak.SppPairs.csv)
* Output
    * SIF results, with columns for psiA (probability A occurred in study area), psiBA (probability of B given A), psiBa (probability of B given A), psiB (derived quantity from the other values), pA (A detection probability), pB (B detection probability) - ex. SIF_2018Dry.csv
    
**03-explore-sif-results.Rmd**: Analysis of SIF results, in conjunction with temporal overlap, RAI overlap, dietary overlap, etc.
