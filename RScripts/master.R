## Load packages 
library(tidyverse)
library(readxl)
library(stringr)
library(splitstackshape)
library(tidycensus)
library(lubridate)
library(sf)
library(raster)
library(rgdal)
library(gdalUtils)
library(janitor)
library(rvest)
library(lubridate)

## Create lists to put in for loops
# Numbers in 'years' represent the last two digits of years
years <- 18:21
quarters <- 1:4
enforcement_type <- c("arrests", "summonses")

##----------SECTION 1: Load all quarterly arrests and summonses files
wd <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1WBQrmAFhEPn7U4_xO-72Zpk8UE-sZo02/Fare Evasion/NYPDFareEvasionProject/mapping-nypd-fare-evasion-enforcement/"
# Set path to 'RScripts/01_data_cleaning/' directory
sd <- paste0(wd, "RScripts/01_data_cleaning/")

# Run script loading all quarterly datasets for arrests and summonses
source(paste0(sd, "01_load_raw_arrests_and_summonses_quarterly_data.R"))

# NOTE: THIS SCRIPT HAS A PART TO BE ADJUSTED WHEN INCORPORATING NEW DATASET 
# In the script, there are several conditions for a dataset to be loaded
# i.e.) a dataset should not come from 2021 Q4
# These conditions are created to load all quarterly datasets with a single loop 
# while reflecting missing quarterly data
# These conditions should be adjusted when incorporating new datasets
# For example, the example condition above should be removed when new datasets for 2021 Q4 get released

##----------SECTION 2: Get quarterly arrests and summonses data tidy
# Run scripts restructuring quarterly arrests and summonses data, respectively
source(paste0(sd, "02-1_get_quarterly_arrests_data_tidy.R"))
source(paste0(sd, "02-2_get_quarterly_summonses_data_tidy.R"))

# A large portion of the two scripts is redundant
# but I decided NOT TO COMBINE the respective for loops for arrests and summonses
# because if I do so, it will be hard to adjust the codes 
# when NYPD later make changes to the data format for either or both enforcement types

# NOTE: THIS SCRIPT HAS A PART TO BE ADJUSTED WHEN INCORPORATING NEW DATASET 
# In the scripts, there are several conditions for a dataset to be ran through the respective for loop
# These conditions should be adjusted when incorporting new datasets

##----------SECTION 3: Stack cleaned quarterly files
# Run script standardizing tidy quarterly datasets and stacking them by enforcement type
source(paste0(sd, "03_stack_tidy_quarterly_arrests_and_summonses_data_by_enforcement_type.R"))

##----------SECTION 4: Fix NYPD naming issues 
source(paste0(sd, "04_fix_nypd_naming_issues_and_join_stacked_arrests_and_summonses_datasets.R"))
# Make sure nypd_names are consistent across quarters and enforcement types

##----------SECTION 5: Make MTA tidy
source(paste0(sd, "05_make_mta_tidy.R"))
# Create a crosswalk table, and change the unit of analysis

# Running the code below shows if there is any nypd station unmatched with an mta station name
# If there is no such station, the output should be "Unknown ()" only
# If seeing station names, find why those station were unmatched, and
# fix the fourth and/or fifth script accordingly
setdiff(crosswalk$mta_station, mta$mta_station)

### DATA AND CODE VALIDATION FINISHED THUS FAR-------------------------
##----------SECTION 6: Statistical Analysis
#source(paste0(sd, "06_collect_data_for_mapping.R"))










##----------SECTION 6: Statistical Analysis
# Global statistics by quarter
mta_tidy %>%
  group_by(Year, Quarter) %>%
  summarise(`Grand Total_arrests` = sum(`Grand Total_arrests`),
            `Grand Total_summonses` = sum(`Grand Total_summonses`)) %>%
  gather(key="enforcement_type", value="total", 3:4) %>%
  mutate(year_quarter=paste0(Year, "-", Quarter)) %>%
  ggplot(aes(x= year_quarter, y = total, group=enforcement_type, color=enforcement_type)) +
  geom_line() +
  ggtitle("Total arrests and summonses by quarter")

# arrests by race
mta_tidy %>%
  group_by(Year, Quarter) %>%
  summarise(American_Indian = sum(`American Indian_arrests`),
            Black = sum(Black_arrests),
            Hispanic = sum(Hispanic_arrests),
            Unknown = sum(Unknown_arrests),
            White = sum(White_arrests)) %>%
  gather(key="race", value="total", 3:7) %>%
  mutate(year_quarter=paste0(Year, "-", Quarter)) %>%
  filter(year_quarter != "2021-3") %>%
  ggplot(aes(x=year_quarter, y = total, group=race, color=race)) +
  geom_line() +
  ggtitle("Total arrests by race")

# summonses by race
mta_tidy %>%
  group_by(Year, Quarter) %>%
  summarise(American_Indian = sum(`American Indian_summonses`),
            Black = sum(Black_summonses),
            Hispanic = sum(Hispanic_summonses),
            Unknown = sum(Unknown_summonses),
            White = sum(White_summonses)) %>%
  gather(key="race", value="total", 3:7) %>%
  mutate(year_quarter=paste0(Year, "-", Quarter)) %>%
  ggplot(aes(x=year_quarter, y = total, group=race, color=race)) +
  geom_line() +
  ggtitle("Total summonses by race")

# summonses by borough
mta_tidy %>%
  left_join(crosswalk, by = "mta_station") %>%
  group_by(Year, Quarter, mta_borough_for_joining) %>%
  filter(!is.na(mta_borough_for_joining)) %>%
  summarise(`Grand Total_summonses` = sum(`Grand Total_summonses`)) %>%
  mutate(year_quarter=paste0(Year, "-", Quarter)) %>%
  ggplot(aes(x=year_quarter, y = `Grand Total_summonses`, group=mta_borough_for_joining, color=mta_borough_for_joining)) +
  geom_line() +
  ggtitle("Total summonses by borough")


## Exploring MTA Developer Resources
# Scrape all download links on 'Fare Data' webpage
# Get all weekly datasets from 2018
faredata_webpage <- rvest::read_html("http://web.mta.info/developers/fare.html")

all_fare_download_links <- faredata_webpage %>%
  html_nodes("a") %>% # find all links
  html_attr("href") %>% # get the url
  str_subset("\\.csv$")

fare_download_links_from_18 <- all_fare_download_links %>%
  str_subset("_18|_19|_20|_21") %>% # find datasets between 2018-2021 only 
  sort()

faredata_filepath <- "http://web.mta.info/developers/"

for (n in 1:length(fare_download_links_from_18)){
  # Download individual data
  faredata <- read_csv(
    paste0(
      faredata_filepath,
      fare_download_links_from_18[n]),
    skip = 2) %>% 
    clean_names() %>%
    mutate(
      posted_date = 
        as.Date(
          str_extract(fare_download_links_from_18[n], "[0-9]+"),
          format = "%y%m%d"))
  
  assign(paste0("faredata_", n), faredata)
}

faredata_combined <- mget(ls(pattern = "faredata_[0-9]+")) %>%
  bind_rows() %>%
  mutate_at(c(3:29, 31:33), as.numeric) %>%
  mutate(total = rowSums(select_if(., is.numeric), na.rm = TRUE))

# 2 units of observations: 'station' > 'remote'
length(unique(faredata_combined$remote)) # 518
length(unique(faredata_1$remote)) # 507
length(unique(faredata_combined$station)) # 507
length(unique(faredata_1$station)) # 495

## Explore MTA Turnstile Data
# remote:
# booth:
# station: 
# line name:
# division: 
remote_booth_station <- read_excel("RawData/MTATurnstile/Remote-Booth-Station.xls") %>% 
  clean_names()

turnstiledata_webpage <- rvest::read_html("http://web.mta.info/developers/turnstile.html")

all_turnstile_download_links <- turnstiledata_webpage %>%
  html_nodes("a") %>% # find all links
  html_attr("href") %>% # get the url
  str_subset("\\.txt$")

turnstile_download_links_from_18 <- all_turnstile_download_links  %>%
  str_subset("_18|_19|_20|_21") %>% # find datasets between 2018-2021 only 
  sort()

turnstiledata_filepath <- "http://web.mta.info/developers/"

for (n in 1:length(turnstile_download_links_from_18)){
  # Download individual data
  tunstiledata <- read_csv(
    paste0(
      turnstiledata_filepath,
      turnstile_download_links_from_18[n]
    ) 
  ) %>% clean_names()
  assign(paste0("turnstiledata_", n), tunstiledata)
}

turnstiledata_combined <- mget(ls(pattern = "turnstiledata_[0-9]+")) %>%
  bind_rows()

# 4 units of observation: 'c_a', 'unit', 'scp', 'station'
glimpse(turnstiledata_200)
length(unique(turnstiledata_200$c_a)) # 750
length(unique(turnstiledata_200$unit)) # 468 == remote
length(unique(turnstiledata_200$scp)) # 222
length(unique(turnstiledata_200$station)) # 378

mta_tidy <- read_csv("/Volumes/GoogleDrive/.shortcut-targets-by-id/1WBQrmAFhEPn7U4_xO-72Zpk8UE-sZo02/Fare Evasion/NYPDFareEvasionProject/CleanedData/05_mta_tidy.csv")
glimpse(mta_tidy)
length(unique(mta_tidy$mta_station))

setdiff(unique(faredata_1$remote), unique(turnstiledata_2$unit))
### For 02/10 meeting

Remote_Booth_Station <- read_excel("/Volumes/GoogleDrive/.shortcut-targets-by-id/1WBQrmAFhEPn7U4_xO-72Zpk8UE-sZo02/Fare Evasion/NYPDFareEvasionProject/RawData/MTARidership/Turnstile/Remote-Booth-Station.xls") %>%
  clean_names()

# Play with turnstile data example
turnstiledata_example %>%
  mutate(observed_at = as.Date(paste0(date, " ", time), format = "%m/%d/%Y %H:%M:%S"))

stations <- read_csv("http://web.mta.info/developers/data/nyct/subway/Stations.csv") %>% 
  clean_names()

# 496 stations
# 445 complexs
stations %>%
  dplyr::select(station_id, complex_id, gtfs_stop_id) %>%
  group_by(complex_id) %>%
  summarise(station_count = n_distinct(station_id),
            gtfs_stop_count = n_distinct(gtfs_stop_id)) %>%
  arrange(desc(station_count, gtfs_stop_count)) #%>%
#filter(station_count > gtfs_stop_count)

stations %>%
  dplyr::select(station_id, complex_id, gtfs_stop_id, stop_name) %>%
  group_by(stop_name) %>%
  summarise(complex_count = n_distinct(complex_id),
            station_count = n_distinct(station_id),
            gtfs_stop_count = n_distinct(gtfs_stop_id)) %>%
  arrange(desc(station_count, gtfs_stop_count)) %>%
  filter(station_count < complex_count | gtfs_stop_count < station_count)

# 378 unique station
turnstiledata_example %>%
  dplyr::select(c_a, unit, scp, station) %>%
  group_by(station) %>%
  summarise(c_a_count = n_distinct(c_a),
            unit_count = n_distinct(unit),
            scp_count = n_distinct(scp)) %>%
  arrange(desc(c_a_count, unit_count, scp_count)) %>%
  filter(station_count < complex_count | gtfs_stop_count < station_count)

## 421 unique data
all_arrests_and_summonses <- read_csv("/Volumes/GoogleDrive/.shortcut-targets-by-id/1WBQrmAFhEPn7U4_xO-72Zpk8UE-sZo02/Fare Evasion/NYPDFareEvasionProject/CleanedData/04_all_arrests_and_summonses.csv")
all_arrests_and_summonses %>%
  dplyr::select(final_Street_Names, final_All_Lines) %>%
  distinct()

# Case Study: 14th St
## NYPD enforcement
all_arrests_and_summonses %>%
  dplyr::select(final_Street_Names, final_All_Lines) %>%
  filter(str_detect(final_Street_Names, "14 ST")) %>%
  distinct() # 5

## MTA fare
faredata_combined %>%
  dplyr::select(station) %>%
  filter(str_detect(station, "14TH ST")) %>%
  distinct() # 7

## 
faredata_combined %>%
  dplyr::select(station) %>%
  filter(str_detect(station, "14TH ST")) %>%
  distinct() 

length(unique(turnstiledata_200$station))