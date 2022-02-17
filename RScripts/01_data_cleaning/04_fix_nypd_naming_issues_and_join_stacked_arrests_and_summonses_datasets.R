## PART 1: Checking and fixing any inconsistencies/potential issues in the stacked 'arrests' dataset
## PART 2: Checking and fixing any inconsistencies/potential issues in the stacked 'summonses' dataset
## PART 2-1: Assigning matching station for 6 out of 10 unique Distict Offices
## PART 2-2: Fixing other stations named inconsistently
## PART 3: Fixing any inconsistencies/potential issues across enforcement types
## PART 3-1: Fixing streets having overlapping lines 
## PART 4: Joining cleaned stacked arrests and summonses datasets

load(paste0(wd, "CleanedData/03.RData"))

##----------PART 1: Checking and fixing any inconsistencies/potential issues in the stacked 'arrests' dataset

# Some stations are incorrectly assigned to multiple boroughs
# The code below shows which 'Station_Names'-'All_Lines' pairs were assigned to multiple boroughs
# If station has been consistently assigned to a certain borough, the first value in the last column should show '1'
# If not, the first value should show a value bigger than '1'
all_arrests %>% 
  select(Borough, Street_Names, All_Lines) %>%
  distinct() %>% 
  arrange(desc(Street_Names, All_Lines)) %>% 
  group_by(Street_Names, All_Lines) %>%
  tally() %>%
  arrange(desc(n))

##----------PART 2: Checking and fixing any inconsistencies/potential issues in the stacked 'summonses' dataset
# The code below shows which 'Station_Names'-'All_Lines' pairs were assigned to multiple boroughs
# If station has been consistently assigned to a certain borough, the first value in the last column should show '1'
# If not, the first value should show a value bigger than '1'
all_summonses %>% 
  select(Borough, Street_Names, All_Lines) %>%
  distinct() %>% 
  arrange(desc(Borough, Street_Names, All_Lines)) %>%
  group_by(Street_Names, All_Lines) %>%
  tally() %>%
  arrange(desc(n))

##----------PART 2-1: Assigning matching station for 6 out of 10 unique Distict Offices
# Remaining four stations that couldn't be matched based on NYPD datasets
# will be fixed later when changing the unit of observations from nypd_names to mta_names

# Each District Office station has two fixing lines: 
# one fixing the value in 'Street_Names'; the other fixing the value'All_Lines'

### QUEENS - 2 out of 2 fixed
all_summonses$Street_Names[all_summonses$Street_Names == "DISTRICT 20 OFFICE"] <- "VAN WYCK BLVD.-BRIARWOOD"
all_summonses$All_Lines[all_summonses$Street_Names == "VAN WYCK BLVD.-BRIARWOOD"] <- "E,F"
all_summonses$Street_Names[all_summonses$Street_Names == "DISTRICT 23 OFFICE"] <- "ROCKAWAY PARK-BEACH 116 ST."
all_summonses$All_Lines[all_summonses$Street_Names == "ROCKAWAY PARK-BEACH 116 ST."] <- "A,S"

### MANHATTAN - 1 out of 4 fixed
all_summonses$Street_Names[all_summonses$Street_Names == "DISTRICT 3 OFFICE"] <- "145 STREET"
all_summonses$All_Lines[all_summonses$Street_Names == "145 STREET"] <- "A,B,C,D"

### BROOKLYN - 2 out of 2 fixed
all_summonses$Street_Names[all_summonses$Street_Names == "DISTRICT 30 OFFICE"] <- "HOYT-SCHERMERHORN"
all_summonses$All_Lines[all_summonses$Street_Names == "HOYT-SCHERMERHORN"] <- "A,C,G"
all_summonses$Street_Names[all_summonses$Street_Names == "DISTRICT 33 OFFICE"] <- "BROADWAY-EASTERN PKWY"
all_summonses$All_Lines[all_summonses$Street_Names == "BROADWAY-EASTERN PKWY"] <- "J,L,Z"

### BRONX - 1 out of 2 fixed
all_summonses$Street_Names[all_summonses$Street_Names == "DISTRICT 12 OFFICE"] <- "EAST 180 STREET"
all_summonses$All_Lines[all_summonses$Street_Names == "EAST 180 STREET"] <- "2,5"

##----------PART 2-2: Fixing other stations named inconsistently
all_summonses$final_Street_Names[all_summonses$Street_Names == "42 ST.-PORT AUTHORITY BUS TERM"] <- "42 ST.-PORT AUTHORITY BUS TERMINAL"

##----------PART 3: Fixing any inconsistencies/potential issues across enforcement types
# Select three columns from stacked arrests and summonses datasets
# Remove duplicates using 'distinct()' function
all_arrests_cleaned_1 <- all_arrests %>% 
  select(Borough, Street_Names, All_Lines) %>%
  distinct()

all_summonses_cleaned_1 <- all_summonses %>% 
  select(Borough, Street_Names, All_Lines) %>%
  distinct()

# Rbind unique stations from each stacked dataset
all_unique_nypd_stations <- rbind(all_arrests_cleaned_1, all_summonses_cleaned_1) %>%
  select(Borough, Street_Names, All_Lines) %>%
  distinct()

# The code below shows which 'Station_Names'-'All_Lines' pairs were assigned to multiple boroughs
# But also eyeball the list to find if stations have overlapping lines
# i.e.) In s20q3, "116 STREET" has “A,B,C” line, while it has “B,C” line in other quarters
all_unique_nypd_stations %>%
  group_by(Street_Names, All_Lines) %>%
  tally() %>% 
  arrange(desc(n, Street_Names, All_Lines)) 

# Change the borough of HALSEY STREET – L line to "Brooklyn"
all_unique_nypd_stations$final_Borough[all_unique_nypd_stations$Street_Names == "HALSEY STREET"] <- "Brooklyn"
# Fix inconsistencies in "111 ST.-ROOSEVELT AVE" naming
all_unique_nypd_stations$final_Street_Names[all_unique_nypd_stations$Street_Names == "111 st-Roosevelt Ave"] <- "111 ST.-ROOSEVELT AVE"

##----------PART 3-1: Fixing streets having overlapping lines 
# Create new 'final_All_Lines' column
all_unique_nypd_stations$final_All_Lines[all_unique_nypd_stations$Street_Names == "116 STREET" &&
                                           all_unique_nypd_stations$All_Lines == "A,B,C"] <- "B,C"
all_unique_nypd_stations$final_All_Lines[all_unique_nypd_stations$Street_Names == "UNION SQUARE" &&
                                           all_unique_nypd_stations$All_Lines == "N,R"] <- "N,Q,R"
all_unique_nypd_stations$final_All_Lines[all_unique_nypd_stations$Street_Names == "36 STREET" &&
                                           all_unique_nypd_stations$Borough == "Brooklyn"] <- "D,N,R"
all_unique_nypd_stations$final_All_Lines[all_unique_nypd_stations$Street_Names == "36 STREET" &&
                                           all_unique_nypd_stations$Borough == "Queens"] <- "M,R"

# 
all_unique_nypd_stations$final_Borough[all_unique_nypd_stations$Street_Names %in% c("MYRTLE-WILLOUGHBY AVENUES", "MYRTLE/WYCKOFF AVENUES")] <- "Brooklyn"


# Fill empty 'final_Borough', 'final_Street_Names', 'final_All_Lines' column cells
# if the "final_X" column value is empty, use the value from regular "X" column
all_unique_nypd_station_final <- all_unique_nypd_stations %>%
  mutate(final_Borough = if_else(is.na(final_Borough), Borough, final_Borough),
         final_Street_Names = if_else(is.na(final_Street_Names), Street_Names, final_Street_Names),
         final_All_Lines = if_else(is.na(final_All_Lines), All_Lines, final_All_Lines)) %>%
  mutate(final_Street_Names = if_else(final_Street_Names == "OFF-SYSTEM", "UNKNOWN", final_Street_Names)) 

# Apply above changes to both stacked arrests and summonses datasets using 'all_unique_nypd_stations_final'
all_arrests_cleaned_final <- all_arrests %>%
  select(Borough, Street_Names, All_Lines, Year, Quarter, 
         `American Indian`, `Asian/Pac Isl`, Black, Hispanic, Unknown, White, `Grand Total`) %>%
  left_join(all_unique_nypd_station_final, by = c("Borough", "Street_Names", "All_Lines")) %>%
  select(-Borough, -Street_Names, -All_Lines) %>% 
  ungroup() %>%
  group_by(Year, Quarter, final_Borough, final_Street_Names, final_All_Lines) %>%
  summarise(`American Indian` = sum(`American Indian`, na.rm = TRUE),
            Black = sum(Black, na.rm = TRUE),
            Hispanic = sum(Hispanic, na.rm = TRUE),
            Unknown = sum(Unknown, na.rm = TRUE),
            White = sum(White, na.rm = TRUE),
            `Grand Total` = sum(`Grand Total`, na.rm = TRUE))

all_summonses_cleaned_final <- all_summonses %>%
  select(Borough, Street_Names, All_Lines, Year, Quarter, 
         `American Indian`, `Asian/Pac Isl`, Black, Hispanic, Unknown, White, `Grand Total`) %>%
  left_join(all_unique_nypd_station_final, by = c("Borough", "Street_Names", "All_Lines")) %>%
  select(-Borough, -Street_Names, -All_Lines) %>%
  ungroup() %>% 
  group_by(Year, Quarter, final_Borough, final_Street_Names, final_All_Lines) %>%
  summarise(`American Indian` = sum(`American Indian`, na.rm = TRUE),
            Black = sum(Black, na.rm = TRUE),
            Hispanic = sum(Hispanic, na.rm = TRUE),
            Unknown = sum(Unknown, na.rm = TRUE),
            White = sum(White, na.rm = TRUE),
            `Grand Total` = sum(`Grand Total`, na.rm = TRUE))

##----------PART 4: Joining cleaned stacked arrests and summonses datasets
all_arrests_and_summonses <- all_arrests_cleaned_final %>%
  full_join(all_summonses_cleaned_final, 
            by = c("final_Borough", "final_Street_Names", "final_All_Lines",
                   "Year", "Quarter")) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))

# Fix column names 
colnames(all_arrests_and_summonses) <- str_replace(
  str_replace(
    colnames(all_arrests_and_summonses), 
    pattern = ".x", replacement = "_arrests"),
  pattern = ".y", replacement = "_summonses")

# Check if there is any station whose borough is assigned inconsistently
all_arrests_and_summonses %>%
  ungroup() %>%
  select(final_Borough, final_Street_Names, final_All_Lines) %>%
  distinct() %>%
  group_by(final_Street_Names, final_All_Lines) %>%
  tally() %>%
  arrange(desc(n))

write_csv(all_arrests_and_summonses,
          paste0(
            wd, 
            "CleanedData/", # filepath
            "04_all_arrests_and_summonses.csv"
            )
          )

save.image(paste0(wd, "CleanedData/04.RData"))