## PART 0: Listing conditions for a dataset to be run in the for loop - SHOULD BE ADJUSTED WHEN INCORPORATING NEW DATASETS 
## PART 1: Reading quarterly raw datasets
## PART 2: Addressing data idiosyncracies
## PART 3: Creating "ID" column showing row ids
## PART 4: Assigning uniformly formatted quarterly raw datasets to new dataframes

# Set filepath to 'RawData/NYPDFareEvasionEnforcement' directory
filepath <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1WBQrmAFhEPn7U4_xO-72Zpk8UE-sZo02/Fare Evasion/NYPDFareEvasionProject/RawData/NYPDFareEvasionEnforcement/"

# Create for loop reading and uniformly (and roughly) reformatting raw quarterly arrests and summonses files
# Several changes in data structure have been reflected using if-else statements

for (y in years){
  for (q in quarters){
    for (e in enforcement_type){
      
      ##----------PART 0: Listing conditions for a dataset to be run in the for loop
      # THIS PART SHOULD BE ADJUSTED WHEN INCORPORATING NEW DATASET
      # The conditions below is to reflect several missing quarterly files
      # Both arrests and summonses datasets for 2021 Q4 are yet to be released
      # So is the arrests dataset for 2021 Q3, and the summonses dataset for 2020 Q4
      
      # The code is saying: for these quarters and enforcement types, do not run anything
      # (because there is no dataset)
      # for other quarters and enforcement types, run codes below
      if (y == 21 & q == 4|
          y == 21 & q == 3 & e == "arrests"|
          y == 20 & q == 4 & e == "summonses"){
      } else {
        
        ##----------PART 1: Reading quarterly raw datasets
        quarterly_raw <- 
          read_excel(
            # Set filepath
            paste0(
              filepath, 
              str_to_title(e),
              # Different combinations of years and quarters
              "/fare-evasion-", e, "-q", q, "-20", y, ".xlsx"),
            # Select sheet of our interest, and skip six rows
            # Table starts from 7th row
            sheet = "Stations-Race", skip = 6)
        
        ##----------PART 2: Addressing data idiosyncracies
        # Reflect changes in sheet format since 19q4
        # Previously, two columns - 'Borough', and 'Station and Line' - showed station information
        # Now just one column - 'Borough/Station/Line' - shows that information
        if (y > 19|| y == 19 & q == 4){
          quarterly_raw <- quarterly_raw %>%
            rename(BSL = `Borough/Station/Line`) %>% 
            mutate(Borough = "")
          # Reflect changes in column names since s20q1 within summonses
          if(y >= 20 & e == "summonses" || y == 19 & q == 4 & e == "summonses"){
            quarterly_raw <- quarterly_raw %>%
              rename(`American Indian` = `AMERICAN INDIAN/ALASKAN NATIVE`,
                     `Asian/Pac Isl` = `ASIAN / PACIFIC ISLANDER`,
                     Black = BLACK,
                     Hispanic = HISPANIC,
                     Unknown = UNKNOWN,
                     White = WHITE)
          }
        }
        # Before changes in 19q4
        else {
        quarterly_raw <- quarterly_raw %>%
            rename(BSL = `Station and Line`)
        }
        
        ##----------PART 3: Creating "ID" column showing row ids
        # This column is useful throughout the data cleaning process
        quarterly_raw <- tibble::rowid_to_column(quarterly_raw, "ID")
        
        ##----------PART 4: Assigning uniformly formatted quarterly raw datasets to new dataframes
        # i.e. a19q3 - "a"rrests dataset from 2019 Q3
        # i.e. s18q1 - "s"ummonses dataset from 2018 Q1
        assign(paste0(str_sub(e, 1, 1), y, "q", q), quarterly_raw)
      }
    }
  }
}

##----------PART 5: Saving all uniformly formatted quarterly raw datasets as an RData object
save(list = ls(pattern = "[0-9]{2}q[1-4]$"),
  file = paste0(wd, "CleanedData/01.RData"))

# THINGS TO CONSIDER: 
## (1) better column name than BSL?
## (2) maybe use janitor::clean_names() function here?