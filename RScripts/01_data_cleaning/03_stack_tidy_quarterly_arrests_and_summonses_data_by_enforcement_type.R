## PART 0: Listing conditions for a dataset to be run in the for loop - SHOULD BE ADJUSTED WHEN INCORPORATING NEW DATASETS 
## PART 1: Retrieving tidy quarterly raw dataset
## PART 2: Standardizing tidy quarterly data format
## PART 3: Assigning standardized tidy quarterly datasets to new dataframes 
## PART 4: Stacking standardized quarterly datasets by enforcement type

load(paste0(wd, "CleanedData/02-1.RData"))
load(paste0(wd, "CleanedData/02-2.RData"))

# Different quarterly files have different numbers of columns 
# For each cleaned quarterly file, select columns we need only

# Create a vector of columns we need
columns_of_interest <- c("Borough", "Street_Names", 
                         "All_Lines", "Line1", "Line2", "Line3", "Line4", "Line5", "Line6", "Line7",
                         "Year", "Quarter", 
                         "American Indian", "Asian/Pac Isl", "Black", "Hispanic", "Unknown",
                         "White", "Grand Total")

for (y in years){
  for (q in quarters){
    for (e in enforcement_type){
      
      ##----------PART 0: Listing conditions for a dataset to be run in the for loop
      # THIS PART SHOULD BE ADJUSTED WHEN INCORPORATING NEW DATASETS
      # This code is to reflect missing datasets
      # It says: if meeting these conditions, do not run codes below
      if (y == 21 & q == 4|
          y == 21 & q == 3 & e == "arrests"|
          y == 20 & q == 4 & e == "summonses"){
      } else {
      
      ##----------PART 1: Retrieving tidy quarterly raw dataset
      data_final <- mget(ls(pattern = paste0(str_sub(e, 1, 1), y, "q", q, "_final")))[[1]] 
      
      ##----------PART 2: Standardizing tidy quarterly data format
      # Find which columns of our interest are missing in the dataset
      # Order of the vectors in 'setdiff' fuction matters
      missing_columns <- setdiff(columns_of_interest, colnames(data_final))
      
      # There are only three cases missing columns:
      # (1) missing "Unknown" columns;
      # (2) missing Line5-7 columns; or
      # (3) missing both "Unknown" and Line5-7 columns
      
      # Therefore, create two if-else statements dealing with missing "Unknown" column
      
      # If without "Unknown" column, create one and fill with 0s
      if ("Unknown" %in% missing_columns){
        data_final$Unknown <- 0
      } else {}
      
      data_final_uniform <- data_final %>%
        select(columns_of_interest)
      
      ##----------PART 3: Assigning standardized tidy quarterly datasets to new dataframes 
      # i.e.) "a18q1_final_uniform" for the arrests dataset for 2018 Q1
      # i.e.) "s19q3_final_uniform" for the summonses dataset for 2019 Q3
      assign(paste0(str_sub(e, 1, 1), y, "q", q, "_final_uniform"), data_final_uniform)
  
      }
    }
  }
}

##----------PART 4: Stacking standardized quarterly datasets by enforcement type
# Stack all the datasets in the environment that meets the regex pattern condition
# starts with "a" or "s"; followed by two digits; followed by 'q'; followed by one quarter digit; ends with "_final_uniform"
all_arrests <- mget(ls(pattern="^a[0-9]{2}q[1-4]_final_uniform$")) %>%
  bind_rows()

write_csv(all_arrests,
          paste0(wd,
          "CleanedData/", # filepath
          "03_all_arrests.csv"))

all_summonses <- mget(ls(pattern="^s[0-9]{2}q[1-4]_final_uniform$")) %>%
  bind_rows()

write_csv(all_summonses,
          paste0(wd,
                 "CleanedData/", # filepath
                 "03_all_summonses.csv"))

save.image(file = paste0(wd, "CleanedData/03.RData"))
