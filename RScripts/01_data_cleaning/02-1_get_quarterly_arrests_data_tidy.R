## PART 0: Listing conditions for a dataset to be run in the for loop - SHOULD BE ADJUSTED WHEN INCORPORATING NEW DATASETS 
## PART 1: Getting uniformly formatted quarterly raw datasets
## PART 2: Filling in "Borough" column
## PART 3: Generating variables for Lines 1-4
## PART 4: Creating column "All_Lines"
## PART 5: Creating column "Street Name"
## PART 6: Assigning tidy quarterly datasets to new dataframes

load(paste0(wd, "CleanedData/01.RData"))

for (y in years){
  for (q in quarters){
    
    ##----------PART 0: Listing conditions for a dataset to be run in the for loop
    # THIS PART SHOULD BE ADJUSTED WHEN INCORPORATING NEW DATASETS
    # This code is to reflect missing arrests datasets for 2021 Q3-4
    # It says: for a21q3, do not run anything
    if (y == 21 & q >= 3){
    } else {
      
      ##----------PART 1: Getting uniformly formatted quarterly raw datasets
      # Retrieve a dataset in the environment, whose name starts with "a" followed by year info, and ends with quarter info
      # i.e.) a18q1 for the arrests dataset for 2018 Q1
      # 'mget(ls())' function yields a 'list' of one dataset that meets the pattern condition
      # '[[1]]' at the end selects the first object in the list, which is the dataset of our interest
      data <- mget(ls(pattern = paste0("^a", y, "q", q, "$")))[[1]] 
      
      ##----------PART 2: Filling in "Borough" column
      
      # Create functions that identify 'rows' with each borough name in 'any' column in data
      # '1' indicates rows (as opposed to '2' indicating columns)
      # Selecting just one column to detect each string pattern is less safe when working with multiple datasets
      # because the borough name may appear in different columns in different datasets
      
      Bronx <- apply(data, 1, function(x) any(x %in% "BRONX")) # gives "TRUE" or "FALSE"
      #Bronx_alternative <- apply(data, 1, function(x) any(x == "BRONX")) # gives "TRUE", "FALSE", or NA
      # Theoretically, 'Bronx' and ‘Bronx_alternative’ should do the same job
      # telling if a row has the borough information. 
      # However, the codes produce different results. 
      # 'Bronx' produces either “TRUE” or “FALSE”
      # while 'Bronx_alternative' produces NA values as well when a value in any column is NA. 
      # This difference does not affect following codes in any way 
      # but I chose 'Bronx' because working with strings with NA values does not seem desirable. 
      Brooklyn <- apply(data, 1, function(x) any(x %in% "BROOKLYN"))
      Manhattan <- apply(data, 1, function(x) any(x %in% "MANHATTAN"))
      Queens <- apply(data, 1, function(x) any(x %in% "QUEENS"))
      
      # For the rows with borough names, assign borough names
      data$Borough[Bronx] <- "Bronx"
      data$Borough[Brooklyn] <- "Brooklyn"
      data$Borough[Manhattan] <- "Manhattan"
      data$Borough[Queens] <- "Queens"
      data$Borough[data$Borough == ""] <- NA
      
      # Is there any more intuitive code to perform the same task? 
      #data_2 <- data %>%
        #mutate(Borough = case_when(as.character(any(. %in% "BRONX")) ~ "Bronx",
                                   #any(. %in% "BROOKLYN") ~ "Brooklyn",
                                   #any(. %in% "MANHATTAN") ~ "Manhattan",
                                   #any(. %in% "QUEENS") ~ "Queens",
                                   #TRUE ~ NA))
      
      # Fill in 'Borough' column
      data <- data %>%
        fill(Borough)
    
      ##----------PART 3: Generating variables for Lines 1-4
      # Create a list x, separating all lines denoted by a comma at every given station
      # Stations with multiple lines will have multiple elements 
      # i.e.) "B,D" -> "B", "D"
      x <- strsplit(data$BSL, split = ",")
      
      # Get the max length of lines at any station in the data
      max_line_length <- max(sapply(x, length))
      
      # Change the length for lines for all stations to the max_line_length
      # Empty elements will be filled with 'NA' values
      # This is to later rbind lines info for all stations
      # Rbind works only when the subjects have the same data structure
      for (i in 1:length(x))
        length(x[[i]]) <- max_line_length
      
      # Rbind to create a new matrix that shows just one line in each column
      z <- do.call(rbind, x)
      
      # remove NA Values
      ### DO WE NEED THIS??
      #z[is.na(z)] <- ""
      # create column names = Lines
      colnames(z) <- paste0("Line", 1:ncol(z))
      # Reset z as a dataframe, fix Line1 = strings with character length of 1
      z = as.data.frame(z)
      z$Line1 <- ifelse(nchar(z$Line1, type = "chars") == 1, 
                        z$Line1, 
                        NA)
    
      # Merge newly generated variable with data
      data_2 <- merge(data, z, by=0) %>%  # by=0 same as by="row.names"
        select(-`Row.names`) %>% 
        arrange(ID)
      
    
      ##----------PART 4: Creating column "All_Lines"
      # Create column labels from 'Line1' to 'LineX'
      cols <- paste0("Line", 1:max_line_length)
      
      # Create a new variable "All_Lines"
      # collapsing individual lines into a single string separated by comma
      data_2$All_Lines <- apply(data_2[, cols], # select individual lines columns only
                                1, # row-wise function (as opposed to '2' - colum-wise function)
                                function(x) paste(x[!is.na(x)], # collapse non-NA values 
                                                  collapse = ",")) # and separate them by comma
      
      # Fix the issue of having different numbers of lines across datasets
      # Create empty columns from "Line7" to "Line1"  
      # Break when 'n' reaches the largest line number in the dataset
      for (n in 7:1) {
        if (paste0("Line", n) %in% cols){
          break
        } else {
          # Create new column
          data_2$new <- NA
          # Rename new column
          new_col_index <- which(colnames(data_2) == "new")
          colnames <- colnames(data_2)
          colnames[new_col_index] <- paste0("Line", n)
          colnames(data_2) <- colnames
        }
      }
      
      # Change NA values back to ""
      ### DO WE NEED THIS??
      # data_2[is.na(data_2)] <- ""
      
      ##----------PART 5: Creating column "Street Name"
      # Create new dataframe called "Streets" by filtering by string length
      Streets <- data_2 %>%
        select(ID, BSL) %>%
        filter(str_length(BSL) > 1) %>% # filter out line info with only one line
        filter(!str_detect(substr(BSL, 1, 2), ",")) %>% # filter out line info with multiple line
        rename(Street_Names = BSL) %>%
        filter(!Street_Names %in% "Grand Total")
    
    # Add new row and fill the same way we filled Borough
    data_3 <- full_join(data_2, Streets, by = "ID") %>% 
      fill(Street_Names)
    
    ## Part 4: Prep final version of dataframe for binding
    # One observation with BSL == "N/A". Set Line1 to N/A so it isn't processed incorrectly in further commands.
    # DO WE NEED THIS?????
    #data_3 <- within(data_3, Line1[BSL == "N/A"] <- "N/A")
    
    # remove all rows where Line1 has no value. 
    # this identifies and removes observations of BSL that are no longer necessary, as 
    # we have already generated new variables for street names and Lines. BSL can then be removed
    data_4 <- data_3 %>%
      filter(!is.na(Line1)) %>%
      select(-BSL) 
    
    # Identify all observations where Street Name == UNKNOWN 
    unknowns_data <- data_2 %>% 
      filter(BSL %in% c("UNKNOWN", "OFF-SYSTEM")) %>% 
      rename(Street_Names = BSL)
    
    # Remove duplicate observations
    unknowns_data <- unknowns_data[!duplicated(unknowns_data$Borough), ]
    
    # re-bind unknown observations to our most cleaned version
    ## Unknowns were lost in a previous step when filtering as they have
    ## no associated MTA lines. This ensures these observations aren't lost in our final analysis,
    ## although it is likely they will not be used (unless calculating the sum of all arrests/summonses).
    data_final <- rbind(data_4, unknowns_data) %>%
      mutate(Quarter = q,
             Year = paste0("20", y))
    
    ## PART 6: Assigning tidy quarterly datasets to new dataframes
    assign(paste0("a", y, "q", q, "_final"), data_final)
    }
  }
}

##----------PART 7: Saving all tidy quarterly datasets as an RData object
save(list = ls(pattern = "a[0-9]{2}q[1-4]_final$"),
     file = paste0(wd, "CleanedData/02-1.RData"))
