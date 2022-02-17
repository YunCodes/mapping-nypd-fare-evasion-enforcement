## PART 0: Getting Uniformly Formatted Quarterly Raw Datasets
## PART 1: Creating Boroughs
## PART 2: Generating Variables for Lines 1-4
## PART 3: Creating Column "All_Lines"
## PART 4: Creating Column "Street Name"
## PART 5: Assigning Tidy Quarterly Datasets as New Dataframes 

for (y in years){
  for (q in quarters){
    
    # The code is to reflect missing data for 2020 Q4, 2021 Q4
    if (y >= 20 & q == 4){
    } else {
    
    ## PART 0: Getting Uniformly Formatted Quarterly Raw Datasets
    data <- mget(ls(pattern = paste0("^s", y, "q", q, "$")))[[1]]
    
    ## PART 1: Creating Boroughs
    # Create unique identifier for each borough, renaming to lowercase
    
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
    
    # Fill in 'Borough' column
    data <- data %>%
      fill(Borough)
    
    ## PART 2: Generating Variables for Lines 1-4
    # Create a list x, separating all lines denoted by a comma at every given station
    x <- strsplit(data$BSL, split = ",")
    
    # Get the max length of lines at any station in the data
    max_line_length <- max(sapply(x, length))
    
    # getting the max length of lines at a given station
    ## This step in particular is difficult for me to explain the functionality of the code.
    for (i in 1:length(x))
      length(x[[i]]) <- max_line_length
    
    # Use rbind to create new vector, assigning new variables for the split text strings
    z <- do.call(rbind, x)
    
    # remove NA Values
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
    
    ### Below four lines of codes are Summonses only
    data_2$BSL[data_2$BSL == "A,B,C,D,E,F,M"] <- NA
    
    data_2 <- data_2 %>% 
      fill(BSL)
    #data_2[data_2 == ""] <- NA
    
    ## PART 3: Creating Column "All_Lines"
    
    # Create column labels from 'Line1' to 'Line7'
    # from a18q1 to a19q1, data is up to Line7
    cols <- paste0("Line", 1:max_line_length)
    
    # This operation only worked with NA values, so swap back to NA prior to creating new column
    # this fills in blank Line columns with NAs
    #data_2[data_2 == ""] <- NA
    
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
        # rename(data_2, new_col_name = new)
      }
    }
    
    # Change NA values back to ""
    #data_2[is.na(data_2)] <- ""
    
    ## PART 4: Creating Column "Street Name"
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
  
    #X#X#X HAROLD: this is for the "DISTRICT 4 OFFICE" which is in Union Sq station
    # seems better to address directly and avoid searching for "N/A"
    #Q: exclude or assign to Union Sq station?
    
    
    # One observation with BSL == "N/A". Set Line1 to N/A so it isn't processed incorrectly in further commands.
    #data_3 <- within(data_3, Line1[BSL == "N/A"] <- "N/A")
    
    data_4 <- data_3 %>%
      filter(!is.na(Line1)) %>%
      select(-BSL)
    
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
    
    data_final <- data_3 %>% 
      filter(!is.na(Line1)) %>% 
      select(-BSL) %>%
      mutate(Quarter = q,
             Year = paste0("20", y))
    
    
#### 여기부터 필요한지 잘 모르겠음. 
    ### Below four lines of codes are Summonses only
    #data_3_off <- data_3 %>% 
      #filter(BSL == "OFF-SYSTEM") %>% 
      #distinct(Borough, .keep_all = TRUE) %>% 
      #select(-BSL)
    
    # remove all rows where Line1 has no value. 
    ## this identifies and removes observations of BSL that are no longer necessary, as 
    ## we have already generated new variables for street names and Lines. BSL can then be removed
    #data_4 <- data_3 %>% 
      #subset(Line1 != "") %>% 
      #select(-BSL)
    
    ### Below one lines of codes are Summonses only
    #data_4 <- rbind(data_4, data_3_off)
    
    #X#X#X HAROLD: to handle entries for arrests in each borough w/UNKNOWN station
    
    # Identify all observations where Street Name == UNKNOWN
    #unknowns_data <- data_2 %>% 
      #filter(BSL == "UNKNOWN") %>% 
      #rename(Street_Names = BSL)
    
    # Remove duplicate observations
    #unknowns_data <- unknowns_data[!duplicated(unknowns_data$Borough), ]
    
    # re-bind unknown observations to our most cleaned version, A1184
    ## Unknowns were lost in a previous step when filtering as they have
    ## no associated MTA lines. This ensures these observations aren't lost in our final analysis,
    ## although it is likely they will not be used (unless calculating the sum of all arrests/summonses).
    #data_unknown <- rbind(data_4, unknowns_data)
    
    # Add line for quarter
    ## This changes by quarter
    #data_final <- data_unknown %>% 
      #mutate(Quarter = q,
             #Year = paste0("20", y))
    
    ## PART 5: Assigning Tidy Quarterly Datasets as New Dataframes
    assign(paste0("s", y, "q", q, "_final"), data_final)
    
    }
  }
}

##----------PART 7: Saving all tidy quarterly datasets as an RData object
save(list = ls(pattern = "s[0-9]{2}q[1-4]_final$"),
     file = paste0(wd, "CleanedData/02-2.RData"))