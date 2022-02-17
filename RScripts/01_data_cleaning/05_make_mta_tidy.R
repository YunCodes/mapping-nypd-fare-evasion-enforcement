# PART 1: Loading MTA Ridership data
# PART 2: Creating a crosswalk table
# PART 3: Changing unit of observations to mta_names and summarise

load(paste0(wd, "CleanedData/04.RData"))

##----------PART 1: Loading MTA Ridership data
mta <- read_excel("/Volumes/GoogleDrive/.shortcut-targets-by-id/1WBQrmAFhEPn7U4_xO-72Zpk8UE-sZo02/Fare Evasion/NYPDFareEvasionProject/mapping-nypd-fare-evasion-enforcement/RawData/MTARidership/2020 Subway Tables_UL.xlsx",
                  sheet = "Annual Total", skip = 1) %>%
  rename(mta_station = `Station (alphabetical by borough)`,
         mta_borough = Boro,
         `2019-2020 ChangePct` = `...11`)

##----------PART 2: Creating a crosswalk table
# Create station column using sprintf function, which manipulates character strings
# in two columns, creating a new column with two strings stringed into one
# and change first letter to uppercase in each word
all_arrests_and_summonses$nypd_station <- str_to_title(
  sprintf(
    "%s (%s)",
    all_arrests_and_summonses$final_Street_Names, 
    all_arrests_and_summonses$final_All_Lines)
)

# Create new dataframe with only distinct nypd names for checking
distinct_nypd_names <- all_arrests_and_summonses %>%
  ungroup() %>%
  dplyr::select(nypd_station)

# Create new dataframe with distinct mta names
distinct_mta_names <- mta %>% 
  dplyr::select(mta_borough, mta_station) %>%
  mutate(mta_borough_for_joining = 
           case_when(
             mta_borough == "Bx" ~ "Bronx",
             mta_borough == "B" ~ "Brooklyn",
             mta_borough == "M" ~ "Manhattan",
             mta_borough == "Q" ~ "Queens")) %>%
  filter(!is.na(mta_borough)) %>%
  distinct()

# In "distinct_nypd_names" dataframe,
# create a new column called "mta_station",
# which starts with the same value in the "nypd_station" column
# but gradually changes into the official mta name format

# Change the following:
# Street = St; 
# Avenue = Av (for some reason this only worked without the backslashes);
# Ave = Av;
# Remove all periods;
distinct_nypd_names$mta_station <- distinct_nypd_names$nypd_station %>% 
  str_replace_all(c('\\Street' = 'St', 
                    'Avenue' = 'Av',
                    'Ave' = 'Av',
                    '\\.' = '',
                    '9th' = '9',
                    '7th' = '7',
                    '86th' = '86',
                    '96th' = '96',
                    'Pk' = 'Park',
                    'Parkwy' = 'Pkwy',
                    'Parkway' = 'Pkwy',
                    'Road' = 'Rd'))

# Individually fix names 

#####------ Bronx ------#####
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station %in% c("149 St-Grand Concourse (4)", "149 St-Grand Concourse (2,5)")] <- 
  "149 St-Grand Concourse (2,4,5)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station %in% c("161 St-Yankee Stadium (B,D)", "161 St-Yankee Stadium (4)")] <- 
  "161 St-Yankee Stadium (B,D,4)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "East 177 St-Parkchester (6)"] <- "Parkchester (6)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "205 St-Norwood (D)"] <- "Norwood-205 St (D)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "238 St-Nereid Av (2,5)"] <- "Nereid Av (2,5)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "241 St-Wakefield (2)"] <- "Wakefield-241 St (2)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Bedford Park Blvd-Lehman Colle (4)"] <- "Bedford Park Blvd-Lehman College (4)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "East 174 St (2,5)"] <- "174 St (2,5)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Dyre Av-Eastchester (5)"] <- "Eastchester-Dyre Av (5)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Fordham Rd/Grand Concource (B,D)"] <- "Fordham Rd (B,D)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Soundview Av (6)"] <- "Morrison Av-Soundview (6)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c("East Tremont Av-West Farms S (2,5)",
                                                                        "East Tremont Av-West Farms Sq (2,5)")] <- "West Farms Sq-East Tremont Av (2,5)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "East 174 St (2,5)"] <- "174 St (2,5)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "East Tremont Av-Westchester Sq (6)"] <- "Westchester Sq-East Tremont Av (6)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "East 143 St-St Marys St (6)"] <- "East 143 St-St Mary's St (6)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "East Tremont Av-Westchester S (6)"] <- "Westchester Sq-East Tremont Av (6)"


#####------ Brooklyn ------#####
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c("9 St (R)", 
                                                                        "4 Av (F,G)",
                                                                        "9 St (M,N,R)")] <- "4 Av (F,G)/9 St (R)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c("Atlantic Av (2,3,4,5)",
                                                                        "Atlantic Av (B,Q)",
                                                                        "Pacific St (D,N,R)",
                                                                        "Pacific St (B,M,N,R)")] <- "Atlantic Av-Barclays Ctr (B,D,N,Q,R,2,3,4,5)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c("Dekalb Av (B,Q,R)")] <- "DeKalb Av (B,Q,R)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "36 St (B,M,N,R)"] <- "36 St (D,N,R)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c("4 Av-9 St (F,G)",
                                                                        "9 St (R)")] <- "4 Av (F,G)/9 St (R)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "45 St (N,R)"] <- "45 St (R)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "71 St (B,M)"] <- "71 St (D)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "95 St-Bay Ridge (R)"] <- "Bay Ridge-95 St (R)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == 'Av "H" (Q)'] <- "Avenue H (Q)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == 'Av "I" (F)'] <- "Avenue I (F)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == 'Av "J" (Q)'] <- "Avenue J (Q)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == 'Av "M" (Q)'] <- "Avenue M (Q)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == 'Av "N" (F)'] <- "Avenue N (F)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == 'Av "P" (F)'] <- "Avenue P (F)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == 'Av "U" (F)'] <- "Avenue U (F)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == 'Av "U" (N)'] <- "Avenue U (N)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == 'Av "U" (Q)'] <- "Avenue U (Q)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == 'Av "X" (F)'] <- "Avenue X (F)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == 'Brighton Beach (D,Q)'] <- "Brighton Beach (B,Q)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c('Broadway-East New York (A,C)',
                                                                        'Broadway-Eastern Pkwy (J,L,Z)')] <- "Broadway Junction (A,C,J,L,Z)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Rockaway Pkwy-Canarsie (L)"] <- "Canarsie-Rockaway Pkwy (L)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Clinton-Washington Avs (A,C)"] <- "Clinton-Washington Avs (C)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c('Stillwell Av-Coney Island (B,D,F,N)',
                                                                        'Stillwell Av-Coney Island (D,F,N,Q)')] <- "Coney Island-Stillwell Av (D,F,N,Q)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Cortelyou Rd (D)"] <- "Cortelyou Rd (Q)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c("Borough Hall (2,3,4,5)",
                                                                        "Court St (R)")] <- "Court St (R)/Borough Hall (2,3,4,5)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Utica Av-Crown Heights (3,4)"] <- "Crown Heights-Utica Av (3,4)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c("Franklin Av (2,3,4,5)",
                                                                        "Botanic Garden (S)")] <- "Franklin Av (2,3,4,5)/Botanic Garden (S)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c("Franklin Av (C)",
                                                                        "Franklin Av (S)")] <- "Franklin Av (C,S)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Grant Av (A,C)"] <- "Grant Av (A)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Hoyt-Schermerhorn (A,C,G)"] <- "Hoyt-Schermerhorn Sts (A,C,G)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Jay St-Borough Hall (A,C,F)"] <- "Jay St-MetroTech (A,C,F,R)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Kosciusko St (J)"] <- "Kosciuszko St (J)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c("Lorimer St (L)",
                                                                        "Metropolitan Av (G)")] <- "Lorimer St (L)/Metropolitan Av (G)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Myrtle/Wyckoff Avs (L)"] <- "Myrtle-Wyckoff Avs (L,M)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c("62 St (D)",
                                                                        "New Utrecht Av (N)")] <- "New Utrecht Av (N)/62 St (D)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Newkirk Av (B,Q)"] <- "Newkirk Plaza (B,Q)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Park Place (S)"] <- "Park Pl (S)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Sheepshead Bay (D,Q)"] <- "Sheepshead Bay (B,Q)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Union St (M,N,R)"] <- "Union St (R)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "West 8 St-Ny Aquarium (Q,F)"] <- "West 8 St-New York Aquarium (F,Q)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c("Myrtle/Wyckoff Avs (L)",
                                                                        "Wyckoff Av (M)")] <- "Myrtle-Wyckoff Avs (L,M)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c("Kings Highway (B,Q)",
                                                                        "Kings Highway (D,Q)")] <- "Kings Hwy (B,Q)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Kings Highway (F)"] <- "Kings Hwy (F)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Kings Highway (N)"] <- "Kings Hwy (N)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Lawrence St (R)"] <- "Jay St-MetroTech (A,C,F,R)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Dekalb Av (L)"] <- "DeKalb Av (L)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Bay Pkwy (B,M)"] <- "Bay Pkwy (N)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Metropolitan Av (M)"] <- "Middle Village-Metropolitan Av (M)"

#####------ Manhattan ------#####
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "110 St-Cathedral Pkwy (1)"] <- "Cathedral Pkwy-110 St (1)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "110 St-Cathedral Pkwy (B,C)"] <- "Cathedral Pkwy-110 St (B,C)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "110 St-Central Park North (2,3)"] <- "Central Park North-110 St (2,3)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "116 St (A,B,C)"] <- "116 St (B,C)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c("14 St (A,C,E)",
                                                                        "8 Av (L)")] <- "14 St (A,C,E)/8 Av (L)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c("14 St (1,2,3)",
                                                                        "14 St (F,M)",
                                                                        "6 Av (L)")] <- "14 St (F,M,1,2,3)/6 Av (L)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c("14 St-Union Square (L)",
                                                                        "14 St (4,5,6)",
                                                                        "Union Square (N,Q,R)",
                                                                        "Union Square (N,R)")] <- "14 St-Union Sq (L,N,Q,R,W,4,5,6)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "148 St-Harlem (3)"] <- "Harlem-148 St (3)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c("168 St-Washington Hts (1)",
                                                                        "168 St-Washington Hts (A,C)")] <- "168 St (A,C,1)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "225 St-Marble Hill (1)"] <- "Marble Hill-225 St (1)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "23 St (N,R)"] <- "23 St (R,W)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "242 St-Van Cortlandt Park (1)"] <- "Van Cortlandt Park-242 St (1)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "28 St (N,R)"] <- "28 St (R,W)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c("34 St-Herald Sq (B,D,F)",
                                                                        "34 St (M,N,Q,R)")] <- "34 St-Herald Sq (B,D,F,M,N,Q,R,W)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c("34 St (7)",
                                                                        "West 34 St/Hudson Yards (7)")] <- "34 St-Hudson Yards (7)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "23 St (N,R)"] <- "23 St (R,W)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c("42 St (B,D,F,M)",
                                                                        "5 Av (7)")] <- "42 St-Bryant Pk (B,D,F,M)/5 Av (7)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "47-50 Sts/Rockefeller Ctr (B,D,F,M)"] <- "47-50 Sts-Rockefeller Center (B,D,F,M)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c("42 St-Port Authority Bus Term (A,C,E)",
                                                                        "42 St-Port Authority Bus Terminal (A,C,E)",
                                                                        "42 St-Times Square (1,2,3)",
                                                                        "42 St-Times Square (7)",
                                                                        "42 St-Times Square (N,Q,R)",
                                                                        "42 St-Times Square (S)")] <- "Times Sq-42 St (N,Q,R,W,S,1,2,3,7)/42 St (A,C,E)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c("42 St-Grand Central (4,5,6)",
                                                                        "42 St-Grand Central (7)",
                                                                        "42 St-Grand Central (S)")] <- "Grand Central-42 St (S,4,5,6,7)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "49 St (N,Q,R)"] <- "49 St (N,R,W)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "5 Av (E,M)"] <- "5 Av-53 St (E,M)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "5 Av (N,Q,R)"] <- "5 Av-59 St (N,R,W)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c("51 St (6)",
                                                                        "Lexington Av (E,M)")] <- "Lexington Av-53 St (E,M)/51 St (6)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c("57 St (N,Q,R)",
                                                                        "57 St (N,R,S)")] <- "57 St-7 Av (N,Q,R,W)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c("59 St-Columbus Circle (1)",
                                                                        "59 St-Columbus Circle (A,B,C,D)")] <- "59 St-Columbus Circle (A,B,C,D,1)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c("59 St (4,5,6)",
                                                                        "Lexington Av (N,Q,R)")] <- "Lexington Av (N,R,W)/59 St (4,5,6)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "72 St (1,2,3,9))"] <- "72 St (1,2,3)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "72nd St (Q)"] <- "72 St (Q)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "8 St-Nyu (N,R)"] <- "8 St-New York University (R,W)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c("Bleecker St (6)",
                                                                        "Broadway/Lafayette (B,D,F,M)",
                                                                        "Bleecker St (J,Z)",
                                                                        "Broadway/Lafayette (B,D,F,Q)")] <- "Broadway-Lafayette St (B,D,F,M)/Bleecker St (6)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Broadway/Lafayette (B,D,F,M)"] <- "49 St (N,R,W)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "49 St (N,Q,R)"] <- "49 St (N,R,W)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "72 St (1,2,3,9)"] <- "72 St (1,2,3)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "96 St (J,Z)"] <- "Bowery (J,Z)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c("Broadway/Nassau (A,C)",
                                                                        "Fulton St (2,3)",
                                                                        "Fulton St (4,5)",
                                                                        "Fulton St (J,Z)")] <- "Fulton St (A,C,J,Z,2,3,4,5)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c("Brooklyn Bridge-City Hall (4,5,6)",
                                                                        "Chambers St (J,Z)")] <- "Brooklyn Bridge-City Hall (4,5,6)/Chambers St (J,Z)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c("Canal St (6)",
                                                                        "Canal St (J,Z)",
                                                                        "Canal St (N,Q,R)")] <- "Brooklyn Bridge-City Hall (4,5,6)/Chambers St (J,Z)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c("Chambers St-World Trade Cente (A,C,E)",
                                                                        "Chambers St-World Trade Center (A,C,E)",
                                                                        "Park Place (2,3)",
                                                                        "Cortlandt St (R)")] <- "Chambers St (A,C)/WTC (E)/Park Pl (2,3)/Cortlandt (R,W)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Cortlandt St (1)"] <- "WTC Cortland (1)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "City Hall (R)"] <- "City Hall (R,W)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c("Delancey St (F)",
                                                                        "Essex St (J,M,Z)")] <- "Delancey St (F)/Essex St (J,M,Z)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "200 St-Dyckman St (1)"] <- "Dyckman St (1)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "200 St-Dyckman St (A)"] <- "Dyckman St (A)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "207 St-Inwood (A)"] <- "Inwood-207 St (A)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Lexington Av (F)"] <- "Lexington Av-63 St (F,Q)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Prince St (N,R)"] <- "Prince St (R,W)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c("South Ferry (1)",
                                                                        "Whitehall St-South Ferry (R)")] <- "South Ferry (1)/Whitehall St (R,W)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c("W 4 St (A,B,C,D,E,F,M)", "W 4 St (A,B,C,D)")] <- "West 4 St-Washington Sq (A,B,C,D,E,F,M)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "81 St-Museum Of Natural History (B,C)"] <- "81 St-Museum of Natural History (B,C)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "79 St (1,9)"] <- "79 St (1)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Grand St (B,D,Q)"] <- "Grand St (B,D)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Rector St (R)"] <- "Rector St (R,W)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "WTC Cortland (1)"] <- "WTC Cortlandt (1)"


#####------ Queens ------#####
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "111 St-Roosevelt Av (7)"] <- "111 St (7)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "30 Av (N,Q)"] <- "30 Av (N,W)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c("23 St-Ely Av (E,M)",
                                                                        "Whitehall St-South Ferry (R)")] <- "Court Sq (E,G,M,7)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "33 St (7)"] <- "33 St-Rawson St (7)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "36 Av (N,Q)"] <- "36 Av (N,W)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "39 Av (N,Q)"] <- "39 Av-Dutch Kills (N,W)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "40 St (7)"] <- "40 St-Lowery St (7)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c("45 Rd-Court House Square (7)",
                                                                        "Court Sq (E,G,M,7)",
                                                                        "Court Square (G)")] <- "Court Sq (E,G,M,7)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "46 St (7)"] <- "46 St-Bliss St (7)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "61 St-Woodside (7)"] <- "Woodside-61 St (7)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c("71 Av-Forest Hills (E,F,M,R)",
                                                                        "74 St-Broadway (7)",
                                                                        "Roosevelt Av-Jackson Heights (E,F,G,R)",
                                                                        "Roosevelt Av-Jackson Heights (E,F,M,R)")] <- "74-Broadway (7)/Jackson Hts-Roosevelt Av (E,F,M,R)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "75 St-Elderts Lane (J,Z)"] <- "75 St-Elderts Ln (J,Z)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Aqueduct-Racetrack (A)"] <- "Aqueduct Racetrack (A)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Astoria Blvd (N,Q)"] <- "Astoria Blvd (N,W)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Ditmars Blvd-Astoria (N,Q)"] <- "Astoria-Ditmars Blvd (N,W)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Beach 67 St (A)"] <- "Beach 67 St-Arverne By The Sea (A)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Broadway (N,Q)"] <- "Broadway (N,W)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Van Wyck Blvd-Briarwood (E,F)"] <- "Briarwood-Van Wyck Blvd (E,F)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Main St-Flushing (7)"] <- "Flushing-Main St (7)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Howard Beach-Jfk Airport (A)"] <- "Howard Beach-JFK Airport (A)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Union Turnpike-Kew Gardens (E,F)"] <- "Kew Gardens-Union Turnpike (E,F)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "179 St-Jamaica (F)"] <- "Jamaica-179 St (F)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c("Parsons/Archer-Jamaica Center (E,J,Z)",
                                                                        "102 St (J,Z)")] <- "Jamaica Center-Parsons-Archer (E,J,Z)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Lefferts Blvd (A)"] <- "Ozone Park-Lefferts Blvd (A)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Queens Plaza (E,F,G,R)"] <- "Queens Plaza (E,M,R)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  %in% c("Queensboro Plaza (N,7)",
                                                                        "Queensboro Plaza (N,Q,7)")] <- "Queensboro Plaza (N,W,7)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Sutphin Blvd-Archer Av (E,J,Z)"] <- "Sutphin Blvd-Archer Av-JFK Airport (E,J,Z)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Willets Point-Shea Stadium (7)"] <- "Mets-Willets Point (7)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "82 St-Jackson Heights (7)"] <- "82 St-Jackson Hts (7)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "63 Drive-Rego Park (M,R)"] <- "63 Dr-Rego Park (M,R)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Grand Av-Newton (M,R)"] <- "Grand Av-Newtown (M,R)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station  == "Woodhaven Blvd (G,R)"] <- "Woodhaven Blvd (M,R)"

# Fix district offices that couldn't be fixed earlier because multiple stations were nearby
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station == "District 1 Office ()"] <- "59 St-Columbus Circle (A,B,C,D,1)"
# "District 2 Office" had multiple options to choose from mta names
# but ultimately chose "Canal St (A,C,E)" after checking the official district office website and location 
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station == "District 2 Office ()"] <- "Canal St (A,C,E)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station == "District 4 Office ()"] <- "14 St-Union Sq (L,N,Q,R,W,4,5,6)"
distinct_nypd_names$mta_station[distinct_nypd_names$mta_station == "District 11 Office ()"] <- "161 St-Yankee Stadium (B,D,4)"

# Join "distinct_nypd_names" with "distinct_mta_names"
# based on the "mta_station" column
crosswalk <- distinct_nypd_names %>% 
  left_join(distinct_mta_names, 
            by = "mta_station") %>% 
  distinct()

write_csv(crosswalk,
          paste0(
            wd,
            "CleanedData/",
            "05_crosswalk.csv"
          )
)

##----------PART 3: Changing unit of observations to mta_names and summarise
mta_tidy <- all_arrests_and_summonses %>% 
  left_join(crosswalk, by = "nypd_station") %>%
  group_by(Year, Quarter, mta_station) %>%
  # summarize by mta_station
  summarise(
    # arrests columns
    `American Indian_arrests` = sum(`American Indian_arrests`, na.rm = TRUE),
    Black_arrests = sum(Black_arrests, na.rm = TRUE),
    Hispanic_arrests = sum(Hispanic_arrests, na.rm = TRUE),
    Unknown_arrests = sum(Unknown_arrests, na.rm = TRUE),
    White_arrests = sum(White_arrests, na.rm = TRUE),
    `Grand Total_arrests` = sum(`Grand Total_arrests`, na.rm = TRUE),
    # summonses columns
    `American Indian_summonses` = sum(`American Indian_summonses`, na.rm = TRUE),
    Black_summonses = sum(Black_summonses, na.rm = TRUE),
    Hispanic_summonses = sum(Hispanic_summonses, na.rm = TRUE),
    Unknown_summonses = sum(Unknown_summonses, na.rm = TRUE),
    White_summonses = sum(White_summonses, na.rm = TRUE),
    `Grand Total_summonses` = sum(`Grand Total_summonses`, na.rm = TRUE))

write_csv(mta_tidy,
          paste0(
            wd,
            "CleanedData/",
            "05_mta_tidy.csv"
            )
          )

save.image(paste0(wd, "CleanedData/05.RData"))