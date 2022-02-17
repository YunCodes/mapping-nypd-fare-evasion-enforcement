# Here are the list of datasets we need for mapping:
# (1) ACS census block-level demographic and socioeconomic data
# (2) 

## Import annual census block group-level ACS data (5-year estimates) using the 'tidycensus' package

# Set Census API key
census_api_key("e2a0be8ff20ee18fd0d7eeac46bf8eda7de4a2d6")

# https://api.census.gov/data/2019/acs/acs5/variables.html

# Load all 2019 ACS 5-year estimates tables' variables 
variables_table <- load_variables(2019, "acs5", cache = TRUE)

# Create a census block group-level spatial data,
# showing the demographic and socioeconomic characteristics
nyc_blockgroup_characteristics <- get_acs(geography = "block group",
                                          state = "NY",
                                          # List all NYC counties
                                          county = c("Bronx County", "Kings County",
                                                     "New York County", "Queens County",
                                                     "Richmond County"),
                                          # use 'variables_table' to find variables of our interest
                                          variables = c(
                                            # racial composition
                                            white = "B03002_003",
                                            black = "B03002_004",
                                            hispanic = "B03002_012",
                                            asian = "B03002_006",
                                            p_total = "B03002_001",
                                            # socioeconomic status
                                            pvty_50_below = "C17002_002",
                                            pvty_50_99 = "C17002_003",
                                            pvty_100_124 = "C17002_004",
                                            pvty_125_149 = "C17002_005",
                                            pvty_150_184 = "C17002_006",
                                            pvty_185_199 = "C17002_007",
                                            pvty_200_above = "C17002_008",
                                            pvty_total = "C17002_001"),
                                          survey = "acs5",
                                          year = 2019,
                                          geometry = TRUE)

### SUBWAY GEOSPATIAL DATA
## Load Subway Stations shp file
geospatial_data_filepath <- paste0(wd, "RawData/geospatial/")

subway_stations_geo <- st_read(
  paste0(
    geospatial_data_filepath,
    "Subway Stations/geo_export_66c42076-68dc-48c1-961a-f6e54d9cf0d3.shp")
) %>%
  clean_names() #%>%
#select(name, line, geometry)

## Load Subway Lines shp file
subway_lines_geo <- st_read(
  paste0(
    geospatial_data_filepath,
    "Subway Lines/geo_export_5a915196-2410-4ca1-82e9-5c67c360a415.shp")
) %>%
  clean_names() #%>%
  #select(name, line, geometry)

# Make sure the subway stations (points) and lines (lines) files have the same datum and projection 
sf::st_crs(subway_stations_geo) == st_crs(subway_lines_geo)
crs(subway_stations_geo)
crs(subway_lines_geo)

# Explore data from CUNY's NYC Mass Transit Spatial Layers Archive
cuny_subway_stations_geo <- st_read(
  paste0(
    geospatial_data_filepath,
    "cuny_stops_nyc_subway_may2019/stops_nyc_subway_may2019.shp"
  )
)

cuny_subway_lines_geo <- st_read(
  paste0(
    geospatial_data_filepath,
    "cuny_routes_nyc_subway_may2019/routes_nyc_subway_may2019.shp"
  )
)

# Make sure the subway stations (points) and lines (lines) files have the same datum and projection 
st_crs(cuny_subway_stations_geo) == st_crs(cuny_subway_lines_geo)
crs(cuny_subway_stations_geo)
crs(cuny_subway_lines_geo)

# Transform data Coordinate Reference System for mapping with Leaflet
subway_stations_geo_converted <- st_transform(subway_stations_geo, "+proj=longlat +ellps=WGS84")
subway_lines_geo_converted <- st_transform(subway_lines_geo, "+proj=longlat +ellps=WGS84")
cuny_subway_stations_geo_converted <- st_transform(cuny_subway_stations_geo, "+proj=longlat +ellps=WGS84")
cuny_subway_lines_geo_converted <- st_transform(cuny_subway_lines_geo, "+proj=longlat +ellps=WGS84")
st_crs(subway_stations_geo_converted) == st_crs(cuny_subway_stations_geo_converted)

# Compare NYC and CUNY 'stations' and 'lines' data
## NYC 'stations'
stations_pal <- colorFactor("Paired", domain = unique(subway_stations_geo$line))

subway_stations_geo_converted %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(stroke = FALSE, fillOpacity = 0.5,
                   color = ~stations_pal(line))

## CUNY 'stations'
cuny_stations_pal <- colorFactor("Paired", domain = unique(cuny_subway_stations_geo_converted$line))

cuny_subway_stations_geo_converted %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(stroke = FALSE, fillOpacity = 0.5,
                   color = ~cuny_stations_pal(line))

mapview(subway_stations_geo_converted, zcol = "line")
mapview(cuny_subway_stations_geo_converted, zcol = "trains")
mapview(subway_lines_geo_converted, zcol = "name")
mapview(cuny_subway_lines_geo_converted, zcol = "route_shor")


# Create a crosswalk from mta_tidy to CUNY subway stations and lines shapefiles
mta_names <- mta_tidy %>%
  ungroup() %>%
  pull(mta_station) %>%
  unique()

cuny_mta_names <- cuny_subway_stations_geo %>%
  as_data_frame() %>%
  dplyr::select(stop_name, trains) %>%
  mutate(trains = str_replace_all(trains, " ", ",")) %>%
  mutate(cuny_mta_station = str_replace(paste0(stop_name, " (", trains, ")"), " - ", "-")) %>%
  pull(cuny_mta_station) %>%
  unique()

# Show mta_names that do not match any cuny_names
setdiff(mta_names, cuny_mta_names)
# Show cuty_mta_names that do not match any mta_names
setdiff(cuny_mta_names, mta_names)

distinct_nypd_names$mta_station[distinct_nypd_names$mta_station == "36 Av (N,Q)"] <- "36 Av (N,W)"

"14 St (A,C,E)/8 Av (L)" 
  
  
## CASE STUDY: 14th St
cuny_subway_stations_geo_converted %>% 
  filter(str_detect(stop_name, "14 St")) %>% #view()
  mapview()

 
## Daily MTA ridership data
daily_mta_ridership <- read_csv("https://new.mta.info/document/20441") %>%
  clean_names() %>% 
  mutate(date = as.Date(date, format = "%m/%d/%Y"),
         subways_percent_of_comparable_pre_pandemic_day = 
           as.numeric(
             str_replace(
               subways_percent_of_comparable_pre_pandemic_day, "%", "")
           )
  ) %>% 
  select(date, 
         subways_total_estimated_ridership,
         subways_percent_of_comparable_pre_pandemic_day)

# Check the earlist and last date in the data
max(daily_mta_ridership$date) # 2021-12-06
min(daily_mta_ridership$date) # 2020-03-01

# See how 
daily_mta_ridership %>%
  ggplot(aes(x = date, y = subways_percent_of_comparable_pre_pandemic_day)) +
  geom_line()

# Group by year and month
monthly_mta_ridership <- daily_mta_ridership %>%
  group_by(year = year(date), month = month(date)) %>%
  summarise(subways_total_estimated_ridership = sum(subways_total_estimated_ridership))

