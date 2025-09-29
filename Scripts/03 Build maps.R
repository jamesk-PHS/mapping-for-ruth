# 1. Housekeeping and set up -------------------------------------------------

# Mapping Packages
# For installation instructions & setting up environment
# see the following link:
# https://public-health-scotland.github.io/knowledge-base/docs/Posit%20Infrastructure?doc=How%20to%20Install%20and%20Use%20Geospatial%20R%20Packages.md

library(tidyverse)
library(leaflet) # Creating map
library(sf) # Reading shape files
library(RColorBrewer) # colour palette

# 2 Importing shapefile data ----------------------------------------------

# greenbelt shapefiles
greenbelt <- read_sf("Greenbelt data/Green_Belt_-_Scotland/pub_grnblt.shp") %>% 
  filter(local_auth %in% local_authorities_oi$CAName) %>%
  st_transform(4326) %>% 
  mutate(colour = "green") # Adding colour variable for mapping

# Scottish Vacant and Derelict Land 
vdl <- read_sf("Greenbelt data/Vacant_and_Derelict_Land_-_Scotland/pub_vdlPolygon.shp") %>% 
  filter(local_auth %in% local_authorities_oi$CAName) %>%
  st_transform(4326) %>% 
  mutate(colour = case_when(site_type == "Derelict" ~ "brown",
                            site_type == "Vacant Land" ~ "grey"),
         site_name = str_to_sentence(site_name))

# Greenfiled/Brownfield status - housing & land supply
housing_land <- read_sf("Housing Land Supply/HLAA2019_GCV.shp") %>%
  janitor::clean_names() %>% 
  filter(authority %in% c("NL", "SL")) %>%  # Filtering for necessary data only
  st_transform(4326)

# Population density & urbanity data
# read in datazone shapefiles

# Create temp files
temp <- tempfile()
temp2 <- tempfile()

# Download the zip file and save to 'temp' 
URL <- "https://maps.gov.scot/ATOM/shapefiles/SG_DataZoneBdry_2011.zip"
download.file(URL, temp)

# Unzip the contents of the temp and save unzipped content in 'temp2'
unzip(zipfile = temp, exdir = temp2)

# Read the shapefile. Alternatively make an assignment, such as f<-sf::read_sf(your_SHP_file)

population_density <-  read_sf(temp2) %>% 
      janitor::clean_names() %>% 
      dplyr::select(data_zone, std_area_km2) %>%
      st_transform(4326) %>% 
  left_join(phsopendata::get_resource("395476ab-0720-4740-be07-ff4467141352") %>% 
  janitor::clean_names(),
    
    by = "data_zone") %>%
  mutate(hb_name = readable_HB_name(hb_name)) %>%
  filter(hb_name == healthboard_oi) %>% 
  
  # add most recent pop estimates
  left_join(
    phsopendata::get_resource("c505f490-c201-44bd-abd1-1bd7a64285ee") %>% 
      janitor::clean_names() %>% 
      filter(year == max(year), sex == "All") %>%
      dplyr::select(data_zone, pop_year = year, all_ages),
    
    by = "data_zone") %>%  # end of left_join() call
  
  # add urban-rural classification
  left_join(
    phsopendata::get_resource("c8bd76cd-6613-4dd7-8a28-6c99a16dc678") %>% 
      janitor::clean_names() %>% 
      dplyr::select(data_zone, ur8_2020_name = urban_rural8fold2020) %>% 
      mutate(ur8_2020_name = case_when(ur8_2020_name == 1 ~ "1 - Large Urban Areas",
                                       ur8_2020_name == 2 ~ "2 - Other Urban Areas",
                                       ur8_2020_name == 3 ~ "3 - Accessible Small Towns",
                                       ur8_2020_name == 4 ~ "4 - Remote Small Towns",
                                       ur8_2020_name == 5 ~ "5 - Very Remote Small Towns",
                                       ur8_2020_name == 6 ~ "6 - Accessible Rural Areas",
                                       ur8_2020_name == 7 ~ "7 - Remote Rural Areas",
                                       ur8_2020_name == 8 ~ "8 - Very Remote Rural Areas"),
             ur8_2020_name = as.factor(ur8_2020_name)),
    
    by = "data_zone") %>%
  
  # calculate population density
  mutate(pop_density = all_ages/std_area_km2,
         pop_density_log = log(all_ages/std_area_km2))

# rural dataset with combined polygons for each classification 
rurality <- population_density %>%
  group_by(ur8_2020_name) %>%
  summarize(geometry = st_union(geometry))

### 3 Build maps ----

# blues <- RColorBrewer::brewer.pal("Blues")
# darker_blues <- RColorBrewer::brewer.pal(name = "Blues", n = 9)[4:8]

#bin_colours <- brewer.pal(n = 9, name = "BuPu")

# Define colour palettes
#pal_1 <- colorBin(bin_colours, population_density$pop_density, bins = 7, pretty = T)
pal_1 <- colorNumeric("magma", population_density$pop_density_log)
pal_2 <- colorFactor("magma", population_density$ur8_2020_name, reverse = T) # Reverse colour scheme to match population density

# Greenbelt map
base_map %>% 
  addPolygons(data = greenbelt,
              fillColor = "green",
              weight = 1,
              opacity = 0.75,
              color = "black") %>%
  addLegend(position = "bottomleft", 
            title = "Green Belt",
            colors = c("green"), 
            labels = c("")) 

# Vacant & Derelict Land map

base_map %>% 
  addPolygons(data = vdl,
              fillColor = ~colour,
              weight = 1,
              opacity = 0.75,
              color = "black") %>%
  addLegend(position = "bottomleft", 
            title = "Vacant and Derelict Land",
            colors = c("grey", "brown"), 
            labels = c("Vacant", "Derelict")) 

# Greenfield/Brownfield map
base_map %>% 
  addPolygons(data = housing_land,
              fillColor = ~devtype2,
              weight = 1,
              opacity = 0.75,
              color = "black") %>%
  addLegend(position = "bottomleft", 
            title = "Greenfield/Brownfield status",
            colors = c("green", "brown"), 
            labels = c("Greenfield", "Brownfield")) 


# Population density
base_map %>% 
  addPolygons(data = population_density,
              fillColor = ~pal_1(pop_density_log),
              fillOpacity = 0.5, # Adjusting opacity with new routes layers
              smoothFactor = 0.1, # better accuracy to avoid "gaps" between polygons when zoomed out
              stroke = F, # this removes borderlines around polygons
              weight = 1) %>%
  addLegend(data = population_density,
            position = "bottomleft", 
            title = "Relative population density",
            pal = pal_1,
            values = ~pop_density_log) 


# Rurality
base_map %>% 
  addPolygons(data = rurality,
              fillColor = ~pal_2(ur8_2020_name),
              fillOpacity = 0.5, # Adjusting opacity with new routes layers
              weight = 1,
              color = ~pal_2(ur8_2020_name)) %>%
  addLegend(data = population_density,
            position = "bottomleft", 
            title = "Urban/Rural Classification",
            pal = pal_2, 
            values = ~ur8_2020_name) 

