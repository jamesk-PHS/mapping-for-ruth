library(phsstyles)
library(leaflet)
library(htmlwidgets)
library(htmltools)
library(glue)
library(dplyr)
library(readxl)
library(janitor)
library(readr)
library(tidyverse)

read_in_localities <- function(dz_level = F) {
  test <- data <- fs::dir_ls(
    glue(
      "/conf/linkage/output/lookups/Unicode/",
      "Geography/HSCP Locality/"
    ),
    regexp = glue("HSCP Localities_DZ11_Lookup_.+?\\.rds$")
  )  |> 
    # Read in the most up to date lookup version
    max()  |> 
    read_rds()
  
  if (dz_level == F) {
    data  |> 
      clean_names() |> 
      mutate(hscp_locality = gsub("&", "and", hscp_locality)) |> 
      select(hscp_locality, hscp2019name, hscp2019, hb2019name, hb2019)  |> 
      distinct()
  } else if (dz_level == T) {
    data  |> 
      clean_names() |> 
      mutate(hscp_locality = gsub("&", "and", hscp_locality))  |> 
      select(datazone2011, hscp_locality, hscp2019name, hscp2019, hb2019name, hb2019)
  }
}

read_in_postcodes <- function(){
  
  fs::dir_ls(glue("/conf/linkage/output/lookups/Unicode/Geography/",
                  "Scottish Postcode Directory/"),
             regexp = glue(".rds$")) %>%
    
    #Read in the most up to date lookup version
    max() %>%
    
    read_rds() %>%
    
    clean_names()  %>% 
    
    select(-c(hscp2019, hscp2019name, hb2019, hb2019name)) %>% 
    
    left_join(read_in_localities(dz_level = T))
  
}


### Geographical look-ups and objects ----

#Locality lookup
lookup <- read_in_localities(dz_level = T)

#Lookup without datazones
lookup2 <- read_in_localities()

# Get number of localities in HSCP/HB
n_loc <- lookup2 %>%
  group_by(hb2019name) %>%
  summarise(locality_n = n()) %>%
  filter(str_detect(hb2019name, "Lanarkshire")) %>%
  group_by(hb2019name) %>%
  summarise(sum = sum(locality_n)) %>% 
  pull()
###### 2. Read in services data ######

## Read in Postcode file for latitudes and longitudes

postcode_lkp <- read_in_postcodes() %>% 
  mutate(postcode = gsub(" ", "", pc7)) %>% 
  select(postcode, grid_reference_easting, grid_reference_northing, latitude, longitude, datazone2011, 
         hscp2019name, hscp2019, hb2019name, hb2019)

prac <- phsopendata::get_resource("0d2e258a-1451-4af1-a7e5-e8327994fa55", row_filters = list(HB = "S08000032")) %>% # Filters For HB
  clean_names()|> 
  as.data.frame() |> 
  ungroup()

###### Manipulate services data ######

## GP Practices ----

prac <- prac %>% 
  select(practice_code, gp_practice_name, practice_list_size, postcode) %>% 
  mutate(postcode = gsub(" ", "", postcode)) 

# Merge practice data with postcode and locality lookups
markers_gp <- left_join(prac, postcode_lkp, by = "postcode") %>% 
  mutate(type = "GP Practice") %>% 
  #filter out HSCP for map
  filter(hb2019name == 'NHS Lanarkshire') |> # Filters For HB
  left_join(lookup, by ='datazone2011')


####Map Code#####


## locality Shapefile 
shp <- sf::read_sf("/conf/linkage/output/lookups/Unicode/Geography/Shapefiles/HSCP Locality (Datazone2011 Base)/HSCP_Locality.shp")
shp <- sf::st_transform(shp,4326)

shp <- shp |> 
  rename(hscp_locality = hscp_local) |> 
  merge(lookup, by = "hscp_locality")

shp_hscp <- shp |> 
  #filter(hscp2019name == HSCP)
  filter(hb2019name == 'NHS Lanarkshire') # Filters For HB

###Add colours to localities based on number of localities 

col_palette <- rainbow(10)

loc.cols <- colorFactor(col_palette, domain = shp_hscp$hscp_locality)

##Year text
tag.map.text <- tags$style(HTML("
                                .leaflet-control.map-title {
                                left: 0%;
                                text-align: centre;
                                padding-left: 5px; 
                                padding-right: 5px; 
                                background: rgba(255,255,255,0.75);
                                font-weight: bold;
                                font-size: 20px;
                                }
                                "))


text1 <- tags$div(
  tag.map.text, HTML(paste("Type Title Here"))
) 

## Create function for adding circle markers
addLegendCustom <- function(map, colors, labels, sizes, opacity = 1) {
  colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes, "px; height:", sizes, "px")
  labelAdditions <- paste0(
    "<div style='display: inline-block;height: ",
    sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>",
    labels, "</div>"
  )
  
  return(addLegend(map,
                   colors = colorAdditions,
                   labels = labelAdditions, opacity = opacity
  ))
}



#### Map

leaflet()  |> 
  addControl(text1, position="topright", className = "map-title")%>%
  addProviderTiles(providers$OpenStreetMap) |> 
  addPolygons(data = shp_hscp,
              fillColor = ~ loc.cols(hscp_locality), 
              fillOpacity = 0.7, 
              color = "black",
              stroke = T, 
              weight = 1, 
              group = "hscp_locality",
              popup = paste0("Locality: <b>", shp_hscp$hscp_locality)) |> 
  # Markers
  addLegend("bottomright", pal = loc.cols, values = shp_hscp$hscp_locality, title = "Locality", opacity = 0.7, group = "Locality") |> 
  addCircleMarkers(
    lng = markers_gp$longitude, lat = markers_gp$latitude, group = "GP practices",
    color = "black", radius = 3.5, weight = 1, opacity = 1, fillOpacity = 1, fillColor = "red"
  )  |> 
  # Custom markers legend
  addLegendCustom(colors = "red", labels = "GP practices", sizes = 10) 



