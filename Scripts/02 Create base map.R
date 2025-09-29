
library(tidyverse)
library(leaflet)
library(sf) # Reading shape files



# Read in public transport network shapefiles --------------------------------------

# Cycling data
cycling_network <- read_sf("Cycling_Network_-_Scotland/pub_cycnt.shp") %>% 
  st_transform(4326) # transform polygons to long/lat system

rail_network <- read_sf("UK_Railways/Railway.shp") %>% 
  filter(FILE_NAME == "gb_north") %>% 
  st_set_crs(27700) %>% # Assign coordinate ref. system to data
  st_transform(4326) # Convert to long/lat system



pal <- colorFactor(
  palette = c('#f0c28d', '#f88de6', '#2596be', 'darkred', '#6ea728', "red", "#6f7e28"),
  domain = All_Services_Locations_filtered$service_type
)




logos <- awesomeIconList(
  "Care Home" = makeAwesomeIcon(
    icon = "glyphicon-bed",
    markerColor = "beige"),
  "Community hospital"= makeAwesomeIcon(
    icon = "glyphicon-heart",
    markerColor = "pink"), 
  "Other Hospitals"= makeAwesomeIcon(
    icon = "glyphicon-heart",
    markerColor = "pink"), 
  "Dentist"= makeAwesomeIcon(
    icon = "glyphicon-apple",
    markerColor = "blue"),
  "Emergency Department"= makeAwesomeIcon(
    icon = "glyphicon-header",
    markerColor = "darkred"),
  "GP Practice"= makeAwesomeIcon(
    icon = "glyphicon-user",
    markerColor = "green"),
  "Minor Injuries Unit"= makeAwesomeIcon(
    icon = "glyphicon-alert",
    markerColor = "red"),
  "Pharmacy"= makeAwesomeIcon(
    icon = "glyphicon-plus-sign",
    markerColor = "darkgreen")
  
)
  

# Create transport links baseline map
base_map <- All_Services_Locations_filtered %>% 
  # Load map
  leaflet()  %>%
  addProviderTiles(
    "OpenStreetMap.Mapnik",
    options = providerTileOptions(opacity = 0.5)
  ) %>% 
  # Set default area & zoom
  setView(lng = -3.99, lat = 55.74, zoom = 9) %>% 
  # Add train network
  addPolylines(data = rail_network,
               weight = 2,
               opacity = 0.5,
               color = "darkgreen",
               group = "Rail network") %>% 
  # Add cycling network
  addPolylines(data = cycling_network,
               weight = 2,
               opacity = 0.25,
               color = "purple",
               group = "Cycle network")




# Iterate over the service types and add each to the map as a unique group:
unique(All_Services_Locations_filtered$service_type) %>%
  purrr::walk( function(x) {
    
    data <- All_Services_Locations_filtered %>% 
      filter(service_type == x)
    
    base_map <<- base_map %>%
      addAwesomeMarkers(data=data,
                        icon = ~logos[service_type], # lookup from list based on ticker
                        label = ~name,
                        clusterOptions = markerClusterOptions(minSize = 20, maxClusterRadius = 1, freezeAtZoom = 20),
                        group = ~x)
  })


# Add final layer controls based on the unique service groups 
base_map <- base_map %>% 
  addLayersControl(
    position = "topright",
    overlayGroups = unique(All_Services_Locations_filtered$service_type),
    baseGroups = c("Cycle network", "Rail network"),
    # set collapsed = FALSE so that controls always displayed
    options = layersControlOptions(collapsed = FALSE)
  ) %>% 
  addLegend(data = All_Services_Locations_filtered,
            position = "bottomleft", 
            title = "Site Type",
            pal = pal, 
            values = ~service_type)

