
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



pal_site_locations <- colorFactor(
  palette = c('#f0c28d','#2596be','darkred','#6ea728', "red",'#f88de6', "#6f7e28","#9370db"),
  domain = c(sort(unique(All_Services_Locations_filtered$service_type)),"Mix Of Services"),
  ordered=TRUE
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

# Find Services Within The Same Postcode ----

postcodes_with_multiple_services <- All_Services_Locations_filtered %>%
  summarise(
    
    services=n(),
    n_service_types=length(unique(service_type)),
    .by=c("postcode")
    
  ) %>%
  arrange(desc(services)) %>%
  filter(services > 1)
  
All_Services_unclustered <- All_Services_Locations_filtered %>%
  filter(!(postcode %in% postcodes_with_multiple_services$postcode))



# Iterate over the service types and add each to the map as a unique group:
unique(All_Services_unclustered$service_type) %>%
  purrr::walk( function(x) {
    
    data <- All_Services_unclustered %>% 
      filter(service_type == x)
    
    base_map <<- base_map %>%
      addAwesomeMarkers(data=data,
                        icon = ~logos[service_type], # lookup from list based on ticker
                        label = ~name,
                        clusterOptions = markerClusterOptions(minSize = 20, maxClusterRadius = 1, freezeAtZoom = 20),
                        group = ~x)
  })


All_Services_Clustered <- All_Services_Locations_filtered %>%
  filter((postcode %in% postcodes_with_multiple_services$postcode))

for (postcode_oi in unique(All_Services_Clustered$postcode)){
  
  data_oi <- All_Services_Clustered %>%
    filter(postcode == postcode_oi)
  
  service_types_oi <- data_oi %>%
    pull(service_type) 
  
  unique_service_types_oi <- unique(service_types_oi)
  
  if (length(unique_service_types_oi) == 1){
  
    # cluster_colour <- logos[[unique_service_types_oi]]$markerColor

    # cluster_colour <- case_when(
    #   
    #   cluster_colour == "darkgreen" ~ "#728224"
    #   
    #   cluster_colour == "darkred" ~ "#973034",
    #   
    #   cluster_colour == "beige" ~ "#FFCA91",
    #   
    #   cluster_colour == "red" ~ "",
    #   
    #   cluster_colour == "blue" ~ "#38A9DC"
    #   
    #   
    # )
    
    cluster_colour <- pal_site_locations(unique_service_types_oi)
    
    cluster_colour <- paste0("rgba(",paste0(as.vector(col2rgb(cluster_colour,alpha=TRUE)),collapse=", "),")")
    
    
  } else{
    
    cluster_colour <- "#9370db"
    
  }
  
  
  base_map <- base_map %>%
    addAwesomeMarkers(data = data_oi,
                      icon = ~logos[service_type],
                      label = ~name,
                      group = ~service_type,
                      clusterOptions = markerClusterOptions(
                        
                        # Looks Complicated but just fixes colour to be selected colour for cluster
                        iconCreateFunction=JS(
                          paste0("function (cluster) {
                        return new L.DivIcon({ html: '<div style=\"background-color:'+'",cluster_colour,"'+'\"><span>' + cluster.getChildCount() + '</span></div>', className: 'marker-cluster', iconSize: new L.Point(40, 40) });}")
                        )),
                      
                      popupOptions = popupOptions(textsize="18px")
                      
    ) 
  
}



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
            pal = pal_site_locations, 
            values = ~c(service_type,"Mix Of Services"))





