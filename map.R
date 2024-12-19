
locations <- data.frame(
  Name = c(
    "Trilogy", "401 Park Garage", "Boston Common Garage", "City Place Garage",
    "Prudential Center Garage", "Back Bay Garage", "Boylston St Garage", "88 Kingston St Garage"
  ),
  Latitude = c(42.3442, 42.3456, 42.3536, 42.3510, 42.3473, 42.3493, 42.3519, 42.3510),
  Longitude = c(-71.1030, -71.1004, -71.0657, -71.0662, -71.0820, -71.0760, -71.0667, -71.0603)
)


leaflet(locations) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%  
  addCircleMarkers(
    ~Longitude, ~Latitude, 
    label = ~Name, 
    color = "red",        
    fillColor = "red",   
    radius = 3
  )
