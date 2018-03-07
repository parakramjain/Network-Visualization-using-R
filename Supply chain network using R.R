# Load the libraries
#```{r, message=FALSE, warning=FALSE}
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}

detachAllPackages()

suppressWarnings({
  library(tidyverse)
  library(dplyr)
  library(ggplot2)
  library(plotly)
  library(RColorBrewer)
  library(shiny)
  library(leaflet)
})
#```

# Standardize variables function
# - Sets the names of the variables to uppercase and replaces all spaces with underscores
standardizeVars <- function(df) return(df %>% setNames(tolower(gsub("__", "_", gsub(" ", "_", gsub(":", "",gsub("-", "",  names(df))))))))

# Set working directory
setwd("C:/Users/parakram.jain/Desktop/Analytics/POC/Supply_Chain")

serv_lvl_base <- read.csv("serv_lvl_sample_base_new.csv", header=TRUE) %>% 
  standardizeVars() %>%
  mutate(ship_site = as.character(ship_site),
         rcv_site = as.character(rcv_site_num),
         ship_lat = as.numeric(ship_lat),
         ship_long = as.numeric(ship_long),
         rcv_lat = as.numeric(rcv_lat),
         rcv_long = as.numeric(rcv_long),
         ship_site_type = as.factor(ifelse(nchar(ship_site) > 6, "Vendor", ifelse(substr(ship_site, 1,1) == "D", "DC", "Store"))),
         rcv_site_type = as.factor(ifelse(nchar(rcv_site) > 6, "Vendor", ifelse(substr(rcv_site, 1,1) == "D", "DC", "Store")))
         )
  
str(serv_lvl_base)
#####################


##### Shiny App #####
pal <- colorFactor(c("navy", "red"), domain = c("ship", "pirate"))

ui <- fluidPage(
  leafletOutput("mymap", width = "100%", height = "100%")
)

server <- function(input, output, session) {

  output$mymap <- renderLeaflet({
    leaflet(data = serv_lvl_base[1:100,]) %>% addTiles() %>%
      ifelse(ship_site_type == "Store", addCircleMarkers(~ship_long, ~ship_lat, popup = ~as.character(ship_site), label = ~as.character(mean_profit_per),
                       radius = ~ifelse(mean_profit_per >= 20, 6, 8),
                       #color = ~pal(mag),
                       color = ~color,
                       stroke = FALSE, 
                       fillOpacity = 0.5), addMarkers(~ship_long, ~ship_lat, popup = ~as.character(ship_site), label = ~as.character(mean_profit_per))) %>%
      ifelse(rcv_site_type == "Store", addCircleMarkers(~rcv_long, ~rcv_lat, popup = ~as.character(rcv_site_num), label = ~as.character(mean_profit_per),
                       radius = ~ifelse(mean_profit_per >= 20, 6, 10),
                       #color = ~pal(mag),
                       color = ~color,
                       stroke = FALSE, 
                       fillOpacity = 0.5), addMarkers(~rcv_long, ~rcv_lat, popup = ~as.character(rcv_site), label = ~as.character(mean_profit_per)))
  })
}

shinyApp(ui, server)

