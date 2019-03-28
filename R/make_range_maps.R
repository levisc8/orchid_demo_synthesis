# Make range maps
# data collection
library(fs)
library(sf)
library(dplyr)
library(ggplot2)

output <- read.csv("Data/Csv/orchid_demog_output.csv")
View(output)
#output <- (output[1:76,])
output$Lat <- as.numeric(output$Lat)
output$Lon <- as.numeric(output$Lon)

paths <- dir_ls("Data/GIS/BIEN", glob = "*.shp$")

world_map <- map_data("world")

wrld_base <- ggplot(world_map) +
  geom_polygon(aes(x = long,
                   y = lat,
                   group = group),
               fill = 'gray70') +
  theme(panel.background = element_blank(),
        panel.grid = element_blank())

#creating the picture maps

locations <- read.csv("Data/Csv/locations.cvs")

Maporchidsnew <- wrld_base +
  geom_point(data = locations,
             aes(
               x = as.numeric(locations$output.Lon),
               y = as.numeric(locations$output.Lat)
                ),
             shape = 1,
             size = 3,
             stroke = 2,
             color = "seagreen")
Maporchidsnew



Maporchids <- wrld_base +
  geom_point(data = orchids_s_i$metadata,
             aes(
               x = orchids_s_i$metadata$Lon,
               y = orchids_s_i$metadata$Lat,
               colour = factor(OrganismType)),
             shape = 1,
             size = 3,
             stroke = 2)

Maporchids

#unique(world_map$region)

#install.packages("countrycode")
library(countrycode)

#changing the countrycode so it acna be used for the regions of the map
countries <- countrycode(orchids_s_i$metadata$Country, "iso3c", "country.name")
countries[countries == "United States"] <- "USA"
#View(orchids_s_i$metadata[27,])
#countries[27] <- "USA"
countries

table(countries)

map_c <- wrld_base +
  geom_map(data=orchids_s_i$metadata, map=world_map,
  aes(map_id=countries), colour="red", fill="green", size=0.2)

map_c

#

Maporchids <- wrld_base +
  geom_point(data = orchids_s_i$metadata,
             aes(
               x = orchids_s_i$metadata$Lon,
               y = orchids_s_i$metadata$Lat,
               colour = factor(OrganismType)),
             shape = 1,
             size = 3,
             stroke = 2)


Map_R0 <- wrld_base +
  geom_point(data = output,
             aes(
               x = Lon,
               y = Lat,
               colour = GrowthForm),
             shape = 1,
             size = output$R0,
             stroke = 2)
Map_R0



for(i in seq_along(paths)) {

  # Get the species name from the path and store the shape file as variable
  # with its name
  sp_name <- gsub("Data/GIS/BIEN/|[0-9]|\\.shp$", "", paths[i]) %>%
    substr(x = ., start = 1, stop = (nchar(.) - 1))


  temp_shp <- st_read(paste(getwd(), paths[i], sep = '/'),
                      stringsAsFactors = FALSE)

  assign(sp_name, temp_shp, envir = .GlobalEnv)

  range_lon <- range(st_coordinates(temp_shp)[ , 1])
  range_lat <- range(st_coordinates(temp_shp)[ , 2])

  buffer <- 10

  min_lon <- range_lon[1] - buffer
  max_lon <- range_lon[2] + buffer
  min_lat <- range_lat[1] - buffer
  max_lat <- range_lat[2] + buffer


  wrld_base +
    geom_sf(data = temp_shp, fill = 'red', alpha = 0.4) +
    xlim(c(min_lon, max_lon)) +
    ylim(c(min_lat, max_lat))


  ggsave(filename = paste(sp_name, '_BIEN_range_map.pdf', sep = ""),
         path = "Figures/Range_maps",
         height = 9,
         width = 9,
         unit = "in",
         dpi = 400)

}






# Garbage maps:

# Lycaste_aromatica
# Cleistesiopsis_bifaria
# cypripedium_calceolus
# Platanthera_hookeri
