# Make range maps

library(fs)
library(sf)
library(dplyr)
library(ggplot2)

paths <- dir_ls('Data/GIS/BIEN', glob = "*.shp$")

world_map <- map_data("world")

wrld_base <- ggplot(world_map) +
  geom_polygon(aes(x = long,
                   y = lat,
                   group = group),
               fill = 'gray70') +
  theme(panel.background = element_blank(),
        panel.grid = element_blank())



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
