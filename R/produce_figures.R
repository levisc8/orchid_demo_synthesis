

#############################
### correcting some types ###
#############################

output <- read.csv('Data/Csv/orchid_demog_output_78.csv')
output$YearPublication <- as.integer(output$YearPublication)
output$StartYear <- as.integer(output$StartYear)
output$StartSeason <- as.integer(output$StartSeason)
output$StartMonth <- as.integer(output$StartMonth)
output$Lat <- as.numeric(output$Lat)
output$Lon <- as.numeric(output$Lon)
output$GrowthForm[output$GrowthForm == "Geophyte"] <- "Herbaceous perennial"



#####################
####     Maps    ####
#####################
#### Each dot is a population ####

paths <- dir_ls('Data/GIS/BIEN', glob = "*.shp$")


### World map ###
world_map <- map_data("world")

wrld_base <- ggplot(world_map) +
  geom_polygon(aes(x = long,
                   y = lat,
                   group = group),
               fill = 'gray70') +
  theme(panel.background = element_blank(),
        panel.grid = element_blank())


MapOrchids<- wrld_base +
  geom_point(data = output,
             aes(
               x = Lon,
               y = Lat,
               group = GrowthForm,
               colour = GrowthForm),
             shape = 1,
             size = 3,
             stroke = 2) +
  coord_fixed(ratio = 1.1)

### Europe ###

EuropeOrchids <- wrld_base +
  geom_point(data = output,
             aes(
               x = Lon,
               y = Lat,
               group = GrowthForm,
               colour = GrowthForm),
             shape = 1,
             size = 3,
             stroke = 2) +
  coord_fixed(xlim=c(-10, 25),
              ylim=c(35, 65),
              ratio=1.3)

### N & C America (Sort of) ###

USOrchids <- wrld_base +
  geom_point(data = output,
             aes(
               x = Lon,
               y = Lat,
               group = GrowthForm,
               colour = GrowthForm),
             shape = 1,
             size = 3,
             stroke = 2) +
  coord_fixed(xlim=c(-125, -60),
            ylim=c(0, 50),
            ratio=1)





#############################
####     Demographics    ####
#############################

##########
### R0 ###
##########

## R0 plotted along both Lat & Lon, as well as sepperately
ggplot(output, aes(x = Lon, y = Lat , group = GrowthForm, color = GrowthForm)) + geom_point(aes(size = R0))
ggplot(output, aes(x = R0, y = Lat, group = GrowthForm, color = GrowthForm)) + geom_point()
ggplot(output, aes(x = Lon, y = R0 , group = GrowthForm, color = GrowthForm)) + geom_point()


### Distribution of R0 values
mu <- ddply(output, c("GrowthForm", "SpeciesAccepted"), summarise, sp.mean.R0 = mean(R0, na.rm = TRUE), sp.mean.GenT = mean(GenT, na.rm = TRUE), sp.mean.Lambda = mean(Lambda, na.rm = TRUE))
mu1 <- ddply(mu, "GrowthForm", summarise, type.mean.R0 = mean(sp.mean.R0, na.rm = TRUE), type.mean.GenT = mean(sp.mean.GenT, na.rm = TRUE), type.mean.Lambda = mean(sp.mean.Lambda, na.rm = TRUE))

# including all sperate populations
R0disALL <- ggplot(output,
                   aes(x = R0,
                       color = GrowthForm,
                       fill = GrowthForm)) +
  geom_density(alpha = 0.4) +
  geom_vline(data = mu1,
             aes(xintercept = type.mean.R0,
                 color = GrowthForm),
             linetype="dashed",
             size = 1) +
  theme(panel.background = element_blank()) +
  xlim(0, 13)+
  ylim(0, 1.1)

# for species mean
R0dismean <- ggplot(mu,
                    aes(x = sp.mean.R0,
                        color = GrowthForm,
                        fill = GrowthForm)) +
  geom_density(alpha = 0.4) +
  geom_vline(data = mu1,
             aes(xintercept = type.mean.R0,
                 color = GrowthForm),
             linetype="dashed",
             size = 1) +
  theme(panel.background = element_blank())+
  xlim(0, 13)+
  ylim(0, 1.1)
