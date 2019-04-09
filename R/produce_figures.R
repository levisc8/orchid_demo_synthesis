

#############################
### correcting some types ###
#############################

# output <- read.csv('Data/Csv/orchid_demog_output.csv')
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

#### Boxplot

boxR0 <- ggplot(mu,
                aes(fill = GrowthForm)) +
  geom_boxplot(aes(y = sp.mean.R0))+
  labs(x = "GrowthForm", y = "R0") +
  theme(axis.ticks.x=element_blank(),
        axis.line = element_line(color="black", size = 1)
  )




############################################
####     Phylos w/ Demographic rates    ####
############################################

#### Part of the make_Phylos that is needed, make_Phylos still calls for original COMAPDRE (without added orchids) ####

fuzz_tree <- readRDS('Data/Serialized/Phylogenies/all_compadre_bien_fuzzy_matched.RDS')

mol_tree <- readRDS('Data/Serialized/Phylogenies/molecular_bien_tree.rds')

Compadre <- orchids

all_orc_occ <- readRDS('Data/Serialized/BIEN/Occurences.rds')

all_orc_names <- unique(all_orc_occ$scrubbed_species_binomial) %>%
  gsub(' ', '_', .)

orc_compadre <- grepl('Orchidaceae', Compadre$metadata$Family)

orc_compadre_spp <- unique(Compadre$metadata$SpeciesAccepted[orc_compadre]) %>%
  gsub(' ', '_', .)

fuzz_tree <- drop.tip(fuzz_tree, setdiff(fuzz_tree$tip.label, all_orc_names))
fuzz_groups <- split(fuzz_tree$tip.label, fuzz_tree$tip.label %in% orc_compadre_spp)
fuzz_tree <- groupOTU(fuzz_tree, fuzz_groups)
fuzz_tree$tip.label[!fuzz_tree$tip.label %in% orc_compadre_spp] <- ""

fuzz_plot <- ggtree(fuzz_tree,
                    aes(color = group),
                    layout = 'rectangular') +
  geom_tippoint(aes(color = group)) +
  scale_color_manual('',
                     breaks = c(TRUE, FALSE),
                     labels = c("In Compadre",
                                "Not in Compadre"),
                     values = c('red', 'black')) +
  theme(legend.position = 'right') +
  ggtitle("Fuzzily matched, not completely molecular tree")




####
#### Only our orchids and their relation (on genus level) ####
our_fuzz_tree <- fuzz_tree
our_fuzz_tree$tip.label <- sub("_.*", "", our_fuzz_tree$tip.label)
our_fuzz_tree <- drop.tip(our_fuzz_tree, setdiff(our_fuzz_tree$tip.label, sub("_.*", "", orc_compadre_spp)))
our_fuzz_tree <- drop.tip(our_fuzz_tree, our_fuzz_tree$tip.label[c(which(duplicated(our_fuzz_tree$tip.label)))])


fuzz_our_orchids <- ggtree(our_fuzz_tree,
                           layout = 'rectangular') +
  geom_tiplab() +
  xlim_tree(60) +
  theme_tree2()


####
#### with demographic rates ####
a <- data.frame(sub(" .*", "", output$SpeciesAccepted), output$GrowthForm, output$R0, output$GenT, output$Lambda)
names(a) <- c("Genus", "GrowthForm", "R0", "GenT", "Lambda")
a <- a[which( a$Genus %in% our_fuzz_tree$tip.label ),]

# facet_plot(fuzz_our_orchids, panel = "R0", data = a, geom = geom_point, mapping = aes(x= R0))
# facet_plot(fuzz_our_orchids, panel = "GenT", data = a, geom = geom_point, mapping = aes(x= GenT))
# facet_plot(fuzz_our_orchids, panel = "Lambda", data = a, geom = geom_point, mapping = aes(x= Lambda))

d=data.frame(y=1:8, .panel='Lambda')
e=data.frame(y=1:8, .panel= "GenT")
f=data.frame(y=1:8, .panel= "R0")

Lambda_tree <- facet_plot(fuzz_our_orchids,
                          panel = "Lambda",
                          data = a,
                          geom = geom_point,
                          mapping = aes(x= Lambda, color = GrowthForm, size = .5)
) +
  geom_hline(data = d, aes(yintercept = y))

GenT_tree <-facet_plot(fuzz_our_orchids,
                       panel = "GenT",
                       data = a,
                       geom = geom_point,
                       mapping = aes(x= GenT, color = GrowthForm, size = .5)) +
  geom_hline(data = e, aes(yintercept = y))

R0_tree <- facet_plot(fuzz_our_orchids,
                      panel = "R0",
                      data = a,
                      geom = geom_point,
                      mapping = aes(x= R0, color = GrowthForm, size = .5)) +
  geom_hline(data = f, aes(yintercept = y))



L_GenT_tree <- facet_plot(Lambda_tree,
                          panel = "GenT",
                          data = a,
                          geom = geom_point,
                          mapping = aes(x= GenT, color = GrowthForm, size = .5)) +
  geom_hline(data = e, aes(yintercept = y))

LGR_tree <- facet_plot(L_GenT_tree,
                       panel = "R0",
                       data = a,
                       geom = geom_point,
                       mapping = aes(x= R0, color = GrowthForm, size = .5)) +
  geom_hline(data = f, aes(yintercept = y))


####
#### plot ####

Lambda_tree
GenT_tree
R0_tree
LGR_tree


