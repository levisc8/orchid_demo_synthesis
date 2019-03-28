############## I also share with you my scribble code
############## if it is not in here, i did not do it.
############## well maybe i did, but i never safed it.




compadre <- readRDS('Data/Serialized/Compadre/compadre_plus_new_studies.rds')

sub_ind <- which(compadre$metadata$Family == 'Orchidaceae')

orchids <- list(metadata = compadre$metadata[sub_ind, ],
                matrixClass = compadre$matrixClass[sub_ind],
                mat = compadre$mat[sub_ind])

countries <- countrycode(orchids$metadata$Country, "iso3c", "country.name")

countries[countries == "United States"] <- "USA"
write.table(table(countries), file = "D:/o1.txt")



compadre <- readRDS('Data/Serialized/Compadre/compadre_unrel_v4.rds')

sub_ind <- which(compadre$metadata$Family == 'Orchidaceae')

orchids <- list(metadata = compadre$metadata[sub_ind, ],
                matrixClass = compadre$matrixClass[sub_ind],
                mat = compadre$mat[sub_ind])

countries <- countrycode(orchids$metadata$Country, "iso3c", "country.name")

countries[countries == "United States"] <- "USA"
write.table(table(countries), file = "D:/o2.txt")

names(orchids)

list.remove()

print(orchids$metadata$Authors[259:264])



#########################
#install.packages("readxl")


n_occur <- data.frame(table(orchids$mat[[1]]))
#gives you a data frame with a list of ids and the number of times they occurred.

n_occur[n_occur$Freq > 1,]
#tells you which ids occurred more than once.

vocabulary[vocabulary$id %in% n_occur$Var1[n_occur$Freq > 1],]

orchids$metadata$DOI.ISBN[158]

Duplicated(orchids$mat[[]])

firstAuthor = strsplit(orchids$metadata$Authors, ';') %>%
  vapply(FUN = function(x) x[1], FUN.VALUE = character(1))

reusedStudies  logical(270)
for(i in 1:dim(orchids$metadata)[2]){
  reusedStudies[i] = any(grepl(orchids$metadata$dupind[i],
                               orchids$metadata$AdditionalSource))
}


#################### arbeit mit gewählten matritzen

#str(cntr_i)
#cntr_i[200]

source('R/subset_orchids.R')
table(orchids$metadata$SpeciesAccepted)
#single_ind <- c(1, 2, 5, 9, 13, 17, 21, 22, 23, 24, 25, 26, 27, 31, 38, 42, 46, 47, 64, 70, 76, 104, 110, 116, 121, 149, 150, 151, 158, 159, 164, 167, 168, 169, 171, 173, 181, 182, 183, 184, 185, 186, 187, 188, 196, 239, 243, 246, 247, 248, 251, 252, 253, 254, 255, 256, 257, 258, 259, 260)

single_ind <- c(1, 2, 5, 9, 13, 17, 21, 22, 23, 24, 25, 26, 28, 29, 30, 32, 33, 34, 35, 36, 37, 38, 42, 46, 48, 53, 64, 70, 77, 86, 95, 104, 110, 116, 122, 131, 140, 149, 150, 152, 153, 159, 164, 167, 168, 169, 171, 172, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 189, 190, 191, 192, 193, 197, 204, 211, 218, 225, 232, 240, 241, 242, 245, 246, 247, 248, 249, 250, 251, 252, 255, 258, 259, 260, 261, 262, 263, 264)

#View(orchids$metadata[,1])
orchids <- list(metadata    = orchids$metadata[single_ind,],
           matrixClass = orchids$matrixClass[single_ind],
           mat         = orchids$mat[single_ind])

View(orchids$metadata[, c("SpeciesAuthor", "MatrixTreatment", "MatrixComposite", "MatrixStartYear", "MatrixEndYear", "MatrixPopulation","DOI.ISBN")])

View(orchids_s_i$metadata)

#output <- (output[1:76,])
View(output)

#summarise demographics per species (accepted)
library(plyr)
library(dplyr)
library(ggplot2)

Orch <- read.csv('Data/Csv/orchid_demog_output.csv')
Orch$GrowthForm[Orch$GrowthForm=="Geophyte"] <- "Herbaceous perennial"
View(Orch)

#Orch <- (Orch[1:76,])
mu <- ddply(Orch, c("GrowthForm", "SpeciesAccepted", "Ecoregion"), summarise, sp.mean.R0 = mean(R0, na.rm = TRUE), sp.mean.GenT = mean (GenT, na.rm=TRUE), sp.mean.Lambda = mean(Lambda, na.rm = TRUE), sp.mean.pRep = mean(pRep, na.rm = TRUE))
View(mu)

#mu <- (mu[c(1:10, 12:15, 17:22, 26, 27, 29:33),])
#View(mu[c(1:14,17,18,20:23),])
ggplot(mu, aes(x = log(sp.mean.GenT))) +
  geom_freqpoly(aes(color = GrowthForm), size = 2) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank())

ggplot(mu, aes(x = log(sp.mean.R0))) +
  geom_freqpoly(aes(color = GrowthForm), size = 2) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank())

ggplot(mu, aes(x = log(sp.mean.Lambda))) +
  geom_freqpoly(aes(color = GrowthForm), size = 2) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank())

ggplot(mu, aes(x = log(sp.mean.pRep))) +
  geom_freqpoly(aes(color = GrowthForm), size = 2) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank())


plot(mu$SpeciesAccepted, mu$sp.mean.GenT)

#gent_ci <- ggplot()+
     geom_point(data = mu,
             aes(
               x = 1:length(mu$sp.mean.GenT)/2,
               y = 1,
               colour = factor(GrowthForm)),
             shape = 1,
             size = mu$sp.mean.GenT,
             stroke = 2)



######### species distribution of "gent", "r0", "lambda", "first rep survival P"

ggg <- ggplot(data=mu, aes(x = SpeciesAccepted, y=mu$sp.mean.GenT, fill = GrowthForm))
gent_bar <- ggg +
            geom_bar( stat = "identity" )
gent_bar

ggR <- ggplot(data=mu, aes(x = SpeciesAccepted, y=mu$sp.mean.R0, fill = GrowthForm))
R0_bar <- ggR +
  geom_bar( stat = "identity" )
R0_bar

ggL <- ggplot(data=mu, aes(x = SpeciesAccepted, y=mu$sp.mean.Lambda, fill = GrowthForm))
Lambda_bar <- ggL +
  geom_bar( stat = "identity" )
Lambda_bar

ggSRP <- ggplot(data=mu, aes(x = SpeciesAccepted, y=mu$sp.mean.pRep, fill = GrowthForm))
SRP_bar <- ggSRP +
  geom_bar( stat = "identity" )
SRP_bar

######### Growthform distribution of "gent", "r0", "lambda", "first rep survival P"

ggg <- ggplot(data=mu, aes(x = GrowthForm, y=sp.mean.GenT, group = GrowthForm, fill = GrowthForm))
gent_box <- ggg +
  geom_boxplot(aes())
gent_box + geom_jitter(aes(), alpha=0.9,
                       position=position_jitter(w=0.1,h=0.1))

ggR0 <- ggplot(data=mu, aes(x = GrowthForm, y=sp.mean.R0, group = GrowthForm, fill = GrowthForm))
R0_box <- ggR0 +
  geom_boxplot(aes())
R0_box + geom_jitter(aes(), alpha=0.9,
                     position=position_jitter(w=0.1,h=0.1))

ggLambda <- ggplot(data=mu, aes(x = GrowthForm, y=sp.mean.Lambda, group = GrowthForm, fill = GrowthForm))
Lambda_box <- ggLambda +
  geom_boxplot(aes())
Lambda_box + geom_jitter(aes(), alpha=0.9,
                         position=position_jitter(w=0.1,h=0.1))

ggSRP <- ggplot(data=mu, aes(x = GrowthForm, y=sp.mean.Lambda, group = GrowthForm, fill = GrowthForm))
SRP_box <- ggSRP +
  geom_boxplot(aes())
SRP_box + geom_jitter(aes(), alpha=0.9,
                         position=position_jitter(w=0.1,h=0.1))


######### mashed versions


GL <- ggplot(data=mu, aes(x = mu$sp.mean.GenT, y=mu$sp.mean.Lambda, color= GrowthForm))
GL_point <- GL +
  geom_point( stat = "identity" )
GL_point

GR <- ggplot(data=mu, aes(x = mu$sp.mean.GenT, y=mu$sp.mean.R0, color= GrowthForm))
GR_point <- GR +
  geom_point( stat = "identity" )
GR_point

GP <- ggplot(data=mu, aes(x = mu$sp.mean.GenT, y=mu$sp.mean.pRep, color= GrowthForm))
GP_point <- GP +
  geom_point( stat = "identity" )
GP_point

LR <- ggplot(data=mu, aes(x = mu$sp.mean.Lambda, y=mu$sp.mean.R0, color= GrowthForm))
LR_point <- LR +
  geom_point( stat = "identity" )
LR_point

LP <- ggplot(data=mu, aes(x = mu$sp.mean.Lambda, y=mu$sp.mean.pRep, color= GrowthForm))
LP_point <- LP +
  geom_point( stat = "identity" )
LP_point

RP <- ggplot(data=mu, aes(x = mu$sp.mean.R0, y=mu$sp.mean.pRep, color= GrowthForm))
RP_point <- RP +
  geom_point( stat = "identity" )
RP_point

LR + geom_smooth(model = lm)

LP + geom_density(kernel = "gaussian")


#################
#locations <- data.frame(output$SpeciesAuthor, output$Country, output$Lat, output$Lon)
View(locations)
#locations <- locations[1:92,]

locations$output.Country[39] <- "USA"
locations$output.Lat[39] <- lat.dec
locations$output.Lon[39] <- lon.dec
locations <- rbind(locations, c("Serapias_cordigera", "ITA" , lat.dec, lon.dec))

write.csv2(locations, "Data/Csv/locations.csv")

-

warnings()


