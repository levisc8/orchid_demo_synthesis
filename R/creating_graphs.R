#######Pretty pictures aka statistical graphics#######


#summarise demographics per species (accepted)
library(plyr)
library(dplyr)
library(ggplot2)

Orch <- read.csv('Data/Csv/orchid_demog_output.csv')
Orch$GrowthForm[Orch$GrowthForm=="Geophyte"] <- "Herbaceous perennial"
View(Orch)


#### mu includes all the pretty data to display. Ecoregion and pRep is added for no particular good reason, might as well get removed.
#### there are many plots for the pRep value though (chance to survive reproduction)

#Orch <- (Orch[1:76,])
mu <- ddply(Orch, c("GrowthForm", "SpeciesAccepted", "Ecoregion"), summarise, sp.mean.R0 = mean(R0, na.rm = TRUE), sp.mean.GenT = mean (GenT, na.rm=TRUE), sp.mean.Lambda = mean(Lambda, na.rm = TRUE), sp.mean.pRep = mean(pRep, na.rm = TRUE))
View(mu)


#### i removed the NaN's here. but dont want to repeat this again, hence the #
#mu <- (mu[c(1:10, 12:15, 17:22, 26, 27, 29:33),])
#View(mu[c(1:10, 12:15, 17:22, 26, 27, 29:33),])
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

###this was one of the last things i worked on, i did it wrong.
###not sure why, but at this point my internship is almost over and i rather try to make the things i did as readable as possible for you
LP + geom_density(kernel = "gaussian")
