# Query BIEN for Orchid spp info

library(BIEN)
library(dplyr)
library(fs)
library(ape)
library(pez)

compadre <- readRDS('Data/Serialized/Compadre/Compadre_unrel_v4.rds')

# Find orchid species and subset them out
orc_ind <- grepl('Orchid', compadre$metadata$Family)

orcs <- list(metadata = compadre$metadata[orc_ind, ],
             matrixClass = compadre$matrixClass[orc_ind],
             mat = compadre$mat[orc_ind])

orc_names <- orcs$metadata$SpeciesAccepted

# Bien is a beast. Seriously

# vignette("BIEN")

# All Orchidaceae occurences.
# orc_occs <- BIEN_occurrence_family("Orchidaceae")
# saveRDS(orc_occs, file = "Data/Serialized/BIEN/Occurences.rds")

orc_occs <- readRDS('Data/Serialized/BIEN/Occurences.rds') %>%
  filter(!is.na(latitude) &
           !is.na(longitude) &
           scrubbed_species_binomial %in% orc_names)

write.csv(orc_occs, file = "Data/Csv/BIEN/Occurences_Spp_in_Compadre.csv",
          row.names = FALSE)

# number of species that are in compadre that aren't in Bien occurence data. Damn!
length(setdiff(unique(orc_names),
               unique(orc_occs$scrubbed_species_binomial)))


# All botanical traits for orchidaceae

# orc_traits <- BIEN_trait_family("Orchidaceae")
# saveRDS(orc_traits, file = "Data/Serialized/BIEN/Botanical_Traits.rds")

orc_traits <- readRDS('Data/Serialized/BIEN/Botanical_Traits.RDS') %>%
  filter(scrubbed_species_binomial %in% orc_names)

write.csv(orc_traits, file = "Data/Csv/BIEN/Traits_Spp_in_Compadre.csv",
          row.names = FALSE)

# number of species that are in compadre that aren't in Bien occurence data. Much better
length(setdiff(unique(orc_names),
               unique(orc_traits$scrubbed_species_binomial)))

# Range maps. details here: http://bien.nceas.ucsb.edu/bien/biendata/bien-3/

# orc_maps <- BIEN_ranges_species(orc_names,
#                                 directory = "Data/GIS/BIEN/",
#                                 include.gid = TRUE) # unique id for each map

# 12 species w/ complete range maps
fs::dir_ls("Data/GIS/BIEN/", glob = '*.dbf$') %>% length()


# BIEN phylogeny. More info at  http://bien.nceas.ucsb.edu/bien/biendata/bien-2/phylogeny/

big_bien_phylo <- BIEN_phylogeny_complete()

conservative_bien_phylo <- BIEN_phylogeny_conservative()

for_merge <- gsub(" ", "_", unique(orc_names))

# This script isn't in the business of making specific phylogenies beyond the
# overlap between BIEN and Compadre. See make_phylogenies.R for the visualizations

# make conservative Compadre phylogeny

conservative_compadre_phylo <- drop.tip(conservative_bien_phylo,
                                        setdiff(conservative_bien_phylo$tip.label,
                                                for_merge))

# Congeneric merge using fuzzy matching to get all of our species into the phylogeny

big_tree <- congeneric.merge(big_bien_phylo,
                                      for_merge)

big_compadre_tree <- drop.tip(big_tree,
                              setdiff(big_tree$tip.label,
                                      for_merge))


# Save everything
saveRDS(big_tree,
        file = "Data/Serialized/Phylogenies/all_compadre_bien_fuzzy_matched.rds")

saveRDS(conservative_compadre_phylo,
        file = "Data/Serialized/Phylogenies/molecular_compadre_tree.rds")

saveRDS(big_compadre_tree,
        file = "Data/Serialized/Phylogenies/all_compadre_fuzzy_matched.rds")

saveRDS(conservative_bien_phylo,
        file = 'Data/Serialized/Phylogenies/molecular_bien_tree.rds')

# Done for now. Add more if you want to see more of what BIEN has to offer
