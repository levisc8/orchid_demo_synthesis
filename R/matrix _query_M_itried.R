source('R/subset_orchids.R')

sub_ind_unmanipulated <- which(orchids$metadata$MatrixTreatment == 'Unmanipulated')

orchids_unmanipulated <- list(metadata = orchids$metadata[sub_ind_unmanipulated, ],
                matrixClass = orchids$matrixClass[sub_ind_unmanipulated],
                mat = orchids$mat[sub_ind_unmanipulated])


sub_ind_mean_pooled <- c(which(orchids_unmanipulated$metadata$MatrixComposite == "Mean"), which(orchids_unmanipulated$metadata$MatrixComposite == "Pooled"))

orchids_mean_pooled <- list(metadata = orchids_unmanipulated$metadata[sub_ind_mean_pooled, ],
                 matrixClass = orchids_unmanipulated$matrixClass[sub_ind_mean_pooled],
                 mat = orchids_unmanipulated$mat[sub_ind_mean_pooled])

# Selecting only matrices that are means of multiple populations
orchids_mean_pooled$metadata[orchids_mean_pooled$metadata$MatrixPopulation %>% grep(";",.),]$MatrixPopulation

saveRDS(orchids_mean_pooled, file = 'Data/Serialized/Compadre/many_orchids.rds')






(orchidsi$mat[[1]]$matA + orchidsi$mat[[2]]$matA + orchidsi$mat[[3]]$matA)/3

print(length(orchids$metadata$SpeciesAuthor[spp_i]))

mean_mat_l <- list()

# cycle through the species
for(spp_i in 1:length(orchids$metadata$SpeciesAuthor)){

  # the id of each species specific matrices
  id_spp <- sum(orchidsi$metadata$SpeciesAuthor == orchidsi$metadata$SpeciesAuthor[spp_i])

  # set up matrix to get the mean
  # start with the first matrix
  mat_sum <- orchidsi$mat[[id_spp[1]]]$matA

  # add matrices up
  for(mat_i in 2:id_spp){

    mat_sum <- mat_sum  +  orchidsi$mat[[id_spp[[mat_i]]]]$matA

  }

  mean_mat_l[[spp_i]] <- mat_sum / length(spp_i)
}

orchidsi$mat[[id_spp[1]]]$matA

orchidsi$mat[[1[[2]]]]$matA


#kj




sub_ind_i <- which(orchids_unmanipulated$metadata$MatrixComposite == "Individual")

orchidsi <- list(metadata = orchids_unmanipulated$metadata[sub_ind_i, ],
                 matrixClass = orchids_unmanipulated$matrixClass[sub_ind_i],
                 mat = orchids_unmanipulated$mat[sub_ind_i])


source_fam <- function(fam){
  subset(orchidsi$metadata, grepl(fam, Family) ) %>%
    select(Authors, YearPublication, Journal, SpeciesAuthor, NumberPopulations, DOI.ISBN) %>%
    unique
}

source_ver <- function(versuch){
  subset(orchidsi$metadata, grepl(versuch, Kingdom) ) %>%
    select(Authors, YearPublication, Journal, SpeciesAuthor, MatrixComposite, NumberPopulations, DOI.ISBN) %>% unique
}

write.csv2(source_ver("Plant"), "D:/Orchidsx.csv")

write.csv2(source_fam("Orchidaceae"), "D:/Orchidsi.csv")

print(orchidsi$metadata$Authors)

