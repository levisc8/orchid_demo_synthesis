# Demographic data cleaning/processing script

compadre <- readRDS('Data/Serialized/Compadre/compadre_plus_new_studies.rds')

sub_ind <- which(compadre$metadata$Family == 'Orchidaceae')

orchids <- list(metadata = compadre$metadata[sub_ind, ],
                matrixClass = compadre$matrixClass[sub_ind],
                mat = compadre$mat[sub_ind])

saveRDS(orchids, file = 'Data/Serialized/Compadre/all_orchids.rds')
