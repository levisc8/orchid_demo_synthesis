# Demographic data cleaning/processing script

compadre <- readRDS('Data/Serialized/Compadre/Compadre_unrel_v4.rds')

sub_ind <- which(compadre$metadata$Family == 'Orchidaceae')

orchids <- list(metadata = compadre$metadata[sub_ind, ],
                matrixClass = compadre$matrixClass[sub_ind],
                mat = compadre$mat[sub_ind])

saveRDS(orchids, file = 'Data/Serialized/Compadre/all_orchids.rds')
