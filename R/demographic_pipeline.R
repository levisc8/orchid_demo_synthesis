# Complete pipeline for deriving demographics. To add additional matrices, use
# R/format_raw_matrix.R to update the the use_db created by matrix_query.R

### if all new .xlsx files are stored
source('R/subset_orchids.R')       ## get Orchids from COMPADRE
source('R/format_raw_matrix.R')    ## add new Orchids from local files


### if only .rds object is available
# orchids <- readRDS('Data/Serialized/Compadre/compadre_plus_new_studies.rds')

### need to select appropriate matrices (means, single or multiple populations etc.)

source('R/derive_demographics.R') # NEEDS EDITING

source('R/LH_PCA.R') # WRITE ME

source('R/produce_figures.R')  # WRITE ME
