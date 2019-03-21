# Complete pipeline for deriving demographics. To add additional matrices, use
# R/format_raw_matrix.R to update the the use_db created by matrix_query.R

source('R/dependencies.R')
source('R/subset_orchids.R')
source_w_path('R/format_raw_matrix.R', 'stash') # change "stash" to "wherever/you/stored/the/excel/files"
source('R/derive_demographics.R')
source('R/LH_PCA.R') # WRITE ME

source('R/produce_figures.R')  # Started


