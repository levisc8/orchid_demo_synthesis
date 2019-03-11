# Complete pipeline for deriving demographics. To add additional matrices, use
# R/format_raw_matrix.R to update the the use_db created by matrix_query.R

source('R/subset_orchids.R')
source('R/derive_demographics.R') # NEEDS EDITING

source('R/LH_PCA.R') # WRITE ME

source('R/produce_figures.R')  # WRITE ME
