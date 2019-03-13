library(readxl)
library(dplyr)
library(tidyr)
library(fs)
library(purrr)
library(rlang)

# 1. get rows of separate matrices
# 2. get matrices matA, matU, matF, matC
# 3. get metadata
# 4. get matrixClass
# 5. Splice it all together
# 6. output a compadre-like object

# prove species name (SpeciesAuthor)

files <- dir_ls('stash', glob = '*.xlsx')

source('R/subset_orchids.R')

metadata <- orchids$metadata %>%
  map_if(~is.factor(.x), ~as.character(.x)) %>%
  as_tibble()
all_mats <- orchids$mat
all_mat_class <- orchids$matrixClass

# Fix a couple data type things
metadata$YearPublication <- as.numeric(metadata$YearPublication)


it <- 1

n_start <- length(all_mats)

for(i in files) {
  # raw matrix (these excel files should be in "Data" somewhere)
  raw_mat <- read_xlsx(i,
                       sheet='MatrixStacked')
  raw_spp <- read_xlsx(i,
                       sheet='SpeciesDescriptors')

  # get rows of separate matrices ---------------------------------

  # get indices of first row
  first_row <- setdiff( c(1:nrow(raw_mat)),
                        which(raw_mat$EnteredBy == 'NA' |
                                grepl('end|End', raw_mat$EnteredBy)) )

  # matrix dimension
  if( is.na(first_row[2]) ){
    # if we only have 1 matrix
    mat_dim <- nrow(raw_mat)
  }else{
    mat_dim   <- (first_row[2]-1)
  }

  # indices raws associated w/ separate matrices
  mat_r_ids  <- lapply(first_row, function(x, add_rows) x:(x+add_rows),
                       mat_dim-1)


  # get matrices matA, matU, matF, matC -------------------------------

  # where do matrices start in the spreadsheet?
  first_col <- which(names(raw_mat) == 'MatrixClassNumber') + 1

  # indices to add to get to "next first column"
  add_cols  <- mat_dim+1

  # indices for columns associatedc w/ matA, matU, matF, and matC -----
  first_cols<- first_col + c(0, add_cols, add_cols*2, add_cols*3)

  # indices raws associated w/ separate matrices
  mat_c_ids  <- lapply(first_cols, function(x, add_cols) x:(x+add_cols),
                       mat_dim-1) %>%
    # precise
    setNames( c('matA', 'matU', 'matF', 'matC') )

  # get matrices
  get_mats <- function(rows_ids, mat_c_ids, raw_mat){

    list( matA = raw_mat[rows_ids,mat_c_ids$matA],
          matU = raw_mat[rows_ids,mat_c_ids$matU],
          matF = raw_mat[rows_ids,mat_c_ids$matF],
          matC = raw_mat[rows_ids,mat_c_ids$matC] )

  }

  # matrices in a list
  mat_list <- lapply(mat_r_ids, get_mats, mat_c_ids, raw_mat)

  # get metadata -----------------------------------------------------

  # load compadre database for reference

  # data we do not have in the excel file
  cmp_nams <- names(orchids$metadata)
  raw_nams <- c(raw_spp %>% names,
                raw_mat %>% names)
  miss_nam <- setdiff(cmp_nams, raw_nams)

  # first line of metadata for matrices and species
  meta_m_id<- which((raw_mat %>% names) %in% cmp_nams)
  meta_s_id<- which((raw_spp %>% names) %in% cmp_nams)

  # metadata for matrices
  meta_mat  <- lapply(first_row,
                      function(x, raw_mat, meta_cols) raw_mat[x, meta_cols],
                      raw_mat,
                      meta_cols = meta_m_id) %>%
    bind_rows

  # metadata for species
  meta_spp  <- raw_spp[ ,meta_s_id] %>% uncount(nrow(meta_mat))

  # metadata in a data frame. NA's are typed for compatibility with the orchid$metadata
  # data frame (otherwise you get a type coercion error when bind_rows()'ing)
  meta_df   <- bind_cols(meta_mat, meta_spp) %>%
    mutate( Lat = NA_real_,
            Lon = NA_real_,
            MatrixDimension = NA_real_,
            SurvivalIssue   = NA_real_ ) %>%
    select( cmp_nams )


  # These all need to be recoded by hand unfortanutely. On the other hand,
  # Aldo's data are formatted a bit differently. I think this should work for
  # either setup

  if(any(grepl('end|End', meta_df$SpeciesAuthor))) {
    meta_df <- meta_df[!grepl('end|End', meta_df$SpeciesAuthor), ]
  }

  if(is.numeric(meta_df$AnnualPeriodicity)) {
    meta_df$AnnualPeriodicity <- as.character(meta_df$AnnualPeriodicity)
  }

  if(is.character(meta_df$Altitude)) {
    meta_df$Altitude <- as.numeric(meta_df$Altitude)
  }

  if(is.character(meta_df$Altitude)) {
    meta_df$Altitude <- as.numeric(meta_df$Altitude)
  }

  if(is.logical(meta_df$AdditionalSource)) {
    meta_df$AdditionalSource <- as.character(meta_df$AdditionalSource)
  }


  if(any(!is.numeric(meta_df$MatrixStartYear),
         !is.numeric(meta_df$MatrixEndYear))) {
    meta_df$MatrixStartYear <- as.numeric(meta_df$MatrixStartYear)
    meta_df$MatrixEndYear <- as.numeric(meta_df$MatrixEndYear)
  }

  # get matrixClass -----------------------------------------------------

  # get ids of matrix class info
  m_class_id <- which(names(raw_mat) %in% names(compadre$matrixClass[[1]]) )

  # get matrix classes
  get_mat_class <- function(rows_ids, m_class_id, raw_mat){

    raw_mat[rows_ids, m_class_id]

  }

  # matrix class information in a list
  mat_class_l <- lapply(mat_r_ids,
                        get_mat_class,
                        m_class_id, raw_mat)


  # output a compadre-like object ---------------------------------------

  # compadre-like object

  # Make sure we aren't duplicating effort. This will just throw an error
  # and stop the whole process though so you need to figure out how to deal
  # with that if it comes up.

  stopifnot(! meta_df$SpeciesAuthor[1] %in% metadata$SpeciesAuthor)

  # combine existing metadata with species-specific info.
  metadata <- bind_rows(metadata, meta_df)

  # This just appends the mat_list to the all_mats object, preserving the order
  # and the length. Otherwise, you'd end up with a mismatch between dim(metadata)[1]
  # and length(al_mats)/length(all_mat_class)

  all_mats[n_start:(n_start + length(mat_list))] <- dots_splice(!!! mat_list,
                                                                .homonyms = 'keep')
  all_mat_class[n_start:(n_start + length(mat_list))] <- dots_splice(!!! mat_class_l,
                                                                     .homonyms = 'keep')

  n_start <- length(all_mats)
}
# store compadre-like object

stopifnot(all.equal(length(all_mats), length(all_mat_class), dim(metadata)[1]))

# another sanity check: matrix dimensions should be equal in mat, matrixClass,
# and metadata.

test <- logical(length(all_mats))

for(i in seq_along(all_mats)) {
  mat_dim <- all_mats[[i]]$matA %>% dim
  mat_cl_dim <- all_mat_class[[i]] %>% dim
  mat_met_dim <- metadata$MatrixDimension[i]

  test[i] <- all.equal(mat_dim[1], mat_cl_dim[1], mat_met_dim)

}

stopifnot(sum(test) == dim(metadata)[1])

out <- list(metadata = metadata,
            matrixClass = all_mat_class,
            mat = all_mats)



saveRDS(out,
        'Data/Serialized/Compadre/compadre_plus_new_studies.rds')
