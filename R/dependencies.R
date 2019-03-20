# Required packages -------------

pkgs <- c(
  "MASS",
  "Matrix",
  "dplyr",
  "tidyr",
  "fs",
  "sf",
  "ape",
  "pez",
  "ggplot2",
  "maps",
  "popbio",
  "popdemo",
  "purrr",
  "rlang",
  "readxl",
  'glue'
)

vapply(pkgs,
       FUN = function(x) library(x,
                                 character.only = TRUE,
                                 logical.return = TRUE),
       FUN.VALUE = logical(1))


# Optional packages -----------------
opt_pkgs <- c('ggtree', 'BIEN')

vapply(opt_pkgs,
       FUN = function(x) try({
         library(x,
                 character.only = TRUE,
                 logical.return = TRUE)
       }),
       FUN.VALUE = logical(1))


# Functions ------------

# This is necessary because the Excel sheets containing the newly digitized studies
# aren't stored on Github, so you'd need to change the path based on how you've
# stored the data. This allows you to only change it once at the top level but still
# access all the values obtained from source()'ing the format_raw_matrix script.

################ arguments #######################
### script_path: the path to the script you want to source()
### raw_data_path: the path to the folder with the Excel sheets. Only put the
###                folder path, not the individual files within it!!!!

source_w_path <- function(script_path, raw_data_path) {

  eval_env <- rlang::child_env(.parent = .GlobalEnv)

  eval_env$data_path <- raw_data_path

  source('R/format_raw_matrix.R', local = eval_env)

  out_list <- rlang::env_get_list(env = eval_env,
                                  nms = rlang::env_names(eval_env))

  rlang::env_bind(.GlobalEnv,
                  !!! out_list)

}

# Gets matrices out of a given species' excel sheet

get_mats <- function(row_ids, mat_c_ids, raw_mat, mat_dim){

  # because of the "end" entries, the matrix columns are character typed
  # instead of numeric. This piping sequence converts it to character matrix
  # then, a numeric vector, then back to a matrix. This sequence is needed
  # because we can't go straight from character to numeric matrix for reasons
  # that currently escape me. byrow = TRUE is not needed
  # as the vector is column major rather than row major.

  matA <- as.matrix(raw_mat[row_ids, mat_c_ids$matA]) %>%
    as.numeric() %>%
    matrix(nrow = mat_dim, ncol = mat_dim)

  matU <- as.matrix(raw_mat[row_ids, mat_c_ids$matU]) %>%
    as.numeric() %>%
    matrix(nrow = mat_dim, ncol = mat_dim)

  matF <- as.matrix(raw_mat[row_ids, mat_c_ids$matF]) %>%
    as.numeric() %>%
    matrix(nrow = mat_dim, ncol = mat_dim)

  matC <- as.matrix(raw_mat[row_ids, mat_c_ids$matC]) %>%
    as.numeric() %>%
    matrix(nrow = mat_dim, ncol = mat_dim)

  out <- list(matA = matA,
              matU = matU,
              matF = matF,
              matC = matC)

  return(out)

}
