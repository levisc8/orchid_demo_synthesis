####Selecting individual matrices by hand because manual labour is nice sometimes, especially while coding :)

source('R/subset_orchids.R')

#####i used this to check for mean matrices and unmanipulated and what not. no means of means, and each population it's very own matrix
#####just like tiffany wanted it to be :)
#View(orchids$metadata[, c("SpeciesAuthor", "MatrixTreatment", "MatrixComposite", "MatrixStartYear", "MatrixEndYear", "MatrixPopulation","DOI.ISBN")])

single_ind <- c(1, 2, 5, 9, 13, 17, 21, 22, 23, 24, 25, 26, 28, 29, 30, 32, 33, 34, 35, 36, 37, 38, 42, 46, 48, 53, 64, 70, 77, 86, 95, 104, 110, 116, 122, 131, 140, 149, 150, 152, 153, 159, 164, 167, 168, 169, 171, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 189, 190, 191, 192, 193, 197, 204, 211, 218, 225, 232, 240, 241, 242, 245, 246, 247, 248, 249, 250, 251, 252, 255, 258, 259, 260, 261, 262, 263, 264)

orchids <- list(metadata    = orchids$metadata[single_ind,],
                matrixClass = orchids$matrixClass[single_ind],
                mat         = orchids$mat[single_ind])


### tadaa! orchids.
#### theres a function missing to remove all those matrices without a fecundity.
##### because they make the rest a bit annoying
