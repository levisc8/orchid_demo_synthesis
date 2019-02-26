# Visualize phylogenies

library(ggtree)
library(ggplot2)
library(dplyr)
library(ape)

# Visualize our coverage of the orchids

fuzz_tree <- readRDS('Data/Serialized/Phylogenies/all_compadre_bien_fuzzy_matched.RDS')

mol_tree <- readRDS('Data/Serialized/Phylogenies/molecular_bien_tree.rds')

Compadre <- readRDS('Data/Serialized/Compadre/Compadre_unrel_v4.rds')

all_orc_occ <- readRDS('Data/Serialized/BIEN/Occurences.rds')

all_orc_names <- unique(all_orc_occ$scrubbed_species_binomial) %>%
  gsub(' ', '_', .)

orc_compadre <- grepl('Orchidaceae', Compadre$metadata$Family)

orc_compadre_spp <- unique(Compadre$metadata$SpeciesAccepted[orc_compadre]) %>%
  gsub(' ', '_', .)

fuzz_tree <- drop.tip(fuzz_tree, setdiff(fuzz_tree$tip.label, all_orc_names))
mol_tree <- drop.tip(mol_tree, setdiff(mol_tree$tip.label, all_orc_names))

fuzz_groups <- split(fuzz_tree$tip.label, fuzz_tree$tip.label %in% orc_compadre_spp)
mol_groups <- split(mol_tree$tip.label, mol_tree$tip.label %in% orc_compadre_spp)

fuzz_tree <- groupOTU(fuzz_tree, fuzz_groups)
mol_tree <- groupOTU(mol_tree, mol_groups)

fuzz_tree$tip.label[!fuzz_tree$tip.label %in% orc_compadre_spp] <- ""
mol_tree$tip.label[!mol_tree$tip.label %in% orc_compadre_spp] <- ""


fuzz_plot <- ggtree(fuzz_tree,
                    aes(color = group),
                    layout = 'circular') +
  geom_tippoint(aes(color = group)) +
  scale_color_manual('',
                     breaks = c(TRUE, FALSE),
                     labels = c("In Compadre",
                                "Not in Compadre"),
                     values = c('red', 'black')) +
  theme(legend.position = 'right') +
  ggtitle("Fuzzily matched, not completely molecular tree")

fuzz_plot

ggsave("Fuzzy_matched_Compadre_coverage.pdf",
       path = "Figures/Phylogenies/",
       height = 8,
       width = 8,
       units = "in",
       dpi = 400)


mol_plot <- ggtree(mol_tree,
                    aes(color = group),
                    layout = 'circular') +
  geom_tippoint(aes(color = group)) +
  scale_color_manual('',
                     breaks = c(TRUE, FALSE),
                     labels = c("In Compadre",
                                "Not in Compadre"),
                     values = c('red', 'black')) +
  theme(legend.position = 'right') +
  ggtitle("strictly matched, molecular tree")

mol_plot

ggsave("Strictly_matched_Compadre_coverage.pdf",
       path = "Figures/Phylogenies/",
       height = 8,
       width = 8,
       units = "in",
       dpi = 400)
