
Orchid Demography Synthesis
---------------------------

Central repository for the our synthesis on orchid demography. The organizational structure is fairly straightforward: *Data format/Data base/Name of stuff it cotains*. Data are split amongst serialized vs csv vs GIS files. Serialized folder is for very large data sets, csv's are mainly used to store subsets of those massive ones (e.g. subsetted for species in Compadre), and GIS files are mostly range maps. Within each of those, indicate the data source in the name of the subfolder. Then within each of those, indicate what each file contains.

Scripts should go in the `R/` folder. Right now, I haven't tried organizing those into an particular set up, but we should once the direction of the analysis becomes more clear.

Dependencies for the project thus far:

1.  `dplyr`

2.  `BIEN`

3.  `fs`

4.  `sf`

5.  `ape`

6.  `pez`

7.  `ggplot2`

8.  `ggtree`

9.  `maps`

10. `popbio`

11. `popdemo`

12. `purrr`

13. `rlang`

14. `readxl`

15. `glue`

16. `MASS`

17. `Matrix`

If you don't already have these, `install.packages()` them and then run each script!

### Note on ggtree

This is hosted on BioConductor and requires R 3.5 or greater. If you don't have that, either install it or avoid the *make\_phylogenies.R* file.

``` r
if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
}

BiocManager::install("ggtree", version = "3.8")
```
