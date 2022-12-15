
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GuiRBioDiv

<!-- badges: start -->
<!-- badges: end -->

The goal of GuiRBioDiv is to provide some wrapper functions to speed-up
alpha diversity analyses of metabarcoding datasets.

It features:

- `alpha_residual` for correction of sequencing depth bias for alpha
  diversity indices with linear models
- `endemic` to identify species/OTUs/ASVs uniquely found in one
  geographic region
- `transformTukey2` to transform numeric vectors to more normally
  distributed values with Tukey’s ladder of power method

## Installation

`GuiRBioDiv` can be installed from GitHub like so:

``` r
install.packages("remotes")
remotes::install_github("lentendu/GuiRBioDiv")
```

## Example

Compute Hill numbers and correct for sequencing depth bias:

``` r
library(GuiRBioDiv)
alphadiv<-alpha_residual(mat_exa, index="hill", inform=T)
#> [1] "index hill0 best linear correlation to counts is with no transformation: Adj. R2 = 0.49 ;  p.value = 1.46e-02"
#> [1] "index hill1 is not linearly correlated to counts, neither to log or sqrt transformed counts"
#> [1] "index hill2 is not linearly correlated to counts, neither to log or sqrt transformed counts"
```
