
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pcrecon: Principal Component Regression for tree ring data

## Motivation

## How to Cite

    #> 
    #> To cite package 'pcrecon' in publications use:
    #> 
    #>   Laura Hocking-Smith, Daniel Hocking and Nicholas Nagle (2020). pcrgecon
    #>   Princpal Component Regression for Dendroclimatology. R package
    #>   version 0.1.0.
    #> 
    #> A BibTeX entry for LaTeX users is
    #> 
    #>   @Manual{,
    #>     title = {pcrecon: Princpal Component Regression for Dendroclimatology},
    #>     author = {Laura Hocking-Smith and Daniel Hocking and Nicholas Nagle},
    #>     year = {2020},
    #>     note = {R package version 0.1.0},
    #>   }

## Installation

The most up-to-date version can be installed from the GitHub repository
using the `remotes` package. It can also be accomplished with the
`devtools` package but devtools got a bit bloated and the components
were split into individual packages. `remotes` is more streamlined if
not engaging in lots of full package development and just wanting to
install packages from online sources besides CRAN.

``` r
# install.packages("remotes")
library(remotes)
remotes::install_github("lgsmith295/pcrecon", build_vignettes = TRUE)
```

## Versioning

## How to Use

The vignette is the best place to start for understanding the use of the
functions in the package. The `reconstruction` vignette goes through all
the steps necessary to go from ITRDB data to a climate reconstruction
using tree ring data.

``` r
library(pcreg)
browseVignettes("pcreg")
vignette("reconstruction")
```

## Licenses

See the [DESCRIPTION](DESCRIPTION) and [LICENSE](LICENSE) files.

## Reporting Issues

Please report bugs to Github Issues
<https://github.com/lgsmith295/pcrecon/issues>. Search existing issues
before reporting to see if it’s already been reported and to check on
the current status. If reporting a new issue add the “bug” label. Try to
describe the problem in detail and include your OS and R
`sessionInfo()`. Bugs can be addressed most efficiently if you include a
reproducible example.

If there is something you would like to see added to the package, follow
the same instructions as above but use the “enhancement” label option. A
good description of the reasoning behind the feature request can be
helpful for prioritizing issues. If you have code to fix a bug or
provide and enhancement, please submit a GitHub pull request after
reading our contributor guidelines and code of conduct.

## Contributions

We welcome contributions from everyone. Before you get started, please
see our [contributor guidelines](CONTRIBUTING.md). Please note that this
project is released with a [Contributor Code of Conduct](CONDUCT.md). By
participating in this project you agree to abide by its terms.

## Acknowledgements

## References
