# Site selection
---

The repository **siteselection** contains R functions, scripts, and notebooks used for site selection for ecological studies. The material was originally developed for state-factor type (Amundson and Jenny 1997) approaches to selecting forest demography plots in Colorado's East River watershed, though the functions are generalizable to other domains. The repo is structured as an R package for portability. Reproducing the enclosed analyses is simply a matter of installing the package, providing Google Auth credentials to access publicly accessible input data, and running the notebooks. 

## Installing

**siteselection** can be installed by calling `remotes::install_github`:

```
remotes::install_github('hmworsham/siteselection')
```

## Directory tree

The repository has the following structure:

``` nohighlight
.
├── DESCRIPTION       <- package description auto-generated in Roxygen2
├── LICENSE           <- MIT license
├── LICENSE.md        <- MIT license details
├── Makefile          <- makefile with commands to ingest data and clean for analysis
├── NAMESPACE         <- imported and exported functions
├── R                 <- supporting functions imported in analysis notebooks
├── config            <- directory containing configuration files
│	└── config.yml    <- yaml file containing paths and variables used across the project
├── data              <- directory to store raw, intermediate, and processed data
│	├── intermediate  <- intermediate data that has been transformed
│	├── processed     <- final, canonical datasets for modeling or reporting
│	└── raw           <- original, immutable data dump
├── inst              <- directory of containing investigations and reportable output
│	├── examples      <- contains preliminary investigations
│	├── ms            <- .Rmd files for manuscripts and reports
│	└── notebooks     <- .R and .Rmd files with number-ordered, sequential analyses
├── logs              <- modeling and analysis logs
├── man               <- documentation for functions exported from ./R
├── models            <- trained and serialized models, model predictions, or summaries
├── tests             <- unit tests for functions in ./R
└── *.Rproj           <- .Rproj file for opening package in self-contained workspace
```

## Notebooks 
| Notebook                                          | Description  |
|:--------------------------------------------------|:-------------------------------------------------------------------|
| 01_create_rasters.R                               | TK  |
| 02.01_derive_topos_conifer_2022.R                 | TK  |
| 02.02_derive_topos_conifer_fullnetwork.r          | TK  |
| 02.03_derive_topos_aspen_2023.R                   | TK  |
| 02.04_derive_topos_sapflux_proposed_2023.R        | TK  |
| 03.01_plot_gradients_conifer_fullnetwork.R        | TK  |
| 03.02_plot_gradients_aspen_cored.R                | TK  |



## Functions

| `siteselection` function     | Source                       | Description                                               |
|:-----------------------------|:-----------------------------|:----------------------------------------------------------|
| `load.pkgs`                  | ./R/helpers.R                | Load list of packages specified in `config.yml`, installing first if necessary  |
| `makepolys`                  | ./R/helpers.R                | Build rectangular or circular polygons of specified radius around specified coordinates  |
| `print.figs`                 | ./R/helpers.R                | Generate and write figures showing site distribution along geophysical gradients  |

