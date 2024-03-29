# siteselection

The repository **siteselection** contains R functions, scripts, and notebooks used for site selection for ecological studies. The material was originally developed for state-factor type (Amundson and Jenny 1997) approaches to choosing forest demography plots in Colorado's East River watershed, though the functions are generalizable to other domains. The repo is structured as an R package for portability. Reproducing the enclosed analyses is simply a matter of installing the package, providing Google Auth credentials to access publicly accessible input data, and running the notebooks. 

## Installing

**siteselection** can be installed by calling `remotes::install_github`:

```
remotes::install_github('hmworsham/siteselection')
```

## Reproducing analyses

1. Optionally, open `siteselection.Rproj` in RStudio to spin up a contained workspace.
2. Run `make ./Makefile` to ingest and clean data. Runs `./inst/notebooks/01_create_rasters`. 
3. Enter Google Auth credentials when prompted. See [`drive_auth()`](https://googledrive.tidyverse.org/reference/drive_auth.html) documentation for details. 
4. Run `R CMD BATCH ./inst/notebooks/<notebook.R>.` or, optionally, open notebooks from the directory in RStudio to run interactively. 

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
| 03.03_plot_gradients_flightpaths.R                | TK  |

## Functions

| `siteselection` function     | Source                       | Description                                               |
|:-----------------------------|:-----------------------------|:----------------------------------------------------------|
| `load.pkgs`                  | ./R/helpers.R                | Load list of packages specified in `config.yml`, installing first if necessary  |
| `makepolys`                  | ./R/helpers.R                | Build rectangular or circular polygons of specified radius around specified coordinates  |
| `print.figs`                 | ./R/helpers.R                | Generate and write figures showing site distribution along geophysical gradients  |

## Getting help
Bugs: Users are encouraged to report bugs directly in GitHub. Select **Issues** in the menu above, and create a **New issue** to start a new bug report, documentation correction, or feature request. Questions to worsham@berkeley.edu.

