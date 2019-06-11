<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/globaltoolbox)](https://cran.r-project.org/package=globaltoolbox)
[![Build Status](https://travis-ci.org/HopkinsIDD/globaltoolbox.svg?branch=master)](https://travis-ci.org/HopkinsIDD/globaltoolbox)
[![Codecov test coverage](https://codecov.io/gh/HopkinsIDD/globaltoolbox/branch/master/graph/badge.svg)](https://codecov.io/gh/HopkinsIDD/globaltoolbox?branch=master)
<!-- badges: end -->

# globaltoolbox

## Installation

Install the globaltoolbox package from github:

```{r}
devtools::install_github("HopkinsIDD/globaltoolbox")
```

## Use

To use globaltoolbox, there are two steps:
 
1. Create the database (only required once)
1. Load the database (only required once per country)
1. Standardize a name using the loaded database


### Create

Before loading the database, please create it with the `create_database` function.

### Load

globaltoolbox provides a `load_gadm` function, which takes the ISO 3166-1 alpha 3 code of a country.  This will download all locations from GADM and add them to the database.

```{r}
globaltoolbox::load_gadm(countries = c('MWI','TZA'))
```

### Search

globaltoolbox provides a `telescoping_standardize` function, which takes your best guess at the name of a location, and returns the standardized name (if it can find it).Then you will need to build a database of countries using their ISO 3166 alpha-3 codes:


Once you have a database, you'll be able to look up location names

```{r}
globaltoolbox::telescoping_standardize(c("TZA::Balkh"))
```
