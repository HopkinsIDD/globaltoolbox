# globaltoolbox

## Installation

Install the globaltoolbox package from github:

```{r}
devtools::install_github("HopkinsIDD/globaltoolbox")
```
Then you will need to build a database of countries using their ISO 3166 alpha-3 codes:

```{r}
globaltoolbox::load_gadm(countries = c('MWI','TZA'))
```

Once you have a database, you'll be able to look up location names

```{r}
globaltoolbox::telescoping_standardize(c("TZA::Balkh"))
```
