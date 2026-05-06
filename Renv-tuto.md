# How to save its renv
## Initialization

```{r}
install.packages("renv")
renv::init()
```

## Code
Install your packages and run your code as always.

## Save Renv
```{r}
renv::snapshot()
```

## Check if Renv is up-to-date
```{r}
renv::status()
```

## Load renv to this session
```{r}
renv::restore()
```