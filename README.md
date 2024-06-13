# CardinalIO

## Mass spectrometry imaging i/o

This package provides fast and efficient parsing, reading, and writing of imzML files for mass spectrometry imaging experiments.

## User Installation

### Bioconductor Release

*CardinalIO* can be installed via the *BiocManager* package.

This is the **recommended** installation method.

```{r install, eval=FALSE}
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("CardinalIO")
```

The same function can be used to update *CardinalIO* and other Bioconductor packages.

Once installed, *CardinalIO* can be loaded with `library()`:

```{r library, eval=FALSE}
library(CardinalIO)
```

### Github Release

*CardinalIO* can also be installed via the *remotes* package.

```{r install, eval=FALSE}
if (!require("remotes", quietly = TRUE))
    install.packages("remotes")

remotes::install_github("kuwisdelu/CardinalIO", ref=remotes::github_release())
```

Previous releases can be installed by specifying the exact version.

```{r library, eval=FALSE}
remotes::install_github("kuwisdelu/CardinalIO@v1.2.1")
```

## Developer Installation

### Bioconductor Devel

The Bioconductor development version of *CardinalIO* can also be installed via the *BiocManager* package.

```{r install, eval=FALSE}
BiocManager::install("CardinalIO", version="devel")
```

This version is **unstable** and should not be used for critical work. However, it is typically more stable than Github devel.

This version should *typically* pass `R CMD check` without errors.

### Github Devel

The most cutting edge version of *CardinalIO* can be installed from Github via the *remotes* package.

```{r install, eval=FALSE}
if (!require("remotes", quietly = TRUE))
    install.packages("remotes")

remotes::install_github("kuwisdelu/CardinalIO")
```

This version is **unstable** and only recommended for developers. It should not be used for critical work.


