# CardinalIO

## Mass spectrometry imaging i/o

This package provides fast and efficient parsing, reading, and writing of imzML files for mass spectrometry imaging experiments.

## Installation

### Bioconductor Release

*CardinalIO* can be installed via the *BiocManager* package.

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

### Bioconductor Devel

The Bioconductor development version of *CardinalIO* can also be installed via the *BiocManager* package.

```{r install, eval=FALSE}
BiocManager::install("CardinalIO", version="devel")
```

This version is unstable and should not be relied on for critical work. However, it is typically more stable than Github version.

### Github Devel

The most cutting edge version of *CardinalIO* can be installed from Github via the *remotes* package.

```{r install, eval=FALSE}
if (!require("remotes", quietly = TRUE))
    install.packages("remotes")

remotes::install_github("kuwisdelu/CardinalIO")
```

This version is unstable and only recommended for developers.



