\name{parseAnalyze}

\alias{class:Analyze75}
\alias{Analyze75}
\alias{Analyze75-class}

\alias{parseAnalyze}
\alias{writeAnalyze}
\alias{writeAnalyze,array-method}

\title{Read or Write Analyze 7.5 Files}

\description{
    Analyze 7.5 is a format originally designed for magnetic resonance imaging (MRI), but is also used for mass spectrometry (MS) imaging.
}

\usage{
parseAnalyze(file, \dots)

\S4method{writeAnalyze}{array}(object, file, type = "float32", domain, \dots)
}

\arguments{
    \item{file}{The file path to either of the ".hdr" or ".img" files.}

    \item{\dots}{Not currently used.}

    \item{object}{Array-like data of at least 3 dimensions.}

    \item{type}{The data type using for writing the ".img" file. Allowed values are "int16", "int32", "float32", and "float64".}

    \item{domain}{An optional numeric vector of domain values (e.g., m/z-values).}
}

\details{
    Because the Analyze 7.5 is originally intended for MRI, it stores the complete data cube as an N-dimensional array. For MRI data, there are typically 4 dimensions. For MS imaging data, there are typically 3 dimensions, where the first dimension is the m/z value axis, and the other two dimensions are spatial. If a ".t2m" file is present (storing the m/z-values for MS imaging data), then it will be parsed as well.
}

\value{
	An object of class \code{Analyze75}, which is a list with components named \code{hdr}, \code{img}, and (if appropriate) \code{t2m}.
}

\author{Kylie A. Bemis}


\examples{
# create a toy data cube
set.seed(2023)
nx <- 3
ny <- 3
nmz <- 500
mz <- seq(500, 510, length.out=nmz)
intensity <- replicate(nx * ny, rlnorm(nmz))
dim(intensity) <- c(nmz, nx, ny)
path <- tempfile(fileext=".hdr")

# write it in Analyze 7.5 format
writeAnalyze(intensity, path, type="float32", domain=mz)

# parse it
parseAnalyze(path)
}

\keyword{IO}
\keyword{file}

