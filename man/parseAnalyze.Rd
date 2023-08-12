\name{parseAnalyze}

\alias{parseAnalyze}
\alias{writeAnalyze}

\title{Read or Write Analyze 7.5 Files}

\description{
    Analyze 7.5 is a format originally designed for magnetic resonance imaging (MRI), but is also used for mass spectrometry (MS) imaging.
}

\usage{
parseAnalyze(file, \dots)

writeAnalyze(object, file, type = "float32", mz, \dots)
}

\arguments{
    \item{file}{The file path to either of the ".hdr" or ".img" files.}

    \item{\dots}{Not currently used.}

    \item{object}{Array-like data of at least 3 dimensions.}

    \item{type}{The data type using for writing the ".img" file. Allowed values are "int16", "int32", "float32", and "float64".}

    \item{mz}{An optional numeric vector of m/z-values.}
}

\details{
    Because the Analyze 7.5 is originally intended for MRI, it stores the complete data cube as an N-dimensional array. For MRI data, there are typically 4 dimensions. For MS imaging data, there are typically 3 dimensions, where the first dimension is the m/z value axis, and the other two dimensions are spatial. If a ".t2m" file is present, it will be parsed as the m/z values.
}

\value{
	An object of class \code{analyze75}, which is a list with components named \code{hdr}, \code{img}, and (if appropriate) \code{mz}.
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
writeAnalyze(intensity, path, type="float32", mz=mz)
}

\keyword{IO}
\keyword{file}
