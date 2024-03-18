\name{writeAnalyze}

\alias{writeAnalyze}
\alias{writeAnalyze,array-method}
\alias{writeAnalyze,matter_arr-method}
\alias{writeAnalyze,sparse_arr-method}

\title{Write an Analyze 7.5 File}

\description{
    Write an Analyze 7.5 file from a N-dimensional array or a matrix with corresponding pixel/voxel positions.
}

\usage{
\S4method{writeAnalyze}{array}(object, file, positions = NULL, domain = NULL,
    type = "float32", \dots)

\S4method{writeAnalyze}{matter_arr}(object, file, positions = NULL, domain = NULL,
    type = "float32", \dots)

\S4method{writeAnalyze}{sparse_arr}(object, file, positions = NULL, domain = NULL,
    type = "float32", \dots)
}

\arguments{
    \item{object}{Array-like data of at least 3 dimensions, or matrix-like data with columns corresponding to rows in \code{positions}.}

    \item{file}{The file path to use for writing the ".img" and ".hdr" files.}

    \item{positions}{A data frame or matrix of pixel/voxel positions corresponding to the columns of \code{object}.}

    \item{domain}{An optional numeric vector of domain values (e.g., m/z-values).}

    \item{type}{The data type using for writing the ".img" file. Allowed values are "int16", "int32", "float32", and "float64".}

    \item{\dots}{Not currently used.}
}

\details{
    If \code{domain} is provided (e.g., for m/z-values), then a ".t2m" file will also be written.
}

\value{
    \code{TRUE} if the file was successfully written; \code{FALSE} otherwise. The output file paths are attached as attributes.
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
writeAnalyze(intensity, path, domain=mz, type="float32")

# parse it
parseAnalyze(path)
}

\keyword{IO}
\keyword{file}

