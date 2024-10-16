\name{writeImzML}

\alias{writeImzML}
\alias{writeImzML,ImzML-method}
\alias{writeImzML,ImzMeta-method}

\title{Write an imzML File}

\description{
    Write an imzML file with experimental and spectrum-level metadata.
}

\usage{
\S4method{writeImzML}{ImzML}(object, file, positions = NULL, mz = NULL, intensity = NULL,
    mz.type = "float64", intensity.type = "float32", asis = FALSE, \dots,
    BPPARAM = bpparam())

\S4method{writeImzML}{ImzMeta}(object, file, positions, mz, intensity, \dots,
    BPPARAM = bpparam())
}

\arguments{
    \item{object}{An object containing MS imaging metadata.}

    \item{file}{The file path to use for writing the ".imzML" file.}

    \item{positions}{A data frame or matrix of raster positions where the mass spectra were collected. Replaces any existing positions in \code{object}.}

    \item{mz}{A numeric vector (for "continuous" format) or list of such vectors (for "processed" format) giving the m/z-values of the mass spectra. Used to write the ".ibd" file if provided.}

    \item{intensity}{A numeric matrix (for "continuous" format) or list of numeric vectors (for "processed" format) giving the intensity values of the mass spectra. Used to write the ".ibd" file if provided.}

    \item{mz.type, intensity.type}{The data types for writing the respective arrays to the ".ibd" file. Allowed types are "int32", "int64", "float32", and "float64".}

    \item{asis}{If \code{TRUE} and \code{mz} and \code{intensity} are both file-backed \code{\link{matter}} objects, then they are only used to infer the binary metadata for writing the ".imzML" file, and the ".ibd" is \emph{not} written.}

    \item{\dots}{Additional arguments passed to \code{\link{chunk_colapply}} or \code{\link{chunk_mapply}}.}

    \item{BPPARAM}{An optional instance of \code{BiocParallelParam}. See documentation for \code{\link{bplapply}}.}
}

\details{
    The \code{ImzML} method writes the ".imzML" file based on the provided \code{ImzML} object. If \code{mz} and \code{intensity} are \emph{both} provided, then it \emph{also} writes the associated ".ibd" file. It performs only minimal checking that the required tags exist in the \code{ImzML} object. It does \emph{not} validate the XML mapping before writing.

    The \code{ImzMeta} method requires all of \code{positions}, \code{mz}, and \code{intensity} to write the files.
}

\value{
	\code{TRUE} if the file was successfully written; \code{FALSE} otherwise. This return value should be checked to make sure the operation completed, as most failure cases will yield warnings rather than errors. The output file paths are attached as attributes.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\link{ImzMeta}},
    \code{\link{parseImzML}}
}

\examples{
# get the path to an example imzML file
path <- exampleImzMLFile()

# parse the file
p <- parseImzML(path, ibd=TRUE)
print(p)

# get the spectra and positions
mz <- as.list(p$ibd$mz)
intensity <- as.list(p$ibd$intensity)
positions <- p$run$spectrumList$positions

# write the file back out
path2 <- tempfile(fileext=".imzML")
writeImzML(p, path2, positions=positions,
    mz=mz, intensity=intensity)
}

\keyword{IO}
\keyword{file}
