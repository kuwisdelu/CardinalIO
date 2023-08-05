\name{parseImzML}

\alias{class:ImzML}
\alias{ImzML}
\alias{ImzML-class}

\alias{parseImzML}
\alias{exampleImzMLPath}

\title{Parse an imzML File}

\description{
    Parse an imzML file for experimental and spectrum-level metadata.
}

\usage{
parseImzML(file, \dots)

exampleImzMLPath(type = c("continuous", "processed"))
}

\arguments{
    \item{file}{The file path to the ".imzML" file.}

    \item{\dots}{Not currently used.}

    \item{type}{The type of example imzML file path to return.}
}

\details{
    The parse imzML file is returned as a \code{ImzML} object, which is a list-like structure that can be travered via the standard \code{$}, \code{"["}, and \code{"[["} operators. Child nodes that contain cvParams and userParams will be \code{imzplist} objects which are also list-like structures that can be traversed the same way.

    The spectrum-level metadata is an exception and will be read in selectively and represented as \code{data.frame}s where each row contains the metadata for a specific spectrum. Metadata for \code{"positions"}, \code{"mzArrays"}, and \code{"intensityArrays"} will be parsed. These will be available in \code{$run$spectrumList}.

    \code{exampleImzMLPath()} will return the local path to the tiny test imzML files originally downloaded from \url{https://ms-imaging.org/imzml/example-files-test/}.
}

\value{
	An object of class \code{ImzML}.
}

\author{Kylie A. Bemis}

\examples{
# get the path to an example imzML file
path <- exampleImzMLPath()

# parse the file
p <- parseImzML(path)
print(p)

# get the spectra positions
p$run$spectrumList$positions
}

\keyword{IO}
\keyword{file}
