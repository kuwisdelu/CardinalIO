\name{parseImzML}

\alias{class:ImzML}
\alias{ImzML}
\alias{ImzML-class}

\alias{parseImzML}

\title{Parse an imzML File}

\description{
    Parse an imzML file for mass spectrometry (MS) imaging experiment metadata and spectrum-level metadata.
}

\usage{
parseImzML(file, ibd = FALSE, extra = NULL,
    extraArrays = NULL, check = ibd, \dots)
}

\arguments{
    \item{file}{The file path to the ".imzML" file.}

    \item{ibd}{Should the binary data file be attached?}

    \item{extra}{Additional cvParam or userParam tags to parse from spectrum and/or scan tags by their \emph{accession} or \emph{name} attributes.}

    \item{extraArrays}{Additional binary data arrays to parse based on identifying \emph{accession} or \emph{name} cvParam tags.}

    \item{check}{Should the UUID, checksum, and size of the binary data file be checked against the corresponding imzML tags and binary data array offsets? This can also be a character vector specifying any combination of "checksum", "uuid", and "filesize" to check.}

    \item{\dots}{Not currently used.}
}

\details{
    The parse imzML file is returned as a \code{ImzML} object, which is a list-like structure that can be travered via the standard \code{$}, \code{"["}, and \code{"[["} operators. Child nodes that contain cvParams and userParams will be \code{imzplist} objects which are also list-like structures that can be traversed the same way.

    The spectrum-level metadata is an exception and will be read in selectively and represented as \code{data.frame}s where each row contains the metadata for a specific spectrum. Metadata for \code{positions}, \code{mzArrays}, and \code{intensityArrays} will be parsed. These will be available in \code{$run$spectrumList}.
}

\value{
	An object of class \code{ImzML}.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\link{ImzMeta}},
    \code{\link{writeImzML}}
}

\examples{
# get the path to an example imzML file
path <- exampleImzMLFile()

# parse the file
p <- parseImzML(path, ibd=TRUE, extra=c(TIC="MS:1000285"))
print(p)

# get the spectra positions
p$run$spectrumList$positions

# get the TIC
p$run$spectrumList$extra

# get the m/z and intensity arrays
p$ibd$mz
p$ibd$intensity
}

\keyword{IO}
\keyword{file}
