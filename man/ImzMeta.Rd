\name{ImzMeta-class}
\docType{class}

\alias{class:ImzMeta}
\alias{ImzMeta}
\alias{ImzMeta-class}

\alias{[[<-,ImzMeta-method}

\title{Mass spectrometry imaging experimental metadata}

\description{
    The \code{ImzMeta} class provides a simpler and more limited interface for tracking mass spectrometry (MS) imaging experimental metadata compared to a full \code{ImzML} instance as returned by \code{\link{parseImzML}}. It is a simple list of expected/required metadata tags that can be easily set by the user. Replacement methods support partial matching to identify the correct controlled-vocabulary parameter.
}

\usage{
## Instance creation
ImzMeta(\dots)
}

\arguments{
    \item{\dots}{Named metadata tags (in the form \code{name=value}, e.g., \code{spectrumRepresentation="profile specrum"}. Use \code{names(ImzMeta())} to see possible tags.}
}

\details{
    The \code{ImzMeta} class supports lossy conversion between itself and \code{ImzML} instances. Only the supported information is captured, so converting from \code{ImzML} and then back to \code{ImzML} will lose some information. It is primarily intended for ease of use when preparing the metadata from scratch and when a complete \code{ImzML} instance is not available at the time of writing the file.
}

\note{
    This class does \emph{not} currently meet minimum reporting guidelines for MS imaging experiments, as that is not its purpose. It is designed to provide the minimum required experimental metadata for writing a valid imzML file. For example, it does not currently support sample metadata, as this would require ontologies that are outside of the scope of the present package. This may be expanded in the future if the need arises.
}

\section{Methods}{
    Standard generic methods:
    \describe{
        \item{\code{x$name, x$name <- value}:}{Get or set a tag.}

        \item{\code{x[["name"]], x[["name"]] <- value}:}{Get or set a tag.}
    }
}

\value{
    An object of class \code{\linkS4class{ImzMeta}}.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\link{parseImzML}},
    \code{\link{writeImzML}}
}

\examples{
## create an empty ImzMeta instance
e <- ImzMeta()

## set some experimental metadata
e$spectrumType <- "MS1 spectrum"
e$spectrumRepresentation <- "profile spectrum"
e

# convert to ImzML instance
as(e, "ImzML")

# convert from a parsed imzML file
path <- exampleImzMLFile()
p <- parseImzML(path)
as(p, "ImzMeta")
}

\keyword{classes}
\keyword{IO}
