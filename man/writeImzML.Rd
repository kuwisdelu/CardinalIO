\name{writeImzML}

\alias{writeImzML}
\alias{writeImzML,ImzML-method}

\title{Write an imzML File}

\description{
    Write an imzML file with experimental and spectrum-level metadata.
}

\usage{
\S4method{writeImzML}{ImzML}(object, file, \dots)
}

\arguments{
    \item{object}{An object containing MS imaging metadata.}

    \item{file}{The file path to use for writing the ".imzML" file.}

    \item{\dots}{Not currently used.}
}

\details{
    The \code{ImzML} method writes \emph{only} the ".imzML" file, and \emph{not} the associated ".ibd" file. It performs only minimal checking that the required fields exist in the \code{ImzML} object. It does \emph{not} validate the XML mapping before writing.

    Other methods may behave differently (e.g., performing more validation or writing the ".ibd" file as well).
}

\value{
	\code{TRUE} if the file was successfully written; \code{FALSE} otherwise. This return value should be checked to make sure the operation completed, as most failure cases will yield warnings rather than errors.
}

\author{Kylie A. Bemis}

\examples{
# get the path to an example imzML file
path <- exampleImzMLFile()

# parse the file
p <- parseImzML(path)
print(p)

# write the file back out
tmp <- tempfile(fileext=".imzML")
writeImzML(p, tmp)
}

\keyword{IO}
\keyword{file}
