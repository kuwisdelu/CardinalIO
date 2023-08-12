\name{exampleImzMLFile}

\alias{exampleImzMLFile}

\title{Example imzML files}

\description{
    Get a local file path to an example imzML file originally downloaded from \url{https://ms-imaging.org/imzml/example-files-test/}.
}

\usage{
exampleImzMLFile(type = c("continuous", "processed"))
}

\arguments{
    \item{type}{The type of example imzML file path to return.}
}

\value{
	A string giving the local file path.
}

\author{Kylie A. Bemis}

\seealso{
    \code{\link{parseImzML}}
}

\examples{
# get the path to an example imzML file
path <- exampleImzMLFile("processed")

# parse the file
p <- parseImzML(path)
print(p)
}

\keyword{file}
