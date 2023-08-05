\name{get_obo}

\alias{get_obo}
\alias{valid_terms}
\alias{find_terms}
\alias{find_term}
\alias{must_terms}

\title{Mass spectrometry imaging ontology}

\description{
    These functions provide ways of getting and querying the ontologies necessary for imzML. Specifically, ontologies for mass spectrometry imaging ('ims'), mass spectrometry ('ms'), and units ('uo') are provided.
}

\usage{
get_obo(obo = c("ims", "ms", "uo"), \dots)

valid_terms(terms, obo = c("ims", "ms", "uo"),
    check = c("any", "name", "accession"))

find_terms(pattern, obo = c("ims", "ms", "uo"),
    value = c("name", "accession"))

find_term(term, obo = c("ims", "ms", "uo"),
    value = c("name", "accession"))

must_terms(obo = c("ims", "ms"),
    value = c("name", "accession"))
}

\arguments{
    \item{obo}{The ontology to get or use.}

    \item{\dots}{Additional arguments passed to \code{\link[ontologyIndex]{get_ontology}}) when first loading the ontology.}

    \item{check}{When validating terms, are they names ('name'), accession IDs ('accession') or either ('any')?}

    \item{value}{Should the term names ('name') or accession IDs ('accession') be returned?}
}

\details{
    \code{get_obo()} caches and returns the requested ontology.

    \code{find_term()} and \code{find_terms()} both query the specified ontology for the given term and return it if found. The former uses partial matching via \code{\link{pmatch}} and must unambiguously resolve to a single term. The latter uses \code{\link{grep}} and finds all potential matching terms.

    \code{must_terms()} returns a character vector of the minimal required terms that must be present in a valid imzML document. This is \emph{not} an extensive list of \code{"MUST"} terms, but only those that are CV parameters of required elements. These also do \code{not} fulfill minimum reporting guidelines for MS imaging experiments. They are only required to create a valid imzML file.
}

\value{
	For \code{get_obo()}, a \code{ontology_index} object.

    For \code{valid_terms()}, a logical vector indicating whether the corresponding terms are valid.

    For all others, a character vector of the requested terms.
}

\seealso{
    \code{\link[ontologyIndex]{get_ontology}}
}

\author{Kylie A. Bemis}

\examples{
# find position-related terms in imaging ontology
find_terms("position", "ims")

# find a specific term's accession ID
find_term("position x", "ims", value="accession")

# find all terms related to a vendor in MS ontology
find_terms("Bruker", "ms")
find_terms("Thermo", "ms")
}

\keyword{utilities}
