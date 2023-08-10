\name{get_obo}

\alias{get_obo}
\alias{valid_terms}
\alias{find_terms}
\alias{find_term}
\alias{find_descendants_in}

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

find_descendants_in(list, terms, obo = c("ims", "ms", "uo"))
}

\arguments{
    \item{obo}{The ontology to get or use.}

    \item{terms}{One or more ontology terms (either names or accessions) to check for validity in the ontology.}

    \item{pattern}{The regular expression pattern to search in the ontology.}

    \item{term}{An ontology term to partially match (by name, \emph{not} accession).}

    \item{\dots}{Additional arguments passed to \code{\link[ontologyIndex]{get_ontology}}) when first loading the ontology.}

    \item{check}{When validating terms, are they names ('name'), accession IDs ('accession') or either ('any')?}

    \item{value}{Should the term names ('name') or accession IDs ('accession') be returned?}

    \item{list}{A named list where the names are accession IDs.}
}

\details{
    \code{get_obo()} caches and returns the requested ontology.

    \code{find_term()} and \code{find_terms()} both query the specified ontology for the given term and return it if found. The former uses partial matching via \code{\link{pmatch}} and must unambiguously resolve to a single term. The latter uses \code{\link{grep}} and finds all potential matching terms.

    \code{find_descendants_in()} finds descendants of particular terms in a named list where the names are accession IDs. It returns the list subsetted to matching descendants.
}

\value{
	For \code{get_obo()}, a \code{ontology_index} object.

    For \code{valid_terms()}, a logical vector indicating whether the corresponding terms are valid.

    For \code{find_descendants_in()}, a subset of the original list.

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
