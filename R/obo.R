
# load an ontology

get_obo <- function(obo = c("ims", "ms", "uo"), ...)
{
	obo <- match.arg(obo)
	if ( !obo %in% names(.ontology_index) ) {
		file <- switch(obo,
			ims=system.file("obo/imagingMS.obo", package="CardinalIO"),
			ms=system.file("obo/psi-ms.obo", package="CardinalIO"),
			uo=system.file("obo/uo.obo", package="CardinalIO"))
		.ontology_index[[obo]] <- get_ontology(file, ...)
	}
	.ontology_index[[obo]]
}

valid_terms <- function(terms, obo = c("ims", "ms", "uo"), ...)
{
	obo <- get_obo(match.arg(obo))
	test_id <- terms %in% obo$id
	test_name <- terms %in% obo$name
	test <- test_id | test_name
	setNames(test, terms)
}

find_terms <- function(pattern, obo = c("ims", "ms", "uo"), ...)
{
	obo <- get_obo(match.arg(obo))
	grep(pattern, obo$name, value=TRUE)
}

find_term <- function(term, obo = c("ims", "ms", "uo"), ...)
{
	obo <- get_obo(match.arg(obo))
	obo$name[pmatch(term, obo$name)]
}

.ontology_index <- new.env()

.must_msi_terms <- c(
	# file content
	"IMS:1000003" = "ibd binary type",
	"IMS:1000009" = "ibd checksum",
	"IMS:1000008" = "ibd identification",
	# scan settings
	"IMS:1000040" = "scan direction",
	"IMS:1000041" = "scan pattern",
	"IMS:1000048" = "scan type",
	"IMS:1000049" = "line scan direction",
	"IMS:1000042" = "max count of pixels x",
	"IMS:1000043" = "max count of pixels y",
	# scan
	"IMS:1000050" = "position x",
	"IMS:1000051" = "position y",
	# spectrum binary data array
	"IMS:1000101" = "external data",
	"IMS:1000102" = "external offset",
	"IMS:1000103" = "external array length",
	"IMS:1000104" = "external encoded length")

.must_ms_terms <- c(
	# file content
	"MS:1000524" = "data file content",
	# contact
	"MS:1000586" = "contact name",
	"MS:1000590" = "contact organization",
	# instrument configuration
	"MS:1000031" = "instrument model",
	# source
	"MS:1000008" = "ionization type",
	# analyzer
	"MS:1000443" = "mass analyzer type",
	# detector
	"MS:1000026" = "detector type",
	# software
	"MS:1000531" = "software",
	# data processing
	"MS:1000452" = "data transformation",
	# spectrum
	"MS:1000559" = "spectrum type",
	"MS:1000525" = "spectrum representation",
	# scan list
	"MS:1000570" = "spectra combination",
	# binary data array
	"MS:1000513" = "binary data array",
	"MS:1000518" = "binary data type",
	"MS:1000572" = "binary data compression type")
