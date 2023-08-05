
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

valid_terms <- function(terms, obo = c("ims", "ms", "uo"),
	check = c("any", "name", "accession"))
{
	index <- get_obo(match.arg(obo))
	test_id <- terms %in% index$id
	test_name <- terms %in% index$name
	test <- switch(match.arg(check),
		any=test_id | test_name,
		name=test_name,
		accession=test_id)
	setNames(test, terms)
}

find_terms <- function(pattern, obo = c("ims", "ms", "uo"),
	value = c("name", "accession"))
{
	value <- match.arg(value)
	index <- get_obo(match.arg(obo))
	terms <- grep(pattern, index$name, value=TRUE)
	switch(match.arg(value),
		name=terms, accession=setNames(names(terms), terms))
}

find_term <- function(term, obo = c("ims", "ms", "uo"),
	value = c("name", "accession"))
{
	obo <- match.arg(obo)
	index <- get_obo(obo)
	term <- index$name[pmatch(term, index$name)]
	if ( !all(valid_terms(term, obo)) )
		stop("could not resolve to a valid term ")
	if ( length(term) != 1L )
		stop("attempt to resolve more than one term")
	switch(match.arg(value),
		name=term, accession=setNames(names(term), term))
}

must_terms <- function(obo = c("ims", "ms"), value = c("name", "accession"))
{
	obo <- match.arg(obo)
	terms <- switch(obo,
		ims=.must_msi_terms,
		ms=.must_ms_terms)
	index <- get_obo(obo)
	switch(match.arg(value),
		name=index$name[terms],
		accession=index$id[terms])
}

.ontology_index <- new.env()

.must_msi_terms <- c(
	# file content
	fileContent = "IMS:1000003", # "ibd binary type"
	fileContent = "IMS:1000009", # "ibd checksum"
	fileContent = "IMS:1000008", # "ibd identification"
	# scan settings
	scanSettings = "IMS:1000040", # "linescan sequence" (previously "scan direction")
	scanSettings = "IMS:1000041", # "scan pattern"
	scanSettings = "IMS:1000048", # "scan type"
	scanSettings = "IMS:1000049", # "line scan direction"
	scanSettings = "IMS:1000042", # "max count of pixels x"
	scanSettings = "IMS:1000043", # "max count of pixels y"
	scanSettings = "IMS:1000044", # "max dimension x"
	scanSettings = "IMS:1000045", # "max dimension y"
	scanSettings = "IMS:1000046", # "pixel size (x)" (previously "pixel size")
	# scan
	scan = "IMS:1000050", # "position x"
	scan = "IMS:1000051", # "position y"
	# spectrum binary data array
	binaryArrayData = "IMS:1000101", # "external data"
	binaryArrayData = "IMS:1000102", # "external offset"
	binaryArrayData = "IMS:1000103", # "external array length"
	binaryArrayData = "IMS:1000104") # "external encoded length"

.must_ms_terms <- c(
	# file content
	fileContent = "MS:1000524", # "data file content"
	# contact
	contact = "MS:1000586", # "contact name"
	contact = "MS:1000590", # "contact affiliation" (previously "contact organization")
	# instrument configuration
	instrumentConfiguration = "MS:1000031", # "instrument model"
	# source
	source = "MS:1000008", # "ionization type"
	# analyzer
	analyzer = "MS:1000443", # "mass analyzer type"
	# detector
	detector = "MS:1000026", # "detector type"
	# software
	software = "MS:1000531", # "software"
	# data processing
	dataProcessing = "MS:1000452", # "data transformation"
	# spectrum
	spectrum = "MS:1000559", # "spectrum type"
	spectrum = "MS:1000525", # "spectrum representation"
	# scan list
	scanList = "MS:1000570", # "spectra combination"
	# binary data array
	binaryArrayData = "MS:1000513", # "binary data array"
	binaryArrayData = "MS:1000518", # "binary data type"
	binaryArrayData = "MS:1000572") # "binary data compression type"
