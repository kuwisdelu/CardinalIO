
# load an ontology

.ontology_index <- new.env()

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
	matches <- grep(pattern, index$name, ignore.case=TRUE, value=TRUE)
	switch(value, name=matches, accession=setNames(names(matches), matches))
}

find_term <- function(term, obo = c("ims", "ms", "uo"),
	value = c("name", "accession"))
{
	obo <- match.arg(obo)
	value <- match.arg(value)
	index <- get_obo(obo)
	possible <- sQuote(find_terms(term, obo, value=value))
	match <- index$name[pmatch(term, index$name)]
	if ( length(possible) == 0L )
		possible <- "[no matches]"
	possible <- paste0(possible, collapse=", ")
	if ( !all(valid_terms(match, obo)) )
		stop("could not resolve ", sQuote(term), " to a valid term;",
			" did you mean one of: ", possible, "?")
	if ( length(match) != 1L )
		stop("could not resolve ", sQuote(term), " to a single term;",
			" did you mean one of: ", possible, "?")
	switch(value, name=match, accession=setNames(names(match), match))
}

find_descendants_in <- function(list, terms, obo = c("ims", "ms", "uo"))
{
	obo <- match.arg(obo)
	ids <- get_descendants(get_obo(obo), terms)
	list[names(list) %in% ids]
}

