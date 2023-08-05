
#### ImzML class ####
## --------------------

setClass("ImzML", contains = "SimpleList")

# note: Does *_NOT_* validate the xml mapping
.valid_ImzML <- function(object)
{
	errors <- NULL
	required_mzML_elements <- c("fileDescription", "softwareList",
		"instrumentConfigurationList", "dataProcessingList", "run")
	missing_elts <- setdiff(required_mzML_elements, names(object))
	if ( length(missing_elts) > 0L )
		errors <- c(errors , paste0("required mzML elements are missing: ",
			paste0(sQuote(missing_elts), collapse=", ")))
	empty_elts <- names(object)[lengths(object) == 0L]
	if ( length(empty_elts) > 0L )
		errors <- c(errors , paste0("no mzML elements can be empty: ",
			paste0(sQuote(empty_elts), collapse=", ")))
	if ( is.null(errors) ) TRUE else errors
}

setValidity("ImzML", .valid_ImzML)

# note: don't export constructor (not to be instantiated directly)
.new_ImzML <- function(..., validate = TRUE)
{
	object <- SimpleList(...)
	object <- as(object, "ImzML")
	if ( validate )
		validObject(object)
	object
}

# show utility
.shorten_strings <- function(x, n = 12L)
{
	ifelse(nchar(x) > n, paste0(substr(x, 1, n), "..."), x)
}

# show utility
.show_list_names <- function(x, n = 12L)
{
	for ( i in seq_along(x) ) {
		len <- length(x[[i]])
		nms <- names(x[[i]])
		if ( len > n )
			nms <- paste0(head(nms, n=n), "...")
		nms <- paste0(nms, collapse=" ")
		size <- paste0("(", len, ")")
		cat("$", names(x)[i], size, ": ", nms, "\n", sep="")
	}
}

setMethod("show", "ImzML", function(object) {
	cat(class(object), ": ", metadata(object)$source, "\n\n", sep="")
	.show_list_names(object, n=7L)
})

setAs("list", "ImzML", function(from) {
	from <- SimpleList(from)
	as(from, "ImzML")
})

# test if cvParam (does *_NOT_* check ontology validity)
.is_cvParam <- function(x) {
	is.character(x) && all(c("cv", "id", "name") %in% names(x))
}

# test if userParam
.is_userParam <- function(x) {
	is.character(x) && "name" %in% names(x)
}

# test if cvParam OR userParam
.is_param <- function(x) {
	.is_cvParam(x) || .is_userParam(x)
}

# print cvParams and userParams
.show_params <- function(x, n = 12L, collapse = ", ")
{
	a <- attributes(x)
	a <- a[setdiff(names(a), c("names", "class"))]
	if ( length(a) > 0L ) {
		a <- vapply(a, as.character, character(1L))
		a <- .shorten_strings(a)
		print.default(a, quote=FALSE)
	}
	if ( length(x) > 0L ) {
		recursive <- vapply(x, is.list, logical(1L))
		lens <- lengths(x)
		ids <- names(x)
		ids <- ifelse(recursive, paste0(ids, "(", lens, ")"), ids)
		nms <- vapply(x, function(tag)
			{
				if ( is.list(tag) ) {
					"..."
				} else if ( .is_cvParam(tag) ) {
					tag["name"] 	# cvParam
				} else if ( .is_userParam(tag) ) {
					"[userParam]"	# userParam
				} else {
					tag[1L]			# other tags (e.g. "softwareRef")
				}
			}, character(1L))
		vals <- vapply(x, function(tag)
			{
				if ( "value" %in% names(tag) ) {
					if ( "unit_name" %in% names(tag) ) {
						paste(tag["value"], tag["unit_name"])
					} else {
						tag["value"]
					}
				} else {
					""
				}
			}, character(1L))
		if ( length(x) > n ) {
			ids <- head(ids, n=n)
			nms <- head(nms, n=n)
			vals <- head(vals, n=n)
		}
		vals <- .shorten_strings(vals)
		sep <- ifelse(nchar(vals) > 0L, "=", "")
		tags <- cbind(ids, "-", nms, sep, vals)
		colnames(tags) <- rep.int("", ncol(tags))
		rownames(tags) <- rep.int("", nrow(tags))
		print.default(tags, quote=FALSE)
		if ( length(x) > n )
			cat(sprintf("... %d more\n", length(x) - n))
	}
}

# print list of cvParams and userParams
print.imzplist <- function(x, n = 12L, collapse = ", ", ...)
{
	cat(sprintf("Params list of length %d\n", length(x)))
	.show_params(x, n=n, collapse=collapse, ...)
	invisible(x)
}

#### parse an imzML file ####
## --------------------------

parseImzML <- function(file, ...)
{
	path <- normalizePath(file, mustWork=TRUE)
	parse <- .Call(C_parseImzML, path)
	parse <- .new_ImzML(parse, validate=FALSE)
	metadata(parse)$source <- file
	metadata(parse)$location <- dirname(file)
	metadata(parse)$name <- basename(file)
	parse
}

exampleImzMLFile <- function(type = c("continuous", "processed"))
{
	path <- switch(match.arg(type),
		continuous="extdata/Example_Continuous_imzML1.1.1/Example_Continuous.imzML",
		processed="extdata/Example_Processed_imzML1.1.1/Example_Processed.imzML")
	system.file(path, package="CardinalIO")
}
