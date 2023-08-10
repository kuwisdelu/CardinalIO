
#### ImzML class ####
## ------------------

setClass("ImzML", contains = "SimpleList")

# no public constructor (get from parseImzML or ImzMeta)

# note: Does *_NOT_* validate the xml mapping
.valid_ImzML <- function(object)
{
	errors <- NULL
	required_tags <- c("fileDescription", "softwareList",
		"instrumentConfigurationList", "dataProcessingList", "run")
	missing_tags <- setdiff(required_tags, names(object))
	if ( length(missing_tags) > 0L )
		errors <- c(errors , paste0("required mzML tags are missing: ",
			paste0(sQuote(missing_tags), collapse=", ")))
	if ( is.null(errors) ) TRUE else errors
}

setValidity("ImzML", .valid_ImzML)

setMethod("path", "ImzML", function(object) metadata(object)[["source"]])

setMethod("mz", "ImzML", function(object) object[["ibd"]][["mz"]])

setMethod("intensity", "ImzML", function(object) object[["ibd"]][["intensity"]])

setMethod("checksum", "ImzML", function(x, ...) attr(x[["ibd"]], "checksum"))

# construct new ImzML from arguments
.new_ImzML <- function(..., validate = TRUE)
{
	object <- SimpleList(...)
	object <- as(object, "ImzML")
	if ( validate )
		validObject(object)
	object
}

# needed to make [[<-, etc. work
setAs("list", "ImzML", function(from) {
	from <- SimpleList(from)
	as(from, "ImzML")
})

setAs("ImzML", "ImzMeta", function(from) {
	.convert_ImzML_to_ImzMeta(from)
})

setAs("ImzML", "ImzMeta", function(from) {
	.convert_ImzMeta_to_ImzML(from)
})

# show utility
.shorten_strings <- function(x, n = 12L)
{
	ifelse(nchar(x) > n, paste0(substr(x, 1, n), "..."), x)
}

.show_list_names <- function(x, n = 12L, sep = " ")
{
	for ( i in seq_along(x) ) {
		len <- length(x[[i]])
		nms <- names(x[[i]])
		if ( len > n )
			nms <- paste0(head(nms, n=n), "...")
		nms <- paste0(nms, collapse=sep)
		size <- paste0("(", len, ")")
		cat("$", names(x)[i], size, ": ", nms, "\n", sep="")
	}
}

setMethod("show", "ImzML", function(object) {
	cat(class(object), ": ", path(object), "\n\n", sep="")
	.show_list_names(object, n=7L)
})

# test if cvParam (does *_NOT_* check ontology validity)
.is_cvParam <- function(x) {
	is.character(x) && all(c("cv", "id", "name") %in% names(x))
}

# test if userParam
.is_userParam <- function(x) {
	is.character(x) && all(c("name", "value") %in% names(x)) && !"cv" %in% names(x)
}

# test if cvParam OR userParam
.is_param <- function(x) {
	.is_cvParam(x) || .is_userParam(x)
}

.xml_attr <- function(x) {
	a <- attributes(x)
	nm <- setdiff(names(a), c("names", "class"))
	a[nm]
}

# print cvParams and userParams
.show_params <- function(x, n = 12L, collapse = ", ")
{
	a <- .xml_attr(x)
	if ( length(a) > 0L ) {
		a <- vapply(a, as.character, character(1L))
		a <- .shorten_strings(a)
		print.default(a, quote=FALSE)
	}
	if ( length(x) > 0L ) {
		recursive <- vapply(x, is.list, logical(1L))
		userparam <- vapply(x, .is_userParam, logical(1L))
		lens <- lengths(x)
		ids <- names(x)
		ids <- ifelse(recursive, paste0(ids, "(", lens, ")"), ids)
		ids <- ifelse(userparam, "[userParam]", ids)
		nms <- vapply(x, function(tag)
			{
				if ( is.list(tag) ) {
					"..."
				} else if ( .is_param(tag) ) {
					tag["name"] 	# cvParam / userParam
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

