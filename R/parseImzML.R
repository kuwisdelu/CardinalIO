
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

setAs("ImzMeta", "ImzML", function(from) {
	.convert_ImzMeta_to_ImzML(from)
})

setMethod("show", "ImzML",
	function(object) {
		n <- 6L
		cat("ImzML: ", metadata(object)[["source"]][1L], "\n\n", sep="")
		for ( i in seq_along(object) ) {
			len <- length(object[[i]])
			nms <- names(object[[i]])
			if ( len > n )
				nms <- paste0(head(nms, n=n), "...")
			nms <- paste0(nms, collapse=" ")
			size <- paste0("(", len, ")")
			cat("$", names(object)[i], size, ": ", nms, "\n", sep="")
		}
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

# show utility
.shorten_strings <- function(x, n = 12L)
{
	ifelse(nchar(x) > n, paste0(substr(x, 1, n), "..."), x)
}

# print list of cvParams and userParams
print.imzplist <- function(x, n = 12L, ...)
{
	cat(sprintf("Params list of length %d\n", length(x)))
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

#### Parse an imzML file ####
## --------------------------

parseImzML <- function(file, ibd = FALSE, extra = NULL,
	extraArrays = NULL, check = ibd, ...)
{
	path <- normalizePath(file, mustWork=TRUE)
	if ( tolower(file_ext(path)) != "imzml" )
		warning("file ", sQuote(path), " does not have '.imzML' extension")
	if ( !is.null(extra) && !is.character(extra) )
		stop("'extra' must be a character vector or NULL")
	parse <- .Call(C_parseImzML, path, extra, extraArrays)
	parse <- .new_ImzML(parse, validate=FALSE)
	if ( !is.null(names(extra)) )
	{
		ex <- parse[["run"]][["spectrumList"]][["extra"]]
		names(ex) <- names(extra)
		parse[["run"]][["spectrumList"]][["extra"]] <- ex
	}
	if ( !is.null(names(extraArrays)) )
	{
		ex <- parse[["run"]][["spectrumList"]][["extraArrays"]]
		names(ex) <- names(extraArrays)
		parse[["run"]][["spectrumList"]][["extraArrays"]] <- ex
	}
	check_opts <- c("checksum", "uuid", "filesize")
	if ( isTRUE(check) ) {
		check <- check_opts
	} else if ( isFALSE(check) ) {
		check <- character()
	} else {
		check <- match.arg(check, check_opts, several.ok=TRUE)
	}
	if ( ibd || length(check) > 0L )
	{
		fileContent <- parse[["fileDescription"]][["fileContent"]]
		mzArrays <- parse[["run"]][["spectrumList"]][["mzArrays"]]
		intensityArrays <- parse[["run"]][["spectrumList"]][["intensityArrays"]]
		extraArrays <- parse[["run"]][["spectrumList"]][["extraArrays"]]
		path_ibd <- paste0(file_path_sans_ext(path), ".ibd")
		path_ibd <- normalizePath(path_ibd, mustWork=TRUE)
		parse[["ibd"]] <- list()
		if ( "checksum" %in% check )
		{
			chk <- find_descendants_in(fileContent, "IMS:1000009", "ims")
			if ( length(chk) == 1L ) {
				chk <- chk[[1L]]
				algo <- switch(chk["id"],
					"IMS:1000090"="md5",
					"IMS:1000091"="sha1",
					"IMS:1000092"="sha256",
					"sha1")
				hash <- tolower(checksum(path_ibd, algo=algo))
				if ( !isTRUE(hash == tolower(chk["value"])) )
					warning(chk["name"], " tag from imzML file [", chk["value"], "] ",
						"does not match ", algo, " checksum from ibd file [", hash, "]")
				attr(parse[["ibd"]], "checksum") <- hash
			} else {
				warning("couldn't determine checksum from imzML file")
			}
		}
		if ( "uuid" %in% check )
		{
			fid <- fileContent[["IMS:1000080"]]
			uuid <- matter_vec(path=path_ibd, type="raw", length=16L)
			uuid <- try(as.raw(uuid), silent=TRUE)
			if ( is.raw(uuid) ) {
				fid_clean <- gsub("[^[:alnum:]]", "", fid["value"])
				if ( !isTRUE(raw2hex(uuid) == tolower(fid_clean)) )
					warning("'uuid' tag from imzML file [", fid_clean, "] ",
						"does not match 'uuid' bytes from ibd file [", raw2hex(uuid), "]")
				parse[["ibd"]][["uuid"]] <- uuid
			} else {
				warning("failed to read 'uuid' bytes from ibd file")				
			}
		}
		if ( "filesize" %in% check )
		{
			size <- file.size(path_ibd)
			mz_offset <- as.numeric(mzArrays[["external offset"]])
			intensity_offset <- as.numeric(intensityArrays[["external offset"]])
			if ( anyNA(mz_offset) )
				warning("missing values in binary data array offsets for m/z arrays")
			if ( anyNA(intensity_offset) )
				warning("missing values in binary data array offsets for intensity arrays")
			max_offset <- max(mz_offset, intensity_offset, na.rm=TRUE)
			if ( max_offset > size )
				warning("maximum binary data array offset from imzML file [", max_offset, "] ",
					"is larger than the ibd file size [", size, "]")
		}
		if ( ibd )
		{
			mzCompression <- mzArrays[["binary data compression type"]]
			if ( isTRUE(all(mzCompression == "no compression")) ) {
				mz <- matter_list(path=path_ibd, type=mzArrays[["binary data type"]],
					offset=as.numeric(mzArrays[["external offset"]]),
					extent=as.numeric(mzArrays[["external array length"]]),
					names=row.names(mzArrays))
			} else {
				mz <- matter_list(path=path_ibd, type="raw",
					offset=as.numeric(mzArrays[["external offset"]]),
					extent=as.numeric(mzArrays[["external encoded length"]]),
					names=row.names(mzArrays))
			}
			intensityCompression <- intensityArrays[["binary data compression type"]]
			if ( isTRUE(all(intensityCompression == "no compression")) ) {
				intensity <- matter_list(path=path_ibd, type=intensityArrays[["binary data type"]],
					offset=as.numeric(intensityArrays[["external offset"]]),
					extent=as.numeric(intensityArrays[["external array length"]]),
					names=row.names(intensityArrays))
			} else {
				intensity <- matter_list(path=path_ibd, type="raw",
					offset=as.numeric(intensityArrays[["external offset"]]),
					extent=as.numeric(intensityArrays[["external encoded length"]]),
					names=row.names(intensityArrays))
			}
			parse[["ibd"]][["mz"]] <- mz
			parse[["ibd"]][["intensity"]] <- intensity
			if ( !is.null(extraArrays) )
			{
				extra <- lapply(extraArrays,
					function(ex)
					{
						if ( anyNA(ex) )
							return(NULL)
						exCompression <- ex[["binary data compression type"]]
						if ( isTRUE(all(exCompression == "no compression")) ) {
							matter_list(path=path_ibd, type=ex[["binary data type"]],
								offset=as.numeric(ex[["external offset"]]),
								extent=as.numeric(ex[["external array length"]]),
								names=row.names(ex))
						} else {
							matter_list(path=path_ibd, type="raw",
								offset=as.numeric(ex[["external offset"]]),
								extent=as.numeric(ex[["external encoded length"]]),
								names=row.names(ex))
						}
					})
				parse[["ibd"]][["extra"]] <- extra
			}
			path <- c(path, path_ibd)
		}
	}
	metadata(parse)[["source"]] <- path
	metadata(parse)[["location"]] <- dirname(path)
	metadata(parse)[["name"]] <- basename(path)
	parse
}

exampleImzMLFile <- function(type = c("continuous", "processed"))
{
	path <- switch(match.arg(type),
		continuous="extdata/Example_Continuous_imzML1.1.1/Example_Continuous.imzML",
		processed="extdata/Example_Processed_imzML1.1.1/Example_Processed.imzML")
	system.file(path, package="CardinalIO")
}

