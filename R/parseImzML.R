
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
			compression <- union(mzCompression, intensityCompression)
			parse[["ibd"]][["mz"]] <- mz
			parse[["ibd"]][["intensity"]] <- intensity
			if ( !is.null(extraArrays) )
			{
				extra <- lapply(extraArrays,
					function(e)
					{
						if ( anyNA(e) )
							return(NULL)
						eCompression <- e[["binary data compression type"]]
						compression <- union(compression, eCompression)
						if ( isTRUE(all(eCompression %in% "no compression")) ) {
							matter_list(path=path_ibd, type=e[["binary data type"]],
								offset=as.numeric(e[["external offset"]]),
								extent=as.numeric(e[["external array length"]]),
								names=row.names(e))
						} else {
							matter_list(path=path_ibd, type="raw",
								offset=as.numeric(e[["external offset"]]),
								extent=as.numeric(e[["external encoded length"]]),
								names=row.names(e))
						}
					})
				parse[["ibd"]][["extra"]] <- extra
			}
			metadata(parse)[["compression"]] <- unique(compression)
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

