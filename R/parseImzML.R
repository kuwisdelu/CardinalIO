
#### Parse an imzML file ####
## --------------------------

parseImzML <- function(file, ibd = FALSE, extra = NULL, check = ibd, ...)
{
	path <- normalizePath(file, mustWork=TRUE)
	if ( file_ext(path) != "imzML" )
		warning("file ", sQuote(path), " does not end have '.imzML' extension")
	if ( !is.null(extra) && !is.character(extra) )
		stop("'extra' must be a character vector or NULL")
	parse <- .Call(C_parseImzML, path, extra)
	parse <- .new_ImzML(parse, validate=FALSE)
	if ( ibd || check )
	{
		binpath <- paste0(file_path_sans_ext(path), ".ibd")
		binpath <- normalizePath(binpath, mustWork=TRUE)
		parse[["ibd"]] <- list()
		if ( check ) {
			fileContent <- parse[["fileDescription"]][["fileContent"]]
			chk <- find_descendants_in(fileContent, "IMS:1000009", "ims")
			if ( length(chk == 1L) ) {
				chk <- chk[[1L]]
				algo <- switch(chk["id"],
					"IMS:1000090"="md5",
					"IMS:1000091"="sha1",
					"IMS:1000092"="sha256",
					"sha1")
				hash <- checksum(binpath, algo=algo)
				if ( !isTRUE(hash == chk["value"]) )
					warning(chk["name"], " tag from imzML file [", chk["value"], "] ",
						"does not match ", algo, " checksum from ibd file [", hash, "]")
				attr(parse[["ibd"]], "checksum") <- hash
			} else {
				warning("couldn't determine checksum from imzML file")
			}
			fid <- fileContent[["IMS:1000080"]]
			uuid <- as.raw(matter_vec(path=binpath, type="raw", length=16L))
			if ( !isTRUE(raw2hex(uuid) == fid["value"]) )
				warning("'uuid' tag from imzML file [", fid["value"], "] ",
					"does not match 'uuid' bytes from ibd file [", raw2hex(uuid), "]")
			parse[["ibd"]][["uuid"]] <- uuid
		}
		if ( ibd ) {
			mzArrays <- parse[["run"]][["spectrumList"]][["mzArrays"]]
			intensityArrays <- parse[["run"]][["spectrumList"]][["intensityArrays"]]
			mz <- matter_list(path=binpath, type=mzArrays[["binary data type"]],
				offset=as.numeric(mzArrays[["external offset"]]),
				extent=as.numeric(mzArrays[["external array length"]]),
				names=row.names(mzArrays))
			intensity <- matter_list(path=binpath, type=intensityArrays[["binary data type"]],
				offset=as.numeric(intensityArrays[["external offset"]]),
				extent=as.numeric(intensityArrays[["external array length"]]),
				names=row.names(intensityArrays))
			parse[["ibd"]][["mz"]] <- mz
			parse[["ibd"]][["intensity"]] <- intensity
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

