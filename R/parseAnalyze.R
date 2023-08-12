
#### Parse Analyze 7.5 files ####
## -------------------------------

parseAnalyze <- function(file, ...)
{
	path <- file_path_sans_ext(file)
	path_hdr <- normalizePath(paste0(path, ".hdr"), mustWork=TRUE)
	path_img <- normalizePath(paste0(path, ".img"), mustWork=TRUE)
	path_t2m <- normalizePath(paste0(path, ".t2m"), mustWork=FALSE)
	if ( file.size(path_hdr) != 348L )
		warning("Analyze 7.5 header is the wrong size: ", sQuote(path_hdr))
	hdr <- .get_analyze75_header(path_hdr)
	img <- .get_analyze75_image(path_img, hdr)
	if ( file.exists(path_t2m) ) {
		len <- dim(img)[1L]
		t2m <- matter_vec(path=path_t2m, type="float32", length=len)
		source <- c(path_hdr, path_img, path_t2m)
		structure(list(hdr=hdr, img=img, mz=t2m),
			source=source, class="analyze75")
	} else {
		source <- c(path_hdr, path_img)
		structure(list(hdr=hdr, img=img),
			source=source, class="analyze75")
	}
}

writeAnalyze <- function(object, file, type = "float32", mz, ...)
{
	x <- as.array(object)
	if ( length(dim(x)) < 3L )
		stop("'object' must be array-like with at least 3 dimensions")
	path <- file_path_sans_ext(file)
	path_hdr <- normalizePath(paste0(path, ".hdr"), mustWork=FALSE)
	path_img <- normalizePath(paste0(path, ".img"), mustWork=FALSE)
	path_t2m <- normalizePath(paste0(path, ".t2m"), mustWork=FALSE)
	if ( file.exists(path_hdr) ) {
		warning("file ", sQuote(path_hdr), " already exists and will be overwritten")
		if ( !file.create(path_hdr) )
			warning("problem overwriting file ", sQuote(path_hdr))
	}
	hdr <- .set_analyze75_header(path_hdr, x, type)
	if ( file.exists(path_img) ) {
		warning("file ", sQuote(path_img), " already exists and will be overwritten")
		if ( !file.create(path_img) )
			warning("problem overwriting file ", sQuote(path_img))
	}
	img <- .set_analyze75_image(path_img, x, type)
	if ( !missing(mz) ) {
		if ( file.exists(path_t2m) ) {
			warning("file ", sQuote(path_t2m), " already exists and will be overwritten")
			if ( !file.create(path_t2m) )
				warning("problem overwriting file ", sQuote(path_t2m))
		}
		t2m <- matter_vec(mz, path=path_t2m, type="float32", readonly=FALSE)
		source <- c(path_hdr, path_img, path_t2m)
	} else {
		source <- c(path_hdr, path_img)
	}
	structure(TRUE, outpath=source)
}

.get_analyze75_header <- function(path, readonly = TRUE)
{
	header_key <- struct(
		sizeof_hdr=c("int32"=1),
		data_type=c("char"=10),
		db_name=c("char"=18),
		extents=c("int32"=1),
		session_error=c("int16"=1),
		regular=c("char"=1),
		hkey_un0=c("char"=1),
		path=path, readonly=readonly, offset=0)
	image_dimensions <- struct(
		dim=c("uint16"=8),
		unused8=c("int16"=1),
		unused9=c("int16"=1),
		unused10=c("int16"=1),
		unused11=c("int16"=1),
		unused12=c("int16"=1),
		unused13=c("int16"=1),
		unused14=c("int16"=1),
		datatype=c("int16"=1),
		bitpix=c("int16"=1),
		dim_un0=c("int16"=1),
		pixdim=c("float32"=8),
		vox_offset=c("float32"=1),
		funused1=c("float32"=1),
		funused2=c("float32"=1),
		funused3=c("float32"=1),
		cal_max=c("float32"=1),
		cal_min=c("float32"=1),
		compressed=c("float32"=1),
		verified=c("float32"=1),
		glmax=c("int32"=1),
		glmin=c("int32"=1),
		path=path, readonly=readonly, offset=40)
	data_history <- struct(
		descript=c("char"=80),
		aux_file=c("char"=24),
		orient=c("char"=1),
		originator=c("char"=10),
		generated=c("char"=10),
		scannum=c("char"=10),
		patient_id=c("char"=10),
		exp_date=c("char"=10),
		exp_time=c("char"=10),
		hist_un0=c("char"=3),
		views=c("int32"=1),
		vols_added=c("int32"=1),
		start_field=c("int32"=1),
		field_skip=c("int32"=1),
		omax=c("int32"=1),
		omin=c("int32"=1),
		smax=c("int32"=1),
		smin=c("int32"=1),
		path=path, readonly=readonly, offset=148)
	list(header_key=header_key,
		image_dimensions=image_dimensions,
		data_history=data_history)
}

.set_analyze75_header <- function(path, x, type)
{
	dim <- dim(x)
	ndims <- length(dim)
	if ( ndims < 3L )
		stop("need at least 3 dimensions")
	if ( ndims < 4L ) {
		dim <- c(dim, 1L)
		ndims <- 4L
	}
	hdr <- .get_analyze75_header(path, readonly=FALSE)
	hdr$header_key[] <- rep(list(0), length(hdr$header_key))
	hdr$image_dimensions[] <- rep(list(0), length(hdr$image_dimensions))
	hdr$data_history[] <- rep(list(0), length(hdr$data_history))
	hdr$header_key$sizeof_hdr <- 348L	# byte size of header
	hdr$header_key$extents <- 16384L	# required for some reason
	hdr$header_key$regular <- charToRaw("r")
	allowed_types <- c("int16", "int32", "float32", "float64")
	type <- match.arg(type, allowed_types)
	size <- switch(type,
		int16=2L,
		int32=4L,
		float32=4L,
		float64=8L)
	type <- switch(type,
		int16=4L,
		int32=8L,
		float32=16L,
		float64=64L)
	hdr$image_dimensions$datatype <- type
	hdr$image_dimensions$bitpix <- size
	hdr$image_dimensions$pixdim[seq_len(ndims)] <- rep.int(1, ndims)
	dims_arr <- rep.int(0L, 8L)
	dims_arr[1L] <- ndims
	dims_arr[seq_len(ndims) + 1L] <- dim
	hdr$image_dimensions$dim <- dims_arr
	hdr
}

.get_analyze75_image <- function(path, hdr, readonly = TRUE)
{
	dims_arr <- hdr[["image_dimensions"]][["dim"]]
	ndims <- dims_arr[1L]
	dim <- dims_arr[seq_len(ndims) + 1L]
	type <- hdr[["image_dimensions"]][["datatype"]]
	type <- switch(as.character(type),
		"4"="int16",
		"8"="int32",
		"16"="float32",
		"64"="float64",
		stop("unsupported Analyze 7.5 datatype (", type, ")"))
	matter_arr(path=path, type=type, dim=dim, readonly=readonly)
}

.set_analyze75_image <- function(path, x, type)
{
	matter_arr(x, path=path, type=type, readonly=FALSE)
}

print.analyze75 <- function(x, n = 6L, collapse = ", ", ...)
{
	dims <- dim(x$img)
	dims <- paste0(dims, collapse=" x ")
	cat("Analyze 7.5: ", attr(x, "source")[2L], "\n\n", sep="")
	cat("$hdr: ", paste0(names(x$hdr), collapse=", "), "\n", sep="")
	type <- as.character(type(x$img))
	cat("$img: <", dims, "> ", type, " array\n", sep="")
	if ( "mz" %in% names(x) )
	{
		vals <- format(head(x$mz, n=n))
		vals <- paste0(vals, collapse=", ")
		if ( length(x$mz) > n )
			vals <- paste(vals, "...")
		cat("$mz: ", vals, "\n", sep="")
	}
	invisible(x)
}

