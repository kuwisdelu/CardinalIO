
#### Analyze75 class ####
## -----------------------

setClass("Analyze75", contains = "SimpleList")

# no public constructor (get from parseAnalyze)

# construct new Analyze75 from arguments
.new_Analyze75 <- function(...)
{
	object <- SimpleList(...)
	as(object, "Analyze75")
}

setMethod("show", "Analyze75",
	function(object) {
		n <- 6L
		dims <- dim(object$img)
		dims <- paste0(dims, collapse=" x ")
		cat("Analyze 7.5: ", metadata(object)[["source"]][2L], "\n\n", sep="")
		cat("$hdr: ", paste0(names(object$hdr), collapse=" "), "\n", sep="")
		type <- as.character(type(object$img))
		cat("$img: <", dims, "> ", type, " array\n", sep="")
		if ( "t2m" %in% names(object) )
		{
			vals <- format(head(object$t2m, n=n))
			vals <- paste0(vals, collapse=", ")
			if ( length(object$t2m) > n )
				vals <- paste(vals, "...")
			cat("$t2m: ", vals, "\n", sep="")
		}
	})

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
	hdr <- .get_Analyze75_header(path_hdr)
	img <- .get_Analyze75_image(path_img, hdr)
	if ( file.exists(path_t2m) ) {
		len <- dim(img)[1L]
		t2m <- matter_vec(path=path_t2m, type="float32", length=len)
		path <- c(path_hdr, path_img, path_t2m)
		parse <- .new_Analyze75(hdr=hdr, img=img, t2m=t2m)
	} else {
		path <- c(path_hdr, path_img)
		parse <- .new_Analyze75(hdr=hdr, img=img)
	}
	metadata(parse)[["source"]] <- path
	metadata(parse)[["location"]] <- dirname(path)
	metadata(parse)[["name"]] <- basename(path)
	parse
}

.get_Analyze75_header <- function(path, readonly = TRUE)
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

.get_Analyze75_image <- function(path, hdr, readonly = TRUE)
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
