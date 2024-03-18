
#### Write an Analyze 7.5 file ####
## --------------------------------

setGeneric("writeAnalyze", function(object, ...) standardGeneric("writeAnalyze"))

setMethod("writeAnalyze", "array", 
	function(object, file, positions = NULL, domain = NULL,
		type = "float32", ...)
	{
		.write_Analyze75(object, file=file,
			positions=positions, domain=domain, type=type)
	})

setMethod("writeAnalyze", "matter_arr", 
	function(object, file, positions = NULL, domain = NULL,
		type = "float32", ...)
	{
		.write_Analyze75(object, file=file,
			positions=positions, domain=domain, type=type)
	})

setMethod("writeAnalyze", "sparse_arr", 
	function(object, file, positions = NULL, domain = NULL,
		type = "float32", ...)
	{
		.write_Analyze75(object, file=file,
			positions=positions, domain=domain, type=type)
	})

.write_Analyze75 <- function(x, file, positions, domain, type)
{
	if ( is.null(positions) ) {
		if ( length(dim(x)) < 3L )
			stop("'x' must be array-like with at least 3 dimensions")
		positions <- expand.grid(lapply(dim(x)[-1L], seq_len))
		dim(x) <- c(dim(x)[1L], prod(dim(x)[-1L]))
	} else {
		if ( length(dim(x)) != 2L )
			stop("'x' must be matrix-like when 'positions' is specified")
		pos_ok <- vapply(positions,
			function(pos) all(pos == as.integer(pos)), logical(1L))
		if ( !all(pos_ok) )
			stop("'positions' must be gridded integer coordinates")
	}
	dim <- c(dim(x)[1L], vapply(positions, max, numeric(1L)))
	path <- file_path_sans_ext(file)
	path_hdr <- normalizePath(paste0(path, ".hdr"), mustWork=FALSE)
	path_img <- normalizePath(paste0(path, ".img"), mustWork=FALSE)
	path_t2m <- normalizePath(paste0(path, ".t2m"), mustWork=FALSE)
	if ( file.exists(path_hdr) ) {
		warning("file ", sQuote(path_hdr), " already exists and will be overwritten")
		if ( !file.create(path_hdr) )
			warning("problem overwriting file ", sQuote(path_hdr))
	}
	hdr <- .set_Analyze75_header(path_hdr, dim, type)
	if ( file.exists(path_img) ) {
		warning("file ", sQuote(path_img), " already exists and will be overwritten")
		if ( !file.create(path_img) )
			warning("problem overwriting file ", sQuote(path_img))
	}
	img <- .set_Analyze75_image(path_img, x, positions, type)
	if ( !missing(domain) && !is.null(domain) ) {
		domain <- as.double(domain)
		if ( file.exists(path_t2m) ) {
			warning("file ", sQuote(path_t2m), " already exists and will be overwritten")
			if ( !file.create(path_t2m) )
				warning("problem overwriting file ", sQuote(path_t2m))
		}
		t2m <- matter_vec(domain, path=path_t2m, type="float32", readonly=FALSE)
		outpath <- c(path_hdr, path_img, path_t2m)
	} else {
		outpath <- c(path_hdr, path_img)
	}
	structure(TRUE, outpath=outpath)
}

.set_Analyze75_header <- function(path, dim, type)
{
	ndims <- length(dim)
	if ( ndims < 3L )
		stop("need at least 3 dimensions")
	if ( ndims < 4L ) {
		dim <- c(dim, 1L)
		ndims <- 4L
	}
	hdr <- .get_Analyze75_header(path, readonly=FALSE)
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

.set_Analyze75_image <- function(path, x, positions, type)
{
	if ( ncol(x) != nrow(positions) )
		stop("extent of array does not match number of positions")
	dim <- vapply(positions, max, numeric(1L))
	img <- matter_mat(0, nrow=nrow(x), ncol=prod(dim),
		path=path, type=type, readonly=FALSE)
	index <- linear_ind(as.matrix(positions), dim)
	for ( i in seq_along(index) )
		img[,index[i]] <- x[,i]
	img
}
