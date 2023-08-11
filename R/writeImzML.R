
#### Write an imzML file ####
## ---------------------------

setGeneric("writeImzML", function(object, ...) standardGeneric("writeImzML"))

setMethod("writeImzML", "ImzML", 
	function(object, file, positions = NULL, mz = NULL, intensity = NULL,
		mz.type = "float32", intensity.type = "float32", asis = FALSE, ...)
	{
		.writeIbdAndImzML(object, file=file, positions=positions,
			mz=mz, intensity=intensity,
			mz.type=mz.type, intensity.type=intensity.type,
			asis=asis)
	})

.writeIbdAndImzML <- function(object, file, positions,
	mz, intensity, mz.type, intensity.type, asis)
{
	if ( !is.character(file) || length(file) != 1L )
		stop("'file' must be a single string")
	path <- normalizePath(file, mustWork=FALSE)
	path <- file_path_sans_ext(path)
	path_imzML <- paste0(path, ".imzML")
	path_ibd <- paste0(path, ".ibd")
	# process positions
	if ( !is.null(positions) )
	{
		positions <- as.data.frame(apply(positions, 2L, as.character))
		if ( ncol(positions) == 2L ) {
			names(positions) <- c("position x", "position y")
		} else if ( ncol(positions) == 3L ) {
			names(positions) <- c("position x", "position y", "position z")
		} else {
			stop("positions must have 2 or 3 columns")
		}
		if ( "spectrumList" %in% names(object$run) ) {
			object$run$spectrumList$positions <- positions
		} else {
			object$run$spectrumList <- list(positions=positions)
		}
	}
	# process binary data arrays
	if ( !is.null(mz) && !is.null(intensity) )
	{
		if ( is.null(object$run$spectrumList$positions) )
			stop("missing required component $run$spectrumList$positions")
		n <- nrow(object$run$spectrumList$positions)
		if ( is.null(dim(intensity)) && length(intensity) != n )
			stop("rows of 'positions' does not match length of 'intensity'")
		if ( !is.null(dim(intensity)) && ncol(intensity) != n )
			stop("rows of 'positions' does not match columns of 'intensity'")
		if ( asis ) {
			# check existing ibd file
			path_ibd <- normalizePath(path_ibd, mustWork=TRUE)
			if ( !is.matter(mz) || !is.matter(intensity) )
				stop("'mz' and 'intensity' must both be matter objects when asis=TRUE")
			if ( length(path(mz)) != 1L || length(path(intensity)) != 1L )
				stop("'mz' or 'intensity' are not stored in a single file")
			if ( path(mz) != path(intensity) )
				stop("'mz' and 'intensity' are not stored in the same file")
			if ( path(mz) != path_ibd ) {
				warning("renaming ibd file from ",
					sQuote(path(mz)), " to ", sQuote(path_ibd))
				if ( !file.rename(path(mz), path_ibd) )
					warning("problem occured while renaming ibd file")
			}
			ibd <- list(mz=mz, intensity=intensity)
		} else {
			# write ibd file
			allowed_types <- c("int32", "int64", "float32", "float64")
			mz.type <- match.arg(mz.type, allowed_types)
			intensity.type <- match.arg(intensity.type, allowed_types)
			ibd <- .writeIbd(path_ibd, mz=mz, intensity=intensity,
				mz.type=mz.type, intensity.type=intensity.type)
		}
		# set ibd metadata
		object <- .setIbdMetadataFromMatter(object, ibd$mz, ibd$intensity)
		outpath <- c(path_imzML, path_ibd)
	} else {
		if ( !is.null(mz) || !is.null(intensity) )
			warning("'mz' and 'intensity' must both be specified")
		outpath <- path_imzML
	}
	# write imzML
	success <- .writeImzML(path_imzML, metadata=object)
	outpath <- normalizePath(outpath, mustWork=TRUE)
	structure(success, outpath=outpath)
}

.writeImzML <- function(path, metadata)
{
	if ( file_ext(path) != "imzML" )
		warning("file ", sQuote(path), " does not have '.imzML' extension")
	if ( file.exists(path) ) {
		warning("file ", sQuote(path), " already exists and will be overwritten")
		if ( !file.create(path) )
			warning("problem overwriting file ", sQuote(path))
	}
	fileContent <- metadata[["fileDescription"]][["fileContent"]]
	ibd_type <- "IMS:1000003"
	ibd_uuid <- "IMS:1000008"
	ibd_checksum <- "IMS:1000009"
	if ( length(find_descendants_in(fileContent, ibd_type)) < 1L)
		stop("missing required tag ", sQuote(ibd_type), " (ibd binary type)")
	if ( length(find_descendants_in(fileContent, ibd_uuid)) < 1L)
		stop("missing required tag ", sQuote(ibd_uuid), " (ibd identification)")
	if ( length(find_descendants_in(fileContent, ibd_checksum)) < 1L)
		stop("missing required tag ", sQuote(ibd_checksum), " (ibd checksum)")
	positions <- metadata[["run"]][["spectrumList"]][["positions"]]
	mzArrays <- metadata[["run"]][["spectrumList"]][["mzArrays"]]
	intensityArrays <- metadata[["run"]][["spectrumList"]][["intensityArrays"]]
	if ( is.null(positions) )
		stop("missing required component $run$spectrumList$positions")
	if ( is.null(mzArrays) )
		stop("missing required component $run$spectrumList$mzArrays")
	if ( is.null(intensityArrays) )
		stop("missing required component $run$spectrumList$intensityArrays")
	mzML <- .new_imzML_skeleton(metadata)
	.Call(C_writeImzML, mzML, positions, mzArrays, intensityArrays, path)
}

.writeIbd <- function(path, mz, intensity, mz.type, intensity.type)
{
	if ( file_ext(path) != "ibd" )
		warning("file ", sQuote(path), " does not have '.ibd' extension")
	if ( file.exists(path) ) {
		warning("file ", sQuote(path), " already exists and will be overwritten")
		if ( !file.create(path) )
			warning("problem overwriting file ", sQuote(path))
	}
	if ( is.numeric(mz) ) {
		.writeContinuousIbd(path, mz, intensity, mz.type, intensity.type)
	} else {
		.writeProcessedIbd(path, mz, intensity, mz.type, intensity.type)
	}
}

.writeContinuousIbd <- function(path, mz, intensity, mz.type, intensity.type)
{
	mz <- as.numeric(mz)
	intensity <- as.matrix(intensity)
	n <- ncol(intensity)
	# check array extents
	if ( nrow(intensity) != length(mz) )
		stop("length of 'mz' does not match rows of 'intensity' matrix")
	# write uuid
	id <- uuid(uppercase=FALSE)
	uuid <- matter_vec(id$bytes, path=path, type="raw", readonly=FALSE)
	# write m/z array
	mz <- matter_vec(mz, path=path, type=mz.type, append=TRUE)
	# write intensity arrays
	intensity <- matter_mat(intensity, path=path, type=intensity.type, append=TRUE)
	# return metadata
	list(uuid=uuid, mz=mz, intensity=intensity)
}

.writeProcessedIbd <- function(path, mz, intensity, mz.type, intensity.type)
{
	mz <- as.list(mz)
	intensity <- as.list(intensity)
	n <- length(intensity)
	# check array extents
	if ( length(mz) != length(intensity) )
		stop("length of 'mz' and 'intensity' do not match")
	if ( any(lengths(mz) != lengths(intensity)) )
		stop("lengths of 'mz' arrays and 'intensity' arrays do not all match")
	# write uuid
	id <- uuid(uppercase=FALSE)
	uuid <- matter_vec(id$bytes, path=path, type="raw", readonly=FALSE)
	# construct combined m/z and intensity arrays
	ind1 <- seq(1L, 2 * n - 1L, by=2L)
	ind2 <- seq(2L, 2 * n, by=2L)
	arrays <- vector("list", length=2 * n)
	for ( i in seq_len(n) )
	{
		arrays[[ind1[[i]]]] <- mz[[i]]
		arrays[[ind2[[i]]]] <- intensity[[i]]
	}
	types <- rep_len(c(mz.type, intensity.type), 2 * n)
	# write combined m/z and intensity arrays
	arrays <- matter_list(arrays, path=path, type=types, append=TRUE)
	mz <- arrays[ind1,drop=NULL]
	intensity <- arrays[ind2,drop=NULL]
	# return metadata
	list(uuid=uuid, mz=mz, intensity=intensity)
}

.getIbdMetadataFromMatter <- function(mz, intensity, algo = "sha1")
{
	if ( is(intensity, "matter_mat") ) {
		n <- ncol(intensity)
	} else {
		n <- length(intensity)
	}
	# get m/z metadata
	mzArrays <- data.frame(row.names=seq_len(n))
	mza <- as.data.frame(atomdata(mz))
	xlen <- sizeof(mza$type) * mza$extent
	mzArrays[["external offset"]] <- rep_len(mza$offset, n)
	mzArrays[["external array length"]] <- rep_len(mza$extent, n)
	mzArrays[["external encoded length"]] <- rep_len(xlen, n)
	mzArrays[["binary data type"]] <- rep_len(mza$type, n)
	mzArrays[] <- lapply(mzArrays, as.character)
	# get intensity metadata
	intensityArrays <- data.frame(row.names=seq_len(n))
	ia <- as.data.frame(atomdata(intensity))
	xlen <- sizeof(ia$type) * ia$extent
	intensityArrays[["external offset"]] <- ia$offset
	intensityArrays[["external array length"]] <- ia$extent
	intensityArrays[["external encoded length"]] <- xlen
	intensityArrays[["binary data type"]] <- ia$type
	intensityArrays[] <- lapply(intensityArrays, as.character)
	# get checksum and uuid as 8-4-4-4-12 formatted string
	path <- path(mz)
	checksum <- checksum(path, algo=algo)
	uuid <- as.raw(matter_vec(path=path, type="raw", length=16L))
	uuid <- raw2hex(uuid)
	uuid <- substr(rep.int(uuid, 5L),
		start=c(0L,9L,13L,17L,21L),
		stop=c(8L,12L,16L,20L,32L))
	uuid <- paste0(uuid, collapse="-")
	# determine ibd type
	if ( length(unique(mza$offset)) == 1L ) {
		type <- "continuous"
	} else {
		type <- "processed"
	}
	# return metadata
	list(mzArrays=mzArrays, intensityArrays=intensityArrays,
		type=type, uuid=uuid, checksum=checksum)
}

.setIbdMetadataFromMatter <- function(metadata, mz, intensity, algo = "sha1")
{
	meta <- .getIbdMetadataFromMatter(mz, intensity, algo)
	type_ids <- c("IMS:1000030", "IMS:1000031")
	hash_ids <- c("IMS:1000090", "IMS:1000091", "IMS:1000092")
	if ( any(type_ids %in% names(metadata$fileDescription$fileContent)) )
		metadata$fileDescription$fileContent[type_ids] <- NULL
	if ( any(hash_ids %in% names(metadata$fileDescription$fileContent)) )
		metadata$fileDescription$fileContent[hash_ids] <- NULL
	type <- switch(meta$type,
		continuous=.cvparam(cv="IMS", id="IMS:1000030", name="continuous"),
		processed=.cvparam(cv="IMS", id="IMS:1000031", name="processed"))
	uuid <- .cvparam(cv="IMS", id="IMS:1000080",
		name="universally unique identifier", value=meta$uuid)
	checksum <- switch(algo,
		md5=.cvparam(cv="IMS", id="IMS:1000090",
			name="ibd MD5", value=meta$checksum),
		sha1=.cvparam(cv="IMS", id="IMS:1000091",
			name="ibd SHA-1", value=meta$checksum),
		sha256=.cvparam(cv="IMS", id="IMS:1000092",
			name="ibd SHA-256", value=meta$checksum))
	metadata$fileDescription$fileContent[[type["id"]]] <- type
	metadata$fileDescription$fileContent[[uuid["id"]]] <- uuid
	metadata$fileDescription$fileContent[[checksum["id"]]] <- checksum
	metadata$run$spectrumList$mzArrays <- meta$mzArrays
	metadata$run$spectrumList$intensityArrays <- meta$intensityArrays
	metadata
}

#### Deparse generic imzML tags ####
## ----------------------------------

.deparse_cvParam <- function(x) {
	value <- if ("value" %in% names(x)) sprintf('value="%s"', x["value"]) else ""
	if ( all(c("unit_cv", "unit_id", "unit_name") %in% names(x)) ) {
		sprintf(paste('<cvParam cvRef="%s" accession="%s" name="%s" %s',
			'unitCvRef="%s" unitAccession="%s" unitName="%s"/>', sep=" "),
			x["cv"], x["id"], x["name"], value,
			x["unit_cv"], x["unit_id"], x["unit_name"])
	} else {
		sprintf('<cvParam cvRef="%s" accession="%s" name="%s" %s/>',
			x["cv"], x["id"], x["name"], value)
	}
}

.deparse_userParam <- function(x) {
	if ( all(c("unit_cv", "unit_id", "unit_name") %in% names(x)) ) {
		sprintf(paste('<userParam name="%s" value="%s"',
			'unitCvRef="%s" unitAccession="%s" unitName="%s"/>', sep=" "),
			x["name"], x["value"],
			x["unit_cv"], x["unit_id"], x["unit_name"])
	} else {
		sprintf('<userParam name="%s" value="%s"/>',
			x["name"], x["value"])
	}
}

.deparse_param <- function(x) {
	if ( .is_cvParam(x) ) {
		.deparse_cvParam(x)
	} else if ( .is_userParam(x) ) {
		.deparse_userParam(x)
	} else {
		NULL
	}
}

.deparse_params <- function(x) {
	tags <- unlist(lapply(x, .deparse_param))
	paste0(paste0(tags, collapse="\n"), "\n")
}

.deparse_imzplist <- function(x, name) {
	if ( is.null(x) )
		return("")
	tags <- .deparse_params(x)
	a <- .xml_attr(x)
	if ( length(a) > 0L ) {
		attr <- paste0(names(a), "=", dQuote(a, q=FALSE), collapse=" ")
		sprintf('<%s %s>\n%s</%s>\n', name, attr, tags, name)
	} else {
		sprintf('<%s>\n%s</%s>\n', name, tags, name)
	}
}

.deparse_taglist <- function(x, tagname, names) {
	if ( is.null(x) )
		return("")
	names <- rep_len(names, length(x))
	plists <- Map(.deparse_imzplist, unname(x), names)
	plists <- paste0(unlist(plists), collapse="")
	sprintf('<%s count="%d">\n%s</%s>\n', tagname, length(x), plists, tagname)
}

#### Deparse specific imzML tags ####
## -----------------------------------

# required
.deparse_fileDescription <- function(object) {
	tagname <- "fileDescription"
	tag <- object[[tagname]]
	if ( is.null(tag) )
		stop("fileDescription is missing with no default")
	fileContent <- .deparse_imzplist(tag[["fileContent"]], "fileContent")
	sourceFileList <- .deparse_taglist(tag[["sourceFileList"]], "sourceFileList", "sourceFile")
	contact <- .deparse_imzplist(tag[["contact"]], "contact")
	sprintf('<%s>\n%s%s%s</%s>\n',
		tagname, fileContent, sourceFileList, contact, tagname)
}

# optional
.deparse_sampleList <- function(object) {
	.deparse_taglist(object[["sampleList"]], "sampleList", "sample")
}

# optional
.deparse_scanSettingsList <- function(object) {
	.deparse_taglist(object[["scanSettingsList"]], "scanSettingsList", "scanSettings")
}

# required
.deparse_softwareList <- function(object) {
	tag <- object[["softwareList"]]
	if ( is.null(tag) ) {
		if ( isNamespaceLoaded("Cardinal") ) {
			package <- "Cardinal"
		} else {
			package <- "CardinalIO"
		}
		sprintf(
			'<softwareList count="1">
				<software id="%s" version="%s">
					<cvParam cvRef="MS" accession="MS:1000799" name="custom unreleased software tool" value="%s"/>
				</software>
			</softwareList>
			', package, packageVersion(package), package)
	} else {
		.deparse_taglist(tag, "softwareList", "software")
	}
}

# part of instrumentConfigurationList
.deparse_instrumentConfiguration <- function(x, id) {
	tagname <- "instrumentConfiguration"
	params <- .deparse_params(x)
	componentList <- x[["componentList"]]
	if ( !is.null(componentList) ) {
		tags <- Map(.deparse_imzplist,
			unname(componentList), names(componentList))
		tags <- paste0(unlist(tags), collapse="")
		componentList <- sprintf('<%s count="%d">\n%s</%s>\n', "componentList",
			length(componentList), tags, "componentList")
	} else {
		componentList <- ""
	}
	softwareRef <- x[["softwareRef"]]
	if ( !is.null(softwareRef) ) {
		softwareRef <- sprintf('<softwareRef ref="%s"/>\n', softwareRef)
	} else {
		softwareRef <- ""
	}
	sprintf('<%s id="%s">\n%s%s%s</%s>\n', tagname, id, params,
		componentList, softwareRef, tagname)
}

# required
.deparse_instrumentConfigurationList <- function(object) {
	tagname <- "instrumentConfigurationList"
	tag <- object[["instrumentConfigurationList"]]
	if ( is.null(tag) ) {
		sprintf(
			'<instrumentConfigurationList count="1">
				<instrumentConfiguration id="%s">
					<cvParam cvRef="MS" accession="MS:1000031" name="instrument model"/>
			    </instrumentConfiguration>
			</instrumentConfigurationList>
			', .get_defaultInstrumentConfigurationRef(NULL))
		
	} else {
		tags <- Map(.deparse_instrumentConfiguration, unname(tag), names(tag))
		tags <- paste0(unlist(tags), collapse="")
		sprintf('<%s count="%d">\n%s</%s>\n', tagname, length(tag), tags, tagname)
	}
}

# part of dataProcessingList
.deparse_dataProcessing <- function(x, id) {
	tagname <- "dataProcessing"
	names <- rep_len("processingMethod", length(x))
	plists <- Map(.deparse_imzplist, unname(x), names)
	plists <- paste0(unlist(plists), collapse="")
	sprintf('<%s id="%s">\n%s</%s>\n', tagname, id, plists, tagname)
}

# required
.deparse_dataProcessingList <- function(object) {
	tagname <- "dataProcessingList"
	tag <- object[["dataProcessingList"]]
	if ( is.null(tag) ) {
		if ( isNamespaceLoaded("Cardinal") ) {
			package <- "Cardinal"
		} else {
			package <- "CardinalIO"
		}
		sprintf(
			'<dataProcessingList count="1">
				<dataProcessing id="%s">
					<processingMethod order="1" softwareRef="%s">
						<cvParam cvRef="MS" accession="MS:1000544" name="Conversion to mzML"/>
					</processingMethod>
				</dataProcessing>
			</dataProcessingList>
			', .get_defaultDataProcessingRef(NULL), package)
	} else {
		tags <- Map(.deparse_dataProcessing, unname(tag), names(tag))
		tags <- paste0(unlist(tags), collapse="")
		sprintf('<%s count="%d">\n%s</%s>\n', tagname, length(tag), tags, tagname)
	}
}

#### Construct imzML skeleton ####
## -------------------------------

.new_referenceableParamGroupList <- function(representation, mz.type, intensity.type)
{
	# spectrum representation
	representation <- c(cv="MS",
		id=unname(find_term(representation, "ms", value="accession")),
		name=unname(find_term(representation, "ms", value="name")))
	representation <- .deparse_cvParam(representation)
	# get allowed data types
	ms <- get_obo("ms")
	binary_data_types <- ms$name[get_descendants(ms, "MS:1000518")]
	# m/z array data type
	if ( !mz.type %in% binary_data_types )
		mz.type <- switch(mz.type,
			int32="32-bit integer",
			int64="64-bit integer",
			float32="32-bit float",
			float64="64-bit float",
			stop("unrecognized binary data type: ", sQuote(mz.type)))
	mz.type <- c(cv="MS",
		id=unname(find_term(mz.type, "ms", value="accession")),
		name=unname(find_term(mz.type, "ms", value="name")))
	mz.type <- .deparse_cvParam(mz.type)
	# intensity array data type
	if ( !intensity.type %in% binary_data_types )
		intensity.type <- switch(intensity.type,
			int32="32-bit integer",
			int64="64-bit integer",
			float32="32-bit float",
			float64="64-bit float",
			stop("unrecognized binary data type: ", sQuote(intensity.type)))
	intensity.type <- c(cv="MS",
		id=unname(find_term(intensity.type, "ms", value="accession")),
		name=unname(find_term(intensity.type, "ms", value="name")))
	intensity.type <- .deparse_cvParam(intensity.type)
	# make referenceableParamGroupList
	sprintf(.referenceableParamGroupList_skeleton,
		representation, mz.type, intensity.type)
}

.referenceableParamGroupList_skeleton <- 
	'<referenceableParamGroupList count="4">
		<referenceableParamGroup id="spectrum1">
			<cvParam cvRef="MS" accession="MS:1000579" name="MS1 spectrum"/>
			<cvParam cvRef="MS" accession="MS:1000511" name="ms level" value="0"/>
			%s
		</referenceableParamGroup>
		<referenceableParamGroup id="scan1">
			<cvParam cvRef="MS" accession="MS:1000093" name="increasing m/z scan"/>
		</referenceableParamGroup>
		<referenceableParamGroup id="mzArray">
			<cvParam cvRef="MS" accession="MS:1000576" name="no compression"/>
			<cvParam cvRef="MS" accession="MS:1000514" name="m/z array" unitCvRef="MS" unitAccession="MS:1000040" unitName="m/z"/>
			<cvParam cvRef="IMS" accession="IMS:1000101" name="external data" value="true"/>
			%s
		</referenceableParamGroup>
		<referenceableParamGroup id="intensityArray">
			<cvParam cvRef="MS" accession="MS:1000576" name="no compression"/>
			<cvParam cvRef="MS" accession="MS:1000515" name="intensity array" unitCvRef="MS" unitAccession="MS:1000131" unitName="number of counts"/>
			<cvParam cvRef="IMS" accession="IMS:1000101" name="external data" value="true"/>
			%s
		</referenceableParamGroup>
	</referenceableParamGroupList>
	'

.get_defaultInstrumentConfigurationRef <- function(object) {
	tag <- object[["instrumentConfigurationList"]]
	if ( is.null(tag) ) {
		"IC1"
	} else {
		names(tag)[1L]
	}
}

.get_defaultDataProcessingRef <- function(object) {
	tag <- object[["dataProcessingList"]]
	if ( is.null(tag) ) {
		"CardinalProcessing"
	} else {
		names(tag)[1L]
	}
}

.new_imzML_skeleton <- function(object)
{
	if ( !is(object, "ImzML") )
		stop("'object' must inherit from 'ImzML'")
	# deparse top-level imzML tags
	fileDescription <- .deparse_fileDescription(object)
	sampleList <- .deparse_sampleList(object)
	scanSettingsList <- .deparse_scanSettingsList(object)
	softwareList <- .deparse_softwareList(object)
	instrumentConfigurationList <- .deparse_instrumentConfigurationList(object)
	dataProcessingList <- .deparse_dataProcessingList(object)
	# get spectrum representation
	fileContent <- object[["fileDescription"]][["fileContent"]]
	representation <- find_descendants_in(fileContent, "MS:1000525", "ms")
	if ( length(representation) != 1L )
		stop("couldn't determine spectrum representation")
	representation <- representation[[1L]]["name"]
	# get m/z array data type
	mzArrays <- object[["run"]][["spectrumList"]][["mzArrays"]]
	mz.type <- unique(mzArrays[["binary data type"]])
	if ( length(mz.type) != 1L )
		stop("couldn't determine m/z array binary data type")
	# get intensity array data type
	intensityArrays <- object[["run"]][["spectrumList"]][["intensityArrays"]]
	intensity.type <- unique(intensityArrays[["binary data type"]])
	if ( length(intensity.type) != 1L )
		stop("couldn't determine intensity array binary data type")
	# get # of spectra
	n <- nrow(object[["run"]][["spectrumList"]][["positions"]])
	# create the imzML skeleton
	referenceableParamGroupList <- .new_referenceableParamGroupList(representation, mz.type, intensity.type)
	sprintf(.imzML_skeleton, fileDescription, referenceableParamGroupList,
		sampleList, scanSettingsList, softwareList,
		instrumentConfigurationList, dataProcessingList,
		.get_defaultInstrumentConfigurationRef(object), n,
		.get_defaultDataProcessingRef(object))
}

.imzML_skeleton <-
	'<mzML xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://psi.hupo.org/ms/mzml http://psidev.info/files/ms/mzML/xsd/mzML1.1.0.xsd" xmlns="http://psi.hupo.org/ms/mzml" version="1.1">
		<cvList count="3">
			<cv id="UO" fullName="Unit Ontology" version="releases/2020-03-10" URI="http://ontologies.berkeleybop.org/uo.obo"/>
			<cv id="MS" fullName="Proteomics Standards Initiative Mass Spectrometry Ontology" version="4.1.0" URI="https://raw.githubusercontent.com/hupo-psi/psi-ms-cv/master/psi-ms.obo"/>
			<cv id="IMS" fullName="Imaging MS Ontology" version="1.1.0" URI="https://raw.githubusercontent.com/imzML/imzML/master/imagingMS.obo"/>
		</cvList>
		%s%s%s%s%s%s%s
		<run id="Experiment01" defaultInstrumentConfigurationRef="%s">
			<spectrumList count="%d" defaultDataProcessingRef="%s">
				<spectrum id="Spectrum=1" defaultArrayLength="0" index="1">
					<referenceableParamGroupRef ref="spectrum1"/>
					<scanList count="1">
						<cvParam cvRef="MS" accession="MS:1000795" name="no combination"/>
						<scan>
							<referenceableParamGroupRef ref="scan1"/>
						</scan>
					</scanList>
					<binaryDataArrayList count="2">
						<binaryDataArray encodedLength="0">
							<referenceableParamGroupRef ref="mzArray"/>
							<binary/>
						</binaryDataArray>
						<binaryDataArray encodedLength="0">
							<referenceableParamGroupRef ref="intensityArray"/>
							<binary/>
						</binaryDataArray>
					</binaryDataArrayList>
				</spectrum>
			</spectrumList>
		</run>
	</mzML>
	'
