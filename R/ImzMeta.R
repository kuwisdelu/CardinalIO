
#### ImzMeta class ####
## --------------------

setClass("ImzMeta", contains = "SimpleList")

ImzMeta <- function(...) .new_ImzMeta(...)

.meta_ms_tags <- list(
	# file content
	spectrumType = 				"MS:1000559",
	spectrumRepresentation = 	"MS:1000525",
	# contact
	contactName = 				"MS:1000586",
	contactAffiliation = 		"MS:1000590",
	contactEmail = 				"MS:1000589",
	# instrument configuration
	instrumentModel = 			"MS:1000031",
	# source
	ionSource = 				"MS:1000008",
	# analyzer
	analyzer = 					"MS:1000443",
	# detector
	detectorType = 				"MS:1000026",
	# data processing
	dataProcessing = 			"MS:1000543")

.meta_ims_tags <- list(
	# scan settings
	lineScanSequence = 			"IMS:1000040",
	scanPattern = 				"IMS:1000041",
	scanType = 					"IMS:1000048",
	lineScanDirection = 		"IMS:1000049",
	pixelSize = 				"IMS:1000046")

# names of expected ImzMeta tags
.get_all_meta_tags <- function() c(.meta_ms_tags, .meta_ims_tags)

# these tags set 'value' attributes (don't check)
.free_meta_tags <- c(
	"contactName",
	"contactAffiliation",
	"contactEmail",
	"pixelSize")

# names of ms tags to validate CV names/accessions
.get_cv_meta_ms_tagnames <- function() {
	setdiff(names(.meta_ms_tags), .free_meta_tags)
}

# names of ims tags to validate CV names/accessions
.get_cv_meta_ims_tagnames <- function() {
	setdiff(names(.meta_ims_tags), .free_meta_tags)
}

# names of ImzMeta tags to validate CV names/accessions
.get_cv_meta_tagnames <- function() {
	setdiff(names(.get_all_meta_tags()), .free_meta_tags)
}

# check an ImzMeta tag
.check_tag <- function(tag, name, parent_id, obo)
{
	if ( !is.null(name) ) {
		obo <- get_obo(obo)
		ids <- get_descendants(obo, parent_id, exclude_roots=TRUE)
		names <- obo$name[ids]
		if ( !all(name %in% names) ) {
			names <- paste0(sQuote(names), collapse=", ")
			return(paste0(tag, " must be one of :", names))
		}
	}
	NULL
}

.valid_ImzMeta <- function(object)
{
	errors <- NULL
	missing_tags <- setdiff(names(.get_all_meta_tags()), names(object))
	if ( length(missing_tags) > 0L )
		errors <- c(errors , paste0("required tags are missing: ",
			paste0(sQuote(missing_tags), collapse=", ")))
	cls_ok <- vapply(object, is, logical(1L), class2="character_OR_NULL")
	if ( !all(cls_ok) )
		errors <- c(errors , "all tags must be character or NULL")
	ms_tags_to_check <- intersect(.get_cv_meta_ms_tagnames(), names(object))
	ims_tags_to_check <- intersect(.get_cv_meta_ims_tagnames(), names(object))
	ms_tag_problems <- unlist(Map(.check_tag,
		tag=ms_tags_to_check,
		name=as.list(object)[ms_tags_to_check],
		parent_id=.get_all_meta_tags()[ms_tags_to_check],
		obo="ms"))
	ims_tag_problems <- unlist(Map(.check_tag,
		tag=ims_tags_to_check,
		name=as.list(object)[ims_tags_to_check],
		parent_id=.get_all_meta_tags()[ims_tags_to_check],
		obo="ims"))
	if ( !is.null(ms_tag_problems) )
		errors <- c(errors, ms_tag_problems)
	if ( !is.null(ims_tag_problems) )
		errors <- c(errors, ims_tag_problems)
	if ( is.null(errors) ) TRUE else errors
}

setValidity("ImzMeta", .valid_ImzMeta)

setReplaceMethod("[[", "ImzMeta", 
	function(x, i, j, ..., value)
	{
		if ( !is.character(i) || length(i) != 1L )
			stop("subscript must be a single string")
		if ( i %in% .get_cv_meta_tagnames() )
		{
			if ( i %in% .get_cv_meta_ms_tagnames() ) {
				obo <- "ms"
			} else if ( i %in% .get_cv_meta_ims_tagnames() ) {
				obo <- "ims"
			} else {
				stop("ImzMeta tag not recognized: ", sQuote(i))
			}
			value <- find_term(value, obo=obo, value="name")
		} else if ( i %in% names(.get_all_meta_tags()) ) {
			value <- as.character(value)
		} else {
			tagnames <- names(.get_all_meta_tags())
			possible <- grep(i, tagnames, ignore.case=TRUE, value=TRUE)
			possible <- sQuote(possible)
			if ( length(possible) == 0L )
				possible <- "[no matches]"
			possible <- paste0(possible, collapse=", ")
			stop("ImzMeta tag not recognized: ", sQuote(i), "; ",
				" did you mean: ", possible, "?")
		}
		x <- callNextMethod(x, i, ..., value=value)
		if ( validObject(x) )
			x
	})

setClassUnion("character_OR_NULL", c("character", "NULL"))

# needed to make [[<-, etc. work
setAs("list", "ImzMeta", function(from) {
	from <- SimpleList(from)
	from@elementType <- "character_OR_NULL"
	as(from, "ImzMeta")
})

# construct empty ImzMeta
.empty_ImzMeta <- function()
{
	tags <- names(.get_all_meta_tags())
	object <- rep(list(NULL), length(tags))
	object <- setNames(object, tags)
	object <- SimpleList(object)
	object@elementType <- "character_OR_NULL"
	as(object, "ImzMeta")
}

# construct new ImzMeta from arguments
.new_ImzMeta <- function(..., validate = TRUE)
{
	args <- list(...)
	if ( length(args) == 1L && (is.list(args[[1L]]) || is(args[[1L]], "List")) )
		args <- args[[1L]]
	object <- .empty_ImzMeta()
	for ( nm in names(args) )
		object[[nm]] <- args[[nm]]
	if ( validate )
		validObject(object)
	object
}

.show_list_values <- function(x, n = 12L, sep=", ")
{
	for ( i in seq_along(x) ) {
		len <- length(x[[i]])
		vals <- x[[i]]
		if ( len > n )
			vals <- paste0(head(vals, n=n), "...")
		vals <- paste0(vals, collapse=sep)
		size <- paste0("(", len, ")")
		cat("$", names(x)[i], size, ": ", vals, "\n", sep="")
	}
}

setMethod("show", "ImzMeta", function(object) {
	description <- "Mass spectrometry imaging experimental metadata"
	cat(class(object), ": ", description, "\n\n", sep="")
	.show_list_values(object, n=7L)
})

#### ImzML to ImzMeta conversion ####
## ----------------------------------

.find_single_term_or_NULL <- function(list, term, obo, attr)
{
	if ( is.null(list) )
		return(NULL)
	termslist <- find_descendants_in(list, term, obo)
	if ( length(termslist) >= 1L ) {
		termslist[[1L]][attr]
	} else {
		NULL
	}
}

.convert_ImzML_to_ImzMeta <- function(from)
{
	tags <- .get_all_meta_tags()
	fileContent <- from[["fileDescription"]][["fileContent"]]
	contact <- from[["fileDescription"]][["contact"]]
	ss1 <- from[["scanSettingsList"]][[1L]]
	ic1 <- from[["instrumentConfigurationList"]][[1L]]
	.new_ImzMeta(
		spectrumType=.find_single_term_or_NULL(
			fileContent, tags$spectrumType, "ms", "name"),
		spectrumRepresentation=.find_single_term_or_NULL(
			fileContent, tags$spectrumRepresentation, "ms", "name"),
		contactName=.find_single_term_or_NULL(
			contact, tags$contactName, "ms", "value"),
		contactAffiliation=.find_single_term_or_NULL(
			contact, tags$contactAffiliation, "ms", "value"),
		contactEmail=.find_single_term_or_NULL(
			contact, tags$contactEmail, "ms", "value"),
		instrumentModel=.find_single_term_or_NULL(
			ic1, tags$instrumentModel, "ms", "name"),
		ionSource=.find_single_term_or_NULL(
			ic1$componentList$source,
			tags$ionSource, "ms", "name"),
		analyzer=.find_single_term_or_NULL(
			ic1$componentList$analyzer,
			tags$analyzer, "ms", "name"),
		detectorType=.find_single_term_or_NULL(
			ic1$componentList$detector,
			tags$detectorType, "ms", "name"),
		lineScanSequence=.find_single_term_or_NULL(
			ss1, tags$lineScanSequence, "ims", "name"),
		scanPattern=.find_single_term_or_NULL(
			ss1, tags$scanPattern, "ims", "name"),
		scanType=.find_single_term_or_NULL(
			ss1, tags$scanType, "ims", "name"),
		lineScanDirection=.find_single_term_or_NULL(
			ss1, tags$lineScanDirection, "ims", "name"),
		pixelSize=.find_single_term_or_NULL(
			ss1, tags$pixelSize, "ims", "value"))
}

#### ImzMeta to ImzML conversion ####
## ----------------------------------

.cvparam <- function(cv, id, name, value)
{
	if ( missing(value) ) {
		setNames(c(cv, id, name), c("cv", "id", "name"))
	} else {
		setNames(c(cv, id, name, value), c("cv", "id", "name", "value"))
	}
}

.fileDescription_from_meta <- function(from)
{
	ms <- get_obo("ms")
	fileContent <- structure(list(), class="imzplist")
	# get spectrum type
	if ( !is.null(from$spectrumType) ) {
		id <- ms$id[ms$name %in% from$spectrumType]
		fileContent[[id]] <- .cvparam("MS", id, from$spectrumType)
	} else {
		stop("missing spectrumType")
	}
	# get spectrum representation
	if ( !is.null(from$spectrumRepresentation) ) {
		id <- ms$id[ms$name %in% from$spectrumRepresentation]
		fileContent[[id]] <- .cvparam("MS", id, from$spectrumRepresentation)
	} else {
		stop("missing spectrumRepresentation")
	}
	# get (optional) contact information
	contact <- structure(list(), class="imzplist")
	if ( !is.null(from$contactName) ) {
		id <- .get_all_meta_tags()$contactName
		contact[[id]] <- .cvparam("MS", id, ms$name[id], from$contactName)
	}
	if ( !is.null(from$contactAffiliation) ) {
		id <- .get_all_meta_tags()$contactAffiliation
		contact[[id]] <- .cvparam("MS", id, ms$name[id], from$contactAffiliation)
	}
	if ( !is.null(from$contactEmail) ) {
		id <- .get_all_meta_tags()$contactEmail
		contact[[id]] <- .cvparam("MS", id, ms$name[id], from$contactEmail)
	}
	# return file description
	if ( length(contact) > 0L ) {
		list(fileContent=fileContent, contact=contact)
	} else {
		list(fileContent=fileContent)
	}
}

.scanSettingsList_from_meta <- function(from)
{
	ssid <- "scansettings1"
	ims <- get_obo("ims")
	scanSettings <- structure(list(), id=ssid, class="imzplist")
	# get line scan sequence (e.g., top down)
	if ( !is.null(from$lineScanSequence) ) {
		id <- ims$id[ims$name %in% from$lineScanSequence]
		scanSettings[[id]] <- .cvparam("IMS", id, from$lineScanSequence)
	}
	# get scan pattern (e.g., meandering, flyback, etc.)
	if ( !is.null(from$scanPattern) ) {
		id <- ims$id[ims$name %in% from$scanPattern]
		scanSettings[[id]] <- .cvparam("IMS", id, from$scanPattern)
	}
	# get scan type (e.g., horizontal line scan)
	if ( !is.null(from$scanType) ) {
		id <- ims$id[ims$name %in% from$scanType]
		scanSettings[[id]] <- .cvparam("IMS", id, from$scanType)
	}
	# get line scan direction (e.g., line scan left right)
	if ( !is.null(from$lineScanDirection) ) {
		id <- ims$id[ims$name %in% from$lineScanDirection]
		scanSettings[[id]] <- .cvparam("IMS", id, from$lineScanDirection)
	}
	# get pixel size (in micrometers)
	if ( !is.null(from$pixelSize) ) {
		id <- .get_all_meta_tags()$pixelSize
		scanSettings[[id]] <- .cvparam("IMS", id, ims$name[id], from$pixelSize)
	}
	# return scan settings list
	if ( length(scanSettings) > 0L ) {
		setNames(list(scanSettings), ssid)
	} else {
		NULL
	}
}

.softwareList_default <- function()
{
	if ( isNamespaceLoaded("Cardinal") ) {
		package <- "Cardinal"
	} else {
		package <- "CardinalIO"
	}
	ms <- get_obo("ms")
	software <- structure(list(), id=package,
		version=packageVersion(package), class="imzplist")
	id <- "MS:1000799"
	software[[id]] <- .cvparam("MS", id, ms$name[id], package)
	setNames(list(software), package)
}

.instrumentConfigurationList_default <- function()
{
	ms <- get_obo("ms")
	icid <- "ic1"
	instrumentConfiguration <- structure(list(), id=icid, class="imzplist")
	id <- "MS:1000031"
	instrumentConfiguration[[id]] <- .cvparam("MS", id, ms$name[id])
	setNames(list(instrumentConfiguration), icid)
}

.instrumentConfigurationList_from_meta <- function(from)
{
	ms <- get_obo("ms")
	icid <- paste0(gsub(" ", "", from$instrumentModel), "1")
	instrumentConfiguration <- structure(list(), id=icid, class="imzplist")
	if ( !is.null(from$instrumentModel) ) {
		id <- ms$id[ms$name %in% from$instrumentModel]
		instrumentConfiguration[[id]] <- .cvparam("MS", id, ms$name[id])
	}
	componentList <- list()
	order <- 0L
	# get ion source
	if ( !is.null(from$ionSource) ) {
		order <- order + 1L
		source <- structure(list(), order=paste0(order), class="imzplist")
		id <- ms$id[ms$name %in% from$ionSource]
		source[[id]] <- .cvparam("MS", id, from$ionSource)
		componentList$source <- source
	}
	# get mass analyzer
	if ( !is.null(from$analyzer) ) {
		order <- order + 1L
		analyzer <- structure(list(), order=paste0(order), class="imzplist")
		id <- ms$id[ms$name %in% from$analyzer]
		analyzer[[id]] <- .cvparam("MS", id, from$analyzer)
		componentList$analyzer <- analyzer
	}
	# get detector type
	if ( !is.null(from$detectorType) ) {
		order <- order + 1L
		detector <- structure(list(), order=paste0(order), class="imzplist")
		id <- ms$id[ms$name %in% from$detectorType]
		detector[[id]] <- .cvparam("MS", id, from$detectorType)
		componentList$detector <- detector
	}
	# return instrument configuration list
	if ( order > 0L )
		instrumentConfiguration$componentList <- componentList
	if ( length(instrumentConfiguration) > 0L ) {
		setNames(list(instrumentConfiguration), icid)
	} else {
		.instrumentConfigurationList_default()
	}
}

.dataProcessingList_default <- function()
{
	if ( isNamespaceLoaded("Cardinal") ) {
		package <- "Cardinal"
	} else {
		package <- "CardinalIO"
	}
	ms <- get_obo("ms")
	processingMethod <- structure(list(), order=paste0(1),
		softwareRef=package, class="imzplist")
	id <- "MS:1000544"
	processingMethod[[id]] <- .cvparam("MS", id, ms$name[id], package)
	dataProcessing <- setNames(list(processingMethod), package)
	setNames(list(dataProcessing), "CardinalProcessing")
}

.dataProcessingList_from_meta <- function(from)
{
	dataProcessingList <- .dataProcessingList_default()
	ms <- get_obo("ms")
	for ( processing in from$dataProcessing ) {
		id <- ms$id[ms$name %in% from$processing]
		dataProcessingList[[1L]][[1L]][[id]] <- .cvparam("MS", id, from$processing)
	}
	dataProcessingList
}

.convert_ImzMeta_to_ImzML <- function(from)
{
	.new_ImzML(
		fileDescription=.fileDescription_from_meta(from),
		scanSettingsList=.scanSettingsList_from_meta(from),
		softwareList=.softwareList_default(),
		instrumentConfigurationList=.instrumentConfigurationList_from_meta(from),
		dataProcessingList=.dataProcessingList_from_meta(from),
		run=list())
}

