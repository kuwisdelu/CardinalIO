
#### Write an imzML file ####
## ---------------------------

setGeneric("writeImzML", function(object, ...) standardGeneric("writeImzML"))

writeImzML <- function(object, file, ...)
{
	if ( !is(object, "ImzML") )
		stop("'object' must inherit from 'ImzML'")
	if ( !is.character(file) || length(file) != 1L )
		stop("'file' must be a single string")
	positions <- object[["run"]][["spectrumList"]][["positions"]]
	mzArrays <- object[["run"]][["spectrumList"]][["mzArrays"]]
	intensityArrays <- object[["run"]][["spectrumList"]][["intensityArrays"]]
	if ( is.null(positions) )
		stop("missing required component $run$spectrumList$positions")
	if ( is.null(mzArrays) )
		stop("missing required component $run$spectrumList$mzArrays")
	if ( is.null(intensityArrays) )
		stop("missing required component $run$spectrumList$intensityArrays")
	mzML <- .new_imzML_skeleton(object)
	.Call(C_writeImzML, mzML, positions, mzArrays, intensityArrays, file)
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
	fileContent <- .deparse_imzplist(tag[["fileContent"]], "fileContent")
	sourceFileList <- .deparse_taglist(tag[["sourceFileList"]], "sourceFileList", "sourceFile")
	contact <- .deparse_imzplist(tag[["contact"]], "contact")
	sprintf('<%s>\n%s%s%s</%s>\n',
		tagname, fileContent, sourceFileList, contact, tagname)
}

# optional
.deparse_sampleList <- function(object) {
	tag <- object[["sampleList"]]
	if ( is.null(tag) ) {
		sprintf(
			'<sampleList count="1">
				<sample id="%s" name="Sample1">
					<cvParam cvRef="MS" accession="MS:1000001" name="sample number" value="1"/>
				</sample>
			</sampleList>
			', .get_sampleRef(NULL))
	} else {
		.deparse_taglist(tag, "sampleList", "sample")
	}
}

# optional
.deparse_scanSettingsList <- function(object) {
	.deparse_taglist(object[["scanSettingsList"]], "scanSettingsList", "scanSettings")
}

# required
.deparse_softwareList <- function(object) {
	tag <- object[["softwareList"]]
	if ( is.null(tag) ) {
		sprintf(
			'<softwareList count="1">
				<software id="CardinalIO" version="%s">
					<cvParam cvRef="MS" accession="MS:1000799" name="custom unreleased software tool" value="CardinalIO"/>
				</software>
			</softwareList>
			', packageVersion("CardinalIO"))
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
		'<dataProcessingList count="1">
			<dataProcessing id="CardinalIOExport">
				<processingMethod order="1" softwareRef="CardinalIO">
					<cvParam cvRef="MS" accession="MS:1000544" name="Conversion to mzML"/>
				</processingMethod>
			</dataProcessing>
		</dataProcessingList>
		'
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
	# m/z array data type
	mz.type <- c(cv="MS",
		id=unname(find_term(mz.type, "ms", value="accession")),
		name=unname(find_term(mz.type, "ms", value="name")))
	mz.type <- .deparse_cvParam(mz.type)
	# intensity array data type
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
	ms <- get_obo("ms")
	fileContent <- object[["fileDescription"]][["fileContent"]]
	representation_ids <- get_descendants(ms, "MS:1000525")
	representation_ids <- which(names(fileContent) %in% representation_ids)
	if ( length(representation_ids) != 1L )
		stop("couldn't determine spectrum representation")
	representation <- fileContent[[representation_ids]]["name"]
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
		.get_defaultInstrumentConfigurationRef(object),
		.get_sampleRef(object), n,
		.get_defaultDataProcessingRef(object))
}

.get_defaultInstrumentConfigurationRef <- function(object) {
	tag <- object[["instrumentConfigurationList"]]
	if ( is.null(tag) ) {
		"IC1"
	} else {
		names(tag)[1L]
	}
}

.get_sampleRef <- function(object) {
	tag <- object[["sampleList"]]
	if ( is.null(tag) ) {
		"sample1"
	} else {
		names(tag)[1L]
	}
}

.get_defaultDataProcessingRef <- function(object) {
	tag <- object[["dataProcessingList"]]
	if ( is.null(tag) ) {
		"CardinalIOExport"
	} else {
		names(tag)[1L]
	}
}

.imzML_skeleton <-
	'<mzML xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://psi.hupo.org/ms/mzml http://psidev.info/files/ms/mzML/xsd/mzML1.1.0.xsd" xmlns="http://psi.hupo.org/ms/mzml" version="1.1">
		<cvList count="3">
			<cv id="UO" fullName="Unit Ontology" version="releases/2020-03-10" URI="http://ontologies.berkeleybop.org/uo.obo"/>
			<cv id="MS" fullName="Proteomics Standards Initiative Mass Spectrometry Ontology" version="4.1.0" URI="https://raw.githubusercontent.com/hupo-psi/psi-ms-cv/master/psi-ms.obo"/>
			<cv id="IMS" fullName="Imaging MS Ontology" version="1.1.0" URI="https://raw.githubusercontent.com/imzML/imzML/master/imagingMS.obo"/>
		</cvList>
		%s%s%s%s%s%s%s
		<run id="Experiment01" defaultInstrumentConfigurationRef="%s" sampleRef="%s">
			<spectrumList count="%d" defaultDataProcessingRef="%s">
				<spectrum id="Spectrum=1" defaultArrayLength="0" index="1">
					<referenceableParamGroupRef ref="spectrum1"/>
					<scanList count="1">
						<cvParam cvRef="MS" accession="MS:1000795" name="no combination"/>
						<scan instrumentConfigurationRef="IC1">
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
