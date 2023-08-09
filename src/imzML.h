
#ifndef IMZML
#define IMZML

#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>

#include "pugixml.h"

// define required cvParam accessions
#define MS_MZ_ARRAY_ID 					"MS:1000514"
#define MS_INTENSITY_ARRAY_ID 			"MS:1000515"
#define IMS_POSITION_X_ID 				"IMS:1000050"
#define IMS_POSITION_Y_ID 				"IMS:1000051"
#define IMS_POSITION_Z_ID 				"IMS:1000052"
#define IMS_EXTERNAL_OFFSET_ID 			"IMS:1000102"
#define IMS_EXTERNAL_ARRAY_LENGTH_ID 	"IMS:1000103"
#define IMS_EXTERNAL_ENCODED_LENGTH_ID 	"IMS:1000104"
#define MS_32_BIT_INTEGER_ID			"MS:1000519"
#define MS_64_BIT_INTEGER_ID			"MS:1000522"
#define MS_32_BIT_FLOAT_ID				"MS:1000521"
#define MS_64_BIT_FLOAT_ID				"MS:1000523"
#define IMS_8_BIT_INTEGER_ID			"IMS:1100000"
#define IMS_16_BIT_INTEGER_ID			"IMS:1100001"
#define IMS_32_BIT_INTEGER_ID			"IMS:1000141" // obselete (included for compatibility)
#define IMS_64_BIT_INTEGER_ID			"IMS:1000142" // obselete (included for compatibility)

// define required cvParam names
#define IMS_POSITION_X_NAME 				"position x"
#define IMS_POSITION_Y_NAME					"position y"
#define IMS_POSITION_Z_NAME					"position z"
#define IMS_EXTERNAL_OFFSET_NAME 			"external offset"
#define IMS_EXTERNAL_ARRAY_LENGTH_NAME 		"external array length"
#define IMS_EXTERNAL_ENCODED_LENGTH_NAME 	"external encoded length"

#define BUFLEN 32 // length of string buffers

// make R string if valid or NA_STRING if empty
#define mkCharOrNA(x) ((*(x)) == '\0' ? NA_STRING : Rf_mkChar((x)))

// unsafe check for interrupt
inline void checkInterrupt(void * nothing)
{
	R_CheckUserInterrupt();
}

// safe check for interrupt (without longjmp)
inline bool pendingInterrupt()
{
	return !(R_ToplevelExec(checkInterrupt, NULL));
}

// find element in R list by name
inline SEXP VECTOR_ELT_BY_NAME(SEXP x, const char * name)
{
	SEXP names = Rf_getAttrib(x, R_NamesSymbol);
	for ( int i = 0; i < LENGTH(x); i++ )
	{
		if( strcmp(CHAR(STRING_ELT(names, i)), name) == 0 )
			return VECTOR_ELT(x, i);
	}
	return R_NilValue;
}

//// imzML class
//---------------

class imzML {

	public:

		//// Create and load imzML
		//-------------------------

		imzML() {}

		~imzML() {}

		bool load_file(const char * file)
		{
			pugi::xml_parse_result result;
			result = _doc.load_file(file);
			if ( result )
			{
				_mzml = _doc.child("mzML");
				_reflist = _mzml.child("referenceableParamGroupList");
				_spectra = _mzml.child("run").child("spectrumList");
			}
			return result;
		}

		bool load_string(const char * str)
		{
			pugi::xml_parse_result result;
			result = _doc.load_string(str);
			if ( result )
			{
				_mzml = _doc.child("mzML");
				_reflist = _mzml.child("referenceableParamGroupList");
				_spectra = _mzml.child("run").child("spectrumList");
			}
			return result;
		}

		bool save_file(const char * file)
		{
			return _doc.save_file(file);
		}

		//// Access major param groups
		//-----------------------------

		pugi::xml_node referenceableParamGroupList()
		{
			return _reflist;
		}	

		pugi::xml_node fileDescription()
		{
			return _mzml.child("fileDescription");
		}

		pugi::xml_node sampleList()
		{
			return _mzml.child("sampleList");
		}

		pugi::xml_node softwareList()
		{
			return _mzml.child("softwareList");
		}

		pugi::xml_node scanSettingsList()
		{
			return _mzml.child("scanSettingsList");
		}

		pugi::xml_node instrumentConfigurationList()
		{
			return _mzml.child("instrumentConfigurationList");
		}

		pugi::xml_node dataProcessingList()
		{
			return _mzml.child("dataProcessingList");
		}

		pugi::xml_node run()
		{
			return _mzml.child("run");
		}

		pugi::xml_node spectrumList()
		{
			return _spectra;
		}

		pugi::xml_node first_spectrum()
		{
			return _spectra.first_child();
		}

		pugi::xml_node last_spectrum()
		{
			return _spectra.last_child();
		}

		int num_spectra()
		{
			return _spectra.attribute("count").as_int();
		}

		//// Find params (including referenceableParamGroups)
		//---------------------------------------------------

		static bool is_cvParam(pugi::xml_node node)
		{
			return strcmp(node.name(), "cvParam") == 0;
		}

		static bool is_userParam(pugi::xml_node node)
		{
			return strcmp(node.name(), "userParam") == 0;
		}

		static bool is_param(pugi::xml_node node)
		{
			return is_cvParam(node) || is_userParam(node);
		}

		// count number of children (including children in refs)
		size_t num_children(pugi::xml_node parent, bool params_only = false, bool include_refs = true)
		{
			size_t n = 0;
			pugi::xml_node group = parent.child("referenceableParamGroupRef");
			pugi::xml_node child = parent.first_child();
			while ( child ) {
				if ( !params_only || is_param(child) )
					n++;
				child = child.next_sibling();
			}
			if ( group && include_refs ) {
				const char * id = group.attribute("ref").value();
				pugi::xml_node ref = _reflist.find_child_by_attribute("id", id);
				child = ref.first_child();
				while ( child ) {
					if ( !params_only || is_param(child) )
						n++;
					child = child.next_sibling();
				}
			}
			return n;
		}

		// count cvParams and userParams
		size_t num_params(pugi::xml_node parent, bool include_refs = true)
		{
			return num_children(parent, true);
		}

		// find a cvParam or userParam
		pugi::xml_node find_param(pugi::xml_node parent, const char * name,
			const char * attr_name, const char * attr_value)
		{
			pugi::xml_node group = parent.child("referenceableParamGroupRef");
			pugi::xml_node child = parent.find_child_by_attribute(name, attr_name, attr_value);
			if ( !child && group ) {
				const char * id = group.attribute("ref").value();
				pugi::xml_node ref = _reflist.find_child_by_attribute("id", id);
				child = ref.find_child_by_attribute(name, attr_name, attr_value);
			}
			return child;
		}

		// find a cvParam or userParam
		pugi::xml_node find_param(pugi::xml_node parent,
			const char * attr_name, const char * attr_value)
		{
			pugi::xml_node group = parent.child("referenceableParamGroupRef");
			pugi::xml_node child = parent.find_child_by_attribute(attr_name, attr_value);
			if ( !child && group ) {
				const char * id = group.attribute("ref").value();
				pugi::xml_node ref = _reflist.find_child_by_attribute("id", id);
				child = ref.find_child_by_attribute(attr_name, attr_value);
			}
			return child;
		}

		// find a specific binary data array for a spectrum
		pugi::xml_node find_binaryDataArray(pugi::xml_node spectrum, const char * id)
		{
			pugi::xml_node node = spectrum.child("binaryDataArrayList").first_child();
			while ( node ) {
				pugi::xml_node ibd = find_param(node, "cvParam", "accession", id);
				if ( ibd )
					return node;
				node = node.next_sibling();
			}
			return node; // will be null after loop
		}

		// get cvParam as R vector
		SEXP get_cvParam(pugi::xml_node node)
		{
			SEXP tag, tagNames;
			size_t n = 3; // NEED cvRef, accession, name
			if ( node.attribute("value") )
				n++;
			if ( node.attribute("unitCvRef") )
				n += 3;
			int i = 0, j = 0;
			PROTECT(tag = Rf_allocVector(STRSXP, n));
			PROTECT(tagNames = Rf_allocVector(STRSXP, n));
			SET_STRING_ELT(tag, i++, Rf_mkChar(node.attribute("cvRef").value()));
			SET_STRING_ELT(tagNames, j++, Rf_mkChar("cv"));
			SET_STRING_ELT(tag, i++, Rf_mkChar(node.attribute("accession").value()));
			SET_STRING_ELT(tagNames, j++, Rf_mkChar("id"));
			SET_STRING_ELT(tag, i++, Rf_mkChar(node.attribute("name").value()));
			SET_STRING_ELT(tagNames, j++, Rf_mkChar("name"));
			if ( node.attribute("value") ) {
				SET_STRING_ELT(tag, i++, Rf_mkChar(node.attribute("value").value()));
				SET_STRING_ELT(tagNames, j++, Rf_mkChar("value"));
			}
			if ( node.attribute("unitCvRef") ) {
				SET_STRING_ELT(tag, i++, Rf_mkChar(node.attribute("unitCvRef").value()));
				SET_STRING_ELT(tag, i++, Rf_mkChar(node.attribute("unitAccession").value()));
				SET_STRING_ELT(tag, i++, Rf_mkChar(node.attribute("unitName").value()));
				SET_STRING_ELT(tagNames, j++, Rf_mkChar("unit_cv"));
				SET_STRING_ELT(tagNames, j++, Rf_mkChar("unit_id"));
				SET_STRING_ELT(tagNames, j++, Rf_mkChar("unit_name"));
			}
			Rf_setAttrib(tag, R_NamesSymbol, tagNames);
			UNPROTECT(2);
			return tag;
		}

		// get userParam as R vector
		SEXP get_userParam(pugi::xml_node node)
		{
			SEXP tag, tagNames;
			size_t n = 2; // NEED name, value
			if ( node.attribute("unitCvRef") )
				n += 3;
			int i = 0, j = 0;
			PROTECT(tag = Rf_allocVector(STRSXP, n));
			PROTECT(tagNames = Rf_allocVector(STRSXP, n));
			SET_STRING_ELT(tag, i++, Rf_mkChar(node.attribute("name").value()));
			SET_STRING_ELT(tag, i++, Rf_mkChar(node.attribute("value").value()));
			SET_STRING_ELT(tagNames, j++, Rf_mkChar("name"));
			SET_STRING_ELT(tagNames, j++, Rf_mkChar("value"));
			if ( node.attribute("unitCvRef") ) {
				SET_STRING_ELT(tag, i++, Rf_mkChar(node.attribute("unitCvRef").value()));
				SET_STRING_ELT(tag, i++, Rf_mkChar(node.attribute("unitAccession").value()));
				SET_STRING_ELT(tag, i++, Rf_mkChar(node.attribute("unitName").value()));
				SET_STRING_ELT(tagNames, j++, Rf_mkChar("unit_cv"));
				SET_STRING_ELT(tagNames, j++, Rf_mkChar("unit_id"));
				SET_STRING_ELT(tagNames, j++, Rf_mkChar("unit_name"));
			}
			Rf_setAttrib(tag, R_NamesSymbol, tagNames);
			UNPROTECT(2);
			return tag;
		}

		// get cvParam/userParam as R vector
		SEXP get_param(pugi::xml_node node)
		{
			if ( is_cvParam(node) )
				return get_cvParam(node);
			else if ( is_userParam(node) )
				return get_userParam(node);
			else
				return R_NilValue;
		}

		// copy cvParam/userParam tags into pre-allocated R list
		int get_params(pugi::xml_node parent, int count, SEXP tags, SEXP tagsNames)
		{
			pugi::xml_node group = parent.child("referenceableParamGroupRef");
			pugi::xml_node node = parent.first_child();
			size_t i = 0;
			while ( node && i < count ) {
				if ( is_param(node) )
				{
					SET_VECTOR_ELT(tags, i, get_param(node));
					if ( is_cvParam(node) )
						SET_STRING_ELT(tagsNames, i, Rf_mkChar(node.attribute("accession").value()));
					else if ( is_userParam(node) )
						SET_STRING_ELT(tagsNames, i, Rf_mkChar(node.attribute("name").value()));
					else
						SET_STRING_ELT(tagsNames, i, R_BlankString);
					i++;
				}
				node = node.next_sibling();
				if ( !node && group )
				{
					const char * id = group.attribute("ref").value();
					pugi::xml_node ref = _reflist.find_child_by_attribute("id", id);
					group = node; // becomes null
					node = ref.first_child();
				}
			}
			if ( i != count )
				Rf_warning("did not read expected number of params in %s", parent.name());
			pugi::xml_attribute attr = parent.first_attribute();
			while ( attr ) {
				Rf_setAttrib(tags, Rf_install(attr.name()), Rf_mkString(attr.value()));
				attr = attr.next_attribute();
			}
			Rf_setAttrib(tags, R_NamesSymbol, tagsNames);
			Rf_setAttrib(tags, R_ClassSymbol, Rf_mkString("imzplist"));
			return i;
		}

		// get cvParam/userParam tags as R list
		SEXP get_params(pugi::xml_node parent)
		{
			size_t n = num_params(parent);
			SEXP tags, tagsNames;
			PROTECT(tags = Rf_allocVector(VECSXP, n));
			PROTECT(tagsNames = Rf_allocVector(STRSXP, n));
			get_params(parent, n, tags, tagsNames);
			UNPROTECT(2);
			return tags;
		}

		// get list of IDed cvParam/userParam containers as R list
		SEXP get_params_list(pugi::xml_node parent, const char * id = "id")
		{
			SEXP tags, tagsNames;
			pugi::xml_attribute count = parent.attribute("count");
			int n = count ? count.as_int() : num_children(parent);
			if ( n == 0 )
				return R_NilValue;
			PROTECT(tags = Rf_allocVector(VECSXP, n));
			PROTECT(tagsNames = Rf_allocVector(STRSXP, n));
			pugi::xml_node node = parent.first_child();
			const char * parentName = parent.name();
			const char * nodeName = node.name();
			int i = 0;
			while ( node && i < n ) {
				if ( i >= n ) {
					Rf_warning("more %ss than %s 'count'", nodeName, parentName);
					break;
				}
				SET_VECTOR_ELT(tags, i, get_params(node));
				SET_STRING_ELT(tagsNames, i, mkCharOrNA(node.attribute(id).value()));
				node = node.next_sibling();
				i++;
			}
			if ( i < n )
				Rf_warning("fewer %ss than %s 'count'", nodeName, parentName);
			Rf_setAttrib(tags, R_NamesSymbol, tagsNames);
			UNPROTECT(2);
			return tags;
		}

		//// Get experiment metadata
		//---------------------------

		SEXP get_fileDescription()
		{
			pugi::xml_node fileContent = fileDescription().child("fileContent");
			pugi::xml_node sourceFileList = fileDescription().child("sourceFileList");
			pugi::xml_node contact = fileDescription().child("contact");
			SEXP tags, tagsNames;
			bool has_sourceFileList = !sourceFileList.empty();
			bool has_contact = !contact.empty();
			int n = 1 + has_sourceFileList + has_contact;
			PROTECT(tags = Rf_allocVector(VECSXP, n));
			PROTECT(tagsNames = Rf_allocVector(STRSXP, n));
			int i = 0, j = 0;
			SET_VECTOR_ELT(tags, i++, get_params(fileContent));
			SET_STRING_ELT(tagsNames, j++, Rf_mkChar("fileContent"));
			if ( has_sourceFileList ) {
				SET_VECTOR_ELT(tags, i++, get_params_list(sourceFileList));
				SET_STRING_ELT(tagsNames, j++, Rf_mkChar("sourceFileList"));
			}
			if ( has_contact ) {
				SET_VECTOR_ELT(tags, i++, get_params(contact));
				SET_STRING_ELT(tagsNames, j++, Rf_mkChar("contact"));
			}
			Rf_setAttrib(tags, R_NamesSymbol, tagsNames);
			UNPROTECT(2);
			return tags;
		}

		SEXP get_sampleList()
		{
			return get_params_list(sampleList());
		}

		SEXP get_softwareList()
		{
			return get_params_list(softwareList());
		}

		SEXP get_scanSettingsList()
		{
			return get_params_list(scanSettingsList());
		}

		// part of instrumentConfiguration
		SEXP get_componentList(pugi::xml_node node)
		{
			if ( strcmp(node.name(), "componentList") != 0 )
				return R_NilValue;
			SEXP tags, tagsNames;
			int n = 3; // source, analyzer, detector
			PROTECT(tags = Rf_allocVector(VECSXP, n));
			PROTECT(tagsNames = Rf_allocVector(STRSXP, n));
			node = node.first_child();
			int i = 0;
			while ( node && i < n ) {
				SET_VECTOR_ELT(tags, i, get_params(node));
				SET_STRING_ELT(tagsNames, i, Rf_mkChar(node.name()));
				node = node.next_sibling();
				i++;
			}
			if ( i != n ) {
				const char * id = node.parent().attribute("id").value();
				if ( i < n )
					Rf_warning("missing instrumentConfiguration components for %s", id);
				if ( i > n )
					Rf_warning("more instrumentConfiguration components than expected for %s", id);
			}
			Rf_setAttrib(tags, R_NamesSymbol, tagsNames);
			UNPROTECT(2);
			return tags;
		}

		// part of instrumentConfigurationList
		SEXP get_instrumentConfiguration(pugi::xml_node node)
		{
			if ( strcmp(node.name(), "instrumentConfiguration") != 0 )
				return R_NilValue;
			pugi::xml_node componentList = node.child("componentList");
			pugi::xml_node softwareRef = node.child("softwareRef");
			bool has_componentList = !componentList.empty();
			bool has_softwareRef = !softwareRef.empty();
			SEXP tags, tagsNames;
			int count = num_params(node);
			int n = count + has_componentList + has_softwareRef;
			PROTECT(tags = Rf_allocVector(VECSXP, n));
			PROTECT(tagsNames = Rf_allocVector(STRSXP, n));
			get_params(node, count, tags, tagsNames);
			int i = count, j = count;
			if ( has_componentList ) {
				SET_VECTOR_ELT(tags, i++, get_componentList(componentList));
				SET_STRING_ELT(tagsNames, j++, Rf_mkChar("componentList"));
			}
			if ( has_softwareRef ) {
				SET_VECTOR_ELT(tags, i++, Rf_mkString(softwareRef.attribute("ref").value()));
				SET_STRING_ELT(tagsNames, j++, Rf_mkChar("softwareRef"));
			}
			UNPROTECT(2);
			return tags;
		}

		SEXP get_instrumentConfigurationList()
		{
			SEXP tags, tagsNames;
			int n = instrumentConfigurationList().attribute("count").as_int();
			PROTECT(tags = Rf_allocVector(VECSXP, n));
			PROTECT(tagsNames = Rf_allocVector(STRSXP, n));
			pugi::xml_node node = instrumentConfigurationList().first_child();
			int i = 0;
			while ( node && i < n ) {
				if ( i >= n ) {
					Rf_warning("more instrumentConfigurations than instrumentConfigurationList 'count'");
					break;
				}
				SET_VECTOR_ELT(tags, i, get_instrumentConfiguration(node));
				SET_STRING_ELT(tagsNames, i, mkCharOrNA(node.attribute("id").value()));
				node = node.next_sibling();
				i++;
			}
			if ( i < n )
				Rf_warning("fewer instrumentConfigurations than instrumentConfigurationList 'count'");
			Rf_setAttrib(tags, R_NamesSymbol, tagsNames);
			UNPROTECT(2);
			return tags;
		}

		// part of dataProcessingList
		SEXP get_dataProcessing(pugi::xml_node node)
		{
			return get_params_list(node, "softwareRef");
		}

		SEXP get_dataProcessingList()
		{
			SEXP tags, tagsNames;
			int n = dataProcessingList().attribute("count").as_int();
			PROTECT(tags = Rf_allocVector(VECSXP, n));
			PROTECT(tagsNames = Rf_allocVector(STRSXP, n));
			pugi::xml_node node = dataProcessingList().first_child();
			int i = 0;
			while ( node && i < n ) {
				if ( i >= n ) {
					Rf_warning("more dataProcessings than dataProcessingList 'count'");
					break;
				}
				SET_VECTOR_ELT(tags, i, get_dataProcessing(node));
				SET_STRING_ELT(tagsNames, i, mkCharOrNA(node.attribute("id").value()));
				node = node.next_sibling();
				i++;
			}
			if ( i < n )
				Rf_warning("fewer dataProcessings than dataProcessingList 'count'");
			Rf_setAttrib(tags, R_NamesSymbol, tagsNames);
			UNPROTECT(2);
			return tags;
		}

		//// Get spectrum metadata
		//--------------------------

		// get spectrum ids as R vector
		SEXP get_spectrum_ids()
		{
			int n = num_spectra();
			SEXP ids;
			PROTECT(ids = Rf_allocVector(STRSXP, n));
			pugi::xml_node spectrum = first_spectrum();
			int i = 0;
			while ( spectrum ) {
				// safe check for interrupt (allow xml destructor to run)
				if ( pendingInterrupt() ) {
					Rf_warning("stopping early; parse may be incomplete");
					break;
				}
				if ( i >= n ) {
					Rf_warning("more spectra than spectrumList 'count'");
					break;
				}
				SET_STRING_ELT(ids, i, Rf_mkChar(spectrum.attribute("id").value()));
				spectrum = spectrum.next_sibling();
				i++;
			}
			if ( i < n )
				Rf_warning("fewer spectra than spectrumList 'count'");
			UNPROTECT(1);
			return ids;
		}

		// get spectrum positions as R data frame
		SEXP get_spectrum_positions()
		{
			int nr = num_spectra();
			int nc = 3; // x, y, z
			SEXP positions, positionsNames, px, py, pz;
			PROTECT(positions = Rf_allocVector(VECSXP, nc));
			PROTECT(positionsNames = Rf_allocVector(STRSXP, nc));
			PROTECT(px = Rf_allocVector(STRSXP, nr));
			PROTECT(py = Rf_allocVector(STRSXP, nr));
			PROTECT(pz = Rf_allocVector(STRSXP, nr));
			SET_STRING_ELT(positionsNames, 0, Rf_mkChar("position x"));
			SET_STRING_ELT(positionsNames, 1, Rf_mkChar("position y"));
			SET_STRING_ELT(positionsNames, 2, Rf_mkChar("position z"));
			pugi::xml_node spectrum = first_spectrum();
			pugi::xml_node scan, x, y, z;
			int i = 0;
			while ( spectrum && i < nr ) {
				// safe check for interrupt (allow xml destructor to run)
				if ( pendingInterrupt() ) {
					Rf_warning("stopping early; parse may be incomplete");
					break;
				}
				scan = spectrum.child("scanList").child("scan");
				x = find_param(scan, "cvParam", "accession", IMS_POSITION_X_ID);
				y = find_param(scan, "cvParam", "accession", IMS_POSITION_Y_ID);
				z = find_param(scan, "cvParam", "accession", IMS_POSITION_Z_ID);
				SET_STRING_ELT(px, i, mkCharOrNA(x.attribute("value").value()));
				SET_STRING_ELT(py, i, mkCharOrNA(y.attribute("value").value()));
				SET_STRING_ELT(pz, i, mkCharOrNA(z.attribute("value").value()));
				spectrum = spectrum.next_sibling();
				i++;
			}
			SET_VECTOR_ELT(positions, 0, px);
			SET_VECTOR_ELT(positions, 1, py);
			SET_VECTOR_ELT(positions, 2, pz);
			Rf_setAttrib(positions, R_NamesSymbol, positionsNames);
			Rf_setAttrib(positions, R_RowNamesSymbol, get_spectrum_ids());
			Rf_setAttrib(positions, R_ClassSymbol, Rf_mkString("data.frame"));
			UNPROTECT(5);
			return positions;
		}

		// get spectrum binary data arrays as R data frame
		SEXP get_spectrum_arrays(const char * id)
		{
			int nr = num_spectra();
			int nc = 4; // offset, length, encoded length, type
			SEXP dataArrays, dataArraysNames, poff, plen, pxlen, ptype;
			PROTECT(dataArrays = Rf_allocVector(VECSXP, nc));
			PROTECT(dataArraysNames = Rf_allocVector(STRSXP, nc));
			PROTECT(poff = Rf_allocVector(STRSXP, nr));
			PROTECT(plen = Rf_allocVector(STRSXP, nr));
			PROTECT(pxlen = Rf_allocVector(STRSXP, nr));
			PROTECT(ptype = Rf_allocVector(STRSXP, nr));
			SET_STRING_ELT(dataArraysNames, 0, Rf_mkChar("external offset"));
			SET_STRING_ELT(dataArraysNames, 1, Rf_mkChar("external array length"));
			SET_STRING_ELT(dataArraysNames, 2, Rf_mkChar("external encoded length"));
			SET_STRING_ELT(dataArraysNames, 3, Rf_mkChar("binary data type"));
			pugi::xml_node spectrum = first_spectrum();
			pugi::xml_node dataArray, off, len, xlen, type;
			int i = 0;
			while ( spectrum && i < nr ) {
				// safe check for interrupt (allow xml destructor to run)
				if ( pendingInterrupt() ) {
					Rf_warning("stopping early; parse may be incomplete");
					break;
				}
				dataArray = find_binaryDataArray(spectrum, id);
				off = find_param(dataArray, "cvParam", "accession", IMS_EXTERNAL_OFFSET_ID);
				len = find_param(dataArray, "cvParam", "accession", IMS_EXTERNAL_ARRAY_LENGTH_ID);
				xlen = find_param(dataArray, "cvParam", "accession", IMS_EXTERNAL_ENCODED_LENGTH_ID);
				// is this too clever?
				if ( (type = find_param(dataArray, "cvParam", "accession", MS_32_BIT_INTEGER_ID)) ) {}
				else if ( (type = find_param(dataArray, "cvParam", "accession", MS_64_BIT_INTEGER_ID)) ) {}
				else if ( (type = find_param(dataArray, "cvParam", "accession", MS_32_BIT_FLOAT_ID)) ) {}
				else if ( (type = find_param(dataArray, "cvParam", "accession", MS_64_BIT_FLOAT_ID)) ) {}
				else if ( (type = find_param(dataArray, "cvParam", "accession", IMS_8_BIT_INTEGER_ID)) ) {}
				else if ( (type = find_param(dataArray, "cvParam", "accession", IMS_16_BIT_INTEGER_ID)) ) {}
				else if ( (type = find_param(dataArray, "cvParam", "accession", IMS_32_BIT_INTEGER_ID)) ) {}
				else if ( (type = find_param(dataArray, "cvParam", "accession", IMS_64_BIT_INTEGER_ID)) ) {}
				else {}
				SET_STRING_ELT(poff, i, mkCharOrNA(off.attribute("value").value()));
				SET_STRING_ELT(plen, i, mkCharOrNA(len.attribute("value").value()));
				SET_STRING_ELT(pxlen, i, mkCharOrNA(xlen.attribute("value").value()));
				SET_STRING_ELT(ptype, i, mkCharOrNA(type.attribute("name").value()));
				spectrum = spectrum.next_sibling();
				i++;
			}
			SET_VECTOR_ELT(dataArrays, 0, poff);
			SET_VECTOR_ELT(dataArrays, 1, plen);
			SET_VECTOR_ELT(dataArrays, 2, pxlen);
			SET_VECTOR_ELT(dataArrays, 3, ptype);
			Rf_setAttrib(dataArrays, R_NamesSymbol, dataArraysNames);
			Rf_setAttrib(dataArrays, R_RowNamesSymbol, get_spectrum_ids());
			Rf_setAttrib(dataArrays, R_ClassSymbol, Rf_mkString("data.frame"));
			UNPROTECT(6);
			return dataArrays;
		}

		SEXP get_spectrum_mzArrays()
		{
			return get_spectrum_arrays(MS_MZ_ARRAY_ID);
		}

		SEXP get_spectrum_intensityArrays()
		{
			return get_spectrum_arrays(MS_INTENSITY_ARRAY_ID);
		}

		// get additional spectrum tags as R data frame
		SEXP get_spectrum_extra(SEXP names)
		{
			if ( Rf_isNull(names) )
				return R_NilValue;
			int nr = num_spectra();
			int nc = LENGTH(names);
			SEXP extra;
			PROTECT(extra = Rf_allocVector(VECSXP, nc));
			for ( int j = 0; j < nc; j++ )
				SET_VECTOR_ELT(extra, j, Rf_allocVector(STRSXP, nr));
			pugi::xml_node spectrum = first_spectrum();
			pugi::xml_node scan, tag;
			int i = 0;
			while ( spectrum && i < nr ) {
				// safe check for interrupt (allow xml destructor to run)
				if ( pendingInterrupt() ) {
					Rf_warning("stopping early; parse may be incomplete");
					break;
				}
				scan = spectrum.child("scanList").child("scan");
				for ( int j = 0; j < nc; j++ )
				{
					tag = find_param(spectrum, "name", CHAR(STRING_ELT(names, j)));
					if ( !tag )
						tag = find_param(scan, "name", CHAR(STRING_ELT(names, j)));
					SEXP x = VECTOR_ELT(extra, j);
					SET_STRING_ELT(x, i, mkCharOrNA(tag.attribute("value").value()));
				}
				spectrum = spectrum.next_sibling();
				i++;
			}
			Rf_setAttrib(extra, R_NamesSymbol, names);
			Rf_setAttrib(extra, R_RowNamesSymbol, get_spectrum_ids());
			Rf_setAttrib(extra, R_ClassSymbol, Rf_mkString("data.frame"));
			UNPROTECT(1);
			return extra;
		}

		SEXP get_spectrumList(SEXP extra)
		{
			SEXP tags, tagsNames;
			int n = 3; // positions, mzArrays, intensityArrays
			if ( !Rf_isNull(extra) )
				n++;
			PROTECT(tags = Rf_allocVector(VECSXP, n));
			PROTECT(tagsNames = Rf_allocVector(STRSXP, n));
			SET_VECTOR_ELT(tags, 0, get_spectrum_positions());
			SET_VECTOR_ELT(tags, 1, get_spectrum_mzArrays());
			SET_VECTOR_ELT(tags, 2, get_spectrum_intensityArrays());
			SET_STRING_ELT(tagsNames, 0, Rf_mkChar("positions"));
			SET_STRING_ELT(tagsNames, 1, Rf_mkChar("mzArrays"));
			SET_STRING_ELT(tagsNames, 2, Rf_mkChar("intensityArrays"));
			if ( !Rf_isNull(extra) )
			{
				SET_VECTOR_ELT(tags, 3, get_spectrum_extra(extra));
				SET_STRING_ELT(tagsNames, 3, Rf_mkChar("extra"));
			}
			Rf_setAttrib(tags, R_NamesSymbol, tagsNames);
			UNPROTECT(2);
			return tags;
		}

		//// Set spectrum metadata
		//--------------------------

		// set spectrum ids from R vector
		bool set_spectrum_ids()
		{
			int n = num_spectra();
			char buffer[BUFLEN];
			pugi::xml_node spectrum = first_spectrum();
			int i = 0;
			while ( i < n )
			{
				// safe check for interrupt (allow xml destructor to run)
				if ( pendingInterrupt() ) {
					Rf_warning("stopping early; file may be incomplete");
					return false;
				}
				// set spectrum id
				snprintf(buffer, BUFLEN, "Spectrum=%d", i + 1);
				spectrum.attribute("id").set_value(buffer);
				snprintf(buffer, BUFLEN, "%d", i + 1);
				spectrum.attribute("index").set_value(buffer);
				// iterate
				i++;
				if ( spectrum.next_sibling() )
					spectrum = spectrum.next_sibling();
				else if ( i < n )
					spectrum = spectrum.parent().append_copy(spectrum);
			}
			return true;
		}

		// set spectrum positions from R data frame
		bool set_spectrum_positions(SEXP positions)
		{
			int n = num_spectra();
			SEXP px = VECTOR_ELT_BY_NAME(positions, IMS_POSITION_X_NAME);
			SEXP py = VECTOR_ELT_BY_NAME(positions, IMS_POSITION_Y_NAME);
			SEXP pz = VECTOR_ELT_BY_NAME(positions, IMS_POSITION_Z_NAME);
			bool has_z = Rf_isNull(pz);
			pugi::xml_node spectrum = first_spectrum();
			pugi::xml_node scan, x, y, z;
			pugi::xml_attribute cv, id, name, value;
			int i = 0;
			while ( i < n )
			{
				// safe check for interrupt (allow xml destructor to run)
				if ( pendingInterrupt() ) {
					Rf_warning("stopping early; file may be incomplete");
					return false;
				}
				scan = spectrum.child("scanList").child("scan");
				// set position x
				x = scan.find_child_by_attribute("cvParam",
					"accession", IMS_POSITION_X_ID);
				if ( x )
					scan.remove_child(x);
				x = scan.append_child("cvParam");
				x.append_attribute("cvRef").set_value("IMS");
				x.append_attribute("accession").set_value(IMS_POSITION_X_ID);
				x.append_attribute("name").set_value(IMS_POSITION_X_NAME);
				x.append_attribute("value").set_value(CHAR(STRING_ELT(px, i)));
				// set position y
				y = scan.find_child_by_attribute("cvParam",
					"accession", IMS_POSITION_Y_ID);
				if ( y )
					scan.remove_child(y);
				y = scan.append_child("cvParam");
				y.append_attribute("cvRef").set_value("IMS");
				y.append_attribute("accession").set_value(IMS_POSITION_Y_ID);
				y.append_attribute("name").set_value(IMS_POSITION_Y_NAME);
				y.append_attribute("value").set_value(CHAR(STRING_ELT(py, i)));
				// set position z
				if ( has_z )
				{
					z = scan.find_child_by_attribute("cvParam",
						"accession", IMS_POSITION_Z_ID);
					if ( z )
						scan.remove_child(z);
					z = scan.append_child("cvParam");
					z.append_attribute("cvRef").set_value("IMS");
					z.append_attribute("accession").set_value(IMS_POSITION_Z_ID);
					z.append_attribute("name").set_value(IMS_POSITION_Z_NAME);
					z.append_attribute("value").set_value(CHAR(STRING_ELT(pz, i)));
				}
				// iterate
				i++;
				if ( spectrum.next_sibling() )
					spectrum = spectrum.next_sibling();
				else if ( i < n )
					spectrum = spectrum.parent().append_copy(spectrum);
			}
			return true;
		}

		// set spectrum positions from R data frame
		bool set_spectrum_arrays(SEXP dataArrays, const char * id)
		{
			int n = num_spectra();
			SEXP poff = VECTOR_ELT_BY_NAME(dataArrays, IMS_EXTERNAL_OFFSET_NAME);
			SEXP plen = VECTOR_ELT_BY_NAME(dataArrays, IMS_EXTERNAL_ARRAY_LENGTH_NAME);
			SEXP pxlen = VECTOR_ELT_BY_NAME(dataArrays, IMS_EXTERNAL_ENCODED_LENGTH_NAME);
			pugi::xml_node spectrum = first_spectrum();
			pugi::xml_node dataArray, binary, off, len, xlen;
			int i = 0;
			while ( i < n )
			{
				// safe check for interrupt (allow xml destructor to run)
				if ( pendingInterrupt() ) {
					Rf_warning("stopping early; file may be incomplete");
					return false;
				}
				dataArray = find_binaryDataArray(spectrum, id);
				binary = dataArray.child("binary");
				// set offset
				off = dataArray.find_child_by_attribute("cvParam",
					"accession", IMS_EXTERNAL_OFFSET_ID);
				if ( off )
					dataArray.remove_child(off);
				off = dataArray.insert_child_before("cvParam", binary);
				off.append_attribute("cvRef").set_value("IMS");
				off.append_attribute("accession").set_value(IMS_EXTERNAL_OFFSET_ID);
				off.append_attribute("name").set_value(IMS_EXTERNAL_OFFSET_NAME);
				off.append_attribute("value").set_value(CHAR(STRING_ELT(poff, i)));
				// set array length
				len = dataArray.find_child_by_attribute("cvParam",
					"accession", IMS_EXTERNAL_ARRAY_LENGTH_ID);
				if ( len )
					dataArray.remove_child(len);
				len = dataArray.insert_child_before("cvParam", binary);
				len.append_attribute("cvRef").set_value("IMS");
				len.append_attribute("accession").set_value(IMS_EXTERNAL_ARRAY_LENGTH_ID);
				len.append_attribute("name").set_value(IMS_EXTERNAL_ARRAY_LENGTH_NAME);
				len.append_attribute("value").set_value(CHAR(STRING_ELT(plen, i)));
				// set encoded length
				xlen = dataArray.find_child_by_attribute("cvParam",
					"accession", IMS_EXTERNAL_ENCODED_LENGTH_ID);
				if ( xlen )
					dataArray.remove_child(xlen);
				xlen = dataArray.insert_child_before("cvParam", binary);
				xlen.append_attribute("cvRef").set_value("IMS");
				xlen.append_attribute("accession").set_value(IMS_EXTERNAL_ENCODED_LENGTH_ID);
				xlen.append_attribute("name").set_value(IMS_EXTERNAL_ENCODED_LENGTH_NAME);
				xlen.append_attribute("value").set_value(CHAR(STRING_ELT(pxlen, i)));
				// iterate
				i++;
				if ( spectrum.next_sibling() )
					spectrum = spectrum.next_sibling();
				else if ( i < n )
					spectrum = spectrum.parent().append_copy(spectrum);
			}
			return true;
		}

		bool set_spectrum_mzArrays(SEXP dataArrays)
		{
			return set_spectrum_arrays(dataArrays, MS_MZ_ARRAY_ID);
		}

		bool set_spectrum_intensityArrays(SEXP dataArrays)
		{
			return set_spectrum_arrays(dataArrays, MS_INTENSITY_ARRAY_ID);
		}

		bool set_spectrumList(SEXP positions, SEXP mzArrays, SEXP intensityArrays)
		{
			if ( !set_spectrum_ids() )
				return false;
			if ( !set_spectrum_positions(positions) )
				return false;
			if ( !set_spectrum_mzArrays(mzArrays) )
				return false;
			if ( !set_spectrum_intensityArrays(intensityArrays) )
				return false;
			return true;
		}

		//// Get run metadata
		//--------------------

		SEXP get_run(SEXP extra)
		{
			SEXP tags, tagsNames;
			PROTECT(tags = Rf_allocVector(VECSXP, 1));
			PROTECT(tagsNames = Rf_allocVector(STRSXP, 1));
			SET_VECTOR_ELT(tags, 0, get_spectrumList(extra));
			SET_STRING_ELT(tagsNames, 0, Rf_mkChar("spectrumList"));
			Rf_setAttrib(tags, R_NamesSymbol, tagsNames);
			UNPROTECT(2);
			return tags;
		}

		bool set_run(SEXP positions, SEXP mzArrays, SEXP intensityArrays)
		{
			return set_spectrumList(positions, mzArrays, intensityArrays);
		}

	protected:

		pugi::xml_document _doc;
		pugi::xml_node _mzml;
		pugi::xml_node _reflist;
		pugi::xml_node _spectra;

};

#endif // IMZML
