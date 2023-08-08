
#include "CardinalIO.h"

extern "C" {

SEXP parseImzML(SEXP file)
{
	imzML doc;
	doc.load_file(CHAR(Rf_asChar(file)));
	SEXP tags, tagsNames;
	int n = 5;
	if ( doc.sampleList() )
		n++;
	if ( doc.scanSettingsList() )
		n++;
	PROTECT(tags = Rf_allocVector(VECSXP, n));
	PROTECT(tagsNames = Rf_allocVector(STRSXP, n));
	int i = 0, j = 0;
	SET_VECTOR_ELT(tags, i++, doc.get_fileDescription());
	SET_STRING_ELT(tagsNames, j++, Rf_mkChar("fileDescription"));
	if ( doc.sampleList() ) {
		SET_VECTOR_ELT(tags, i++, doc.get_sampleList());
		SET_STRING_ELT(tagsNames, j++, Rf_mkChar("sampleList"));
	}
	if ( doc.scanSettingsList() ) {
		SET_VECTOR_ELT(tags, i++, doc.get_scanSettingsList());
		SET_STRING_ELT(tagsNames, j++, Rf_mkChar("scanSettingsList"));
	}
	SET_VECTOR_ELT(tags, i++, doc.get_softwareList());
	SET_STRING_ELT(tagsNames, j++, Rf_mkChar("softwareList"));
	SET_VECTOR_ELT(tags, i++, doc.get_instrumentConfigurationList());
	SET_STRING_ELT(tagsNames, j++, Rf_mkChar("instrumentConfigurationList"));
	SET_VECTOR_ELT(tags, i++, doc.get_dataProcessingList());
	SET_STRING_ELT(tagsNames, j++, Rf_mkChar("dataProcessingList"));
	SET_VECTOR_ELT(tags, i++, doc.get_run());
	SET_STRING_ELT(tagsNames, j++, Rf_mkChar("run"));
	Rf_setAttrib(tags, R_NamesSymbol, tagsNames);
	UNPROTECT(2);
	return tags;
}

SEXP writeImzML(SEXP xml, SEXP positions,
	SEXP mzArrays, SEXP intensityArrays, SEXP file)
{
	imzML doc;
	doc.load_string(CHAR(STRING_ELT(xml, 0)));
	if ( !doc.set_run(positions, mzArrays, intensityArrays) )
		return Rf_ScalarLogical(false);
	if ( !doc.save_file(CHAR(STRING_ELT(file, 0))) )
		return Rf_ScalarLogical(false);
	return Rf_ScalarLogical(true);
}

} // extern "C"
