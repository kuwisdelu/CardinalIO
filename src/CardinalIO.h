
#ifndef CARDINALIO
#define CARDINALIO

#include "imzML.h"

extern "C" {

SEXP parseImzML(SEXP file, SEXP extra);

SEXP writeImzML(SEXP xml, SEXP positions,
	SEXP mzArrays, SEXP intensityArrays,
	SEXP file);

} // extern "C"

#endif // CARDINALIO
