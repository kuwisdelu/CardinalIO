#include <R_ext/Rdynload.h>

#include "CardinalIO.h"

#define CALLDEF(name, n)  {#name, (DL_FUNC) &name, n}

extern "C" {

static const R_CallMethodDef callMethods[] = {
	CALLDEF(parseImzML, 1),
	CALLDEF(writeImzML, 5),
	{NULL, NULL, 0}
};

void R_init_CardinalIO(DllInfo * info)
{
	R_registerRoutines(info, NULL, callMethods, NULL, NULL);
}

} // extern "C"
