#include "Rheaders.h"
#include <R_ext/Rdynload.h>

extern SEXP cxhull_(SEXP, SEXP, SEXP);
extern SEXP cxhullEdges_(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallMethods[] = {
    {"cxhull_",      (DL_FUNC) &cxhull_,      3},
    {"cxhullEdges_", (DL_FUNC) &cxhullEdges_, 3},
    {NULL, NULL, 0}
};

void R_init_cxhull(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallMethods, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}