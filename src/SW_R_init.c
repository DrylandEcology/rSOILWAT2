#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>


/* .C calls */
//extern void SWRC_PDF_estimate_parameters(
//  int swrc_type, int pdf_type,
//  double *swrcp,
//  double sand, double clay, double gravel
//);
//
//static R_NativePrimitiveArgType SWRC_PDF_estimate_parameters_t[6] = {
//  INTSXP, INTSXP,
//  REALSXP,
//  REALSXP, REALSXP, REALSXP
//};
//
//static const R_CMethodDef CEntries[] = {
//  {
//    "SWRC_PDF_estimate_parameters",
//    (DL_FUNC) &SWRC_PDF_estimate_parameters,
//    6,
//    SWRC_PDF_estimate_parameters_t
//  },
//  {NULL, NULL, 0, NULL}
//};
//

/* .Call calls */
extern SEXP start(SEXP, SEXP, SEXP, SEXP);
extern SEXP tempError();
extern SEXP onGetInputDataFromFiles(SEXP, SEXP);
extern SEXP onGetOutput(SEXP);
extern SEXP sw_consts();
extern SEXP sw_quiet(SEXP);
extern SEXP rSW2_SWRC_PDF_estimate_parameters(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP rSW2_SWRC_check_parameters(SEXP, SEXP);
extern SEXP rSW2_SWRC(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"start",                      (DL_FUNC) &start,                      4},
  {"tempError",                  (DL_FUNC) &tempError,                  0},
  {"onGetInputDataFromFiles",    (DL_FUNC) &onGetInputDataFromFiles,    2},
  {"onGetOutput",                (DL_FUNC) &onGetOutput,                1},
  {"sw_consts",                  (DL_FUNC) &sw_consts,                  0},
  {"sw_quiet",                   (DL_FUNC) &sw_quiet,                   1},
  {"rSW2_SWRC_PDF_estimate_parameters", (DL_FUNC) &rSW2_SWRC_PDF_estimate_parameters, 5},
  {"rSW2_SWRC_check_parameters", (DL_FUNC) &rSW2_SWRC_check_parameters, 2},
  {"rSW2_SWRC",                  (DL_FUNC) &rSW2_SWRC,                  6},
  {NULL, NULL, 0}
};


/* Register package calls with R */
void R_init_rSOILWAT2(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
