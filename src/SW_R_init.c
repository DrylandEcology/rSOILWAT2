#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>


/* .Call calls */
extern SEXP start(SEXP, SEXP, SEXP, SEXP);
extern SEXP tempError(void);
extern SEXP onGetInputDataFromFiles(SEXP, SEXP);
extern SEXP onGetOutput(SEXP);
extern SEXP rSW2_processAllWeather(SEXP, SEXP);
extern SEXP rSW2_readAllWeatherFromDisk(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP sw_consts(void);
extern SEXP sw_quiet(SEXP);
extern SEXP rSW2_SWRC_PTF_estimate_parameters(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP sw_check_SWRC_vs_PTF(SEXP, SEXP);
extern SEXP rSW2_SWRC_check_parameters(SEXP, SEXP);
extern SEXP rSW2_SWRC(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP rSW2_estimate_PotNatVeg_composition(SEXP, SEXP, SEXP, SEXP,SEXP, SEXP, SEXP,
                                                SEXP,SEXP, SEXP, SEXP, SEXP,SEXP, SEXP,
                                                SEXP, SEXP,SEXP, SEXP, SEXP);
extern SEXP rSW2_calc_SiteClimate(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"start",                      (DL_FUNC) &start,                      4},
  {"tempError",                  (DL_FUNC) &tempError,                  0},
  {"onGetInputDataFromFiles",    (DL_FUNC) &onGetInputDataFromFiles,    2},
  {"onGetOutput",                (DL_FUNC) &onGetOutput,                1},
  {"rSW2_processAllWeather",     (DL_FUNC) &rSW2_processAllWeather,     2},
  {"rSW2_readAllWeatherFromDisk",(DL_FUNC) &rSW2_readAllWeatherFromDisk,5},
  {"sw_consts",                  (DL_FUNC) &sw_consts,                  0},
  {"sw_quiet",                   (DL_FUNC) &sw_quiet,                   1},
  {"rSW2_SWRC_PTF_estimate_parameters", (DL_FUNC) &rSW2_SWRC_PTF_estimate_parameters, 5},
  {"sw_check_SWRC_vs_PTF",       (DL_FUNC) &sw_check_SWRC_vs_PTF,       2},
  {"rSW2_SWRC_check_parameters", (DL_FUNC) &rSW2_SWRC_check_parameters, 2},
  {"rSW2_SWRC",                  (DL_FUNC) &rSW2_SWRC,                  6},
  {"rSW2_estimate_PotNatVeg_composition", (DL_FUNC) &rSW2_estimate_PotNatVeg_composition, 19},
  {"rSW2_calc_SiteClimate",      (DL_FUNC) &rSW2_calc_SiteClimate,      6},
  {NULL, NULL, 0}
};


/* Register package calls with R */
void R_init_rSOILWAT2(DllInfo *dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
  R_forceSymbols(dll, TRUE);
}
