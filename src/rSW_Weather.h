
#include "SOILWAT2/include/Times.h"
#include "SOILWAT2/include/SW_Weather.h"
#include <Rinternals.h> // for SEXP

/* =================================================== */
/*             Global Function Declarations            */
/* --------------------------------------------------- */
SEXP onGet_SW_WTH_setup(void);
void onSet_SW_WTH_setup(SEXP SW_WTH, LOG_INFO* LogInfo);
SEXP onGet_WTH_DATA(void);
void onSet_WTH_DATA(SEXP weatherList, LOG_INFO* LogInfo);
SEXP rSW2_calc_SiteClimate(SEXP weatherList, SEXP yearStart, SEXP yearEnd,
                           SEXP do_C4vars, SEXP do_Cheatgrass_ClimVars, SEXP latitude,
                           SEXP elevation);
