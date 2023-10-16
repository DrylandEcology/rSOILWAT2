#include "SOILWAT2/include/Times.h"
#include <Rinternals.h> // for SEXP

/* =================================================== */
/*             Global Function Declarations            */
/* --------------------------------------------------- */
void rSW_SWC_construct(void);
SEXP onGet_SW_SWC(LOG_INFO* LogInfo);
void onSet_SW_SWC(SEXP SWC, LOG_INFO* LogInfo);
SEXP onGet_SW_SWC_hists(LOG_INFO* LogInfo);
SEXP onGet_SW_SWC_hist(TimeInt year, LOG_INFO* LogInfo);
void onSet_SW_SWC_hist(LOG_INFO* LogInfo);
