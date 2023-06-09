#include "SOILWAT2/include/Times.h"
#include <Rinternals.h> // for SEXP

/* =================================================== */
/*             Global Function Declarations            */
/* --------------------------------------------------- */
void rSW_SWC_construct(void);
SEXP onGet_SW_SWC(void);
void onSet_SW_SWC(SEXP SWC);
SEXP onGet_SW_SWC_hists(void);
SEXP onGet_SW_SWC_hist(TimeInt year);
void onSet_SW_SWC_hist(void);
