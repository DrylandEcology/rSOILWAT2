
#include "SOILWAT2/Times.h"
#include <Rinternals.h> // for SEXP

/* Function Declarations */
void rSW_WTH_new_year2(TimeInt year);
SEXP onGet_SW_WTH(void);
void onSet_SW_WTH(SEXP SW_WTH);
SEXP onGet_WTH_DATA(void);
SEXP onGet_WTH_DATA_YEAR(TimeInt year);
Bool onSet_WTH_DATA(SEXP WTH_DATA_YEAR, TimeInt year);
