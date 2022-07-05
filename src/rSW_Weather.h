
#include "SOILWAT2/Times.h"
#include "SOILWAT2/SW_Weather.h"
#include <Rinternals.h> // for SEXP

/* =================================================== */
/*             Global Function Declarations            */
/* --------------------------------------------------- */
SEXP onGet_SW_WTH(void);
void onSet_SW_WTH_setup(SEXP SW_WTH);
SEXP onGet_WTH_DATA(void);
void onSet_SW_WTH_read();
Bool onSet_WTH_DATA_YEAR(TimeInt year, SW_WEATHER_HIST *hist);
