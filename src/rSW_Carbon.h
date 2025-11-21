#include "SOILWAT2/include/SW_Defines.h" // for TimeInt
#include <Rinternals.h> // for SEXP

/* =================================================== */
/*             Global Function Declarations            */
/* --------------------------------------------------- */
SEXP onGet_SW_CARBON(void);

void onSet_swCarbon(
    SEXP object,
    TimeInt startYr,
    TimeInt endYr,
    TimeInt vegYear,
    LOG_INFO* LogInfo
);
