#include <Rinternals.h> // for SEXP

/* =================================================== */
/*             Global Function Declarations            */
/* --------------------------------------------------- */
SEXP onGet_SW_SIT(void);
void onSet_SW_SIT(SEXP SW_SIT, LOG_INFO* LogInfo);
void onSet_SW_SIT_transp(SEXP SW_SIT, LOG_INFO* LogInfo);
SEXP onGet_SW_SOILS(void);
void onSet_SW_SOILS(SEXP SW_SOILS, LOG_INFO* LogInfo);
