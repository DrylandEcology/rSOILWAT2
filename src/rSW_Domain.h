#include <Rinternals.h> // for SEXP

/* =================================================== */
/*             Global Function Declarations            */
/* --------------------------------------------------- */
SEXP onGet_SW_DOM(void);
void onSet_SW_DOM(SEXP swDOM, LOG_INFO* LogInfo);
SEXP onGet_SW_SPINUP(void);
SEXP onGet_SW_MDL(void);
void onSet_SW_SPINUP(SEXP SW_DOM, LOG_INFO* LogInfo);
void onSet_SW_MDL(SEXP SW_MDL, LOG_INFO* LogInfo);
