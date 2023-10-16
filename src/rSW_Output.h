#include <Rinternals.h> // for SEXP

/* =================================================== */
/*             Global Function Declarations            */
/* --------------------------------------------------- */
SEXP onGet_SW_OUT(void);
void onSet_SW_OUT(SEXP OUT, LOG_INFO* LogInfo);
void setGlobalrSOILWAT2_OutputVariables(SEXP outputData);
SEXP onGetOutput(SEXP inputData, LOG_INFO* LogInfo);
SEXP onGetOutputDeprecated(SEXP inputData);
