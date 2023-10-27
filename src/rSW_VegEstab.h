
#include "SOILWAT2/include/generic.h"
#include <Rinternals.h> // for SEXP

/* =================================================== */
/*             Global Function Declarations            */
/* --------------------------------------------------- */
SEXP onGet_SW_VES(void);
void onSet_SW_VES(SEXP VES, LOG_INFO* LogInfo);
void onGet_SW_VES_spps(SEXP SPP);
void onSet_SW_VES_spp(SEXP SPP, IntU i, LOG_INFO* LogInfo);
