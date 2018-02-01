
#include "SOILWAT2/generic.h"
#include <Rinternals.h> // for SEXP

/* Function Declarations */
SEXP onGet_SW_VES(void);
void onSet_SW_VES(SEXP VES);
void onGet_SW_VES_spps(SEXP SPP);
void onSet_SW_VES_spp(SEXP SPP, IntU i);
