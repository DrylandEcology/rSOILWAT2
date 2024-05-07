/* =================================================== */
/*             Global Function Declarations            */
/* --------------------------------------------------- */
void rSW_CTL_setup_model2(void);
void rSW_CTL_setup_domain(Bool from_files, SEXP InputData, unsigned long userSUID, SW_DOMAIN* SW_Domain, LOG_INFO* LogInfo);
void rSW_CTL_obtain_inputs(Bool from_files, SEXP InputData, SEXP weatherList, LOG_INFO* LogInfo);
