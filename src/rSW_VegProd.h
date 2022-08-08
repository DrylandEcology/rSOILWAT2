#include <Rinternals.h> // for SEXP

/* =================================================== */
/*             Global Function Declarations            */
/* --------------------------------------------------- */
SEXP onGet_SW_VPD();
void onSet_SW_VPD(SEXP SW_VPD);
SEXP rSW2_estimate_PotNatVeg_composition(SEXP MAP_mm, SEXP MAT_C, SEXP mean_monthly_ppt_mm,
                                         SEXP mean_monthly_Temp_C, SEXP shrub_limit, SEXP SumGrasses_Fraction,
                                         SEXP fill_empty_with_BareGround, SEXP warn_extrapolation, SEXP dailyC4vars,
                                         SEXP isNorth, SEXP Succulents_Fraction, SEXP Annuals_Fraction, SEXP C4_Fraction,
                                         SEXP C3_Fraction, SEXP Shrubs_Fraction, SEXP Forbs_Fraction,
                                         SEXP Trees_Fraction, SEXP BareGround_Fraction);

