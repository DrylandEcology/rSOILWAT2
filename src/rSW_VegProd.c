/********************************************************/
/********************************************************/
/*	Source file: Veg_Prod.c
Type: module
Application: SOILWAT - soilwater dynamics simulator
Purpose: Read / write and otherwise manage the model's
vegetation production parameter information.
*/
/********************************************************/
/********************************************************/

/* =================================================== */
/* =================================================== */
/*                INCLUDES / DEFINES                   */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "SOILWAT2/include/generic.h"
#include "SOILWAT2/include/filefuncs.h"
#include "SOILWAT2/include/Times.h"
#include "SOILWAT2/include/myMemory.h"

#include "SOILWAT2/include/SW_Defines.h"
#include "SOILWAT2/include/SW_Files.h"
#include "SOILWAT2/include/SW_Main_lib.h"

#include "SOILWAT2/include/SW_VegProd.h"
#include "rSW_VegProd.h"
#include "SW_R_lib.h"

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>



/* =================================================== */
/*                  Local Variables                    */
/* --------------------------------------------------- */
static char *cVegProd_names[] = {
	"veg_method",
	"nYearsDynamicShort",
	"nYearsDynamicLong",
	"Composition", 
    "Albedo", 
    "CanopyHeight",
	"VegetationInterceptionParameters", 
    "LitterInterceptionParameters",
	"EsTpartitioning_param", 
    "Es_param_limit", 
    "Shade", 
    "HydraulicRedistribution_use",
	"HydraulicRedistribution", 
    "CriticalSoilWaterPotential", 
    "MonthlyVeg",
	"CO2Coefficients", 
    "vegYear", 
    "isBiomAsIf100Cover"
};

char *cMonths[] = {
	"January", "February", "March", "April", "May", "June", "July",
	"August", "September", "October", "November", "December"
};


/* =================================================== */
/*             Global Function Definitions             */
/* --------------------------------------------------- */

SEXP onGet_SW_VPD(void) {
    int i;
    int k;
    SW_VEGPROD_INPUTS *vi = &SoilWatRun.VegProdIn;
    SW_VEGPROD_RUN_INPUTS *v = &SoilWatRun.RunIn.VegProdRunIn;
    SEXP swProd2;
    SEXP VegProd;

    SEXP VegComp, LandCover_names, vegtype_names;
    SEXP Albedo, veg_method;
    SEXP nYearsDynamicShort;
	SEXP nYearsDynamicLong;

    SEXP Canopy, Canopy_names, Canopy_names_x;
    char *cCanopy_names_x[] = { "xinflec", "yinflec", "range", "slope", "height_cm" };
    double *p_Canopy;

    SEXP VegInterception, VegInterception_names, VegInterception_names_x;
    char *cVegInterception_x[] = { "kSmax", "kdead" };
    double *p_VegInterception;

    SEXP LitterInterception, LitterInterception_names, LitterInterception_names_x;
    char *cLitterInterception_x[] = { "kSmax" };
    double *p_LitterInterception;

    SEXP EsTpartitioning_param;
    SEXP Es_param_limit;

    SEXP Shade, Shade_names, Shade_names_x;
    char *cShade_names_x[] = { "ShadeScale", "ShadeMaximalDeadBiomass", "tanfuncXinflec", "yinflec", "range", "slope" };
    double *p_Shade;

    SEXP Hydraulic_flag;//"Flag"
    SEXP Hydraulic, Hydraulic_names, Hydraulic_names_x;
    double *p_Hydraulic;
    char *cHydraulic_names[] = { "MaxCondRoot", "SoilWaterPotential50", "ShapeCond" };

    SEXP CSWP;

    SEXP VegYear;
    SEXP IsBiomAsIf100Cover;

    SEXP CO2Coefficients, CO2_names, CO2_col_names;
    double *p_CO2Coefficients;
    char *cCO2_col_names[] = { "Biomass Coeff1", "Biomass Coeff2", "WUE Coeff1", "WUE Coeff2" };

    SEXP MonthlyVeg, MonthlyVeg_names;
    SEXP monBiomass;
    double *p_monBiomass;

    PROTECT(vegtype_names = allocVector(STRSXP, NVEGTYPES));
    for (k = 0; k < NVEGTYPES; k++) {
        SET_STRING_ELT(vegtype_names, k, mkChar(key2veg[k]));
    }

    SEXP MonthlyVeg_Column_names, MonthlyVeg_Row_names;
    char *cMonthlyVeg_Column_names[] = { "Litter", "Biomass", "Live_pct", "LAI_conv" };


    /* Get values for slot: veg_method */
    PROTECT(veg_method = NEW_INTEGER(1));
    INTEGER(veg_method)[0] = vi->veg_method;

    /* Get values for slot: nYearsDynamicShort */
    PROTECT(nYearsDynamicShort = NEW_INTEGER(1));
    INTEGER(nYearsDynamicShort)[0] = vi->nYearsDynamicShort;

    /* Get values for slot: nYearsDynamicLong */
    PROTECT(nYearsDynamicLong = NEW_INTEGER(1));
    INTEGER(nYearsDynamicLong)[0] = vi->nYearsDynamicLong;


    /* Get values for slot: Composition */
    PROTECT(VegComp = allocVector(REALSXP, NVEGTYPES + 1));
    for (k = 0; k < NVEGTYPES; k++) {
        REAL(VegComp)[k] = v->veg[k].cov.fCover;
    }
    REAL(VegComp)[NVEGTYPES] = v->bare_cov.fCover;

    PROTECT(LandCover_names = allocVector(STRSXP, NVEGTYPES + 1));
    for (k = 0; k < NVEGTYPES; k++) {
        SET_STRING_ELT(LandCover_names, k, mkChar(key2veg[k]));
    }
    SET_STRING_ELT(LandCover_names, NVEGTYPES, mkChar("Bare Ground"));
    setAttrib(VegComp, R_NamesSymbol, LandCover_names);


    /* Get values for slot: Albedo */
    PROTECT(Albedo = allocVector(REALSXP, NVEGTYPES + 1));
    for (k = 0; k < NVEGTYPES; k++) {
        REAL(Albedo)[k] = vi->veg[k].cov.albedo;
    }
    REAL(Albedo)[NVEGTYPES] = vi->bare_cov.albedo;
    setAttrib(Albedo, R_NamesSymbol, LandCover_names);


    /* Get values for slot: CanopyHeight */
    PROTECT(Canopy = allocMatrix(REALSXP, NVEGTYPES, 5));
    p_Canopy = REAL(Canopy);
    for (k = 0; k < NVEGTYPES; k++) {
        p_Canopy[k] = vi->veg[k].cnpy.xinflec;
        p_Canopy[k + NVEGTYPES] = vi->veg[k].cnpy.yinflec;
        p_Canopy[k + 2 * NVEGTYPES] = vi->veg[k].cnpy.range;
        p_Canopy[k + 3 * NVEGTYPES] = vi->veg[k].cnpy.slope;
        p_Canopy[k + 4 * NVEGTYPES] = vi->veg[k].canopy_height_constant;
    }
    PROTECT(Canopy_names = allocVector(VECSXP, 2));
    PROTECT(Canopy_names_x = allocVector(STRSXP, 5));
    for (i = 0; i < 5; i++) {
        SET_STRING_ELT(Canopy_names_x, i, mkChar(cCanopy_names_x[i]));
    }
    SET_VECTOR_ELT(Canopy_names, 0, vegtype_names);
    SET_VECTOR_ELT(Canopy_names, 1, Canopy_names_x);
    setAttrib(Canopy, R_DimNamesSymbol, Canopy_names);


    /* Get values for slot: VegetationInterceptionParameters */
    PROTECT(VegInterception = allocMatrix(REALSXP, NVEGTYPES, 2));
    p_VegInterception = REAL(VegInterception);
    for (k = 0; k < NVEGTYPES; k++) {
        p_VegInterception[k] = vi->veg[k].veg_kSmax;
        p_VegInterception[k + NVEGTYPES] = vi->veg[k].veg_kdead;
    }
    PROTECT(VegInterception_names = allocVector(VECSXP, 2));
    PROTECT(VegInterception_names_x = allocVector(STRSXP, 2));
    for (i = 0; i < 2; i++) {
        SET_STRING_ELT(VegInterception_names_x, i, mkChar(cVegInterception_x[i]));
    }
    SET_VECTOR_ELT(VegInterception_names, 0, vegtype_names);
    SET_VECTOR_ELT(VegInterception_names, 1, VegInterception_names_x);
    setAttrib(VegInterception, R_DimNamesSymbol, VegInterception_names);


    /* Get values for slot: LitterInterceptionParameters */
    PROTECT(LitterInterception = allocMatrix(REALSXP, NVEGTYPES, 1));
    p_LitterInterception = REAL(LitterInterception);
    for (k = 0; k < NVEGTYPES; k++) {
        p_LitterInterception[k] = vi->veg[k].lit_kSmax;
    }
    PROTECT(LitterInterception_names = allocVector(VECSXP, 2));
    PROTECT(LitterInterception_names_x = allocVector(STRSXP, 1));
    for (i = 0; i < 1; i++) {
        SET_STRING_ELT(LitterInterception_names_x, i, mkChar(cLitterInterception_x[i]));
    }
    SET_VECTOR_ELT(LitterInterception_names, 0, vegtype_names);
    SET_VECTOR_ELT(LitterInterception_names, 1, LitterInterception_names_x);
    setAttrib(LitterInterception, R_DimNamesSymbol, LitterInterception_names);


    /* Get values for slot: EsTpartitioning_param */
    PROTECT(EsTpartitioning_param = allocVector(REALSXP, NVEGTYPES));
    for (k = 0; k < NVEGTYPES; k++) {
        REAL(EsTpartitioning_param)[k] = vi->veg[k].EsTpartitioning_param;
    }
    setAttrib(EsTpartitioning_param, R_NamesSymbol, vegtype_names);


    /* Get values for slot: Es_param_limit */
    PROTECT(Es_param_limit = allocVector(REALSXP, NVEGTYPES));
    for (k = 0; k < NVEGTYPES; k++) {
        REAL(Es_param_limit)[k] = vi->veg[k].Es_param_limit;
    }
    setAttrib(Es_param_limit, R_NamesSymbol, vegtype_names);


    /* Get values for slot: Shade */
    PROTECT(Shade = allocMatrix(REALSXP, NVEGTYPES, 6));
    p_Shade = REAL(Shade);
    for (k = 0; k < NVEGTYPES; k++) {
        p_Shade[k] = vi->veg[k].shade_scale;
        p_Shade[k + NVEGTYPES] = vi->veg[k].shade_deadmax;
        p_Shade[k + 2 * NVEGTYPES] = vi->veg[k].tr_shade_effects.xinflec;
        p_Shade[k + 3 * NVEGTYPES] = vi->veg[k].tr_shade_effects.yinflec;
        p_Shade[k + 4 * NVEGTYPES] = vi->veg[k].tr_shade_effects.range;
        p_Shade[k + 5 * NVEGTYPES] = vi->veg[k].tr_shade_effects.slope;
    }
    PROTECT(Shade_names = allocVector(VECSXP, 2));
    PROTECT(Shade_names_x = allocVector(STRSXP, 6));
    for (i = 0; i < 6; i++) {
        SET_STRING_ELT(Shade_names_x, i, mkChar(cShade_names_x[i]));
    }
    SET_VECTOR_ELT(Shade_names, 0, vegtype_names);
    SET_VECTOR_ELT(Shade_names, 1, Shade_names_x);
    setAttrib(Shade, R_DimNamesSymbol, Shade_names);


    /* Get values for slot: HydraulicRedistribution_use */
    PROTECT(Hydraulic_flag = allocVector(LGLSXP, NVEGTYPES));
    for (k = 0; k < NVEGTYPES; k++) {
        LOGICAL_POINTER(Hydraulic_flag)[k] = vi->veg[k].flagHydraulicRedistribution;
    }
    setAttrib(Hydraulic_flag, R_NamesSymbol, vegtype_names);


    /* Get values for slot: HydraulicRedistribution */
    PROTECT(Hydraulic = allocMatrix(REALSXP, NVEGTYPES, 3));
    p_Hydraulic = REAL(Hydraulic);
    for (k = 0; k < NVEGTYPES; k++) {
        p_Hydraulic[k] = vi->veg[k].maxCondroot;
        p_Hydraulic[k + NVEGTYPES] = vi->veg[k].swpMatric50;
        p_Hydraulic[k + 2 * NVEGTYPES] = vi->veg[k].shapeCond;
    }
    PROTECT(Hydraulic_names = allocVector(VECSXP, 2));
    PROTECT(Hydraulic_names_x = allocVector(STRSXP, 3));
    for (i = 0; i < 3; i++) {
        SET_STRING_ELT(Hydraulic_names_x, i, mkChar(cHydraulic_names[i]));
    }
    SET_VECTOR_ELT(Hydraulic_names, 0, vegtype_names);
    SET_VECTOR_ELT(Hydraulic_names, 1, Hydraulic_names_x);
    setAttrib(Hydraulic, R_DimNamesSymbol, Hydraulic_names);


    /* Get values for slot: CriticalSoilWaterPotential */
    PROTECT(CSWP = allocVector(REALSXP, NVEGTYPES));
    for (k = 0; k < NVEGTYPES; k++) {
        REAL(CSWP)[k] = vi->veg[k].SWPcrit / -10;
    }
    setAttrib(CSWP, R_NamesSymbol, vegtype_names);


    /* Get values for slot: MonthlyVeg */
    PROTECT(MonthlyVeg_Column_names = allocVector(STRSXP, 4));
    for (i = 0; i < 4; i++) {
        SET_STRING_ELT(MonthlyVeg_Column_names, i, mkChar(cMonthlyVeg_Column_names[i]));
    }
    PROTECT(MonthlyVeg_Row_names = allocVector(STRSXP, 12));
    for (i = 0; i < 12; i++) {
        SET_STRING_ELT(MonthlyVeg_Row_names, i, mkChar(cMonths[i]));
    }
    PROTECT(MonthlyVeg_names = allocVector(VECSXP, 2));
    SET_VECTOR_ELT(MonthlyVeg_names, 0, MonthlyVeg_Row_names);
    SET_VECTOR_ELT(MonthlyVeg_names, 1, MonthlyVeg_Column_names);

    PROTECT(MonthlyVeg = allocVector(VECSXP, NVEGTYPES));
    for (k = 0; k < NVEGTYPES; k++) {
        PROTECT(monBiomass = allocMatrix(REALSXP, 12, 4));
        p_monBiomass = REAL(monBiomass);
        for (i = 0; i < 12; i++) {
            p_monBiomass[i + 12 * 0] = v->veg[k].litter[i];
            p_monBiomass[i + 12 * 1] = v->veg[k].biomass[i];
            p_monBiomass[i + 12 * 2] = v->veg[k].pct_live[i];
            p_monBiomass[i + 12 * 3] = v->veg[k].lai_conv[i];
        }
        setAttrib(monBiomass, R_DimNamesSymbol, MonthlyVeg_names);
        SET_VECTOR_ELT(MonthlyVeg, k, monBiomass);
        UNPROTECT(1);
    }
    setAttrib(MonthlyVeg, R_NamesSymbol, vegtype_names);


    /* Get values for slot: CO2Coefficients */
    PROTECT(CO2_col_names = allocVector(STRSXP, 4));
    for (i = 0; i < 4; i++) {
        SET_STRING_ELT(CO2_col_names, i, mkChar(cCO2_col_names[i]));
    }
    PROTECT(CO2Coefficients = allocMatrix(REALSXP, NVEGTYPES, 4));
    p_CO2Coefficients = REAL(CO2Coefficients);
    for (k = 0; k < NVEGTYPES; k++) {
        p_CO2Coefficients[k] = vi->veg[k].co2_bio_coeff1;
        p_CO2Coefficients[k + NVEGTYPES] = vi->veg[k].co2_bio_coeff2;
        p_CO2Coefficients[k + 2 * NVEGTYPES] = vi->veg[k].co2_wue_coeff1;
        p_CO2Coefficients[k + 3 * NVEGTYPES] = vi->veg[k].co2_wue_coeff2;
    }
    PROTECT(CO2_names = allocVector(VECSXP, 2));
    SET_VECTOR_ELT(CO2_names, 1, CO2_col_names);
    SET_VECTOR_ELT(CO2_names, 0, vegtype_names);
    setAttrib(CO2Coefficients, R_DimNamesSymbol, CO2_names);


    /* Get values for slot: vegYear */
    PROTECT(VegYear = NEW_INTEGER(1));
    INTEGER(VegYear)[0] = vi->vegYear;


    /* Get values for slot: isBiomAsIf100Cover */
    PROTECT(IsBiomAsIf100Cover = NEW_LOGICAL(1));
    LOGICAL_POINTER(IsBiomAsIf100Cover)[0] = vi->isBiomAsIf100Cover;


    /* Create class swProd2 */
    PROTECT(swProd2 = MAKE_CLASS("swProd2"));
    PROTECT(VegProd = NEW_OBJECT(swProd2));

    /* Copy elements to slots of class swProd2 */
    SET_SLOT(VegProd, install(cVegProd_names[0]), veg_method);
    SET_SLOT(VegProd, install(cVegProd_names[1]), nYearsDynamicShort);
    SET_SLOT(VegProd, install(cVegProd_names[2]), nYearsDynamicLong);
    SET_SLOT(VegProd, install(cVegProd_names[3]), VegComp);
    SET_SLOT(VegProd, install(cVegProd_names[4]), Albedo);
    SET_SLOT(VegProd, install(cVegProd_names[5]), Canopy);
    SET_SLOT(VegProd, install(cVegProd_names[6]), VegInterception);
    SET_SLOT(VegProd, install(cVegProd_names[7]), LitterInterception);
    SET_SLOT(VegProd, install(cVegProd_names[8]), EsTpartitioning_param);
    SET_SLOT(VegProd, install(cVegProd_names[9]), Es_param_limit);
    SET_SLOT(VegProd, install(cVegProd_names[10]), Shade);
    SET_SLOT(VegProd, install(cVegProd_names[11]), Hydraulic_flag);
    SET_SLOT(VegProd, install(cVegProd_names[12]), Hydraulic);
    SET_SLOT(VegProd, install(cVegProd_names[13]), CSWP);
    SET_SLOT(VegProd, install(cVegProd_names[14]), MonthlyVeg);
    SET_SLOT(VegProd, install(cVegProd_names[15]), CO2Coefficients);
    SET_SLOT(VegProd, install(cVegProd_names[16]), VegYear);
    SET_SLOT(VegProd, install(cVegProd_names[17]), IsBiomAsIf100Cover);

    /* Memory clean up */
    UNPROTECT(37);

    return VegProd;
}

void onSet_SW_VPD(SEXP SW_VPD, LOG_INFO* LogInfo) {
    int i;
    int k;
    SW_VEGPROD_INPUTS *vi = &SoilWatRun.VegProdIn;
    SW_VEGPROD_RUN_INPUTS *v = &SoilWatRun.RunIn.VegProdRunIn;

    SEXP veg_method;
    SEXP VegComp;
    SEXP nYearsDynamicShort;
    SEXP nYearsDynamicLong;
    SEXP Albedo;
    SEXP Canopy;
    double *p_Canopy;
    SEXP VegInterception;
    double *p_VegInterception;
    SEXP LitterInterception;
    double *p_LitterInterception;
    SEXP EsTpartitioning_param;
    SEXP Es_param_limit;
    SEXP Shade;
    double *p_Shade;
    SEXP Hydraulic;
    SEXP Hydraulic_flag;
    SEXP CSWP;
    SEXP MonthlyVeg;
    SEXP monBiomass;
    double *p_monBiomass;
    SEXP CO2Coefficients;
    SEXP VegYear;
    SEXP IsBiomAsIf100Cover;


    /* Set values using slot: veg_method */
    PROTECT(veg_method = GET_SLOT(SW_VPD, install(cVegProd_names[0])));
    vi->veg_method = INTEGER(veg_method)[0];


    /* Set values using slots: nYearsDynamicShort, nYearsDynamicLong */
    PROTECT(nYearsDynamicShort = GET_SLOT(SW_VPD, install(cVegProd_names[1])));
    vi->nYearsDynamicShort = INTEGER(nYearsDynamicShort)[0];

    PROTECT(nYearsDynamicLong = GET_SLOT(SW_VPD, install(cVegProd_names[2])));
    vi->nYearsDynamicLong = INTEGER(nYearsDynamicLong)[0];


    /* Set values using slot: Composition */
    PROTECT(VegComp = GET_SLOT(SW_VPD, install(cVegProd_names[3])));
    for (k = 0; k < NVEGTYPES; k++) {
        v->veg[k].cov.fCover = REAL(VegComp)[k];
    }
    v->bare_cov.fCover = REAL(VegComp)[NVEGTYPES]; //Bare Ground


    /* Set values using slot: Albedo */
    PROTECT(Albedo = GET_SLOT(SW_VPD, install(cVegProd_names[4])));
    for (k = 0; k < NVEGTYPES; k++) {
        vi->veg[k].cov.albedo = REAL(Albedo)[k];
    }
    vi->bare_cov.albedo = REAL(Albedo)[NVEGTYPES]; //Bare Ground


    /* Set values using slot: CanopyHeight */
    PROTECT(Canopy = GET_SLOT(SW_VPD, install(cVegProd_names[5])));
    p_Canopy = REAL(Canopy);
    for (k = 0; k < NVEGTYPES; k++) {
        vi->veg[k].cnpy.xinflec = p_Canopy[k];
        vi->veg[k].cnpy.yinflec = p_Canopy[k + NVEGTYPES];
        vi->veg[k].cnpy.range = p_Canopy[k + 2 * NVEGTYPES];
        vi->veg[k].cnpy.slope = p_Canopy[k + 3 * NVEGTYPES];
        vi->veg[k].canopy_height_constant = p_Canopy[k + 4 * NVEGTYPES];
    }


    /* Set values using slot: VegetationInterceptionParameters */
    PROTECT(VegInterception = GET_SLOT(SW_VPD, install(cVegProd_names[6])));
    p_VegInterception = REAL(VegInterception);
    for (k = 0; k < NVEGTYPES; k++) {
        vi->veg[k].veg_kSmax = p_VegInterception[k];
        vi->veg[k].veg_kdead = p_VegInterception[k + NVEGTYPES];
    }


    /* Set values using slot: LitterInterceptionParameters */
    PROTECT(LitterInterception = GET_SLOT(SW_VPD, install(cVegProd_names[7])));
    p_LitterInterception = REAL(LitterInterception);
    for (k = 0; k < NVEGTYPES; k++) {
        vi->veg[k].lit_kSmax = p_LitterInterception[k];
    }


    /* Set values using slot: EsTpartitioning_param */
    PROTECT(EsTpartitioning_param = GET_SLOT(SW_VPD, install(cVegProd_names[8])));
    for (k = 0; k < NVEGTYPES; k++) {
        vi->veg[k].EsTpartitioning_param = REAL(EsTpartitioning_param)[k];
    }


    /* Set values using slot: Es_param_limit */
    PROTECT(Es_param_limit = GET_SLOT(SW_VPD, install(cVegProd_names[9])));
    for (k = 0; k < NVEGTYPES; k++) {
        vi->veg[k].Es_param_limit = REAL(Es_param_limit)[k];
    }


    /* Set values using slot: Shade */
    PROTECT(Shade = GET_SLOT(SW_VPD, install(cVegProd_names[10])));
    p_Shade = REAL(Shade);
    for (k = 0; k < NVEGTYPES; k++) {
        vi->veg[k].shade_scale = p_Shade[k];
        vi->veg[k].shade_deadmax = p_Shade[k + NVEGTYPES];
        vi->veg[k].tr_shade_effects.xinflec = p_Shade[k + 2 * NVEGTYPES];
        vi->veg[k].tr_shade_effects.yinflec = p_Shade[k + 3 * NVEGTYPES];
        vi->veg[k].tr_shade_effects.range = p_Shade[k + 4 * NVEGTYPES];
        vi->veg[k].tr_shade_effects.slope = p_Shade[k + 5 * NVEGTYPES];
    }


    /* Set values using slot: HydraulicRedistribution_use */
    PROTECT(Hydraulic_flag = GET_SLOT(SW_VPD, install(cVegProd_names[11])));
    for (k = 0; k < NVEGTYPES; k++) {
        vi->veg[k].flagHydraulicRedistribution = LOGICAL_POINTER(Hydraulic_flag)[k];
    }


    /* Set values using slot: HydraulicRedistribution */
    PROTECT(Hydraulic = GET_SLOT(SW_VPD, install(cVegProd_names[12])));
    for (k = 0; k < NVEGTYPES; k++) {
        vi->veg[k].maxCondroot = REAL(Hydraulic)[k];
        vi->veg[k].swpMatric50 = REAL(Hydraulic)[k + NVEGTYPES];
        vi->veg[k].shapeCond = REAL(Hydraulic)[k + 2 * NVEGTYPES];
    }


    /* Set values using slot: CriticalSoilWaterPotential */
    PROTECT(CSWP = GET_SLOT(SW_VPD, install(cVegProd_names[13])));
    for (k = 0; k < NVEGTYPES; k++) {
        vi->veg[k].SWPcrit = -10 * REAL(CSWP)[k];
        // for use with get_swa for properly partitioning swa
        vi->critSoilWater[k] = REAL(CSWP)[k];
    }
    get_critical_rank(vi);


    /* Set values using slot: MonthlyVeg */
    PROTECT(MonthlyVeg = GET_SLOT(SW_VPD, install(cVegProd_names[14])));
    for (k = 0; k < NVEGTYPES; k++) {
        PROTECT(monBiomass =  VECTOR_ELT(MonthlyVeg, k));
        p_monBiomass = REAL(monBiomass);
        for (i = 0; i < 12; i++) {
            v->veg[k].litter[i] = p_monBiomass[i + 12 * 0];
            v->veg[k].biomass[i] = p_monBiomass[i + 12 * 1];
            v->veg[k].pct_live[i] = p_monBiomass[i + 12 * 2];
            v->veg[k].lai_conv[i] = p_monBiomass[i + 12 * 3];
        }
        UNPROTECT(1);
    }


    /* Set values using slot: CO2Coefficients */
    PROTECT(CO2Coefficients = GET_SLOT(SW_VPD, install(cVegProd_names[15])));
    for (k = 0; k < NVEGTYPES; k++) {
        vi->veg[k].co2_bio_coeff1 = REAL(CO2Coefficients)[k];
        vi->veg[k].co2_bio_coeff2 = REAL(CO2Coefficients)[k + NVEGTYPES];
        vi->veg[k].co2_wue_coeff1 = REAL(CO2Coefficients)[k + 2 * NVEGTYPES];
        vi->veg[k].co2_wue_coeff2 = REAL(CO2Coefficients)[k + 3 * NVEGTYPES];
    }


    /* Set values using slot: vegYear */
    PROTECT(VegYear = GET_SLOT(SW_VPD, install(cVegProd_names[16])));
    vi->vegYear = INTEGER(VegYear)[0];


    /* Set values using slot: isBiomAsIf100Cover */
    PROTECT(IsBiomAsIf100Cover = GET_SLOT(SW_VPD, install(cVegProd_names[17])));
    vi->isBiomAsIf100Cover = LOGICAL(IsBiomAsIf100Cover)[0];


    /* Wrap up */
    SW_VPD_fix_cover(&SoilWatRun.RunIn.VegProdRunIn, LogInfo);
    if (LogInfo->stopRun) {
        goto freeMem; // Exit function prematurely due to error
    }

    if (EchoInits) {
        echo_VegProd(&SoilWatRun.RunIn.VegProdRunIn, &SoilWatRun.VegProdIn);
    }

freeMem:
    UNPROTECT(18);
}

// `estimate_PotNatVeg_composition()` is R interface to rSW2_estimate_PotNatVeg_composition()
SEXP rSW2_estimate_PotNatVeg_composition(SEXP MAP_mm, SEXP MAT_C, SEXP mean_monthly_ppt_mm,
                                         SEXP mean_monthly_Temp_C, SEXP shrub_limit, SEXP SumGrasses_Fraction,
                                         SEXP fill_empty_with_BareGround, SEXP warn_extrapolation, SEXP dailyC4vars,
                                         SEXP isNorth, SEXP fixBareGround, SEXP Succulents_Fraction, SEXP Annuals_Fraction,
                                         SEXP C4_Fraction, SEXP C3_Fraction, SEXP Shrubs_Fraction, SEXP Forbs_Fraction,
                                         SEXP Trees_Fraction, SEXP BareGround_Fraction) {

    int nRes = 4;
    int nL0 = 8;
    int nL1 = 5;
    int nL2 = 7;
    int nGrasses = 3;

    double RelAbundanceL0[8], RelAbundanceL1[5], RelAbundanceL2[7], grasses[3];

    LOG_INFO local_LogInfo;
    sw_init_logs(current_sw_verbosity, &local_LogInfo);


    // "final_" in the beginning meaning it's the final R -> conversion
    double final_MAP_cm = asReal(MAP_mm) / 10, final_MAT_C = asReal(MAT_C), final_MonPPT_cm[MAX_MONTHS],
    final_MonTemp_C[MAX_MONTHS], final_shrubLimit = asReal(shrub_limit),
    final_SumGrassesFraction = asReal(SumGrasses_Fraction), C4Variables[3] = {SW_MISSING, SW_MISSING, SW_MISSING};

    // Following variable names end with "_D" to denote they are of C type double
    double Succulents_Fraction_D = ISNAN(asReal(Succulents_Fraction)) ? SW_MISSING : asReal(Succulents_Fraction);
    double Annuals_Fraction_D = ISNAN(asReal(Annuals_Fraction)) ? 0.0 : asReal(Annuals_Fraction);
    double C4_Fraction_D = ISNAN(asReal(C4_Fraction)) ? SW_MISSING : asReal(C4_Fraction);
    double C3_Fraction_D = ISNAN(asReal(C3_Fraction)) ? SW_MISSING : asReal(C3_Fraction);
    double Shrubs_Fraction_D = ISNAN(asReal(Shrubs_Fraction)) ? SW_MISSING : asReal(Shrubs_Fraction);
    double Forbs_Fraction_D = ISNAN(asReal(Forbs_Fraction)) ? SW_MISSING : asReal(Forbs_Fraction);
    double Trees_Fraction_D = ISNAN(asReal(Trees_Fraction)) ? 0.0 : asReal(Trees_Fraction);
    double BareGround_Fraction_D = ISNAN(asReal(BareGround_Fraction)) ? 0.0 : asReal(BareGround_Fraction);

    double inputValues_D[8] = {Succulents_Fraction_D, Forbs_Fraction_D, C3_Fraction_D,
        C4_Fraction_D, Annuals_Fraction_D, Shrubs_Fraction_D, Trees_Fraction_D, BareGround_Fraction_D};

    char *finalListNames[] = {
        "Rel_Abundance_L0", "Rel_Abundance_L1", "Rel_Abundance_L2", "Grasses"
    };
    char *RelAbundanceL0Names[] = {
        "Succulents", "Forbs", "Grasses_C3", "Grasses_C4",
        "Grasses_Annuals", "Shrubs", "Trees", "BareGround"
    };
    char *RelAbundanceL1Names[] = {
        "SW_TREES", "SW_SHRUB", "SW_FORBS", "SW_GRASS", "SW_BAREGROUND"
    };
    char *RelAbundanceL2Names[] = {
        "SW_TREENL", "SW_TREEBL", "SW_SHRUB", "SW_FORBS", "SW_GRASS3", "SW_GRASS4", "SW_BAREGROUND"
    };
    char *grassesNames[] = {"Grasses_C3", "Grasses_C4", "Grasses_Annuals"};

    // "inter_" meaning the intermediate R -> C conversion
    int index, julyMin = 0, degAbove65 = 1, frostFreeDays = 2;

    Bool final_fill_empty_with_BareGround = (Bool) asLogical(fill_empty_with_BareGround) ? swTRUE : swFALSE,
    final_warn_extrapolation = (Bool) asLogical(warn_extrapolation) ? swTRUE : swFALSE,
    final_isNorth = (Bool) asLogical(isNorth) ? swTRUE : swFALSE,
    final_fix_bareGround = (Bool) asLogical(fixBareGround) ? swTRUE : swFALSE;

    SEXP cRelAbL2Names, cRelAbL1Names, cRelAbL0Names, cgrasses;
    SEXP cfinalNames;
    SEXP final_RelAbundanceL2, final_RelAbundanceL1, final_RelAbundanceL0;
    SEXP final_grasses;
    SEXP res;

    res = PROTECT(allocVector(VECSXP, nRes));
    cfinalNames = PROTECT(allocVector(STRSXP, nRes));

    cRelAbL0Names = PROTECT(allocVector(STRSXP, nL0));
    final_RelAbundanceL0 = PROTECT(allocVector(REALSXP, nL0));

    final_RelAbundanceL1 = PROTECT(allocVector(REALSXP, nL1));
    cRelAbL1Names = PROTECT(allocVector(STRSXP, nL1));

    final_RelAbundanceL2 = PROTECT(allocVector(REALSXP, nL2));
    cRelAbL2Names = PROTECT(allocVector(STRSXP, nL2));

    final_grasses = PROTECT(allocVector(REALSXP, nGrasses));
    cgrasses = PROTECT(allocVector(STRSXP, nGrasses));

    for(index = 0; index < nRes; index++) {
        SET_STRING_ELT(cfinalNames, index, mkChar(finalListNames[index]));
    }

    for(index = 0; index < nL0; index++) {
        SET_STRING_ELT(cRelAbL0Names, index, mkChar(RelAbundanceL0Names[index]));
    }

    for(index = 0; index < nL1; index++) {
        SET_STRING_ELT(cRelAbL1Names, index, mkChar(RelAbundanceL1Names[index]));
    }

    for(index = 0; index < nL2; index++) {
        SET_STRING_ELT(cRelAbL2Names, index, mkChar(RelAbundanceL2Names[index]));
    }

    for(index = 0; index < nGrasses; index++) {
        SET_STRING_ELT(cgrasses, index, mkChar(grassesNames[index]));
    }

    namesgets(res, cfinalNames);
    namesgets(final_RelAbundanceL0, cRelAbL0Names);
    namesgets(final_RelAbundanceL1, cRelAbL1Names);
    namesgets(final_RelAbundanceL2, cRelAbL2Names);
    namesgets(final_grasses, cgrasses);

    for(index = 0; index < MAX_MONTHS; index++) {
        final_MonPPT_cm[index] = REAL(mean_monthly_ppt_mm)[index] / 10;
        final_MonTemp_C[index] = REAL(mean_monthly_Temp_C)[index];
    }

    // Check if dailyC4vars is not NULL and assume that not NA/NAN
    if(!isNull(dailyC4vars)) {
        // Coerce `dailyC4vars` to numeric (double)
        dailyC4vars = PROTECT(coerceVector(dailyC4vars, REALSXP));
        C4Variables[julyMin] = REAL(dailyC4vars)[0];
        C4Variables[frostFreeDays] = REAL(dailyC4vars)[1];
        C4Variables[degAbove65] = REAL(dailyC4vars)[2];
        UNPROTECT(1);
    } else {
        C4Variables[0] = SW_MISSING;
        C4Variables[1] = SW_MISSING;
        C4Variables[2] = SW_MISSING;
    }

    estimatePotNatVegComposition(final_MAT_C, final_MAP_cm, final_MonTemp_C,
          final_MonPPT_cm, inputValues_D, final_shrubLimit, final_SumGrassesFraction, C4Variables,
          final_fill_empty_with_BareGround, final_isNorth, final_warn_extrapolation,
          final_fix_bareGround, grasses, RelAbundanceL0, RelAbundanceL1, RelAbundanceL2, &local_LogInfo);
    if(local_LogInfo.stopRun) {
        goto report;
    }

    for (index = 0; index < nL0; index++) {
        REAL(final_RelAbundanceL0)[index] = RelAbundanceL0[index];
    }

    for (index = 0; index < nL1; index++) {
        REAL(final_RelAbundanceL1)[index] = RelAbundanceL1[index];
    }

    for (index = 0; index < nL2; index++) {
        REAL(final_RelAbundanceL2)[index] = RelAbundanceL2[index];
    }

    for (index = 0; index < nGrasses; index++) {
        REAL(final_grasses)[index] = grasses[index];
    }

    SET_VECTOR_ELT(res, 0, final_RelAbundanceL0);
    SET_VECTOR_ELT(res, 1, final_RelAbundanceL1);
    SET_VECTOR_ELT(res, 2, final_RelAbundanceL2);
    SET_VECTOR_ELT(res, 3, final_grasses);

    report: {
        // Note: no SOILWAT2 memory was allocated, nothing to deallocate
        UNPROTECT(10);

        sw_write_warnings("(rVegProd) ", &local_LogInfo);
        sw_fail_on_error(&local_LogInfo);
    }

    return res;

}
