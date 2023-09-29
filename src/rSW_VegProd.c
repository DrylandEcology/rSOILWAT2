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
static char *MyFileName;


static char *cVegProd_names[] = {
	"veg_method", "Composition", "Albedo", "CanopyHeight",
	"VegetationInterceptionParameters", "LitterInterceptionParameters",
	"EsTpartitioning_param", "Es_param_limit", "Shade", "HydraulicRedistribution_use",
	"HydraulicRedistribution", "CriticalSoilWaterPotential", "MonthlyVeg",
	"CO2Coefficients"
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
	SW_VEGPROD *v = &SoilWatAll.VegProd;
	SEXP swProd;
	SEXP VegProd;

	SEXP VegComp, VegComp_names, vegtype_names, col_names;
	SEXP Albedo, veg_method;

	SEXP Canopy, Canopy_names, Canopy_names_x;
	char *cCanopy_names_x[] = { "xinflec", "yinflec", "range", "slope", "height_cm" };
	RealD *p_Canopy;

	SEXP VegInterception, VegInterception_names, VegInterception_names_x;
	char *cVegInterception_x[] = { "kSmax", "kdead" };
	RealD *p_VegInterception;

	SEXP LitterInterception, LitterInterception_names, LitterInterception_names_x;
	char *cLitterInterception_x[] = { "kSmax" };
	RealD *p_LitterInterception;

	SEXP EsTpartitioning_param;
	SEXP Es_param_limit;

	SEXP Shade, Shade_names, Shade_names_x;
	char *cShade_names_x[] = { "ShadeScale", "ShadeMaximalDeadBiomass", "tanfuncXinflec", "yinflec", "range", "slope" };
	RealD *p_Shade;

	SEXP Hydraulic_flag;//"Flag"
	SEXP Hydraulic, Hydraulic_names, Hydraulic_names_x;
	RealD *p_Hydraulic;
	char *cHydraulic_names[] = { "MaxCondRoot", "SoilWaterPotential50", "ShapeCond" };

	SEXP CSWP;

	SEXP MonthlyVeg;
	SEXP Grasslands, Grasslands_names;
	SEXP Shrublands, Shrublands_names;
	SEXP Forest, Forest_names;
	SEXP Forb, Forb_names;

	char *cvegtype_names[] = { "Grasses", "Shrubs", "Trees", "Forbs" };
	PROTECT(vegtype_names = allocVector(STRSXP, NVEGTYPES));
	for (i = 0; i < NVEGTYPES; i++)
		SET_STRING_ELT(vegtype_names, i, mkChar(cvegtype_names[i]));

	/* CO2 */
	// Initialize variables
	SEXP CO2Coefficients, CO2_names, CO2_col_names;
	RealD *p_CO2Coefficients;

	// Create row and column names
	char *cCO2_col_names[] = { "Biomass Coeff1", "Biomass Coeff2", "WUE Coeff1", "WUE Coeff2" };
	PROTECT(CO2_col_names = allocVector(STRSXP, 4));
	for (i = 0; i < 4; i++)
		SET_STRING_ELT(CO2_col_names, i, mkChar(cCO2_col_names[i]));

	// Create matrix containing the multipliers
	PROTECT(CO2Coefficients = allocMatrix(REALSXP, NVEGTYPES, 4));
	p_CO2Coefficients = REAL(CO2Coefficients);
	p_CO2Coefficients[0] = v->veg[SW_GRASS].co2_bio_coeff1;
	p_CO2Coefficients[1] = v->veg[SW_SHRUB].co2_bio_coeff1;
	p_CO2Coefficients[2] = v->veg[SW_TREES].co2_bio_coeff1;
	p_CO2Coefficients[3] = v->veg[SW_FORBS].co2_bio_coeff1;
	p_CO2Coefficients[4] = v->veg[SW_GRASS].co2_bio_coeff2;
	p_CO2Coefficients[5] = v->veg[SW_SHRUB].co2_bio_coeff2;
	p_CO2Coefficients[6] = v->veg[SW_TREES].co2_bio_coeff2;
	p_CO2Coefficients[7] = v->veg[SW_FORBS].co2_bio_coeff2;
	p_CO2Coefficients[8] = v->veg[SW_GRASS].co2_wue_coeff1;
	p_CO2Coefficients[9] = v->veg[SW_SHRUB].co2_wue_coeff1;
	p_CO2Coefficients[10] = v->veg[SW_TREES].co2_wue_coeff1;
	p_CO2Coefficients[11] = v->veg[SW_FORBS].co2_wue_coeff1;
	p_CO2Coefficients[12] = v->veg[SW_GRASS].co2_wue_coeff2;
	p_CO2Coefficients[13] = v->veg[SW_SHRUB].co2_wue_coeff2;
	p_CO2Coefficients[14] = v->veg[SW_TREES].co2_wue_coeff2;
	p_CO2Coefficients[15] = v->veg[SW_FORBS].co2_wue_coeff2;

	// Integrate values with names
	PROTECT(CO2_names = allocVector(VECSXP, 2));
	SET_VECTOR_ELT(CO2_names, 1, CO2_col_names);
	SET_VECTOR_ELT(CO2_names, 0, vegtype_names);
	setAttrib(CO2Coefficients, R_DimNamesSymbol, CO2_names);

	RealD *p_Grasslands, *p_Shrublands, *p_Forest, *p_Forb;
	SEXP MonthlyVeg_Column_names, MonthlyVeg_Row_names;
	char *cMonthlyVeg_Column_names[] = { "Litter", "Biomass", "Live_pct", "LAI_conv" };

	PROTECT(swProd = MAKE_CLASS("swProd"));
	PROTECT(VegProd = NEW_OBJECT(swProd));

    PROTECT(veg_method = NEW_INTEGER(1));
    INTEGER(veg_method)[0] = v->veg_method;

	PROTECT(VegComp = allocVector(REALSXP, NVEGTYPES + 1));
	REAL(VegComp)[0] = v->veg[SW_GRASS].cov.fCover; //Grass
	REAL(VegComp)[1] = v->veg[SW_SHRUB].cov.fCover; //Shrub
	REAL(VegComp)[2] = v->veg[SW_TREES].cov.fCover; //Tree
	REAL(VegComp)[3] = v->veg[SW_FORBS].cov.fCover; //forb
	REAL(VegComp)[4] = v->bare_cov.fCover; //Bare Ground

	PROTECT(VegComp_names = allocVector(STRSXP, NVEGTYPES + 1));
	SET_STRING_ELT(VegComp_names, 0, mkChar("Grasses"));
	SET_STRING_ELT(VegComp_names, 1, mkChar("Shrubs"));
	SET_STRING_ELT(VegComp_names, 2, mkChar("Trees"));
	SET_STRING_ELT(VegComp_names, 3, mkChar("Forbs"));
	SET_STRING_ELT(VegComp_names, 4, mkChar("Bare Ground"));
	setAttrib(VegComp, R_NamesSymbol, VegComp_names);

	PROTECT(Albedo = allocVector(REALSXP, NVEGTYPES + 1));
	REAL(Albedo)[0] = v->veg[SW_GRASS].cov.albedo; //Grass
	REAL(Albedo)[1] = v->veg[SW_SHRUB].cov.albedo; //Shrub
	REAL(Albedo)[2] = v->veg[SW_TREES].cov.albedo; //Tree
	REAL(Albedo)[3] = v->veg[SW_FORBS].cov.albedo; //forb
	REAL(Albedo)[4] = v->bare_cov.albedo; //bare ground
	setAttrib(Albedo, R_NamesSymbol, VegComp_names);

	PROTECT(Canopy = allocMatrix(REALSXP, 5, NVEGTYPES));
	p_Canopy = REAL(Canopy);
	p_Canopy[0] = v->veg[SW_GRASS].cnpy.xinflec;
	p_Canopy[1] = v->veg[SW_GRASS].cnpy.yinflec;
	p_Canopy[2] = v->veg[SW_GRASS].cnpy.range;
	p_Canopy[3] = v->veg[SW_GRASS].cnpy.slope;
	p_Canopy[4] = v->veg[SW_GRASS].canopy_height_constant;
	p_Canopy[5] = v->veg[SW_SHRUB].cnpy.xinflec;
	p_Canopy[6] = v->veg[SW_SHRUB].cnpy.yinflec;
	p_Canopy[7] = v->veg[SW_SHRUB].cnpy.range;
	p_Canopy[8] = v->veg[SW_SHRUB].cnpy.slope;
	p_Canopy[9] = v->veg[SW_SHRUB].canopy_height_constant;
	p_Canopy[10] = v->veg[SW_TREES].cnpy.xinflec;
	p_Canopy[11] = v->veg[SW_TREES].cnpy.yinflec;
	p_Canopy[12] = v->veg[SW_TREES].cnpy.range;
	p_Canopy[13] = v->veg[SW_TREES].cnpy.slope;
	p_Canopy[14] = v->veg[SW_TREES].canopy_height_constant;
	p_Canopy[15] = v->veg[SW_FORBS].cnpy.xinflec;
	p_Canopy[16] = v->veg[SW_FORBS].cnpy.yinflec;
	p_Canopy[17] = v->veg[SW_FORBS].cnpy.range;
	p_Canopy[18] = v->veg[SW_FORBS].cnpy.slope;
	p_Canopy[19] = v->veg[SW_FORBS].canopy_height_constant;
	PROTECT(Canopy_names = allocVector(VECSXP, 2));
	PROTECT(Canopy_names_x = allocVector(STRSXP, 5));
	for (i = 0; i < 5; i++)
		SET_STRING_ELT(Canopy_names_x, i, mkChar(cCanopy_names_x[i]));
	SET_VECTOR_ELT(Canopy_names, 0, Canopy_names_x);
	SET_VECTOR_ELT(Canopy_names, 1, vegtype_names);
	setAttrib(Canopy, R_DimNamesSymbol, Canopy_names);

	PROTECT(VegInterception = allocMatrix(REALSXP, 2, NVEGTYPES));
	p_VegInterception = REAL(VegInterception);
	p_VegInterception[0] = v->veg[SW_GRASS].veg_kSmax;
	p_VegInterception[1] = v->veg[SW_GRASS].veg_kdead;
	p_VegInterception[2] = v->veg[SW_SHRUB].veg_kSmax;
	p_VegInterception[3] = v->veg[SW_SHRUB].veg_kdead;
	p_VegInterception[4] = v->veg[SW_TREES].veg_kSmax;
	p_VegInterception[5] = v->veg[SW_TREES].veg_kdead;
	p_VegInterception[6] = v->veg[SW_FORBS].veg_kSmax;
	p_VegInterception[7] = v->veg[SW_FORBS].veg_kdead;
	PROTECT(VegInterception_names = allocVector(VECSXP, 2));
	PROTECT(VegInterception_names_x = allocVector(STRSXP, 2));
	for (i = 0; i < 2; i++)
		SET_STRING_ELT(VegInterception_names_x, i, mkChar(cVegInterception_x[i]));
	SET_VECTOR_ELT(VegInterception_names, 0, VegInterception_names_x);
	SET_VECTOR_ELT(VegInterception_names, 1, vegtype_names);
	setAttrib(VegInterception, R_DimNamesSymbol, VegInterception_names);

	PROTECT(LitterInterception = allocMatrix(REALSXP, 1, NVEGTYPES));
	p_LitterInterception = REAL(LitterInterception);
	p_LitterInterception[0] = v->veg[SW_GRASS].lit_kSmax;
	p_LitterInterception[1] = v->veg[SW_SHRUB].lit_kSmax;
	p_LitterInterception[2] = v->veg[SW_TREES].lit_kSmax;
	p_LitterInterception[3] = v->veg[SW_FORBS].lit_kSmax;
	PROTECT(LitterInterception_names = allocVector(VECSXP, 2));
	PROTECT(LitterInterception_names_x = allocVector(STRSXP, 1));
	for (i = 0; i < 1; i++)
		SET_STRING_ELT(LitterInterception_names_x, i, mkChar(cLitterInterception_x[i]));
	SET_VECTOR_ELT(LitterInterception_names, 0, LitterInterception_names_x);
	SET_VECTOR_ELT(LitterInterception_names, 1, vegtype_names);
	setAttrib(LitterInterception, R_DimNamesSymbol, LitterInterception_names);

	PROTECT(EsTpartitioning_param = allocVector(REALSXP, NVEGTYPES));
	REAL(EsTpartitioning_param)[0] = v->veg[SW_GRASS].EsTpartitioning_param; //Grass
	REAL(EsTpartitioning_param)[1] = v->veg[SW_SHRUB].EsTpartitioning_param; //Shrub
	REAL(EsTpartitioning_param)[2] = v->veg[SW_TREES].EsTpartitioning_param; //Tree
	REAL(EsTpartitioning_param)[3] = v->veg[SW_FORBS].EsTpartitioning_param; //forb
	setAttrib(EsTpartitioning_param, R_NamesSymbol, vegtype_names);

	PROTECT(Es_param_limit = allocVector(REALSXP, NVEGTYPES));
	REAL(Es_param_limit)[0] = v->veg[SW_GRASS].Es_param_limit; //Grass
	REAL(Es_param_limit)[1] = v->veg[SW_SHRUB].Es_param_limit; //Shrub
	REAL(Es_param_limit)[2] = v->veg[SW_TREES].Es_param_limit; //Tree
	REAL(Es_param_limit)[3] = v->veg[SW_FORBS].Es_param_limit; //forb
	setAttrib(Es_param_limit, R_NamesSymbol, vegtype_names);

	PROTECT(Shade = allocMatrix(REALSXP, 6, NVEGTYPES));
	p_Shade = REAL(Shade);
	p_Shade[0] = v->veg[SW_GRASS].shade_scale;
	p_Shade[1] = v->veg[SW_GRASS].shade_deadmax;
	p_Shade[2] = v->veg[SW_GRASS].tr_shade_effects.xinflec;
	p_Shade[3] = v->veg[SW_GRASS].tr_shade_effects.yinflec;
	p_Shade[4] = v->veg[SW_GRASS].tr_shade_effects.range;
	p_Shade[5] = v->veg[SW_GRASS].tr_shade_effects.slope;
	p_Shade[6] = v->veg[SW_SHRUB].shade_scale;
	p_Shade[7] = v->veg[SW_SHRUB].shade_deadmax;
	p_Shade[8] = v->veg[SW_SHRUB].tr_shade_effects.xinflec;
	p_Shade[9] = v->veg[SW_SHRUB].tr_shade_effects.yinflec;
	p_Shade[10] = v->veg[SW_SHRUB].tr_shade_effects.range;
	p_Shade[11] = v->veg[SW_SHRUB].tr_shade_effects.slope;
	p_Shade[12] = v->veg[SW_TREES].shade_scale;
	p_Shade[13] = v->veg[SW_TREES].shade_deadmax;
	p_Shade[14] = v->veg[SW_TREES].tr_shade_effects.xinflec;
	p_Shade[15] = v->veg[SW_TREES].tr_shade_effects.yinflec;
	p_Shade[16] = v->veg[SW_TREES].tr_shade_effects.range;
	p_Shade[17] = v->veg[SW_TREES].tr_shade_effects.slope;
	p_Shade[18] = v->veg[SW_FORBS].shade_scale;
	p_Shade[19] = v->veg[SW_FORBS].shade_deadmax;
	p_Shade[20] = v->veg[SW_FORBS].tr_shade_effects.xinflec;
	p_Shade[21] = v->veg[SW_FORBS].tr_shade_effects.yinflec;
	p_Shade[22] = v->veg[SW_FORBS].tr_shade_effects.range;
	p_Shade[23] = v->veg[SW_FORBS].tr_shade_effects.slope;
	PROTECT(Shade_names = allocVector(VECSXP, 2));
	PROTECT(Shade_names_x = allocVector(STRSXP, 6));
	for (i = 0; i < 6; i++)
		SET_STRING_ELT(Shade_names_x, i, mkChar(cShade_names_x[i]));
	SET_VECTOR_ELT(Shade_names, 0, Shade_names_x);
	SET_VECTOR_ELT(Shade_names, 1, vegtype_names);
	setAttrib(Shade, R_DimNamesSymbol, Shade_names);

	PROTECT(Hydraulic_flag = allocVector(LGLSXP, NVEGTYPES));
	LOGICAL_POINTER(Hydraulic_flag)[0] = v->veg[SW_GRASS].flagHydraulicRedistribution; //Grass
	LOGICAL_POINTER(Hydraulic_flag)[1] = v->veg[SW_SHRUB].flagHydraulicRedistribution; //Shrub
	LOGICAL_POINTER(Hydraulic_flag)[2] = v->veg[SW_TREES].flagHydraulicRedistribution; //Tree
	LOGICAL_POINTER(Hydraulic_flag)[3] = v->veg[SW_FORBS].flagHydraulicRedistribution; //forb
	setAttrib(Hydraulic_flag, R_NamesSymbol, vegtype_names);

	PROTECT(Hydraulic = allocMatrix(REALSXP, 3, NVEGTYPES));
	p_Hydraulic = REAL(Hydraulic);
	p_Hydraulic[0] = v->veg[SW_GRASS].maxCondroot;
	p_Hydraulic[1] = v->veg[SW_GRASS].swpMatric50;
	p_Hydraulic[2] = v->veg[SW_GRASS].shapeCond;
	p_Hydraulic[3] = v->veg[SW_SHRUB].maxCondroot;
	p_Hydraulic[4] = v->veg[SW_SHRUB].swpMatric50;
	p_Hydraulic[5] = v->veg[SW_SHRUB].shapeCond;
	p_Hydraulic[6] = v->veg[SW_TREES].maxCondroot;
	p_Hydraulic[7] = v->veg[SW_TREES].swpMatric50;
	p_Hydraulic[8] = v->veg[SW_TREES].shapeCond;
	p_Hydraulic[9] = v->veg[SW_FORBS].maxCondroot;
	p_Hydraulic[10] = v->veg[SW_FORBS].swpMatric50;
	p_Hydraulic[11] = v->veg[SW_FORBS].shapeCond;
	PROTECT(Hydraulic_names = allocVector(VECSXP, 2));
	PROTECT(Hydraulic_names_x = allocVector(STRSXP, 3));
	for (i = 0; i < 3; i++) {
		SET_STRING_ELT(Hydraulic_names_x, i, mkChar(cHydraulic_names[i]));
	}
	SET_VECTOR_ELT(Hydraulic_names, 0, Hydraulic_names_x);
	SET_VECTOR_ELT(Hydraulic_names, 1, vegtype_names);
	setAttrib(Hydraulic, R_DimNamesSymbol, Hydraulic_names);

	PROTECT(CSWP = allocVector(REALSXP, NVEGTYPES));
	REAL(CSWP)[0] = v->veg[SW_GRASS].SWPcrit / -10; //Grass
	REAL(CSWP)[1] = v->veg[SW_SHRUB].SWPcrit / -10; //Shrub
	REAL(CSWP)[2] = v->veg[SW_TREES].SWPcrit / -10; //Tree
	REAL(CSWP)[3] = v->veg[SW_FORBS].SWPcrit / -10; //Forb
	setAttrib(CSWP, R_NamesSymbol, vegtype_names);

	PROTECT(MonthlyVeg_Column_names = allocVector(STRSXP, 4));
	for (i = 0; i < 4; i++)
		SET_STRING_ELT(MonthlyVeg_Column_names, i, mkChar(cMonthlyVeg_Column_names[i]));
	PROTECT(MonthlyVeg_Row_names = allocVector(STRSXP, 12));
	for (i = 0; i < 12; i++)
		SET_STRING_ELT(MonthlyVeg_Row_names, i, mkChar(cMonths[i]));

	PROTECT(Grasslands = allocMatrix(REALSXP, 12, 4));
	p_Grasslands = REAL(Grasslands);
	for (i = 0; i < 12; i++) {
		p_Grasslands[i + 12 * 0] = v->veg[SW_GRASS].litter[i];
		p_Grasslands[i + 12 * 1] = v->veg[SW_GRASS].biomass[i];
		p_Grasslands[i + 12 * 2] = v->veg[SW_GRASS].pct_live[i];
		p_Grasslands[i + 12 * 3] = v->veg[SW_GRASS].lai_conv[i];
	}
	PROTECT(Grasslands_names = allocVector(VECSXP, 2));
	SET_VECTOR_ELT(Grasslands_names, 0, MonthlyVeg_Row_names);
	SET_VECTOR_ELT(Grasslands_names, 1, MonthlyVeg_Column_names);
	setAttrib(Grasslands, R_DimNamesSymbol, Grasslands_names);

	PROTECT(Shrublands = allocMatrix(REALSXP, 12, 4));
	p_Shrublands = REAL(Shrublands);
	for (i = 0; i < 12; i++) {
		p_Shrublands[i + 12 * 0] = v->veg[SW_SHRUB].litter[i];
		p_Shrublands[i + 12 * 1] = v->veg[SW_SHRUB].biomass[i];
		p_Shrublands[i + 12 * 2] = v->veg[SW_SHRUB].pct_live[i];
		p_Shrublands[i + 12 * 3] = v->veg[SW_SHRUB].lai_conv[i];
	}
	PROTECT(Shrublands_names = allocVector(VECSXP, 2));
	SET_VECTOR_ELT(Shrublands_names, 0, MonthlyVeg_Row_names);
	SET_VECTOR_ELT(Shrublands_names, 1, MonthlyVeg_Column_names);
	setAttrib(Shrublands, R_DimNamesSymbol, Shrublands_names);

	PROTECT(Forest = allocMatrix(REALSXP, 12, 4));
	p_Forest = REAL(Forest);
	for (i = 0; i < 12; i++) {
		p_Forest[i + 12 * 0] = v->veg[SW_TREES].litter[i];
		p_Forest[i + 12 * 1] = v->veg[SW_TREES].biomass[i];
		p_Forest[i + 12 * 2] = v->veg[SW_TREES].pct_live[i];
		p_Forest[i + 12 * 3] = v->veg[SW_TREES].lai_conv[i];
	}
	PROTECT(Forest_names = allocVector(VECSXP, 2));
	SET_VECTOR_ELT(Forest_names, 0, MonthlyVeg_Row_names);
	SET_VECTOR_ELT(Forest_names, 1, MonthlyVeg_Column_names);
	setAttrib(Forest, R_DimNamesSymbol, Forest_names);

	PROTECT(Forb = allocMatrix(REALSXP, 12, 4));
	p_Forb = REAL(Forb);
	for (i = 0; i < 12; i++) {
		p_Forb[i + 12 * 0] = v->veg[SW_FORBS].litter[i];
		p_Forb[i + 12 * 1] = v->veg[SW_FORBS].biomass[i];
		p_Forb[i + 12 * 2] = v->veg[SW_FORBS].pct_live[i];
		p_Forb[i + 12 * 3] = v->veg[SW_FORBS].lai_conv[i];
	}
	PROTECT(Forb_names = allocVector(VECSXP, 2));
	SET_VECTOR_ELT(Forb_names, 0, MonthlyVeg_Row_names);
	SET_VECTOR_ELT(Forb_names, 1, MonthlyVeg_Column_names);
	setAttrib(Forb, R_DimNamesSymbol, Forb_names);

	PROTECT(MonthlyVeg = allocVector(VECSXP, NVEGTYPES));
	SET_VECTOR_ELT(MonthlyVeg, SW_TREES, Forest);
	SET_VECTOR_ELT(MonthlyVeg, SW_SHRUB, Shrublands);
	SET_VECTOR_ELT(MonthlyVeg, SW_FORBS, Forb);
	SET_VECTOR_ELT(MonthlyVeg, SW_GRASS, Grasslands);

	PROTECT(col_names = allocVector(STRSXP, NVEGTYPES));
	SET_STRING_ELT(col_names, 0, mkChar("Trees"));
	SET_STRING_ELT(col_names, 1, mkChar("Shrubs"));
	SET_STRING_ELT(col_names, 2, mkChar("Forbs"));
	SET_STRING_ELT(col_names, 3, mkChar("Grasses"));
	setAttrib(MonthlyVeg, R_NamesSymbol, col_names);

    SET_SLOT(VegProd, install(cVegProd_names[0]), veg_method);
	SET_SLOT(VegProd, install(cVegProd_names[1]), VegComp);
	SET_SLOT(VegProd, install(cVegProd_names[2]), Albedo);
	SET_SLOT(VegProd, install(cVegProd_names[3]), Canopy);
	SET_SLOT(VegProd, install(cVegProd_names[4]), VegInterception);
	SET_SLOT(VegProd, install(cVegProd_names[5]), LitterInterception);
	SET_SLOT(VegProd, install(cVegProd_names[6]), EsTpartitioning_param);
	SET_SLOT(VegProd, install(cVegProd_names[7]), Es_param_limit);
	SET_SLOT(VegProd, install(cVegProd_names[8]), Shade);
	SET_SLOT(VegProd, install(cVegProd_names[9]), Hydraulic_flag);
	SET_SLOT(VegProd, install(cVegProd_names[10]), Hydraulic);
	SET_SLOT(VegProd, install(cVegProd_names[11]), CSWP);
	SET_SLOT(VegProd, install(cVegProd_names[12]), MonthlyVeg);
	SET_SLOT(VegProd, install(cVegProd_names[13]), CO2Coefficients);

	UNPROTECT(41);
	return VegProd;
}

void onSet_SW_VPD(SEXP SW_VPD) {
	int i;
	SW_VEGPROD *v = &SoilWatAll.VegProd;

    SEXP veg_method;
	SEXP VegComp;
	SEXP Albedo;
	SEXP Canopy;
	RealD *p_Canopy;
	SEXP VegInterception;
	RealD *p_VegInterception;
	SEXP LitterInterception;
	RealD *p_LitterInterception;
	SEXP EsTpartitioning_param;
	SEXP Es_param_limit;
	SEXP Shade;
	RealD *p_Shade;
	SEXP Hydraulic;
	SEXP Hydraulic_flag;
	SEXP CSWP;
	SEXP MonthlyVeg, Grasslands, Shrublands, Forest, Forb;
	SEXP CO2Coefficients;
	RealD *p_Grasslands, *p_Shrublands, *p_Forest, *p_Forb;

	MyFileName = PathInfo.InFiles[eVegProd];

    PROTECT(veg_method = GET_SLOT(SW_VPD, install(cVegProd_names[0])));
    v->veg_method = INTEGER(veg_method)[0];

	PROTECT(VegComp = GET_SLOT(SW_VPD, install(cVegProd_names[1])));
	v->veg[SW_GRASS].cov.fCover = REAL(VegComp)[0]; //Grass
	v->veg[SW_SHRUB].cov.fCover = REAL(VegComp)[1]; //Shrub
	v->veg[SW_TREES].cov.fCover = REAL(VegComp)[2]; //Tree
	v->veg[SW_FORBS].cov.fCover = REAL(VegComp)[3]; //Forb
	v->bare_cov.fCover = REAL(VegComp)[4]; //Bare Ground

	PROTECT(Albedo = GET_SLOT(SW_VPD, install(cVegProd_names[2])));
	v->veg[SW_GRASS].cov.albedo = REAL(Albedo)[0]; //Grass
	v->veg[SW_SHRUB].cov.albedo = REAL(Albedo)[1]; //Shrub
	v->veg[SW_TREES].cov.albedo = REAL(Albedo)[2]; //Tree
	v->veg[SW_FORBS].cov.albedo = REAL(Albedo)[3]; //Forb
	v->bare_cov.albedo = REAL(Albedo)[4]; //Bare Ground

	PROTECT(Canopy = GET_SLOT(SW_VPD, install(cVegProd_names[3])));
	p_Canopy = REAL(Canopy);
	v->veg[SW_GRASS].cnpy.xinflec = p_Canopy[0];
	v->veg[SW_GRASS].cnpy.yinflec = p_Canopy[1];
	v->veg[SW_GRASS].cnpy.range = p_Canopy[2];
	v->veg[SW_GRASS].cnpy.slope = p_Canopy[3];
	v->veg[SW_GRASS].canopy_height_constant = p_Canopy[4];
	v->veg[SW_SHRUB].cnpy.xinflec = p_Canopy[5];
	v->veg[SW_SHRUB].cnpy.yinflec = p_Canopy[6];
	v->veg[SW_SHRUB].cnpy.range = p_Canopy[7];
	v->veg[SW_SHRUB].cnpy.slope = p_Canopy[8];
	v->veg[SW_SHRUB].canopy_height_constant = p_Canopy[9];
	v->veg[SW_TREES].cnpy.xinflec = p_Canopy[10];
	v->veg[SW_TREES].cnpy.yinflec = p_Canopy[11];
	v->veg[SW_TREES].cnpy.range = p_Canopy[12];
	v->veg[SW_TREES].cnpy.slope = p_Canopy[13];
	v->veg[SW_TREES].canopy_height_constant = p_Canopy[14];
	v->veg[SW_FORBS].cnpy.xinflec = p_Canopy[15];
	v->veg[SW_FORBS].cnpy.yinflec = p_Canopy[16];
	v->veg[SW_FORBS].cnpy.range = p_Canopy[17];
	v->veg[SW_FORBS].cnpy.slope = p_Canopy[18];
	v->veg[SW_FORBS].canopy_height_constant = p_Canopy[19];

	PROTECT(VegInterception = GET_SLOT(SW_VPD, install(cVegProd_names[4])));
	p_VegInterception = REAL(VegInterception);
	v->veg[SW_GRASS].veg_kSmax = p_VegInterception[0];
	v->veg[SW_GRASS].veg_kdead = p_VegInterception[1];
	v->veg[SW_SHRUB].veg_kSmax = p_VegInterception[2];
	v->veg[SW_SHRUB].veg_kdead = p_VegInterception[3];
	v->veg[SW_TREES].veg_kSmax = p_VegInterception[4];
	v->veg[SW_TREES].veg_kdead = p_VegInterception[5];
	v->veg[SW_FORBS].veg_kSmax = p_VegInterception[6];
	v->veg[SW_FORBS].veg_kdead = p_VegInterception[7];

	PROTECT(LitterInterception = GET_SLOT(SW_VPD, install(cVegProd_names[5])));
	p_LitterInterception = REAL(LitterInterception);
	v->veg[SW_GRASS].lit_kSmax = p_LitterInterception[0];
	v->veg[SW_SHRUB].lit_kSmax = p_LitterInterception[1];
	v->veg[SW_TREES].lit_kSmax = p_LitterInterception[2];
	v->veg[SW_FORBS].lit_kSmax = p_LitterInterception[3];

	PROTECT(EsTpartitioning_param = GET_SLOT(SW_VPD, install(cVegProd_names[6])));
	v->veg[SW_GRASS].EsTpartitioning_param = REAL(EsTpartitioning_param)[0]; //Grass
	v->veg[SW_SHRUB].EsTpartitioning_param = REAL(EsTpartitioning_param)[1]; //Shrub
	v->veg[SW_TREES].EsTpartitioning_param = REAL(EsTpartitioning_param)[2]; //Tree
	v->veg[SW_FORBS].EsTpartitioning_param = REAL(EsTpartitioning_param)[3]; //Forb

	PROTECT(Es_param_limit = GET_SLOT(SW_VPD, install(cVegProd_names[7])));
	v->veg[SW_GRASS].Es_param_limit = REAL(Es_param_limit)[0]; //Grass
	v->veg[SW_SHRUB].Es_param_limit = REAL(Es_param_limit)[1]; //Shrub
	v->veg[SW_TREES].Es_param_limit = REAL(Es_param_limit)[2]; //Tree
	v->veg[SW_FORBS].Es_param_limit = REAL(Es_param_limit)[3]; //Forb

	PROTECT(Shade = GET_SLOT(SW_VPD, install(cVegProd_names[8])));
	p_Shade = REAL(Shade);
	v->veg[SW_GRASS].shade_scale = p_Shade[0];
	v->veg[SW_GRASS].shade_deadmax = p_Shade[1];
	v->veg[SW_GRASS].tr_shade_effects.xinflec = p_Shade[2];
	v->veg[SW_GRASS].tr_shade_effects.yinflec = p_Shade[3];
	v->veg[SW_GRASS].tr_shade_effects.range = p_Shade[4];
	v->veg[SW_GRASS].tr_shade_effects.slope = p_Shade[5];
	v->veg[SW_SHRUB].shade_scale = p_Shade[6];
	v->veg[SW_SHRUB].shade_deadmax = p_Shade[7];
	v->veg[SW_SHRUB].tr_shade_effects.xinflec = p_Shade[8];
	v->veg[SW_SHRUB].tr_shade_effects.yinflec = p_Shade[9];
	v->veg[SW_SHRUB].tr_shade_effects.range = p_Shade[10];
	v->veg[SW_SHRUB].tr_shade_effects.slope = p_Shade[11];
	v->veg[SW_TREES].shade_scale = p_Shade[12];
	v->veg[SW_TREES].shade_deadmax = p_Shade[13];
	v->veg[SW_TREES].tr_shade_effects.xinflec = p_Shade[14];
	v->veg[SW_TREES].tr_shade_effects.yinflec = p_Shade[15];
	v->veg[SW_TREES].tr_shade_effects.range = p_Shade[16];
	v->veg[SW_TREES].tr_shade_effects.slope = p_Shade[17];
	v->veg[SW_FORBS].shade_scale = p_Shade[18];
	v->veg[SW_FORBS].shade_deadmax = p_Shade[19];
	v->veg[SW_FORBS].tr_shade_effects.xinflec = p_Shade[20];
	v->veg[SW_FORBS].tr_shade_effects.yinflec = p_Shade[21];
	v->veg[SW_FORBS].tr_shade_effects.range = p_Shade[22];
	v->veg[SW_FORBS].tr_shade_effects.slope = p_Shade[23];

	PROTECT(Hydraulic_flag = GET_SLOT(SW_VPD, install(cVegProd_names[9])));
	PROTECT(Hydraulic = GET_SLOT(SW_VPD, install(cVegProd_names[10])));
	v->veg[SW_GRASS].flagHydraulicRedistribution = LOGICAL_POINTER(Hydraulic_flag)[0]; //Grass
	v->veg[SW_SHRUB].flagHydraulicRedistribution = LOGICAL_POINTER(Hydraulic_flag)[1]; //Shrub
	v->veg[SW_TREES].flagHydraulicRedistribution = LOGICAL_POINTER(Hydraulic_flag)[2]; //Tree
	v->veg[SW_FORBS].flagHydraulicRedistribution = LOGICAL_POINTER(Hydraulic_flag)[3]; //Forb
	v->veg[SW_GRASS].maxCondroot = REAL(Hydraulic)[0]; //Grass
	v->veg[SW_GRASS].swpMatric50 = REAL(Hydraulic)[1]; //Grass
	v->veg[SW_GRASS].shapeCond = REAL(Hydraulic)[2]; //Grass
	v->veg[SW_SHRUB].maxCondroot = REAL(Hydraulic)[3]; //Shrub
	v->veg[SW_SHRUB].swpMatric50 = REAL(Hydraulic)[4]; //Shrub
	v->veg[SW_SHRUB].shapeCond = REAL(Hydraulic)[5]; //Shrub
	v->veg[SW_TREES].maxCondroot = REAL(Hydraulic)[6]; //Tree
	v->veg[SW_TREES].swpMatric50 = REAL(Hydraulic)[7]; //Tree
	v->veg[SW_TREES].shapeCond = REAL(Hydraulic)[8]; //Tree
	v->veg[SW_FORBS].maxCondroot = REAL(Hydraulic)[9]; //Forb
	v->veg[SW_FORBS].swpMatric50 = REAL(Hydraulic)[10]; //Forb
	v->veg[SW_FORBS].shapeCond = REAL(Hydraulic)[11]; //Forb

	PROTECT(CSWP = GET_SLOT(SW_VPD, install(cVegProd_names[11])));
	v->veg[SW_GRASS].SWPcrit = -10 * REAL(CSWP)[0]; //Grass
	v->veg[SW_SHRUB].SWPcrit = -10 * REAL(CSWP)[1]; //Shrub
	v->veg[SW_TREES].SWPcrit = -10 * REAL(CSWP)[2]; //Tree
	v->veg[SW_FORBS].SWPcrit = -10 * REAL(CSWP)[3]; //Forb

	// getting critSoilWater for use with SWA and get_critical_rank()
	// critSoilWater goes tree, shrub, forb, grass
	v->critSoilWater[0] = REAL(CSWP)[2];
	v->critSoilWater[1] = REAL(CSWP)[1];
	v->critSoilWater[2] = REAL(CSWP)[3];
	v->critSoilWater[3] = REAL(CSWP)[0];

	get_critical_rank(&SoilWatAll.VegProd);

	PROTECT(MonthlyVeg = GET_SLOT(SW_VPD, install(cVegProd_names[12])));
	PROTECT(Grasslands = VECTOR_ELT(MonthlyVeg, SW_GRASS));
	p_Grasslands = REAL(Grasslands);
	for (i = 0; i < 12; i++) {
		v->veg[SW_GRASS].litter[i] = p_Grasslands[i + 12 * 0];
		v->veg[SW_GRASS].biomass[i] = p_Grasslands[i + 12 * 1];
		v->veg[SW_GRASS].pct_live[i] = p_Grasslands[i + 12 * 2];
		v->veg[SW_GRASS].lai_conv[i] = p_Grasslands[i + 12 * 3];
	}
	PROTECT(Shrublands = VECTOR_ELT(MonthlyVeg, SW_SHRUB));
	p_Shrublands = REAL(Shrublands);
	for (i = 0; i < 12; i++) {
		v->veg[SW_SHRUB].litter[i] = p_Shrublands[i + 12 * 0];
		v->veg[SW_SHRUB].biomass[i] = p_Shrublands[i + 12 * 1];
		v->veg[SW_SHRUB].pct_live[i] = p_Shrublands[i + 12 * 2];
		v->veg[SW_SHRUB].lai_conv[i] = p_Shrublands[i + 12 * 3];
	}
	PROTECT(Forest = VECTOR_ELT(MonthlyVeg, SW_TREES));
	p_Forest = REAL(Forest);
	for (i = 0; i < 12; i++) {
		v->veg[SW_TREES].litter[i] = p_Forest[i + 12 * 0];
		v->veg[SW_TREES].biomass[i] = p_Forest[i + 12 * 1];
		v->veg[SW_TREES].pct_live[i] = p_Forest[i + 12 * 2];
		v->veg[SW_TREES].lai_conv[i] = p_Forest[i + 12 * 3];
	}
	PROTECT(Forb = VECTOR_ELT(MonthlyVeg, SW_FORBS));
	p_Forb = REAL(Forb);
	for (i = 0; i < 12; i++) {
		v->veg[SW_FORBS].litter[i] = p_Forb[i + 12 * 0];
		v->veg[SW_FORBS].biomass[i] = p_Forb[i + 12 * 1];
		v->veg[SW_FORBS].pct_live[i] = p_Forb[i + 12 * 2];
		v->veg[SW_FORBS].lai_conv[i] = p_Forb[i + 12 * 3];
	}

	PROTECT(CO2Coefficients = GET_SLOT(SW_VPD, install(cVegProd_names[13])));
	v->veg[SW_GRASS].co2_bio_coeff1 = REAL(CO2Coefficients)[0];
	v->veg[SW_SHRUB].co2_bio_coeff1 = REAL(CO2Coefficients)[1];
	v->veg[SW_TREES].co2_bio_coeff1 = REAL(CO2Coefficients)[2];
	v->veg[SW_FORBS].co2_bio_coeff1 = REAL(CO2Coefficients)[3];
	v->veg[SW_GRASS].co2_bio_coeff2 = REAL(CO2Coefficients)[4];
	v->veg[SW_SHRUB].co2_bio_coeff2 = REAL(CO2Coefficients)[5];
	v->veg[SW_TREES].co2_bio_coeff2 = REAL(CO2Coefficients)[6];
	v->veg[SW_FORBS].co2_bio_coeff2 = REAL(CO2Coefficients)[7];
	v->veg[SW_GRASS].co2_wue_coeff1 = REAL(CO2Coefficients)[8];
	v->veg[SW_SHRUB].co2_wue_coeff1 = REAL(CO2Coefficients)[9];
	v->veg[SW_TREES].co2_wue_coeff1 = REAL(CO2Coefficients)[10];
	v->veg[SW_FORBS].co2_wue_coeff1 = REAL(CO2Coefficients)[11];
	v->veg[SW_GRASS].co2_wue_coeff2 = REAL(CO2Coefficients)[12];
	v->veg[SW_SHRUB].co2_wue_coeff2 = REAL(CO2Coefficients)[13];
	v->veg[SW_TREES].co2_wue_coeff2 = REAL(CO2Coefficients)[14];
	v->veg[SW_FORBS].co2_wue_coeff2 = REAL(CO2Coefficients)[15];


  SW_VPD_fix_cover(&SoilWatAll.VegProd, &LogInfo);
  if(LogInfo.stopRun) {
    UNPROTECT(18); // Unprotect the eighteen protected variables before exiting
    return; // Exit function prematurely due to error
  }

	if (EchoInits)
		_echo_VegProd(SoilWatAll.VegProd.veg, SoilWatAll.VegProd.bare_cov);

	UNPROTECT(18);
}

SEXP rSW2_estimate_PotNatVeg_composition(SEXP MAP_mm, SEXP MAT_C, SEXP mean_monthly_ppt_mm,
                                         SEXP mean_monthly_Temp_C, SEXP shrub_limit, SEXP SumGrasses_Fraction,
                                         SEXP fill_empty_with_BareGround, SEXP warn_extrapolation, SEXP dailyC4vars,
                                         SEXP isNorth, SEXP fixBareGround, SEXP Succulents_Fraction, SEXP Annuals_Fraction,
                                         SEXP C4_Fraction, SEXP C3_Fraction, SEXP Shrubs_Fraction, SEXP Forbs_Fraction,
                                         SEXP Trees_Fraction, SEXP BareGround_Fraction) {

    double RelAbundanceL0[8], RelAbundanceL1[5], grasses[3];
    LOG_INFO local_LogInfo;
    sw_init_logs(LogInfo.logfp, &local_LogInfo);

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

    char *RelAbundanceL0Names[] = {"Succulents", "Forbs", "Grasses_C3", "Grasses_C4",
        "Grasses_Annuals", "Shrubs", "Trees", "BareGround"};
    char *RelAbundanceL1Names[] = {"SW_TREES", "SW_SHRUB", "SW_FORBS", "SW_GRASS", "SW_BAREGROUND"};
    char *finalListNames[] = {"Rel_Abundance_L0", "Rel_Abundance_L1", "Grasses"};
    char *grassesNames[] = {"Grasses_C3", "Grasses_C4", "Grasses_Annuals"};

    // "inter_" meaning the intermediate R -> C conversion
    int index, julyMin = 0, degAbove65 = 1, frostFreeDays = 2;

    Bool final_fill_empty_with_BareGround = (Bool) asLogical(fill_empty_with_BareGround) ? swTRUE : swFALSE,
    final_warn_extrapolation = (Bool) asLogical(warn_extrapolation) ? swTRUE : swFALSE,
    final_isNorth = (Bool) asLogical(isNorth) ? swTRUE : swFALSE,
    final_fix_bareGround = (Bool) asLogical(fixBareGround) ? swTRUE : swFALSE;

    SEXP cRelAbL1Names, cRelAbL0Names, cfinalNames, cgrasses,
    final_RelAbundanceL1, final_RelAbundanceL0, final_grasses, res;

    res = PROTECT(allocVector(VECSXP, 3));
    final_RelAbundanceL0 = PROTECT(allocVector(REALSXP, 8));
    final_RelAbundanceL1 = PROTECT(allocVector(REALSXP, 5));
    final_grasses = PROTECT(allocVector(REALSXP, 3));
    cRelAbL1Names = PROTECT(allocVector(STRSXP, 5));
    cRelAbL0Names = PROTECT(allocVector(STRSXP, 8));
    cfinalNames = PROTECT(allocVector(STRSXP, 3));
    cgrasses = PROTECT(allocVector(STRSXP, 3));

    for(index = 0; index < 3; index++) {
        SET_STRING_ELT(cfinalNames, index, mkChar(finalListNames[index]));
        SET_STRING_ELT(cgrasses, index, mkChar(grassesNames[index]));
    }

    for(index = 0; index < 8; index++) {
        SET_STRING_ELT(cRelAbL0Names, index, mkChar(RelAbundanceL0Names[index]));
        if(index < 5) SET_STRING_ELT(cRelAbL1Names, index, mkChar(RelAbundanceL1Names[index]));
    }

    namesgets(final_RelAbundanceL0, cRelAbL0Names);
    namesgets(final_RelAbundanceL1, cRelAbL1Names);
    namesgets(res, cfinalNames);
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
          final_fix_bareGround, grasses, RelAbundanceL0, RelAbundanceL1, &local_LogInfo);
    if(local_LogInfo.stopRun) {
        goto report;
    }

    for(index = 0; index < 8; index++) {
        REAL(final_RelAbundanceL0)[index] = RelAbundanceL0[index];
        if(index < 5) {
            REAL(final_RelAbundanceL1)[index] = RelAbundanceL1[index];
        }
        if(index < 3) {
            REAL(final_grasses)[index] = grasses[index];
        }
    }

    SET_VECTOR_ELT(res, 0, final_RelAbundanceL0);
    SET_VECTOR_ELT(res, 1, final_RelAbundanceL1);
    SET_VECTOR_ELT(res, 2, final_grasses);

    report: {
        UNPROTECT(8);

        if(local_LogInfo.numWarnings > 0) {
            sw_write_logs(FALSE, &local_LogInfo); // Note: `FALSE` is not used
        }

        if(local_LogInfo.stopRun) {
            SW_CTL_clear_model(FALSE, &SoilWatAll, &PathInfo);
            sw_check_exit(FALSE, &local_LogInfo); // Note: `FALSE` is not used
        }
    }

    return res;

}
