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

#include "SOILWAT2/generic.h"
#include "SOILWAT2/filefuncs.h"
#include "SOILWAT2/Times.h"
#include "SOILWAT2/myMemory.h"

#include "SOILWAT2/SW_Defines.h"
#include "SOILWAT2/SW_Files.h"

#include "SOILWAT2/SW_VegProd.h"
#include "rSW_VegProd.h"

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>


/* =================================================== */
/*                  Global Variables                   */
/* --------------------------------------------------- */
extern Bool EchoInits;
extern SW_VEGPROD SW_VegProd;

/* =================================================== */
/*                Module-Level Variables               */
/* --------------------------------------------------- */
static char *MyFileName;

/* =================================================== */
/* =================================================== */
/*             Private Function Definitions            */
/* --------------------------------------------------- */


/* =================================================== */
/* =================================================== */
/*             Public Function Definitions             */
/* --------------------------------------------------- */


SEXP onGet_SW_VPD() {
	int i;
	SW_VEGPROD *v = &SW_VegProd;
	SEXP swProd;
	SEXP VegProd;
	char *cVegProd_names[] = { "Composition", "Albedo", "Cover_stcr", "CanopyHeight", "VegetationInterceptionParameters", "LitterInterceptionParameters",
		"EsTpartitioning_param", "Es_param_limit", "Shade", "HydraulicRedistribution_use", "HydraulicRedistribution", "CriticalSoilWaterPotential",
		"MonthlyProductionValues_grass", "MonthlyProductionValues_shrub", "MonthlyProductionValues_tree", "MonthlyProductionValues_forb", "CO2Coefficients" };

	SEXP VegComp, VegComp_names;
	SEXP Albedo;
	SEXP conv_stcr, col_names;

	SEXP Canopy, Canopy_names, Canopy_names_x;
	char *cCanopy_names_x[] = { "xinflec", "yinflec", "range", "slope", "height_cm" };
	RealD *p_Canopy;

	SEXP VegInterception, VegInterception_names, VegInterception_names_x;
	char *cVegInterception_x[] = { "a", "b", "c", "d" };
	RealD *p_VegInterception;

	SEXP LitterInterception, LitterInterception_names, LitterInterception_names_x;
	char *cLitterInterception_x[] = { "a", "b", "c", "d" };
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

	SEXP Grasslands, Grasslands_names;
	SEXP Shrublands, Shrublands_names;
	SEXP Forest, Forest_names;
	SEXP Forb, Forb_names;

	/* CO2 */
	// Initialize variables
	SEXP CO2Coefficients, CO2_names, CO2_row_names, CO2_col_names;
	RealD *p_CO2Coefficients;

	// Create row and column names
	char *cCO2_row_names[] = { "Grasses", "Shrubs", "Trees", "Forbs" };
	char *cCO2_col_names[] = { "Biomass Coeff1", "Biomass Coeff2", "WUE Coeff1", "WUE Coeff2" };
	PROTECT(CO2_col_names = allocVector(STRSXP, 4));
	for (i = 0; i < 4; i++)
		SET_STRING_ELT(CO2_col_names, i, mkChar(cCO2_col_names[i]));
	PROTECT(CO2_row_names = allocVector(STRSXP, 4));
	for (i = 0; i < 4; i++)
		SET_STRING_ELT(CO2_row_names, i, mkChar(cCO2_row_names[i]));

	// Create matrix containing the multipliers
	PROTECT(CO2Coefficients = allocMatrix(REALSXP, 4, 4));
	p_CO2Coefficients = REAL(CO2Coefficients);
	p_CO2Coefficients[0] = v->grass.co2_bio_coeff1;
	p_CO2Coefficients[1] = v->shrub.co2_bio_coeff1;
	p_CO2Coefficients[2] = v->tree.co2_bio_coeff1;
	p_CO2Coefficients[3] = v->forb.co2_bio_coeff1;
	p_CO2Coefficients[4] = v->grass.co2_bio_coeff2;
	p_CO2Coefficients[5] = v->shrub.co2_bio_coeff2;
	p_CO2Coefficients[6] = v->tree.co2_bio_coeff2;
	p_CO2Coefficients[7] = v->forb.co2_bio_coeff2;
	p_CO2Coefficients[8] = v->grass.co2_wue_coeff1;
	p_CO2Coefficients[9] = v->shrub.co2_wue_coeff1;
	p_CO2Coefficients[10] = v->tree.co2_wue_coeff1;
	p_CO2Coefficients[11] = v->forb.co2_wue_coeff1;
	p_CO2Coefficients[12] = v->grass.co2_wue_coeff2;
	p_CO2Coefficients[13] = v->shrub.co2_wue_coeff2;
	p_CO2Coefficients[14] = v->tree.co2_wue_coeff2;
	p_CO2Coefficients[15] = v->forb.co2_wue_coeff2;

	// Integrate values with names
	PROTECT(CO2_names = allocVector(VECSXP, 2));
	SET_VECTOR_ELT(CO2_names, 1, CO2_col_names);
	SET_VECTOR_ELT(CO2_names, 0, CO2_row_names);
	setAttrib(CO2Coefficients, R_DimNamesSymbol, CO2_names);

	RealD *p_Grasslands, *p_Shrublands, *p_Forest, *p_Forb;
	SEXP MonthlyProductionValues_Column_names, MonthlyProductionValues_Row_names;
	char *cMonthlyProductionValues_Column_names[] = { "Litter", "Biomass", "Live_pct", "LAI_conv" };
	char *cMonths[] = { "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December" };

	PROTECT(swProd = MAKE_CLASS("swProd"));
	PROTECT(VegProd = NEW_OBJECT(swProd));

	PROTECT(VegComp = allocVector(REALSXP, 5));
	REAL(VegComp)[0] = v->grass.cov.fCover; //Grass
	REAL(VegComp)[1] = v->shrub.cov.fCover; //Shrub
	REAL(VegComp)[2] = v->tree.cov.fCover; //Tree
	REAL(VegComp)[3] = v->forb.cov.fCover; //forb
	REAL(VegComp)[4] = v->bare_cov.fCover; //Bare Ground

	PROTECT(VegComp_names = allocVector(STRSXP, 5));
	SET_STRING_ELT(VegComp_names, 0, mkChar("Grasses"));
	SET_STRING_ELT(VegComp_names, 1, mkChar("Shrubs"));
	SET_STRING_ELT(VegComp_names, 2, mkChar("Trees"));
	SET_STRING_ELT(VegComp_names, 3, mkChar("Forbs"));
	SET_STRING_ELT(VegComp_names, 4, mkChar("Bare Ground"));
	setAttrib(VegComp, R_NamesSymbol, VegComp_names);

	PROTECT(Albedo = allocVector(REALSXP, 5));
	REAL(Albedo)[0] = v->grass.cov.albedo; //Grass
	REAL(Albedo)[1] = v->shrub.cov.albedo; //Shrub
	REAL(Albedo)[2] = v->tree.cov.albedo; //Tree
	REAL(Albedo)[3] = v->forb.cov.albedo; //forb
	REAL(Albedo)[4] = v->bare_cov.albedo; //bare ground
	setAttrib(Albedo, R_NamesSymbol, VegComp_names);

	PROTECT(conv_stcr = allocVector(REALSXP, 4));
	REAL(conv_stcr)[0] = v->grass.conv_stcr; //Grass
	REAL(conv_stcr)[1] = v->shrub.conv_stcr; //Shrub
	REAL(conv_stcr)[2] = v->tree.conv_stcr; //Tree
	REAL(conv_stcr)[3] = v->forb.conv_stcr; //forb

	PROTECT(col_names = allocVector(STRSXP, 4));
	SET_STRING_ELT(col_names, 0, mkChar("Grasses"));
	SET_STRING_ELT(col_names, 1, mkChar("Shrubs"));
	SET_STRING_ELT(col_names, 2, mkChar("Trees"));
	SET_STRING_ELT(col_names, 3, mkChar("Forbs"));
	setAttrib(conv_stcr, R_NamesSymbol, col_names);

	PROTECT(Canopy = allocMatrix(REALSXP, 5, 4));
	p_Canopy = REAL(Canopy);
	p_Canopy[0] = v->grass.cnpy.xinflec;
	p_Canopy[1] = v->grass.cnpy.yinflec;
	p_Canopy[2] = v->grass.cnpy.range;
	p_Canopy[3] = v->grass.cnpy.slope;
	p_Canopy[4] = v->grass.canopy_height_constant;
	p_Canopy[5] = v->shrub.cnpy.xinflec;
	p_Canopy[6] = v->shrub.cnpy.yinflec;
	p_Canopy[7] = v->shrub.cnpy.range;
	p_Canopy[8] = v->shrub.cnpy.slope;
	p_Canopy[9] = v->shrub.canopy_height_constant;
	p_Canopy[10] = v->tree.cnpy.xinflec;
	p_Canopy[11] = v->tree.cnpy.yinflec;
	p_Canopy[12] = v->tree.cnpy.range;
	p_Canopy[13] = v->tree.cnpy.slope;
	p_Canopy[14] = v->tree.canopy_height_constant;
	p_Canopy[15] = v->forb.cnpy.xinflec;
	p_Canopy[16] = v->forb.cnpy.yinflec;
	p_Canopy[17] = v->forb.cnpy.range;
	p_Canopy[18] = v->forb.cnpy.slope;
	p_Canopy[19] = v->forb.canopy_height_constant;
	PROTECT(Canopy_names = allocVector(VECSXP, 2));
	PROTECT(Canopy_names_x = allocVector(STRSXP, 5));
	for (i = 0; i < 5; i++)
		SET_STRING_ELT(Canopy_names_x, i, mkChar(cCanopy_names_x[i]));
	SET_VECTOR_ELT(Canopy_names, 0, Canopy_names_x);
	SET_VECTOR_ELT(Canopy_names, 1, col_names);
	setAttrib(Canopy, R_DimNamesSymbol, Canopy_names);

	PROTECT(VegInterception = allocMatrix(REALSXP, 4, 4));
	p_VegInterception = REAL(VegInterception);
	p_VegInterception[0] = v->grass.veg_intPPT_a;
	p_VegInterception[1] = v->grass.veg_intPPT_b;
	p_VegInterception[2] = v->grass.veg_intPPT_c;
	p_VegInterception[3] = v->grass.veg_intPPT_d;
	p_VegInterception[4] = v->shrub.veg_intPPT_a;
	p_VegInterception[5] = v->shrub.veg_intPPT_b;
	p_VegInterception[6] = v->shrub.veg_intPPT_c;
	p_VegInterception[7] = v->shrub.veg_intPPT_d;
	p_VegInterception[8] = v->tree.veg_intPPT_a;
	p_VegInterception[9] = v->tree.veg_intPPT_b;
	p_VegInterception[10] = v->tree.veg_intPPT_c;
	p_VegInterception[11] = v->tree.veg_intPPT_d;
	p_VegInterception[12] = v->forb.veg_intPPT_a;
	p_VegInterception[13] = v->forb.veg_intPPT_b;
	p_VegInterception[14] = v->forb.veg_intPPT_c;
	p_VegInterception[15] = v->forb.veg_intPPT_d;
	PROTECT(VegInterception_names = allocVector(VECSXP, 2));
	PROTECT(VegInterception_names_x = allocVector(STRSXP, 4));
	for (i = 0; i < 4; i++)
		SET_STRING_ELT(VegInterception_names_x, i, mkChar(cVegInterception_x[i]));
	SET_VECTOR_ELT(VegInterception_names, 0, VegInterception_names_x);
	SET_VECTOR_ELT(VegInterception_names, 1, col_names);
	setAttrib(VegInterception, R_DimNamesSymbol, VegInterception_names);

	PROTECT(LitterInterception = allocMatrix(REALSXP, 4, 4));
	p_LitterInterception = REAL(LitterInterception);
	p_LitterInterception[0] = v->grass.litt_intPPT_a;
	p_LitterInterception[1] = v->grass.litt_intPPT_b;
	p_LitterInterception[2] = v->grass.litt_intPPT_c;
	p_LitterInterception[3] = v->grass.litt_intPPT_d;
	p_LitterInterception[4] = v->shrub.litt_intPPT_a;
	p_LitterInterception[5] = v->shrub.litt_intPPT_b;
	p_LitterInterception[6] = v->shrub.litt_intPPT_c;
	p_LitterInterception[7] = v->shrub.litt_intPPT_d;
	p_LitterInterception[8] = v->tree.litt_intPPT_a;
	p_LitterInterception[9] = v->tree.litt_intPPT_b;
	p_LitterInterception[10] = v->tree.litt_intPPT_c;
	p_LitterInterception[11] = v->tree.litt_intPPT_d;
	p_LitterInterception[12] = v->forb.litt_intPPT_a;
	p_LitterInterception[13] = v->forb.litt_intPPT_b;
	p_LitterInterception[14] = v->forb.litt_intPPT_c;
	p_LitterInterception[15] = v->forb.litt_intPPT_d;
	PROTECT(LitterInterception_names = allocVector(VECSXP, 2));
	PROTECT(LitterInterception_names_x = allocVector(STRSXP, 4));
	for (i = 0; i < 4; i++)
		SET_STRING_ELT(LitterInterception_names_x, i, mkChar(cLitterInterception_x[i]));
	SET_VECTOR_ELT(LitterInterception_names, 0, LitterInterception_names_x);
	SET_VECTOR_ELT(LitterInterception_names, 1, col_names);
	setAttrib(LitterInterception, R_DimNamesSymbol, LitterInterception_names);

	PROTECT(EsTpartitioning_param = allocVector(REALSXP, 4));
	REAL(EsTpartitioning_param)[0] = v->grass.EsTpartitioning_param; //Grass
	REAL(EsTpartitioning_param)[1] = v->shrub.EsTpartitioning_param; //Shrub
	REAL(EsTpartitioning_param)[2] = v->tree.EsTpartitioning_param; //Tree
	REAL(EsTpartitioning_param)[3] = v->forb.EsTpartitioning_param; //forb
	setAttrib(EsTpartitioning_param, R_NamesSymbol, col_names);

	PROTECT(Es_param_limit = allocVector(REALSXP, 4));
	REAL(Es_param_limit)[0] = v->grass.Es_param_limit; //Grass
	REAL(Es_param_limit)[1] = v->shrub.Es_param_limit; //Shrub
	REAL(Es_param_limit)[2] = v->tree.Es_param_limit; //Tree
	REAL(Es_param_limit)[3] = v->forb.Es_param_limit; //forb
	setAttrib(Es_param_limit, R_NamesSymbol, col_names);

	PROTECT(Shade = allocMatrix(REALSXP, 6, 4));
	p_Shade = REAL(Shade);
	p_Shade[0] = v->grass.shade_scale;
	p_Shade[1] = v->grass.shade_deadmax;
	p_Shade[2] = v->grass.tr_shade_effects.xinflec;
	p_Shade[3] = v->grass.tr_shade_effects.yinflec;
	p_Shade[4] = v->grass.tr_shade_effects.range;
	p_Shade[5] = v->grass.tr_shade_effects.slope;
	p_Shade[6] = v->shrub.shade_scale;
	p_Shade[7] = v->shrub.shade_deadmax;
	p_Shade[8] = v->shrub.tr_shade_effects.xinflec;
	p_Shade[9] = v->shrub.tr_shade_effects.yinflec;
	p_Shade[10] = v->shrub.tr_shade_effects.range;
	p_Shade[11] = v->shrub.tr_shade_effects.slope;
	p_Shade[12] = v->tree.shade_scale;
	p_Shade[13] = v->tree.shade_deadmax;
	p_Shade[14] = v->tree.tr_shade_effects.xinflec;
	p_Shade[15] = v->tree.tr_shade_effects.yinflec;
	p_Shade[16] = v->tree.tr_shade_effects.range;
	p_Shade[17] = v->tree.tr_shade_effects.slope;
	p_Shade[18] = v->forb.shade_scale;
	p_Shade[19] = v->forb.shade_deadmax;
	p_Shade[20] = v->forb.tr_shade_effects.xinflec;
	p_Shade[21] = v->forb.tr_shade_effects.yinflec;
	p_Shade[22] = v->forb.tr_shade_effects.range;
	p_Shade[23] = v->forb.tr_shade_effects.slope;
	PROTECT(Shade_names = allocVector(VECSXP, 2));
	PROTECT(Shade_names_x = allocVector(STRSXP, 6));
	for (i = 0; i < 6; i++)
		SET_STRING_ELT(Shade_names_x, i, mkChar(cShade_names_x[i]));
	SET_VECTOR_ELT(Shade_names, 0, Shade_names_x);
	SET_VECTOR_ELT(Shade_names, 1, col_names);
	setAttrib(Shade, R_DimNamesSymbol, Shade_names);

	PROTECT(Hydraulic_flag = allocVector(LGLSXP, 4));
	LOGICAL_POINTER(Hydraulic_flag)[0] = v->grass.flagHydraulicRedistribution; //Grass
	LOGICAL_POINTER(Hydraulic_flag)[1] = v->shrub.flagHydraulicRedistribution; //Shrub
	LOGICAL_POINTER(Hydraulic_flag)[2] = v->tree.flagHydraulicRedistribution; //Tree
	LOGICAL_POINTER(Hydraulic_flag)[3] = v->forb.flagHydraulicRedistribution; //forb
	setAttrib(Hydraulic_flag, R_NamesSymbol, col_names);

	PROTECT(Hydraulic = allocMatrix(REALSXP, 3, 4));
	p_Hydraulic = REAL(Hydraulic);
	p_Hydraulic[0] = v->grass.maxCondroot;
	p_Hydraulic[1] = v->grass.swpMatric50;
	p_Hydraulic[2] = v->grass.shapeCond;
	p_Hydraulic[3] = v->shrub.maxCondroot;
	p_Hydraulic[4] = v->shrub.swpMatric50;
	p_Hydraulic[5] = v->shrub.shapeCond;
	p_Hydraulic[6] = v->tree.maxCondroot;
	p_Hydraulic[7] = v->tree.swpMatric50;
	p_Hydraulic[8] = v->tree.shapeCond;
	p_Hydraulic[9] = v->forb.maxCondroot;
	p_Hydraulic[10] = v->forb.swpMatric50;
	p_Hydraulic[11] = v->forb.shapeCond;
	PROTECT(Hydraulic_names = allocVector(VECSXP, 2));
	PROTECT(Hydraulic_names_x = allocVector(STRSXP, 3));
	for (i = 0; i < 3; i++) {
		SET_STRING_ELT(Hydraulic_names_x, i, mkChar(cHydraulic_names[i]));
	}
	SET_VECTOR_ELT(Hydraulic_names, 0, Hydraulic_names_x);
	SET_VECTOR_ELT(Hydraulic_names, 1, col_names);
	setAttrib(Hydraulic, R_DimNamesSymbol, Hydraulic_names);

	PROTECT(CSWP = allocVector(REALSXP, 4));
	REAL(CSWP)[0] = v->grass.SWPcrit / -10; //Grass
	REAL(CSWP)[1] = v->shrub.SWPcrit / -10; //Shrub
	REAL(CSWP)[2] = v->tree.SWPcrit / -10; //Tree
	REAL(CSWP)[3] = v->forb.SWPcrit / -10; //Forb
	setAttrib(CSWP, R_NamesSymbol, col_names);

	PROTECT(MonthlyProductionValues_Column_names = allocVector(STRSXP, 4));
	for (i = 0; i < 4; i++)
		SET_STRING_ELT(MonthlyProductionValues_Column_names, i, mkChar(cMonthlyProductionValues_Column_names[i]));
	PROTECT(MonthlyProductionValues_Row_names = allocVector(STRSXP, 12));
	for (i = 0; i < 12; i++)
		SET_STRING_ELT(MonthlyProductionValues_Row_names, i, mkChar(cMonths[i]));

	PROTECT(Grasslands = allocMatrix(REALSXP, 12, 4));
	p_Grasslands = REAL(Grasslands);
	for (i = 0; i < 12; i++) {
		p_Grasslands[i + 12 * 0] = v->grass.litter[i];
		p_Grasslands[i + 12 * 1] = v->grass.biomass[i];
		p_Grasslands[i + 12 * 2] = v->grass.pct_live[i];
		p_Grasslands[i + 12 * 3] = v->grass.lai_conv[i];
	}
	PROTECT(Grasslands_names = allocVector(VECSXP, 2));
	SET_VECTOR_ELT(Grasslands_names, 0, MonthlyProductionValues_Row_names);
	SET_VECTOR_ELT(Grasslands_names, 1, MonthlyProductionValues_Column_names);
	setAttrib(Grasslands, R_DimNamesSymbol, Grasslands_names);

	PROTECT(Shrublands = allocMatrix(REALSXP, 12, 4));
	p_Shrublands = REAL(Shrublands);
	for (i = 0; i < 12; i++) {
		p_Shrublands[i + 12 * 0] = v->shrub.litter[i];
		p_Shrublands[i + 12 * 1] = v->shrub.biomass[i];
		p_Shrublands[i + 12 * 2] = v->shrub.pct_live[i];
		p_Shrublands[i + 12 * 3] = v->shrub.lai_conv[i];
	}
	PROTECT(Shrublands_names = allocVector(VECSXP, 2));
	SET_VECTOR_ELT(Shrublands_names, 0, MonthlyProductionValues_Row_names);
	SET_VECTOR_ELT(Shrublands_names, 1, MonthlyProductionValues_Column_names);
	setAttrib(Shrublands, R_DimNamesSymbol, Shrublands_names);

	PROTECT(Forest = allocMatrix(REALSXP, 12, 4));
	p_Forest = REAL(Forest);
	for (i = 0; i < 12; i++) {
		p_Forest[i + 12 * 0] = v->tree.litter[i];
		p_Forest[i + 12 * 1] = v->tree.biomass[i];
		p_Forest[i + 12 * 2] = v->tree.pct_live[i];
		p_Forest[i + 12 * 3] = v->tree.lai_conv[i];
	}
	PROTECT(Forest_names = allocVector(VECSXP, 2));
	SET_VECTOR_ELT(Forest_names, 0, MonthlyProductionValues_Row_names);
	SET_VECTOR_ELT(Forest_names, 1, MonthlyProductionValues_Column_names);
	setAttrib(Forest, R_DimNamesSymbol, Forest_names);

	PROTECT(Forb = allocMatrix(REALSXP, 12, 4));
	p_Forb = REAL(Forb);
	for (i = 0; i < 12; i++) {
		p_Forb[i + 12 * 0] = v->forb.litter[i];
		p_Forb[i + 12 * 1] = v->forb.biomass[i];
		p_Forb[i + 12 * 2] = v->forb.pct_live[i];
		p_Forb[i + 12 * 3] = v->forb.lai_conv[i];
	}
	PROTECT(Forb_names = allocVector(VECSXP, 2));
	SET_VECTOR_ELT(Forb_names, 0, MonthlyProductionValues_Row_names);
	SET_VECTOR_ELT(Forb_names, 1, MonthlyProductionValues_Column_names);
	setAttrib(Forb, R_DimNamesSymbol, Forb_names);

	SET_SLOT(VegProd, install(cVegProd_names[0]), VegComp);
	SET_SLOT(VegProd, install(cVegProd_names[1]), Albedo);
	SET_SLOT(VegProd, install(cVegProd_names[2]), conv_stcr);
	SET_SLOT(VegProd, install(cVegProd_names[3]), Canopy);
	SET_SLOT(VegProd, install(cVegProd_names[4]), VegInterception);
	SET_SLOT(VegProd, install(cVegProd_names[5]), LitterInterception);
	SET_SLOT(VegProd, install(cVegProd_names[6]), EsTpartitioning_param);
	SET_SLOT(VegProd, install(cVegProd_names[7]), Es_param_limit);
	SET_SLOT(VegProd, install(cVegProd_names[8]), Shade);
	SET_SLOT(VegProd, install(cVegProd_names[9]), Hydraulic_flag);
	SET_SLOT(VegProd, install(cVegProd_names[10]), Hydraulic);
	SET_SLOT(VegProd, install(cVegProd_names[11]), CSWP);
	SET_SLOT(VegProd, install(cVegProd_names[12]), Grasslands);
	SET_SLOT(VegProd, install(cVegProd_names[13]), Shrublands);
	SET_SLOT(VegProd, install(cVegProd_names[14]), Forest);
	SET_SLOT(VegProd, install(cVegProd_names[15]), Forb);
	SET_SLOT(VegProd, install(cVegProd_names[16]), CO2Coefficients);

	UNPROTECT(40);
	return VegProd;
}

void onSet_SW_VPD(SEXP SW_VPD) {
	int i;
	SW_VEGPROD *v = &SW_VegProd;

	char *cVegProd_names[] = { "Composition", "Albedo", "Cover_stcr", "CanopyHeight", "VegetationInterceptionParameters", "LitterInterceptionParameters",
		"EsTpartitioning_param", "Es_param_limit", "Shade", "HydraulicRedistribution_use", "HydraulicRedistribution", "CriticalSoilWaterPotential",
		"MonthlyProductionValues_grass", "MonthlyProductionValues_shrub", "MonthlyProductionValues_tree", "MonthlyProductionValues_forb", "CO2Coefficients" };
	SEXP VegComp;
	SEXP Albedo;
	SEXP conv_stcr;
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
	SEXP Grasslands;
	SEXP Shrublands;
	SEXP Forest;
	SEXP Forb;
	SEXP CO2Coefficients;
	RealD *p_Grasslands, *p_Shrublands, *p_Forest, *p_Forb;
	RealD fraction_sum = 0.;

	MyFileName = SW_F_name(eVegProd);

	PROTECT(VegComp = GET_SLOT(SW_VPD, install(cVegProd_names[0])));
	v->grass.cov.fCover = REAL(VegComp)[0]; //Grass
	v->shrub.cov.fCover = REAL(VegComp)[1]; //Shrub
	v->tree.cov.fCover = REAL(VegComp)[2]; //Tree
	v->forb.cov.fCover = REAL(VegComp)[3]; //Forb
	v->bare_cov.fCover = REAL(VegComp)[4]; //Bare Ground

	PROTECT(Albedo = GET_SLOT(SW_VPD, install(cVegProd_names[1])));
	v->grass.cov.albedo = REAL(Albedo)[0]; //Grass
	v->shrub.cov.albedo = REAL(Albedo)[1]; //Shrub
	v->tree.cov.albedo = REAL(Albedo)[2]; //Tree
	v->forb.cov.albedo = REAL(Albedo)[3]; //Forb
	v->bare_cov.albedo = REAL(Albedo)[4]; //Bare Ground

	PROTECT(conv_stcr = GET_SLOT(SW_VPD, install(cVegProd_names[2])));
	v->grass.conv_stcr = REAL(conv_stcr)[0]; //Grass
	v->shrub.conv_stcr = REAL(conv_stcr)[1]; //Shrub
	v->tree.conv_stcr = REAL(conv_stcr)[2]; //Tree
	v->forb.conv_stcr = REAL(conv_stcr)[3]; //Forb

	PROTECT(Canopy = GET_SLOT(SW_VPD, install(cVegProd_names[3])));
	p_Canopy = REAL(Canopy);
	v->grass.cnpy.xinflec = p_Canopy[0];
	v->grass.cnpy.yinflec = p_Canopy[1];
	v->grass.cnpy.range = p_Canopy[2];
	v->grass.cnpy.slope = p_Canopy[3];
	v->grass.canopy_height_constant = p_Canopy[4];
	v->shrub.cnpy.xinflec = p_Canopy[5];
	v->shrub.cnpy.yinflec = p_Canopy[6];
	v->shrub.cnpy.range = p_Canopy[7];
	v->shrub.cnpy.slope = p_Canopy[8];
	v->shrub.canopy_height_constant = p_Canopy[9];
	v->tree.cnpy.xinflec = p_Canopy[10];
	v->tree.cnpy.yinflec = p_Canopy[11];
	v->tree.cnpy.range = p_Canopy[12];
	v->tree.cnpy.slope = p_Canopy[13];
	v->tree.canopy_height_constant = p_Canopy[14];
	v->forb.cnpy.xinflec = p_Canopy[15];
	v->forb.cnpy.yinflec = p_Canopy[16];
	v->forb.cnpy.range = p_Canopy[17];
	v->forb.cnpy.slope = p_Canopy[18];
	v->forb.canopy_height_constant = p_Canopy[19];

	PROTECT(VegInterception = GET_SLOT(SW_VPD, install(cVegProd_names[4])));
	p_VegInterception = REAL(VegInterception);
	v->grass.veg_intPPT_a = p_VegInterception[0];
	v->grass.veg_intPPT_b = p_VegInterception[1];
	v->grass.veg_intPPT_c = p_VegInterception[2];
	v->grass.veg_intPPT_d = p_VegInterception[3];
	v->shrub.veg_intPPT_a = p_VegInterception[4];
	v->shrub.veg_intPPT_b = p_VegInterception[5];
	v->shrub.veg_intPPT_c = p_VegInterception[6];
	v->shrub.veg_intPPT_d = p_VegInterception[7];
	v->tree.veg_intPPT_a = p_VegInterception[8];
	v->tree.veg_intPPT_b = p_VegInterception[9];
	v->tree.veg_intPPT_c = p_VegInterception[10];
	v->tree.veg_intPPT_d = p_VegInterception[11];
	v->forb.veg_intPPT_a = p_VegInterception[12];
	v->forb.veg_intPPT_b = p_VegInterception[13];
	v->forb.veg_intPPT_c = p_VegInterception[14];
	v->forb.veg_intPPT_d = p_VegInterception[15];

	PROTECT(LitterInterception = GET_SLOT(SW_VPD, install(cVegProd_names[5])));
	p_LitterInterception = REAL(LitterInterception);
	v->grass.litt_intPPT_a = p_LitterInterception[0];
	v->grass.litt_intPPT_b = p_LitterInterception[1];
	v->grass.litt_intPPT_c = p_LitterInterception[2];
	v->grass.litt_intPPT_d = p_LitterInterception[3];
	v->shrub.litt_intPPT_a = p_LitterInterception[4];
	v->shrub.litt_intPPT_b = p_LitterInterception[5];
	v->shrub.litt_intPPT_c = p_LitterInterception[6];
	v->shrub.litt_intPPT_d = p_LitterInterception[7];
	v->tree.litt_intPPT_a = p_LitterInterception[8];
	v->tree.litt_intPPT_b = p_LitterInterception[9];
	v->tree.litt_intPPT_c = p_LitterInterception[10];
	v->tree.litt_intPPT_d = p_LitterInterception[11];
	v->forb.litt_intPPT_a = p_LitterInterception[12];
	v->forb.litt_intPPT_b = p_LitterInterception[13];
	v->forb.litt_intPPT_c = p_LitterInterception[14];
	v->forb.litt_intPPT_d = p_LitterInterception[15];

	PROTECT(EsTpartitioning_param = GET_SLOT(SW_VPD, install(cVegProd_names[6])));
	v->grass.EsTpartitioning_param = REAL(EsTpartitioning_param)[0]; //Grass
	v->shrub.EsTpartitioning_param = REAL(EsTpartitioning_param)[1]; //Shrub
	v->tree.EsTpartitioning_param = REAL(EsTpartitioning_param)[2]; //Tree
	v->forb.EsTpartitioning_param = REAL(EsTpartitioning_param)[3]; //Forb

	PROTECT(Es_param_limit = GET_SLOT(SW_VPD, install(cVegProd_names[7])));
	v->grass.Es_param_limit = REAL(Es_param_limit)[0]; //Grass
	v->shrub.Es_param_limit = REAL(Es_param_limit)[1]; //Shrub
	v->tree.Es_param_limit = REAL(Es_param_limit)[2]; //Tree
	v->forb.Es_param_limit = REAL(Es_param_limit)[3]; //Forb

	PROTECT(Shade = GET_SLOT(SW_VPD, install(cVegProd_names[8])));
	p_Shade = REAL(Shade);
	v->grass.shade_scale = p_Shade[0];
	v->grass.shade_deadmax = p_Shade[1];
	v->grass.tr_shade_effects.xinflec = p_Shade[2];
	v->grass.tr_shade_effects.yinflec = p_Shade[3];
	v->grass.tr_shade_effects.range = p_Shade[4];
	v->grass.tr_shade_effects.slope = p_Shade[5];
	v->shrub.shade_scale = p_Shade[6];
	v->shrub.shade_deadmax = p_Shade[7];
	v->shrub.tr_shade_effects.xinflec = p_Shade[8];
	v->shrub.tr_shade_effects.yinflec = p_Shade[9];
	v->shrub.tr_shade_effects.range = p_Shade[10];
	v->shrub.tr_shade_effects.slope = p_Shade[11];
	v->tree.shade_scale = p_Shade[12];
	v->tree.shade_deadmax = p_Shade[13];
	v->tree.tr_shade_effects.xinflec = p_Shade[14];
	v->tree.tr_shade_effects.yinflec = p_Shade[15];
	v->tree.tr_shade_effects.range = p_Shade[16];
	v->tree.tr_shade_effects.slope = p_Shade[17];
	v->forb.shade_scale = p_Shade[18];
	v->forb.shade_deadmax = p_Shade[19];
	v->forb.tr_shade_effects.xinflec = p_Shade[20];
	v->forb.tr_shade_effects.yinflec = p_Shade[21];
	v->forb.tr_shade_effects.range = p_Shade[22];
	v->forb.tr_shade_effects.slope = p_Shade[23];

	PROTECT(Hydraulic_flag = GET_SLOT(SW_VPD, install(cVegProd_names[9])));
	PROTECT(Hydraulic = GET_SLOT(SW_VPD, install(cVegProd_names[10])));
	v->grass.flagHydraulicRedistribution = LOGICAL_POINTER(Hydraulic_flag)[0]; //Grass
	v->shrub.flagHydraulicRedistribution = LOGICAL_POINTER(Hydraulic_flag)[1]; //Shrub
	v->tree.flagHydraulicRedistribution = LOGICAL_POINTER(Hydraulic_flag)[2]; //Tree
	v->forb.flagHydraulicRedistribution = LOGICAL_POINTER(Hydraulic_flag)[3]; //Forb
	v->grass.maxCondroot = REAL(Hydraulic)[0]; //Grass
	v->grass.swpMatric50 = REAL(Hydraulic)[1]; //Grass
	v->grass.shapeCond = REAL(Hydraulic)[2]; //Grass
	v->shrub.maxCondroot = REAL(Hydraulic)[3]; //Shrub
	v->shrub.swpMatric50 = REAL(Hydraulic)[4]; //Shrub
	v->shrub.shapeCond = REAL(Hydraulic)[5]; //Shrub
	v->tree.maxCondroot = REAL(Hydraulic)[6]; //Tree
	v->tree.swpMatric50 = REAL(Hydraulic)[7]; //Tree
	v->tree.shapeCond = REAL(Hydraulic)[8]; //Tree
	v->forb.maxCondroot = REAL(Hydraulic)[9]; //Forb
	v->forb.swpMatric50 = REAL(Hydraulic)[10]; //Forb
	v->forb.shapeCond = REAL(Hydraulic)[11]; //Forb

	PROTECT(CSWP = GET_SLOT(SW_VPD, install(cVegProd_names[11])));
	v->grass.SWPcrit = -10 * REAL(CSWP)[0]; //Grass
	v->shrub.SWPcrit = -10 * REAL(CSWP)[1]; //Shrub
	v->tree.SWPcrit = -10 * REAL(CSWP)[2]; //Tree
	v->forb.SWPcrit = -10 * REAL(CSWP)[3]; //Forb

 	//PROTECT(MonthlyProductionValues = VECTOR_ELT(SW_VPD, 11));
	PROTECT(Grasslands = GET_SLOT(SW_VPD, install(cVegProd_names[12])));
	p_Grasslands = REAL(Grasslands);
	for (i = 0; i < 12; i++) {
		v->grass.litter[i] = p_Grasslands[i + 12 * 0];
		v->grass.biomass[i] = p_Grasslands[i + 12 * 1];
		v->grass.pct_live[i] = p_Grasslands[i + 12 * 2];
		v->grass.lai_conv[i] = p_Grasslands[i + 12 * 3];
	}
	PROTECT(Shrublands = GET_SLOT(SW_VPD, install(cVegProd_names[13])));
	p_Shrublands = REAL(Shrublands);
	for (i = 0; i < 12; i++) {
		v->shrub.litter[i] = p_Shrublands[i + 12 * 0];
		v->shrub.biomass[i] = p_Shrublands[i + 12 * 1];
		v->shrub.pct_live[i] = p_Shrublands[i + 12 * 2];
		v->shrub.lai_conv[i] = p_Shrublands[i + 12 * 3];
	}
	PROTECT(Forest = GET_SLOT(SW_VPD, install(cVegProd_names[14])));
	p_Forest = REAL(Forest);
	for (i = 0; i < 12; i++) {
		v->tree.litter[i] = p_Forest[i + 12 * 0];
		v->tree.biomass[i] = p_Forest[i + 12 * 1];
		v->tree.pct_live[i] = p_Forest[i + 12 * 2];
		v->tree.lai_conv[i] = p_Forest[i + 12 * 3];
	}
	PROTECT(Forb = GET_SLOT(SW_VPD, install(cVegProd_names[15])));
	p_Forb = REAL(Forb);
	for (i = 0; i < 12; i++) {
		v->forb.litter[i] = p_Forb[i + 12 * 0];
		v->forb.biomass[i] = p_Forb[i + 12 * 1];
		v->forb.pct_live[i] = p_Forb[i + 12 * 2];
		v->forb.lai_conv[i] = p_Forb[i + 12 * 3];
	}

	PROTECT(CO2Coefficients = GET_SLOT(SW_VPD, install(cVegProd_names[16])));
	v->grass.co2_bio_coeff1 = REAL(CO2Coefficients)[0];
	v->shrub.co2_bio_coeff1 = REAL(CO2Coefficients)[1];
	v->tree.co2_bio_coeff1 = REAL(CO2Coefficients)[2];
	v->forb.co2_bio_coeff1 = REAL(CO2Coefficients)[3];
	v->grass.co2_bio_coeff2 = REAL(CO2Coefficients)[4];
	v->shrub.co2_bio_coeff2 = REAL(CO2Coefficients)[5];
	v->tree.co2_bio_coeff2 = REAL(CO2Coefficients)[6];
	v->forb.co2_bio_coeff2 = REAL(CO2Coefficients)[7];
	v->grass.co2_wue_coeff1 = REAL(CO2Coefficients)[8];
	v->shrub.co2_wue_coeff1 = REAL(CO2Coefficients)[9];
	v->tree.co2_wue_coeff1 = REAL(CO2Coefficients)[10];
	v->forb.co2_wue_coeff1 = REAL(CO2Coefficients)[11];
	v->grass.co2_wue_coeff2 = REAL(CO2Coefficients)[12];
	v->shrub.co2_wue_coeff2 = REAL(CO2Coefficients)[13];
	v->tree.co2_wue_coeff2 = REAL(CO2Coefficients)[14];
	v->forb.co2_wue_coeff2 = REAL(CO2Coefficients)[15];


	fraction_sum = v->grass.cov.fCover + v->shrub.cov.fCover + v->tree.cov.fCover + v->forb.cov.fCover + v->bare_cov.fCover;
	if (!EQ(fraction_sum, 1.0)) {
		LogError(logfp, LOGWARN, "%s : Fractions of vegetation components were normalized, "
			"sum of fractions (%5.4f) != 1.0.\nNew coefficients are:", MyFileName, fraction_sum);
		v->grass.cov.fCover /= fraction_sum;
		v->shrub.cov.fCover /= fraction_sum;
		v->tree.cov.fCover /= fraction_sum;
		v->bare_cov.fCover /= fraction_sum;
		v->forb.cov.fCover /= fraction_sum;
		LogError(logfp, LOGWARN, "  Grassland fraction : %5.4f", v->grass.cov.fCover);
		LogError(logfp, LOGWARN, "  Shrubland fraction : %5.4f", v->shrub.cov.fCover);
		LogError(logfp, LOGWARN, "  Forest/tree fraction : %5.4f", v->tree.cov.fCover);
		LogError(logfp, LOGWARN, "  FORB fraction : %5.4f", v->forb.cov.fCover);
		LogError(logfp, LOGWARN, "  Bare Ground fraction : %5.4f", v->bare_cov.fCover);
	}

	SW_VPD_init();

	if (EchoInits)
		_echo_VegProd();

	UNPROTECT(17);
}
