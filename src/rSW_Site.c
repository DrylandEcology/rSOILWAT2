/********************************************************/
/********************************************************/
/*	Application: SOILWAT - soilwater dynamics simulator
 Source file: Site.c
 Type: module
 Purpose: Read / write and otherwise manage the
 site specific information.  See also the
 Layer module.
 */
/********************************************************/
/********************************************************/

/* =================================================== */
/*                INCLUDES / DEFINES                   */
/* --------------------------------------------------- */

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
#include "SOILWAT2/include/SW_SoilWater.h"

#include "SOILWAT2/include/SW_Site.h"
#include "rSW_Site.h"
#include "SW_R_lib.h"

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>



/* =================================================== */
/*                  Local Variables                    */
/* --------------------------------------------------- */

static char *MyFileName;


static char *cSW_SIT[] = {
  "SWClimits", "ModelFlags", "ModelCoefficients",
  "SnowSimulationParameters", "DrainageCoefficient", "EvaporationCoefficients",
  "TranspirationCoefficients", "IntrinsicSiteParams", "SoilTemperatureFlag",
  "SoilTemperatureConstants",
  "SoilDensityInputType",
  "TranspirationRegions",
  "swrc_flags", "has_swrcp"
};

static char *cLayers[] = {
  "depth_cm", "bulkDensity_g/cm^3", "gravel_content",
  "EvapBareSoil_frac", "transpGrass_frac", "transpShrub_frac", "transpTree_frac",
  "transpForb_frac", "sand_frac", "clay_frac", "impermeability_frac", "soilTemp_c"
};

static char *cSWRCp[] = {
  "Param1", "Param2", "Param3", "Param4", "Param5", "Param6"
};


/* =================================================== */
/*             Local Function Definitions              */
/* --------------------------------------------------- */

/* Copy soil properties into "Layers" matrix */
static SEXP onGet_SW_LYR(void) {
	int i, dmax = 0;
	SW_SITE *v = &SoilWatRun.Site;
	SEXP Layers, Layers_names, Layers_names_y;
	RealD *p_Layers;

	PROTECT(Layers = allocMatrix(REALSXP, v->n_layers, 12));
	p_Layers = REAL(Layers);
	for (i = 0; i < (v->n_layers); i++) {
		p_Layers[i + (v->n_layers) * 0] = dmax = v->width[i] + dmax;
		p_Layers[i + (v->n_layers) * 1] = v->soilDensityInput[i];
		p_Layers[i + (v->n_layers) * 2] = v->fractionVolBulk_gravel[i];
		p_Layers[i + (v->n_layers) * 3] = v->evap_coeff[i];
		p_Layers[i + (v->n_layers) * 4] = v->transp_coeff[SW_GRASS][i];
		p_Layers[i + (v->n_layers) * 5] = v->transp_coeff[SW_SHRUB][i];
		p_Layers[i + (v->n_layers) * 6] = v->transp_coeff[SW_TREES][i];
		p_Layers[i + (v->n_layers) * 7] = v->transp_coeff[SW_FORBS][i];
		p_Layers[i + (v->n_layers) * 8] = v->fractionWeightMatric_sand[i];
		p_Layers[i + (v->n_layers) * 9] = v->fractionWeightMatric_clay[i];
		p_Layers[i + (v->n_layers) * 10] = v->impermeability[i];
		p_Layers[i + (v->n_layers) * 11] = v->avgLyrTempInit[i];
	}

	PROTECT(Layers_names = allocVector(VECSXP, 2));
	PROTECT(Layers_names_y = allocVector(STRSXP, 12));
	for (i = 0; i < 12; i++) {
		SET_STRING_ELT(Layers_names_y, i, mkChar(cLayers[i]));
	}
	SET_VECTOR_ELT(Layers_names, 1, Layers_names_y);
	setAttrib(Layers, R_DimNamesSymbol, Layers_names);

	UNPROTECT(3);
	return Layers;
}

/* Function `onSet_SW_LYR()` corresponds to SOILWAT2's `SW_LYR_read()`,
   previously named `_read_layers()`
*/
static void onSet_SW_LYR(SEXP SW_LYR, LOG_INFO* LogInfo) {

	SW_SITE *v = &SoilWatRun.Site;
	LyrIndex lyrno;
	int i, j, k, columns;
	RealF dmin = 0.0, dmax, evco, trco_veg[NVEGTYPES], psand, pclay, soildensity, imperm, soiltemp, f_gravel;
	RealD *p_Layers;

	/* note that Files.read() must be called prior to this. */
	MyFileName = SoilWatDomain.PathInfo.InFiles[eLayers];

	j = nrows(SW_LYR);
	p_Layers = REAL(SW_LYR);
	columns = ncols(SW_LYR);

	/* Check that we have 12 values per layer */
	/* Adjust number if new variables are added */
	if (columns != 12) {
		LogError(
			LogInfo,
			LOGERROR,
			"%s : Too few columns in layers specified (%d).\n",
			MyFileName, columns
		);
        return; // Exit function prematurely due to error
	}

	for (i = 0; i < j; i++) {
		lyrno = SoilWatRun.Site.n_layers++;

		dmax = p_Layers[i + j * 0];
		soildensity = p_Layers[i + j * 1];
		f_gravel = p_Layers[i + j * 2];
		evco = p_Layers[i + j * 3];
		trco_veg[SW_GRASS] = p_Layers[i + j * 4];
		trco_veg[SW_SHRUB] = p_Layers[i + j * 5];
		trco_veg[SW_TREES] = p_Layers[i + j * 6];
		trco_veg[SW_FORBS] = p_Layers[i + j * 7];
		psand = p_Layers[i + j * 8];
		pclay = p_Layers[i + j * 9];
		imperm = p_Layers[i + j * 10];
		soiltemp = p_Layers[i + j * 11];

		v->width[lyrno] = dmax - dmin;
		dmin = dmax;
		v->soilDensityInput[lyrno] = soildensity;
		v->fractionVolBulk_gravel[lyrno] = f_gravel;
		v->evap_coeff[lyrno] = evco;
		ForEachVegType(k) {
			v->transp_coeff[k][lyrno] = trco_veg[k];
		}
		v->fractionWeightMatric_sand[lyrno] = psand;
		v->fractionWeightMatric_clay[lyrno] = pclay;
		v->impermeability[lyrno] = imperm;
		v->avgLyrTempInit[lyrno] = soiltemp;

		if (lyrno >= MAX_LAYERS) {
			LogError(
				LogInfo,
				LOGERROR,
				"%s : Too many layers specified (%d).\n"
				"Maximum number of layers is %d\n",
				MyFileName, lyrno + 1, MAX_LAYERS
			);
            return; // Exit function prematurely due to error
		}
	}
}


/* Copy SWRC parameters into "SWRCp" matrix */
static SEXP onGet_SW_SWRCp(void) {
	int i, k;
	SW_SITE *v = &SoilWatRun.Site;
	SEXP SWRCp, SWRCp_names, SWRCp_names_y;
	RealD *p_SWRCp;

	PROTECT(SWRCp = allocMatrix(REALSXP, v->n_layers, SWRC_PARAM_NMAX));
	p_SWRCp = REAL(SWRCp);
	for (i = 0; i < (v->n_layers); i++) {
		for (k = 0; k < SWRC_PARAM_NMAX; k++) {
			p_SWRCp[i + (v->n_layers) * k] = v->swrcp[i][k];
		}
	}

	PROTECT(SWRCp_names = allocVector(VECSXP, 2));
	PROTECT(SWRCp_names_y = allocVector(STRSXP, SWRC_PARAM_NMAX));
	for (i = 0; i < SWRC_PARAM_NMAX; i++) {
		SET_STRING_ELT(SWRCp_names_y, i, mkChar(cSWRCp[i]));
	}
	SET_VECTOR_ELT(SWRCp_names, 1, SWRCp_names_y);
	setAttrib(SWRCp, R_DimNamesSymbol, SWRCp_names);

	UNPROTECT(3);
	return SWRCp;
}

/* Function `onSet_SW_SWRCp()` corresponds to SOILWAT2's `SW_SWRC_read()` */
static void onSet_SW_SWRCp(SEXP SW_SWRCp, LOG_INFO* LogInfo) {

	SW_SITE *v = &SoilWatRun.Site;
	int i, k;
	RealD *p_SWRCp;

	/* note that Files.read() must be called prior to this. */
	MyFileName = SoilWatDomain.PathInfo.InFiles[eSWRCp];

	/* Check that we have n = `SWRC_PARAM_NMAX` values per layer */
	if (ncols(SW_SWRCp) != SWRC_PARAM_NMAX) {
		LogError(
			LogInfo,
			LOGERROR,
			"%s : Bad number of SWRC parameters %d -- must be %d.\n",
			MyFileName, ncols(SW_SWRCp), SWRC_PARAM_NMAX
		);
        return; // Exit function prematurely due to error
	}

	/* Check that we have `SW_Site.n_layers` */
	if (nrows(SW_SWRCp) != SoilWatRun.Site.n_layers) {
		LogError(
			LogInfo,
			LOGERROR,
			"%s : Number of layers with SWRC parameters (%d) "
			"must match number of soil layers (%d)\n",
			MyFileName, nrows(SW_SWRCp), SoilWatRun.Site.n_layers
		);
        return; // Exit function prematurely due to error
	}

	/* Copy values */
	p_SWRCp = REAL(SW_SWRCp);

	for (i = 0; i < (v->n_layers); i++) {
		for (k = 0; k < SWRC_PARAM_NMAX; k++) {
			v->swrcp[i][k] = p_SWRCp[i + (v->n_layers) * k];
		}
	}
}


/* =================================================== */
/*             Global Function Definitions             */
/* --------------------------------------------------- */

/* Copy SOILWAT2 soil properties and SWRC parameters into S4 class "swSoils" */
SEXP onGet_SW_SOILS(void) {
	SEXP swSoils, SW_SOILS;

	PROTECT(swSoils = MAKE_CLASS("swSoils"));
	PROTECT(SW_SOILS = NEW_OBJECT(swSoils));

	SET_SLOT(SW_SOILS, install("Layers"), onGet_SW_LYR());
	SET_SLOT(SW_SOILS, install("SWRCp"), onGet_SW_SWRCp());

	UNPROTECT(2);
	return SW_SOILS;
}

/* Copy S4 class "swSoils" into SOILWAT2 soil properties and SWRC parameters */
void onSet_SW_SOILS(SEXP SW_SOILS, LOG_INFO* LogInfo) {
	SEXP SW_LYR, SW_SWRCp;

	PROTECT(SW_LYR = GET_SLOT(SW_SOILS, install("Layers")));
	onSet_SW_LYR(SW_LYR, LogInfo);

	PROTECT(SW_SWRCp = GET_SLOT(SW_SOILS, install("SWRCp")));
	onSet_SW_SWRCp(SW_SWRCp, LogInfo);

	UNPROTECT(2);
}


SEXP onGet_SW_SIT(void) {
	int i;
	SW_SITE *v = &SoilWatRun.Site;
	SW_MODEL *m = &SoilWatRun.Model;

	SEXP swSite;
	SEXP SW_SIT;

	SEXP SWClimits, SWClimits_names;
	char *cSWClimits[] = { "swc_min", "swc_init", "swc_wet" };

	SEXP ModelFlags, ModelCoefficients, ModelFlags_names, ModelCoefficients_names;
	char *cModelFlags[] = { "Reset", "DeepDrain" };
	char *cModelCoefficients[] = {"PETmultiplier", "DailyRunoff", "DailyRunon" };

	SEXP SnowSimulationParameters, SnowSimulationParameters_names;
	char *cSnowSimulationParameters[] = { "TminAccu2", "TmaxCrit", "lambdaSnow", "RmeltMin", "RmeltMax" };

	SEXP DrainageCoefficient, DrainageCoefficient_names;

	SEXP EvaporationCoefficients, EvaporationCoefficients_names;
	char *cEvaporationCoefficients[] = { "RateShift", "RateSlope", "InflectionPoint", "Range" };

	SEXP TranspirationCoefficients, TranspirationCoefficients_names;
	char *cTranspirationCoefficients[] = { "RateShift", "RateShape", "InflectionPoint", "Range" };

	SEXP IntrinsicSiteParams, IntrinsicSiteParams_names;
	char *cIntrinsicSiteParams[] = { "Longitude", "Latitude", "Altitude", "Slope", "Aspect" };

	SEXP SoilTemperatureConstants_use, SoilTemperatureConstants, SoilTemperatureConstants_names;
	char *cSoilTempValues[] = {
		"BiomassLimiter_g/m^2",
		"T1constant_a", "T1constant_b", "T1constant_c",
		"cs_constant_SoilThermCondct", "cs_constant",
		"sh_constant_SpecificHeatCapacity",
		"ConstMeanAirTemp",
		"deltaX_Param", "MaxDepth"
	};

	SEXP swrc_flags, swrc_names;
	char *cSWRCflags[] = {"swrc_name", "ptf_name"};

	SEXP has_swrcp;

	SEXP SoilDensityInputType;

	SEXP TranspirationRegions, TranspirationRegions_names, TranspirationRegions_names_y;
	char *cTranspirationRegions[] = { "ndx", "layer" };
	int *p_transp; // ideally `LyrIndex` so that same type as `_TranspRgnBounds`, but R API INTEGER() is signed

	MyFileName = SoilWatDomain.PathInfo.InFiles[eSite];

	PROTECT(swSite = MAKE_CLASS("swSite"));
	PROTECT(SW_SIT = NEW_OBJECT(swSite));

	PROTECT(SWClimits = allocVector(REALSXP, 3));
	REAL(SWClimits)[0] = v->_SWCMinVal;
	REAL(SWClimits)[1] = v->_SWCInitVal;
	REAL(SWClimits)[2] = v->_SWCWetVal;
	PROTECT(SWClimits_names = allocVector(STRSXP, 3));
	for (i = 0; i < 3; i++)
		SET_STRING_ELT(SWClimits_names, i, mkChar(cSWClimits[i]));
	setAttrib(SWClimits, R_NamesSymbol, SWClimits_names);

	PROTECT(ModelFlags = NEW_LOGICAL(2));
	LOGICAL(ModelFlags)[0] = v->reset_yr;
	LOGICAL(ModelFlags)[1] = v->deepdrain;
	PROTECT(ModelFlags_names = allocVector(STRSXP,2));
	for(i=0;i<2;i++)
		SET_STRING_ELT(ModelFlags_names,i,mkChar(cModelFlags[i]));
	setAttrib(ModelFlags, R_NamesSymbol, ModelFlags_names);

	PROTECT(ModelCoefficients = NEW_NUMERIC(3));
	REAL(ModelCoefficients)[0] = v->pet_scale;
	REAL(ModelCoefficients)[1] = v->percentRunoff;
	REAL(ModelCoefficients)[2] = v->percentRunon;
	PROTECT(ModelCoefficients_names = allocVector(STRSXP, 3));
	for (i = 0; i < 3; i++)
		SET_STRING_ELT(ModelCoefficients_names, i, mkChar(cModelCoefficients[i]));
	setAttrib(ModelCoefficients, R_NamesSymbol, ModelCoefficients_names);

	PROTECT(SnowSimulationParameters = allocVector(REALSXP, 5));
	REAL(SnowSimulationParameters)[0] = v->TminAccu2;
	REAL(SnowSimulationParameters)[1] = v->TmaxCrit;
	REAL(SnowSimulationParameters)[2] = v->lambdasnow;
	REAL(SnowSimulationParameters)[3] = v->RmeltMin;
	REAL(SnowSimulationParameters)[4] = v->RmeltMax;
	PROTECT(SnowSimulationParameters_names = allocVector(STRSXP, 5));
	for (i = 0; i < 5; i++)
		SET_STRING_ELT(SnowSimulationParameters_names, i, mkChar(cSnowSimulationParameters[i]));
	setAttrib(SnowSimulationParameters, R_NamesSymbol, SnowSimulationParameters_names);

	PROTECT(DrainageCoefficient = NEW_NUMERIC(1));
	REAL(DrainageCoefficient)[0] = v->slow_drain_coeff;
	PROTECT(DrainageCoefficient_names = allocVector(STRSXP,1));
	SET_STRING_ELT(DrainageCoefficient_names, 0, mkChar("SlowDrainCoefficientPerYear_cm/dy"));
	setAttrib(DrainageCoefficient, R_NamesSymbol, DrainageCoefficient_names);

	PROTECT(EvaporationCoefficients = allocVector(REALSXP,4));
	REAL(EvaporationCoefficients)[0] = v->evap.xinflec;
	REAL(EvaporationCoefficients)[1] = v->evap.slope;
	REAL(EvaporationCoefficients)[2] = v->evap.yinflec;
	REAL(EvaporationCoefficients)[3] = v->evap.range;
	PROTECT(EvaporationCoefficients_names = allocVector(STRSXP,4));
	for (i = 0; i < 4; i++)
		SET_STRING_ELT(EvaporationCoefficients_names, i, mkChar(cEvaporationCoefficients[i]));
	setAttrib(EvaporationCoefficients, R_NamesSymbol, EvaporationCoefficients_names);

	PROTECT(TranspirationCoefficients = allocVector(REALSXP,4));
	REAL(TranspirationCoefficients)[0] = v->transp.xinflec;
	REAL(TranspirationCoefficients)[1] = v->transp.slope;
	REAL(TranspirationCoefficients)[2] = v->transp.yinflec;
	REAL(TranspirationCoefficients)[3] = v->transp.range;
	PROTECT(TranspirationCoefficients_names = allocVector(STRSXP,4));
	for (i = 0; i < 4; i++)
		SET_STRING_ELT(TranspirationCoefficients_names, i, mkChar(cTranspirationCoefficients[i]));
	setAttrib(TranspirationCoefficients, R_NamesSymbol, TranspirationCoefficients_names);

	// SOILWAT2 calculates internally in radians, but input/output are in arc-degrees
	PROTECT(IntrinsicSiteParams = allocVector(REALSXP, 5));
	REAL(IntrinsicSiteParams)[0] = m->longitude * rad_to_deg;
	REAL(IntrinsicSiteParams)[1] = m->latitude * rad_to_deg;
	REAL(IntrinsicSiteParams)[2] = m->elevation;
	REAL(IntrinsicSiteParams)[3] = m->slope * rad_to_deg;
	REAL(IntrinsicSiteParams)[4] = missing(m->aspect) ? SW_MISSING : m->aspect * rad_to_deg;
	PROTECT(IntrinsicSiteParams_names = allocVector(STRSXP, 5));
	for (i = 0; i < 5; i++)
		SET_STRING_ELT(IntrinsicSiteParams_names, i, mkChar(cIntrinsicSiteParams[i]));
	setAttrib(IntrinsicSiteParams, R_NamesSymbol, IntrinsicSiteParams_names);

	PROTECT(SoilTemperatureConstants_use = NEW_LOGICAL(1));
	LOGICAL(SoilTemperatureConstants_use)[0] = v->use_soil_temp;

	PROTECT(SoilTemperatureConstants = NEW_NUMERIC(10));
	REAL(SoilTemperatureConstants)[0] = v->bmLimiter;
	REAL(SoilTemperatureConstants)[1] = v->t1Param1;
	REAL(SoilTemperatureConstants)[2] = v->t1Param2;
	REAL(SoilTemperatureConstants)[3] = v->t1Param3;
	REAL(SoilTemperatureConstants)[4] = v->csParam1;
	REAL(SoilTemperatureConstants)[5] = v->csParam2;
	REAL(SoilTemperatureConstants)[6] = v->shParam;
	REAL(SoilTemperatureConstants)[7] = v->Tsoil_constant;
	REAL(SoilTemperatureConstants)[8] = v->stDeltaX;
	REAL(SoilTemperatureConstants)[9] = v->stMaxDepth;
	PROTECT(SoilTemperatureConstants_names = allocVector(STRSXP,10));
	for (i = 0; i < 10; i++)
		SET_STRING_ELT(SoilTemperatureConstants_names, i, mkChar(cSoilTempValues[i]));
	setAttrib(SoilTemperatureConstants, R_NamesSymbol, SoilTemperatureConstants_names);

	PROTECT(SoilDensityInputType = ScalarInteger(v->type_soilDensityInput));

	PROTECT(TranspirationRegions = allocMatrix(INTSXP,(v->n_transp_rgn),2));
	p_transp = INTEGER(TranspirationRegions);
	for (i = 0; i < (v->n_transp_rgn); i++) {
		p_transp[i + (v->n_transp_rgn) * 0] = (i + 1);
		p_transp[i + (v->n_transp_rgn) * 1] = (v->_TranspRgnBounds[i]+1);
	}
	PROTECT(TranspirationRegions_names = allocVector(VECSXP,2));
	PROTECT(TranspirationRegions_names_y = allocVector(STRSXP,2));
	for (i = 0; i < 2; i++)
		SET_STRING_ELT(TranspirationRegions_names_y, i, mkChar(cTranspirationRegions[i]));
	SET_VECTOR_ELT(TranspirationRegions_names, 1, TranspirationRegions_names_y);
	setAttrib(TranspirationRegions, R_DimNamesSymbol, TranspirationRegions_names);


	PROTECT(swrc_flags = NEW_CHARACTER(2));
	SET_STRING_ELT(swrc_flags, 0, mkChar(v->site_swrc_name));
	SET_STRING_ELT(swrc_flags, 1, mkChar(v->site_ptf_name));

	PROTECT(swrc_names = NEW_CHARACTER(2));
	for (i = 0; i < 2; i++) {
		SET_STRING_ELT(swrc_names, i, mkChar(cSWRCflags[i]));
	}
	setAttrib(swrc_flags, R_NamesSymbol, swrc_names);

	PROTECT(has_swrcp = NEW_LOGICAL(1));
	LOGICAL(has_swrcp)[0] = v->site_has_swrcp;


	// Fill all slots of `SW_SIT`
	SET_SLOT(SW_SIT, install(cSW_SIT[0]), SWClimits);
	SET_SLOT(SW_SIT, install(cSW_SIT[1]), ModelFlags);
	SET_SLOT(SW_SIT, install(cSW_SIT[2]), ModelCoefficients);
	SET_SLOT(SW_SIT, install(cSW_SIT[3]), SnowSimulationParameters);
	SET_SLOT(SW_SIT, install(cSW_SIT[4]), DrainageCoefficient);
	SET_SLOT(SW_SIT, install(cSW_SIT[5]), EvaporationCoefficients);
	SET_SLOT(SW_SIT, install(cSW_SIT[6]), TranspirationCoefficients);
	SET_SLOT(SW_SIT, install(cSW_SIT[7]), IntrinsicSiteParams);
	SET_SLOT(SW_SIT, install(cSW_SIT[8]), SoilTemperatureConstants_use);
	SET_SLOT(SW_SIT, install(cSW_SIT[9]), SoilTemperatureConstants);
	SET_SLOT(SW_SIT, install(cSW_SIT[10]), SoilDensityInputType);
	SET_SLOT(SW_SIT, install(cSW_SIT[11]), TranspirationRegions);
	SET_SLOT(SW_SIT, install(cSW_SIT[12]), swrc_flags);
	SET_SLOT(SW_SIT, install(cSW_SIT[13]), has_swrcp);

	UNPROTECT(28);
	return SW_SIT;
}

void onSet_SW_SIT(SEXP SW_SIT, LOG_INFO* LogInfo) {
	int i;
	SW_SITE *v = &SoilWatRun.Site;
	SW_MODEL *m = &SoilWatRun.Model;

	SEXP SWClimits;
	SEXP ModelFlags;
	SEXP ModelCoefficients;
	SEXP SnowSimulationParameters;
	SEXP DrainageCoefficient;
	SEXP EvaporationCoefficients;
	SEXP TranspirationCoefficients;
	SEXP IntrinsicSiteParams;
	SEXP SoilTemperatureConstants_use;
	SEXP SoilTemperatureConstants;
	SEXP SoilDensityInputType;
	SEXP TranspirationRegions;
	SEXP swrc_flags, has_swrcp;

	int *p_transp; // ideally `LyrIndex` so that same type as `_TranspRgnBounds`, but R API INTEGER() is signed

  #ifdef RSWDEBUG
  int debug = 0;
  #endif

	MyFileName = SoilWatDomain.PathInfo.InFiles[eSite];

	LyrIndex r; /* transp region definition number */
	Bool too_many_regions = FALSE;

	#ifdef RSWDEBUG
	if (debug) sw_printf("'onSet_SW_SIT':");
	#endif

	PROTECT(SWClimits = GET_SLOT(SW_SIT, install("SWClimits")));
	v->_SWCMinVal = REAL(SWClimits)[0];
	v->_SWCInitVal = REAL(SWClimits)[1];
	v->_SWCWetVal = REAL(SWClimits)[2];
	#ifdef RSWDEBUG
	if (debug) sw_printf(" > 'SWClimits'");
	#endif

	PROTECT(ModelFlags = GET_SLOT(SW_SIT, install("ModelFlags")));
	v->reset_yr = LOGICAL(ModelFlags)[0];
	v->deepdrain = LOGICAL(ModelFlags)[1];
	#ifdef RSWDEBUG
	if (debug) sw_printf(" > 'flags'");
	#endif

	PROTECT(ModelCoefficients = GET_SLOT(SW_SIT, install("ModelCoefficients")));
	v->pet_scale = REAL(ModelCoefficients)[0];
	v->percentRunoff = REAL(ModelCoefficients)[1];
	v->percentRunon = REAL(ModelCoefficients)[2];
	#ifdef RSWDEBUG
	if (debug) sw_printf(" > 'coefs'");
	#endif

	PROTECT(SnowSimulationParameters = GET_SLOT(SW_SIT, install("SnowSimulationParameters")));
	v->TminAccu2 = REAL(SnowSimulationParameters)[0];
	v->TmaxCrit = REAL(SnowSimulationParameters)[1];
	v->lambdasnow = REAL(SnowSimulationParameters)[2];
	v->RmeltMin = REAL(SnowSimulationParameters)[3];
	v->RmeltMax = REAL(SnowSimulationParameters)[4];
	#ifdef RSWDEBUG
	if (debug) sw_printf(" > 'snow'");
	#endif

	PROTECT(DrainageCoefficient = GET_SLOT(SW_SIT, install("DrainageCoefficient")));
	v->slow_drain_coeff = REAL(DrainageCoefficient)[0];
	#ifdef RSWDEBUG
	if (debug) sw_printf(" > 'drain-coef'");
	#endif

	PROTECT(EvaporationCoefficients = GET_SLOT(SW_SIT, install("EvaporationCoefficients")));
	v->evap.xinflec = REAL(EvaporationCoefficients)[0];
	v->evap.slope = REAL(EvaporationCoefficients)[1];
	v->evap.yinflec = REAL(EvaporationCoefficients)[2];
	v->evap.range = REAL(EvaporationCoefficients)[3];
	#ifdef RSWDEBUG
	if (debug) sw_printf(" > 'evap-coef'");
	#endif

	PROTECT(TranspirationCoefficients = GET_SLOT(SW_SIT, install("TranspirationCoefficients")));
	v->transp.xinflec = REAL(TranspirationCoefficients)[0];
	v->transp.slope = REAL(TranspirationCoefficients)[1];
	v->transp.yinflec = REAL(TranspirationCoefficients)[2];
	v->transp.range = REAL(TranspirationCoefficients)[3];
	#ifdef RSWDEBUG
	if (debug) sw_printf(" > 'transp-coef'");
	#endif

	// SOILWAT2 calculates internally in radians, but input/output are in arc-degrees
	PROTECT(IntrinsicSiteParams = GET_SLOT(SW_SIT, install("IntrinsicSiteParams")));
	m->longitude = REAL(IntrinsicSiteParams)[0] * deg_to_rad;
	m->latitude = REAL(IntrinsicSiteParams)[1] * deg_to_rad;
	m->elevation = REAL(IntrinsicSiteParams)[2];
	m->slope = REAL(IntrinsicSiteParams)[3] * deg_to_rad;
	m->aspect = REAL(IntrinsicSiteParams)[4];
	m->aspect = missing(m->aspect) ? SW_MISSING : m->aspect * deg_to_rad;
	#ifdef RSWDEBUG
	if (debug) sw_printf(" > 'location'");
	#endif

	PROTECT(SoilTemperatureConstants_use = GET_SLOT(SW_SIT, install("SoilTemperatureFlag")));
	v->use_soil_temp = LOGICAL(SoilTemperatureConstants_use)[0];
	#ifdef RSWDEBUG
	if (debug) sw_printf(" > 'soiltemp-flag'");
	#endif

	PROTECT(SoilTemperatureConstants = GET_SLOT(SW_SIT, install("SoilTemperatureConstants")));
	v->bmLimiter = REAL(SoilTemperatureConstants)[0];
	v->t1Param1 = REAL(SoilTemperatureConstants)[1];
	v->t1Param2 = REAL(SoilTemperatureConstants)[2];
	v->t1Param3 = REAL(SoilTemperatureConstants)[3];
	v->csParam1 = REAL(SoilTemperatureConstants)[4];
	v->csParam2 = REAL(SoilTemperatureConstants)[5];
	v->shParam = REAL(SoilTemperatureConstants)[6];
	v->Tsoil_constant = REAL(SoilTemperatureConstants)[7];
	v->stDeltaX = REAL(SoilTemperatureConstants)[8];
	v->stMaxDepth = REAL(SoilTemperatureConstants)[9];
	#ifdef RSWDEBUG
	if (debug) sw_printf(" > 'soiltemp-constants'");
	#endif

	PROTECT(SoilDensityInputType = GET_SLOT(SW_SIT, install("SoilDensityInputType")));
	v->type_soilDensityInput = INTEGER(SoilDensityInputType)[0];
	#ifdef RSWDEBUG
	if (debug) sw_printf(" > 'density-type'");
	#endif


	PROTECT(swrc_flags = GET_SLOT(SW_SIT, install("swrc_flags")));
	strcpy(v->site_swrc_name, CHAR(STRING_ELT(swrc_flags, 0)));
	v->site_swrc_type = encode_str2swrc(v->site_swrc_name, LogInfo);
    if(LogInfo->stopRun) {
        UNPROTECT(12); // Unprotect the twelve protected variables before exiting
        return; // Exit function prematurely due to error
    }
	strcpy(v->site_ptf_name, CHAR(STRING_ELT(swrc_flags, 1)));
	v->site_ptf_type = encode_str2ptf(v->site_ptf_name);
	PROTECT(has_swrcp = GET_SLOT(SW_SIT, install("has_swrcp")));
	v->site_has_swrcp = LOGICAL(has_swrcp)[0];

	#ifdef RSWDEBUG
	if (debug) sw_printf(" > 'swrc/ptf-type'");
	#endif


	PROTECT(TranspirationRegions = GET_SLOT(SW_SIT, install("TranspirationRegions")));
	p_transp = INTEGER(TranspirationRegions);
	v->n_transp_rgn = nrows(TranspirationRegions);
	if (MAX_TRANSP_REGIONS < v->n_transp_rgn) {
		too_many_regions = TRUE;
	} else {
		for (i = 0; i < v->n_transp_rgn; i++) {
			v->_TranspRgnBounds[p_transp[i + v->n_transp_rgn * 0] - 1] = p_transp[i + v->n_transp_rgn * 1] - 1;
		}
	}
	if (too_many_regions) {
		LogError(LogInfo, LOGERROR, "siteparam.in : Number of transpiration regions"
				" exceeds maximum allowed (%d > %d)\n", v->n_transp_rgn, MAX_TRANSP_REGIONS);

        UNPROTECT(14); // Unprotect the fourteen protected variables before exiting
        return; // Exit function prematurely due to error
	}
	#ifdef RSWDEBUG
	if (debug) sw_printf(" > 'transp-regions'");
	#endif

	/* check for any discontinuities (reversals) in the transpiration regions */
	for (r = 1; r < v->n_transp_rgn; r++) {
		if (v->_TranspRgnBounds[r - 1] >= v->_TranspRgnBounds[r]) {
			LogError(LogInfo, LOGERROR, "siteparam.in : Discontinuity/reversal in transpiration regions.\n");

            UNPROTECT(14); // Unprotect the fourteen protected variables before exiting
            return; // Exit function prematurely due to error
		}
	}

	#ifdef RSWDEBUG
	if (debug) sw_printf(" ... done. \n");
	#endif

	UNPROTECT(14);
}
