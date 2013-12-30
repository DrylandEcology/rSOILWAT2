/********************************************************/
/********************************************************/
/*	Application: SOILWAT - soilwater dynamics simulator
 Source file: Site.c
 Type: module
 Purpose: Read / write and otherwise manage the
 site specific information.  See also the
 Layer module.
 History:
 (8/28/01) -- INITIAL CODING - cwb
 (10/12/2009) - (drs) added altitude
 11/02/2010	(drs) added 5 snow parameters to SW_SITE to be read in from siteparam.in
 10/19/2010	(drs) added HydraulicRedistribution flag, and maxCondroot, swp50, and shapeCond parameters to SW_SIT_read()  and _echo_inputs()
 07/20/2011	(drs) updated _read_layers() to read impermeability values from each soil layer from soils.in file
 added calculation for saturated swc in water_eqn()
 updated _echo_inputs() to print impermeability and saturated swc values
 09/08/2011	(drs) moved all hydraulic redistribution parameters to SW_VegProd.h struct VegType
 09/15/2011	(drs)	deleted albedo from SW_SIT_read() and _echo_inputs(): moved it to SW_VegProd.h to make input vegetation type dependent
 02/03/2012	(drs)	if input of SWCmin < 0 then estimate SWCmin with 'SW_SWC_SWCres' for each soil layer
 02/04/2012	(drs)	included SW_VegProd.h and created global variable extern SW_VegProd: to access vegetation-specific SWPcrit
 02/04/2012	(drs)	added calculation of swc at SWPcrit for each vegetation type and layer to function '_init_site_info()'
 added vwc/swc at SWPcrit to '_echo_inputs()'
 05/24/2012  (DLM) edited SW_SIT_read(void) function to be able to read in Soil Temperature constants from siteparam.in file
 05/24/2012  (DLM) edited _echo_inputs(void) function to echo the Soil Temperature constants to the logfile
 05/25/2012  (DLM) edited _read_layers( void) function to read in the initial soil temperature for each layer
 05/25/2012  (DLM) edited _echo_inputs( void) function to echo the read in soil temperatures for each layer
 05/30/2012  (DLM) edited _read_layers & _echo_inputs functions to read in/echo the deltaX parameter
 05/31/2012  (DLM) edited _read_layers & _echo_inputs functions to read in/echo stMaxDepth & use_soil_temp variables
 05/31/2012  (DLM) edited _init_site_info(void) to check if stMaxDepth & stDeltaX values are usable, if not it resets them to the defaults (180 & 15).
 11/06/2012	(clk)	In SW_SIT_read(void), added lines to read in aspect and slope from siteparam.in
 11/06/2012	(clk)	In _echo_inputs(void), added lines to echo aspect and slope to logfile
 11/30/2012	(clk)	In SW_SIT_read(void), added lines to read in percentRunoff from siteparam.in
 11/30/2012	(clk)	In _echo_inputs(void), added lines to echo percentRunoff to logfile
 04/16/2013	(clk)	changed the water_eqn to use the fraction of gravel content in the calculation
 Added the function calculate_soilBulkDensity() which is used to calculate the bulk density of the soil from the inputed matric density. Using eqn 20 from Saxton 2006
 Needed to change the input from soils.in to save to soilMatric_density instead of soilBulk_density
 Changed read_layers() to do a few different things
 First, it now reads in a value for fractionVolBulk_gravel from soils.in
 Secondly, it calls the calculate_soilBulkDensity function for each layer
 Lastly, since fieldcap and wiltpt were removed from soils.in, those values are now calculated within read_layers()
 05/16/2013	(drs)	fixed in _init_site_info() the check of transpiration region validity: it gave error if only one layer was present
 06/24/2013	(rjm)	added function void SW_SIT_clear_layers(void) to free allocated soil layers
 06/27/2013	(drs)	closed open files if LogError() with LOGFATAL is called in SW_SIT_read(), _read_layers()
 07/09/2013	(clk)	added the initialization of all the new variables

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

#include "generic.h"
#include "filefuncs.h"
#include "myMemory.h"
#include "SW_Defines.h"

#include "SW_Files.h"
#include "SW_Site.h"
#include "SW_SoilWater.h"

#include "SW_VegProd.h"

/* =================================================== */
/*                  Global Variables                   */
/* --------------------------------------------------- */

extern SW_VEGPROD SW_VegProd;
#ifdef RSOILWAT
extern Bool collectInData;
extern SEXP InputData;
#endif
SW_SITE SW_Site; /* declared here, externed elsewhere */

extern Bool EchoInits;

/* =================================================== */
/*                Module-Level Variables               */
/* --------------------------------------------------- */
static char *MyFileName;

/* transpiration regions  shallow, moderately shallow,  */
/* deep and very deep. units are in layer numbers. */
static LyrIndex _TranspRgnBounds[MAX_TRANSP_REGIONS];

/* for these three, units are cm/cm if < 1, -bars if >= 1 */
static RealD _SWCInitVal, /* initialization value for swc */
_SWCWetVal, /* value for a "wet" day,       */
_SWCMinVal; /* lower bound on swc.          */

/* =================================================== */
/* =================================================== */
/*             Private Function Definitions            */
/* --------------------------------------------------- */

static void _init_site_info(void);
static void _read_layers(void);
static void _echo_inputs(void);

static void water_eqn(RealD fractionGravel, RealD sand, RealD clay, LyrIndex n) {
	/* --------------------------------------------------- */
	RealD theta33, theta33t, OM = 0., thetasMatric33, thetasMatric33t; /* Saxton et al. auxiliary variables */

	SW_Site.lyr[n]->thetasMatric = -14.2 * sand - 3.7 * clay + 50.5;
	SW_Site.lyr[n]->psisMatric = powe(10.0, (-1.58* sand - 0.63*clay + 2.17));
	SW_Site.lyr[n]->bMatric = -0.3 * sand + 15.7 * clay + 3.10;

	if (ZRO(SW_Site.lyr[n]->bMatric)) {
		LogError(stdout, LOGFATAL, "Value of beta in SW_SIT_read() = %f\n"
				"Possible division by zero.  Exiting", SW_Site.lyr[n]->bMatric);
	}

	SW_Site.lyr[n]->binverseMatric = 1.0 / SW_Site.lyr[n]->bMatric;

	/* saturated soil water content: Saxton, K. E. and W. J. Rawls. 2006. Soil water characteristic estimates by texture and organic matter for hydrologic solutions. Soil Science Society of America Journal 70:1569-1578. */
	theta33t = -0.251 * sand + 0.195 * clay + 0.011 * OM + 0.006 * (sand * OM) - 0.027 * (clay * OM) + 0.452 * (sand * clay) + 0.299;
	theta33 = theta33t + (1.283 * powe(theta33t, 2) - 0.374 * theta33t - 0.015);

	thetasMatric33t = 0.278 * sand + 0.034 * clay + 0.022 * OM - 0.018 * sand * OM - 0.027 * clay * OM - 0.584 * sand * clay + 0.078;
	thetasMatric33 = thetasMatric33t + (0.636 * thetasMatric33t - 0.107);

	SW_Site.lyr[n]->swcBulk_saturated = SW_Site.lyr[n]->width * (theta33 + thetasMatric33 - 0.097 * sand + 0.043) * (1 - fractionGravel);

}

static void calculate_soilBulkDensity(RealD matricDensity, RealD fractionGravel, LyrIndex n) {
	/* ---------------------------------------------------------------- */
	/* used to calculate the bulk density from the given matric density */
	/* ---------------------------------------------------------------- */
	SW_Site.lyr[n]->soilBulk_density = matricDensity * (1 - fractionGravel) + (fractionGravel * 2.65); /*eqn. 20 from Saxton et al. 2006  to calculate the bulk density of soil */
}

static LyrIndex _newlayer(void) {
	/* --------------------------------------------------- */
	/* first time called with no layers so SW_Site.lyr
	 not initialized yet, malloc() required.  For each
	 layer thereafter realloc() is called.
	 */
	SW_SITE *v = &SW_Site;
	v->n_layers++;

	v->lyr = (!v->lyr) /* if not yet defined */
	? (SW_LAYER_INFO **) /* malloc() it  */
	Mem_Calloc(v->n_layers, sizeof(SW_LAYER_INFO *), "_newlayer()")

	:
		(SW_LAYER_INFO **) /* else realloc() */
		Mem_ReAlloc(v->lyr, sizeof(SW_LAYER_INFO *) * (v->n_layers));
	v->lyr[v->n_layers - 1] = (SW_LAYER_INFO *) Mem_Calloc(1, sizeof(SW_LAYER_INFO), "_newlayer()");
	return v->n_layers - 1;
}

/*static void _clear_layer(LyrIndex n) {
 --------------------------------------------------- 

 memset(SW_Site.lyr[n], 0, sizeof(SW_LAYER_INFO));

 }*/

/* =================================================== */
/* =================================================== */
/*             Public Function Definitions             */
/* --------------------------------------------------- */

void SW_SIT_construct(void) {
	/* =================================================== */
	/* note that an initializer that is called during
	 * execution (better called clean() or something)
	 * will need to free all allocated memory first
	 * before clearing structure.
	 */
	memset(&SW_Site, 0, sizeof(SW_Site));

}

void SW_SIT_read(void) {
	/* =================================================== */
	/* 5-Feb-2002 (cwb) Removed rgntop requirement in
	 *    transpiration regions section of input
	 */
	SW_SITE *v = &SW_Site;
	FILE *f;
	int lineno = 0, x;
	LyrIndex r, region, /* transp region definition number */
	rgnlow; /* lower layer of region */
	Bool too_many_regions = FALSE;

	/* note that Files.read() must be called prior to this. */
	MyFileName = SW_F_name(eSite);

	f = OpenFile(MyFileName, "r");

	v->n_transp_rgn = 0;
	while (GetALine(f, inbuf)) {
		switch (lineno) {
		case 0:
			_SWCMinVal = atof(inbuf);
			break;
		case 1:
			_SWCInitVal = atof(inbuf);
			break;
		case 2:
			_SWCWetVal = atof(inbuf);
			break;
		case 3:
			v->reset_yr = itob(atoi(inbuf));
			break;
		case 4:
			v->deepdrain = itob(atoi(inbuf));
			break;
		case 5:
			v->pet_scale = atof(inbuf);
			break;
		case 6:
			v->percentRunoff = atof(inbuf);
			break;
		case 7:
			v->TminAccu2 = atof(inbuf);
			break;
		case 8:
			v->TmaxCrit = atof(inbuf);
			break;
		case 9:
			v->lambdasnow = atof(inbuf);
			break;
		case 10:
			v->RmeltMin = atof(inbuf);
			break;
		case 11:
			v->RmeltMax = atof(inbuf);
			break;
		case 12:
			v->slow_drain_coeff = atof(inbuf);
			break;
		case 13:
			v->evap.xinflec = atof(inbuf);
			break;
		case 14:
			v->evap.slope = atof(inbuf);
			break;
		case 15:
			v->evap.yinflec = atof(inbuf);
			break;
		case 16:
			v->evap.range = atof(inbuf);
			break;
		case 17:
			v->transp.xinflec = atof(inbuf);
			break;
		case 18:
			v->transp.slope = atof(inbuf);
			break;
		case 19:
			v->transp.yinflec = atof(inbuf);
			break;
		case 20:
			v->transp.range = atof(inbuf);
			break;
		case 21:
			v->latitude = atof(inbuf);
			break;
		case 22:
			v->altitude = atof(inbuf);
			break;
		case 23:
			v->slope = atof(inbuf);
			break;
		case 24:
			v->aspect = atof(inbuf);
			break;
		case 25:
			v->bmLimiter = atof(inbuf);
			break;
		case 26:
			v->t1Param1 = atof(inbuf);
			break;
		case 27:
			v->t1Param2 = atof(inbuf);
			break;
		case 28:
			v->t1Param3 = atof(inbuf);
			break;
		case 29:
			v->csParam1 = atof(inbuf);
			break;
		case 30:
			v->csParam2 = atof(inbuf);
			break;
		case 31:
			v->shParam = atof(inbuf);
			break;
		case 32:
			v->meanAirTemp = atof(inbuf);
			break;
		case 33:
			v->stDeltaX = atof(inbuf);
			break;
		case 34:
			v->stMaxDepth = atof(inbuf);
			break;
		case 35:
			v->use_soil_temp = itob(atoi(inbuf));
			break;
		default:
			if (lineno > 35 + MAX_TRANSP_REGIONS)
				break; /* skip extra lines */

			if (MAX_TRANSP_REGIONS < v->n_transp_rgn) {
				too_many_regions = TRUE;
				goto Label_End_Read;
			}
			x = sscanf(inbuf, "%d %d", &region, &rgnlow);
			if (x < 2) {
				CloseFile(&f);
				LogError(logfp, LOGFATAL, "%s : Bad record %d.\n", MyFileName, lineno);
			}
			_TranspRgnBounds[region - 1] = rgnlow - 1;
			v->n_transp_rgn++;
		}

		lineno++;
	}

	Label_End_Read:

	CloseFile(&f);

	if (too_many_regions) {
		LogError(logfp, LOGFATAL, "%s : Number of transpiration regions"
				" exceeds maximum allowed (%d > %d)\n", MyFileName, v->n_transp_rgn, MAX_TRANSP_REGIONS);
	}

	/* check for any discontinuities (reversals) in the transpiration regions */
	for (r = 1; r < v->n_transp_rgn; r++) {
		if (_TranspRgnBounds[r - 1] >= _TranspRgnBounds[r]) {
			LogError(logfp, LOGFATAL, "%s : Discontinuity/reversal in transpiration regions.\n", SW_F_name(eSite));

		}
	}

	_read_layers();
#ifndef RSOILWAT
	_init_site_info();
#else
	if(!collectInData)
		_init_site_info();
#endif
	if (EchoInits)
		_echo_inputs();
}

static void _read_layers(void) {
	/* =================================================== */
	/* 5-Feb-2002 (cwb) removed dmin requirement in input file */

	SW_SITE *v = &SW_Site;
	FILE *f;
	Bool evap_ok = TRUE, /* mitigate gaps in layers' evap coeffs */
	transp_ok_forb = TRUE, transp_ok_tree = TRUE, /* same for transpiration coefficients */
	transp_ok_shrub = TRUE, /* same for transpiration coefficients */
	transp_ok_grass = TRUE, /* same for transpiration coefficients */
	fail = FALSE;
	LyrIndex lyrno;
	int x;
	char *errtype = '\0';
	RealF dmin = 0.0, dmax, evco, trco_forb, trco_tree, trco_shrub, trco_grass, psand, pclay, matricd, imperm, soiltemp, fval = 0, f_gravel;

	/* note that Files.read() must be called prior to this. */
	MyFileName = SW_F_name(eLayers);

	f = OpenFile(MyFileName, "r");

	while (GetALine(f, inbuf)) {
		lyrno = _newlayer();

		x = sscanf(inbuf, "%f %f %f %f %f %f %f %f %f %f %f %f", &dmax, &matricd, &f_gravel, &evco, &trco_grass, &trco_shrub, &trco_tree, &trco_forb, &psand, &pclay, &imperm,
				&soiltemp);
		if (x < 10) {
			CloseFile(&f);
			LogError(logfp, LOGFATAL, "%s : Incomplete record %d.\n", MyFileName, lyrno + 1);
		}
		if (LT(matricd,0.)) {
			fail = TRUE;
			fval = matricd;
			errtype = Str_Dup("bulk density");
		} else if (LT(f_gravel,0.) || GT(f_gravel,1)) {
			fail = TRUE;
			fval = f_gravel;
			errtype = Str_Dup("gravel content");
		} else if (LE(psand,0.)) {
			fail = TRUE;
			fval = psand;
			errtype = Str_Dup("sand proportion");
		} else if (LE(pclay,0.)) {
			fail = TRUE;
			fval = pclay;
			errtype = Str_Dup("clay proportion");
		} else if (LT(imperm,0.)) {
			fail = TRUE;
			fval = imperm;
			errtype = Str_Dup("impermeability");
		}
		if (fail) {
			CloseFile(&f);
			LogError(logfp, LOGFATAL, "%s : Invalid %s (%5.4f) in layer %d.\n", MyFileName, errtype, fval, lyrno + 1);
		}

		v->lyr[lyrno]->width = dmax - dmin;
		dmin = dmax;
		v->lyr[lyrno]->soilMatric_density = matricd;
		v->lyr[lyrno]->fractionVolBulk_gravel = f_gravel;
		v->lyr[lyrno]->evap_coeff = evco;
		v->lyr[lyrno]->transp_coeff_forb = trco_forb;
		v->lyr[lyrno]->transp_coeff_tree = trco_tree;
		v->lyr[lyrno]->transp_coeff_shrub = trco_shrub;
		v->lyr[lyrno]->transp_coeff_grass = trco_grass;
		v->lyr[lyrno]->fractionWeightMatric_sand = psand;
		v->lyr[lyrno]->fractionWeightMatric_clay = pclay;
		v->lyr[lyrno]->impermeability = imperm;
		v->lyr[lyrno]->my_transp_rgn_tree = 0;
		v->lyr[lyrno]->my_transp_rgn_forb = 0;
		v->lyr[lyrno]->my_transp_rgn_shrub = 0;
		v->lyr[lyrno]->my_transp_rgn_grass = 0;
		v->lyr[lyrno]->sTemp = soiltemp;

		if (evap_ok) {
			if (GT(v->lyr[lyrno]->evap_coeff, 0.0)) {
				v->n_evap_lyrs++;
			} else
				evap_ok = FALSE;
		}
		if (transp_ok_forb) {
			if (GT(v->lyr[lyrno]->transp_coeff_forb, 0.0))
				v->n_transp_lyrs_forb++;
			else
				transp_ok_forb = FALSE;
		}
		if (transp_ok_tree) {
			if (GT(v->lyr[lyrno]->transp_coeff_tree, 0.0))
				v->n_transp_lyrs_tree++;
			else
				transp_ok_tree = FALSE;
		}
		if (transp_ok_shrub) {
			if (GT(v->lyr[lyrno]->transp_coeff_shrub, 0.0))
				v->n_transp_lyrs_shrub++;
			else
				transp_ok_shrub = FALSE;
		}
		if (transp_ok_grass) {
			if (GT(v->lyr[lyrno]->transp_coeff_grass, 0.0))
				v->n_transp_lyrs_grass++;
			else
				transp_ok_grass = FALSE;
		}

		water_eqn(f_gravel, psand, pclay, lyrno);

		v->lyr[lyrno]->swcBulk_fieldcap = SW_SWPmatric2VWCBulk(f_gravel, 0.333, lyrno) * v->lyr[lyrno]->width;
		v->lyr[lyrno]->swcBulk_wiltpt = SW_SWPmatric2VWCBulk(f_gravel, 15, lyrno) * v->lyr[lyrno]->width;
		calculate_soilBulkDensity(matricd, f_gravel, lyrno);

		if (lyrno == MAX_LAYERS) {
			CloseFile(&f);
			LogError(logfp, LOGFATAL, "%s : Too many layers specified (%d).\n"
					"Maximum number of layers is %d\n", MyFileName, lyrno + 1, MAX_LAYERS);
		}

	}
	CloseFile(&f);

	/* n_layers set in _newlayer() */
#ifdef RSOILWAT
	if (v->deepdrain && !collectInData) {
		lyrno = _newlayer();
		v->lyr[lyrno]->width = 1.0;
	}
#else
	if (v->deepdrain) {
		lyrno = _newlayer();
		v->lyr[lyrno]->width = 1.0;
	}
#endif
}
#ifdef RSOILWAT
SEXP onGet_SW_LYR() {
	int i, dmax = 0;
	SW_SITE *v = &SW_Site;
	SEXP swSoils, SW_SOILS;
	SEXP Layers,Layers_names,Layers_names_y;
	char *cLayers[] = { "depth_cm", "bulkDensity_g/cm^3", "gravel_content", "EvapBareSoil_frac", "transpGrass_frac", "transpShrub_frac",
			"transpTree_frac", "transpForb_frac", "sand_frac", "clay_frac", "impermeability_frac", "soilTemp_c" };
	RealD *p_Layers;
	RealD temp;

	PROTECT(swSoils = MAKE_CLASS("swSoils"));
	PROTECT(SW_SOILS = NEW_OBJECT(swSoils));
	PROTECT(Layers = allocMatrix(REALSXP,v->n_layers,12));
	p_Layers = REAL(Layers);
	for (i = 0; i < (v->n_layers); i++) {
		temp = v->lyr[i]->width;
		p_Layers[i + (v->n_layers) * 0] = dmax = v->lyr[i]->width + dmax;
		p_Layers[i + (v->n_layers) * 1] = v->lyr[i]->soilMatric_density;
		p_Layers[i + (v->n_layers) * 2] = v->lyr[i]->fractionVolBulk_gravel;
		p_Layers[i + (v->n_layers) * 3] = v->lyr[i]->evap_coeff;
		p_Layers[i + (v->n_layers) * 4] = v->lyr[i]->transp_coeff_grass;
		p_Layers[i + (v->n_layers) * 5] = v->lyr[i]->transp_coeff_shrub;
		p_Layers[i + (v->n_layers) * 6] = v->lyr[i]->transp_coeff_tree;
		p_Layers[i + (v->n_layers) * 7] = v->lyr[i]->transp_coeff_forb;
		p_Layers[i + (v->n_layers) * 8] = v->lyr[i]->fractionWeightMatric_sand;
		p_Layers[i + (v->n_layers) * 9] = v->lyr[i]->fractionWeightMatric_clay;
		p_Layers[i + (v->n_layers) * 10] = v->lyr[i]->impermeability;
		p_Layers[i + (v->n_layers) * 11] = v->lyr[i]->sTemp;
	}
	PROTECT(Layers_names = allocVector(VECSXP,2));
	PROTECT(Layers_names_y = allocVector(STRSXP,12));
	for(i=0;i<12;i++)
		SET_STRING_ELT(Layers_names_y,i,mkChar(cLayers[i]));
	SET_VECTOR_ELT(Layers_names,1,Layers_names_y);
	setAttrib(Layers, R_DimNamesSymbol, Layers_names);

	SET_SLOT(SW_SOILS,install("Layers"),Layers);

	UNPROTECT(5);
	return SW_SOILS;
}
void onSet_SW_LYR(SEXP SW_SOILS) {

	SW_SITE *v = &SW_Site;
	Bool evap_ok = TRUE, /* mitigate gaps in layers' evap coeffs */
	transp_ok_tree = TRUE, /* same for transpiration coefficients */
	transp_ok_forb = TRUE,
	transp_ok_shrub = TRUE, /* same for transpiration coefficients */
	transp_ok_grass = TRUE, /* same for transpiration coefficients */
	fail = FALSE;
	LyrIndex lyrno;
	int x, i, j, columns;
	char *errtype = '\0';
	RealF dmin = 0.0, dmax, evco, trco_forb, trco_tree, trco_shrub, trco_grass, psand, pclay, matricd, imperm, soiltemp, fval = 0, f_gravel;
	RealD *p_Layers;
	SEXP SW_LYR;
	/* note that Files.read() must be called prior to this. */
	PROTECT(SW_LYR = GET_SLOT(SW_SOILS,install("Layers")));

	MyFileName = SW_F_name(eLayers);

	j = nrows(SW_LYR);
	p_Layers = REAL(SW_LYR);
	columns = ncols(SW_LYR);
	if(columns != 12)
		LogError(logfp, LOGFATAL, "%s : Too few columns in layers specified (%d).\n", MyFileName, columns);
	for (i = 0; i < j; i++) {
		lyrno = _newlayer();

		dmax = p_Layers[i + j * 0];
		matricd = p_Layers[i + j * 1];
		f_gravel = p_Layers[i + j * 2];
		evco = p_Layers[i + j * 3];
		trco_grass = p_Layers[i + j * 4];
		trco_shrub = p_Layers[i + j * 5];
		trco_tree = p_Layers[i + j * 6];
		trco_forb = p_Layers[i + j * 7];
		psand = p_Layers[i + j * 8];
		pclay = p_Layers[i + j * 9];
		imperm = p_Layers[i + j * 10];
		soiltemp = p_Layers[i + j * 11];

		if (LT(matricd,0.)) {
			fail = TRUE;
			fval = matricd;
			errtype = Str_Dup("bulk density");
		} else if (LT(f_gravel,0.) || GT(f_gravel,1.)) {
			fail = TRUE;
			fval = f_gravel;
			errtype = Str_Dup("gravel content");
		} else if (LE(psand,0.)) {
			fail = TRUE;
			fval = psand;
			errtype = Str_Dup("sand proportion");
		} else if (LE(pclay,0.)) {
			fail = TRUE;
			fval = pclay;
			errtype = Str_Dup("clay proportion");
		} else if (LT(imperm,0.)) {
			fail = TRUE;
			fval = imperm;
			errtype = Str_Dup("impermeability");
		}
		if (fail) {
			LogError(logfp, LOGFATAL, "%s : Invalid %s (%5.4f) in layer %d.\n", MyFileName, errtype, fval, lyrno + 1);
		}

		v->lyr[lyrno]->width = dmax - dmin;
		dmin = dmax;
		v->lyr[lyrno]->soilMatric_density = matricd;
		v->lyr[lyrno]->fractionVolBulk_gravel = f_gravel;
		v->lyr[lyrno]->evap_coeff = evco;
		v->lyr[lyrno]->transp_coeff_tree = trco_tree;
		v->lyr[lyrno]->transp_coeff_shrub = trco_shrub;
		v->lyr[lyrno]->transp_coeff_grass = trco_grass;
		v->lyr[lyrno]->transp_coeff_forb = trco_forb;
		v->lyr[lyrno]->fractionWeightMatric_sand = psand;
		v->lyr[lyrno]->fractionWeightMatric_clay = pclay;
		v->lyr[lyrno]->impermeability = imperm;
		v->lyr[lyrno]->my_transp_rgn_tree = 0;
		v->lyr[lyrno]->my_transp_rgn_shrub = 0;
		v->lyr[lyrno]->my_transp_rgn_grass = 0;
		v->lyr[lyrno]->my_transp_rgn_forb = 0;
		v->lyr[lyrno]->sTemp = soiltemp;

		if (evap_ok) {
			if (GT(v->lyr[lyrno]->evap_coeff, 0.0)) {
				v->n_evap_lyrs++;
			} else
				evap_ok = FALSE;
		}
		if (transp_ok_tree) {
			if (GT(v->lyr[lyrno]->transp_coeff_tree, 0.0))
				v->n_transp_lyrs_tree++;
			else
				transp_ok_tree = FALSE;
		}
		if (transp_ok_shrub) {
			if (GT(v->lyr[lyrno]->transp_coeff_shrub, 0.0))
				v->n_transp_lyrs_shrub++;
			else
				transp_ok_shrub = FALSE;
		}
		if (transp_ok_forb) {
			if (GT(v->lyr[lyrno]->transp_coeff_forb, 0.0))
				v->n_transp_lyrs_forb++;
			else
				transp_ok_forb = FALSE;
		}
		if (transp_ok_grass) {
			if (GT(v->lyr[lyrno]->transp_coeff_grass, 0.0))
				v->n_transp_lyrs_grass++;
			else
				transp_ok_grass = FALSE;
		}

		water_eqn(f_gravel, psand, pclay, lyrno);

		v->lyr[lyrno]->swcBulk_fieldcap = SW_SWPmatric2VWCBulk(f_gravel, 0.333, lyrno) * v->lyr[lyrno]->width;
		v->lyr[lyrno]->swcBulk_wiltpt = SW_SWPmatric2VWCBulk(f_gravel, 15, lyrno) * v->lyr[lyrno]->width;
		calculate_soilBulkDensity(matricd, f_gravel, lyrno);

		if (lyrno == MAX_LAYERS) {
			LogError(logfp, LOGFATAL, "%s : Too many layers specified (%d).\n"
					"Maximum number of layers is %d\n", MyFileName, lyrno + 1, MAX_LAYERS);
		}

	}
	/* n_layers set in _newlayer() */
	if (v->deepdrain) {
		lyrno = _newlayer();
		v->lyr[lyrno]->width = 1.0;
	}
	UNPROTECT(1);
}
SEXP onGet_SW_SIT() {
	int i;
	SW_SITE *v = &SW_Site;

	SEXP swSite;
	SEXP SW_SIT;
	char *cSW_SIT[] = { "SWClimits", "ModelFlags", "ModelCoefficients", "SnowSimulationParameters", "DrainageCoefficient", "EvaporationCoefficients", "TranspirationCoefficients",
			"IntrinsicSiteParams", "SoilTemperatureFlag", "SoilTemperatureConstants", "TranspirationRegions" };

	SEXP SWClimits, SWClimits_names;
	char *cSWClimits[] = { "swc_min", "swc_init", "swc_wet" };

	SEXP ModelFlags, ModelCoefficients, ModelFlags_names, ModelCoefficients_names;
	char *cModelFlags[] = { "Reset", "DeepDrain" };
	char *cModelCoefficients[] = {"PETmultiplier", "DailyRunoff" };

	SEXP SnowSimulationParameters, SnowSimulationParameters_names;
	char *cSnowSimulationParameters[] = { "TminAccu2", "TmaxCrit", "lambdaSnow", "RmeltMin", "RmeltMax" };

	SEXP DrainageCoefficient, DrainageCoefficient_names;

	SEXP EvaporationCoefficients, EvaporationCoefficients_names;
	char *cEvaporationCoefficients[] = { "RateShift", "RateSlope", "InflectionPoint", "Range" };

	SEXP TranspirationCoefficients, TranspirationCoefficients_names;
	char *cTranspirationCoefficients[] = { "RateShift", "RateShape", "InflectionPoint", "Range" };

	SEXP IntrinsicSiteParams, IntrinsicSiteParams_names;
	char *cIntrinsicSiteParams[] = { "Latitude", "Altitude", "Slope", "Aspect" };

	SEXP SoilTemperatureConstants_use, SoilTemperatureConstants, SoilTemperatureConstants_names;
	char *cSoilTempValues[] = { "BiomassLimiter_g/m^2", "T1constant_a", "T1constant_b","T1constant_c", "cs_constant_SoilThermCondct", "cs_constant", "sh_constant_SpecificHeatCapacity",
			"ConstMeanAirTemp", "deltaX_Param", "MaxDepth" };
	char *cSoilTemperatureConstants[] = { "Values", "CalcSoilTemp" };

	SEXP TranspirationRegions, TranspirationRegions_names, TranspirationRegions_names_y;
	char *cTranspirationRegions[] = { "ndx", "layer" };
	RealD *p_transp;

	MyFileName = SW_F_name(eSite);

	PROTECT(swSite = MAKE_CLASS("swSite"));
	PROTECT(SW_SIT = NEW_OBJECT(swSite));

	PROTECT(SWClimits = allocVector(REALSXP, 3));
	REAL(SWClimits)[0] = _SWCMinVal;
	REAL(SWClimits)[1] = _SWCInitVal;
	REAL(SWClimits)[2] = _SWCWetVal;
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

	PROTECT(ModelCoefficients = NEW_NUMERIC(2));
	REAL(ModelCoefficients)[0] = v->pet_scale;
	REAL(ModelCoefficients)[1] = v->percentRunoff;
	PROTECT(ModelCoefficients_names = allocVector(STRSXP, 2));
	for (i = 0; i < 2; i++)
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

	PROTECT(IntrinsicSiteParams = allocVector(REALSXP,4));
	REAL(IntrinsicSiteParams)[0] = v->latitude;
	REAL(IntrinsicSiteParams)[1] = v->altitude;
	REAL(IntrinsicSiteParams)[2] = v->slope;
	REAL(IntrinsicSiteParams)[3] = v->aspect;
	PROTECT(IntrinsicSiteParams_names = allocVector(STRSXP,4));
	for (i = 0; i < 4; i++)
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
	REAL(SoilTemperatureConstants)[7] = v->meanAirTemp;
	REAL(SoilTemperatureConstants)[8] = v->stDeltaX;
	REAL(SoilTemperatureConstants)[9] = v->stMaxDepth;
	PROTECT(SoilTemperatureConstants_names = allocVector(STRSXP,10));
	for (i = 0; i < 10; i++)
		SET_STRING_ELT(SoilTemperatureConstants_names, i, mkChar(cSoilTempValues[i]));
	setAttrib(SoilTemperatureConstants, R_NamesSymbol, SoilTemperatureConstants_names);

	PROTECT(TranspirationRegions = allocMatrix(REALSXP,(v->n_transp_rgn),2));
	p_transp = REAL(TranspirationRegions);
	for (i = 0; i < (v->n_transp_rgn); i++) {
		p_transp[i + (v->n_transp_rgn) * 0] = (i + 1);
		p_transp[i + (v->n_transp_rgn) * 1] = (_TranspRgnBounds[i]+1);
	}
	PROTECT(TranspirationRegions_names = allocVector(VECSXP,2));
	PROTECT(TranspirationRegions_names_y = allocVector(STRSXP,2));
	for (i = 0; i < 2; i++)
		SET_STRING_ELT(TranspirationRegions_names_y, i, mkChar(cTranspirationRegions[i]));
	SET_VECTOR_ELT(TranspirationRegions_names, 1, TranspirationRegions_names_y);
	setAttrib(TranspirationRegions, R_DimNamesSymbol, TranspirationRegions_names);

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
	SET_SLOT(SW_SIT, install(cSW_SIT[10]), TranspirationRegions);

	UNPROTECT(24);
	return SW_SIT;
}
void onSet_SW_SIT(SEXP SW_SIT) {
	int i;
	SW_SITE *v = &SW_Site;

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
	SEXP TranspirationRegions;
	RealD *p_transp;

	MyFileName = SW_F_name(eSite);

	int lineno = 0, x;
	LyrIndex r, region, /* transp region definition number */
	rgnlow; /* lower layer of region */
	Bool too_many_regions = FALSE;

	PROTECT(SWClimits = GET_SLOT(SW_SIT, install("SWClimits")));
	_SWCMinVal = REAL(SWClimits)[0];
	_SWCInitVal = REAL(SWClimits)[1];
	_SWCWetVal = REAL(SWClimits)[2];

	PROTECT(ModelFlags = GET_SLOT(SW_SIT, install("ModelFlags")));
	v->reset_yr = LOGICAL(ModelFlags)[0];
	v->deepdrain = LOGICAL(ModelFlags)[1];
	PROTECT(ModelCoefficients = GET_SLOT(SW_SIT, install("ModelCoefficients")));
	v->pet_scale = REAL(ModelCoefficients)[0];
	v->percentRunoff = REAL(ModelCoefficients)[1];

	PROTECT(SnowSimulationParameters = GET_SLOT(SW_SIT, install("SnowSimulationParameters")));
	v->TminAccu2 = REAL(SnowSimulationParameters)[0];
	v->TmaxCrit = REAL(SnowSimulationParameters)[1];
	v->lambdasnow = REAL(SnowSimulationParameters)[2];
	v->RmeltMin = REAL(SnowSimulationParameters)[3];
	v->RmeltMax = REAL(SnowSimulationParameters)[4];

	PROTECT(DrainageCoefficient = GET_SLOT(SW_SIT, install("DrainageCoefficient")));
	v->slow_drain_coeff = REAL(DrainageCoefficient)[0];

	PROTECT(EvaporationCoefficients = GET_SLOT(SW_SIT, install("EvaporationCoefficients")));
	v->evap.xinflec = REAL(EvaporationCoefficients)[0];
	v->evap.slope = REAL(EvaporationCoefficients)[1];
	v->evap.yinflec = REAL(EvaporationCoefficients)[2];
	v->evap.range = REAL(EvaporationCoefficients)[3];

	PROTECT(TranspirationCoefficients = GET_SLOT(SW_SIT, install("TranspirationCoefficients")));
	v->transp.xinflec = REAL(TranspirationCoefficients)[0];
	v->transp.slope = REAL(TranspirationCoefficients)[1];
	v->transp.yinflec = REAL(TranspirationCoefficients)[2];
	v->transp.range = REAL(TranspirationCoefficients)[3];

	PROTECT(IntrinsicSiteParams = GET_SLOT(SW_SIT, install("IntrinsicSiteParams")));
	v->latitude = REAL(IntrinsicSiteParams)[0];
	v->altitude = REAL(IntrinsicSiteParams)[1];
	v->slope = REAL(IntrinsicSiteParams)[2];
	v->aspect = REAL(IntrinsicSiteParams)[3];

	PROTECT(SoilTemperatureConstants_use = GET_SLOT(SW_SIT, install("SoilTemperatureFlag")));
	v->use_soil_temp = LOGICAL(SoilTemperatureConstants_use)[0];
	PROTECT(SoilTemperatureConstants = GET_SLOT(SW_SIT, install("SoilTemperatureConstants")));
	v->bmLimiter = REAL(SoilTemperatureConstants)[0];
	v->t1Param1 = REAL(SoilTemperatureConstants)[1];
	v->t1Param2 = REAL(SoilTemperatureConstants)[2];
	v->t1Param3 = REAL(SoilTemperatureConstants)[3];
	v->csParam1 = REAL(SoilTemperatureConstants)[4];
	v->csParam2 = REAL(SoilTemperatureConstants)[5];
	v->shParam = REAL(SoilTemperatureConstants)[6];
	v->meanAirTemp = REAL(SoilTemperatureConstants)[7];
	v->stDeltaX = REAL(SoilTemperatureConstants)[8];
	v->stMaxDepth = REAL(SoilTemperatureConstants)[9];

	PROTECT(TranspirationRegions = GET_SLOT(SW_SIT, install("TranspirationRegions")));
	p_transp = REAL(TranspirationRegions);
	v->n_transp_rgn = nrows(TranspirationRegions);

	if (MAX_TRANSP_REGIONS < v->n_transp_rgn) {
		too_many_regions = TRUE;
	} else {
		for (i = 0; i < v->n_transp_rgn; i++) {
			_TranspRgnBounds[(int)(p_transp[i + v->n_transp_rgn * 0]-1)] = (p_transp[i + v->n_transp_rgn * 1]-1);
		}
	}
	if (too_many_regions) {
		LogError(logfp, LOGFATAL, "siteparam.in : Number of transpiration regions"
				" exceeds maximum allowed (%d > %d)\n", v->n_transp_rgn, MAX_TRANSP_REGIONS);
	}

	/* check for any discontinuities (reversals) in the transpiration regions */
	for (r = 1; r < v->n_transp_rgn; r++) {
		if (_TranspRgnBounds[r - 1] >= _TranspRgnBounds[r]) {
			LogError(logfp, LOGFATAL, "siteparam.in : Discontinuity/reversal in transpiration regions.\n");
		}
	}

	onSet_SW_LYR(GET_SLOT(InputData,install("soils")));
	_init_site_info();
	if (EchoInits)
		_echo_inputs();
	UNPROTECT(11);
}
#endif
static void _init_site_info(void) {
	/* =================================================== */
	/* potentially this routine can be called whether the
	 * layer data came from a file or a function call which
	 * still requires initialization.
	 */
	/* 5-Mar-2002 (cwb) added normalization for ev and tr coefficients */
	/* 1-Oct-03 (cwb) removed sum_evap_coeff and sum_transp_coeff  */

	SW_SITE *sp = &SW_Site;
	SW_LAYER_INFO *lyr;
	LyrIndex s, r, curregion;
	int wiltminflag = 0, initminflag = 0;
	RealD evsum = 0., trsum_forb = 0., trsum_tree = 0., trsum_shrub = 0., trsum_grass = 0., swcmin_help1, swcmin_help2;

	/* sp->deepdrain indicates an extra (dummy) layer for deep drainage
	 * has been added, so n_layers really should be n_layers -1
	 * otherwise, the bottom layer is functional, so don't decrement n_layers
	 * and set deep_layer to zero as a flag.
	 * NOTE: deep_lyr is base0, n_layers is BASE1
	 */
	sp->deep_lyr = (sp->deepdrain) ? --sp->n_layers : 0;

	ForEachSoilLayer(s)
	{
		lyr = sp->lyr[s];
		/* sum ev and tr coefficients for later */
		evsum += lyr->evap_coeff;
		trsum_forb += lyr->transp_coeff_forb;
		trsum_tree += lyr->transp_coeff_tree;
		trsum_shrub += lyr->transp_coeff_shrub;
		trsum_grass += lyr->transp_coeff_grass;

		/* calculate soil water content at SWPcrit for each vegetation type */
		lyr->swcBulk_atSWPcrit_forb = SW_SWPmatric2VWCBulk(lyr->fractionVolBulk_gravel, SW_VegProd.forb.SWPcrit, s) * lyr->width;
		lyr->swcBulk_atSWPcrit_tree = SW_SWPmatric2VWCBulk(lyr->fractionVolBulk_gravel, SW_VegProd.tree.SWPcrit, s) * lyr->width;
		lyr->swcBulk_atSWPcrit_shrub = SW_SWPmatric2VWCBulk(lyr->fractionVolBulk_gravel, SW_VegProd.shrub.SWPcrit, s) * lyr->width;
		lyr->swcBulk_atSWPcrit_grass = SW_SWPmatric2VWCBulk(lyr->fractionVolBulk_gravel, SW_VegProd.grass.SWPcrit, s) * lyr->width;

		/* Find which transpiration region the current soil layer
		 * is in and check validity of result. Region bounds are
		 * base1 but s is base0.
		 */
		/* for forbs */
		curregion = 0;
		ForEachTranspRegion(r)
		{
			if (s < _TranspRgnBounds[r]) {
				if (ZRO(lyr->transp_coeff_forb))
					break; /* end of transpiring layers */
				curregion = r + 1;
				break;
			}
		}

		if (curregion || ZRO(_TranspRgnBounds[curregion])) {
			lyr->my_transp_rgn_forb = curregion;
			sp->n_transp_lyrs_forb = max(sp->n_transp_lyrs_forb, s);

		} else if (s == 0) {
			LogError(logfp, LOGFATAL, "%s : Top soil layer must be included\n"
					"  in your forb tranpiration regions.\n", SW_F_name(eSite));
		} else if (r < sp->n_transp_rgn) {
			LogError(logfp, LOGFATAL, "%s : Transpiration region %d \n"
					"  is deeper than the deepest layer with a\n"
					"  forb transpiration coefficient > 0 (%d) in '%s'.\n"
					"  Please fix the discrepancy and try again.\n", SW_F_name(eSite), r + 1, s, SW_F_name(eLayers));
		}

		/* for trees */
		curregion = 0;
		ForEachTranspRegion(r)
		{
			if (s < _TranspRgnBounds[r]) {
				if (ZRO(lyr->transp_coeff_tree))
					break; /* end of transpiring layers */
				curregion = r + 1;
				break;
			}
		}

		if (curregion || ZRO(_TranspRgnBounds[curregion])) {
			lyr->my_transp_rgn_tree = curregion;
			sp->n_transp_lyrs_tree = max(sp->n_transp_lyrs_tree, s);

		} else if (s == 0) {
			LogError(logfp, LOGFATAL, "%s : Top soil layer must be included\n"
					"  in your tree tranpiration regions.\n", SW_F_name(eSite));
		} else if (r < sp->n_transp_rgn) {
			LogError(logfp, LOGFATAL, "%s : Transpiration region %d \n"
					"  is deeper than the deepest layer with a\n"
					"  tree transpiration coefficient > 0 (%d) in '%s'.\n"
					"  Please fix the discrepancy and try again.\n", SW_F_name(eSite), r + 1, s, SW_F_name(eLayers));
		}

		/* for shrubs */
		curregion = 0;
		ForEachTranspRegion(r)
		{
			if (s < _TranspRgnBounds[r]) {
				if (ZRO(lyr->transp_coeff_shrub))
					break; /* end of transpiring layers */
				curregion = r + 1;
				break;
			}
		}

		if (curregion || ZRO(_TranspRgnBounds[curregion])) {
			lyr->my_transp_rgn_shrub = curregion;
			sp->n_transp_lyrs_shrub = max(sp->n_transp_lyrs_shrub, s);

		} else if (s == 0) {
			LogError(logfp, LOGFATAL, "%s : Top soil layer must be included\n"
					"  in your shrub tranpiration regions.\n", SW_F_name(eSite));
		} else if (r < sp->n_transp_rgn) {
			LogError(logfp, LOGFATAL, "%s : Transpiration region %d \n"
					"  is deeper than the deepest layer with a\n"
					"  shrub transpiration coefficient > 0 (%d) in '%s'.\n"
					"  Please fix the discrepancy and try again.\n", SW_F_name(eSite), r + 1, s, SW_F_name(eLayers));
		}
		/* for grasses */
		curregion = 0;
		ForEachTranspRegion(r)
		{
			if (s < _TranspRgnBounds[r]) {
				if (ZRO(lyr->transp_coeff_grass))
					break; /* end of transpiring layers */
				curregion = r + 1;
				break;
			}
		}

		if (curregion || ZRO(_TranspRgnBounds[curregion])) {
			lyr->my_transp_rgn_grass = curregion;
			sp->n_transp_lyrs_grass = max(sp->n_transp_lyrs_grass, s);

		} else if (s == 0) {
			LogError(logfp, LOGFATAL, "%s : Top soil layer must be included\n"
					"  in your grass tranpiration regions.\n", SW_F_name(eSite));
		} else if (r < sp->n_transp_rgn) {
			LogError(logfp, LOGFATAL, "%s : Transpiration region %d \n"
					"  is deeper than the deepest layer with a\n"
					"  grass transpiration coefficient > 0 (%d) in '%s'.\n"
					"  Please fix the discrepancy and try again.\n", SW_F_name(eSite), r + 1, s, SW_F_name(eLayers));
		}

		/* Compute swc wet and dry limits and init value */
		if (LT(_SWCMinVal, 0.0)) { /* estimate swcBulk_min for each layer based on residual SWC from an equation in Rawls WJ, Brakensiek DL (1985) Prediction of soil water properties for hydrological modeling. In Watershed management in the Eighties (eds Jones EB, Ward TJ), pp. 293-299. American Society of Civil Engineers, New York.
		 or based on SWC at -3 MPa if smaller (= suction at residual SWC from Fredlund DG, Xing AQ (1994) EQUATIONS FOR THE SOIL-WATER CHARACTERISTIC CURVE. Canadian Geotechnical Journal, 31, 521-532.) */
			swcmin_help1 = SW_VWCBulkRes(lyr->fractionVolBulk_gravel, lyr->fractionWeightMatric_sand, lyr->fractionWeightMatric_clay, lyr->swcBulk_saturated / lyr->width)
					* lyr->width;
			swcmin_help2 = SW_SWPmatric2VWCBulk(lyr->fractionVolBulk_gravel, 30., s) * lyr->width;
			lyr->swcBulk_min = fmax(0., fmin(swcmin_help1, swcmin_help2));
		} else if (GE(_SWCMinVal, 1.0)) { /* assume that unit(_SWCMinVal) == -bar */
			lyr->swcBulk_min = SW_SWPmatric2VWCBulk(lyr->fractionVolBulk_gravel, _SWCMinVal, s) * lyr->width;
		} else { /* assume that unit(_SWCMinVal) == cm/cm */
			lyr->swcBulk_min = _SWCMinVal * lyr->width;
		}

		lyr->swcBulk_wet = GE(_SWCWetVal, 1.0) ? SW_SWPmatric2VWCBulk(lyr->fractionVolBulk_gravel, _SWCWetVal, s) * lyr->width : _SWCWetVal * lyr->width;
		lyr->swcBulk_init = GE(_SWCInitVal, 1.0) ? SW_SWPmatric2VWCBulk(lyr->fractionVolBulk_gravel, _SWCInitVal, s) * lyr->width : _SWCInitVal * lyr->width;

		/* test validity of values */
		if (LT(lyr->swcBulk_init, lyr->swcBulk_min))
			initminflag++;
		if (LT(lyr->swcBulk_wiltpt, lyr->swcBulk_min))
			wiltminflag++;
		if (LE(lyr->swcBulk_wet, lyr->swcBulk_min)) {
			LogError(logfp, LOGFATAL, "%s : Layer %d\n"
					"  calculated swcBulk_wet (%7.4f) <= swcBulk_min (%7.4f).\n"
					"  Recheck parameters and try again.", MyFileName, s + 1, lyr->swcBulk_wet, lyr->swcBulk_min);
		}

	} /*end ForEachSoilLayer */

	if (wiltminflag) {
		LogError(logfp, LOGWARN, "%s : %d layers were found in which wiltpoint < swcBulk_min.\n"
				"  You should reconsider wiltpoint or swcBulk_min.\n"
				"  See site parameter file for swcBulk_min and site.log for swc details.", MyFileName, wiltminflag);
	}

	if (initminflag) {
		LogError(logfp, LOGWARN, "%s : %d layers were found in which swcBulk_init < swcBulk_min.\n"
				"  You should reconsider swcBulk_init or swcBulk_min.\n"
				"  See site parameter file for swcBulk_init and site.log for swc details.", MyFileName, initminflag);
	}

	/* normalize the evap and transp coefficients separately
	 * to avoid obfuscation in the above loop */
	if (!EQ(evsum, 1.0)) {
		LogError(logfp, LOGWARN, "%s : Evap coefficients were normalized, "
				"ev_co sum (%5.4f) != 1.0.\nNew coefficients are:", MyFileName, evsum);
		ForEachEvapLayer(s)
		{
			SW_Site.lyr[s]->evap_coeff /= evsum;
			LogError(logfp, LOGNOTE, "  Layer %d : %5.4f", s + 1, SW_Site.lyr[s]->evap_coeff);
		}
	}
	if (!EQ(trsum_forb, 1.0)) {
		LogError(logfp, LOGWARN, "%s : Transp coefficients for forbs were normalized, "
				"tr_co_forb sum (%5.4f) != 1.0.\nNew Coefficients are:", MyFileName, trsum_forb);
		ForEachForbTranspLayer(s)
		{
			SW_Site.lyr[s]->transp_coeff_forb /= trsum_forb;
			LogError(logfp, LOGNOTE, "  Layer %d : %5.4f", s + 1, SW_Site.lyr[s]->transp_coeff_forb);
		}
	}
	if (!EQ(trsum_tree, 1.0)) {
		LogError(logfp, LOGWARN, "%s : Transp coefficients for trees were normalized, "
				"tr_co_tree sum (%5.4f) != 1.0.\nNew coefficients are:", MyFileName, trsum_tree);
		ForEachTreeTranspLayer(s)
		{
			SW_Site.lyr[s]->transp_coeff_tree /= trsum_tree;
			LogError(logfp, LOGNOTE, "  Layer %d : %5.4f", s + 1, SW_Site.lyr[s]->transp_coeff_tree);
		}
	}
	if (!EQ(trsum_shrub, 1.0)) {
		LogError(logfp, LOGWARN, "%s : Transp coefficients for shrubs were normalized, "
				"tr_co_shrub sum (%5.4f) != 1.0.\nNew coefficients are:", MyFileName, trsum_shrub);
		ForEachShrubTranspLayer(s)
		{
			SW_Site.lyr[s]->transp_coeff_shrub /= trsum_shrub;
			LogError(logfp, LOGNOTE, "  Layer %d : %5.4f", s + 1, SW_Site.lyr[s]->transp_coeff_shrub);
		}
	}
	if (!EQ(trsum_grass, 1.0)) {
		LogError(logfp, LOGWARN, "%s : Transp coefficients for grasses were normalized, "
				"tr_co_grass sum (%5.4f) != 1.0.\nNew coefficients are:", MyFileName, trsum_grass);
		ForEachGrassTranspLayer(s)
		{
			SW_Site.lyr[s]->transp_coeff_grass /= trsum_grass;
			LogError(logfp, LOGNOTE, "  Layer %d : %5.4f", s + 1, SW_Site.lyr[s]->transp_coeff_grass);
		}
	}

	sp->stNRGR = (sp->stMaxDepth / sp->stDeltaX) - 1; // getting the number of regressions, for use in the soil_temperature function
	if (!EQ(fmod(sp->stMaxDepth, sp->stDeltaX), 0.0) || (sp->stNRGR > MAX_ST_RGR)) {
		// resets it to the default values if the remainder of the division != 0.  fmod is like the % symbol except it works with double values
		// without this reset, then there wouldn't be a whole number of regressions in the soil_temperature function (ie there is a remainder from the division), so this way I don't even have to deal with that possibility
		if (sp->stNRGR > MAX_ST_RGR)
			LogError(logfp, LOGWARN,
					"\nSOIL_TEMP FUNCTION ERROR: the number of regressions is > the maximum number of regressions.  resetting max depth, deltaX, nRgr values to 180, 15, & 11 respectively\n");
		else
			LogError(logfp, LOGWARN,
					"\nSOIL_TEMP FUNCTION ERROR: max depth is not evenly divisible by deltaX (ie the remainder != 0).  resetting max depth, deltaX, nRgr values to 180, 15, & 11 respectively\n");

		sp->stMaxDepth = 180.0;
		sp->stNRGR = 11;
		sp->stDeltaX = 15.0;
	}

}

void SW_SIT_clear_layers(void) {
	/* =================================================== */
	/*
	 * For multiple runs with the shared library, the need to
	 * remove the allocated soil layer arises to avoid leaks.(rjm 2013)
	 */
	LyrIndex i, j;
	SW_SITE *s = &SW_Site;

	j = SW_Site.n_layers;

#ifdef RSOILWAT
	if (s->deepdrain && !collectInData) {
		j++;
	}
#else
	if (s->deepdrain) {
			j++;
	}
#endif

	for (i = 0; i < j; i++) {
		free(s->lyr[i]);
		s->lyr[i] = NULL;
	}
	free(s->lyr);
	s->lyr = NULL;
}

static void _echo_inputs(void) {
	/* =================================================== */
	SW_SITE *s = &SW_Site;
	LyrIndex i;

	LogError(logfp, LOGNOTE, "\n\n=====================================================\n"
			"Site Related Parameters:\n"
			"---------------------\n");
	LogError(logfp, LOGNOTE, "  Site File: %s\n", SW_F_name(eSite));
	LogError(logfp, LOGNOTE, "  Reset SWC values each year: %s\n", (s->reset_yr) ? "TRUE" : "FALSE");
	LogError(logfp, LOGNOTE, "  Use deep drainage reservoir: %s\n", (s->deepdrain) ? "TRUE" : "FALSE");
	LogError(logfp, LOGNOTE, "  Slow Drain Coefficient: %5.4f\n", s->slow_drain_coeff);
	LogError(logfp, LOGNOTE, "  PET Scale: %5.4f\n", s->pet_scale);
	LogError(logfp, LOGNOTE, "  Proportion of surface water lost: %5.4f\n", s->percentRunoff);
	LogError(logfp, LOGNOTE, "  Latitude (radians): %4.2f\n", s->latitude);
	LogError(logfp, LOGNOTE, "  Altitude (m a.s.l.): %4.2f \n", s->altitude);
	LogError(logfp, LOGNOTE, "  Slope (degrees): %4.2f\n", s->slope);
	LogError(logfp, LOGNOTE, "  Aspect (degrees): %4.2f\n", s->aspect);

	LogError(logfp, LOGNOTE, "\nSnow simulation parameters (SWAT2K model):\n----------------------\n");
	LogError(logfp, LOGNOTE, "  Avg. air temp below which ppt is snow ( C): %5.4f\n", s->TminAccu2);
	LogError(logfp, LOGNOTE, "  Snow temperature at which snow melt starts ( C): %5.4f\n", s->TmaxCrit);
	LogError(logfp, LOGNOTE, "  Relative contribution of avg. air temperature to todays snow temperture vs. yesterday's snow temperature (0-1): %5.4f\n", s->lambdasnow);
	LogError(logfp, LOGNOTE, "  Minimum snow melt rate on winter solstice (cm/day/C): %5.4f\n", s->RmeltMin);
	LogError(logfp, LOGNOTE, "  Maximum snow melt rate on summer solstice (cm/day/C): %5.4f\n", s->RmeltMax);

	LogError(logfp, LOGNOTE, "\nSoil Temperature Constants:\n----------------------\n");
	LogError(logfp, LOGNOTE, "  Biomass Limiter constant: %5.4f\n", s->bmLimiter);
	LogError(logfp, LOGNOTE, "  T1Param1: %5.4f\n", s->t1Param1);
	LogError(logfp, LOGNOTE, "  T1Param2: %5.4f\n", s->t1Param2);
	LogError(logfp, LOGNOTE, "  T1Param3: %5.4f\n", s->t1Param3);
	LogError(logfp, LOGNOTE, "  csParam1: %5.4f\n", s->csParam1);
	LogError(logfp, LOGNOTE, "  csParam2: %5.4f\n", s->csParam2);
	LogError(logfp, LOGNOTE, "  shParam: %5.4f\n", s->shParam);
	LogError(logfp, LOGNOTE, "  meanAirTemp: %5.4f\n", s->meanAirTemp);
	LogError(logfp, LOGNOTE, "  deltaX: %5.4f\n", s->stDeltaX);
	LogError(logfp, LOGNOTE, "  max depth: %5.4f\n", s->stMaxDepth);
	LogError(logfp, LOGNOTE, "  Make soil temperature calculations: %s\n", (s->use_soil_temp) ? "TRUE" : "FALSE");
	LogError(logfp, LOGNOTE, "  Number of regressions for the soil temperature function: %d\n", s->stNRGR);

	LogError(logfp, LOGNOTE, "\nLayer Related Values:\n----------------------\n");
	LogError(logfp, LOGNOTE, "  Soils File: %s\n", SW_F_name(eLayers));
	LogError(logfp, LOGNOTE, "  Number of soil layers: %d\n", s->n_layers);
	LogError(logfp, LOGNOTE, "  Number of evaporation layers: %d\n", s->n_evap_lyrs);
	LogError(logfp, LOGNOTE, "  Number of forb transpiration layers: %d\n", s->n_transp_lyrs_forb);
	LogError(logfp, LOGNOTE, "  Number of tree transpiration layers: %d\n", s->n_transp_lyrs_tree);
	LogError(logfp, LOGNOTE, "  Number of shrub transpiration layers: %d\n", s->n_transp_lyrs_shrub);
	LogError(logfp, LOGNOTE, "  Number of grass transpiration layers: %d\n", s->n_transp_lyrs_grass);
	LogError(logfp, LOGNOTE, "  Number of transpiration regions: %d\n", s->n_transp_rgn);

	LogError(logfp, LOGNOTE, "\nLayer Specific Values:\n----------------------\n");
	LogError(logfp, LOGNOTE, "\n  Layer information on a per centimeter depth basis:\n");
	LogError(logfp, LOGNOTE,
			"  Lyr Width   BulkD 	%%Gravel    FieldC   WiltPt   %%Sand  %%Clay VWC at Forb-critSWP 	VWC at Tree-critSWP	VWC at Shrub-critSWP	VWC at Grass-critSWP	EvCo   	TrCo_Forb   TrCo_Tree  TrCo_Shrub  TrCo_Grass   TrRgn_Forb    TrRgn_Tree   TrRgn_Shrub   TrRgn_Grass   Wet     Min      Init     Saturated    Impermeability\n");
	LogError(logfp, LOGNOTE,
			"       (cm)   (g/cm^3)  (prop)    (cm/cm)  (cm/cm)   (prop) (prop)  (cm/cm)			(cm/cm)                (cm/cm)            		(cm/cm)         (prop)    (prop)      (prop)     (prop)    (prop)        (int)           (int) 	      	(int) 	    (int) 	    (cm/cm)  (cm/cm)  (cm/cm)  (cm/cm)      (frac)\n");
	LogError(logfp, LOGNOTE,
			"  --- -----   ------    ------     ------   ------   -----  ------   ------                	-------			------            		------          ------    ------      ------      ------   ------       ------   	 -----	        -----       -----   	 ----     ----     ----    ----         ----\n");
	ForEachSoilLayer(i)
	{
		LogError(logfp, LOGNOTE,
				"  %3d %5.1f %9.5f %6.2f %8.5f %8.5f %6.2f %6.2f %6.2f %6.2f %6.2f %6.2f %9.2f %9.2f %9.2f %9.2f %9.2f %10d %10d %15d %15d %15.4f %9.4f %9.4f %9.4f %9.4f\n",
				i + 1, s->lyr[i]->width, s->lyr[i]->soilBulk_density, s->lyr[i]->fractionVolBulk_gravel, s->lyr[i]->swcBulk_fieldcap / s->lyr[i]->width,
				s->lyr[i]->swcBulk_wiltpt / s->lyr[i]->width, s->lyr[i]->fractionWeightMatric_sand, s->lyr[i]->fractionWeightMatric_clay,
				s->lyr[i]->swcBulk_atSWPcrit_forb / s->lyr[i]->width, s->lyr[i]->swcBulk_atSWPcrit_tree / s->lyr[i]->width,
				s->lyr[i]->swcBulk_atSWPcrit_shrub / s->lyr[i]->width, s->lyr[i]->swcBulk_atSWPcrit_grass / s->lyr[i]->width, s->lyr[i]->evap_coeff,
				s->lyr[i]->transp_coeff_forb, s->lyr[i]->transp_coeff_tree, s->lyr[i]->transp_coeff_shrub, s->lyr[i]->transp_coeff_grass, s->lyr[i]->my_transp_rgn_forb,
				s->lyr[i]->my_transp_rgn_tree, s->lyr[i]->my_transp_rgn_shrub, s->lyr[i]->my_transp_rgn_grass, s->lyr[i]->swcBulk_wet / s->lyr[i]->width,
				s->lyr[i]->swcBulk_min / s->lyr[i]->width, s->lyr[i]->swcBulk_init / s->lyr[i]->width, s->lyr[i]->swcBulk_saturated / s->lyr[i]->width,
				s->lyr[i]->impermeability);

	}
	LogError(logfp, LOGNOTE, "\n  Actual per-layer values:\n");
	LogError(logfp, LOGNOTE,
			"  Lyr Width  BulkD	 %%Gravel   FieldC   WiltPt %%Sand  %%Clay	SWC at Forb-critSWP     SWC at Tree-critSWP	SWC at Shrub-critSWP	SWC at Grass-critSWP	 Wet    Min      Init  Saturated	SoilTemp\n");
	LogError(logfp, LOGNOTE,
			"       (cm)  (g/cm^3)	(prop)    (cm)     (cm)  (prop) (prop)   (cm)    	(cm)        		(cm)            (cm)            (cm)   (cm)      (cm)     (cm)		(celcius)\n");
	LogError(logfp, LOGNOTE,
			"  --- -----  -------	------   ------   ------ ------ ------   ------        	------            	------          ----   		----     ----     ----    ----		----\n");

	ForEachSoilLayer(i)
	{
		LogError(logfp, LOGNOTE, "  %3d %5.1f %9.5f %6.2f %8.5f %8.5f %6.2f %6.2f %7.4f %7.4f %7.4f %7.4f %7.4f %7.4f %8.4f %7.4f %5.4f\n", i + 1, s->lyr[i]->width,
				s->lyr[i]->soilBulk_density, s->lyr[i]->fractionVolBulk_gravel, s->lyr[i]->swcBulk_fieldcap, s->lyr[i]->swcBulk_wiltpt, s->lyr[i]->fractionWeightMatric_sand,
				s->lyr[i]->fractionWeightMatric_clay, s->lyr[i]->swcBulk_atSWPcrit_forb, s->lyr[i]->swcBulk_atSWPcrit_tree, s->lyr[i]->swcBulk_atSWPcrit_shrub,
				s->lyr[i]->swcBulk_atSWPcrit_grass, s->lyr[i]->swcBulk_wet, s->lyr[i]->swcBulk_min, s->lyr[i]->swcBulk_init, s->lyr[i]->swcBulk_saturated, s->lyr[i]->sTemp);
	}

	LogError(logfp, LOGNOTE, "\n  Water Potential values:\n");
	LogError(logfp, LOGNOTE, "  Lyr       FieldCap         WiltPt            Forb-critSWP     Tree-critSWP     Shrub-critSWP    Grass-critSWP    Wet            Min            Init\n");
	LogError(logfp, LOGNOTE,
			"            (bars)           (bars)            (bars)           (bars)           (bars)           (bars)           (bars)         (bars)         (bars)\n");
	LogError(logfp, LOGNOTE,
			"  ---       -----------      ------------      -----------      -----------      -----------      -----------      -----------    -----------    --------------    --------------\n");

	ForEachSoilLayer(i)
	{
		LogError(logfp, LOGNOTE, "  %3d   %15.4f   %15.4f  %15.4f %15.4f  %15.4f  %15.4f  %15.4f   %15.4f   %15.4f\n", i + 1,
				SW_SWCbulk2SWPmatric(s->lyr[i]->fractionVolBulk_gravel, s->lyr[i]->swcBulk_fieldcap, i),
				SW_SWCbulk2SWPmatric(s->lyr[i]->fractionVolBulk_gravel, s->lyr[i]->swcBulk_wiltpt, i),
				SW_SWCbulk2SWPmatric(s->lyr[i]->fractionVolBulk_gravel, s->lyr[i]->swcBulk_atSWPcrit_forb, i),
				SW_SWCbulk2SWPmatric(s->lyr[i]->fractionVolBulk_gravel, s->lyr[i]->swcBulk_atSWPcrit_tree, i),
				SW_SWCbulk2SWPmatric(s->lyr[i]->fractionVolBulk_gravel, s->lyr[i]->swcBulk_atSWPcrit_shrub, i),
				SW_SWCbulk2SWPmatric(s->lyr[i]->fractionVolBulk_gravel, s->lyr[i]->swcBulk_atSWPcrit_grass, i),
				SW_SWCbulk2SWPmatric(s->lyr[i]->fractionVolBulk_gravel, s->lyr[i]->swcBulk_wet, i),
				SW_SWCbulk2SWPmatric(s->lyr[i]->fractionVolBulk_gravel, s->lyr[i]->swcBulk_min, i),
				SW_SWCbulk2SWPmatric(s->lyr[i]->fractionVolBulk_gravel, s->lyr[i]->swcBulk_init, i));

	}

	LogError(logfp, LOGNOTE, "\n------------ End of Site Parameters ------------------\n");
	//fflush(logfp);

}

#ifdef DEBUG_MEM
#include "myMemory.h"
/*======================================================*/
void SW_SIT_SetMemoryRefs( void) {
	/* when debugging memory problems, use the bookkeeping
	 code in myMemory.c
	 This routine sets the known memory refs in this module
	 so they can be  checked for leaks, etc.  All refs will
	 have been cleared by a call to ClearMemoryRefs() before
	 this, and will be checked via CheckMemoryRefs() after
	 this, most likely in the main() function.
	 */
	LyrIndex l;

	NoteMemoryRef(SW_Site.lyr);
	ForEachSoilLayer(l) {
		NoteMemoryRef(SW_Site.lyr[l]);
	}
}

#endif
