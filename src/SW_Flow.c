/********************************************************/
/********************************************************/
/*	Source file: SW_Flow.c
 Type: module
 Application: SOILWAT - soilwater dynamics simulator
 Purpose: This is the interesting part of the model--
 the flow of water through the soil.

 ORIGINAL NOTES/COMMENTS
 ********************************************************************
 PURPOSE: Water-flow submodel.  This submodel is a rewrite of a
 model originally written by William Parton.  It simulates
 the flow of water through the plant canopy and soil.
 See "Abiotic Section of ELM", as a reference.
 The subroutines called are listed in the file "subwatr.f"

 HISTORY:
 4/30/92  (not tested - first pass)
 7/2/92   (SLC) Reset swc to 0 if less than 0.  (Due to roundoff)
 See function "chkzero" in the subwatr.f file.  Each
 swc is checked for negative values.
 1/17/94  (SLC) Set daily array to zero when no transpiration or
 evaporation.
 1/27/94  (TLB) Added daily deep drainage variable
 (4/10/2000) -- INITIAL CODING - cwb
 9/21/01	I have to make some transformation from the record-oriented
 structure of the new design to the state array parameter
 structure of the old design.  I thought it would be worth
 trying to rewrite the routines, but with the current version
 of the record layouts, it required too much coupling with the
 other modules, and I refrained for two reasons.  First was
 the time involved, and second was the possiblity of leaving
 the code in SoWat_flow_subs.c as is to facilitate putting
 it into a library.
 10/09/2009	(drs) added snow accumulation, snow sublimation
 and snow melt to SW_Water_Flow()
 01/12/2010	(drs) turned not-fuctional snow sublimation to snow_sublimation (void)
 02/02/2010	(drs) added to SW_Water_Flow(): saving values for standcrop_int, litter_int and soil_inf
 02/08/2010	(drs) if there is a snowpack then
 - rain infiltrates directly to soil (no vegetation or litter interception of today)
 - no transpiration or evaporation (except evaporation of yesterdays interception)
 only
 - infiltrate water high
 - infiltrate water low
 10/04/2010	(drs) moved call to SW_SWC_adjust_snow() back to SW_WTH_new_day()
 10/19/2010	(drs) added call to hydraulic_redistribution() in SW_Water_Flow() after all the evap/transp/infiltration is computed
 added temporary array lyrHydRed to arrays2records() and records2arrays()
 11/16/2010	(drs) added call to forest_intercepted_water() depending on SW_VegProd.Type..., added forest_h2o to trace forest intercepted water
 renamed standcrop_h2o_qum -> veg_h2o_qum
 renamed totstcr_h2o -> totveg_h2o
 01/03/2011	(drs) changed type of lyrTrRegions[MAX_LAYERS] from int to IntU to avoid warning message that ' pointer targets differ in signedness' in function transp_weighted_avg()
 01/04/2011	(drs) added snowmelt to h2o_for_soil after interception, otherwise last day of snowmelt (when snowpack is gone) wasn't made available and aet became too small
 01/06/2011	(drs) layer drainage was incorrectly calculated if hydraulic redistribution pumped water into a layer below pwp such that its swc was finally higher than pwp -> fixed it by first calculating hydraulic redistribution and then as final step infiltrate_water_low()
 01/06/2011	(drs) call to infiltrate_water_low() has to be the last swc affecting calculation
 02/19/2011	(drs) calculate runoff as adjustment to snowmelt events before infiltration
 02/22/2011	(drs) init aet for the day in SW_Water_Flow(), instead implicitely in evap_litter_veg()
 02/22/2011	(drs) added snowloss to AET
 07/21/2011	(drs) added module variables 'lyrImpermeability' and 'lyrSWCBulk_Saturated' and initialize them in records2arrays()
 07/22/2011	(drs) adjusted soil_infiltration for pushed back water to surface (difference in standingWater)
 07/22/2011	(drs) included evaporation from standingWater into evap_litter_veg_surfaceWater() and reduce_rates_by_surfaceEvaporation(): it includes it to AET
 08/22/2011	(drs) in SW_Water_Flow(void): added snowdepth_scale = 1 - snow depth/vegetation height
 - vegetation interception = only when snowdepth_scale > 0, scaled by snowdepth_scale
 - litter interception = only when no snow cover
 - transpiration = only when snowdepth_scale > 0, scaled by snowdepth_scale
 - bare-soil evaporation = only when no snow cover
 09/08/2011	(drs) interception, evaporation from intercepted, E-T partitioning, transpiration, and hydraulic redistribution for each vegetation type (tree, shrub, grass) of SW_VegProd separately, scaled by their fractions
 replaced PET with unevaped in pot_soil_evap() and pot_transp(): to simulate reduction of atmospheric demand underneath canopies
 09/09/2011	(drs) moved transp_weighted_avg() from before infiltration and percolation to directly before call to pot_transp()
 09/21/2011	(drs)	scaled all (potential) evaporation and transpiration flux rates with PET: SW_Flow_lib.h: reduce_rates_by_unmetEvapDemand() is obsolete
 09/26/2011	(drs)	replaced all uses of monthly SW_VegProd and SW_Sky records with the daily replacements
 02/03/2012	(drs)	added variable 'snow_evap_rate': snow loss is part of aet and needs accordingly also to be scaled so that sum of all et rates is not more than pet
 01/28/2012	(drs)	transpiration can only remove water from soil down to lyrSWCBulk_Wiltpts (instead lyrSWCBulkmin)
 02/03/2012	(drs)	added 'lyrSWCBulk_HalfWiltpts' = 0.5 * SWC at -1.5 MPa
 soil evaporation extracts water down to 'lyrSWCBulk_HalfWiltpts' according to the FAO-56 model, e.g., Burt CM, Mutziger AJ, Allen RG, Howell TA (2005) Evaporation Research: Review and Interpretation. Journal of Irrigation and Drainage Engineering, 131, 37-58.
 02/04/2012	(drs)	added 'lyrSWCBulkatSWPcrit_xx' for each vegetation type
 transpiration can only remove water from soil down to 'lyrSWCBulkatSWPcrit_xx' (instead lyrSWCBulkmin)
 02/04/2012	(drs)	snow loss is fixed and can also include snow redistribution etc., so don't scale to PET
 05/25/2012  (DLM) added module level variables lyroldsTemp [MAX_LAYERS] & lyrsTemp [MAX_LAYERS] to keep track of soil temperatures, added lyrbDensity to keep track of the bulk density for each layer
 05/25/2012  (DLM) edited records2arrays(void); & arrays2records(void); functions to move values to / from lyroldsTemp & lyrTemp & lyrbDensity
 05/25/2012  (DLM) added call to soil_temperature function in SW_Water_Flow(void)
 11/06/2012	(clk)	added slope and aspect to the call to petfunc()
 11/30/2012	(clk)	added lines to calculate the surface runoff and to adjust the surface water level based on that value
 01/31/2013	(clk)	With the addition of a new type of vegetation, bare ground, needed to add a new function call to pot_soil_evap_bs() and create a few new variables:
 RealD lyrEvap_BareGround [MAX_LAYERS] was created,
 RealD soil_evap_rate_bs was created, and initialized at 1.0,
 new function, pot_soil_evap_bs(), is called if fractionBareGround is not zero and there is no snowpack on the ground,
 Added soil_evap_rate_bs to rate_help, and then adjusted soil_evap_rate_bs by rate_help if needed,
 Added call to remove bare-soil evap from swv,
 And added lyrEvap_BareGround into the calculation for SW_Soilwat.evaporation.
 Also, added SW_W_VegProd.bareGround_albedo*SW_VegProd.fractionBareGround to the paramater in petfunc() that was originally SW_VegProd.grass.albedo*SW_VegProd.fractionGrass + SW_VegProd.shrub.albedo*SW_VegProd.fractionShrub + SW_VegProd.tree.albedo*SW_VegProd.fractionTree
 04/16/2013	(clk)	Renamed a lot of the variables to better reflect BULK versus MATRIC values
 updated the use of these variables in all the files
 06/24/2013	(rjm)	added 'soil_temp_error', 'soil_temp_init' and 'fusion_pool_init' as global variable
 added function SW_FLW_construct() to init global variables between consecutive calls to SoilWat as dynamic library
 07/09/2013	(clk)	with the addition of forbs as a vegtype, needed to add a lot of calls to this code and so basically just copied and pasted the code for the other vegtypes
 09/26/2013 (drs) records2arrays(): Init hydraulic redistribution to zero; if not used and not initialized, then there could be non-zero values resulting
 */
/********************************************************/
/********************************************************/

/* =================================================== */
/*                INCLUDES / DEFINES                   */
/* --------------------------------------------------- */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "generic.h"
#include "filefuncs.h"
#include "SW_Defines.h"
#include "SW_Model.h"
#include "SW_Site.h"
#include "SW_SoilWater.h"
#include "SW_Flow_lib.h"
/*#include "SW_VegEstab.h" */
#include "SW_VegProd.h"
#include "SW_Weather.h"
#include "SW_Sky.h"

/* =================================================== */
/*                  Global Variables                   */
/* --------------------------------------------------- */
extern SW_MODEL SW_Model;
extern SW_SITE SW_Site;
extern SW_SOILWAT SW_Soilwat;
extern SW_WEATHER SW_Weather;
extern SW_VEGPROD SW_VegProd;
extern SW_SKY SW_Sky; // 

extern unsigned int soil_temp_error;  // simply keeps track of whether or not an error has been reported in the soil_temperature function.  0 for no, 1 for yes.
extern unsigned int soil_temp_init; // simply keeps track of whether or not the regression values for the soil_temperature function have been initialized.  0 for no, 1 for yes.
extern unsigned int fusion_pool_init;

/* *************************************************** */
/*                Module-Level Variables               */
/* --------------------------------------------------- */

/* temporary arrays for SoWat_flow_subs.c subroutines.
 * array indexing in those routines will be from
 * zero rather than 1.  see records2arrays().
 */
IntU lyrTrRegions_Forb[MAX_LAYERS], lyrTrRegions_Tree[MAX_LAYERS], lyrTrRegions_Shrub[MAX_LAYERS], lyrTrRegions_Grass[MAX_LAYERS];
RealD lyrSWCBulk[MAX_LAYERS], lyrDrain[MAX_LAYERS], lyrTransp_Forb[MAX_LAYERS], lyrTransp_Tree[MAX_LAYERS], lyrTransp_Shrub[MAX_LAYERS], lyrTransp_Grass[MAX_LAYERS],
		lyrTranspCo_Forb[MAX_LAYERS], lyrTranspCo_Tree[MAX_LAYERS], lyrTranspCo_Shrub[MAX_LAYERS], lyrTranspCo_Grass[MAX_LAYERS], lyrEvap_BareGround[MAX_LAYERS],
		lyrEvap_Forb[MAX_LAYERS], lyrEvap_Tree[MAX_LAYERS], lyrEvap_Shrub[MAX_LAYERS], lyrEvap_Grass[MAX_LAYERS], lyrEvapCo[MAX_LAYERS], lyrSWCBulk_FieldCaps[MAX_LAYERS],
		lyrWidths[MAX_LAYERS], lyrSWCBulk_Wiltpts[MAX_LAYERS], lyrSWCBulk_HalfWiltpts[MAX_LAYERS], lyrSWCBulk_Mins[MAX_LAYERS], lyrSWCBulk_atSWPcrit_Forb[MAX_LAYERS],
		lyrSWCBulk_atSWPcrit_Tree[MAX_LAYERS], lyrSWCBulk_atSWPcrit_Shrub[MAX_LAYERS], lyrSWCBulk_atSWPcrit_Grass[MAX_LAYERS], lyrpsisMatric[MAX_LAYERS],
		lyrthetasMatric[MAX_LAYERS], lyrBetasMatric[MAX_LAYERS], lyrBetaInvMatric[MAX_LAYERS], lyrSumTrCo[MAX_TRANSP_REGIONS + 1], lyrHydRed_Forb[MAX_LAYERS],
		lyrHydRed_Tree[MAX_LAYERS], lyrHydRed_Shrub[MAX_LAYERS], lyrHydRed_Grass[MAX_LAYERS], lyrImpermeability[MAX_LAYERS], lyrSWCBulk_Saturated[MAX_LAYERS],
		lyroldsTemp[MAX_LAYERS], lyrsTemp[MAX_LAYERS], lyrbDensity[MAX_LAYERS];

RealD drainout; /* h2o drained out of deepest layer */

static RealD forb_h2o_qum[TWO_DAYS], tree_h2o_qum[TWO_DAYS], shrub_h2o_qum[TWO_DAYS], grass_h2o_qum[TWO_DAYS], litter_h2o_qum[TWO_DAYS], standingWater[TWO_DAYS]; /* water on soil surface if layer below is saturated */


/* *************************************************** */
/* *************************************************** */
/*            Private functions                        */
/* --------------------------------------------------- */
static void records2arrays(void);
static void arrays2records(void);

/* *************************************************** */
/* *************************************************** */
/*             Public functions                        */
/* --------------------------------------------------- */
/* There is only one external function here and it is
 * only called from SW_Soilwat, so it is declared there.
 * but the compiler may complain if not predeclared here
 * This is a specific option for the compiler and may
 * not always occur.
 */

void SW_FLW_construct(void) {
	/* 06/26/2013	(rjm) added function SW_FLW_construct() to init global variables between consecutive calls to SoilWat as dynamic library */
	int i=0;
	soil_temp_error = 0;
	soil_temp_init = 0;
	fusion_pool_init = 0;
	//These only have to be cleared if a loop is wrong in the code.
	for (i = 0; i < MAX_LAYERS; i++) {
		lyrTrRegions_Forb[i] = lyrTrRegions_Tree[i] = lyrTrRegions_Shrub[i] = lyrTrRegions_Grass[i] = 0;
		lyrSWCBulk[i] = lyrDrain[i] = lyrTransp_Forb[i] = lyrTransp_Tree[i] = lyrTransp_Shrub[i] = lyrTransp_Grass[i] = 0;
		lyrTranspCo_Forb[i] = lyrTranspCo_Tree[i] = lyrTranspCo_Shrub[i] = lyrTranspCo_Grass[i] = lyrEvap_BareGround[i] = 0;
		lyrEvap_Forb[i] = lyrEvap_Tree[i] = lyrEvap_Shrub[i] = lyrEvap_Grass[i] = lyrEvapCo[i] = lyrSWCBulk_FieldCaps[i] = 0;
		lyrWidths[i] = lyrSWCBulk_Wiltpts[i] = lyrSWCBulk_HalfWiltpts[i] = lyrSWCBulk_Mins[i] = lyrSWCBulk_atSWPcrit_Forb[i] = 0;
		lyrSWCBulk_atSWPcrit_Tree[i] = lyrSWCBulk_atSWPcrit_Shrub[i] = lyrSWCBulk_atSWPcrit_Grass[i] = lyrpsisMatric[i] = 0;
		lyrthetasMatric[i] = lyrBetasMatric[i] = lyrBetaInvMatric[i] = lyrSumTrCo[MAX_TRANSP_REGIONS + 1] = lyrHydRed_Forb[i] = 0;
		lyrHydRed_Tree[i] = lyrHydRed_Shrub[i] = lyrHydRed_Grass[i] = lyrImpermeability[i] = lyrSWCBulk_Saturated[i] = 0;
		lyroldsTemp[i] = lyrsTemp[i] = lyrbDensity[i] = 0;
	}
	//When running as a library make sure these are set to zero.
	drainout = 0;
	forb_h2o_qum[0]=tree_h2o_qum[0]=shrub_h2o_qum[0]=grass_h2o_qum[0]=litter_h2o_qum[0]=standingWater[0]=0;
	forb_h2o_qum[1]=tree_h2o_qum[1]=shrub_h2o_qum[1]=grass_h2o_qum[1]=litter_h2o_qum[1]=standingWater[1]=0;
}

void SW_Water_Flow(void);

/* *************************************************** */
/* *************************************************** */
/*            The Water Flow                           */
/* --------------------------------------------------- */
void SW_Water_Flow(void) {

	RealD swpot_avg_forb, swpot_avg_tree, swpot_avg_shrub, swpot_avg_grass, soil_evap_forb, soil_evap_tree, soil_evap_shrub, soil_evap_grass, soil_evap_rate_forb = 1.,
			soil_evap_rate_tree = 1., soil_evap_rate_shrub = 1., soil_evap_rate_grass = 1., soil_evap_rate_bs = 1., transp_forb, transp_tree, transp_shrub, transp_grass,
			transp_rate_forb = 1., transp_rate_tree = 1., transp_rate_shrub = 1., transp_rate_grass = 1., snow_evap_rate, surface_evap_forb_rate, surface_evap_tree_rate,
			surface_evap_shrub_rate, surface_evap_grass_rate, surface_evap_litter_rate, surface_evap_standingWater_rate, grass_h2o, shrub_h2o, tree_h2o, forb_h2o, litter_h2o,
			litter_h2o_help, surface_h2o, h2o_for_soil = 0., ppt_toUse, snowmelt, snowdepth_scale_grass = 1., snowdepth_scale_shrub = 1., snowdepth_scale_tree = 1.,
			snowdepth_scale_forb = 1., rate_help;

	int doy;

	doy = SW_Model.doy; /* base1 */
	/*	month = SW_Model.month;*//* base0 */

	records2arrays();

	/* snowdepth scaling */
	SW_Soilwat.snowdepth = SW_SnowDepth(SW_Soilwat.snowpack[Today], SW_Sky.snow_density_daily[doy]);
	/* if snow depth is deeper than vegetation height then
	 - rain and snowmelt infiltrates directly to soil (no vegetation or litter interception of today)
	 only
	 - evaporation of yesterdays interception
	 - infiltrate water high
	 - infiltrate water low */

	if (GT(SW_VegProd.grass.veg_height_daily[doy], 0.)) {
		snowdepth_scale_grass = 1. - SW_Soilwat.snowdepth / SW_VegProd.grass.veg_height_daily[doy];
	} else {
		snowdepth_scale_grass = 1.;
	}
	if (GT(SW_VegProd.forb.veg_height_daily[doy], 0.)) {
		snowdepth_scale_forb = 1. - SW_Soilwat.snowdepth / SW_VegProd.forb.veg_height_daily[doy];
	} else {
		snowdepth_scale_forb = 1.;
	}
	if (GT(SW_VegProd.shrub.veg_height_daily[doy], 0.)) {
		snowdepth_scale_shrub = 1. - SW_Soilwat.snowdepth / SW_VegProd.shrub.veg_height_daily[doy];
	} else {
		snowdepth_scale_shrub = 1.;
	}
	if (GT(SW_VegProd.tree.veg_height_daily[doy], 0.)) {
		snowdepth_scale_tree = 1. - SW_Soilwat.snowdepth / SW_VegProd.tree.veg_height_daily[doy];
	} else {
		snowdepth_scale_tree = 1.;
	}

	/* Interception */
	ppt_toUse = SW_Weather.now.rain[Today]; /* ppt is partioned into ppt = snow + rain */
	if (GT(SW_VegProd.fractionTree, 0.) && GT(snowdepth_scale_tree, 0.)) { /* trees present AND trees not fully covered in snow */
		tree_intercepted_water(&h2o_for_soil, &tree_h2o, ppt_toUse, SW_VegProd.tree.lai_live_daily[doy], snowdepth_scale_tree * SW_VegProd.fractionTree,
				SW_VegProd.tree.veg_intPPT_a, SW_VegProd.tree.veg_intPPT_b, SW_VegProd.tree.veg_intPPT_c, SW_VegProd.tree.veg_intPPT_d);
		ppt_toUse = h2o_for_soil; /* amount of rain that is not intercepted by the forest canopy */
	} else { /* snow depth is more than vegetation height  */
		h2o_for_soil = ppt_toUse;
		tree_h2o = 0.;
	} /* end forest interception */

	if (GT(SW_VegProd.fractionShrub, 0.) && GT(snowdepth_scale_shrub, 0.)) {
		shrub_intercepted_water(&h2o_for_soil, &shrub_h2o, ppt_toUse, SW_VegProd.shrub.vegcov_daily[doy], snowdepth_scale_shrub * SW_VegProd.fractionShrub,
				SW_VegProd.shrub.veg_intPPT_a, SW_VegProd.shrub.veg_intPPT_b, SW_VegProd.shrub.veg_intPPT_c, SW_VegProd.shrub.veg_intPPT_d);
		ppt_toUse = h2o_for_soil; /* amount of rain that is not intercepted by the shrub canopy */
	} else {
		shrub_h2o = 0.;
	} /* end shrub interception */

	if (GT(SW_VegProd.fractionForb, 0.) && GT(snowdepth_scale_forb, 0.)) { /* forbs present AND not fully covered in snow */
		forb_intercepted_water(&h2o_for_soil, &forb_h2o, ppt_toUse, SW_VegProd.forb.vegcov_daily[doy], snowdepth_scale_forb * SW_VegProd.fractionForb,
				SW_VegProd.forb.veg_intPPT_a, SW_VegProd.forb.veg_intPPT_b, SW_VegProd.forb.veg_intPPT_c, SW_VegProd.forb.veg_intPPT_d);
		ppt_toUse = h2o_for_soil; /* amount of rain that is not intercepted by the forbs */
	} else { /* snow depth is more than vegetation height  */
		forb_h2o = 0.;

	} /* end forb interception */

	if (GT(SW_VegProd.fractionGrass, 0.) && GT(snowdepth_scale_grass, 0.)) {
		grass_intercepted_water(&h2o_for_soil, &grass_h2o, ppt_toUse, SW_VegProd.grass.vegcov_daily[doy], snowdepth_scale_grass * SW_VegProd.fractionGrass,
				SW_VegProd.grass.veg_intPPT_a, SW_VegProd.grass.veg_intPPT_b, SW_VegProd.grass.veg_intPPT_c, SW_VegProd.grass.veg_intPPT_d);
	} else {
		grass_h2o = 0.;
	} /* end grass interception */

	if (EQ(SW_Soilwat.snowpack[Today], 0.)) { /* litter interception only when no snow */
		litter_h2o_help = 0.;

		if (GT(SW_VegProd.fractionTree, 0.)) {
			litter_intercepted_water(&h2o_for_soil, &litter_h2o, SW_VegProd.tree.litter_daily[doy], SW_VegProd.fractionTree, SW_VegProd.tree.litt_intPPT_a,
					SW_VegProd.tree.litt_intPPT_b, SW_VegProd.tree.litt_intPPT_c, SW_VegProd.tree.litt_intPPT_d);
			litter_h2o_help += litter_h2o;
		}

		if (GT(SW_VegProd.fractionShrub, 0.)) {
			litter_intercepted_water(&h2o_for_soil, &litter_h2o, SW_VegProd.shrub.litter_daily[doy], SW_VegProd.fractionShrub, SW_VegProd.shrub.litt_intPPT_a,
					SW_VegProd.shrub.litt_intPPT_b, SW_VegProd.shrub.litt_intPPT_c, SW_VegProd.shrub.litt_intPPT_d);
			litter_h2o_help += litter_h2o;
		}

		if (GT(SW_VegProd.fractionForb, 0.)) {
			litter_intercepted_water(&h2o_for_soil, &litter_h2o, SW_VegProd.forb.litter_daily[doy], SW_VegProd.fractionForb, SW_VegProd.forb.litt_intPPT_a,
					SW_VegProd.forb.litt_intPPT_b, SW_VegProd.forb.litt_intPPT_c, SW_VegProd.forb.litt_intPPT_d);
			litter_h2o_help += litter_h2o;
		}

		if (GT(SW_VegProd.fractionGrass, 0.)) {
			litter_intercepted_water(&h2o_for_soil, &litter_h2o, SW_VegProd.grass.litter_daily[doy], SW_VegProd.fractionGrass, SW_VegProd.grass.litt_intPPT_a,
					SW_VegProd.grass.litt_intPPT_b, SW_VegProd.grass.litt_intPPT_c, SW_VegProd.grass.litt_intPPT_d);
			litter_h2o_help += litter_h2o;
		}

		litter_h2o = litter_h2o_help;
	} else {
		litter_h2o = 0.;
	}

	/* Sum cumulative intercepted components */
	SW_Soilwat.tree_int = tree_h2o;
	SW_Soilwat.shrub_int = shrub_h2o;
	SW_Soilwat.forb_int = forb_h2o;
	SW_Soilwat.grass_int = grass_h2o;
	SW_Soilwat.litter_int = litter_h2o;

	tree_h2o_qum[Today] = tree_h2o_qum[Yesterday] + tree_h2o;
	shrub_h2o_qum[Today] = shrub_h2o_qum[Yesterday] + shrub_h2o;
	forb_h2o_qum[Today] = forb_h2o_qum[Yesterday] + forb_h2o;
	grass_h2o_qum[Today] = grass_h2o_qum[Yesterday] + grass_h2o;
	litter_h2o_qum[Today] = litter_h2o_qum[Yesterday] + litter_h2o;
	/* End Interception */

	/* Surface water */
	standingWater[Today] = standingWater[Yesterday];

	/* Soil infiltration = rain+snowmelt - interception, but should be = rain+snowmelt - interception + (throughfall+stemflow) */
	surface_h2o = standingWater[Today];
	snowmelt = SW_Weather.now.snowmelt[Today];
	snowmelt = fmax( 0., snowmelt * (1. - SW_Weather.pct_snowRunoff/100.) ); /* amount of snowmelt is changed by runon/off as percentage */
	SW_Weather.snowRunoff = SW_Weather.now.snowmelt[Today] - snowmelt;
	h2o_for_soil += snowmelt; /* if there is snowmelt, it goes un-intercepted to the soil */
	h2o_for_soil += surface_h2o;
	SW_Weather.soil_inf = h2o_for_soil;

	/* Percolation for saturated soil conditions */
	infiltrate_water_high(lyrSWCBulk, lyrDrain, &drainout, h2o_for_soil, SW_Site.n_layers, lyrSWCBulk_FieldCaps, lyrSWCBulk_Saturated, lyrImpermeability,
			&standingWater[Today]);

	SW_Weather.soil_inf -= standingWater[Today]; /* adjust soil_infiltration for pushed back or infiltrated surface water */

	/* Surface water runoff */
	SW_Weather.surfaceRunoff = standingWater[Today] * SW_Site.percentRunoff;
	standingWater[Today] = fmax(0.0, (standingWater[Today] - SW_Weather.surfaceRunoff));
	surface_h2o = standingWater[Today];

	/* PET */
	SW_Soilwat.pet = SW_Site.pet_scale
			* petfunc(doy, SW_Weather.now.temp_avg[Today], SW_Site.latitude, SW_Site.altitude, SW_Site.slope, SW_Site.aspect,
					SW_VegProd.grass.albedo * SW_VegProd.fractionGrass + SW_VegProd.shrub.albedo * SW_VegProd.fractionShrub + SW_VegProd.forb.albedo * SW_VegProd.fractionForb
							+ SW_VegProd.tree.albedo * SW_VegProd.fractionTree + SW_VegProd.bareGround_albedo * SW_VegProd.fractionBareGround, SW_Sky.r_humidity_daily[doy],
					SW_Sky.windspeed_daily[doy], SW_Sky.cloudcov_daily[doy], SW_Sky.transmission_daily[doy]);

	/* Bare-soil evaporation rates */
	if (GT(SW_VegProd.fractionBareGround, 0.) && EQ(SW_Soilwat.snowpack[Today], 0.)) /* bare ground present AND no snow on ground */
	{
		pot_soil_evap_bs(&soil_evap_rate_bs, SW_Site.n_evap_lyrs, lyrEvapCo, SW_Soilwat.pet, SW_Site.evap.xinflec, SW_Site.evap.slope, SW_Site.evap.yinflec,
				SW_Site.evap.range, lyrWidths, lyrSWCBulk);
		soil_evap_rate_bs *= SW_VegProd.fractionBareGround;
	} else {
		soil_evap_rate_bs = 0;
	}

	/* Tree transpiration & bare-soil evaporation rates */
	if (GT(SW_VegProd.fractionTree, 0.) && GT(snowdepth_scale_tree, 0.)) { /* trees present AND trees not fully covered in snow */
		tree_EsT_partitioning(&soil_evap_tree, &transp_tree, SW_VegProd.tree.lai_live_daily[doy], SW_VegProd.tree.EsTpartitioning_param);

		if (EQ(SW_Soilwat.snowpack[Today], 0.)) { /* bare-soil evaporation only when no snow */
			pot_soil_evap(&soil_evap_rate_tree, SW_Site.n_evap_lyrs, lyrEvapCo, SW_VegProd.tree.total_agb_daily[doy], soil_evap_tree, SW_Soilwat.pet, SW_Site.evap.xinflec,
					SW_Site.evap.slope, SW_Site.evap.yinflec, SW_Site.evap.range, lyrWidths, lyrSWCBulk, SW_VegProd.tree.Es_param_limit);
			soil_evap_rate_tree *= SW_VegProd.fractionTree;
		} else {
			soil_evap_rate_tree = 0.;
		}

		transp_weighted_avg(&swpot_avg_tree, SW_Site.n_transp_rgn, SW_Site.n_transp_lyrs_tree, lyrTrRegions_Tree, lyrTranspCo_Tree, lyrSWCBulk);

		pot_transp(&transp_rate_tree, swpot_avg_tree, SW_VegProd.tree.biolive_daily[doy], SW_VegProd.tree.biodead_daily[doy], transp_tree, SW_Soilwat.pet,
				SW_Site.transp.xinflec, SW_Site.transp.slope, SW_Site.transp.yinflec, SW_Site.transp.range, SW_VegProd.tree.shade_scale, SW_VegProd.tree.shade_deadmax,
				SW_VegProd.tree.tr_shade_effects.xinflec, SW_VegProd.tree.tr_shade_effects.slope, SW_VegProd.tree.tr_shade_effects.yinflec,
				SW_VegProd.tree.tr_shade_effects.range);
		transp_rate_tree *= snowdepth_scale_tree * SW_VegProd.fractionTree;
	} else {
		soil_evap_rate_tree = 0.;
		transp_rate_tree = 0.;
	}

	/* Shrub transpiration & bare-soil evaporation rates */
	if (GT(SW_VegProd.fractionShrub, 0.) && GT(snowdepth_scale_shrub, 0.)) { /* shrubs present AND shrubs not fully covered in snow */
		shrub_EsT_partitioning(&soil_evap_shrub, &transp_shrub, SW_VegProd.shrub.lai_live_daily[doy], SW_VegProd.shrub.EsTpartitioning_param);

		if (EQ(SW_Soilwat.snowpack[Today], 0.)) { /* bare-soil evaporation only when no snow */
			pot_soil_evap(&soil_evap_rate_shrub, SW_Site.n_evap_lyrs, lyrEvapCo, SW_VegProd.shrub.total_agb_daily[doy], soil_evap_shrub, SW_Soilwat.pet, SW_Site.evap.xinflec,
					SW_Site.evap.slope, SW_Site.evap.yinflec, SW_Site.evap.range, lyrWidths, lyrSWCBulk, SW_VegProd.shrub.Es_param_limit);
			soil_evap_rate_shrub *= SW_VegProd.fractionShrub;
		} else {
			soil_evap_rate_shrub = 0.;
		}

		transp_weighted_avg(&swpot_avg_shrub, SW_Site.n_transp_rgn, SW_Site.n_transp_lyrs_shrub, lyrTrRegions_Shrub, lyrTranspCo_Shrub, lyrSWCBulk);

		pot_transp(&transp_rate_shrub, swpot_avg_shrub, SW_VegProd.shrub.biolive_daily[doy], SW_VegProd.shrub.biodead_daily[doy], transp_shrub, SW_Soilwat.pet,
				SW_Site.transp.xinflec, SW_Site.transp.slope, SW_Site.transp.yinflec, SW_Site.transp.range, SW_VegProd.shrub.shade_scale, SW_VegProd.shrub.shade_deadmax,
				SW_VegProd.shrub.tr_shade_effects.xinflec, SW_VegProd.shrub.tr_shade_effects.slope, SW_VegProd.shrub.tr_shade_effects.yinflec,
				SW_VegProd.shrub.tr_shade_effects.range);
		transp_rate_shrub *= snowdepth_scale_shrub * SW_VegProd.fractionShrub;

	} else {
		soil_evap_rate_shrub = 0.;
		transp_rate_shrub = 0.;
	}

	/* Forb transpiration & bare-soil evaporation rates */
	if (GT(SW_VegProd.fractionForb, 0.) && GT(snowdepth_scale_forb, 0.)) { /* forbs present AND forbs not fully covered in snow */
		forb_EsT_partitioning(&soil_evap_forb, &transp_forb, SW_VegProd.forb.lai_live_daily[doy], SW_VegProd.forb.EsTpartitioning_param);

		if (EQ(SW_Soilwat.snowpack[Today], 0.)) { /* bare-soil evaporation only when no snow */
			pot_soil_evap(&soil_evap_rate_forb, SW_Site.n_evap_lyrs, lyrEvapCo, SW_VegProd.forb.total_agb_daily[doy], soil_evap_forb, SW_Soilwat.pet, SW_Site.evap.xinflec,
					SW_Site.evap.slope, SW_Site.evap.yinflec, SW_Site.evap.range, lyrWidths, lyrSWCBulk, SW_VegProd.forb.Es_param_limit);
			soil_evap_rate_forb *= SW_VegProd.fractionForb;
		} else {
			soil_evap_rate_forb = 0.;
		}

		transp_weighted_avg(&swpot_avg_forb, SW_Site.n_transp_rgn, SW_Site.n_transp_lyrs_forb, lyrTrRegions_Forb, lyrTranspCo_Forb, lyrSWCBulk);

		pot_transp(&transp_rate_forb, swpot_avg_forb, SW_VegProd.forb.biolive_daily[doy], SW_VegProd.forb.biodead_daily[doy], transp_forb, SW_Soilwat.pet,
				SW_Site.transp.xinflec, SW_Site.transp.slope, SW_Site.transp.yinflec, SW_Site.transp.range, SW_VegProd.forb.shade_scale, SW_VegProd.forb.shade_deadmax,
				SW_VegProd.forb.tr_shade_effects.xinflec, SW_VegProd.forb.tr_shade_effects.slope, SW_VegProd.forb.tr_shade_effects.yinflec,
				SW_VegProd.forb.tr_shade_effects.range);
		transp_rate_forb *= snowdepth_scale_forb * SW_VegProd.fractionForb;

	} else {
		soil_evap_rate_forb = 0.;
		transp_rate_forb = 0.;
	}

	/* Grass transpiration & bare-soil evaporation rates */
	if (GT(SW_VegProd.fractionGrass, 0.) && GT(snowdepth_scale_grass, 0.)) { /* grasses present AND grasses not fully covered in snow */
		grass_EsT_partitioning(&soil_evap_grass, &transp_grass, SW_VegProd.grass.lai_live_daily[doy], SW_VegProd.grass.EsTpartitioning_param);

		if (EQ(SW_Soilwat.snowpack[Today], 0.)) { /* bare-soil evaporation only when no snow */
			pot_soil_evap(&soil_evap_rate_grass, SW_Site.n_evap_lyrs, lyrEvapCo, SW_VegProd.grass.total_agb_daily[doy], soil_evap_grass, SW_Soilwat.pet, SW_Site.evap.xinflec,
					SW_Site.evap.slope, SW_Site.evap.yinflec, SW_Site.evap.range, lyrWidths, lyrSWCBulk, SW_VegProd.grass.Es_param_limit);
			soil_evap_rate_grass *= SW_VegProd.fractionGrass;
		} else {
			soil_evap_rate_grass = 0.;
		}

		transp_weighted_avg(&swpot_avg_grass, SW_Site.n_transp_rgn, SW_Site.n_transp_lyrs_grass, lyrTrRegions_Grass, lyrTranspCo_Grass, lyrSWCBulk);

		pot_transp(&transp_rate_grass, swpot_avg_grass, SW_VegProd.grass.biolive_daily[doy], SW_VegProd.grass.biodead_daily[doy], transp_grass, SW_Soilwat.pet,
				SW_Site.transp.xinflec, SW_Site.transp.slope, SW_Site.transp.yinflec, SW_Site.transp.range, SW_VegProd.grass.shade_scale, SW_VegProd.grass.shade_deadmax,
				SW_VegProd.grass.tr_shade_effects.xinflec, SW_VegProd.grass.tr_shade_effects.slope, SW_VegProd.grass.tr_shade_effects.yinflec,
				SW_VegProd.grass.tr_shade_effects.range);
		transp_rate_grass *= snowdepth_scale_grass * SW_VegProd.fractionGrass;
	} else {
		soil_evap_rate_grass = 0.;
		transp_rate_grass = 0.;
	}

	/* Potential evaporation rates of intercepted and surface water */
	surface_evap_tree_rate = tree_h2o_qum[Today];
	surface_evap_shrub_rate = shrub_h2o_qum[Today];
	surface_evap_forb_rate = forb_h2o_qum[Today];
	surface_evap_grass_rate = grass_h2o_qum[Today];
	surface_evap_litter_rate = litter_h2o_qum[Today];
	surface_evap_standingWater_rate = standingWater[Today];
	snow_evap_rate = SW_Weather.now.snowloss[Today]; /* but this is fixed and can also include snow redistribution etc., so don't scale to PET */

	/* Scale all (potential) evaporation and transpiration flux rates to PET */
	rate_help = surface_evap_tree_rate + surface_evap_forb_rate + surface_evap_shrub_rate + surface_evap_grass_rate + surface_evap_litter_rate
			+ surface_evap_standingWater_rate + soil_evap_rate_tree + transp_rate_tree + soil_evap_rate_forb + transp_rate_forb + soil_evap_rate_shrub + transp_rate_shrub
			+ soil_evap_rate_grass + transp_rate_grass + soil_evap_rate_bs;

	if (GT(rate_help, SW_Soilwat.pet)) {
		rate_help = SW_Soilwat.pet / rate_help;

		surface_evap_tree_rate *= rate_help;
		surface_evap_forb_rate *= rate_help;
		surface_evap_shrub_rate *= rate_help;
		surface_evap_grass_rate *= rate_help;
		surface_evap_litter_rate *= rate_help;
		surface_evap_standingWater_rate *= rate_help;
		soil_evap_rate_tree *= rate_help;
		transp_rate_tree *= rate_help;
		soil_evap_rate_forb *= rate_help;
		transp_rate_forb *= rate_help;
		soil_evap_rate_shrub *= rate_help;
		transp_rate_shrub *= rate_help;
		soil_evap_rate_grass *= rate_help;
		transp_rate_grass *= rate_help;
		soil_evap_rate_bs *= rate_help;
	}

	/* Start adding components to AET */
	SW_Soilwat.aet = 0.; /* init aet for the day */
	SW_Soilwat.aet += snow_evap_rate;

	/* Evaporation of intercepted and surface water */
	evap_fromSurface(&tree_h2o_qum[Today], &surface_evap_tree_rate, &SW_Soilwat.aet);
	evap_fromSurface(&shrub_h2o_qum[Today], &surface_evap_shrub_rate, &SW_Soilwat.aet);
	evap_fromSurface(&forb_h2o_qum[Today], &surface_evap_forb_rate, &SW_Soilwat.aet);
	evap_fromSurface(&grass_h2o_qum[Today], &surface_evap_grass_rate, &SW_Soilwat.aet);
	evap_fromSurface(&litter_h2o_qum[Today], &surface_evap_litter_rate, &SW_Soilwat.aet);
	evap_fromSurface(&standingWater[Today], &surface_evap_standingWater_rate, &SW_Soilwat.aet);

	SW_Soilwat.tree_evap = surface_evap_tree_rate;
	SW_Soilwat.shrub_evap = surface_evap_shrub_rate;
	SW_Soilwat.forb_evap = surface_evap_forb_rate;
	SW_Soilwat.grass_evap = surface_evap_grass_rate;
	SW_Soilwat.litter_evap = surface_evap_litter_rate;
	SW_Soilwat.surfaceWater_evap = surface_evap_standingWater_rate;

	/* bare-soil evaporation */
	if (GT(SW_VegProd.fractionBareGround, 0.) && EQ(SW_Soilwat.snowpack[Today], 0.)) {
		/* remove bare-soil evap from swv */
		remove_from_soil(lyrSWCBulk, lyrEvap_BareGround, &SW_Soilwat.aet, SW_Site.n_evap_lyrs, lyrEvapCo, soil_evap_rate_bs, lyrSWCBulk_HalfWiltpts);
	} else {
		/* Set daily array to zero, no evaporation */
		LyrIndex i;
		for (i = 0; i < SW_Site.n_evap_lyrs;)
			lyrEvap_BareGround[i++] = 0.;
	}

	/* Tree transpiration and bare-soil evaporation */
	if (GT(SW_VegProd.fractionTree, 0.) && GT(snowdepth_scale_tree, 0.)) {
		/* remove bare-soil evap from swc */
		remove_from_soil(lyrSWCBulk, lyrEvap_Tree, &SW_Soilwat.aet, SW_Site.n_evap_lyrs, lyrEvapCo, soil_evap_rate_tree, lyrSWCBulk_HalfWiltpts);

		/* remove transp from swc */
		remove_from_soil(lyrSWCBulk, lyrTransp_Tree, &SW_Soilwat.aet, SW_Site.n_transp_lyrs_tree, lyrTranspCo_Tree, transp_rate_tree, lyrSWCBulk_atSWPcrit_Tree);
	} else {
		/* Set daily array to zero, no evaporation or transpiration */
		LyrIndex i;
		for (i = 0; i < SW_Site.n_evap_lyrs;)
			lyrEvap_Tree[i++] = 0.;
		for (i = 0; i < SW_Site.n_transp_lyrs_tree;)
			lyrTransp_Tree[i++] = 0.;
	}

	/* Shrub transpiration and bare-soil evaporation */
	if (GT(SW_VegProd.fractionShrub, 0.) && GT(snowdepth_scale_shrub, 0.)) {
		/* remove bare-soil evap from swc */
		remove_from_soil(lyrSWCBulk, lyrEvap_Shrub, &SW_Soilwat.aet, SW_Site.n_evap_lyrs, lyrEvapCo, soil_evap_rate_shrub, lyrSWCBulk_HalfWiltpts);

		/* remove transp from swc */
		remove_from_soil(lyrSWCBulk, lyrTransp_Shrub, &SW_Soilwat.aet, SW_Site.n_transp_lyrs_shrub, lyrTranspCo_Shrub, transp_rate_shrub, lyrSWCBulk_atSWPcrit_Shrub);
	} else {
		/* Set daily array to zero, no evaporation or transpiration */
		LyrIndex i;
		for (i = 0; i < SW_Site.n_evap_lyrs;)
			lyrEvap_Shrub[i++] = 0.;
		for (i = 0; i < SW_Site.n_transp_lyrs_shrub;)
			lyrTransp_Shrub[i++] = 0.;
	}

	/* Forb transpiration and bare-soil evaporation */
	if (GT(SW_VegProd.fractionForb, 0.) && GT(snowdepth_scale_forb, 0.)) {
		/* remove bare-soil evap from swc */
		remove_from_soil(lyrSWCBulk, lyrEvap_Forb, &SW_Soilwat.aet, SW_Site.n_evap_lyrs, lyrEvapCo, soil_evap_rate_forb, lyrSWCBulk_HalfWiltpts);

		/* remove transp from swc */
		remove_from_soil(lyrSWCBulk, lyrTransp_Forb, &SW_Soilwat.aet, SW_Site.n_transp_lyrs_forb, lyrTranspCo_Forb, transp_rate_forb, lyrSWCBulk_atSWPcrit_Forb);
	} else {
		/* Set daily array to zero, no evaporation or transpiration */
		LyrIndex i;
		for (i = 0; i < SW_Site.n_evap_lyrs;)
			lyrEvap_Forb[i++] = 0.;
		for (i = 0; i < SW_Site.n_transp_lyrs_forb;)
			lyrTransp_Forb[i++] = 0.;
	}

	/* Grass transpiration & bare-soil evaporation */
	if (GT(SW_VegProd.fractionGrass, 0.) && GT(snowdepth_scale_grass, 0.)) {
		/* remove bare-soil evap from swc */
		remove_from_soil(lyrSWCBulk, lyrEvap_Grass, &SW_Soilwat.aet, SW_Site.n_evap_lyrs, lyrEvapCo, soil_evap_rate_grass, lyrSWCBulk_HalfWiltpts);

		/* remove transp from swc */
		remove_from_soil(lyrSWCBulk, lyrTransp_Grass, &SW_Soilwat.aet, SW_Site.n_transp_lyrs_grass, lyrTranspCo_Grass, transp_rate_grass, lyrSWCBulk_atSWPcrit_Grass);
	} else {
		/* Set daily array to zero, no evaporation or transpiration */
		LyrIndex i;
		for (i = 0; i < SW_Site.n_evap_lyrs;)
			lyrEvap_Grass[i++] = 0.;
		for (i = 0; i < SW_Site.n_transp_lyrs_grass;)
			lyrTransp_Grass[i++] = 0.;
	}

	/* Hydraulic redistribution */
	if (SW_VegProd.grass.flagHydraulicRedistribution && GT(SW_VegProd.fractionGrass, 0.) && GT(SW_VegProd.grass.biolive_daily[doy], 0.)) {
		hydraulic_redistribution(lyrSWCBulk, lyrSWCBulk_Wiltpts, lyrTranspCo_Grass, lyrHydRed_Grass, SW_Site.n_layers, SW_VegProd.grass.maxCondroot,
				SW_VegProd.grass.swpMatric50, SW_VegProd.grass.shapeCond, SW_VegProd.fractionGrass);
	}
	if (SW_VegProd.forb.flagHydraulicRedistribution && GT(SW_VegProd.fractionForb, 0.) && GT(SW_VegProd.forb.biolive_daily[doy], 0.)) {
		hydraulic_redistribution(lyrSWCBulk, lyrSWCBulk_Wiltpts, lyrTranspCo_Forb, lyrHydRed_Forb, SW_Site.n_layers, SW_VegProd.forb.maxCondroot, SW_VegProd.forb.swpMatric50,
				SW_VegProd.forb.shapeCond, SW_VegProd.fractionForb);
	}
	if (SW_VegProd.shrub.flagHydraulicRedistribution && GT(SW_VegProd.fractionShrub, 0.) && GT(SW_VegProd.shrub.biolive_daily[doy], 0.)) {
		hydraulic_redistribution(lyrSWCBulk, lyrSWCBulk_Wiltpts, lyrTranspCo_Shrub, lyrHydRed_Shrub, SW_Site.n_layers, SW_VegProd.shrub.maxCondroot,
				SW_VegProd.shrub.swpMatric50, SW_VegProd.shrub.shapeCond, SW_VegProd.fractionShrub);
	}
	if (SW_VegProd.tree.flagHydraulicRedistribution && GT(SW_VegProd.fractionTree, 0.) && GT(SW_VegProd.tree.biolive_daily[doy], 0.)) {
		hydraulic_redistribution(lyrSWCBulk, lyrSWCBulk_Wiltpts, lyrTranspCo_Tree, lyrHydRed_Tree, SW_Site.n_layers, SW_VegProd.tree.maxCondroot, SW_VegProd.tree.swpMatric50,
				SW_VegProd.tree.shapeCond, SW_VegProd.fractionTree);
	}

	/* Calculate percolation for unsaturated soil water conditions. */
	/* 01/06/2011	(drs) call to infiltrate_water_low() has to be the last swc affecting calculation */

	infiltrate_water_low(lyrSWCBulk, lyrDrain, &drainout, SW_Site.n_layers, SW_Site.slow_drain_coeff, SLOW_DRAIN_DEPTH, lyrSWCBulk_FieldCaps, lyrWidths, lyrSWCBulk_Mins,
			lyrSWCBulk_Saturated, lyrImpermeability, &standingWater[Today]);

	SW_Soilwat.surfaceWater = standingWater[Today];

	/* Soil Temperature starts here */

	double biomass; // computing the standing crop biomass real quickly to condense the call to soil_temperature
	biomass = SW_VegProd.grass.biomass_daily[doy] * SW_VegProd.fractionGrass + SW_VegProd.shrub.biomass_daily[doy] * SW_VegProd.fractionShrub
			+ SW_VegProd.forb.biomass_daily[doy] * SW_VegProd.fractionForb + SW_VegProd.tree.biolive_daily[doy] * SW_VegProd.fractionTree; // changed to exclude tree biomass, bMatric/c it was breaking the soil_temperature function

			// soil_temperature function computes the soil temp for each layer and stores it in lyrsTemp
			// doesn't affect SWC at all, but needs it for the calculation, so therefore the temperature is the last calculation done
	if (SW_Site.use_soil_temp)
		soil_temperature(SW_Weather.now.temp_avg[Today], SW_Soilwat.pet, SW_Soilwat.aet, biomass, lyrSWCBulk, lyrbDensity, lyrWidths, lyroldsTemp, lyrsTemp, SW_Site.n_layers,
				lyrSWCBulk_FieldCaps, lyrSWCBulk_Wiltpts, SW_Site.bmLimiter, SW_Site.t1Param1, SW_Site.t1Param2, SW_Site.t1Param3, SW_Site.csParam1, SW_Site.csParam2,
				SW_Site.shParam, SW_Soilwat.snowpack[Today], SW_Site.meanAirTemp /*SW_Weather.hist.temp_year_avg*/, SW_Site.stDeltaX, SW_Site.stMaxDepth, SW_Site.stNRGR);

	/* Soil Temperature ends here */

	/* Move local values into main arrays */
	arrays2records();

	standingWater[Yesterday] = standingWater[Today];
	litter_h2o_qum[Yesterday] = litter_h2o_qum[Today];
	tree_h2o_qum[Yesterday] = tree_h2o_qum[Today];
	shrub_h2o_qum[Yesterday] = shrub_h2o_qum[Today];
	forb_h2o_qum[Yesterday] = forb_h2o_qum[Today];
	grass_h2o_qum[Yesterday] = grass_h2o_qum[Today];

} /* END OF WATERFLOW */

static void records2arrays(void) {
	/* some values are unchanged by the water subs but
	 * are still required in an array format.
	 * Also, some arrays start out empty and are
	 * filled during the water flow.
	 * See arrays2records() for the modified arrays.
	 *
	 * 3/24/2003 - cwb - when running with steppe, the
	 *       static variable firsttime would only be set once
	 *       so the firsttime tasks were done only the first
	 *       year, but what we really want with stepwat is
	 *       to firsttime tasks on the first day of each year.
	 * 1-Oct-03 (cwb) - Removed references to sum_transp_coeff.
	 *       see also Site.c.
	 */
	LyrIndex i;

	ForEachSoilLayer(i)
	{
		lyrSWCBulk[i] = SW_Soilwat.swcBulk[Today][i];
		lyroldsTemp[i] = SW_Soilwat.sTemp[i];
	}

	if (SW_Model.doy == SW_Model.firstdoy) {
		ForEachSoilLayer(i)
		{
			lyrTrRegions_Tree[i] = SW_Site.lyr[i]->my_transp_rgn_tree;
			lyrTrRegions_Forb[i] = SW_Site.lyr[i]->my_transp_rgn_forb;
			lyrTrRegions_Shrub[i] = SW_Site.lyr[i]->my_transp_rgn_shrub;
			lyrTrRegions_Grass[i] = SW_Site.lyr[i]->my_transp_rgn_grass;
			lyrSWCBulk_FieldCaps[i] = SW_Site.lyr[i]->swcBulk_fieldcap;
			lyrWidths[i] = SW_Site.lyr[i]->width;
			lyrSWCBulk_Wiltpts[i] = SW_Site.lyr[i]->swcBulk_wiltpt;
			lyrSWCBulk_HalfWiltpts[i] = SW_Site.lyr[i]->swcBulk_wiltpt / 2.;
			lyrSWCBulk_atSWPcrit_Tree[i] = SW_Site.lyr[i]->swcBulk_atSWPcrit_tree;
			lyrSWCBulk_atSWPcrit_Forb[i] = SW_Site.lyr[i]->swcBulk_atSWPcrit_forb;
			lyrSWCBulk_atSWPcrit_Shrub[i] = SW_Site.lyr[i]->swcBulk_atSWPcrit_shrub;
			lyrSWCBulk_atSWPcrit_Grass[i] = SW_Site.lyr[i]->swcBulk_atSWPcrit_grass;
			lyrSWCBulk_Mins[i] = SW_Site.lyr[i]->swcBulk_min;
			lyrpsisMatric[i] = SW_Site.lyr[i]->psisMatric;
			lyrthetasMatric[i] = SW_Site.lyr[i]->thetasMatric;
			lyrBetasMatric[i] = SW_Site.lyr[i]->bMatric;
			lyrBetaInvMatric[i] = SW_Site.lyr[i]->binverseMatric;
			lyrImpermeability[i] = SW_Site.lyr[i]->impermeability;
			lyrSWCBulk_Saturated[i] = SW_Site.lyr[i]->swcBulk_saturated;
			lyrbDensity[i] = SW_Site.lyr[i]->soilBulk_density;

			/*Init hydraulic redistribution to zero */
			lyrHydRed_Tree[i] = 0.;
			lyrHydRed_Shrub[i] = 0.;
			lyrHydRed_Forb[i] = 0.;
			lyrHydRed_Grass[i] = 0.;
		}

		ForEachTreeTranspLayer(i)
		{
			lyrTranspCo_Tree[i] = SW_Site.lyr[i]->transp_coeff_tree;
		}

		ForEachShrubTranspLayer(i)
		{
			lyrTranspCo_Shrub[i] = SW_Site.lyr[i]->transp_coeff_shrub;
		}

		ForEachForbTranspLayer(i)
		{
			lyrTranspCo_Forb[i] = SW_Site.lyr[i]->transp_coeff_forb;
		}

		ForEachGrassTranspLayer(i)
		{
			lyrTranspCo_Grass[i] = SW_Site.lyr[i]->transp_coeff_grass;
		}

		ForEachEvapLayer(i)
			lyrEvapCo[i] = SW_Site.lyr[i]->evap_coeff;

	} /* end firsttime stuff */

}

static void arrays2records(void) {
	/* move output quantities from arrays to
	 * the appropriate records.
	 */
	LyrIndex i;

	ForEachSoilLayer(i)
	{
		SW_Soilwat.swcBulk[Today][i] = lyrSWCBulk[i];
		SW_Soilwat.drain[i] = lyrDrain[i];
		SW_Soilwat.hydred_tree[i] = lyrHydRed_Tree[i];
		SW_Soilwat.hydred_shrub[i] = lyrHydRed_Shrub[i];
		SW_Soilwat.hydred_forb[i] = lyrHydRed_Forb[i];
		SW_Soilwat.hydred_grass[i] = lyrHydRed_Grass[i];
		SW_Soilwat.sTemp[i] = lyrsTemp[i];
	}

	if (SW_Site.deepdrain)
		SW_Soilwat.swcBulk[Today][SW_Site.deep_lyr] = drainout;

	ForEachTreeTranspLayer(i)
	{
		SW_Soilwat.transpiration_tree[i] = lyrTransp_Tree[i];
	}

	ForEachShrubTranspLayer(i)
	{
		SW_Soilwat.transpiration_shrub[i] = lyrTransp_Shrub[i];
	}

	ForEachForbTranspLayer(i)
	{
		SW_Soilwat.transpiration_forb[i] = lyrTransp_Forb[i];
	}

	ForEachGrassTranspLayer(i)
	{
		SW_Soilwat.transpiration_grass[i] = lyrTransp_Grass[i];
	}

	ForEachEvapLayer(i)
	{
		SW_Soilwat.evaporation[i] = lyrEvap_BareGround[i] + lyrEvap_Tree[i] + lyrEvap_Forb[i] + lyrEvap_Shrub[i] + lyrEvap_Grass[i];
	}

}
