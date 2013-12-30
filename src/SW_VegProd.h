/********************************************************/
/********************************************************/
/*  Source file: SW_VegProd.h
 Type: header
 Application: SOILWAT - soilwater dynamics simulator
 Purpose: Support routines and definitions for
 vegetation production parameter information.
 History:
 (8/28/01) -- INITIAL CODING - cwb
 11/16/2010	(drs) added LAIforest, biofoliage_for, lai_conv_for, TypeGrassOrShrub, TypeForest to SW_VEGPROD
 02/22/2011	(drs) added variable litter_for to SW_VEGPROD
 08/22/2011	(drs) added variable veg_height [MAX_MONTHS] to SW_VEGPROD
 09/08/2011	(drs) struct SW_VEGPROD now contains an element each for the types grasses, shrubs, and trees, as well as a variable describing the relative composition of vegetation by grasses, shrubs, and trees (changed from Bool to RealD types)
 the earlier SW_VEGPROD variables describing the vegetation is now contained in an additional struct VegType
 09/08/2011	(drs) added variables tanfunc_t tr_shade_effects, and RealD shade_scale and shade_deadmax to struct VegType (they were previously hidden in code in SW_Flow_lib.h)
 09/08/2011	(drs) moved all hydraulic redistribution variables from SW_Site.h to struct VegType
 09/08/2011	(drs) added variables RealD veg_intPPT_a, veg_intPPT_b, veg_intPPT_c, veg_intPPT_d to struct VegType for parameters in intercepted rain = (a + b*veg) + (c+d*veg) * ppt; Grasses+Shrubs: veg=vegcov, Trees: veg=LAI, which were previously hidden in code in SW_Flow_lib.c
 09/09/2011	(drs) added variable RealD EsTpartitioning_param to struct VegType as parameter for partitioning of bare-soil evaporation and transpiration as in Es = exp(-param*LAI)
 09/09/2011	(drs) added variable RealD Es_param_limit to struct VegType as parameter for for scaling and limiting bare soil evaporation rate: if totagb > Es_param_limit then no bare-soil evaporation
 09/13/2011	(drs) added variables RealD litt_intPPT_a, litt_intPPT_b, litt_intPPT_c, litt_intPPT_d to struct VegType for parameters in litter intercepted rain = (a + b*litter) + (c+d*litter) * ppt, which were previously hidden in code in SW_Flow_lib.c
 09/13/2011	(drs)	added variable RealD canopy_height_constant to struct VegType; if > 0 then constant canopy height (cm) and overriding cnpy-tangens=f(biomass)
 09/15/2011	(drs)	added variable RealD albedo to struct VegType (previously in SW_Site.h struct SW_SITE)
 09/26/2011	(drs)	added a daily variable for each monthly input in struct VegType: RealD litter_daily, biomass_daily, pct_live_daily, veg_height_daily, lai_conv_daily, lai_conv_daily, lai_live_daily, pct_cover_daily, vegcov_daily, biolive_daily, biodead_daily, total_agb_daily each of [MAX_DAYS]
 09/26/2011	(dsr)	removed monthly variables RealD veg_height, lai_live, pct_cover, vegcov, biolive, biodead, total_agb each [MAX_MONTHS] from struct VegType because replaced with daily records
 02/04/2012	(drs)	added variable RealD SWPcrit to struct VegType: critical soil water potential below which vegetation cannot sustain transpiration
 01/29/2013	(clk) added variable RealD fractionBareGround to now allow for bare ground as a part of the total vegetation.
 01/31/2013	(clk)	added varilabe RealD bareGround_albedo instead of creating a bareGround VegType, because only need albedo and not the other data members
 04/09/2013	(clk) changed the variable name swp50 to swpMatric50. Therefore also updated the use of swp50 to swpMatric50 in SW_VegProd.c and SW_Flow.c.
 07/09/2013	(clk)	add the variables forb and fractionForb to SW_VEGPROD
 */
/********************************************************/
/********************************************************/

#ifndef SW_VEGPROD_H
#define SW_VEGPROD_H

#include "SW_Defines.h"    /* for MAX_MONTHS and tanfunc_t*/
#ifdef RSOILWAT
#include <R.h>
#include <Rdefines.h>
#include <Rconfig.h>
#include <Rinterface.h>
#include <Rinternals.h>
#endif

typedef struct {
	RealD conv_stcr; /* divisor for lai_standing gives pct_cover */
	tanfunc_t cnpy, /* canopy height based on biomass */
	tr_shade_effects; /* shading effect on transpiration based on live and dead biomass */
	RealD canopy_height_constant; /* if > 0 then constant canopy height (cm) and overriding cnpy-tangens=f(biomass) */

	RealD shade_scale, /* scaling of live and dead biomass shading effects */
	shade_deadmax; /* maximal dead biomass for shading effects */

	RealD albedo;

	RealD litter[MAX_MONTHS], /* monthly litter values (g/m**2)    */
	biomass[MAX_MONTHS], /* monthly aboveground biomass (g/m**2) */
	pct_live[MAX_MONTHS], /* monthly live biomass in percent   */
	lai_conv[MAX_MONTHS]; /* monthly amount of biomass   needed to produce lai=1 (g/m**2)      */

	RealD litter_daily[MAX_DAYS + 1], /* daily interpolation of monthly litter values (g/m**2)    */
	biomass_daily[MAX_DAYS + 1], /* daily interpolation of monthly aboveground biomass (g/m**2) */
	pct_live_daily[MAX_DAYS + 1], /* daily interpolation of monthly live biomass in percent   */
	veg_height_daily[MAX_DAYS + 1], /* daily interpolation of monthly height of vegetation (cm)   */
	lai_conv_daily[MAX_DAYS + 1], /* daily interpolation of monthly amount of biomass needed to produce lai=1 (g/m**2)        */
	lai_live_daily[MAX_DAYS + 1], /* daily interpolation of lai of live biomass               */
	pct_cover_daily[MAX_DAYS + 1], vegcov_daily[MAX_DAYS + 1], /* daily interpolation of veg cover for today; function of monthly biomass */
	biolive_daily[MAX_DAYS + 1], /* daily interpolation of biomass * pct_live               */
	biodead_daily[MAX_DAYS + 1], /* daily interpolation of biomass - biolive                */
	total_agb_daily[MAX_DAYS + 1]; /* daily interpolation of sum of aboveground biomass & litter */

	Bool flagHydraulicRedistribution; /*1: allow hydraulic redistribution/lift to occur; 0: turned off */
	RealD maxCondroot, /* hydraulic redistribution: maximum radial soil-root conductance of the entire active root system for water (cm/-bar/day) */
	swpMatric50, /* hydraulic redistribution: soil water potential (-bar) where conductance is reduced by 50% */
	shapeCond; /* hydraulic redistribution: shaping parameter for the empirical relationship from van Genuchten to model relative soil-root conductance for water */

	RealD SWPcrit; /* critical soil water potential below which vegetation cannot sustain transpiration (-bar) */

	RealD veg_intPPT_a, veg_intPPT_b, veg_intPPT_c, veg_intPPT_d; /* vegetation intercepted rain = (a + b*veg) + (c+d*veg) * ppt; Grasses+Shrubs: veg=vegcov, Trees: veg=LAI */
	RealD litt_intPPT_a, litt_intPPT_b, litt_intPPT_c, litt_intPPT_d; /* litter intercepted rain = (a + b*litter) + (c+d*litter) * ppt */

	RealD EsTpartitioning_param; /* Parameter for partitioning of bare-soil evaporation and transpiration as in Es = exp(-param*LAI) */

	RealD Es_param_limit; /* parameter for scaling and limiting bare soil evaporation rate */

} VegType;

typedef struct {
	VegType grass, shrub, tree, forb;

	RealD fractionGrass, /* grass component fraction of total vegetation */
	fractionShrub, /* shrub component fraction of total vegetation */
	fractionTree, /* tree component fraction of total vegetation */
	fractionForb, /* forb component fraction of total vegetation */
	fractionBareGround; /* bare ground component fraction of total vegetation */

	RealD bareGround_albedo; /* create this here instead of creating a bareGround VegType, because it only needs albedo and no other data member */

} SW_VEGPROD;

void SW_VPD_read(void);
void SW_VPD_init(void);
void SW_VPD_construct(void);

#ifdef RSOILWAT
SEXP onGet_SW_VPD();
void onSet_SW_VPD(SEXP SW_VPD);
#endif

#endif
