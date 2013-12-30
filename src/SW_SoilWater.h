/********************************************************/
/********************************************************/
/*	Source file: SW_SoilWater.h
 Type: header
 Application: SOILWAT - soilwater dynamics simulator
 Purpose: Definitions for actual soil water content,
 the reason for the model.
 History:
 9/11/01 -- INITIAL CODING - cwb
 1/25/02 - cwb- removed SW_TIMES dy element from hist.
 10/9/2009	-	(drs) added snow accumulation, snow sublimation and snow melt
 20100202 (drs) added lyrdrain[MAX_LAYERS], crop_int, litt_int, soil_inf to SW_SOILWAT_OUTPUTS;
 added standcrop_int, litter_int, soil_inf to SW_SOILWTAT;
 04/16/2010	(drs) added swa[MAX_LAYERS] to SW_SOILWAT_OUTPUTS: available soil water (cm/layer) = swc - (wilting point)
 10/04/2010	(drs) added snowMAUS snow accumulation, sublimation and melt algorithm: Trnka, M., Kocmánková, E., Balek, J., Eitzinger, J., Ruget, F., Formayer, H., Hlavinka, P., Schaumberger, A., Horáková, V., Mozny, M. & Zalud, Z. (2010) Simple snow cover model for agrometeorological applications. Agricultural and Forest Meteorology, 150, 1115-1127.
 added snowMAUS model parameters, replaced SW_SWC_snow_accumulation, SW_SWC_snow_sublimation, and SW_SWC_snow_melt with SW_SWC_adjust_snow(temp_min, temp_max, *ppt, *rain, *snow, *snowmelt)
 10/15/2010	(drs) replaced snowMAUS parameters with optimized SWAT2K parameters: Neitsch S, Arnold J, Kiniry J, Williams J. 2005. Soil and water assessment tool (SWAT) theoretical documentation. version 2005. Blackland Research Center, Texas Agricultural Experiment Station: Temple, TX.
 11/02/2010	(drs) moved snow parameters to SW_Site.h/c to be read in from siteparam.in
 10/19/2010 (drs)	added for hydraulic redistribution: hydred [MAX_LAYERS] to SW_SOILWAT_OUTPUTS and SW_SOILWAT
 11/16/2010	(drs)	added for_int to SW_SOILWAT_OUTPUTS, and forest_int to SW_SOILWAT
 renamed crop_evap -> veg_evap, standcrop_evap -> vegetation_evap
 01/04/2011	(drs) added parameter '*snowloss' to function SW_SWC_adjust_snow()
 02/19/2011	(drs) moved soil_inf from SW_SOILWAT and SW_SOILWAT_OUTPUTS to SW_WEATHER and SW_WEATHER_OUTPUTS
 07/22/2011	(drs) added for saturated conditions: surfaceWater and surfaceWater_evap to SW_SOILWAT_OUTPUTS and SW_SOILWAT
 08/22/2011	(drs) added function RealD SW_SnowDepth( RealD SWE, RealD snowdensity)
 09/08/2011	(drs) replaced in both struct SW_SOILWAT_OUTPUTS and SW_SOILWAT RealD crop_int, forest_int with RealD tree_int, shrub_int, grass_int
 09/09/2011	(drs) replaced in both struct SW_SOILWAT_OUTPUTS and SW_SOILWAT RealD veg_evap with RealD tree_evap, shrub_evap, grass_evap
 09/09/2011	(drs) replaced in both struct SW_SOILWAT_OUTPUTS and SW_SOILWAT RealD transpiration and hydred with RealD transpiration_xx, hydred_xx for each vegetation type (tree, shrub, grass)
 09/12/2011	(drs) added RealD snowdepth [TWO_DAYS] to struct SW_SOILWAT_OUTPUTS and SW_SOILWAT
 02/03/2012	(drs)	added function 'RealD SW_SWC_SWCres(RealD sand, RealD clay, RealD porosity)': which calculates 'Brooks-Corey' residual volumetric soil water based on Rawls & Brakensiek (1985)
 05/25/2012  (DLM) added sTemp[MAX_LAYERS] var to SW_SOILWAT_OUTPUTS struct & SW_SOILWAT struct to keep track of the soil temperatures
 04/16/2013	(clk)	Added the variables vwcMatric, and swaMatric to SW_SOILWAT_OUTPUTS
 Also, renamed a few of the other variables to better reflect MATRIC vs BULK values and SWC vs VWC.
 modified the use of these variables throughout the rest of the code.
 07/09/2013	(clk)	Added the variables transp_forb, forb_evap, hydred_forb, and forb_int to SW_SOILWAT_OUTPUTS
 Added the variables transpiration_forb, hydred_forb, forb_evap, and forb_int to SW_SOILWAT

 */
/********************************************************/
/********************************************************/

#ifndef SW_SOILWATER_H
#define SW_SOILWATER_H

#ifdef RSOILWAT
#include <R.h>
#include <Rdefines.h>
#include <Rconfig.h>
#include <Rinterface.h>
#include <Rinternals.h>
#endif
#include "generic.h"
#include "SW_Defines.h"
#include "SW_Times.h"
#include "SW_Site.h"

typedef enum {
	SW_Adjust_Avg = 1, SW_Adjust_StdErr
} SW_AdjustMethods;

/* parameters for historical (measured) swc values */
typedef struct {
	int method; /* method: 1=average; 2=hist+/- stderr */
	SW_TIMES yr;
	char *file_prefix; /* prefix to historical swc filenames */
	RealD swc[MAX_DAYS][MAX_LAYERS], std_err[MAX_DAYS][MAX_LAYERS];

} SW_SOILWAT_HIST;

/* accumulators for output values hold only the */
/* current period's values (eg, weekly or monthly) */
typedef struct {
	RealD wetdays[MAX_LAYERS], vwcBulk[MAX_LAYERS], /* soil water content cm/cm */
	vwcMatric[MAX_LAYERS], swcBulk[MAX_LAYERS], /* soil water content cm/layer */
	swpMatric[MAX_LAYERS], /* soil water potential */
	swaBulk[MAX_LAYERS], /* available soil water cm/layer, swc-(wilting point) */
	swaMatric[MAX_LAYERS], transp_total[MAX_LAYERS], transp_tree[MAX_LAYERS], transp_forb[MAX_LAYERS], transp_shrub[MAX_LAYERS], transp_grass[MAX_LAYERS], evap[MAX_LAYERS],
			lyrdrain[MAX_LAYERS], hydred_total[MAX_LAYERS], hydred_tree[MAX_LAYERS], /* hydraulic redistribution cm/layer */
			hydred_forb[MAX_LAYERS], hydred_shrub[MAX_LAYERS], hydred_grass[MAX_LAYERS], surfaceWater, total_evap, surfaceWater_evap, tree_evap, forb_evap, shrub_evap,
			grass_evap, litter_evap, total_int, tree_int, forb_int, shrub_int, grass_int, litter_int, snowpack, snowdepth, et, aet, pet, deep, sTemp[MAX_LAYERS]; // soil temperature in celcius for each layer
} SW_SOILWAT_OUTPUTS;

typedef struct {

	/* current daily soil water related values */
	Bool is_wet[MAX_LAYERS]; /* swc sufficient to count as wet today */
	RealD swcBulk[TWO_DAYS][MAX_LAYERS], snowpack[TWO_DAYS], /* swe of snowpack, if accumulation flag set */
	snowdepth, transpiration_tree[MAX_LAYERS], transpiration_forb[MAX_LAYERS], transpiration_shrub[MAX_LAYERS], transpiration_grass[MAX_LAYERS], evaporation[MAX_LAYERS],
			drain[MAX_LAYERS], /* amt of swc able to drain from curr layer to next */
			hydred_tree[MAX_LAYERS], /* hydraulic redistribution cm/layer */
			hydred_forb[MAX_LAYERS], hydred_shrub[MAX_LAYERS], hydred_grass[MAX_LAYERS], surfaceWater, surfaceWater_evap, pet, aet, litter_evap, tree_evap, forb_evap,
			shrub_evap, grass_evap, litter_int, tree_int, forb_int, shrub_int, grass_int, sTemp[MAX_LAYERS]; // soil temperature

	SW_SOILWAT_OUTPUTS dysum, /* helpful placeholder */
	wksum, mosum, yrsum, /* accumulators for *avg */
	wkavg, moavg, yravg; /* averages or sums as appropriate */
	Bool hist_use;
	SW_SOILWAT_HIST hist;

} SW_SOILWAT;

void SW_SWC_construct(void);
void SW_SWC_new_year(void);
void SW_SWC_read(void);
void SW_SWC_water_flow(void);
void SW_SWC_adjust_swc(TimeInt doy);
void SW_SWC_adjust_snow(RealD temp_min, RealD temp_max, RealD ppt, RealD *rain, RealD *snow, RealD *snowmelt, RealD *snowloss);
RealD SW_SnowDepth(RealD SWE, RealD snowdensity);
void SW_SWC_end_day(void);
RealD SW_SWCbulk2SWPmatric(RealD fractionGravel, RealD swcBulk, LyrIndex n);
RealD SW_SWPmatric2VWCBulk(RealD fractionGravel, RealD swpMatric, LyrIndex n);
RealD SW_VWCBulkRes(RealD fractionGravel, RealD sand, RealD clay, RealD porosity);

#ifdef RSOILWAT
SEXP onGet_SW_SWC();
void onSet_SW_SWC(SEXP SWC);
SEXP onGet_SW_SWC_hists();
SEXP onGet_SW_SWC_hist(TimeInt year);
void onSet_SW_SWC_hist(SEXP lyrs);
#endif

#ifdef DEBUG_MEM
void SW_SWC_SetMemoryRefs(void);
#endif

#endif
