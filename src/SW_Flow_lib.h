/********************************************************/
/********************************************************/
/*	Source file: SW_Flow_lib.h
 Type: header
 Application: SOILWAT - soilwater dynamics simulator
 Purpose: Support definitions/declarations for
 water flow subroutines in SoWat_flow_subs.h.
 History:
 10-20-03 (cwb) Added drainout variable.
 10/19/2010	(drs) added function hydraulic_redistribution()
 11/16/2010	(drs) added function forest_intercepted_water() and MAX_WINTFOR
 renamed evap_litter_stdcrop() -> evap_litter_veg()
 08/22/2011	(drs) renamed bs_ev_tr_loss() to EsT_partitioning()
 08/22/2011	(drs) added forest_EsT_partitioning() to partition bare-soil evaporation (Es) and transpiration (T) in forests
 09/08/2011	(drs) renamed stcrp_intercepted_water() to grass_intercepted_water();
 added shrub_intercepted_water() as current copy from grass_intercepted_water();
 added double scale to each xx_intercepted_water() function to scale for snowdepth effects and vegetation type fraction
 added double a,b,c,d to each xx_intercepted_water() function for for parameters in intercepted rain = (a + b*veg) + (c+d*veg) * ppt
 09/09/2011	(drs) added xx_EsT_partitioning() for each vegetation type (grass, shrub, tree); added double lai_param as parameter in exp-equation
 09/09/2011	(drs) replaced evap_litter_veg_surfaceWater() with evap_fromSurface() to be called for each surface water pool seperately
 09/09/2011	(drs) replaced SHD_xx constanst with input parameters in pot_transp()
 09/09/2011	(drs) added double Es_param_limit to pot_soil_evap() to scale and limit bare-soil evaporation rate (previously hidden constant in code)
 09/09/2011	(drs) renamed reduce_rates_by_surfaceEvaporation() to reduce_rates_by_unmetEvapDemand()
 09/11/2011	(drs) added double scale to hydraulic_redistribution() function to scale for vegetation type fraction
 09/13/2011	(drs)	added double scale, a,b,c,d to each litter_intercepted_water() function for parameters in litter intercepted rain = ((a + b*blitter) + (c+d*blitter) * ppt) * scale
 09/21/2011	(drs)	reduce_rates_by_unmetEvapDemand() is obsolete, complete E and T scaling in SW_Flow.c
 05/25/2012  (DLM) added function soil_temperature to header file
 05/31/2012  (DLM) added ST_RGR_VALUES struct to keep track of variables used in the soil_temperature function
 11/06/2012	(clk) added slope and aspect as parameters for petfunc()
 01/31/2013	(clk) added new function, pot_soil_evap_bs()
 03/07/2013	(clk) add new array, lyrFrozen to keep track of whether a certain soil layer is frozen. 1 = frozen, 0 = not frozen.
 07/09/2013	(clk) added two new functions: forb_intercepted_water and forb_EsT_partitioning
 */
/********************************************************/
/********************************************************/

#ifndef SW_WATERSUBS_H
#define SW_WATERSUBS_H

/* Standing crop can only intercept so much precip
 * This is the limiter used inside stdcrop_intercepted()
 */
#define MAX_WINTSTCR (vegcov * .1)
#define MAX_WINTFOR (ppt)

/* Litter can only intercept so much precip.
 * This is the limiter used inside litter_intercepted()
 */
#define MAX_WINTLIT (blitter * .2)

// this structure is for keeping track of the variables used in the soil_temperature function (mainly the regressions)
typedef struct {

	double depths[MAX_LAYERS], depthsR[MAX_ST_RGR + 1], fcR[MAX_ST_RGR], wpR[MAX_ST_RGR], bDensityR[MAX_ST_RGR], oldsFusionPool[MAX_LAYERS], oldsTempR[MAX_ST_RGR + 1];

	int lyrFrozen[MAX_LAYERS];

	unsigned int x1BoundsR[MAX_ST_RGR], x2BoundsR[MAX_ST_RGR], x1Bounds[MAX_LAYERS], x2Bounds[MAX_LAYERS];
} ST_RGR_VALUES;

/* =================================================== */
/* =================================================== */
/*                Function Definitions                 */
/* --------------------------------------------------- */
void grass_intercepted_water(double *pptleft, double *wintgrass, double ppt, double vegcov, double scale, double a, double b, double c, double d);

void shrub_intercepted_water(double *pptleft, double *wintshrub, double ppt, double vegcov, double scale, double a, double b, double c, double d);

void tree_intercepted_water(double *pptleft, double *wintfor, double ppt, double LAI, double scale, double a, double b, double c, double d);

void forb_intercepted_water(double *pptleft, double *wintforb, double ppt, double vegcov, double scale, double a, double b, double c, double d);

void litter_intercepted_water(double *pptleft, double *wintlit, double blitter, double scale, double a, double b, double c, double d);

void infiltrate_water_high(double swc[], double drain[], double *drainout, double pptleft, unsigned int nlyrs, double swcfc[], double swcsat[], double impermeability[],
		double *standingWater);

double petfunc(unsigned int doy, double avgtemp, double rlat, double elev, double slope, double aspect, double reflec, double humid, double windsp, double cloudcov,
		double transcoeff);

double svapor(double temp);

void transp_weighted_avg(double *swp_avg, unsigned int n_tr_rgns, unsigned int n_layers, unsigned int tr_regions[], double tr_coeff[], double swc[]);

void grass_EsT_partitioning(double *fbse, double *fbst, double blivelai, double lai_param);
void shrub_EsT_partitioning(double *fbse, double *fbst, double blivelai, double lai_param);
void tree_EsT_partitioning(double *fbse, double *fbst, double blivelai, double lai_param);
void forb_EsT_partitioning(double *fbse, double *fbst, double blivelai, double lai_param);

void pot_soil_evap(double *bserate, unsigned int nelyrs, double ecoeff[], double totagb, double fbse, double petday, double shift, double shape, double inflec, double range,
		double width[], double swc[], double Es_param_limit);

void pot_soil_evap_bs(double *bserate, unsigned int nelyrs, double ecoeff[], double petday, double shift, double shape, double inflec, double range, double width[],
		double swc[]);

void pot_transp(double *bstrate, double swpavg, double biolive, double biodead, double fbst, double petday, double swp_shift, double swp_shape, double swp_inflec,
		double swp_range, double shade_scale, double shade_deadmax, double shade_xinflex, double shade_slope, double shade_yinflex, double shade_range);

double watrate(double swp, double petday, double shift, double shape, double inflec, double range);

void evap_litter_veg_surfaceWater(double *cwlit, double *cwstcr, double *standingWater, double *wevap, double *aet, double petday);

void evap_fromSurface(double *water_pool, double *evap_rate, double *aet);

void remove_from_soil(double swc[], double qty[], double *aet, unsigned int nlyrs, double ecoeff[], double rate, double swcmin[]);

void infiltrate_water_low(double swc[], double drain[], double *drainout, unsigned int nlyrs, double sdrainpar, double sdraindpth, double swcfc[], double width[],
		double swcmin[], double swcsat[], double impermeability[], double *standingWater);

void hydraulic_redistribution(double swc[], double swcwp[], double lyrRootCo[], double hydred[], unsigned int nlyrs, double maxCondroot, double swp50, double shapeCond,
		double scale);

void soil_temperature(double airTemp, double pet, double aet, double biomass, double swc[], double bDensity[], double width[], double oldsTemp[], double sTemp[],
		unsigned int nlyrs, double fc[], double wp[], double bmLimiter, double t1Param1, double t1Param2, double t1Param3, double csParam1, double csParam2, double shParam,
		double snowpack, double meanAirTemp, double deltaX, double theMaxDepth, unsigned int nRgr);

#endif
