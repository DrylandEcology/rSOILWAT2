/********************************************************/
/********************************************************/
/*  Source file: rSW_Output_rSOILWAT2.c
  Type: module
  Application: SOILWAT - soilwater dynamics simulator
  Purpose: define `get_XXX` functions for rSOILWAT2 interface
    see SW_Output_core.c and SW_Output.h

  History:
  2018 June 04 (drs) moved output formatter `get_XXX` functions from
    previous `SW_Output.c` to dedicated `rSW_Output_rSOILWAT2.c`
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
#include <ctype.h>

#include "SOILWAT2/generic.h"
#include "SOILWAT2/filefuncs.h"
#include "SOILWAT2/myMemory.h"
#include "SOILWAT2/Times.h"

#include "SOILWAT2/SW_Carbon.h"
#include "SOILWAT2/SW_Defines.h"
#include "SOILWAT2/SW_Files.h"
#include "SOILWAT2/SW_Model.h"
#include "SOILWAT2/SW_Site.h"
#include "SOILWAT2/SW_SoilWater.h"
#include "SOILWAT2/SW_Times.h"
#include "SOILWAT2/SW_Weather.h"
#include "SOILWAT2/SW_VegEstab.h"
#include "SOILWAT2/SW_VegProd.h"

#include "SOILWAT2/SW_Output.h"
#include "rSW_Output.h"

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

/* =================================================== */
/*                  Global Variables                   */
/* --------------------------------------------------- */
extern SW_SITE SW_Site;
extern SW_SOILWAT SW_Soilwat;
extern SW_MODEL SW_Model;
extern SW_WEATHER SW_Weather;
extern SW_VEGPROD SW_VegProd;
extern SW_VEGESTAB SW_VegEstab;
extern SW_CARBON SW_Carbon;

// defined in `SW_Output_core.c`:
extern SW_OUTPUT SW_Output[SW_OUTNKEYS];
extern TimeInt tOffset;

// defined in `rSW_Output.c`
extern RealD *p_rOUT[SW_OUTNKEYS][SW_OUTNPERIODS];
extern unsigned int nrow_TimeOUT[SW_OUTNPERIODS];
extern unsigned int nrow_OUT[SW_OUTNPERIODS];
extern unsigned int irow_OUT[SW_OUTNPERIODS];


/* =================================================== */
/* =================================================== */
/*             Private Function Definitions            */
/* --------------------------------------------------- */

static void get_outvalleader(OutKey k, OutPeriod pd);

/** Corresponds to function `get_outstrleader` of `SOILWAT2-standalone`
*/
static void get_outvalleader(OutKey k, OutPeriod pd) {
	RealD *p;

	p = p_rOUT[k][pd];
	p[irow_OUT[pd] + nrow_OUT[pd] * 0] = SW_Model.simyear;

	switch (pd) {
		case eSW_Day:
			p[irow_OUT[eSW_Day] + nrow_OUT[eSW_Day] * 1] = SW_Model.doy;
			break;

		case eSW_Week:
			p[irow_OUT[eSW_Week] + nrow_OUT[eSW_Week] * 1] =
				SW_Model.week + 1 - tOffset;
			break;

		case eSW_Month:
			p[irow_OUT[eSW_Month] + nrow_OUT[eSW_Month] * 1] =
				SW_Model.month + 1 - tOffset;
			break;

		case eSW_Year:
			break;
	}
}


/* =================================================== */
/* =================================================== */
/*             Function Definitions                    */
/*             (declared in SW_Output.h)               */
/* --------------------------------------------------- */
void get_co2effects(OutPeriod pd) {
	int k;
	RealD biomass_total = 0., biolive_total = 0.;
	SW_VEGPROD *v = &SW_VegProd;
	SW_VEGPROD_OUTPUTS *vo = NULL;
	RealD *p;

	set_VEGPROD_aggslot(pd, &vo);

	ForEachVegType(k)
	{
		biomass_total += vo->veg[k].biomass;
		biolive_total += vo->veg[k].biolive;
	}

	// Store into output array
	get_outvalleader(eSW_CO2Effects, pd);
	p = p_rOUT[eSW_CO2Effects][pd];

	// NOTE: `get_co2effects` uses a different order of vegetation types than the rest of SoilWat!!!
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 0)] = vo->veg[SW_GRASS].biomass;
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 1)] = vo->veg[SW_SHRUB].biomass;
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 2)] = vo->veg[SW_TREES].biomass;
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 3)] = vo->veg[SW_FORBS].biomass;
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 4)] = biomass_total;
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 5)] = vo->veg[SW_GRASS].biolive;
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 6)] = vo->veg[SW_SHRUB].biolive;
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 7)] = vo->veg[SW_TREES].biolive;
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 8)] = vo->veg[SW_FORBS].biolive;
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 9)] = biolive_total;

	// No averaging or summing required:
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 10)] =
		v->veg[SW_GRASS].co2_multipliers[BIO_INDEX][SW_Model.simyear];
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 11)] =
		v->veg[SW_SHRUB].co2_multipliers[BIO_INDEX][SW_Model.simyear];
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 12)] =
		v->veg[SW_TREES].co2_multipliers[BIO_INDEX][SW_Model.simyear];
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 12)] =
		v->veg[SW_FORBS].co2_multipliers[BIO_INDEX][SW_Model.simyear];
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 14)] =
		v->veg[SW_GRASS].co2_multipliers[WUE_INDEX][SW_Model.simyear];
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 15)] =
		v->veg[SW_SHRUB].co2_multipliers[WUE_INDEX][SW_Model.simyear];
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 16)] =
		v->veg[SW_TREES].co2_multipliers[WUE_INDEX][SW_Model.simyear];
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 17)] =
		v->veg[SW_FORBS].co2_multipliers[WUE_INDEX][SW_Model.simyear];
}

void get_estab(OutPeriod pd)
{
	SW_VEGESTAB *v = &SW_VegEstab;
	IntU i;
	RealD *p;

	i = (IntU) pd; // silence `-Wunused-parameter`

	// Store into output array
	get_outvalleader(eSW_Estab, pd);
	p = p_rOUT[eSW_Estab][pd];

	for (i = 0; i < v->count; i++) {
		p[irow_OUT[pd] + nrow_OUT[pd] * (i + nrow_TimeOUT[pd])] = v->parms[i]->estab_doy;
	}
}

void get_temp(OutPeriod pd)
{
	SW_WEATHER_OUTPUTS *vo = NULL;
	RealD *p;

  #ifdef SWDEBUG
  int debug = 0;
  #endif

  #ifdef SWDEBUG
  if (debug) swprintf("'get_temp': start for %d ... ", pd);
  #endif

	set_WEATHER_aggslot(pd, &vo);

	// Store into output array
	get_outvalleader(eSW_Temp, pd);
	p = p_rOUT[eSW_Temp][pd];
	#ifdef SWDEBUG
	if (debug) swprintf("irows %d-%d ... ",
		irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 0),
		irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 3));
	#endif
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 0)] = vo->temp_max;
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 1)] = vo->temp_min;
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 2)] = vo->temp_avg;
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 3)] = vo->surfaceTemp;

	#ifdef SWDEBUG
		if (debug) swprintf("completed\n");
	#endif
}

void get_precip(OutPeriod pd)
{
	SW_WEATHER_OUTPUTS *vo = NULL;
	RealD *p;

	set_WEATHER_aggslot(pd, &vo);

	// Store into output array
	get_outvalleader(eSW_Precip, pd);
	p = p_rOUT[eSW_Precip][pd];
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 0)] = vo->ppt;
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 1)] = vo->rain;
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 2)] = vo->snow;
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 3)] = vo->snowmelt;
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 4)] = vo->snowloss;
}

void get_vwcBulk(OutPeriod pd)
{
	LyrIndex i;
	SW_SOILWAT_OUTPUTS *vo = NULL;
	RealD *p;

	set_SOILWAT_aggslot(pd, &vo);

	// Store into output array
	get_outvalleader(eSW_VWCBulk, pd);
	p = p_rOUT[eSW_VWCBulk][pd];

	ForEachSoilLayer(i) {
		/* vwcBulk at this point is identical to swcBulk */
		p[irow_OUT[pd] + nrow_OUT[pd] * (i + nrow_TimeOUT[pd])] = vo->vwcBulk[i] / SW_Site.lyr[i]->width;
	}
}

void get_vwcMatric(OutPeriod pd)
{
	LyrIndex i;
	RealD convert;
	SW_SOILWAT_OUTPUTS *vo = NULL;
	RealD *p;

	set_SOILWAT_aggslot(pd, &vo);

	// Store into output array
	get_outvalleader(eSW_VWCMatric, pd);
	p = p_rOUT[eSW_VWCMatric][pd];

	ForEachSoilLayer(i) {
		/* vwcMatric at this point is identical to swcBulk */
		convert = 1. / (1. - SW_Site.lyr[i]->fractionVolBulk_gravel) / SW_Site.lyr[i]->width;
		p[irow_OUT[pd] + nrow_OUT[pd] * (i + nrow_TimeOUT[pd])] = vo->vwcMatric[i] * convert;
	}
}

void get_swa(OutPeriod pd)
{
	LyrIndex i;
	int k;
	SW_SOILWAT_OUTPUTS *vo = NULL;
	RealD *p;

	set_SOILWAT_aggslot(pd, &vo);

	// Store into output array
	get_outvalleader(eSW_SWA, pd);
	p = p_rOUT[eSW_SWA][pd];

	ForEachSoilLayer(i) {
		ForEachVegType(k) {
			p[irow_OUT[pd] + nrow_OUT[pd] * (i + nrow_TimeOUT[pd])] = vo->SWA_VegType[k][i];
		}
	}
}


void get_swcBulk(OutPeriod pd)
{
	LyrIndex i;
	SW_SOILWAT_OUTPUTS *vo = NULL;
	RealD *p;

	set_SOILWAT_aggslot(pd, &vo);

	// Store into output array
	get_outvalleader(eSW_SWCBulk, pd);
	p = p_rOUT[eSW_SWCBulk][pd];

	ForEachSoilLayer(i) {
		p[irow_OUT[pd] + nrow_OUT[pd] * (i + nrow_TimeOUT[pd])] = vo->swcBulk[i];
	}
}

void get_swpMatric(OutPeriod pd)
{
	LyrIndex i;
	SW_SOILWAT_OUTPUTS *vo = NULL;
	RealD *p;

	set_SOILWAT_aggslot(pd, &vo);

	// Store into output array
	get_outvalleader(eSW_SWPMatric, pd);
	p = p_rOUT[eSW_SWPMatric][pd];

	ForEachSoilLayer(i) {
		p[irow_OUT[pd] + nrow_OUT[pd] * (i + nrow_TimeOUT[pd])] =
			SW_SWCbulk2SWPmatric(SW_Site.lyr[i]->fractionVolBulk_gravel,
				vo->swpMatric[i], i);
	}
}

void get_swaBulk(OutPeriod pd)
{
	LyrIndex i;
	SW_SOILWAT_OUTPUTS *vo = NULL;
	RealD *p;

	set_SOILWAT_aggslot(pd, &vo);

	// Store into output array
	get_outvalleader(eSW_SWABulk, pd);
	p = p_rOUT[eSW_SWABulk][pd];

	ForEachSoilLayer(i) {
		p[irow_OUT[pd] + nrow_OUT[pd] * (i + nrow_TimeOUT[pd])] = vo->swaBulk[i];
	}
}

void get_swaMatric(OutPeriod pd)
{
	LyrIndex i;
	RealD convert;
	SW_SOILWAT_OUTPUTS *vo = NULL;
	RealD *p;

	set_SOILWAT_aggslot(pd, &vo);

	// Store into output array
	get_outvalleader(eSW_SWAMatric, pd);
	p = p_rOUT[eSW_SWAMatric][pd];

	ForEachSoilLayer(i) {
		convert = 1. / (1. - SW_Site.lyr[i]->fractionVolBulk_gravel);
		p[irow_OUT[pd] + nrow_OUT[pd] * (i + nrow_TimeOUT[pd])] = vo->swaMatric[i] * convert;
	}
}

void get_surfaceWater(OutPeriod pd)
{
	SW_SOILWAT_OUTPUTS *vo = NULL;
	RealD *p;

	set_SOILWAT_aggslot(pd, &vo);

	// Store into output array
	get_outvalleader(eSW_SurfaceWater, pd);
	p = p_rOUT[eSW_SurfaceWater][pd];
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 0)] = vo->surfaceWater;
}

void get_runoffrunon(OutPeriod pd) {
	SW_WEATHER_OUTPUTS *vo = NULL;
	RealD *p;

	set_WEATHER_aggslot(pd, &vo);

	// Store into output array
	get_outvalleader(eSW_Runoff, pd);
	p = p_rOUT[eSW_Runoff][pd];
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 0)] =
		vo->surfaceRunoff + vo->snowRunoff - vo->surfaceRunon;
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 1)] = vo->surfaceRunoff;
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 2)] = vo->snowRunoff;
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 3)] = vo->surfaceRunon;
}

void get_transp(OutPeriod pd)
{
	LyrIndex i;
	int k;
	SW_SOILWAT_OUTPUTS *vo = NULL;
	RealD *p;

	set_SOILWAT_aggslot(pd, &vo);

	// Store into output array
	get_outvalleader(eSW_Transp, pd);
	p = p_rOUT[eSW_Transp][pd];

	/* total transpiration */
	ForEachSoilLayer(i) {
		p[irow_OUT[pd] + nrow_OUT[pd] * (i + nrow_TimeOUT[pd])] = vo->transp_total[i];
	}

	/* transpiration for each vegetation type */
	ForEachVegType(k) {
		ForEachSoilLayer(i) {
			p[irow_OUT[pd] + nrow_OUT[pd] * (i + nrow_TimeOUT[pd]) + (nrow_OUT[pd] * SW_Site.n_layers * (k + 1))] = vo->transp[k][i];
		}
	}
}


void get_evapSoil(OutPeriod pd)
{
	LyrIndex i;
	SW_SOILWAT_OUTPUTS *vo = NULL;
	RealD *p;

	set_SOILWAT_aggslot(pd, &vo);

	// Store into output array
	get_outvalleader(eSW_EvapSoil, pd);
	p = p_rOUT[eSW_EvapSoil][pd];

	ForEachEvapLayer(i) {
		p[irow_OUT[pd] + nrow_OUT[pd] * (i + nrow_TimeOUT[pd])] = vo->evap[i];
	}
}

void get_evapSurface(OutPeriod pd)
{
	int k;
	SW_SOILWAT_OUTPUTS *vo = NULL;
	RealD *p;

	set_SOILWAT_aggslot(pd, &vo);

	// Store into output array
	get_outvalleader(eSW_EvapSurface, pd);
	p = p_rOUT[eSW_EvapSurface][pd];

	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 0)] = vo->total_evap;
	ForEachVegType(k) {
		p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + k + 1)] = vo->evap_veg[k];
	}
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + NVEGTYPES + 1)] = vo->litter_evap;
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + NVEGTYPES + 2)] = vo->surfaceWater_evap;
}

void get_interception(OutPeriod pd)
{
	int k;
	SW_SOILWAT_OUTPUTS *vo = NULL;
	RealD *p;

	set_SOILWAT_aggslot(pd, &vo);

	// Store into output array
	get_outvalleader(eSW_Interception, pd);
	p = p_rOUT[eSW_Interception][pd];

	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 0)] = vo->total_int;
	ForEachVegType(k) {
		p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 1)] = vo->int_veg[k];
	}
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + NVEGTYPES + 1)] = vo->litter_int;
}

void get_soilinf(OutPeriod pd)
{
	SW_WEATHER_OUTPUTS *vo = NULL;
	RealD *p;

	set_WEATHER_aggslot(pd, &vo);

	// Store into output array
	get_outvalleader(eSW_SoilInf, pd);
	p = p_rOUT[eSW_SoilInf][pd];
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 0)] = vo->soil_inf;
}

void get_lyrdrain(OutPeriod pd)
{
	LyrIndex i;
	SW_SOILWAT_OUTPUTS *vo = NULL;
	RealD *p;

	set_SOILWAT_aggslot(pd, &vo);

	// Store into output array
	get_outvalleader(eSW_LyrDrain, pd);
	p = p_rOUT[eSW_LyrDrain][pd];

	for (i = 0; i < SW_Site.n_layers - 1; i++)
	{
		p[irow_OUT[pd] + nrow_OUT[pd] * (i + nrow_TimeOUT[pd])] = vo->lyrdrain[i];
	}
}

void get_hydred(OutPeriod pd)
{
	LyrIndex i;
	int k;
	SW_SOILWAT_OUTPUTS *vo = NULL;
	RealD *p;

	set_SOILWAT_aggslot(pd, &vo);

	// Store into output array
	get_outvalleader(eSW_HydRed, pd);
	p = p_rOUT[eSW_HydRed][pd];

	ForEachSoilLayer(i) {
		p[irow_OUT[pd] + nrow_OUT[pd] * (i + nrow_TimeOUT[pd])] = vo->hydred_total[i];
	}

	ForEachVegType(k) {
		ForEachSoilLayer(i) {
			p[irow_OUT[pd] + nrow_OUT[pd] * (i + nrow_TimeOUT[pd]) + (nrow_OUT[pd] * SW_Site.n_layers * (k + 1))] = vo->hydred[k][i];
		}
	}
}

void get_aet(OutPeriod pd)
{
	SW_SOILWAT_OUTPUTS *vo = NULL;
	RealD *p;

	set_SOILWAT_aggslot(pd, &vo);

	// Store into output array
	get_outvalleader(eSW_AET, pd);
	p = p_rOUT[eSW_AET][pd];
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 0)] = vo->aet;
}

void get_pet(OutPeriod pd)
{
	SW_SOILWAT_OUTPUTS *vo = NULL;
	RealD *p;

	set_SOILWAT_aggslot(pd, &vo);

	// Store into output array
	get_outvalleader(eSW_PET, pd);
	p = p_rOUT[eSW_PET][pd];
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 0)] = vo->pet;
}

void get_wetdays(OutPeriod pd)
{
	LyrIndex i;
	RealD *p;

	// Store into output array
	get_outvalleader(eSW_WetDays, pd);
	p = p_rOUT[eSW_WetDays][pd];

	if (pd == eSW_Day)
	{
		ForEachSoilLayer(i) {
			p[irow_OUT[pd] + nrow_OUT[pd] * (i + nrow_TimeOUT[pd])] =
				(SW_Soilwat.is_wet[i]) ? 1 : 0;
		}

	} else
	{
		SW_SOILWAT_OUTPUTS *vo = NULL;
		set_SOILWAT_aggslot(pd, &vo);

		ForEachSoilLayer(i) {
			p[irow_OUT[pd] + nrow_OUT[pd] * (i + nrow_TimeOUT[pd])] = (int) vo->wetdays[i];
		}
	}
}

void get_snowpack(OutPeriod pd)
{
	SW_SOILWAT_OUTPUTS *vo = NULL;
	RealD *p;

	set_SOILWAT_aggslot(pd, &vo);

	// Store into output array
	get_outvalleader(eSW_SnowPack, pd);
	p = p_rOUT[eSW_SnowPack][pd];
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 0)] = vo->snowpack;
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 1)] = vo->snowdepth;
}

void get_deepswc(OutPeriod pd)
{
	SW_SOILWAT_OUTPUTS *vo = NULL;
	RealD *p;

	set_SOILWAT_aggslot(pd, &vo);

	// Store into output array
	get_outvalleader(eSW_DeepSWC, pd);
	p = p_rOUT[eSW_DeepSWC][pd];
	p[irow_OUT[pd] + nrow_OUT[pd] * (nrow_TimeOUT[pd] + 0)] = vo->deep;
}

void get_soiltemp(OutPeriod pd)
{
	LyrIndex i;
	SW_SOILWAT_OUTPUTS *vo = NULL;
	RealD *p;

	set_SOILWAT_aggslot(pd, &vo);

	// Store into output array
	get_outvalleader(eSW_SoilTemp, pd);
	p = p_rOUT[eSW_SoilTemp][pd];

	ForEachSoilLayer(i)
	{
		p[irow_OUT[pd] + nrow_OUT[pd] * (i + nrow_TimeOUT[pd])] = vo->sTemp[i];
	}
}
