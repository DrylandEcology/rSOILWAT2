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
extern SW_OUTPUT SW_Output[SW_OUTNKEYS]; // defined in `SW_Output_core.c`

extern RealD *p_rOUT[SW_OUTNKEYS][SW_OUTNPERIODS]; // defined in `rSW_Output.c`
extern unsigned int nrow_OUT[SW_OUTNPERIODS]; // defined in `rSW_Output.c`

extern char _Sep; // defined in `SW_Output_core.c`: output delimiter
extern TimeInt tOffset; // defined in `SW_Output_core.c`: 1 or 0 means we're writing previous or current period
extern Bool bFlush_output; // defined in `SW_Output_core.c`: process partial period ?
extern char sw_outstr[OUTSTRLEN]; // defined in `SW_Output_core.c`: string with formatted output which is to be written to output files


/* =================================================== */
/* =================================================== */
/*             Function Definitions                    */
/*             (declared in SW_Output.h)               */
/* --------------------------------------------------- */


void get_co2effects(OutPeriod pd) {
	SW_VEGPROD *v = &SW_VegProd;
	int delta;
	RealD *p;

	RealD biomass_total = SW_MISSING, biolive_total = SW_MISSING;
	RealD biomass_grass = SW_MISSING, biomass_shrub = SW_MISSING,
		biomass_tree = SW_MISSING, biomass_forb = SW_MISSING;
	RealD biolive_grass = SW_MISSING, biolive_shrub = SW_MISSING,
		biolive_tree = SW_MISSING, biolive_forb = SW_MISSING;

	// Grab the multipliers that were just used
	// No averaging or summing required
	RealD bio_mult_grass = v->veg[SW_GRASS].co2_multipliers[BIO_INDEX][SW_Model.simyear];
	RealD bio_mult_shrub = v->veg[SW_SHRUB].co2_multipliers[BIO_INDEX][SW_Model.simyear];
	RealD bio_mult_tree = v->veg[SW_TREES].co2_multipliers[BIO_INDEX][SW_Model.simyear];
	RealD bio_mult_forb = v->veg[SW_FORBS].co2_multipliers[BIO_INDEX][SW_Model.simyear];
	RealD wue_mult_grass = v->veg[SW_GRASS].co2_multipliers[WUE_INDEX][SW_Model.simyear];
	RealD wue_mult_shrub = v->veg[SW_SHRUB].co2_multipliers[WUE_INDEX][SW_Model.simyear];
	RealD wue_mult_tree = v->veg[SW_TREES].co2_multipliers[WUE_INDEX][SW_Model.simyear];
	RealD wue_mult_forb = v->veg[SW_FORBS].co2_multipliers[WUE_INDEX][SW_Model.simyear];

	switch(pd) {
		case eSW_Day:
			biomass_grass = v->dysum.veg[SW_GRASS].biomass;
			biomass_shrub = v->dysum.veg[SW_SHRUB].biomass;
			biomass_tree = v->dysum.veg[SW_TREES].biomass;
			biomass_forb = v->dysum.veg[SW_FORBS].biomass;
			biolive_grass = v->dysum.veg[SW_GRASS].biolive;
			biolive_shrub = v->dysum.veg[SW_SHRUB].biolive;
			biolive_tree = v->dysum.veg[SW_TREES].biolive;
			biolive_forb = v->dysum.veg[SW_FORBS].biolive;
			biomass_total = biomass_grass + biomass_shrub + biomass_tree + biomass_forb;
			biolive_total = biolive_grass + biolive_shrub + biolive_tree + biolive_forb;

			delta = SW_Output[eSW_CO2Effects].dy_row;
			p = p_rOUT[eSW_CO2Effects][eSW_Day];
			p[delta + nrow_OUT[eSW_Day] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Day] * 1] = SW_Model.doy;
			p[delta + nrow_OUT[eSW_Day] * 2] = biomass_grass;
			p[delta + nrow_OUT[eSW_Day] * 3] = biomass_shrub;
			p[delta + nrow_OUT[eSW_Day] * 4] = biomass_tree;
			p[delta + nrow_OUT[eSW_Day] * 5] = biomass_forb;
			p[delta + nrow_OUT[eSW_Day] * 6] = biomass_total;
			p[delta + nrow_OUT[eSW_Day] * 7] = biolive_grass;
			p[delta + nrow_OUT[eSW_Day] * 8] = biolive_shrub;
			p[delta + nrow_OUT[eSW_Day] * 9] = biolive_tree;
			p[delta + nrow_OUT[eSW_Day] * 10] = biolive_forb;
			p[delta + nrow_OUT[eSW_Day] * 11] = biolive_total;
			p[delta + nrow_OUT[eSW_Day] * 12] = bio_mult_grass;
			p[delta + nrow_OUT[eSW_Day] * 13] = bio_mult_shrub;
			p[delta + nrow_OUT[eSW_Day] * 14] = bio_mult_tree;
			p[delta + nrow_OUT[eSW_Day] * 15] = bio_mult_forb;
			p[delta + nrow_OUT[eSW_Day] * 16] = wue_mult_grass;
			p[delta + nrow_OUT[eSW_Day] * 17] = wue_mult_shrub;
			p[delta + nrow_OUT[eSW_Day] * 18] = wue_mult_tree;
			p[delta + nrow_OUT[eSW_Day] * 19] = wue_mult_forb;
			SW_Output[eSW_CO2Effects].dy_row++;
			break;

		case eSW_Week:
			biomass_grass = v->wkavg.veg[SW_GRASS].biomass;
			biomass_shrub = v->wkavg.veg[SW_SHRUB].biomass;
			biomass_tree = v->wkavg.veg[SW_TREES].biomass;
			biomass_forb = v->wkavg.veg[SW_FORBS].biomass;
			biolive_grass = v->wkavg.veg[SW_GRASS].biolive;
			biolive_shrub = v->wkavg.veg[SW_SHRUB].biolive;
			biolive_tree = v->wkavg.veg[SW_TREES].biolive;
			biolive_forb = v->wkavg.veg[SW_FORBS].biolive;
			biomass_total = biomass_grass + biomass_shrub + biomass_tree + biomass_forb;
			biolive_total = biolive_grass + biolive_shrub + biolive_tree + biolive_forb;

			delta = SW_Output[eSW_CO2Effects].wk_row;
			p = p_rOUT[eSW_CO2Effects][eSW_Week];
			p[delta + nrow_OUT[eSW_Week] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Week] * 1] = (SW_Model.week + 1) - tOffset;
			p[delta + nrow_OUT[eSW_Week] * 2] = biomass_grass;
			p[delta + nrow_OUT[eSW_Week] * 3] = biomass_shrub;
			p[delta + nrow_OUT[eSW_Week] * 4] = biomass_tree;
			p[delta + nrow_OUT[eSW_Week] * 5] = biomass_forb;
			p[delta + nrow_OUT[eSW_Week] * 6] = biomass_total;
			p[delta + nrow_OUT[eSW_Week] * 7] = biolive_grass;
			p[delta + nrow_OUT[eSW_Week] * 8] = biolive_shrub;
			p[delta + nrow_OUT[eSW_Week] * 9] = biolive_tree;
			p[delta + nrow_OUT[eSW_Week] * 10] = biomass_forb;
			p[delta + nrow_OUT[eSW_Week] * 11] = biolive_total;
			p[delta + nrow_OUT[eSW_Week] * 12] = bio_mult_grass;
			p[delta + nrow_OUT[eSW_Week] * 13] = bio_mult_shrub;
			p[delta + nrow_OUT[eSW_Week] * 14] = bio_mult_tree;
			p[delta + nrow_OUT[eSW_Week] * 15] = bio_mult_forb;
			p[delta + nrow_OUT[eSW_Week] * 16] = wue_mult_grass;
			p[delta + nrow_OUT[eSW_Week] * 17] = wue_mult_shrub;
			p[delta + nrow_OUT[eSW_Week] * 18] = wue_mult_tree;
			p[delta + nrow_OUT[eSW_Week] * 19] = wue_mult_forb;
			SW_Output[eSW_CO2Effects].wk_row++;
			break;

		case eSW_Month:
			biomass_grass = v->moavg.veg[SW_GRASS].biomass;
			biomass_shrub = v->moavg.veg[SW_SHRUB].biomass;
			biomass_tree = v->moavg.veg[SW_TREES].biomass;
			biomass_forb = v->moavg.veg[SW_FORBS].biomass;
			biolive_grass = v->moavg.veg[SW_GRASS].biolive;
			biolive_shrub = v->moavg.veg[SW_SHRUB].biolive;
			biolive_tree = v->moavg.veg[SW_TREES].biolive;
			biolive_forb = v->moavg.veg[SW_FORBS].biolive;
			biomass_total = biomass_grass + biomass_shrub + biomass_tree + biomass_forb;
			biolive_total = biolive_grass + biolive_shrub + biolive_tree + biolive_forb;

			delta = SW_Output[eSW_CO2Effects].mo_row;
			p = p_rOUT[eSW_CO2Effects][eSW_Month];
			p[delta + nrow_OUT[eSW_Month] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Month] * 1] = (SW_Model.month) - tOffset + 1;
			p[delta + nrow_OUT[eSW_Month] * 2] = biomass_grass;
			p[delta + nrow_OUT[eSW_Month] * 3] = biomass_shrub;
			p[delta + nrow_OUT[eSW_Month] * 4] = biomass_tree;
			p[delta + nrow_OUT[eSW_Month] * 5] = biomass_forb;
			p[delta + nrow_OUT[eSW_Month] * 6] = biomass_total;
			p[delta + nrow_OUT[eSW_Month] * 8] = biolive_grass;
			p[delta + nrow_OUT[eSW_Month] * 7] = biolive_shrub;
			p[delta + nrow_OUT[eSW_Month] * 9] = biolive_tree;
			p[delta + nrow_OUT[eSW_Month] * 10] = biolive_forb;
			p[delta + nrow_OUT[eSW_Month] * 11] = biolive_total;
			p[delta + nrow_OUT[eSW_Month] * 12] = bio_mult_grass;
			p[delta + nrow_OUT[eSW_Month] * 13] = bio_mult_shrub;
			p[delta + nrow_OUT[eSW_Month] * 14] = bio_mult_tree;
			p[delta + nrow_OUT[eSW_Month] * 15] = bio_mult_forb;
			p[delta + nrow_OUT[eSW_Month] * 16] = wue_mult_grass;
			p[delta + nrow_OUT[eSW_Month] * 17] = wue_mult_shrub;
			p[delta + nrow_OUT[eSW_Month] * 18] = wue_mult_tree;
			p[delta + nrow_OUT[eSW_Month] * 19] = wue_mult_forb;
			SW_Output[eSW_CO2Effects].mo_row++;
			break;

		case eSW_Year:
			biomass_grass = v->yravg.veg[SW_GRASS].biomass;
			biomass_shrub = v->yravg.veg[SW_SHRUB].biomass;
			biomass_tree = v->yravg.veg[SW_TREES].biomass;
			biomass_forb = v->yravg.veg[SW_FORBS].biomass;
			biolive_grass = v->yravg.veg[SW_GRASS].biolive;
			biolive_shrub = v->yravg.veg[SW_SHRUB].biolive;
			biolive_tree = v->yravg.veg[SW_TREES].biolive;
			biolive_forb = v->yravg.veg[SW_FORBS].biolive;
			biomass_total = biomass_grass + biomass_shrub + biomass_tree + biomass_forb;
			biolive_total = biolive_grass + biolive_shrub + biolive_tree + biolive_forb;

			delta = SW_Output[eSW_CO2Effects].yr_row;
			p = p_rOUT[eSW_CO2Effects][eSW_Year];
			p[delta + nrow_OUT[eSW_Year] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Year] * 1] = biomass_grass;
			p[delta + nrow_OUT[eSW_Year] * 2] = biomass_shrub;
			p[delta + nrow_OUT[eSW_Year] * 3] = biomass_tree;
			p[delta + nrow_OUT[eSW_Year] * 4] = biomass_forb;
			p[delta + nrow_OUT[eSW_Year] * 5] = biomass_total;
			p[delta + nrow_OUT[eSW_Year] * 6] = biolive_grass;
			p[delta + nrow_OUT[eSW_Year] * 7] = biolive_shrub;
			p[delta + nrow_OUT[eSW_Year] * 8] = biolive_tree;
			p[delta + nrow_OUT[eSW_Year] * 9] = biolive_forb;
			p[delta + nrow_OUT[eSW_Year] * 10] = biolive_total;
			p[delta + nrow_OUT[eSW_Year] * 11] = bio_mult_grass;
			p[delta + nrow_OUT[eSW_Year] * 12] = bio_mult_shrub;
			p[delta + nrow_OUT[eSW_Year] * 13] = bio_mult_tree;
			p[delta + nrow_OUT[eSW_Year] * 14] = bio_mult_forb;
			p[delta + nrow_OUT[eSW_Year] * 15] = wue_mult_grass;
			p[delta + nrow_OUT[eSW_Year] * 16] = wue_mult_shrub;
			p[delta + nrow_OUT[eSW_Year] * 17] = wue_mult_tree;
			p[delta + nrow_OUT[eSW_Year] * 18] = wue_mult_forb;
			SW_Output[eSW_CO2Effects].yr_row++;
			break;
	}
}

void get_estab(OutPeriod pd)
{
	/* --------------------------------------------------- */
	/* the establishment check produces, for each species in
	 * the given set, a day of year >=0 that the species
	 * established itself in the current year.  The output
	 * will be a single row of numbers for each year.  Each
	 * column represents a species in the order it was entered
	 * in the estabs.in file.  The value will be the day that
	 * the species established, or 0 if it didn't establish
	 * this year.
	 */
	SW_VEGESTAB *v = &SW_VegEstab;
	IntU i;
	int delta;
	RealD *p;

	switch(pd)
	{
		case eSW_Day:
			delta = SW_Output[eSW_Estab].dy_row;
			p = p_rOUT[eSW_Estab][eSW_Day];
			p[delta + nrow_OUT[eSW_Day] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Day] * 1] = SW_Model.doy;

			for (i = 0; i < v->count; i++) {
				p[delta + nrow_OUT[eSW_Day] * (i + 2)] = v->parms[i]->estab_doy;
			}

			SW_Output[eSW_Estab].dy_row++;
			break;

		case eSW_Week:
			delta = SW_Output[eSW_Estab].wk_row;
			p = p_rOUT[eSW_Estab][eSW_Week];
			p[delta + nrow_OUT[eSW_Week] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Week] * 1] = (SW_Model.week + 1) - tOffset;

			for (i = 0; i < v->count; i++) {
				p[delta + nrow_OUT[eSW_Week] * (i + 2)] = v->parms[i]->estab_doy;
			}

			SW_Output[eSW_Estab].wk_row++;
			break;

		case eSW_Month:
			delta = SW_Output[eSW_Estab].mo_row;
			p = p_rOUT[eSW_Estab][eSW_Month];
			p[delta + nrow_OUT[eSW_Month] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Month] * 1] = (SW_Model.month + 1) - tOffset;

			for (i = 0; i < v->count; i++) {
				p[delta + nrow_OUT[eSW_Month] * (i + 2)] = v->parms[i]->estab_doy;
			}

			SW_Output[eSW_Estab].mo_row++;
			break;

		case eSW_Year:
			delta = SW_Output[eSW_Estab].yr_row;
			p = p_rOUT[eSW_Estab][eSW_Year];
			p[delta + nrow_OUT[eSW_Year] * 0] = SW_Model.simyear;

			for (i = 0; i < v->count; i++) {
				p[delta + nrow_OUT[eSW_Year] * (i + 1)] = v->parms[i]->estab_doy;
			}

			SW_Output[eSW_Estab].yr_row++;
			break;
	}
}

void get_temp(OutPeriod pd)
{
	int delta;
	RealD *p;
	SW_WEATHER *v = &SW_Weather;

  #ifdef SWDEBUG
  int debug = 0;
  #endif

  #ifdef SWDEBUG
  if (debug) swprintf("'get_temp': start for %d ... ", pd);
  #endif

	switch (pd)
	{
	case eSW_Day:
		#ifdef SWDEBUG
		if (debug) swprintf("%d doy ... ", SW_Model.doy);
		#endif
		delta = SW_Output[eSW_Temp].dy_row;
		p = p_rOUT[eSW_Temp][eSW_Day];
		p[delta + nrow_OUT[eSW_Day] * 0] = SW_Model.simyear;
		p[delta + nrow_OUT[eSW_Day] * 1] = SW_Model.doy;
		p[delta + nrow_OUT[eSW_Day] * 2] = v->dysum.temp_max;
		p[delta + nrow_OUT[eSW_Day] * 3] = v->dysum.temp_min;
		p[delta + nrow_OUT[eSW_Day] * 4] = v->dysum.temp_avg;
		p[delta + nrow_OUT[eSW_Day] * 5] = v->dysum.surfaceTemp;
		SW_Output[eSW_Temp].dy_row++;
		break;

	case eSW_Week:
		#ifdef SWDEBUG
		if (debug) swprintf("%d wk ... ", (SW_Model.week + 1) - tOffset);
		#endif
		delta = SW_Output[eSW_Temp].wk_row;
		p = p_rOUT[eSW_Temp][eSW_Week];
		p[delta + nrow_OUT[eSW_Week] * 0] = SW_Model.simyear;
		p[delta + nrow_OUT[eSW_Week] * 1] = (SW_Model.week + 1) - tOffset;
		p[delta + nrow_OUT[eSW_Week] * 2] = v->wkavg.temp_max;
		p[delta + nrow_OUT[eSW_Week] * 3] = v->wkavg.temp_min;
		p[delta + nrow_OUT[eSW_Week] * 4] = v->wkavg.temp_avg;
		p[delta + nrow_OUT[eSW_Week] * 5] = v->wkavg.surfaceTemp;
		SW_Output[eSW_Temp].wk_row++;
		break;

	case eSW_Month:
		#ifdef SWDEBUG
		if (debug) swprintf("%d mon ... ", (SW_Model.month + 1) - tOffset);
		#endif
		delta = SW_Output[eSW_Temp].mo_row;
		p = p_rOUT[eSW_Temp][eSW_Month];
		p[delta + nrow_OUT[eSW_Month] * 0] = SW_Model.simyear;
		p[delta + nrow_OUT[eSW_Month] * 1] = (SW_Model.month + 1) - tOffset;
		p[delta + nrow_OUT[eSW_Month] * 2] = v->moavg.temp_max;
		p[delta + nrow_OUT[eSW_Month] * 3] = v->moavg.temp_min;
		p[delta + nrow_OUT[eSW_Month] * 4] = v->moavg.temp_avg;
		p[delta + nrow_OUT[eSW_Month] * 5] = v->moavg.surfaceTemp;
		SW_Output[eSW_Temp].mo_row++;
		break;

	case eSW_Year:
		#ifdef SWDEBUG
		if (debug) swprintf("%d yr ... ", SW_Model.simyear);
		#endif
		delta = SW_Output[eSW_Temp].yr_row;
		p = p_rOUT[eSW_Temp][eSW_Year];
		p[delta + nrow_OUT[eSW_Year] * 0] = SW_Model.simyear;
		p[delta + nrow_OUT[eSW_Year] * 1] = v->yravg.temp_max;
		p[delta + nrow_OUT[eSW_Year] * 2] = v->yravg.temp_min;
		p[delta + nrow_OUT[eSW_Year] * 3] = v->yravg.temp_avg;
		p[delta + nrow_OUT[eSW_Year] * 4] = v->yravg.surfaceTemp;
		SW_Output[eSW_Temp].yr_row++;
		break;
	}


	#ifdef SWDEBUG
		if (debug) swprintf("completed\n");
	#endif
}

void get_precip(OutPeriod pd)
{
	/* --------------------------------------------------- */
	/* 	20091015 (drs) ppt is divided into rain and snow and all three values are output into precip */
	SW_WEATHER *v = &SW_Weather;
	int delta;
	RealD *p;

	switch(pd)
	{
	case eSW_Day:
		delta = SW_Output[eSW_Precip].dy_row;
		p = p_rOUT[eSW_Precip][eSW_Day];
		p[delta + nrow_OUT[eSW_Day] * 0] = SW_Model.simyear;
		p[delta + nrow_OUT[eSW_Day] * 1] = SW_Model.doy;
		p[delta + nrow_OUT[eSW_Day] * 2] = v->dysum.ppt;
		p[delta + nrow_OUT[eSW_Day] * 3] = v->dysum.rain;
		p[delta + nrow_OUT[eSW_Day] * 4] = v->dysum.snow;
		p[delta + nrow_OUT[eSW_Day] * 5] = v->dysum.snowmelt;
		p[delta + nrow_OUT[eSW_Day] * 6] = v->dysum.snowloss;
		SW_Output[eSW_Precip].dy_row++;
		break;

	case eSW_Week:
		delta = SW_Output[eSW_Precip].wk_row;
		p = p_rOUT[eSW_Precip][eSW_Week];
		p[delta + nrow_OUT[eSW_Week] * 0] = SW_Model.simyear;
		p[delta + nrow_OUT[eSW_Week] * 1] = (SW_Model.week + 1) - tOffset;
		p[delta + nrow_OUT[eSW_Week] * 2] = v->wkavg.ppt;
		p[delta + nrow_OUT[eSW_Week] * 3] = v->wkavg.rain;
		p[delta + nrow_OUT[eSW_Week] * 4] = v->wkavg.snow;
		p[delta + nrow_OUT[eSW_Week] * 5] = v->wkavg.snowmelt;
		p[delta + nrow_OUT[eSW_Week] * 6] = v->wkavg.snowloss;
		SW_Output[eSW_Precip].wk_row++;
		break;

	case eSW_Month:
		delta = SW_Output[eSW_Precip].mo_row;
		p = p_rOUT[eSW_Precip][eSW_Month];
		p[delta + nrow_OUT[eSW_Month] * 0] = SW_Model.simyear;
		p[delta + nrow_OUT[eSW_Month] * 1] = (SW_Model.month + 1) - tOffset;
		p[delta + nrow_OUT[eSW_Month] * 2] = v->moavg.ppt;
		p[delta + nrow_OUT[eSW_Month] * 3] = v->moavg.rain;
		p[delta + nrow_OUT[eSW_Month] * 4] = v->moavg.snow;
		p[delta + nrow_OUT[eSW_Month] * 5] = v->moavg.snowmelt;
		p[delta + nrow_OUT[eSW_Month] * 6] = v->moavg.snowloss;
		SW_Output[eSW_Precip].mo_row++;
		break;

	case eSW_Year:
		delta = SW_Output[eSW_Precip].yr_row;
		p = p_rOUT[eSW_Precip][eSW_Year];
		p[delta + nrow_OUT[eSW_Year] * 0] = SW_Model.simyear;
		p[delta + nrow_OUT[eSW_Year] * 1] = v->yravg.ppt;
		p[delta + nrow_OUT[eSW_Year] * 2] = v->yravg.rain;
		p[delta + nrow_OUT[eSW_Year] * 3] = v->yravg.snow;
		p[delta + nrow_OUT[eSW_Year] * 4] = v->yravg.snowmelt;
		p[delta + nrow_OUT[eSW_Year] * 5] = v->yravg.snowloss;
		SW_Output[eSW_Precip].yr_row++;
	}
}

void get_vwcBulk(OutPeriod pd)
{
	/* --------------------------------------------------- */
	LyrIndex i;
	SW_SOILWAT *v = &SW_Soilwat;
	int delta;
	RealD *p;

	switch (pd)
	{ /* vwcBulk at this point is identical to swcBulk */
	case eSW_Day:
		delta = SW_Output[eSW_VWCBulk].dy_row;
		p = p_rOUT[eSW_VWCBulk][eSW_Day];
		p[delta + nrow_OUT[eSW_Day] * 0] = SW_Model.simyear;
		p[delta + nrow_OUT[eSW_Day] * 1] = SW_Model.doy;
		ForEachSoilLayer(i) {
			p[delta + nrow_OUT[eSW_Day] * (i + 2)] = v->dysum.vwcBulk[i] / SW_Site.lyr[i]->width;
		}
		SW_Output[eSW_VWCBulk].dy_row++;
		break;

	case eSW_Week:
		delta = SW_Output[eSW_VWCBulk].wk_row;
		p = p_rOUT[eSW_VWCBulk][eSW_Week];
		p[delta + nrow_OUT[eSW_Week] * 0] = SW_Model.simyear;
		p[delta + nrow_OUT[eSW_Week] * 1] = (SW_Model.week + 1) - tOffset;
		ForEachSoilLayer(i) {
			p[delta + nrow_OUT[eSW_Week] * (i + 2)] = v->wkavg.vwcBulk[i] / SW_Site.lyr[i]->width;
		}
		SW_Output[eSW_VWCBulk].wk_row++;
		break;

	case eSW_Month:
		delta = SW_Output[eSW_VWCBulk].mo_row;
		p = p_rOUT[eSW_VWCBulk][eSW_Month];
		p[delta + nrow_OUT[eSW_Month] * 0] = SW_Model.simyear;
		p[delta + nrow_OUT[eSW_Month] * 1] = (SW_Model.month + 1) - tOffset;
		ForEachSoilLayer(i) {
			p[delta + nrow_OUT[eSW_Month] * (i + 2)] = v->moavg.vwcBulk[i] / SW_Site.lyr[i]->width;
		}
		SW_Output[eSW_VWCBulk].mo_row++;
		break;

	case eSW_Year:
		delta = SW_Output[eSW_VWCBulk].yr_row;
		p = p_rOUT[eSW_VWCBulk][eSW_Year];
		p[delta + nrow_OUT[eSW_Year] * 0] = SW_Model.simyear;
		ForEachSoilLayer(i) {
			p[delta + nrow_OUT[eSW_Year] * (i + 1)] = v->yravg.vwcBulk[i] / SW_Site.lyr[i]->width;
		}
		SW_Output[eSW_VWCBulk].yr_row++;
		break;
	}
}

void get_vwcMatric(OutPeriod pd)
{
	/* --------------------------------------------------- */
	LyrIndex i;
	SW_SOILWAT *v = &SW_Soilwat;
	RealD convert;
	int delta;
	RealD *p;

	/* vwcMatric at this point is identical to swcBulk */
	switch (pd)
	{
	case eSW_Day:
		delta = SW_Output[eSW_VWCMatric].dy_row;
		p = p_rOUT[eSW_VWCMatric][eSW_Day];
		p[delta + nrow_OUT[eSW_Day] * 0] = SW_Model.simyear;
		p[delta + nrow_OUT[eSW_Day] * 1] = SW_Model.doy;
		ForEachSoilLayer(i) {
			convert = 1. / (1. - SW_Site.lyr[i]->fractionVolBulk_gravel) / SW_Site.lyr[i]->width;
			p[delta + nrow_OUT[eSW_Day] * (i + 2)] = v->dysum.vwcMatric[i] * convert;
		}
		SW_Output[eSW_VWCMatric].dy_row++;
		break;

	case eSW_Week:
		delta = SW_Output[eSW_VWCMatric].wk_row;
		p = p_rOUT[eSW_VWCMatric][eSW_Week];
		p[delta + nrow_OUT[eSW_Week] * 0] = SW_Model.simyear;
		p[delta + nrow_OUT[eSW_Week] * 1] = (SW_Model.week + 1) - tOffset;
		ForEachSoilLayer(i) {
			convert = 1. / (1. - SW_Site.lyr[i]->fractionVolBulk_gravel) / SW_Site.lyr[i]->width;
			p[delta + nrow_OUT[eSW_Week] * (i + 2)] = v->wkavg.vwcMatric[i] * convert;
		}
		SW_Output[eSW_VWCMatric].wk_row++;
		break;

	case eSW_Month:
		delta = SW_Output[eSW_VWCMatric].mo_row;
		p = p_rOUT[eSW_VWCMatric][eSW_Month];
		p[delta + nrow_OUT[eSW_Month] * 0] = SW_Model.simyear;
		p[delta + nrow_OUT[eSW_Month] * 1] = (SW_Model.month + 1) - tOffset;
		ForEachSoilLayer(i) {
			convert = 1. / (1. - SW_Site.lyr[i]->fractionVolBulk_gravel) / SW_Site.lyr[i]->width;
			p[delta + nrow_OUT[eSW_Month] * (i + 2)] = v->moavg.vwcMatric[i] * convert;
		}
		SW_Output[eSW_VWCMatric].mo_row++;
		break;

	case eSW_Year:
		delta = SW_Output[eSW_VWCMatric].yr_row;
		p = p_rOUT[eSW_VWCMatric][eSW_Year];
		p[delta + nrow_OUT[eSW_Year] * 0] = SW_Model.simyear;
		ForEachSoilLayer(i) {
			convert = 1. / (1. - SW_Site.lyr[i]->fractionVolBulk_gravel) / SW_Site.lyr[i]->width;
      p[delta + nrow_OUT[eSW_Year] * (i + 1)] = v->yravg.vwcMatric[i] * convert;
		}
		SW_Output[eSW_VWCMatric].yr_row++;
		break;
	}
}

void get_swa(OutPeriod pd)
{
	/* --------------------------------------------------- */
	/* added 21-Oct-03, cwb */
	LyrIndex i;
	int k;
	SW_SOILWAT *v = &SW_Soilwat;
	int delta;
	RealD *p;

	switch (pd)
	{
		case eSW_Day:
			delta = SW_Output[eSW_SWA].dy_row;
			p = p_rOUT[eSW_SWA][eSW_Day];
			p[delta + nrow_OUT[eSW_Day] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Day] * 1] = SW_Model.doy;

			ForEachSoilLayer(i) {
				ForEachVegType(k) {
					p[delta + nrow_OUT[eSW_Day] * (i + 2)] = v->dysum.SWA_VegType[k][i];
				}
			}

			SW_Output[eSW_SWA].dy_row++;
			break;

		case eSW_Week:
			delta = SW_Output[eSW_SWA].wk_row;
			p = p_rOUT[eSW_SWA][eSW_Week];
			p[delta + nrow_OUT[eSW_Week] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Week] * 1] = (SW_Model.week + 1) - tOffset;

			ForEachSoilLayer(i) {
				ForEachVegType(k) {
					p[delta + nrow_OUT[eSW_Day] * (i + 2)] = v->wkavg.SWA_VegType[k][i];
				}
			}

			SW_Output[eSW_SWA].wk_row++;
			break;

		case eSW_Month:
			delta = SW_Output[eSW_SWA].mo_row;
			p = p_rOUT[eSW_SWA][eSW_Month];
			p[delta+ nrow_OUT[eSW_Month] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Month] * 1] = (SW_Model.month + 1) - tOffset;

			ForEachSoilLayer(i) {
				ForEachVegType(k) {
					p[delta + nrow_OUT[eSW_Day] * (i + 2)] = v->moavg.SWA_VegType[k][i];
				}
			}

			SW_Output[eSW_SWA].mo_row++;
			break;

		case eSW_Year:
			delta = SW_Output[eSW_SWA].yr_row;
			p = p_rOUT[eSW_SWA][eSW_Year];
			p[delta + nrow_OUT[eSW_Year] * 0] = SW_Model.simyear;

			ForEachSoilLayer(i) {
				ForEachVegType(k) {
					p[delta + nrow_OUT[eSW_Day] * (i + 1)] = v->yravg.SWA_VegType[k][i];
				}
			}

			SW_Output[eSW_SWA].yr_row++;
			break;
	}
}


void get_swcBulk(OutPeriod pd)
{
	/* --------------------------------------------------- */
	/* added 21-Oct-03, cwb */
	int delta;
	RealD *p;
	LyrIndex i;
	SW_SOILWAT *v = &SW_Soilwat;

	switch (pd)
	{
		case eSW_Day:
			delta = SW_Output[eSW_SWCBulk].dy_row;
			p = p_rOUT[eSW_SWCBulk][eSW_Day];
			p[delta + nrow_OUT[eSW_Day] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Day] * 1] = SW_Model.doy;
			ForEachSoilLayer(i) {
				p[delta + nrow_OUT[eSW_Day] * (i + 2)] = v->dysum.swcBulk[i];
			}
			SW_Output[eSW_SWCBulk].dy_row++;
			break;

		case eSW_Week:
			delta = SW_Output[eSW_SWCBulk].wk_row;
			p = p_rOUT[eSW_SWCBulk][eSW_Week];
			p[delta + nrow_OUT[eSW_Week] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Week] * 1] = (SW_Model.week + 1) - tOffset;
			ForEachSoilLayer(i) {
				p[delta + nrow_OUT[eSW_Week] * (i + 2)] = v->wkavg.swcBulk[i];
			}
			SW_Output[eSW_SWCBulk].wk_row++;
			break;

		case eSW_Month:
			delta = SW_Output[eSW_SWCBulk].mo_row;
			p = p_rOUT[eSW_SWCBulk][eSW_Month];
			p[delta + nrow_OUT[eSW_Month] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Month] * 1] = (SW_Model.month + 1) - tOffset;
			ForEachSoilLayer(i) {
				p[delta + nrow_OUT[eSW_Month] * (i + 2)] = v->moavg.swcBulk[i];
			}
			SW_Output[eSW_SWCBulk].mo_row++;
			break;

		case eSW_Year:
			delta = SW_Output[eSW_SWCBulk].yr_row;
			p = p_rOUT[eSW_SWCBulk][eSW_Year];
			p[delta + nrow_OUT[eSW_Year] * 0] = SW_Model.simyear;
			ForEachSoilLayer(i) {
				p[delta + nrow_OUT[eSW_Year] * (i + 1)] = v->yravg.swcBulk[i];
			}
			SW_Output[eSW_SWCBulk].yr_row++;
			break;
	}
}

void get_swpMatric(OutPeriod pd)
{
	/* --------------------------------------------------- */
	/* can't take arithmetic average of swp because it's
	 * exponential.  At this time (until I remember to look
	 * up whether harmonic or some other average is better
	 * and fix this) we're not averaging swp but converting
	 * the averaged swc.  This also avoids converting for
	 * each day.
	 *
	 * added 12-Oct-03, cwb */

	LyrIndex i;
	SW_SOILWAT *v = &SW_Soilwat;
	int delta;
	RealD *p;

	switch (pd)
	{
		case eSW_Day:
			delta = SW_Output[eSW_SWPMatric].dy_row;
			p = p_rOUT[eSW_SWPMatric][eSW_Day];
			p[delta + nrow_OUT[eSW_Day] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Day] * 1] = SW_Model.doy;
			ForEachSoilLayer(i) {
				p[delta + nrow_OUT[eSW_Day] * (i + 2)] = SW_SWCbulk2SWPmatric(SW_Site.lyr[i]->fractionVolBulk_gravel, v->dysum.swpMatric[i], i);
			}
			SW_Output[eSW_SWPMatric].dy_row++;
			break;

		case eSW_Week:
			delta = SW_Output[eSW_SWPMatric].wk_row;
			p = p_rOUT[eSW_SWPMatric][eSW_Week];
			p[delta + nrow_OUT[eSW_Week] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Week] * 1] = (SW_Model.week + 1) - tOffset;
			ForEachSoilLayer(i) {
				p[delta + nrow_OUT[eSW_Week] * (i + 2)] = SW_SWCbulk2SWPmatric(SW_Site.lyr[i]->fractionVolBulk_gravel, v->wkavg.swpMatric[i], i);
			}
			SW_Output[eSW_SWPMatric].wk_row++;
			break;

		case eSW_Month:
			delta = SW_Output[eSW_SWPMatric].mo_row;
			p = p_rOUT[eSW_SWPMatric][eSW_Month];
			p[delta + nrow_OUT[eSW_Month] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Month] * 1] = (SW_Model.month + 1) - tOffset;
			ForEachSoilLayer(i) {
				p[delta + nrow_OUT[eSW_Month] * (i + 2)] = SW_SWCbulk2SWPmatric(SW_Site.lyr[i]->fractionVolBulk_gravel, v->moavg.swpMatric[i], i);
			}
			SW_Output[eSW_SWPMatric].mo_row++;
			break;

		case eSW_Year:
			delta = SW_Output[eSW_SWPMatric].yr_row;
			p = p_rOUT[eSW_SWPMatric][eSW_Year];
			p[delta + nrow_OUT[eSW_Year] * 0] = SW_Model.simyear;
			ForEachSoilLayer(i) {
				p[delta + nrow_OUT[eSW_Year] * (i + 1)] = SW_SWCbulk2SWPmatric(SW_Site.lyr[i]->fractionVolBulk_gravel, v->yravg.swpMatric[i], i);
			}
			SW_Output[eSW_SWPMatric].yr_row++;
			break;
	}
}

void get_swaBulk(OutPeriod pd)
{
	/* --------------------------------------------------- */

	LyrIndex i;
	SW_SOILWAT *v = &SW_Soilwat;
	int delta;
	RealD *p;

	switch (pd)
	{
		case eSW_Day:
			delta = SW_Output[eSW_SWABulk].dy_row;
			p = p_rOUT[eSW_SWABulk][eSW_Day];
			p[delta + nrow_OUT[eSW_Day] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Day] * 1] = SW_Model.doy;
			ForEachSoilLayer(i) {
				p[delta + nrow_OUT[eSW_Day] * (i + 2)] = v->dysum.swaBulk[i];
			}
			SW_Output[eSW_SWABulk].dy_row++;
			break;

		case eSW_Week:
			delta = SW_Output[eSW_SWABulk].wk_row;
			p = p_rOUT[eSW_SWABulk][eSW_Week];
			p[delta + nrow_OUT[eSW_Week] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Week] * 1] = (SW_Model.week + 1) - tOffset;
			ForEachSoilLayer(i) {
				p[delta + nrow_OUT[eSW_Week] * (i + 2)] = v->wkavg.swaBulk[i];
			}
			SW_Output[eSW_SWABulk].wk_row++;
			break;

		case eSW_Month:
			delta = SW_Output[eSW_SWABulk].mo_row;
			p = p_rOUT[eSW_SWABulk][eSW_Month];
			p[delta + nrow_OUT[eSW_Month] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Month] * 1] = (SW_Model.month + 1) - tOffset;
			ForEachSoilLayer(i) {
				p[delta + nrow_OUT[eSW_Month] * (i + 2)] = v->moavg.swaBulk[i];
			}
			SW_Output[eSW_SWABulk].mo_row++;
			break;

		case eSW_Year:
			delta = SW_Output[eSW_SWABulk].yr_row;
			p = p_rOUT[eSW_SWABulk][eSW_Year];
			p[delta + nrow_OUT[eSW_Year] * 0] = SW_Model.simyear;
			ForEachSoilLayer(i) {
				p[delta + nrow_OUT[eSW_Year] * (i + 1)] = v->yravg.swaBulk[i];
			}
			SW_Output[eSW_SWABulk].yr_row++;
			break;
	}
}

void get_swaMatric(OutPeriod pd)
{
	/* --------------------------------------------------- */

	LyrIndex i;
	SW_SOILWAT *v = &SW_Soilwat;
	RealD convert;
	int delta;
	RealD *p;

	switch (pd)
	{
		case eSW_Day:
			delta = SW_Output[eSW_SWAMatric].dy_row;
			p = p_rOUT[eSW_SWAMatric][eSW_Day];
			p[delta + nrow_OUT[eSW_Day] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Day] * 1] = SW_Model.doy;
			ForEachSoilLayer(i)
			{
				convert = 1. / (1. - SW_Site.lyr[i]->fractionVolBulk_gravel);
				p[delta + nrow_OUT[eSW_Day] * (i + 2)] = v->dysum.swaMatric[i] * convert;
			}
			SW_Output[eSW_SWAMatric].dy_row++;
			break;

		case eSW_Week:
			delta = SW_Output[eSW_SWAMatric].wk_row;
			p = p_rOUT[eSW_SWAMatric][eSW_Week];
			p[delta + nrow_OUT[eSW_Week] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Week] * 1] = (SW_Model.week + 1) - tOffset;
			ForEachSoilLayer(i)
			{
				convert = 1. / (1. - SW_Site.lyr[i]->fractionVolBulk_gravel);
				p[delta + nrow_OUT[eSW_Week] * (i + 2)] = v->wkavg.swaMatric[i] * convert;
			}
			SW_Output[eSW_SWAMatric].wk_row++;
			break;

		case eSW_Month:
			delta = SW_Output[eSW_SWAMatric].mo_row;
			p = p_rOUT[eSW_SWAMatric][eSW_Month];
			p[delta + nrow_OUT[eSW_Month] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Month] * 1] = (SW_Model.month + 1) - tOffset;
			ForEachSoilLayer(i)
			{
				convert = 1. / (1. - SW_Site.lyr[i]->fractionVolBulk_gravel);
				p[delta + nrow_OUT[eSW_Month] * (i + 2)] = v->moavg.swaMatric[i] * convert;
			}
			SW_Output[eSW_SWAMatric].mo_row++;
			break;

		case eSW_Year:
			delta = SW_Output[eSW_SWAMatric].yr_row;
			p = p_rOUT[eSW_SWAMatric][eSW_Year];
			p[delta + nrow_OUT[eSW_Year] * 0] = SW_Model.simyear;
			ForEachSoilLayer(i)
			{
				convert = 1. / (1. - SW_Site.lyr[i]->fractionVolBulk_gravel);
				p[delta + nrow_OUT[eSW_Year] * (i + 1)] = v->yravg.swaMatric[i] * convert;
			}
			SW_Output[eSW_SWAMatric].yr_row++;
			break;
	}
}

void get_surfaceWater(OutPeriod pd)
{
	/* --------------------------------------------------- */
	SW_SOILWAT *v = &SW_Soilwat;
	int delta;
	RealD *p;

	switch (pd)
	{
		case eSW_Day:
			delta = SW_Output[eSW_SurfaceWater].dy_row;
			p = p_rOUT[eSW_SurfaceWater][eSW_Day];
			p[delta + nrow_OUT[eSW_Day] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Day] * 1] = SW_Model.doy;
			p[delta + nrow_OUT[eSW_Day] * 2] = v->dysum.surfaceWater;
			SW_Output[eSW_SurfaceWater].dy_row++;
			break;

		case eSW_Week:
			delta = SW_Output[eSW_SurfaceWater].wk_row;
			p = p_rOUT[eSW_SurfaceWater][eSW_Week];
			p[delta + nrow_OUT[eSW_Week] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Week] * 1] = (SW_Model.week + 1) - tOffset;
			p[delta + nrow_OUT[eSW_Week] * 2] = v->wkavg.surfaceWater;
			SW_Output[eSW_SurfaceWater].wk_row++;
			break;

		case eSW_Month:
			delta = SW_Output[eSW_SurfaceWater].mo_row;
			p = p_rOUT[eSW_SurfaceWater][eSW_Month];
			p[delta + nrow_OUT[eSW_Month] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Month] * 1] = (SW_Model.month + 1) - tOffset;
			p[delta + nrow_OUT[eSW_Month] * 2] = v->moavg.surfaceWater;
			SW_Output[eSW_SurfaceWater].mo_row++;
			break;

		case eSW_Year:
			delta = SW_Output[eSW_SurfaceWater].yr_row;
			p = p_rOUT[eSW_SurfaceWater][eSW_Year];
			p[delta + nrow_OUT[eSW_Year] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Year] * 1] = v->yravg.surfaceWater;
			SW_Output[eSW_SurfaceWater].yr_row++;
			break;
	}
}

void get_runoffrunon(OutPeriod pd) {
	/* --------------------------------------------------- */
	/* (12/13/2012) (clk) Added function to output runoff variables */

	SW_WEATHER *w = &SW_Weather;
	RealD val_netRunoff = SW_MISSING, val_surfaceRunoff = SW_MISSING,
		val_surfaceRunon = SW_MISSING, val_snowRunoff = SW_MISSING;
	int delta;
	RealD *p;

	switch (pd)
	{
		case eSW_Day:
			val_surfaceRunoff = w->dysum.surfaceRunoff;
			val_surfaceRunon = w->dysum.surfaceRunon;
			val_snowRunoff = w->dysum.snowRunoff;
			break;
		case eSW_Week:
			val_surfaceRunoff = w->wkavg.surfaceRunoff;
			val_surfaceRunon = w->wkavg.surfaceRunon;
			val_snowRunoff = w->wkavg.snowRunoff;
			break;
		case eSW_Month:
			val_surfaceRunoff = w->moavg.surfaceRunoff;
			val_surfaceRunon = w->moavg.surfaceRunon;
			val_snowRunoff = w->moavg.snowRunoff;
			break;
		case eSW_Year:
			val_surfaceRunoff = w->yravg.surfaceRunoff;
			val_surfaceRunon = w->yravg.surfaceRunon;
			val_snowRunoff = w->yravg.snowRunoff;
			break;
	}
	val_netRunoff = val_surfaceRunoff + val_snowRunoff - val_surfaceRunon;

	switch (pd) {
		case eSW_Day:
			delta = SW_Output[eSW_Runoff].dy_row;
			p = p_rOUT[eSW_Runoff][eSW_Day];
			p[delta + nrow_OUT[eSW_Day] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Day] * 1] = SW_Model.doy;
			p[delta + nrow_OUT[eSW_Day] * 2] = val_netRunoff;
			p[delta + nrow_OUT[eSW_Day] * 3] = val_surfaceRunoff;
			p[delta + nrow_OUT[eSW_Day] * 4] = val_snowRunoff;
			p[delta + nrow_OUT[eSW_Day] * 5] = val_surfaceRunon;
			SW_Output[eSW_Runoff].dy_row++;
			break;

		case eSW_Week:
			delta = SW_Output[eSW_Runoff].wk_row;
			p = p_rOUT[eSW_Runoff][eSW_Week];
			p[delta + nrow_OUT[eSW_Week] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Week] * 1] = (SW_Model.week + 1) - tOffset;
			p[delta + nrow_OUT[eSW_Week] * 2] = val_netRunoff;
			p[delta + nrow_OUT[eSW_Week] * 3] = val_surfaceRunoff;
			p[delta + nrow_OUT[eSW_Week] * 4] = val_snowRunoff;
			p[delta + nrow_OUT[eSW_Week] * 5] = val_surfaceRunon;
			SW_Output[eSW_Runoff].wk_row++;
			break;

		case eSW_Month:
			delta = SW_Output[eSW_Runoff].mo_row;
			p = p_rOUT[eSW_Runoff][eSW_Month];
			p[delta + nrow_OUT[eSW_Month] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Month] * 1] = (SW_Model.month + 1) - tOffset;
			p[delta + nrow_OUT[eSW_Month] * 2] = val_netRunoff;
			p[delta + nrow_OUT[eSW_Month] * 3] = val_surfaceRunoff;
			p[delta + nrow_OUT[eSW_Month] * 4] = val_snowRunoff;
			p[delta + nrow_OUT[eSW_Month] * 5] = val_surfaceRunon;
			SW_Output[eSW_Runoff].mo_row++;
			break;

		case eSW_Year:
			delta = SW_Output[eSW_Runoff].yr_row;
			p = p_rOUT[eSW_Runoff][eSW_Year];
			p[delta + nrow_OUT[eSW_Year] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Year] * 1] = val_netRunoff;
			p[delta + nrow_OUT[eSW_Year] * 2] = val_surfaceRunoff;
			p[delta + nrow_OUT[eSW_Year] * 3] = val_snowRunoff;
			p[delta + nrow_OUT[eSW_Year] * 4] = val_surfaceRunon;
			SW_Output[eSW_Runoff].yr_row++;
			break;
		}
}

void get_transp(OutPeriod pd)
{
	LyrIndex i;
	int k;
	SW_SOILWAT *v = &SW_Soilwat;
	int delta;
	RealD *p;

	switch (pd)
	{
		case eSW_Day:
			delta = SW_Output[eSW_Transp].dy_row;
			p = p_rOUT[eSW_Transp][eSW_Day];
			p[delta + nrow_OUT[eSW_Day] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Day] * 1] = SW_Model.doy;

			/* total transpiration */
			ForEachSoilLayer(i) {
				p[delta + nrow_OUT[eSW_Day] * (i + 2)] = v->dysum.transp_total[i];
			}

			/* transpiration for each vegetation type */
			ForEachVegType(k) {
				ForEachSoilLayer(i) {
					p[delta + nrow_OUT[eSW_Day] * (i + 2) + (nrow_OUT[eSW_Day] * SW_Site.n_layers * (k + 1))] = v->dysum.transp[k][i];
				}
			}

			SW_Output[eSW_Transp].dy_row++;
			break;

		case eSW_Week:
			delta = SW_Output[eSW_Transp].wk_row;
			p = p_rOUT[eSW_Transp][eSW_Week];
			p[delta + nrow_OUT[eSW_Week] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Week] * 1] = (SW_Model.week + 1) - tOffset;

			/* total transpiration */
			ForEachSoilLayer(i) {
				p[delta + nrow_OUT[eSW_Week] * (i + 2)] = v->wkavg.transp_total[i];
			}

			/* transpiration for each vegetation type */
			ForEachVegType(k) {
				ForEachSoilLayer(i) {
					p[delta + nrow_OUT[eSW_Week] * (i + 2) + (nrow_OUT[eSW_Week] * SW_Site.n_layers * (k + 1))] = v->wkavg.transp[k][i];
				}
			}

			SW_Output[eSW_Transp].wk_row++;
			break;

		case eSW_Month:
			delta = SW_Output[eSW_Transp].mo_row;
			p = p_rOUT[eSW_Transp][eSW_Month];
			p[delta + nrow_OUT[eSW_Month] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Month] * 1] = (SW_Model.month + 1) - tOffset;

			/* total transpiration */
			ForEachSoilLayer(i) {
				p[delta + nrow_OUT[eSW_Month] * (i + 2)] = v->moavg.transp_total[i];
			}

			/* transpiration for each vegetation type */
			ForEachVegType(k) {
				ForEachSoilLayer(i) {
					p[delta + nrow_OUT[eSW_Month] * (i + 2) + (nrow_OUT[eSW_Month] * SW_Site.n_layers * (k + 1))] = v->moavg.transp[k][i];
				}
			}

			SW_Output[eSW_Transp].mo_row++;
			break;

		case eSW_Year:
			delta = SW_Output[eSW_Transp].yr_row;
			p = p_rOUT[eSW_Transp][eSW_Year];
			p[delta + nrow_OUT[eSW_Year] * 0] = SW_Model.simyear;

			/* total transpiration */
			ForEachSoilLayer(i) {
				p[delta + nrow_OUT[eSW_Year] * (i + 1)] = v->yravg.transp_total[i];
			}

			/* transpiration for each vegetation type */
			ForEachVegType(k) {
				ForEachSoilLayer(i) {
					p[delta + nrow_OUT[eSW_Year] * (i + 2) + (nrow_OUT[eSW_Year] * SW_Site.n_layers * (k + 1))] = v->yravg.transp[k][i];
				}
			}

			SW_Output[eSW_Transp].yr_row++;
			break;
	}
}


void get_evapSoil(OutPeriod pd)
{
	/* --------------------------------------------------- */
	LyrIndex i;
	SW_SOILWAT *v = &SW_Soilwat;
	int delta;
	RealD *p;

	switch (pd)
	{
		case eSW_Day:
			delta = SW_Output[eSW_EvapSoil].dy_row;
			p = p_rOUT[eSW_EvapSoil][eSW_Day];
			p[delta + nrow_OUT[eSW_Day] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Day] * 1] = SW_Model.doy;
			ForEachEvapLayer(i) {
				p[delta + nrow_OUT[eSW_Day] * (i + 2)] = v->dysum.evap[i];
			}
			SW_Output[eSW_EvapSoil].dy_row++;
			break;

		case eSW_Week:
			delta = SW_Output[eSW_EvapSoil].wk_row;
			p = p_rOUT[eSW_EvapSoil][eSW_Week];
			p[delta + nrow_OUT[eSW_Week] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Week] * 1] = (SW_Model.week + 1) - tOffset;
			ForEachEvapLayer(i) {
				p[delta + nrow_OUT[eSW_Week] * (i + 2)] = v->wkavg.evap[i];
			}
			SW_Output[eSW_EvapSoil].wk_row++;
			break;

		case eSW_Month:
			delta = SW_Output[eSW_EvapSoil].mo_row;
			p = p_rOUT[eSW_EvapSoil][eSW_Month];
			p[delta + nrow_OUT[eSW_Month] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Month] * 1] = (SW_Model.month + 1) - tOffset;
			ForEachEvapLayer(i) {
				p[delta + nrow_OUT[eSW_Month] * (i + 2)] = v->moavg.evap[i];
			}
			SW_Output[eSW_EvapSoil].mo_row++;
			break;

		case eSW_Year:
			delta = SW_Output[eSW_EvapSoil].yr_row;
			p = p_rOUT[eSW_EvapSoil][eSW_Year];
			p[delta + nrow_OUT[eSW_Year] * 0] = SW_Model.simyear;
			ForEachEvapLayer(i) {
				p[delta + nrow_OUT[eSW_Year] * (i + 1)] = v->yravg.evap[i];
			}
			SW_Output[eSW_EvapSoil].yr_row++;
			break;
	}
}

void get_evapSurface(OutPeriod pd)
{
	/* --------------------------------------------------- */
	SW_SOILWAT *v = &SW_Soilwat;
	int k;
	int delta;
	RealD *p;

	switch (pd)
	{
		case eSW_Day:
			delta = SW_Output[eSW_EvapSurface].dy_row;
			p = p_rOUT[eSW_EvapSurface][eSW_Day];
			p[delta + nrow_OUT[eSW_Day] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Day] * 1] = SW_Model.doy;
			p[delta + nrow_OUT[eSW_Day] * 2] = v->dysum.total_evap;
			ForEachVegType(k) {
				p[delta + nrow_OUT[eSW_Day] * (k + 3)] = v->dysum.evap_veg[k];
			}
			p[delta + nrow_OUT[eSW_Day] * 7] = v->dysum.litter_evap;
			p[delta + nrow_OUT[eSW_Day] * 8] = v->dysum.surfaceWater_evap;
			SW_Output[eSW_EvapSurface].dy_row++;
			break;

		case eSW_Week:
			delta = SW_Output[eSW_EvapSurface].wk_row;
			p = p_rOUT[eSW_EvapSurface][eSW_Week];
			p[delta + nrow_OUT[eSW_Week] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Week] * 1] = (SW_Model.week + 1) - tOffset;
			p[delta + nrow_OUT[eSW_Week] * 2] = v->wkavg.total_evap;
			ForEachVegType(k) {
				p[delta + nrow_OUT[eSW_Week] * (k + 3)] = v->wkavg.evap_veg[k];
			}
			p[delta + nrow_OUT[eSW_Week] * 7] = v->wkavg.litter_evap;
			p[delta + nrow_OUT[eSW_Week] * 8] = v->wkavg.surfaceWater_evap;
			SW_Output[eSW_EvapSurface].wk_row++;
			break;

		case eSW_Month:
			delta = SW_Output[eSW_EvapSurface].mo_row;
			p = p_rOUT[eSW_EvapSurface][eSW_Month];
			p[delta + nrow_OUT[eSW_Month] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Month] * 1] = (SW_Model.month + 1) - tOffset;
			p[delta + nrow_OUT[eSW_Month] * 2] = v->moavg.total_evap;
			ForEachVegType(k) {
				p[delta + nrow_OUT[eSW_Month] * (k + 3)] = v->moavg.evap_veg[k];
			}
			p[delta + nrow_OUT[eSW_Month] * 7] = v->moavg.litter_evap;
			p[delta + nrow_OUT[eSW_Month] * 8] = v->moavg.surfaceWater_evap;
			SW_Output[eSW_EvapSurface].mo_row++;
			break;

		case eSW_Year:
			delta = SW_Output[eSW_EvapSurface].yr_row;
			p = p_rOUT[eSW_EvapSurface][eSW_Year];
			p[delta + nrow_OUT[eSW_Year] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Year] * 1] = v->yravg.total_evap;
			ForEachVegType(k) {
				p[delta + nrow_OUT[eSW_Year] * (k + 3)] = v->yravg.evap_veg[k];
			}
			p[delta + nrow_OUT[eSW_Year] * 6] = v->yravg.litter_evap;
			p[delta + nrow_OUT[eSW_Year] * 7] = v->yravg.surfaceWater_evap;
			SW_Output[eSW_EvapSurface].yr_row++;
			break;
	}
}

void get_interception(OutPeriod pd)
{
	/* --------------------------------------------------- */
	SW_SOILWAT *v = &SW_Soilwat;
	int k;
	int delta;
	RealD *p;

	switch (pd)
	{
		case eSW_Day:
			delta = SW_Output[eSW_Interception].dy_row;
			p = p_rOUT[eSW_Interception][eSW_Day];
			p[delta + nrow_OUT[eSW_Day] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Day] * 1] = SW_Model.doy;
			p[delta + nrow_OUT[eSW_Day] * 2] = v->dysum.total_int;
			ForEachVegType(k) {
				p[delta + nrow_OUT[eSW_Day] * 3] = v->dysum.int_veg[k];
			}
			p[delta + nrow_OUT[eSW_Day] * 7] = v->dysum.litter_int;
			SW_Output[eSW_Interception].dy_row++;
			break;

		case eSW_Week:
			delta = SW_Output[eSW_Interception].wk_row;
			p = p_rOUT[eSW_Interception][eSW_Week];
			p[delta + nrow_OUT[eSW_Week] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Week] * 1] = (SW_Model.week + 1) - tOffset;
			p[delta + nrow_OUT[eSW_Week] * 2] = v->wkavg.total_int;
			ForEachVegType(k) {
				p[delta + nrow_OUT[eSW_Week] * 3] = v->wkavg.int_veg[k];
			}
			p[delta + nrow_OUT[eSW_Week] * 7] = v->wkavg.litter_int;
			SW_Output[eSW_Interception].wk_row++;
			break;

		case eSW_Month:
			delta = SW_Output[eSW_Interception].mo_row;
			p = p_rOUT[eSW_Interception][eSW_Month];
			p[delta + nrow_OUT[eSW_Month] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Month] * 1] = (SW_Model.month + 1) - tOffset;
			p[delta + nrow_OUT[eSW_Month] * 2] = v->moavg.total_int;
			ForEachVegType(k) {
				p[delta + nrow_OUT[eSW_Month] * 3] = v->moavg.int_veg[k];
			}
			p[delta + nrow_OUT[eSW_Month] * 7] = v->moavg.litter_int;
			SW_Output[eSW_Interception].mo_row++;
			break;

		case eSW_Year:
			delta = SW_Output[eSW_Interception].yr_row;
			p = p_rOUT[eSW_Interception][eSW_Year];
			p[delta + nrow_OUT[eSW_Year] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Year] * 1] = v->yravg.total_int;
			p[delta + nrow_OUT[eSW_Year] * 2] = v->yravg.int_veg[SW_TREES];
			ForEachVegType(k) {
				p[delta + nrow_OUT[eSW_Year] * 3] = v->yravg.int_veg[k];
			}
			p[delta + nrow_OUT[eSW_Year] * 6] = v->yravg.litter_int;
			SW_Output[eSW_Interception].yr_row++;
			break;
	}
}

void get_soilinf(OutPeriod pd)
{
	/* --------------------------------------------------- */
	/* 20100202 (drs) added */
	/* 20110219 (drs) added runoff */
	/* 12/13/2012	(clk)	moved runoff, now named snowRunoff, to get_runoffrunon(); */
	SW_WEATHER *v = &SW_Weather;
	int delta;
	RealD *p;

	switch (pd)
	{
		case eSW_Day:
			delta = SW_Output[eSW_SoilInf].dy_row;
			p = p_rOUT[eSW_SoilInf][eSW_Day];
			p[delta + nrow_OUT[eSW_Day] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Day] * 1] = SW_Model.doy;
			p[delta + nrow_OUT[eSW_Day] * 2] = v->dysum.soil_inf;
			SW_Output[eSW_SoilInf].dy_row++;
			break;

		case eSW_Week:
			delta = SW_Output[eSW_SoilInf].wk_row;
			p = p_rOUT[eSW_SoilInf][eSW_Week];
			p[delta + nrow_OUT[eSW_Week] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Week] * 1] = (SW_Model.week + 1) - tOffset;
			p[delta + nrow_OUT[eSW_Week] * 2] = v->wkavg.soil_inf;
			SW_Output[eSW_SoilInf].wk_row++;
			break;

		case eSW_Month:
			delta = SW_Output[eSW_SoilInf].mo_row;
			p = p_rOUT[eSW_SoilInf][eSW_Month];
			p[delta + nrow_OUT[eSW_Month] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Month] * 1] = (SW_Model.month + 1) - tOffset;
			p[delta + nrow_OUT[eSW_Month] * 2] = v->moavg.soil_inf;
			SW_Output[eSW_SoilInf].mo_row++;
			break;

		case eSW_Year:
			delta = SW_Output[eSW_SoilInf].yr_row;
			p_rOUT[eSW_SoilInf][eSW_Year][delta + nrow_OUT[eSW_Year] * 0] = SW_Model.simyear;
			p_rOUT[eSW_SoilInf][eSW_Year][delta + nrow_OUT[eSW_Year] * 1] = v->yravg.soil_inf;
			SW_Output[eSW_SoilInf].yr_row++;
			break;
	}
}

void get_lyrdrain(OutPeriod pd)
{
	/* --------------------------------------------------- */
	/* 20100202 (drs) added */
	LyrIndex i;
	SW_SOILWAT *v = &SW_Soilwat;
	int delta;
	RealD *p;

	switch (pd)
	{
		case eSW_Day:
			delta = SW_Output[eSW_LyrDrain].dy_row;
			p = p_rOUT[eSW_LyrDrain][eSW_Day];
			p[delta + nrow_OUT[eSW_Day] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Day] * 1] = SW_Model.doy;
			for (i = 0; i < SW_Site.n_layers - 1; i++)
			{
				p[delta + nrow_OUT[eSW_Day] * (i + 2)] = v->dysum.lyrdrain[i];
			}
			SW_Output[eSW_LyrDrain].dy_row++;
			break;

		case eSW_Week:
			delta = SW_Output[eSW_LyrDrain].wk_row;
			p = p_rOUT[eSW_LyrDrain][eSW_Week];
			p[delta + nrow_OUT[eSW_Week] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Week] * 1] = (SW_Model.week + 1) - tOffset;
			for (i = 0; i < SW_Site.n_layers - 1; i++)
			{
				p[delta + nrow_OUT[eSW_Week] * (i + 2)] = v->wkavg.lyrdrain[i];
			}
			SW_Output[eSW_LyrDrain].wk_row++;
			break;

		case eSW_Month:
			delta = SW_Output[eSW_LyrDrain].mo_row;
			p = p_rOUT[eSW_LyrDrain][eSW_Month];
			p[delta + nrow_OUT[eSW_Month] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Month] * 1] = (SW_Model.month + 1) - tOffset;
			for (i = 0; i < SW_Site.n_layers - 1; i++)
			{
				p[delta + nrow_OUT[eSW_Month] * (i + 2)] = v->moavg.lyrdrain[i];
			}
			SW_Output[eSW_LyrDrain].mo_row++;
			break;

		case eSW_Year:
			delta = SW_Output[eSW_LyrDrain].yr_row;
			p = p_rOUT[eSW_LyrDrain][eSW_Year];
			p[delta + nrow_OUT[eSW_Year] * 0] = SW_Model.simyear;
			for (i = 0; i < SW_Site.n_layers - 1; i++)
			{
				p[delta + nrow_OUT[eSW_Year] * (i + 1)] = v->yravg.lyrdrain[i];
			}
			SW_Output[eSW_LyrDrain].yr_row++;
			break;
	}
}

void get_hydred(OutPeriod pd)
{
	/* --------------------------------------------------- */
	/* 20101020 (drs) added */
	LyrIndex i;
	int k;
	SW_SOILWAT *v = &SW_Soilwat;
	int delta;
	RealD *p;

	switch (pd)
	{
		case eSW_Day:
			delta = SW_Output[eSW_HydRed].dy_row;
			p = p_rOUT[eSW_HydRed][eSW_Day];
			p[delta + nrow_OUT[eSW_Day] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Day] * 1] = SW_Model.doy;

			ForEachSoilLayer(i) {
				p[delta + nrow_OUT[eSW_Day] * (i + 2)] = v->dysum.hydred_total[i];
			}

			ForEachVegType(k) {
				ForEachSoilLayer(i) {
					p[delta + nrow_OUT[eSW_Day] * (i + 2) + (nrow_OUT[eSW_Day] * SW_Site.n_layers * (k + 1))] = v->dysum.hydred[k][i];
				}
			}

			SW_Output[eSW_HydRed].dy_row++;
			break;

		case eSW_Week:
			delta = SW_Output[eSW_HydRed].wk_row;
			p = p_rOUT[eSW_HydRed][eSW_Week];
			p[delta + nrow_OUT[eSW_Week] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Week] * 1] = (SW_Model.week + 1) - tOffset;

			ForEachSoilLayer(i) {
				p[delta + nrow_OUT[eSW_Week] * (i + 2)] = v->wkavg.hydred_total[i];
			}

			ForEachVegType(k) {
				ForEachSoilLayer(i) {
					p[delta + nrow_OUT[eSW_Week] * (i + 2) + (nrow_OUT[eSW_Week] * SW_Site.n_layers * (k + 1))] = v->wkavg.hydred[k][i];
				}
			}

			SW_Output[eSW_HydRed].wk_row++;
			break;

		case eSW_Month:
			delta = SW_Output[eSW_HydRed].mo_row;
			p = p_rOUT[eSW_HydRed][eSW_Month];
			p[delta + nrow_OUT[eSW_Month] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Month] * 1] = (SW_Model.month + 1) - tOffset;

			ForEachSoilLayer(i) {
				p[delta + nrow_OUT[eSW_Month] * (i + 2)] = v->moavg.hydred_total[i];
			}

			ForEachVegType(k) {
				ForEachSoilLayer(i) {
					p[delta + nrow_OUT[eSW_Month] * (i + 2) + (nrow_OUT[eSW_Month] * SW_Site.n_layers * (k + 1))] = v->moavg.hydred[k][i];
				}
			}

			SW_Output[eSW_HydRed].mo_row++;
			break;

		case eSW_Year:
			delta = SW_Output[eSW_HydRed].yr_row;
			p = p_rOUT[eSW_HydRed][eSW_Year];
			p[delta + nrow_OUT[eSW_Year] * 0] = SW_Model.simyear;

			ForEachSoilLayer(i) {
				p[delta + nrow_OUT[eSW_Year] * (i + 2)] = v->yravg.hydred_total[i];
			}

			ForEachVegType(k) {
				ForEachSoilLayer(i) {
					p[delta + nrow_OUT[eSW_Year] * (i + 1) + (nrow_OUT[eSW_Year] * SW_Site.n_layers * (k + 1))] = v->yravg.hydred[k][i];
				}
			}

			SW_Output[eSW_HydRed].yr_row++;
			break;
	}
}

void get_aet(OutPeriod pd)
{
	/* --------------------------------------------------- */
	SW_SOILWAT *v = &SW_Soilwat;
	int delta;
	RealD *p;

	switch (pd)
	{
		case eSW_Day:
			delta = SW_Output[eSW_AET].dy_row;
			p = p_rOUT[eSW_AET][eSW_Day];
			p[delta + nrow_OUT[eSW_Day] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Day] * 1] = SW_Model.doy;
			p[delta + nrow_OUT[eSW_Day] * 2] = v->dysum.aet;
			SW_Output[eSW_AET].dy_row++;
			break;

		case eSW_Week:
			delta = SW_Output[eSW_AET].wk_row;
			p = p_rOUT[eSW_AET][eSW_Week];
			p[delta + nrow_OUT[eSW_Week] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Week] * 1] = (SW_Model.week + 1) - tOffset;
			p[delta + nrow_OUT[eSW_Week] * 2] = v->wkavg.aet;
			SW_Output[eSW_AET].wk_row++;
			break;

		case eSW_Month:
			delta = SW_Output[eSW_AET].mo_row;
			p = p_rOUT[eSW_AET][eSW_Month];
			p[delta + nrow_OUT[eSW_Month] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Month] * 1] = (SW_Model.month + 1) - tOffset;
			p[delta + nrow_OUT[eSW_Month] * 2] = v->moavg.aet;
			SW_Output[eSW_AET].mo_row++;
			break;

		case eSW_Year:
			delta = SW_Output[eSW_AET].yr_row;
			p_rOUT[eSW_AET][eSW_Year][delta + nrow_OUT[eSW_Year] * 0] = SW_Model.simyear;
			p_rOUT[eSW_AET][eSW_Year][delta + nrow_OUT[eSW_Year] * 1] = v->yravg.aet;
			SW_Output[eSW_AET].yr_row++;
			break;
	}
}

void get_pet(OutPeriod pd)
{
	/* --------------------------------------------------- */
	SW_SOILWAT *v = &SW_Soilwat;
	int delta;
	RealD *p;

	switch (pd)
	{
		case eSW_Day:
			delta = SW_Output[eSW_PET].dy_row;
			p = p_rOUT[eSW_PET][eSW_Day];
			p[delta + nrow_OUT[eSW_Day] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Day] * 1] = SW_Model.doy;
			p[delta + nrow_OUT[eSW_Day] * 2] = v->dysum.pet;
			SW_Output[eSW_PET].dy_row++;
			break;

		case eSW_Week:
			delta = SW_Output[eSW_PET].wk_row;
			p = p_rOUT[eSW_PET][eSW_Week];
			p[delta + nrow_OUT[eSW_Week] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Week] * 1] = (SW_Model.week + 1) - tOffset;
			p[delta + nrow_OUT[eSW_Week] * 2] = v->wkavg.pet;
			SW_Output[eSW_PET].wk_row++;
			break;

		case eSW_Month:
			delta = SW_Output[eSW_PET].mo_row;
			p = p_rOUT[eSW_PET][eSW_Month];
			p[delta + nrow_OUT[eSW_Month] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Month] * 1] = (SW_Model.month + 1) - tOffset;
			p[delta + nrow_OUT[eSW_Month] * 2] = v->moavg.pet;
			SW_Output[eSW_PET].mo_row++;
			break;

		case eSW_Year:
			delta = SW_Output[eSW_PET].yr_row;
			p_rOUT[eSW_PET][eSW_Year][delta + nrow_OUT[eSW_Year] * 0] = SW_Model.simyear;
			p_rOUT[eSW_PET][eSW_Year][delta + nrow_OUT[eSW_Year] * 1] = v->yravg.pet;
			SW_Output[eSW_PET].yr_row++;
			break;
	}
}

void get_wetdays(OutPeriod pd)
{
	/* --------------------------------------------------- */
	LyrIndex i;
	SW_SOILWAT *v = &SW_Soilwat;
	int delta;
	RealD *p;


	switch (pd)
	{
		case eSW_Day:
			delta = SW_Output[eSW_WetDays].dy_row;
			p = p_rOUT[eSW_WetDays][eSW_Day];
			p[delta + nrow_OUT[eSW_Day] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Day] * 1] = SW_Model.doy;
			ForEachSoilLayer(i)
			{
				p[delta + nrow_OUT[eSW_Day] * (i + 2)] = (v->is_wet[i]) ? 1 : 0;
			}
			SW_Output[eSW_WetDays].dy_row++;
			break;

		case eSW_Week:
			delta = SW_Output[eSW_WetDays].wk_row;
			p = p_rOUT[eSW_WetDays][eSW_Week];
			p[delta + nrow_OUT[eSW_Week] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Week] * 1] = (SW_Model.week + 1) - tOffset;
			ForEachSoilLayer(i)
			{
				p[delta + nrow_OUT[eSW_Week] * (i + 2)] = (int) v->wkavg.wetdays[i];
			}
			SW_Output[eSW_WetDays].wk_row++;
			break;

		case eSW_Month:
			delta = SW_Output[eSW_WetDays].mo_row;
			p = p_rOUT[eSW_WetDays][eSW_Month];
			p[delta + nrow_OUT[eSW_Month] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Month] * 1] = (SW_Model.month + 1) - tOffset;
			ForEachSoilLayer(i)
			{
				p[delta + nrow_OUT[eSW_Month] * (i + 2)] = (int) v->moavg.wetdays[i];
			}
			SW_Output[eSW_WetDays].mo_row++;
			break;

		case eSW_Year:
			delta = SW_Output[eSW_WetDays].yr_row;
			p = p_rOUT[eSW_WetDays][eSW_Year];
			p[delta + nrow_OUT[eSW_Year] * 0] = SW_Model.simyear;
			ForEachSoilLayer(i)
			{
				p[delta + nrow_OUT[eSW_Year] * (i + 1)] = (int) v->yravg.wetdays[i];
			}
			SW_Output[eSW_WetDays].yr_row++;
			break;
	}
}

void get_snowpack(OutPeriod pd)
{
	/* --------------------------------------------------- */
	SW_SOILWAT *v = &SW_Soilwat;
	int delta;
	RealD *p;

	switch (pd)
	{
		case eSW_Day:
			delta = SW_Output[eSW_SnowPack].dy_row;
			p = p_rOUT[eSW_SnowPack][eSW_Day];
			p[delta + nrow_OUT[eSW_Day] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Day] * 1] = SW_Model.doy;
			p[delta + nrow_OUT[eSW_Day] * 2] = v->dysum.snowpack;
			p[delta + nrow_OUT[eSW_Day] * 3] = v->dysum.snowdepth;
			SW_Output[eSW_SnowPack].dy_row++;
			break;

		case eSW_Week:
			delta = SW_Output[eSW_SnowPack].wk_row;
			p = p_rOUT[eSW_SnowPack][eSW_Week];
			p[delta + nrow_OUT[eSW_Week] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Week] * 1] = (SW_Model.week + 1) - tOffset;
			p[delta + nrow_OUT[eSW_Week] * 2] = v->wkavg.snowpack;
			p[delta + nrow_OUT[eSW_Week] * 3] = v->wkavg.snowdepth;
			SW_Output[eSW_SnowPack].wk_row++;
			break;

		case eSW_Month:
			delta = SW_Output[eSW_SnowPack].mo_row;
			p = p_rOUT[eSW_SnowPack][eSW_Month];
			p[delta + nrow_OUT[eSW_Month] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Month] * 1] = (SW_Model.month + 1) - tOffset;
			p[delta + nrow_OUT[eSW_Month] * 2] = v->moavg.snowpack;
			p[delta + nrow_OUT[eSW_Month] * 3] = v->moavg.snowdepth;
			SW_Output[eSW_SnowPack].mo_row++;
			break;

		case eSW_Year:
		delta = SW_Output[eSW_SnowPack].yr_row;
			p = p_rOUT[eSW_SnowPack][eSW_Year];
			p[delta + nrow_OUT[eSW_Year] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Year] * 1] = v->yravg.snowpack;
			p[delta + nrow_OUT[eSW_Year] * 2] = v->yravg.snowdepth;
			SW_Output[eSW_SnowPack].yr_row++;
			break;
	}
}

void get_deepswc(OutPeriod pd)
{
	/* --------------------------------------------------- */
	SW_SOILWAT *v = &SW_Soilwat;
	int delta;
	RealD *p;

	switch (pd)
	{
		case eSW_Day:
			delta = SW_Output[eSW_DeepSWC].dy_row;
			p = p_rOUT[eSW_DeepSWC][eSW_Day];
			p[delta + nrow_OUT[eSW_Day] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Day] * 1] = SW_Model.doy;
			p[delta + nrow_OUT[eSW_Day] * 2] = v->dysum.deep;
			SW_Output[eSW_DeepSWC].dy_row++;
			break;

		case eSW_Week:
			delta = SW_Output[eSW_DeepSWC].wk_row;
			p = p_rOUT[eSW_DeepSWC][eSW_Week];
			p[delta + nrow_OUT[eSW_Week] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Week] * 1] = (SW_Model.week + 1) - tOffset;
			p[delta + nrow_OUT[eSW_Week] * 2] = v->wkavg.deep;
			SW_Output[eSW_DeepSWC].wk_row++;
			break;

		case eSW_Month:
			delta = SW_Output[eSW_DeepSWC].mo_row;
			p = p_rOUT[eSW_DeepSWC][eSW_Month];
			p[delta + nrow_OUT[eSW_Month] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Month] * 1] = (SW_Model.month + 1) - tOffset;
			p[delta + nrow_OUT[eSW_Month] * 2] = v->moavg.deep;
			SW_Output[eSW_DeepSWC].mo_row++;
			break;

		case eSW_Year:
			delta = SW_Output[eSW_DeepSWC].yr_row;
			p = p_rOUT[eSW_DeepSWC][eSW_Year];
			p[delta + nrow_OUT[eSW_Year] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Year] * 1] = v->yravg.deep;
			SW_Output[eSW_DeepSWC].yr_row++;
			break;
	}
}

void get_soiltemp(OutPeriod pd)
{
	/* --------------------------------------------------- */
	LyrIndex i;
	SW_SOILWAT *v = &SW_Soilwat;
	int delta;
	RealD *p;

	switch (pd)
	{
		case eSW_Day:
			delta = SW_Output[eSW_SoilTemp].dy_row;
			p = p_rOUT[eSW_SoilTemp][eSW_Day];
			p[delta + nrow_OUT[eSW_Day] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Day] * 1] = SW_Model.doy;
			ForEachSoilLayer(i)
			{
				p[delta + nrow_OUT[eSW_Day] * (i + 2)] = v->dysum.sTemp[i];
			}
			SW_Output[eSW_SoilTemp].dy_row++;
			break;

		case eSW_Week:
			delta = SW_Output[eSW_SoilTemp].wk_row;
			p = p_rOUT[eSW_SoilTemp][eSW_Week];
			p[delta + nrow_OUT[eSW_Week] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Week] * 1] = (SW_Model.week + 1) - tOffset;
			ForEachSoilLayer(i)
			{
				p[delta + nrow_OUT[eSW_Week] * (i + 2)] = v->wkavg.sTemp[i];
			}
			SW_Output[eSW_SoilTemp].wk_row++;
			break;

		case eSW_Month:
			delta = SW_Output[eSW_SoilTemp].mo_row;
			p = p_rOUT[eSW_SoilTemp][eSW_Month];
			p[delta + nrow_OUT[eSW_Month] * 0] = SW_Model.simyear;
			p[delta + nrow_OUT[eSW_Month] * 1] = (SW_Model.month + 1) - tOffset;
			ForEachSoilLayer(i)
			{
				p[delta + nrow_OUT[eSW_Month] * (i + 2)] = v->moavg.sTemp[i];
			}
			SW_Output[eSW_SoilTemp].mo_row++;
			break;

		case eSW_Year:
			delta = SW_Output[eSW_SoilTemp].yr_row;
			p = p_rOUT[eSW_SoilTemp][eSW_Year];
			p[delta + nrow_OUT[eSW_Year] * 0] = SW_Model.simyear;
			ForEachSoilLayer(i)
			{
				p[delta + nrow_OUT[eSW_Year] * (i + 1)] = v->yravg.sTemp[i];
			}
			SW_Output[eSW_SoilTemp].yr_row++;
			break;
	}
}
