/********************************************************/
/********************************************************/
/*  Source file: SW_Weather.h
 Type: header
 Application: SOILWAT - soilwater dynamics simulator
 Purpose: Support definitions/declarations for
 weather-related information.
 History:
 (8/28/01) -- INITIAL CODING - cwb
 20091014 (drs) added pct_snowdrift as input to weathsetup.in
 20091015 (drs) ppt is divided into rain and snow, added snowmelt
 01/04/2011	(drs) added variable 'snowloss' to SW_WEATHER_2DAYS and to SW_WEATHER_OUTPUTS
 02/16/2011	(drs) added variable 'pct_runoff' to SW_WEATHER as input to weathsetup.in
 02/19/2011	(drs) added variable 'runoff' to SW_WEATHER_2DAYS and to SW_WEATHER_OUTPUTS
 moved soil_inf from SW_Soilwat to SW_Weather (added to SW_WEATHER and to SW_WEATHER_OUTPUTS)
 06/01/2012  (DLM) added temp_year_avg variable to SW_WEATHER_HIST struct & temp_month_avg[MAX_MONTHS] variable
 11/30/2012	(clk) added variable 'surfaceRunoff' to SW_WEATHER and SW_WEATHER_OUTPUTS
 changed 'runoff' to 'snowRunoff' to better distinguish between surface runoff and snowmelt runoff

 */
/********************************************************/
/********************************************************/

#ifndef SW_WEATHER_H
#define SW_WEATHER_H

#include "SW_Times.h"
#ifdef RSOILWAT
#include <R.h>
#include <Rdefines.h>
#include <Rconfig.h>
#include <Rinterface.h>
#include <Rinternals.h>
#endif

/* missing values may be different than with other things */
#define WTH_MISSING   999.

/*  all temps are in degrees C, all precip is in cm */
/*  in fact, all water variables are in cm throughout
 *  the model.  this facilitates additions and removals
 *  as they're always in the right units.
 */

typedef struct {
	/* comes from markov weather day-to-day */
	RealD temp_avg[TWO_DAYS], temp_run_avg[TWO_DAYS], temp_yr_avg, /* year's avg for STEPPE */
	temp_max[TWO_DAYS], temp_min[TWO_DAYS], ppt[TWO_DAYS], /* 20091015 (drs) ppt is divided into rain and snow */
	rain[TWO_DAYS], snow[TWO_DAYS], snowmelt[TWO_DAYS], snowloss[TWO_DAYS], ppt_actual[TWO_DAYS], /* 20091015 (drs) was here previously, but not used in code as of today */
	gsppt; /* gr. season ppt only needs one day */

} SW_WEATHER_2DAYS;

typedef struct {
	/* comes from historical weather files */
	RealD temp_max[MAX_DAYS], temp_min[MAX_DAYS], temp_avg[MAX_DAYS], ppt[MAX_DAYS], temp_month_avg[MAX_MONTHS], temp_year_avg;
} SW_WEATHER_HIST;

/* accumulators for output values hold only the */
/* current period's values (eg, weekly or monthly) */
typedef struct {
	RealD temp_max, temp_min, temp_avg, ppt, rain, snow, snowmelt, snowloss, /* 20091015 (drs) ppt is divided into rain and snow */
	snowRunoff, surfaceRunoff, soil_inf, et, aet, pet;
} SW_WEATHER_OUTPUTS;

typedef struct {

	Bool use_markov, /* TRUE=use markov for any year missing a weather */
	/*      file, which means markov must be initialized */
	/* FALSE = fail if any weather file is missing.  */
	use_snow;
	RealD pct_snowdrift, pct_snowRunoff;
	TimeInt days_in_runavg;
	SW_TIMES yr;
	RealD scale_precip[MAX_MONTHS], scale_temp_max[MAX_MONTHS], scale_temp_min[MAX_MONTHS],
		scale_skyCover[MAX_MONTHS], scale_wind[MAX_MONTHS], scale_rH[MAX_MONTHS], scale_transmissivity[MAX_MONTHS];
	char name_prefix[MAX_FILENAMESIZE];
	RealD snowRunoff, surfaceRunoff, soil_inf;

	/* This section is required for computing the output quantities.  */
	SW_WEATHER_OUTPUTS dysum, /* helpful placeholder */
	wksum, mosum, yrsum, /* accumulators for *avg */
	wkavg, moavg, yravg; /* averages or sums as appropriate*/
	SW_WEATHER_HIST hist;
	SW_WEATHER_2DAYS now;

} SW_WEATHER;

void SW_WTH_read(void);
void SW_WTH_init(void);
void SW_WTH_construct(void);
void SW_WTH_new_day(void);
void SW_WTH_new_year(void);
void SW_WTH_sum_today(void);
void SW_WTH_end_day(void);
void SW_WTH_clear_runavg_list(void);

#ifdef RSOILWAT
SEXP onGet_SW_WTH();
void onSet_SW_WTH(SEXP SW_WTH);
SEXP onGet_WTH_DATA(void);
SEXP onGet_WTH_DATA_YEAR(TimeInt year);
Bool onSet_WTH_DATA(SEXP WTH_DATA_YEAR, TimeInt year);
#endif

#ifdef DEBUG_MEM
void SW_WTH_SetMemoryRefs(void);
#endif

#endif
