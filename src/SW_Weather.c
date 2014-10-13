/********************************************************/
/********************************************************/
/*	Source file: Weather.c
 Type: class
 Application: SOILWAT - soilwater dynamics simulator
 Purpose: Read / write and otherwise manage the model's
 weather-related information.
 History:
 (8/28/01) -- INITIAL CODING - cwb
 12/02 - IMPORTANT CHANGE - cwb
 refer to comments in Times.h regarding base0
 20090916 (drs) changed  in SW_WTH_new_day() the following code:
 wn->temp_avg[Today]     = (tmpmax + tmpmin) / 2.;
 to wn->temp_avg[Today]     = (wn->temp_max[Today] + wn->temp_min[Today]) / 2.;
 so that monthly scaling factors also influence average T and not only Tmin and Tmax
 20091009 (drs) moved call to SW_SWC_adjust_snow () to SW_Flow.c
 and split it up into snow accumulation and snow melt
 20091014 (drs) added pct_snowdrift as input to weathsetup.in
 20091015 (drs) ppt is divided into rain and snow, added snowmelt
 20101004 (drs) moved call to SW_SWC_adjust_snow() from SW_Flow.c back to SW_WTH_new_day(), see comment 20091009
 01/04/2011	(drs) added parameter '*snowloss' to function SW_SWC_adjust_snow() call and updated function SW_WTH_new_year()
 01/05/2011	(drs) removed unused variables rain and snow from SW_WTH_new_day()
 02/16/2011	(drs) added pct_runoff read from input file to SW_WTH_read()
 02/19/2011	(drs) to track 'runoff', updated functions SW_WTH_new_year(), SW_WTH_new_day()
 moved soil_inf from SW_Soilwat to SW_Weather
 09/30/2011	(drs)	weather name prefix no longer read in from file weathersetup.in with function SW_WTH_read(), but extracted from SW_Files.c:SW_WeatherPrefix()
 01/13/2011	(drs)	function '_read_hist' didn't close opened files: after reaching OS-limit of openend connections, no files could be read any more -> added 'fclose(f);' to close open connections after use
 06/01/2012  (DLM) edited _read_hist() function to calculate the yearly avg air temperature & the monthly avg air temperatures...
 11/30/2010	(clk) added appopriate lines for 'surfaceRunoff' in SW_WTH_new_year() and SW_WTH_new_day();
 06/21/2013	(DLM)	variable 'tail' got too large by 1 and leaked memory when accessing array runavg_list[] in function _runavg_temp(): changed test from '(tail < SW_Weather.days_in_runavg)' to '(tail < (SW_Weather.days_in_runavg-1))'
 in function _clear_hist_weather() temp_max was tested twice, one should be temp_min
 06/24/2013	(rjm) added function void SW_WTH_clear_runavg_list(void) to free memory
 06/24/2013	(rjm) 	made 'tail' and 'firsttime' a module-level static variable (instead of function-level): otherwise it will not get reset to 0 between consecutive calls as a dynamic library
 need to set these variables to 0 resp TRUE in function SW_WTH_construct()
 06/27/2013	(drs)	closed open files if LogError() with LOGFATAL is called in SW_WTH_read(), _read_hist()
 08/26/2013 (rjm) removed extern SW_OUTPUT never used.
 */
/********************************************************/
/********************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "generic.h"
#include "filefuncs.h"
#include "myMemory.h"
#include "Times.h"
#include "SW_Defines.h"
#include "SW_Files.h"
#include "SW_Model.h"
#include "SW_Output.h"
#include "SW_SoilWater.h"
#include "SW_Weather.h"
#include "SW_Markov.h"
#include "SW_Sky.h"

/* =================================================== */
/*                  Global Variables                   */
/* --------------------------------------------------- */
extern SW_MODEL SW_Model;
extern SW_MARKOV SW_Markov;
//extern SW_OUTPUT SW_Output[];
#ifdef RSOILWAT
extern Bool useFiles;
extern SEXP InputData;
extern SEXP WeatherList;
extern Bool bWeatherList;
extern Bool collectInData;
#endif
SW_WEATHER SW_Weather; /* declared here, externed elsewhere */

/* =================================================== */
/*                Module-Level Variables               */
/* --------------------------------------------------- */
static char *MyFileName;
static RealD *runavg_list; /* used in run_tmp_avg() */
static Bool weth_found; /* TRUE=success reading this years weather file */
static TimeInt tail;
static Bool firsttime;
static int wthdataIndex;

/* =================================================== */
/* =================================================== */
/*             Private Function Definitions            */
/* --------------------------------------------------- */

static Bool _read_hist(TimeInt year);
static void _update_yesterday(void);

static void _clear_hist_weather(void) {
	/* --------------------------------------------------- */
	SW_WEATHER_HIST *wh = &SW_Weather.hist;
	TimeInt d;
	int i;
	SW_WEATHER_OUTPUTS *wo[7] = { &SW_Weather.dysum, &SW_Weather.wksum, &SW_Weather.mosum, &SW_Weather.yrsum, &SW_Weather.wkavg, &SW_Weather.moavg, &SW_Weather.yravg };

	for (d = 0; d < MAX_DAYS; d++)
		wh->ppt[d] = wh->temp_max[d] = wh->temp_min[d] = WTH_MISSING;

	for (i = 0; i < 7; i++)
		memset(wo[i], 0, sizeof(SW_WEATHER_OUTPUTS));
}

static void _clear_runavg(void) {
	/* --------------------------------------------------- */
	TimeInt i;

	for (i = 0; i < SW_Weather.days_in_runavg; i++)
		runavg_list[i] = WTH_MISSING;
}

void SW_WTH_clear_runavg_list(void) {
	free(runavg_list);
	runavg_list = NULL;
}

static RealD _runavg_temp(RealD avg) {
	/* --------------------------------------------------- */
	int i, cnt, numdays;
	RealD sum = 0.;

	runavg_list[tail] = avg;
	numdays = (SW_Model.doy < SW_Weather.days_in_runavg) ? SW_Model.doy : SW_Weather.days_in_runavg;

	for (i = 0, cnt = 0; i < numdays; i++, cnt++) {
		if (!missing(runavg_list[i])) {
			sum += runavg_list[i];
		}
	}
	tail = (tail < (SW_Weather.days_in_runavg - 1)) ? tail + 1 : 0;
	return ((cnt) ? sum / cnt : WTH_MISSING);
}

static void _todays_weth(RealD *tmax, RealD *tmin, RealD *ppt) {
	/* --------------------------------------------------- */
	/* If use_markov=FALSE and no weather file found, we won't
	 * get this far because the new_year() will fail, so if
	 * no weather file found and we make it here, use_markov=TRUE
	 * and we call mkv_today().  Otherwise, we're using this
	 * year's weather file and this logic sets today's value
	 * to yesterday's if today's is missing.  This may not
	 * always be most desirable, especially for ppt, so its
	 * default is 0.
	 */
	SW_WEATHER *w = &SW_Weather;
	TimeInt doy = SW_Model.doy - 1;

	if (!weth_found) {
		*ppt = w->now.ppt[Yesterday]; /* reqd for markov */
		SW_MKV_today(doy, tmax, tmin, ppt);

	} else {
		*tmax = (!missing(w->hist.temp_max[doy])) ? w->hist.temp_max[doy] : w->now.temp_max[Yesterday];
		*tmin = (!missing(w->hist.temp_min[doy])) ? w->hist.temp_min[doy] : w->now.temp_min[Yesterday];
		*ppt = (!missing(w->hist.ppt[doy])) ? w->hist.ppt[doy] : 0.;
	}

}

/* =================================================== */
/* =================================================== */
/*             Public Function Definitions             */
/* --------------------------------------------------- */
void SW_WTH_construct(void) {
	/* =================================================== */
	tail = 0;
	firsttime = TRUE;
	/* clear the module structure */
	memset(&SW_Weather, 0, sizeof(SW_WEATHER));
	SW_Markov.ppt_events = 0;
	wthdataIndex = 0;
}

void SW_WTH_init(void) {
	/* =================================================== */
	/* nothing to initialize */
	/* this is a stub to make all objects more consistent */

}

void SW_WTH_new_year(void) {
	/* =================================================== */
	SW_WEATHER_2DAYS *wn = &SW_Weather.now;
	TimeInt year = SW_Model.year;
#ifdef RSOILWAT
	int i=0;
#endif
	_clear_runavg();
	memset(&SW_Weather.yrsum, 0, sizeof(SW_WEATHER_OUTPUTS));

	if (year < SW_Weather.yr.first) {
		weth_found = FALSE;
	} else {
#ifndef RSOILWAT
		weth_found = _read_hist(year);
#else
		weth_found = FALSE;
		if(bWeatherList) {
			for(i=0;i<LENGTH(WeatherList);i++) {
				if( year == *INTEGER(GET_SLOT(VECTOR_ELT( WeatherList, i),install("year"))) ) {
					weth_found = onSet_WTH_DATA(GET_SLOT(VECTOR_ELT( WeatherList, i),install("data")), year);
				}
			}
		} else {
			for(i=0;i<LENGTH(GET_SLOT(InputData,install("weatherHistory")));i++) {
				if( year == *INTEGER(GET_SLOT(VECTOR_ELT( GET_SLOT(InputData,install("weatherHistory")), i),install("year"))) ) {
					weth_found = onSet_WTH_DATA(GET_SLOT(VECTOR_ELT( GET_SLOT(InputData,install("weatherHistory")), i),install("data")), year);
				}
			}
		}
		wthdataIndex++;
#endif
	}

	if (!weth_found && !SW_Weather.use_markov) {
		LogError(logfp, LOGFATAL, "Markov Simulator turned off and weather file found not for year %d", year);
	}

	/* setup today's weather because it's used as a default
	 * value when weather for the first day is missing.
	 * Notice that temps of 0. are reasonable for January
	 * (doy=1) and are below the critical temps for freezing
	 * and with ppt=0 there's nothing to freeze.
	 */
	if (!weth_found && firsttime) {
		wn->temp_max[Today] = wn->temp_min[Today] = wn->ppt[Today] = wn->rain[Today] = wn->snow[Today] = wn->snowmelt[Today] = wn->snowloss[Today] = wn->gsppt = 0.;

		SW_Weather.snowRunoff = SW_Weather.surfaceRunoff = SW_Weather.soil_inf = 0.;
	}

	firsttime = FALSE;
}

void SW_WTH_end_day(void) {
	/* =================================================== */
	_update_yesterday();
}

void SW_WTH_new_day(void) {
	/* =================================================== */
	/* guarantees that today's weather will not be invalid
	 * via _todays_weth()
	 *
	 *  20-Jul-2002 -- added growing season computation to
	 *                 facilitate the steppe/soilwat interface.
	 *  06-Dec-2002 -- modified the seasonal computation to
	 *                 account for n-s hemispheres.
	 *	16-Sep-2009 -- (drs) scaling factors were only applied to Tmin and Tmax
	 *					but not to Taverage -> corrected
	 *	09-Oct-2009	-- (drs) commented out snow adjustement, because call moved to SW_Flow.c
	 * 	20091015 (drs) ppt is divided into rain and snow
	 */

	SW_WEATHER *w = &SW_Weather;
	SW_WEATHER_2DAYS *wn = &SW_Weather.now;
	RealD tmpmax, tmpmin, ppt;
	TimeInt month = SW_Model.month;

#ifdef STEPWAT
	/*
	 TimeInt doy = SW_Model.doy;
	 Bool is_warm;
	 Bool is_growingseason = swFALSE;
	 */
#endif

	/* get the plain unscaled values */
	_todays_weth(&tmpmax, &tmpmin, &ppt);

	/* scale the weather according to monthly factors */
	wn->temp_max[Today] = tmpmax + w->scale_temp_max[month];
	wn->temp_min[Today] = tmpmin + w->scale_temp_min[month];
	wn->ppt_actual[Today] = ppt;

	wn->temp_avg[Today] = (wn->temp_max[Today] + wn->temp_min[Today]) / 2.;
	wn->temp_run_avg[Today] = _runavg_temp(wn->temp_avg[Today]);

	ppt *= w->scale_precip[month];

	wn->ppt[Today] = wn->rain[Today] = ppt;
	wn->snowmelt[Today] = wn->snowloss[Today] = wn->snow[Today] = 0.;
	w->snowRunoff = w->surfaceRunoff = w->soil_inf = 0.;

	if (w->use_snow)
		SW_SWC_adjust_snow(wn->temp_min[Today], wn->temp_max[Today], wn->ppt[Today], &wn->rain[Today], &wn->snow[Today], &wn->snowmelt[Today], &wn->snowloss[Today]);
#ifdef STEPWAT
	/* This is a nice idea but doesn't work as I'd like, so
	 * I'll go back to letting STEPPE handle it for now.

	 is_warm = (wn->temp_run_avg[Today] > 1.0);

	 is_growingseason = (doy >= m->startstart && doy <= m->daymid)
	 ? is_warm
	 : (is_growingseason && is_warm);
	 if (doy >= m->startstart && doy <= m->daymid && is_growingseason)
	 wn->gsppt = wn->gsppt + wn->ppt[Today];
	 else if (is_growingseason)
	 wn->gsppt += wn->ppt[Today];
	 */
#endif

}

void SW_WTH_read(void) {
	/* =================================================== */
	SW_WEATHER *w = &SW_Weather;
	const int nitems = 18;
	FILE *f;
	int lineno = 0, month, x;
	RealF sppt, stmax, stmin;
	RealF sky,wind,rH,transmissivity;

	MyFileName = SW_F_name(eWeather);
	f = OpenFile(MyFileName, "r");

	while (GetALine(f, inbuf)) {
		switch (lineno) {
		case 0:
			w->use_snow = itob(atoi(inbuf));
			break;
		case 1:
			w->pct_snowdrift = atoi(inbuf);
			break;
		case 2:
			w->pct_snowRunoff = atoi(inbuf);
			break;
		case 3:
			w->use_markov = itob(atoi(inbuf));
			break;
		case 4:
			w->yr.first = YearTo4Digit(atoi(inbuf));
			break;
		case 5:
			w->days_in_runavg = atoi(inbuf);
			runavg_list = (RealD *) Mem_Calloc(w->days_in_runavg, sizeof(RealD), "SW_WTH_read()");
			break;
		default:
			if (lineno == 6 + MAX_MONTHS)
				break;
			x = sscanf(inbuf, "%d %f %f %f %f %f %f %f", &month, &sppt, &stmax, &stmin,&sky,&wind,&rH,&transmissivity);
			if (x < 4) {
				CloseFile(&f);
				LogError(logfp, LOGFATAL, "%s : Bad record %d.", MyFileName, lineno);
			}
			w->scale_precip[month - 1] = sppt;
			w->scale_temp_max[month - 1] = stmax;
			w->scale_temp_min[month - 1] = stmin;
			w->scale_skyCover[month - 1] = sky;
			w->scale_wind[month - 1] = wind;
			w->scale_rH[month - 1] = rH;
			w->scale_transmissivity[month - 1] = transmissivity;
		}
		lineno++;
	}
	SW_WeatherPrefix(w->name_prefix);
	CloseFile(&f);
	if (lineno < nitems - 1) {
		LogError(logfp, LOGFATAL, "%s : Too few input lines.", MyFileName);
	}
	w->yr.last = SW_Model.endyr;
	w->yr.total = w->yr.last - w->yr.first + 1;
	if (w->use_markov) {
		SW_MKV_construct();
		if (!SW_MKV_read_prob()) {
			LogError(logfp, LOGFATAL, "%s: Markov weather requested but could not open %s", MyFileName, SW_F_name(eMarkovProb));
		}
		if (!SW_MKV_read_cov()) {
			LogError(logfp, LOGFATAL, "%s: Markov weather requested but could not open %s", MyFileName, SW_F_name(eMarkovCov));
		}
	} else if (SW_Model.startyr < w->yr.first) {
		LogError(logfp, LOGFATAL, "%s : Model year (%d) starts before weather files (%d)"
				" and use_Markov=FALSE.\nPlease synchronize the years"
				" or set up the Markov weather files", MyFileName, SW_Model.startyr, w->yr.first);
	}
	/* else we assume weather files match model run years */

	/* required for PET */
	SW_SKY_read();
#ifndef RSOILWAT
	SW_SKY_init(w->scale_skyCover, w->scale_wind, w->scale_rH, w->scale_transmissivity);
#else
	if(!collectInData)
		SW_SKY_init(w->scale_skyCover, w->scale_wind, w->scale_rH, w->scale_transmissivity);
#endif
}
#ifdef RSOILWAT
SEXP onGet_SW_WTH() {
	int i;
	RealD *p_MonthlyValues;
	SW_WEATHER *w = &SW_Weather;

	SEXP swWeather;
	SEXP SW_WTH, SW_WTH_names;

	SEXP use_snow, pct_snowdrift, pct_snowRunoff, use_markov, yr_first, days_in_runavg;
	SEXP MonthlyScalingParams, MonthlyScalingParams_names, MonthlyScalingParams_names_x, MonthlyScalingParams_names_y;

	char *cSW_WTH_names[] = {"UseSnow", "pct_SnowDrift", "pct_SnowRunoff", "use_Markov", "FirstYear_Historical", "DaysRunningAverage", "MonthlyScalingParams"};
	char *cMonthlyScalingParams_names[] = {"PPT", "MaxT", "MinT","SkyCover","Wind","rH","Transmissivity"};
	char *cMonths[] = {"January","February","March","April","May","June","July","August","September","October","November","December"};

	PROTECT(swWeather = MAKE_CLASS("swWeather"));
	PROTECT(SW_WTH = NEW_OBJECT(swWeather)); //allocVector(VECSXP, 9 + w->use_markov)

	PROTECT(use_snow = NEW_LOGICAL(1));
	LOGICAL_POINTER(use_snow)[0] = w->use_snow;
	PROTECT(pct_snowdrift = NEW_NUMERIC(1));
	REAL(pct_snowdrift)[0] = w->pct_snowdrift;
	PROTECT(pct_snowRunoff = NEW_NUMERIC(1));
	REAL(pct_snowRunoff)[0] = w->pct_snowRunoff;
	PROTECT(use_markov = NEW_LOGICAL(1));
	LOGICAL_POINTER(use_markov)[0] = w->use_markov;
	PROTECT(yr_first = NEW_INTEGER(1));
	INTEGER_POINTER(yr_first)[0] = w->yr.first;
	PROTECT(days_in_runavg = NEW_INTEGER(1));
	INTEGER_POINTER(days_in_runavg)[0] = w->days_in_runavg;

	PROTECT(MonthlyScalingParams = allocMatrix(REALSXP,12,7));
	p_MonthlyValues = REAL(MonthlyScalingParams);
	for (i = 0; i < 12; i++) {
		p_MonthlyValues[i + 12 * 0] = w->scale_precip[i];
		p_MonthlyValues[i + 12 * 1] = w->scale_temp_max[i];
		p_MonthlyValues[i + 12 * 2] = w->scale_temp_min[i];
		p_MonthlyValues[i + 12 * 3] = w->scale_skyCover[i];
		p_MonthlyValues[i + 12 * 4] = w->scale_wind[i];
		p_MonthlyValues[i + 12 * 5] = w->scale_rH[i];
		p_MonthlyValues[i + 12 * 6] = w->scale_transmissivity[i];
	}
	PROTECT(MonthlyScalingParams_names = allocVector(VECSXP, 2));
	PROTECT(MonthlyScalingParams_names_x = allocVector(STRSXP,12));
	for (i = 0; i < 12; i++)
		SET_STRING_ELT(MonthlyScalingParams_names_x, i, mkChar(cMonths[i]));
	PROTECT(MonthlyScalingParams_names_y = allocVector(STRSXP,7));
	for (i = 0; i < 7; i++)
		SET_STRING_ELT(MonthlyScalingParams_names_y, i, mkChar(cMonthlyScalingParams_names[i]));
	SET_VECTOR_ELT(MonthlyScalingParams_names, 0, MonthlyScalingParams_names_x);
	SET_VECTOR_ELT(MonthlyScalingParams_names, 1, MonthlyScalingParams_names_y);
	setAttrib(MonthlyScalingParams, R_DimNamesSymbol, MonthlyScalingParams_names);

	SET_SLOT(SW_WTH, install(cSW_WTH_names[0]), use_snow);
	SET_SLOT(SW_WTH, install(cSW_WTH_names[1]), pct_snowdrift);
	SET_SLOT(SW_WTH, install(cSW_WTH_names[2]), pct_snowRunoff);
	SET_SLOT(SW_WTH, install(cSW_WTH_names[3]), use_markov);
	SET_SLOT(SW_WTH, install(cSW_WTH_names[4]), yr_first);
	SET_SLOT(SW_WTH, install(cSW_WTH_names[5]), days_in_runavg);
	SET_SLOT(SW_WTH, install(cSW_WTH_names[6]), MonthlyScalingParams);

	UNPROTECT(12);
	return SW_WTH;
}
void onSet_SW_WTH(SEXP SW_WTH) {
	int i;
	SW_WEATHER *w = &SW_Weather;
	SEXP use_snow, pct_snowdrift, pct_snowRunoff, use_markov, yr_first, days_in_runavg;
	SEXP MonthlyScalingParams;
	SEXP SW_SKY;
	SEXP SW_MKV, SW_MKV_prob, SW_MKV_conv;
	RealD *p_MonthlyValues;
	char *cSW_WTH_names[] = {"UseSnow", "pct_SnowDrift", "pct_SnowRunoff", "use_Markov", "FirstYear_Historical", "DaysRunningAverage", "MonthlyScalingParams", "Cloud",
		"weatherYearsIn", "markov"};

	MyFileName = SW_F_name(eWeather);

	PROTECT(use_snow = GET_SLOT(SW_WTH, install(cSW_WTH_names[0])));
	w->use_snow = (Bool) *INTEGER(use_snow);
	PROTECT(pct_snowdrift = GET_SLOT(SW_WTH, install(cSW_WTH_names[1])));
	w->pct_snowdrift = *REAL(pct_snowdrift);
	PROTECT(pct_snowRunoff = GET_SLOT(SW_WTH, install(cSW_WTH_names[2])));
	w->pct_snowRunoff = *REAL(pct_snowRunoff);
	PROTECT(use_markov = GET_SLOT(SW_WTH, install(cSW_WTH_names[3])));
	w->use_markov = (Bool) *INTEGER(use_markov);
	PROTECT(yr_first = GET_SLOT(SW_WTH, install(cSW_WTH_names[4])));
	w->yr.first = *INTEGER(yr_first);
	PROTECT(days_in_runavg = GET_SLOT(SW_WTH, install(cSW_WTH_names[5])));
	w->days_in_runavg = *INTEGER(days_in_runavg);
	runavg_list = (RealD *) Mem_Calloc(w->days_in_runavg, sizeof(RealD), "SW_WTH_read()");
	PROTECT(MonthlyScalingParams = GET_SLOT(SW_WTH, install(cSW_WTH_names[6])));
	p_MonthlyValues = REAL(MonthlyScalingParams);
	for (i = 0; i < 12; i++) {
		w->scale_precip[i] = p_MonthlyValues[i + 12 * 0];
		w->scale_temp_max[i] = p_MonthlyValues[i + 12 * 1];
		w->scale_temp_min[i] = p_MonthlyValues[i + 12 * 2];
		w->scale_skyCover[i] = p_MonthlyValues[i + 12 * 3];
		w->scale_wind[i] = p_MonthlyValues[i + 12 * 4];
		w->scale_rH[i] = p_MonthlyValues[i + 12 * 5];
		w->scale_transmissivity[i] = p_MonthlyValues[i + 12 * 6];
	}

	SW_WeatherPrefix(w->name_prefix);

	w->yr.last = SW_Model.endyr;
	w->yr.total = w->yr.last - w->yr.first + 1;

	if (w->use_markov) {
		PROTECT(SW_MKV = GET_SLOT(InputData, install("markov")));
		PROTECT(SW_MKV_prob = GET_SLOT(SW_MKV, install("Prob")));
		PROTECT(SW_MKV_conv = GET_SLOT(SW_MKV, install("Conv")));
		SW_MKV_construct();
		if (!onSet_MKV_prob(SW_MKV_prob)) {
			LogError(logfp, LOGFATAL, "Markov_prob: Something Went Wrong");
		}
		if (!onSet_MKV_conv(SW_MKV_conv)) {
			LogError(logfp, LOGFATAL, "Markov_conv: Something Went Wrong");
		}
	} else if (SW_Model.startyr < w->yr.first) {
		LogError(logfp, LOGFATAL, "weathersetup.in : Model year (%d) starts before weather files (%d)"
				" and use_Markov=swFALSE.\nPlease synchronize the years"
				" or set up the Markov weather files", SW_Model.startyr, w->yr.first);
	}

	PROTECT(SW_SKY = GET_SLOT(InputData, install("cloud")));
	onSet_SW_SKY(SW_SKY);
	SW_SKY_init(w->scale_skyCover, w->scale_wind, w->scale_rH, w->scale_transmissivity);
	if(w->use_markov)
		UNPROTECT(11);
	else
		UNPROTECT(8);
}
SEXP onGet_WTH_DATA(void) {
	TimeInt year;
	SEXP WTH_DATA, WTH_DATA_names;
	Bool setNames = FALSE;
	char cYear[5];
	int years = ((SW_Model.endyr + 1) - SW_Model.startyr), i = 0;

	PROTECT(WTH_DATA = allocVector(VECSXP,years));
	PROTECT(WTH_DATA_names = allocVector(STRSXP,years));
	weth_found = TRUE;
	for (year = SW_Model.startyr; year <= SW_Model.endyr; year++) {
		if (year < SW_Weather.yr.first) {
			weth_found = FALSE;
		} else {
			if(_read_hist(year)) {
				SET_VECTOR_ELT(WTH_DATA, i, onGet_WTH_DATA_YEAR(year));
				sprintf(cYear, "%4d", year);
				SET_STRING_ELT(WTH_DATA_names, i, mkChar(cYear));
				setNames=TRUE;
			}
		}
		if (!weth_found && !SW_Weather.use_markov) {
			LogError(logfp, LOGFATAL, "Markov Simulator turned off and weather file found not for year %d", year);
		}
		i++;
	}
	if(setNames) {
		setAttrib(WTH_DATA, R_NamesSymbol, WTH_DATA_names);
		UNPROTECT(2);
	} else {
		PROTECT(WTH_DATA = allocVector(VECSXP,0));
		UNPROTECT(3);
	}
	return WTH_DATA;
}
SEXP onGet_WTH_DATA_YEAR(TimeInt year) {
	int i,days;
	SEXP swWeatherData;
	SEXP WeatherData;
	SEXP Year, Year_names, Year_names_y;
	SEXP nYear;
	char *cYear[] = {"DOY", "Tmax_C", "Tmin_C", "PPT_cm"};
	RealD *p_Year;
	SW_WEATHER_HIST *wh = &SW_Weather.hist;

	days = Time_get_lastdoy_y(year);

	PROTECT(swWeatherData = MAKE_CLASS("swWeatherData"));
	PROTECT(WeatherData = NEW_OBJECT(swWeatherData));

	PROTECT(nYear = NEW_INTEGER(1));
	INTEGER(nYear)[0] = year;

	PROTECT(Year = allocMatrix(REALSXP,days,4));
	p_Year = REAL(Year);
	for (i = 0; i < days; i++) {
		p_Year[i + days * 0] = (i + 1);
		p_Year[i + days * 1] = wh->temp_max[i];
		p_Year[i + days * 2] = wh->temp_min[i];
		p_Year[i + days * 3] = wh->ppt[i];
	}
	PROTECT(Year_names = allocVector(VECSXP,2));
	PROTECT(Year_names_y = allocVector(STRSXP,4));
	for (i = 0; i < 4; i++)
	SET_STRING_ELT(Year_names_y, i, mkChar(cYear[i]));
	SET_VECTOR_ELT(Year_names, 1, Year_names_y);
	setAttrib(Year, R_DimNamesSymbol, Year_names);

	SET_SLOT(WeatherData,install("data"),Year);
	SET_SLOT(WeatherData,install("year"),nYear);

	UNPROTECT(6);
	return WeatherData;
}
Bool onSet_WTH_DATA(SEXP WTH_DATA_YEAR, TimeInt year) {
	SW_WEATHER_HIST *wh = &SW_Weather.hist;
	//SEXP dim;
	int x, lineno = 0, k = 0, i, j,days;
	RealD *p_WTH_DATA;
	RealF tmpmax, tmpmin, ppt, acc = 0.0;
	TimeInt doy;

	//dim = getAttrib(WTH_DATA_YEAR, R_DimSymbol);
	days = Time_get_lastdoy_y(year);

	if (nrows(WTH_DATA_YEAR) != days && ncols(WTH_DATA_YEAR) != 4)
	return FALSE;
	p_WTH_DATA = REAL(WTH_DATA_YEAR);
	_clear_hist_weather();

	for (i = 0; i < days; i++) {
		doy = p_WTH_DATA[i + days * 0];
		if (doy < 1 || doy > days) {
			LogError(logfp, LOGFATAL, "weath.%4d : Day of year out of range, line %d.", year, lineno);
		}
		//MAX_DAYS; //?
		/* --- Make the assignments ---- */
		doy--;
		wh->temp_max[doy] = p_WTH_DATA[i + days * 1];
		wh->temp_min[doy] = p_WTH_DATA[i + days * 2];
		wh->temp_avg[doy] = (p_WTH_DATA[i + days * 1] + p_WTH_DATA[i + days * 2]) / 2.0;
		wh->ppt[doy] = p_WTH_DATA[i + days * 3];

		/* Reassign if invalid values are found.  The values are
		 * either valid or WTH_MISSING.  If they were not
		 * present in the file, we wouldn't get this far because
		 * sscanf() would return too few items.
		 */
		if (missing(tmpmax)) {
			wh->temp_max[doy] = WTH_MISSING;
			LogError(logfp, LOGWARN, "weath.%4d : Missing max temp on doy=%d.", year, doy + 1);
		}
		if (missing(tmpmin)) {
			wh->temp_min[doy] = WTH_MISSING;
			LogError(logfp, LOGWARN, "weath.%4d : Missing min temp on doy=%d.", year, doy + 1);
		}
		if (missing(ppt)) {
			wh->ppt[doy] = 0.;
			LogError(logfp, LOGWARN, "weath.%4d : Missing PPT on doy=%d.", year, doy + 1);
		}

		if (!missing(tmpmax) && !missing(tmpmin)) {
			k++;
			acc += wh->temp_avg[doy];
		}
	} /* end of input lines */

	wh->temp_year_avg = acc / (k + 0.0);

	x = 0;
	for (i = 0; i < MAX_MONTHS; i++) {
		k = 31;
		if (i == 8 || i == 3 || i == 5 || i == 10)
		k = 30; // september, april, june, & november all have 30 days...
		else if (i == 1) {
			k = 28; // february has 28 days, except if it's a leap year, in which case it has 29 days...
			if (isleapyear(year))
			k = 29;
		}
		acc = 0.0;
		for (j = 0; j < k; j++)
		acc += wh->temp_avg[j + x];
		wh->temp_month_avg[i] = acc / (k + 0.0);
		x += k;
	}
	return TRUE;
}
#endif

static Bool _read_hist(TimeInt year) {
	/* =================================================== */
	/* Read the historical (measured) weather files.
	 * Format is
	 * day-of-month, month number, year, doy, mintemp, maxtemp, ppt
	 *
	 * I dislike the inclusion of the first three columns
	 * but this is the old format.  If a new format emerges
	 * these columns will likely be removed.
	 *
	 * temps are in degrees C, ppt is in cm,
	 *
	 * 26-Jan-02 - changed format of the input weather files.
	 *
	 */

	SW_WEATHER_HIST *wh = &SW_Weather.hist;
	FILE *f;
	int x, lineno = 0, k = 0, i, j;
	RealF tmpmax, tmpmin, ppt, acc = 0.0;
	TimeInt doy;

	char fname[MAX_FILENAMESIZE];

	sprintf(fname, "%s.%4d", SW_Weather.name_prefix, year);

	if (NULL == (f = fopen(fname, "r")))
		return FALSE;

	_clear_hist_weather();

	while (GetALine(f, inbuf)) {
		lineno++;
		x = sscanf(inbuf, "%d %f %f %f", &doy, &tmpmax, &tmpmin, &ppt);
		if (x < 4) {
			CloseFile(&f);
			LogError(logfp, LOGFATAL, "%s : Incomplete record %d (doy=%d).", fname, lineno, doy);
		}
		if (x > 4) {
			CloseFile(&f);
			LogError(logfp, LOGFATAL, "%s : Too many values in record %d (doy=%d).", fname, lineno, doy);
		}
		if (doy < 1 || doy > MAX_DAYS) {
			CloseFile(&f);
			LogError(logfp, LOGFATAL, "%s : Day of year out of range, line %d.", fname, lineno);
		}

		/* --- Make the assignments ---- */
		doy--;
		wh->temp_max[doy] = tmpmax;
		wh->temp_min[doy] = tmpmin;
		wh->temp_avg[doy] = (tmpmax + tmpmin) / 2.0;
		wh->ppt[doy] = ppt;

		/* Reassign if invalid values are found.  The values are
		 * either valid or WTH_MISSING.  If they were not
		 * present in the file, we wouldn't get this far because
		 * sscanf() would return too few items.
		 */
		if (missing(tmpmax)) {
			wh->temp_max[doy] = WTH_MISSING;
			LogError(logfp, LOGWARN, "%s : Missing max temp on doy=%d.", fname, doy + 1);
		}
		if (missing(tmpmin)) {
			wh->temp_min[doy] = WTH_MISSING;
			LogError(logfp, LOGWARN, "%s : Missing min temp on doy=%d.", fname, doy + 1);
		}
		if (missing(ppt)) {
			wh->ppt[doy] = 0.;
			LogError(logfp, LOGWARN, "%s : Missing PPT on doy=%d.", fname, doy + 1);
		}

		if (!missing(tmpmax) && !missing(tmpmin)) {
			k++;
			acc += wh->temp_avg[doy];
		}
	} /* end of input lines */

	wh->temp_year_avg = acc / (k + 0.0);

	x = 0;
	for (i = 0; i < MAX_MONTHS; i++) {
		k = 31;
		if (i == 8 || i == 3 || i == 5 || i == 10)
			k = 30; // september, april, june, & november all have 30 days...
		else if (i == 1) {
			k = 28; // february has 28 days, except if it's a leap year, in which case it has 29 days...
			if (isleapyear(year))
				k = 29;
		}

		acc = 0.0;
		for (j = 0; j < k; j++)
			acc += wh->temp_avg[j + x];
		wh->temp_month_avg[i] = acc / (k + 0.0);
		x += k;
	}

	fclose(f);
	return TRUE;
}

static void _update_yesterday(void) {
	/* --------------------------------------------------- */
	/* save today's temp values as yesterday */
	/* this must be done after all calculations are
	 * finished for the day and before today's weather
	 * is read from the file.  Assumes Today's weather
	 * is always validated (non-missing).
	 */
	SW_WEATHER_2DAYS *wn = &SW_Weather.now;

	wn->temp_max[Yesterday] = wn->temp_max[Today];
	wn->temp_min[Yesterday] = wn->temp_min[Today];
	wn->temp_avg[Yesterday] = wn->temp_avg[Today];
	wn->temp_run_avg[Yesterday] = wn->temp_run_avg[Today];

	wn->ppt_actual[Yesterday] = wn->ppt_actual[Today];
	wn->ppt[Yesterday] = wn->ppt[Today];
	wn->snow[Yesterday] = wn->snow[Today];
	wn->rain[Yesterday] = wn->rain[Today];
	wn->snowmelt[Yesterday] = wn->snowmelt[Today];
	wn->snowloss[Yesterday] = wn->snowloss[Today];

}

#ifdef DEBUG_MEM
#include "myMemory.h"
/*======================================================*/
void SW_WTH_SetMemoryRefs( void) {
	/* when debugging memory problems, use the bookkeeping
	 code in myMemory.c
	 This routine sets the known memory refs in this module
	 so they can be  checked for leaks, etc.  All refs will
	 have been cleared by a call to ClearMemoryRefs() before
	 this, and will be checked via CheckMemoryRefs() after
	 this, most likely in the main() function.
	 */

	NoteMemoryRef(runavg_list);

}

#endif
