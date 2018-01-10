/********************************************************/
/********************************************************/
/*	Source file: Weather.c
 Type: class
 Application: SOILWAT - soilwater dynamics simulator
 Purpose: Read / write and otherwise manage the model's
 weather-related information.
 */
/********************************************************/
/********************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "SOILWAT2/generic.h"
#include "SOILWAT2/filefuncs.h"
#include "SOILWAT2/Times.h"
#include "SOILWAT2/myMemory.h"

#include "SOILWAT2/SW_Defines.h"
#include "SOILWAT2/SW_Files.h"
#include "SOILWAT2/SW_Model.h"
#include "SOILWAT2/SW_Markov.h"
#include "SOILWAT2/SW_Sky.h"

#include "SOILWAT2/SW_Weather.h"
#include "rSW_Weather.h"
#include "rSW_Markov.h"
#include "rSW_Sky.h"

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>


/* =================================================== */
/*                  Global Variables                   */
/* --------------------------------------------------- */
extern SW_MODEL SW_Model;
extern SW_WEATHER SW_Weather;

extern SEXP InputData;
extern SEXP WeatherList;
extern Bool bWeatherList;

extern Bool weth_found; /* TRUE=success reading this years weather file */
extern RealD *runavg_list; /* used in run_tmp_avg() */

/* =================================================== */
/*                Module-Level Variables               */
/* --------------------------------------------------- */
static char *MyFileName;


/* =================================================== */
/* =================================================== */
/*             Private Function Definitions            */
/* --------------------------------------------------- */


/* =================================================== */
/* =================================================== */
/*             Public Function Definitions             */
/* --------------------------------------------------- */

void rSW_WTH_new_year2(TimeInt year) {
  int i = 0;

  if (bWeatherList) {
    for (i = 0; i < LENGTH(WeatherList); i++) {
      if (year == *INTEGER(GET_SLOT(VECTOR_ELT(WeatherList, i), install("year")))) {
        weth_found = onSet_WTH_DATA(GET_SLOT(VECTOR_ELT(WeatherList, i), install("data")), year);
      }
    }

  } else {
    for (i = 0; i < LENGTH(GET_SLOT(InputData, install("weatherHistory"))); i++) {
      if (year == *INTEGER(GET_SLOT(VECTOR_ELT(GET_SLOT(InputData, install("weatherHistory")), i), install("year")))) {
        weth_found = onSet_WTH_DATA(GET_SLOT(VECTOR_ELT(GET_SLOT(InputData, install("weatherHistory")), i), install("data")), year);
      }
    }
  }

}

SEXP onGet_SW_WTH() {
	int i;
	RealD *p_MonthlyValues;
	SW_WEATHER *w = &SW_Weather;

	SEXP swWeather;
	SEXP SW_WTH;

	SEXP use_snow, pct_snowdrift, pct_snowRunoff, use_markov, yr_first, days_in_runavg;
	SEXP MonthlyScalingParams, MonthlyScalingParams_names, MonthlyScalingParams_names_x, MonthlyScalingParams_names_y;

	char *cSW_WTH_names[] = {"UseSnow", "pct_SnowDrift", "pct_SnowRunoff", "use_Markov", "FirstYear_Historical", "DaysRunningAverage", "MonthlyScalingParams"};
	char *cMonthlyScalingParams_names[] = {"PPT", "MaxT", "MinT", "SkyCover", "Wind", "rH", "Transmissivity"};
	char *cMonths[] = {"January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"};

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
				" and use_Markov=FALSE.\nPlease synchronize the years"
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
	char fname[MAX_FILENAMESIZE];
	FILE *f;
	int nWeathData = 0, i;

	for (year = SW_Model.startyr; year <= SW_Model.endyr; year++) {
		sprintf(fname, "%s.%4d", SW_Weather.name_prefix, year);
		if (NULL == (f = fopen(fname, "r"))) {
			break;
		}
		fclose(f);
		nWeathData++;
	}

	PROTECT(WTH_DATA = allocVector(VECSXP,nWeathData));
	PROTECT(WTH_DATA_names = allocVector(STRSXP,nWeathData));

	if (nWeathData > 0){
		weth_found = TRUE;
		for (year = SW_Model.startyr, i = 0; year <= SW_Model.endyr; year++, i++) {
			if (year < SW_Weather.yr.first) {
				weth_found = FALSE;
			} else {
				if(_read_weather_hist(year)) {
					SET_VECTOR_ELT(WTH_DATA, i, onGet_WTH_DATA_YEAR(year));
					sprintf(cYear, "%4d", year);
					SET_STRING_ELT(WTH_DATA_names, i, mkChar(cYear));
					setNames=TRUE;
				}
			}
			if (!weth_found && !SW_Weather.use_markov) {
				LogError(logfp, LOGFATAL, "Markov Simulator turned off and weather file found not for year %d", year);
			}
		}
	} else {
		weth_found = FALSE;
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
	int x, lineno = 0, k = 0, i, j, days;
	RealD *p_WTH_DATA;
	RealF acc = 0.0;
	TimeInt doy;

	days = Time_get_lastdoy_y(year);

	if (nrows(WTH_DATA_YEAR) != days || ncols(WTH_DATA_YEAR) != 4) {
		LogError(logfp, LOGFATAL, "weath.%4d : Wrong number of days or columns in data. Expected rows %d had %d. Expected columns 4 had %d.", year, days,nrows(WTH_DATA_YEAR),ncols(WTH_DATA_YEAR));
		return FALSE;
	}

	p_WTH_DATA = REAL(WTH_DATA_YEAR);
	_clear_hist_weather();

	for (i = 0; i < days; i++) {
		doy = p_WTH_DATA[i + days * 0];
		if (doy < 1 || doy > days) {
			LogError(logfp, LOGFATAL, "weath.%4d : Day of year out of range, line %d.", year, lineno);
		}

		/* --- Make the assignments ---- */
		doy--;

		/* Reassign if invalid values are found.  The values are
		 * either valid or WTH_MISSING.  If they were not
		 * present in the file, we wouldn't get this far because
		 * sscanf() would return too few items.
		 */
		j = i + days * 1;
		if (p_WTH_DATA[j] == SW_MISSING || p_WTH_DATA[j] == WTH_MISSING || ISNA(p_WTH_DATA[j])) {
			wh->temp_max[doy] = WTH_MISSING;
			LogError(logfp, LOGWARN, "weath.%4d : Missing max temp on doy=%d.", year, doy + 1);
		} else {
			wh->temp_max[doy] = p_WTH_DATA[j];
		}

		j = i + days * 2;
		if (p_WTH_DATA[j] == SW_MISSING || p_WTH_DATA[j] == WTH_MISSING || ISNA(p_WTH_DATA[j])) {
			wh->temp_min[doy] = WTH_MISSING;
			LogError(logfp, LOGWARN, "weath.%4d : Missing min temp on doy=%d.", year, doy + 1);
		} else {
			wh->temp_min[doy] = p_WTH_DATA[j];
		}

		j = i + days * 3;
		if (p_WTH_DATA[j] == SW_MISSING || p_WTH_DATA[j] == WTH_MISSING || ISNA(p_WTH_DATA[j])) {
			wh->ppt[doy] = 0.;
			LogError(logfp, LOGWARN, "weath.%4d : Missing PPT on doy=%d.", year, doy + 1);
		} else {
			wh->ppt[doy] = p_WTH_DATA[j];
		}

		if (wh->temp_max[doy] != WTH_MISSING && wh->temp_min[doy] != WTH_MISSING) {
			k++;
			wh->temp_avg[doy] = (wh->temp_max[doy] + wh->temp_min[doy]) / 2.0;
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
		for (j = 0; j < k; j++) {
		  acc += wh->temp_avg[j + x];
		}
		wh->temp_month_avg[i] = acc / (k + 0.0);
		x += k;
	}
	return TRUE;
}
