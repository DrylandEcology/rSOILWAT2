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
#include "SOILWAT2/SW_Model.h" // externs `SW_Model`
#include "SOILWAT2/SW_Markov.h"
#include "SOILWAT2/SW_Sky.h"

#include "SOILWAT2/SW_Weather.h" // externs `SW_Weather`
#include "rSW_Weather.h"
#include "SW_R_lib.h" // externs `InputData`, `WeatherList`, `bWeatherList`

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>


/* =================================================== */
/*                  Local Variables                    */
/* --------------------------------------------------- */
static char *MyFileName;

static char *cSW_WTH_names[] = {
  "UseSnow", "pct_SnowDrift", "pct_SnowRunoff",
  "use_weathergenerator", "use_weathergenerator_only",
  "FirstYear_Historical",
  "MonthlyScalingParams"
};



/* =================================================== */
/*             Local Function Definitions              */
/* --------------------------------------------------- */

static SEXP onGet_WTH_DATA_YEAR(TimeInt year);



/* =================================================== */
/*             Global Function Definitions             */
/* --------------------------------------------------- */


/**
  @brief Copy weather setup from `SOILWAT2` `SW_WEATHER`
    to `rSOILWAT2` S4 `swWeather`

  Called by `onGetInputDataFromFiles()`.
*/
SEXP onGet_SW_WTH_setup() {
	int i;
	const int nitems = 6;
	RealD *p_MonthlyValues;
	SW_WEATHER *w = &SW_Weather;

	SEXP swWeather;
	SEXP SW_WTH;

	SEXP
		use_snow, pct_snowdrift, pct_snowRunoff,
		use_weathergenerator, use_weathergenerator_only, yr_first;
	SEXP MonthlyScalingParams, MonthlyScalingParams_names, MonthlyScalingParams_names_x, MonthlyScalingParams_names_y;

	char *cMonthlyScalingParams_names[] = {"PPT", "MaxT", "MinT", "SkyCover", "Wind", "rH"};
	char *cMonths[] = {"January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"};

	PROTECT(swWeather = MAKE_CLASS("swWeather"));
	PROTECT(SW_WTH = NEW_OBJECT(swWeather));

	PROTECT(use_snow = NEW_LOGICAL(1));
	LOGICAL_POINTER(use_snow)[0] = w->use_snow;
	PROTECT(pct_snowdrift = NEW_NUMERIC(1));
	REAL(pct_snowdrift)[0] = w->pct_snowdrift;
	PROTECT(pct_snowRunoff = NEW_NUMERIC(1));
	REAL(pct_snowRunoff)[0] = w->pct_snowRunoff;
	PROTECT(use_weathergenerator = NEW_LOGICAL(1));
	LOGICAL_POINTER(use_weathergenerator)[0] = w->use_weathergenerator;
	PROTECT(use_weathergenerator_only = NEW_LOGICAL(1));
	LOGICAL_POINTER(use_weathergenerator_only)[0] = w->use_weathergenerator_only;
	PROTECT(yr_first = NEW_INTEGER(1));
	INTEGER_POINTER(yr_first)[0] = w->yr.first;

	PROTECT(MonthlyScalingParams = allocMatrix(REALSXP, 12, nitems));
	p_MonthlyValues = REAL(MonthlyScalingParams);
	for (i = 0; i < 12; i++) {
		p_MonthlyValues[i + 12 * 0] = w->scale_precip[i];
		p_MonthlyValues[i + 12 * 1] = w->scale_temp_max[i];
		p_MonthlyValues[i + 12 * 2] = w->scale_temp_min[i];
		p_MonthlyValues[i + 12 * 3] = w->scale_skyCover[i];
		p_MonthlyValues[i + 12 * 4] = w->scale_wind[i];
		p_MonthlyValues[i + 12 * 5] = w->scale_rH[i];
	}
	PROTECT(MonthlyScalingParams_names = allocVector(VECSXP, 2));
	PROTECT(MonthlyScalingParams_names_x = allocVector(STRSXP, 12));
	for (i = 0; i < 12; i++)
		SET_STRING_ELT(MonthlyScalingParams_names_x, i, mkChar(cMonths[i]));
	PROTECT(MonthlyScalingParams_names_y = allocVector(STRSXP, nitems));
	for (i = 0; i < nitems; i++)
		SET_STRING_ELT(MonthlyScalingParams_names_y, i, mkChar(cMonthlyScalingParams_names[i]));
	SET_VECTOR_ELT(MonthlyScalingParams_names, 0, MonthlyScalingParams_names_x);
	SET_VECTOR_ELT(MonthlyScalingParams_names, 1, MonthlyScalingParams_names_y);
	setAttrib(MonthlyScalingParams, R_DimNamesSymbol, MonthlyScalingParams_names);

	SET_SLOT(SW_WTH, install(cSW_WTH_names[0]), use_snow);
	SET_SLOT(SW_WTH, install(cSW_WTH_names[1]), pct_snowdrift);
	SET_SLOT(SW_WTH, install(cSW_WTH_names[2]), pct_snowRunoff);
	SET_SLOT(SW_WTH, install(cSW_WTH_names[3]), use_weathergenerator);
	SET_SLOT(SW_WTH, install(cSW_WTH_names[4]), use_weathergenerator_only);
	SET_SLOT(SW_WTH, install(cSW_WTH_names[5]), yr_first);
	SET_SLOT(SW_WTH, install(cSW_WTH_names[6]), MonthlyScalingParams);

	UNPROTECT(12);
	return SW_WTH;
}


/**
  @brief Copy weather setup from `rSOILWAT2` S4 `swWeather`
    to `SOILWAT2` `SW_WEATHER`

  Called by `rSW_CTL_obtain_inputs()` if `from_files` is `FALSE`.
*/
void onSet_SW_WTH_setup(SEXP SW_WTH) {
	int i, tmp;
	SW_WEATHER *w = &SW_Weather;
	SEXP
		use_snow, pct_snowdrift, pct_snowRunoff,
		use_weathergenerator, use_weathergenerator_only, yr_first;
	SEXP MonthlyScalingParams;
	RealD *p_MonthlyValues;

	MyFileName = SW_F_name(eWeather);

	PROTECT(use_snow = GET_SLOT(SW_WTH, install(cSW_WTH_names[0])));
	w->use_snow = (Bool) *INTEGER(use_snow);
	PROTECT(pct_snowdrift = GET_SLOT(SW_WTH, install(cSW_WTH_names[1])));
	w->pct_snowdrift = *REAL(pct_snowdrift);
	PROTECT(pct_snowRunoff = GET_SLOT(SW_WTH, install(cSW_WTH_names[2])));
	w->pct_snowRunoff = *REAL(pct_snowRunoff);

	PROTECT(use_weathergenerator = GET_SLOT(SW_WTH, install(cSW_WTH_names[3])));
	w->use_weathergenerator = (Bool) *INTEGER(use_weathergenerator);
	PROTECT(use_weathergenerator_only = GET_SLOT(SW_WTH, install(cSW_WTH_names[4])));
	w->use_weathergenerator_only = (Bool) *INTEGER(use_weathergenerator_only);
	if (w->use_weathergenerator_only) {
		w->use_weathergenerator = TRUE;
	}

	PROTECT(yr_first = GET_SLOT(SW_WTH, install(cSW_WTH_names[5])));
	tmp = *INTEGER(yr_first);
	w->yr.first = (tmp < 0) ? SW_Model.startyr : yearto4digit(tmp);

	PROTECT(MonthlyScalingParams = GET_SLOT(SW_WTH, install(cSW_WTH_names[6])));
	p_MonthlyValues = REAL(MonthlyScalingParams);
	for (i = 0; i < 12; i++) {
		w->scale_precip[i] = p_MonthlyValues[i + 12 * 0];
		w->scale_temp_max[i] = p_MonthlyValues[i + 12 * 1];
		w->scale_temp_min[i] = p_MonthlyValues[i + 12 * 2];
		w->scale_skyCover[i] = p_MonthlyValues[i + 12 * 3];
		w->scale_wind[i] = p_MonthlyValues[i + 12 * 4];
		w->scale_rH[i] = p_MonthlyValues[i + 12 * 5];
	}

	SW_WeatherPrefix(w->name_prefix);

	w->yr.last = SW_Model.endyr;
	w->yr.total = w->yr.last - w->yr.first + 1;

	if (!w->use_weathergenerator && SW_Model.startyr < w->yr.first) {
		LogError(
			logfp,
			LOGFATAL,
			"%s : Model year (%d) starts before weather files (%d)"
				" and use_weathergenerator=swFALSE.\nPlease synchronize the years"
				" or set up the Markov weather files",
			MyFileName, SW_Model.startyr, w->yr.first
		);
	}

	UNPROTECT(7);
}


/**
  @brief Copy all weather data from `SOILWAT2` data structure
    to `rSOILWAT2` list of `swWeatherData`

  Called by `onGetInputDataFromFiles()`
*/
SEXP onGet_WTH_DATA(void) {
	TimeInt year;
	SEXP WTH_DATA, WTH_DATA_names;
	char cYear[5];
	int n_yrs, i;

	// number of years
	n_yrs = SW_Model.endyr - SW_Model.startyr + 1;
	PROTECT(WTH_DATA = allocVector(VECSXP, n_yrs));
	PROTECT(WTH_DATA_names = allocVector(STRSXP, n_yrs));

	for (year = SW_Model.startyr, i = 0; year <= SW_Model.endyr; year++, i++) {
		sprintf(cYear, "%4d", year);
		SET_STRING_ELT(WTH_DATA_names, i, mkChar(cYear));

        // copy values from SOILWAT2 variables to rSOILWAT2 S4 class object
        SET_VECTOR_ELT(WTH_DATA, i, onGet_WTH_DATA_YEAR(year));
	}

	setAttrib(WTH_DATA, R_NamesSymbol, WTH_DATA_names);

	UNPROTECT(2);
	return WTH_DATA;
}


/**
  @brief Copy weather data for `year` from `SOILWAT2` data structure
    to `rSOILWAT2` `swWeatherData`

  Called by `onGet_WTH_DATA()`
*/
SEXP onGet_WTH_DATA_YEAR(TimeInt year) {
	int i,days;
	const int nitems = 4;
	SEXP swWeatherData;
	SEXP WeatherData;
	SEXP Year, Year_names, Year_names_y;
	SEXP nYear;
	char *cYear[] = {"DOY", "Tmax_C", "Tmin_C", "PPT_cm"};
	RealD *p_Year;
	SW_WEATHER *w = &SW_Weather;

	days = Time_get_lastdoy_y(year);

	PROTECT(swWeatherData = MAKE_CLASS("swWeatherData"));
	PROTECT(WeatherData = NEW_OBJECT(swWeatherData));

	PROTECT(nYear = NEW_INTEGER(1));
	INTEGER(nYear)[0] = year;

	PROTECT(Year = allocMatrix(REALSXP, days, nitems));
	p_Year = REAL(Year);
	for (i = 0; i < days; i++) {
		p_Year[i + days * 0] = (i + 1);
		p_Year[i + days * 1] = w->allHist[year - SW_Model.startyr]->temp_max[i];
		p_Year[i + days * 2] = w->allHist[year - SW_Model.startyr]->temp_min[i];
		p_Year[i + days * 3] = w->allHist[year - SW_Model.startyr]->ppt[i];
	}

	PROTECT(Year_names = allocVector(VECSXP, 2));
	PROTECT(Year_names_y = allocVector(STRSXP, nitems));
	for (i = 0; i < nitems; i++) {
		SET_STRING_ELT(Year_names_y, i, mkChar(cYear[i]));
	}
	SET_VECTOR_ELT(Year_names, 1, Year_names_y);
	setAttrib(Year, R_DimNamesSymbol, Year_names);

	SET_SLOT(WeatherData, install("data"), Year);
	SET_SLOT(WeatherData, install("year"), nYear);

	UNPROTECT(6);
	return WeatherData;
}



/**
  @brief Copy all weather data from `rSOILWAT2` list of `swWeatherData` to
    `SOILWAT2` `allHist`

  @note However, currently, it creates `allHist` and
    reads all data from files instead.

  Called by `rSW_CTL_obtain_inputs()` if `from_files` is `FALSE`.
*/
void onSet_SW_WTH_read() {

    int year;

    SW_WEATHER *w = &SW_Weather;
    SW_MODEL *m = &SW_Model;

    w->n_years = m->endyr - m->startyr + 1;

    w->allHist = (SW_WEATHER_HIST **)malloc(sizeof(SW_WEATHER_HIST *) * w->n_years);

    for(year = 0; year < w->n_years; year++) {
        w->allHist[year] = (SW_WEATHER_HIST *)malloc(sizeof(SW_WEATHER_HIST));
    }

    readAllWeather(w->allHist, w->yr.first, w->n_years);

}
