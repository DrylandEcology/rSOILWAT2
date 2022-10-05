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
  "UseSnow",
  "pct_SnowDrift",
  "pct_SnowRunoff",
  "use_weathergenerator",
  "use_weathergenerator_only",
  "FirstYear_Historical", // removed from SOILWAT2; kept here for backwards compatibility
  "MonthlyScalingParams"
};



/* =================================================== */
/*             Local Function Definitions              */
/* --------------------------------------------------- */

static SEXP onGet_WTH_DATA_YEAR(TimeInt year);
static void rSW2_setAllWeather(
  SEXP listAllW,
  SW_WEATHER_HIST **allHist,
  int startYear,
  unsigned int n_years,
  Bool use_weathergenerator_only
);


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
	LOGICAL_POINTER(use_weathergenerator)[0] = w->generateWeatherMethod == 2;
	PROTECT(use_weathergenerator_only = NEW_LOGICAL(1));
	LOGICAL_POINTER(use_weathergenerator_only)[0] = w->use_weathergenerator_only;
	PROTECT(yr_first = NEW_INTEGER(1));
	/* `SW_weather.yr` was removed from SOILWAT2:
	INTEGER_POINTER(yr_first)[0] = w->yr.first;
	*/
	INTEGER_POINTER(yr_first)[0] = SW_Weather.startYear;

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
	int i;
	SW_WEATHER *w = &SW_Weather;
	SEXP
		use_snow, pct_snowdrift, pct_snowRunoff,
		use_weathergenerator, use_weathergenerator_only;
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
	w->generateWeatherMethod = *INTEGER(use_weathergenerator) ? 2 : 0;
	PROTECT(use_weathergenerator_only = GET_SLOT(SW_WTH, install(cSW_WTH_names[4])));
	w->use_weathergenerator_only = (Bool) *INTEGER(use_weathergenerator_only);
	if (w->use_weathergenerator_only) {
		w->generateWeatherMethod = 2;
	}

	/* `SW_weather.yr` was removed from SOILWAT2:
	SEXP yr_first;
	int tmp;

	PROTECT(yr_first = GET_SLOT(SW_WTH, install(cSW_WTH_names[5])));
	tmp = *INTEGER(yr_first);
	w->yr.first = (tmp < 0) ? SW_Model.startyr : yearto4digit(tmp);
	*/

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

  /* `SW_weather.yr` was removed from SOILWAT2:
	w->yr.last = SW_Model.endyr;
	w->yr.total = w->yr.last - w->yr.first + 1;

	if (SW_Weather.generateWeatherMethod != 2 && SW_Model.startyr < w->yr.first) {
		LogError(
			logfp,
			LOGFATAL,
			"%s : Model year (%d) starts before weather files (%d)"
				" and weather generator turned off.\n"
				" Please synchronize the years or set up the weather generator files",
			MyFileName, SW_Model.startyr, w->yr.first
		);
	}
	*/

	UNPROTECT(6);
}


/**
  @brief Copy all weather data from `SOILWAT2` data structure
    to `rSOILWAT2` list of `swWeatherData`

  Called by `onGetInputDataFromFiles()`
*/
SEXP onGet_WTH_DATA(void) {
	TimeInt year, yearIndex;
	SEXP WTH_DATA, WTH_DATA_names;
	char cYear[5];

	PROTECT(WTH_DATA = allocVector(VECSXP, SW_Weather.n_years));
	PROTECT(WTH_DATA_names = allocVector(STRSXP, SW_Weather.n_years));

	for (yearIndex = 0; yearIndex < SW_Weather.n_years; yearIndex++) {
		year = SW_Weather.startYear + yearIndex;
		sprintf(cYear, "%4d", year);
		SET_STRING_ELT(WTH_DATA_names, yearIndex, mkChar(cYear));

		// copy values from SOILWAT2 variables to rSOILWAT2 S4 class object
		SET_VECTOR_ELT(WTH_DATA, yearIndex, onGet_WTH_DATA_YEAR(year));
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
	int i, days, yearIndex;
	const int nitems = 4;
	SEXP swWeatherData;
	SEXP WeatherData;
	SEXP Year, Year_names, Year_names_y;
	SEXP nYear;
	char *cYear[] = {"DOY", "Tmax_C", "Tmin_C", "PPT_cm"};
	RealD *p_Year;
	SW_WEATHER *w = &SW_Weather;

	days = Time_get_lastdoy_y(year);
	yearIndex = year - SW_Weather.startYear;

	PROTECT(swWeatherData = MAKE_CLASS("swWeatherData"));
	PROTECT(WeatherData = NEW_OBJECT(swWeatherData));

	PROTECT(nYear = NEW_INTEGER(1));
	INTEGER(nYear)[0] = year;

	PROTECT(Year = allocMatrix(REALSXP, days, nitems));
	p_Year = REAL(Year);
	for (i = 0; i < days; i++) {
		p_Year[i + days * 0] = (i + 1);
		p_Year[i + days * 1] = w->allHist[yearIndex]->temp_max[i];
		p_Year[i + days * 2] = w->allHist[yearIndex]->temp_min[i];
		p_Year[i + days * 3] = w->allHist[yearIndex]->ppt[i];
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
  @brief Move all weather data from `rSOILWAT2` to `SOILWAT2`

  Equivalent functionality to `SW_WTH_read()`with the difference that
  `SOILWAT2` `allHist` is filled with values that are
  copied from `rSOILWAT2` list of `swWeatherData`
  instead of being read from files on disk.


  Called by `rSW_CTL_obtain_inputs()` if `from_files` is `FALSE`.

  @note Elements `endyr` and `startyr` of `SW_Model` must be set/updated
    via `onSet_SW_MDL()` before this function is called.
*/
void onSet_WTH_DATA(void) {

  SEXP listAllWeather;

  // Determine which `rSOILWAT2` list of `swWeatherData` we are using
  listAllWeather = bWeatherList ?
    WeatherList :
    GET_SLOT(InputData, install("weatherHistory"));


  // Deallocate (previous, if any) `allHist`
  // (using value of `SW_Weather.n_years` previously used to allocate)
  // `SW_WTH_construct()` sets `n_years` to zero
  deallocateAllWeather(&SW_Weather);

  // Update number of years and first calendar year represented
  SW_Weather.n_years = SW_Model.endyr - SW_Model.startyr + 1;
  SW_Weather.startYear = SW_Model.startyr;

  // Allocate new `allHist` (based on current `SW_Weather.n_years`)
  allocateAllWeather(&SW_Weather);


  // Equivalent to `readAllWeather()`:
  // fill `SOILWAT2` `allHist` with values from `rSOILWAT2`
  rSW2_setAllWeather(
    listAllWeather,
    SW_Weather.allHist,
    SW_Weather.startYear,
    SW_Weather.n_years,
    SW_Weather.use_weathergenerator_only
  );
}


// Equivalent to `readAllWeather()`:
// fill `SOILWAT2` `allHist` with values from `rSOILWAT2`
static void rSW2_setAllWeather(
  SEXP listAllW,
  SW_WEATHER_HIST **allHist,
  int startYear,
  unsigned int n_years,
  Bool use_weathergenerator_only
) {
  int nList, yearIndex, year, numDaysYear, day, doy, i, j;
  Bool weth_found;
  SEXP tmpW, yrWData;
  double *p_yrWData;

  nList = LENGTH(listAllW);

  // Loop over years and move weather data to `SOILWAT2` `allHist`
  for (yearIndex = 0; yearIndex < n_years; yearIndex++) {

    if (use_weathergenerator_only) {
      // Set values to missing for call to `generateMissingWeather()`
      _clear_hist_weather(allHist[yearIndex]);

    } else {
      year = yearIndex + startYear;

      // Locate suitable year among rSOILWAT2 list of `swWeatherData`
      weth_found = swFALSE;

      for (i = 0; !weth_found && i < nList; i++) {
        tmpW = VECTOR_ELT(listAllW, i);
        weth_found = (Bool) year == *INTEGER(GET_SLOT(tmpW, install("year")));
      }

      if (weth_found) {
        yrWData = GET_SLOT(tmpW, install("data"));
        weth_found = !isnull(yrWData);
      }
      if (weth_found) {
        numDaysYear = Time_get_lastdoy_y(year);

        if (nrows(yrWData) != numDaysYear || ncols(yrWData) != 4) {
          LogError(
            logfp,
            LOGFATAL,
            "Weather data (year %d): wrong dimensions: "
            "expected %d rows (had %d) and 4 columns (had %d).\n",
            year,
            numDaysYear,
            nrows(yrWData),
            ncols(yrWData)
          );
          return;
        }

        p_yrWData = REAL(yrWData);

        // Loop over days of current year
        for (day = 0; day < numDaysYear; day++) {

            doy = p_yrWData[day + numDaysYear * 0];
            if (doy < 1 || doy > numDaysYear) {
              LogError(
                logfp,
                LOGFATAL,
                "Weather data (year %d): "
                "day of year out of range (%d), expected: %d.\n",
                year,
                doy,
                day + 1
              );
            }

            // Copy weather data to `SOILWAT2` `allHist`

            // Translate `NA` to `SW_MISSING`
            j = day + numDaysYear * 1;
            allHist[yearIndex]->temp_max[day] =
              (R_FINITE(p_yrWData[j]) && !missing(p_yrWData[j])) ?
              p_yrWData[j] :
              SW_MISSING;

            j = day + numDaysYear * 2;
            allHist[yearIndex]->temp_min[day] =
              (R_FINITE(p_yrWData[j]) && !missing(p_yrWData[j])) ?
              p_yrWData[j] :
              SW_MISSING;

            j = day + numDaysYear * 3;
            allHist[yearIndex]->ppt[day] =
              (R_FINITE(p_yrWData[j]) && !missing(p_yrWData[j])) ?
              p_yrWData[j] :
              SW_MISSING;


            // Calculate average air temperature
            if (
              !missing(allHist[yearIndex]->temp_max[day]) &&
              !missing(allHist[yearIndex]->temp_min[day])
            ) {
              allHist[yearIndex]->temp_avg[day] = (
                allHist[yearIndex]->temp_max[day] +
                allHist[yearIndex]->temp_min[day]
              ) / 2.;
            }
        }

      } else {
        // Set values to missing for call to `generateMissingWeather()`
        _clear_hist_weather(allHist[yearIndex]);
      }
    }
  }
}
