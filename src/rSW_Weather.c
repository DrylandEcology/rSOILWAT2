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

#include "SOILWAT2/include/generic.h"
#include "SOILWAT2/include/filefuncs.h"
#include "SOILWAT2/include/Times.h"
#include "SOILWAT2/include/myMemory.h"
#include "SOILWAT2/include/SW_Flow_lib_PET.h"

#include "SOILWAT2/include/SW_Defines.h"
#include "SOILWAT2/include/SW_Files.h"
#include "SOILWAT2/include/SW_Model.h"
#include "SOILWAT2/include/SW_Markov.h"
#include "SOILWAT2/include/SW_Sky.h"
#include "SOILWAT2/include/SW_Main_lib.h"

#include "SOILWAT2/include/SW_Weather.h"
#include "rSW_Weather.h"
#include "SW_R_lib.h" // externs `SoilWatAll`

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>


/* =================================================== */
/*                  Local Variables                    */
/* --------------------------------------------------- */
static char *MyFileName;

static char *cSW_WTH_names[] = {
  "MonthlyScalingParams",
  "UseSnow",
  "pct_SnowDrift",
  "pct_SnowRunoff",
  "use_weathergenerator",
  "use_weathergenerator_only",
  "FirstYear_Historical", // removed from SOILWAT2; kept here for backwards compatibility
  "use_cloudCoverMonthly",
  "use_windSpeedMonthly",
  "use_humidityMonthly",
  "desc_rsds",
  "dailyInputFlags"
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
  Bool use_weathergenerator_only,
  Bool use_cloudCoverMonthly,
  Bool use_humidityMonthly,
  Bool use_windSpeedMonthly,
  Bool *dailyInputFlags,
  RealD *cloudcov,
  RealD *windspeed,
  RealD *r_humidity,
  LOG_INFO* LogInfo
);

static void rSW2_set_weather_hist(
  SEXP listAllW,
  TimeInt year,
  SW_WEATHER_HIST *yearWeather,
  Bool *dailyInputFlags,
  LOG_INFO* LogInfo
);

static double value_or_missing(double x) {
  return (R_FINITE(x) && !missing(x)) ? x : SW_MISSING;
}

/* =================================================== */
/*             Global Function Definitions             */
/* --------------------------------------------------- */


/**
  @brief Copy weather setup from `SOILWAT2` `SW_WEATHER`
    to `rSOILWAT2` S4 `swWeather`

  Called by `onGetInputDataFromFiles()`.
*/
SEXP onGet_SW_WTH_setup(void) {
	int i;
	const int nitems = 8;
	RealD *p_MonthlyValues;
	SW_WEATHER *w = &SoilWatAll.Weather;

	SEXP swWeather;
	SEXP SW_WTH;

    SEXP
        use_snow, pct_snowdrift, pct_snowRunoff,
        use_weathergenerator, use_weathergenerator_only,
        yr_first,
        use_cloudCoverMonthly, use_windSpeedMonthly, use_humidityMonthly,
        desc_rsds,
        dailyInputFlags;
	SEXP MonthlyScalingParams, MonthlyScalingParams_names, MonthlyScalingParams_names_x, MonthlyScalingParams_names_y;

	char *cMonthlyScalingParams_names[] = {"PPT", "MaxT", "MinT", "SkyCover", "Wind", "rH", "ActVP", "ShortWR"};
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
	INTEGER_POINTER(yr_first)[0] = SoilWatAll.Weather.startYear;

	PROTECT(use_cloudCoverMonthly = NEW_LOGICAL(1));
	LOGICAL_POINTER(use_cloudCoverMonthly)[0] = w->use_cloudCoverMonthly;
	PROTECT(use_windSpeedMonthly = NEW_LOGICAL(1));
	LOGICAL_POINTER(use_windSpeedMonthly)[0] = w->use_windSpeedMonthly;
	PROTECT(use_humidityMonthly = NEW_LOGICAL(1));
	LOGICAL_POINTER(use_humidityMonthly)[0] = w->use_humidityMonthly;

	PROTECT(desc_rsds = NEW_INTEGER(1));
	INTEGER_POINTER(desc_rsds)[0] = w->desc_rsds;

	PROTECT(dailyInputFlags = allocVector(LGLSXP, MAX_INPUT_COLUMNS));
	for (i = 0; i < MAX_INPUT_COLUMNS; i++) {
		LOGICAL_POINTER(dailyInputFlags)[i] = w->dailyInputFlags[i];
	}

	PROTECT(MonthlyScalingParams = allocMatrix(REALSXP, 12, nitems));
	p_MonthlyValues = REAL(MonthlyScalingParams);
	for (i = 0; i < 12; i++) {
		p_MonthlyValues[i + 12 * 0] = w->scale_precip[i];
		p_MonthlyValues[i + 12 * 1] = w->scale_temp_max[i];
		p_MonthlyValues[i + 12 * 2] = w->scale_temp_min[i];
		p_MonthlyValues[i + 12 * 3] = w->scale_skyCover[i];
		p_MonthlyValues[i + 12 * 4] = w->scale_wind[i];
		p_MonthlyValues[i + 12 * 5] = w->scale_rH[i];
		p_MonthlyValues[i + 12 * 6] = w->scale_actVapPress[i];
		p_MonthlyValues[i + 12 * 7] = w->scale_shortWaveRad[i];
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

	SET_SLOT(SW_WTH, install(cSW_WTH_names[0]), MonthlyScalingParams);
	SET_SLOT(SW_WTH, install(cSW_WTH_names[1]), use_snow);
	SET_SLOT(SW_WTH, install(cSW_WTH_names[2]), pct_snowdrift);
	SET_SLOT(SW_WTH, install(cSW_WTH_names[3]), pct_snowRunoff);
	SET_SLOT(SW_WTH, install(cSW_WTH_names[4]), use_weathergenerator);
	SET_SLOT(SW_WTH, install(cSW_WTH_names[5]), use_weathergenerator_only);
	SET_SLOT(SW_WTH, install(cSW_WTH_names[6]), yr_first);
	SET_SLOT(SW_WTH, install(cSW_WTH_names[7]), use_cloudCoverMonthly);
	SET_SLOT(SW_WTH, install(cSW_WTH_names[8]), use_windSpeedMonthly);
	SET_SLOT(SW_WTH, install(cSW_WTH_names[9]), use_humidityMonthly);
	SET_SLOT(SW_WTH, install(cSW_WTH_names[10]), desc_rsds);
	SET_SLOT(SW_WTH, install(cSW_WTH_names[11]), dailyInputFlags);


	UNPROTECT(17);
	return SW_WTH;
}


/**
  @brief Copy weather setup from `rSOILWAT2` S4 `swWeather`
    to `SOILWAT2` `SW_WEATHER`

  Called by `rSW_CTL_obtain_inputs()` if `from_files` is `FALSE`.
*/
void onSet_SW_WTH_setup(SEXP SW_WTH, LOG_INFO* LogInfo) {
	int i;
	SW_WEATHER *w = &SoilWatAll.Weather;
	SEXP
        use_snow, pct_snowdrift, pct_snowRunoff,
        use_weathergenerator, use_weathergenerator_only,
        use_cloudCoverMonthly, use_windSpeedMonthly, use_humidityMonthly,
        desc_rsds,
        dailyInputFlags;
	SEXP MonthlyScalingParams;
	RealD *p_MonthlyValues;
	int *p_dailyInputFlags;

	MyFileName = PathInfo.InFiles[eWeather];

    // Copy weather prefix from PathInfo to Weather within `SoilWatAll`
    strcpy(SoilWatAll.Weather.name_prefix, PathInfo.weather_prefix);

	PROTECT(MonthlyScalingParams = GET_SLOT(SW_WTH, install(cSW_WTH_names[0])));
	p_MonthlyValues = REAL(MonthlyScalingParams);
	for (i = 0; i < 12; i++) {
		w->scale_precip[i] = p_MonthlyValues[i + 12 * 0];
		w->scale_temp_max[i] = p_MonthlyValues[i + 12 * 1];
		w->scale_temp_min[i] = p_MonthlyValues[i + 12 * 2];
		w->scale_skyCover[i] = p_MonthlyValues[i + 12 * 3];
		w->scale_wind[i] = p_MonthlyValues[i + 12 * 4];
		w->scale_rH[i] = p_MonthlyValues[i + 12 * 5];
		w->scale_actVapPress[i] = p_MonthlyValues[i + 12 * 6];
		w->scale_shortWaveRad[i] = p_MonthlyValues[i + 12 * 7];
	}

	PROTECT(use_snow = GET_SLOT(SW_WTH, install(cSW_WTH_names[1])));
	w->use_snow = (Bool) *INTEGER(use_snow);
	PROTECT(pct_snowdrift = GET_SLOT(SW_WTH, install(cSW_WTH_names[2])));
	w->pct_snowdrift = *REAL(pct_snowdrift);
	PROTECT(pct_snowRunoff = GET_SLOT(SW_WTH, install(cSW_WTH_names[3])));
	w->pct_snowRunoff = *REAL(pct_snowRunoff);

	PROTECT(use_weathergenerator = GET_SLOT(SW_WTH, install(cSW_WTH_names[4])));
	w->generateWeatherMethod = *INTEGER(use_weathergenerator) ? 2 : 0;
	PROTECT(use_weathergenerator_only = GET_SLOT(SW_WTH, install(cSW_WTH_names[5])));
	w->use_weathergenerator_only = (Bool) *INTEGER(use_weathergenerator_only);
	if (w->use_weathergenerator_only) {
		w->generateWeatherMethod = 2;
	}

	/* `SW_weather.yr` was removed from SOILWAT2:
	SEXP yr_first;
	int tmp;

	PROTECT(yr_first = GET_SLOT(SW_WTH, install(cSW_WTH_names[6])));
	tmp = *INTEGER(yr_first);
	w->yr.first = (tmp < 0) ? SW_Model.startyr : yearto4digit(tmp);
	*/

	PROTECT(use_cloudCoverMonthly = GET_SLOT(SW_WTH, install(cSW_WTH_names[7])));
	w->use_cloudCoverMonthly = (Bool) *INTEGER(use_cloudCoverMonthly);
	PROTECT(use_windSpeedMonthly = GET_SLOT(SW_WTH, install(cSW_WTH_names[8])));
	w->use_windSpeedMonthly = (Bool) *INTEGER(use_windSpeedMonthly);
	PROTECT(use_humidityMonthly = GET_SLOT(SW_WTH, install(cSW_WTH_names[9])));
	w->use_humidityMonthly = (Bool) *INTEGER(use_humidityMonthly);

	PROTECT(desc_rsds = GET_SLOT(SW_WTH, install(cSW_WTH_names[10])));
	w->desc_rsds = (unsigned int) *INTEGER(desc_rsds);

	PROTECT(dailyInputFlags = GET_SLOT(SW_WTH, install(cSW_WTH_names[11])));
	p_dailyInputFlags = INTEGER(dailyInputFlags);
	for (i = 0; i < MAX_INPUT_COLUMNS; i++) {
		w->dailyInputFlags[i] = (Bool) p_dailyInputFlags[i];
	}

  /* `SW_weather.yr` was removed from SOILWAT2:
	w->yr.last = SW_Model.endyr;
	w->yr.total = w->yr.last - w->yr.first + 1;

	if (SW_Weather.generateWeatherMethod != 2 && SW_Model.startyr < w->yr.first) {
		LogError(
			logfp,
			LOGERROR,
			"%s : Model year (%d) starts before weather files (%d)"
				" and weather generator turned off.\n"
				" Please synchronize the years or set up the weather generator files",
			MyFileName, SW_Model.startyr, w->startYear
		);
	}
	*/

	check_and_update_dailyInputFlags(
		w->use_cloudCoverMonthly,
		w->use_humidityMonthly,
		w->use_windSpeedMonthly,
		w->dailyInputFlags,
    LogInfo
	);

	UNPROTECT(11);
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

	PROTECT(WTH_DATA = allocVector(VECSXP, SoilWatAll.Weather.n_years));
	PROTECT(WTH_DATA_names = allocVector(STRSXP, SoilWatAll.Weather.n_years));

	for (yearIndex = 0; yearIndex < SoilWatAll.Weather.n_years; yearIndex++) {
		year = SoilWatAll.Weather.startYear + yearIndex;
		snprintf(cYear, sizeof cYear, "%4d", year);
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

  Daily weather elements that are not internally stored by `SOILWAT2` are
  returned as missing values; these are
  `"windSpeed_east_mPERs"`, `"windSpeed_north_mPERs"`,
  `"rHmax_pct"`, `"rHmin_pct"`, `"specHavg_pct"`, `"Tdewpoint_C"`

  Called by `onGet_WTH_DATA()`
*/
SEXP onGet_WTH_DATA_YEAR(TimeInt year) {
	int i, days, yearIndex;
	const int nitems = 15;
	SEXP swWeatherData;
	SEXP WeatherData;
	SEXP Year, Year_names, Year_names_y;
	SEXP nYear;
	char *cYear[] = {
		"DOY",
		"Tmax_C", "Tmin_C", "PPT_cm",
		"cloudCov_pct",
		"windSpeed_mPERs", "windSpeed_east_mPERs", "windSpeed_north_mPERs",
		"rHavg_pct", "rHmax_pct", "rHmin_pct", "specHavg_pct", "Tdewpoint_C",
		"actVP_kPa",
		"shortWR"
	};
	RealD *p_Year;
	SW_WEATHER *w = &SoilWatAll.Weather;

	days = Time_get_lastdoy_y(year);
	yearIndex = year - SoilWatAll.Weather.startYear;

	PROTECT(swWeatherData = MAKE_CLASS("swWeatherData"));
	PROTECT(WeatherData = NEW_OBJECT(swWeatherData));

	PROTECT(nYear = NEW_INTEGER(1));
	INTEGER(nYear)[0] = year;

	PROTECT(Year = allocMatrix(REALSXP, days, nitems));
	p_Year = REAL(Year);
	for (i = 0; i < days; i++) {
		p_Year[i + days * 0] = (i + 1);
		p_Year[i + days * (TEMP_MAX + 1)] = w->allHist[yearIndex]->temp_max[i];
		p_Year[i + days * (TEMP_MIN + 1)] = w->allHist[yearIndex]->temp_min[i];
		p_Year[i + days * (PPT + 1)] = w->allHist[yearIndex]->ppt[i];

		p_Year[i + days * (CLOUD_COV + 1)] = w->allHist[yearIndex]->cloudcov_daily[i];

		p_Year[i + days * (WIND_SPEED + 1)] = w->allHist[yearIndex]->windspeed_daily[i];
		p_Year[i + days * (WIND_EAST + 1)] = NA_REAL; // windSpeed_east_mPERs
		p_Year[i + days * (WIND_NORTH + 1)] = NA_REAL; // windSpeed_north_mPERs

		p_Year[i + days * (REL_HUMID + 1)] = w->allHist[yearIndex]->r_humidity_daily[i];
		p_Year[i + days * (REL_HUMID_MAX + 1)] = NA_REAL; // rHmax_pct
		p_Year[i + days * (REL_HUMID_MIN + 1)] = NA_REAL; // rHmin_pct
		p_Year[i + days * (SPEC_HUMID + 1)] = NA_REAL; // specHavg_pct
		p_Year[i + days * (TEMP_DEWPOINT + 1)] = NA_REAL; // Tdewpoint_C

		p_Year[i + days * (ACTUAL_VP + 1)] = w->allHist[yearIndex]->actualVaporPressure[i];
		p_Year[i + days * (SHORT_WR + 1)] = w->allHist[yearIndex]->shortWaveRad[i];
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

  @note `SW_Weather` (via `onSet_SW_WTH_setup()`) and
    `SW_Sky` (via `onSet_SW_SKY()`) must be set/updated
    before this function is called.
*/
void onSet_WTH_DATA(SEXP weatherList, LOG_INFO* LogInfo) {


  // Deallocate (previous, if any) `allHist`
  // (using value of `SW_Weather.n_years` previously used to allocate)
  // `SW_WTH_construct()` sets `n_years` to zero
  deallocateAllWeather(&SoilWatAll.Weather);

  // Update number of years and first calendar year represented
  SoilWatAll.Weather.n_years = SoilWatAll.Model.endyr - SoilWatAll.Model.startyr + 1;
  SoilWatAll.Weather.startYear = SoilWatAll.Model.startyr;

  // Allocate new `allHist` (based on current `SW_Weather.n_years`)
  allocateAllWeather(&SoilWatAll.Weather, LogInfo);
  if(LogInfo->stopRun) {
    return; // Exit function prematurely due to error
  }


  // Equivalent to `readAllWeather()`:
  // fill `SOILWAT2` `allHist` with values from `rSOILWAT2`
  rSW2_setAllWeather(
    weatherList,
    SoilWatAll.Weather.allHist,
    SoilWatAll.Weather.startYear,
    SoilWatAll.Weather.n_years,
    SoilWatAll.Weather.use_weathergenerator_only,
    SoilWatAll.Weather.use_cloudCoverMonthly,
    SoilWatAll.Weather.use_humidityMonthly,
    SoilWatAll.Weather.use_windSpeedMonthly,
    SoilWatAll.Weather.dailyInputFlags,
    SoilWatAll.Sky.cloudcov,
    SoilWatAll.Sky.windspeed,
    SoilWatAll.Sky.r_humidity,
    LogInfo
  );
}


// Equivalent to `readAllWeather()`:
// fill `SOILWAT2` `allHist` with values from `rSOILWAT2`
static void rSW2_setAllWeather(
  SEXP listAllW,
  SW_WEATHER_HIST **allHist,
  int startYear,
  unsigned int n_years,
  Bool use_weathergenerator_only,
  Bool use_cloudCoverMonthly,
  Bool use_humidityMonthly,
  Bool use_windSpeedMonthly,
  Bool *dailyInputFlags,
  RealD *cloudcov,
  RealD *windspeed,
  RealD *r_humidity,
  LOG_INFO* LogInfo
) {
    unsigned int yearIndex, year;

    /* Interpolation is to be in base0 in `interpolate_monthlyValues()` */
    Bool interpAsBase1 = swFALSE;
    SW_MODEL *SW_Model = &SoilWatAll.Model;

    for(yearIndex = 0; yearIndex < n_years; yearIndex++) {
        year = yearIndex + startYear;

        // Set all daily weather values to missing
        _clear_hist_weather(allHist[yearIndex]);

        // Update yearly day/month information needed when interpolating
        // cloud cover, wind speed, and relative humidity if necessary
        Time_new_year(year, SW_Model->days_in_month, SW_Model->cum_monthdays);

        if(use_cloudCoverMonthly) {
            interpolate_monthlyValues(cloudcov, interpAsBase1,
                    SW_Model->cum_monthdays, SW_Model->days_in_month,
                    allHist[yearIndex]->cloudcov_daily);
        }

        if(use_humidityMonthly) {
            interpolate_monthlyValues(r_humidity, interpAsBase1,
                    SW_Model->cum_monthdays, SW_Model->days_in_month,
                    allHist[yearIndex]->r_humidity_daily);
        }

        if(use_windSpeedMonthly) {
            interpolate_monthlyValues(windspeed, interpAsBase1,
                    SW_Model->cum_monthdays, SW_Model->days_in_month,
                    allHist[yearIndex]->windspeed_daily);
        }

        // Read daily weather values from disk
        if (!use_weathergenerator_only) {

            rSW2_set_weather_hist(
              listAllW,
              year,
              allHist[yearIndex],
              dailyInputFlags,
              LogInfo
            );

            if(LogInfo->stopRun) {
                return; // Exit function prematurely due to error
            }
        }
    }
}


// Equivalent to `_read_weather_hist()`:
// fill `SOILWAT2` `allHist` with values from `rSOILWAT2`
static void rSW2_set_weather_hist(
  SEXP listAllW,
  TimeInt year,
  SW_WEATHER_HIST *yearWeather,
  Bool *dailyInputFlags,
  LOG_INFO* LogInfo
) {

    // Locate suitable year among rSOILWAT2 list of `swWeatherData`
    int i, nList = LENGTH(listAllW);
    TimeInt numDaysYear;
    Bool weth_found = FALSE;
    SEXP tmpW, yrWData;
    double *p_yrWData;

    for (i = 0; !weth_found && i < nList; i++) {
      tmpW = VECTOR_ELT(listAllW, i);
      weth_found = (Bool) year == *INTEGER(GET_SLOT(tmpW, install("year")));
    }

    if (weth_found) {
      yrWData = GET_SLOT(tmpW, install("data"));
      weth_found = !isnull(yrWData);
    }

    if (!weth_found) {
      return;
    }

    numDaysYear = Time_get_lastdoy_y(year);
    if (nrows(yrWData) != numDaysYear) {
      LogError(
        LogInfo,
        LOGERROR,
        "Weather data (year %d): "
        "expected %d rows (had %d).\n",
        year,
        numDaysYear,
        nrows(yrWData)
      );
      return; // Exit function prematurely due to error
    }

    // Suitable year among rSOILWAT2 weather list located
    p_yrWData = REAL(yrWData);


    // Copy values from rSOILWAT2 to SOILWAT2
    int doy;

    // Pre-calculate logic for calculation of daily variables
    Bool hasMaxMinTemp = (Bool) (dailyInputFlags[TEMP_MAX] && dailyInputFlags[TEMP_MIN]);
    Bool hasMaxMinRelHumid = (Bool) (dailyInputFlags[REL_HUMID_MAX] && dailyInputFlags[REL_HUMID_MIN]);
    Bool hasEastNorthWind = (Bool) (dailyInputFlags[WIND_EAST] && dailyInputFlags[WIND_NORTH]);

    // Calculate if daily input values of humidity are to be used instead of
    // being interpolated from monthly values
    Bool useHumidityDaily = (Bool) (hasMaxMinRelHumid || dailyInputFlags[REL_HUMID] ||
                                    dailyInputFlags[SPEC_HUMID] || dailyInputFlags[ACTUAL_VP]);

    double v1, v2, es, e, relHum, tempSlope, svpVal;


    // Loop over days of current year
    for (doy = 0; doy < numDaysYear; doy++) {

        /* --- Make the assignments ---- */
        // (Translate R's `NA` to SOILWAT2's `SW_MISSING`)

        if (doy != p_yrWData[doy + numDaysYear * 0] - 1) {
            LogError(
                LogInfo,
                LOGERROR,
                "Weather data (year %d): "
                "day of year out of range (%d), expected: %d.\n",
                year,
                p_yrWData[doy + numDaysYear * 0],
                doy + 1
            );
            return; // Exit function prematurely due to error
        }

        // Maximum daily temperature [C]
        yearWeather->temp_max[doy] = value_or_missing(
            p_yrWData[doy + numDaysYear * (TEMP_MAX + 1)]
        );

        // Minimum daily temperature [C]
        yearWeather->temp_min[doy] = value_or_missing(
            p_yrWData[doy + numDaysYear * (TEMP_MIN + 1)]
        );

        // Calculate average air temperature if min/max not missing
        if (
            !missing(yearWeather->temp_max[doy]) &&
            !missing(yearWeather->temp_min[doy])
        ) {
            yearWeather->temp_avg[doy] =
                (yearWeather->temp_max[doy] + yearWeather->temp_min[doy]) / 2.0;
        }

        // Precipitation [cm]
        yearWeather->ppt[doy] = value_or_missing(
            p_yrWData[doy + numDaysYear * (PPT + 1)]
        );


        // Cloud cover [%]
        if (dailyInputFlags[CLOUD_COV]) {
            yearWeather->cloudcov_daily[doy] = value_or_missing(
                p_yrWData[doy + numDaysYear * (CLOUD_COV + 1)]
            );
        }


        // Wind speed [m/s]
        if (dailyInputFlags[WIND_SPEED]) {
            yearWeather->windspeed_daily[doy] = value_or_missing(
                p_yrWData[doy + numDaysYear * (WIND_SPEED + 1)]
            );

        } else if (hasEastNorthWind) {

            // Make sure wind is not calculated over SW_MISSING
            v1 = value_or_missing(
                p_yrWData[doy + numDaysYear * (WIND_EAST + 1)]
            );
            v2 = value_or_missing(
                p_yrWData[doy + numDaysYear * (WIND_NORTH + 1)]
            );

            if (!missing(v1) && !missing(v2)) {
                yearWeather->windspeed_daily[doy] = sqrt(
                    squared(v1) + squared(v2)
                );
            }
        }


        // Relative humidity [%]
        if (useHumidityDaily) {
            if (hasMaxMinRelHumid) {
                // Make sure rH is not calculated over SW_MISSING
                v1 = value_or_missing(
                    p_yrWData[doy + numDaysYear * (REL_HUMID_MAX + 1)]
                );
                v2 = value_or_missing(
                    p_yrWData[doy + numDaysYear * (REL_HUMID_MIN + 1)]
                );

                if (!missing(v1) && !missing(v2)) {
                    yearWeather->r_humidity_daily[doy] = (v1 + v2) / 2;
                }

            } else if (dailyInputFlags[REL_HUMID]) {
                yearWeather->r_humidity_daily[doy] = value_or_missing(
                    p_yrWData[doy + numDaysYear * (REL_HUMID + 1)]
                );

            } else if (dailyInputFlags[SPEC_HUMID]) {
                // Make sure rH is not calculated over SW_MISSING
                v1 = value_or_missing(
                    p_yrWData[doy + numDaysYear * (SPEC_HUMID + 1)]
                );

                if (!missing(yearWeather->temp_avg[doy]) && !missing(v1)) {
                    // Specific humidity (Bolton 1980)
                    es = 6.112 * exp(17.67 * yearWeather->temp_avg[doy]) /
                         (yearWeather->temp_avg[doy] + 243.5);

                    e = (v1 * 1013.25) / (.378 * v1 + .622);

                    relHum = e / es;
                    relHum = max(0., relHum);

                    yearWeather->r_humidity_daily[doy] = min(100., relHum);
                }
            }

            // Actual vapor pressure [kPa]
            if (dailyInputFlags[ACTUAL_VP]) {
                yearWeather->actualVaporPressure[doy] = value_or_missing(
                    p_yrWData[doy + numDaysYear * (ACTUAL_VP + 1)]
                );

            } else {
                v1 = value_or_missing(
                    p_yrWData[doy + numDaysYear * (TEMP_DEWPOINT + 1)]
                );


                if (dailyInputFlags[TEMP_DEWPOINT] && !missing(v1)) {
                    yearWeather->actualVaporPressure[doy] = actualVaporPressure3(v1);

                } else if (hasMaxMinTemp && hasMaxMinRelHumid) {
                    // Make sure vp is not calculated over SW_MISSING
                    v1 = value_or_missing(
                        p_yrWData[doy + numDaysYear * (REL_HUMID_MAX + 1)]
                    );
                    v2 = value_or_missing(
                        p_yrWData[doy + numDaysYear * (REL_HUMID_MIN + 1)]
                    );

                    if (
                        !missing(yearWeather->temp_max[doy]) &&
                        !missing(yearWeather->temp_min[doy]) &&
                        !missing(v1) && !missing(v2)
                    ) {
                        yearWeather->actualVaporPressure[doy] = actualVaporPressure2(
                            v1,
                            v2,
                            yearWeather->temp_max[doy],
                            yearWeather->temp_min[doy]
                        );
                    }

                } else if (dailyInputFlags[REL_HUMID] || dailyInputFlags[SPEC_HUMID]) {
                    if (
                        !missing(yearWeather->r_humidity_daily[doy]) &&
                        !missing(yearWeather->temp_avg[doy])
                    ) {
                        yearWeather->actualVaporPressure[doy] = actualVaporPressure1(
                            yearWeather->r_humidity_daily[doy],
                            yearWeather->temp_avg[doy]
                        );
                    }
                }
            }


            // Relative humidity [%] if still missing
            if (
                missing(yearWeather->r_humidity_daily[doy]) &&
                (dailyInputFlags[ACTUAL_VP] || dailyInputFlags[TEMP_DEWPOINT])
            ) {
                // Make sure rH is not calculated over SW_MISSING
                if (
                    !missing(yearWeather->temp_avg[doy]) &&
                    !missing(yearWeather->actualVaporPressure[doy])
                ) {
                    svpVal = svp(yearWeather->temp_avg[doy], &tempSlope);

                    yearWeather->r_humidity_daily[doy] =
                        yearWeather->actualVaporPressure[doy] / svpVal;
                }
            }
        }


        // Downward surface shortwave radiation
        if (dailyInputFlags[SHORT_WR]) {
            yearWeather->shortWaveRad[doy] = value_or_missing(
                p_yrWData[doy + numDaysYear * (SHORT_WR + 1)]
            );
        }
    }
}

// `calc_SiteClimate()` is R interface to rSW2_calc_SiteClimate()
SEXP rSW2_calc_SiteClimate(SEXP weatherList, SEXP yearStart, SEXP yearEnd,
                           SEXP do_C4vars, SEXP do_Cheatgrass_ClimVars, SEXP latitude) {

    SW_WEATHER_HIST **allHist = NULL;

    SW_CLIMATE_YEARLY climateOutput;
    SW_CLIMATE_CLIM climateAverages;

    LOG_INFO local_LogInfo;
    sw_init_logs(current_sw_verbosity, &local_LogInfo);

    TimeInt
      days_in_month[MAX_MONTHS],
      cum_monthdays[MAX_MONTHS];


    int numYears = asInteger(yearEnd) - asInteger(yearStart) + 1, year, calcSiteOutputNum = 10,
    index, numUnprotects = 11;

    Bool dailyInputFlags[MAX_INPUT_COLUMNS];
    double cloudcov[MAX_MONTHS], windspeed[MAX_MONTHS], r_humidity[MAX_MONTHS];

    SEXP res = NULL, monthlyMean, monthlyMax, monthlyMin, monthlyPPT, MAT_C, MAP_cm, vectorNames,
    C4Variables, Cheatgrass, cnamesC4SEXP, cnamesCheatgrassSEXP;

    char *cnames[] = {"meanMonthlyTempC","minMonthlyTempC","maxMonthlyTempC",
        "meanMonthlyPPTcm","MAP_cm","MAT_C", "dailyTempMin", "dailyTempMean",
        "dailyC4vars","Cheatgrass_ClimVars"};

    char *cnamesC4[] = {"Month7th_NSadj_MinTemp_C","LengthFreezeFreeGrowingPeriod_NSadj_Days",
        "DegreeDaysAbove65F_NSadj_DaysC","Month7th_NSadj_MinTemp_C.sd", "LengthFreezeFreeGrowingPeriod_NSadj_Days.sd",
        "DegreeDaysAbove65F_NSadj_DaysC.sd"};

    char *cnamesCheatgrass[] = {"Month7th_PPT_mm","MeanTemp_ofDriestQuarter_C","MinTemp_of2ndMonth_C",
        "Month7th_PPT_mm_SD","MeanTemp_ofDriestQuarter_C_SD","MinTemp_of2ndMonth_C_SD"};

    Bool inNorthHem = asReal(latitude) > 0.0;

    monthlyMean = PROTECT(allocVector(REALSXP, MAX_MONTHS));
    monthlyMax = PROTECT(allocVector(REALSXP, MAX_MONTHS));
    monthlyMin = PROTECT(allocVector(REALSXP, MAX_MONTHS));
    monthlyPPT = PROTECT(allocVector(REALSXP, MAX_MONTHS));
    MAT_C = PROTECT(allocVector(REALSXP, 1));
    MAP_cm = PROTECT(allocVector(REALSXP, 1));
    vectorNames = PROTECT(allocVector(STRSXP, calcSiteOutputNum));
    cnamesC4SEXP = PROTECT(allocVector(STRSXP, 6));
    cnamesCheatgrassSEXP = PROTECT(allocVector(STRSXP, 6));
    C4Variables = PROTECT(allocVector(REALSXP, 6));
    Cheatgrass = PROTECT(allocVector(REALSXP, 6));

    allHist = (SW_WEATHER_HIST **)Mem_Malloc(sizeof(SW_WEATHER_HIST *) * numYears,
                                            "rSW2_calc_SiteClimate",
                                            &local_LogInfo);
    if(local_LogInfo.stopRun) {
        goto report;
    }

    init_allHist_years(allHist, numYears);

    for(year = 0; year < numYears; year++) {
        allHist[year] = (SW_WEATHER_HIST *)Mem_Malloc(sizeof(SW_WEATHER_HIST),
                                                      "rSW2_calc_SiteClimate",
                                                      &local_LogInfo);
        if(local_LogInfo.stopRun) {
            goto report;
        }
    }

    Time_init_model(days_in_month);

    // Set `dailyInputFlags`: currently, `calcSiteClimate()` use only tmax, tmin, ppt
    for (index = 0; index < MAX_INPUT_COLUMNS; index++) {
      dailyInputFlags[index] = FALSE;
    }
    dailyInputFlags[TEMP_MAX] = TRUE;
    dailyInputFlags[TEMP_MIN] = TRUE;
    dailyInputFlags[PPT] = TRUE;

    // Fill SOILWAT `allHist` with data from weatherList
    rSW2_setAllWeather(
      weatherList,
      allHist,
      asInteger(yearStart),
      numYears,
      FALSE, // use_weathergenerator_only
      FALSE, // use_cloudCoverMonthly,
      FALSE, // use_windSpeedMonthly,
      FALSE, // use_humidityMonthly,
      dailyInputFlags,
      cloudcov,
      windspeed,
      r_humidity,
      &local_LogInfo
    );
    if(local_LogInfo.stopRun) {
        goto report;
    }

    // Allocate memory of structs for climate on SOILWAT side
    allocateClimateStructs(numYears, &climateOutput, &climateAverages,
                           &local_LogInfo);
    if(local_LogInfo.stopRun) {
        goto report;
    }

    // Calculate climate variables
    calcSiteClimate(allHist, cum_monthdays, days_in_month,
                numYears, asInteger(yearStart), inNorthHem, &climateOutput);

    // Average climate variables
    averageClimateAcrossYears(&climateOutput, numYears, &climateAverages);

    res = PROTECT(allocVector(VECSXP, calcSiteOutputNum));
    numUnprotects++;

    double *xmonthlyMean = REAL(monthlyMean), *xmonthlyMax = REAL(monthlyMax),
    *xmonthlyMin = REAL(monthlyMin), *xmontlyPPT = REAL(monthlyPPT);

    for(index = 0; index < MAX_MONTHS; index++) {
        xmonthlyMean[index] = climateAverages.meanTempMon_C[index];
        xmonthlyMax[index] = climateAverages.maxTempMon_C[index];
        xmonthlyMin[index] = climateAverages.minTempMon_C[index];
        xmontlyPPT[index] = climateAverages.PPTMon_cm[index];
    }

    for(index = 0; index < calcSiteOutputNum; index++) {
        SET_STRING_ELT(vectorNames, index, mkChar(cnames[index]));
    }

    for(index = 0; index < 6; index++) {
        SET_STRING_ELT(cnamesC4SEXP, index, mkChar(cnamesC4[index]));
        SET_STRING_ELT(cnamesCheatgrassSEXP, index, mkChar(cnamesCheatgrass[index]));
    }

    // Set names of res, C4Variables and Cheatgrass
    namesgets(res, vectorNames);
    namesgets(C4Variables, cnamesC4SEXP);
    namesgets(Cheatgrass, cnamesCheatgrassSEXP);

    // Set mean annual temperature and precipitation values
    REAL(MAT_C)[0] = climateAverages.meanTemp_C;
    REAL(MAP_cm)[0] = climateAverages.PPT_cm;

    // Set C4Variables and Cheatgrass values

    REAL(C4Variables)[0] = climateAverages.minTemp7thMon_C;
    REAL(C4Variables)[1] = climateAverages.frostFree_days;
    REAL(C4Variables)[2] = climateAverages.ddAbove65F_degday;

    REAL(C4Variables)[3] = climateAverages.sdC4[0];
    REAL(C4Variables)[4] = climateAverages.sdC4[1];
    REAL(C4Variables)[5] = climateAverages.sdC4[2];

    REAL(Cheatgrass)[0] = climateAverages.PPT7thMon_mm;
    REAL(Cheatgrass)[1] = climateAverages.meanTempDriestQtr_C;
    REAL(Cheatgrass)[2] = climateAverages.minTemp2ndMon_C;

    REAL(Cheatgrass)[3] = climateAverages.sdCheatgrass[0];
    REAL(Cheatgrass)[4] = climateAverages.sdCheatgrass[1];
    REAL(Cheatgrass)[5] = climateAverages.sdCheatgrass[2];

    // Set mean average monthly temperature
    SET_VECTOR_ELT(res, 0, monthlyMean);

    // Set mean minimum temperature
    SET_VECTOR_ELT(res, 1, monthlyMin);

    // Set mean maximum temperature
    SET_VECTOR_ELT(res, 2, monthlyMax);

    // Set mean annual precipitation (cm)
    SET_VECTOR_ELT(res, 3, monthlyPPT);

    // Set mean annual temperature (C)
    SET_VECTOR_ELT(res, 4, MAP_cm);

    // Set mean annual precipitation (cm)
    SET_VECTOR_ELT(res, 5, MAT_C);

    // Set values of the two standard deviation categories (C4 and cheatgrass)
    // in result variable
    SET_VECTOR_ELT(res, 8, C4Variables);
    SET_VECTOR_ELT(res, 9, Cheatgrass);

    report: {
        // Note: no SOILWAT2 memory was allocated
        UNPROTECT(numUnprotects);
        deallocateClimateStructs(&climateOutput, &climateAverages);
        free_allHist(allHist, numYears);

        sw_write_warnings(&local_LogInfo);
        sw_fail_on_error(&local_LogInfo);
    }

    return res;

}

void init_allHist_years(SW_WEATHER_HIST **allHist, int numYears) {
    int year;

    for(year = 0; year < numYears; year++) {
        allHist[year] = NULL;
    }
}

void free_allHist(SW_WEATHER_HIST **allHist, int numYears) {
    int year;

    if(!isnull(allHist)) {

        for(year = 0; year < numYears; year++) {
            if(!isnull(allHist[year])) {
                free(allHist[year]);
            }
        }

        free(allHist);
        allHist = NULL;
    }
}
