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

#include "SOILWAT2/include/SW_Defines.h"
#include "SOILWAT2/include/SW_Files.h"
#include "SOILWAT2/include/SW_Model.h" // externs `SW_Model`
#include "SOILWAT2/include/SW_Markov.h"
#include "SOILWAT2/include/SW_Sky.h"

#include "SOILWAT2/include/SW_Weather.h" // externs `SW_Weather`
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
  "MonthlyScalingParams",
  "use_cloudCoverMonthly",
  "use_windSpeedMonthly",
  "use_relHumidityMonthly",
  "has_temp2",
  "has_ppt",
  "has_cloudCover",
  "has_sfcWind",
  "has_windComp",
  "has_hurs",
  "has_hurs2",
  "has_huss",
  "has_tdps",
  "has_vp",
  "has_rsds"
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
SEXP onGet_SW_WTH_setup(void) {
	int i;
	const int nitems = 8;
	RealD *p_MonthlyValues;
	SW_WEATHER *w = &SW_Weather;

	SEXP swWeather;
	SEXP SW_WTH;

	SEXP
		use_snow, pct_snowdrift, pct_snowRunoff,
		use_weathergenerator, use_weathergenerator_only, yr_first, use_cloudCoverMonthly, use_windSpeedMonthly,
        use_relHumidityMonthly, has_temp2, has_ppt, has_cloudCover, has_sfcWind, has_windComp, has_hurs, has_hurs2,
        has_huss, has_tdps, has_vp, has_rsds;
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
	INTEGER_POINTER(yr_first)[0] = SW_Weather.startYear;

    PROTECT(use_cloudCoverMonthly = NEW_LOGICAL(1));
    LOGICAL_POINTER(use_cloudCoverMonthly)[0] = w->use_cloudCoverMonthly;
    PROTECT(use_windSpeedMonthly = NEW_LOGICAL(1));
    LOGICAL_POINTER(use_windSpeedMonthly)[0] = w->use_windSpeedMonthly;
    PROTECT(use_relHumidityMonthly = NEW_LOGICAL(1));
    LOGICAL_POINTER(use_relHumidityMonthly)[0] = w->use_relHumidityMonthly;

    PROTECT(has_temp2 = NEW_LOGICAL(1));
    LOGICAL_POINTER(has_temp2)[0] = w->has_temp2;
    PROTECT(has_ppt = NEW_LOGICAL(1));
    LOGICAL_POINTER(has_ppt)[0] = w->has_ppt;
    PROTECT(has_cloudCover = NEW_LOGICAL(1));
    LOGICAL_POINTER(has_cloudCover)[0] = w->has_cloudCover;
    PROTECT(has_sfcWind = NEW_LOGICAL(1));
    LOGICAL_POINTER(has_sfcWind)[0] = w->has_sfcWind;
    PROTECT(has_windComp = NEW_LOGICAL(1));
    LOGICAL_POINTER(has_windComp)[0] = w->has_windComp;
    PROTECT(has_hurs = NEW_LOGICAL(1));
    LOGICAL_POINTER(has_hurs)[0] = w->has_hurs;
    PROTECT(has_hurs2 = NEW_LOGICAL(1));
    LOGICAL_POINTER(has_hurs2)[0] = w->has_hurs2;
    PROTECT(has_huss = NEW_LOGICAL(1));
    LOGICAL_POINTER(has_huss)[0] = w->has_huss;
    PROTECT(has_tdps = NEW_LOGICAL(1));
    LOGICAL_POINTER(has_tdps)[0] = w->has_tdps;
    PROTECT(has_vp = NEW_LOGICAL(1));
    LOGICAL_POINTER(has_vp)[0] = w->has_vp;
    PROTECT(has_rsds = NEW_LOGICAL(1));
    LOGICAL_POINTER(has_rsds)[0] = w->has_rsds;

	PROTECT(MonthlyScalingParams = allocMatrix(REALSXP, 12, nitems));
	p_MonthlyValues = REAL(MonthlyScalingParams);
	for (i = 0; i < 12; i++) {
		p_MonthlyValues[i + 12 * 0] = w->scale_precip[i];
		p_MonthlyValues[i + 12 * 1] = w->scale_temp_max[i];
		p_MonthlyValues[i + 12 * 2] = w->scale_temp_min[i];
		p_MonthlyValues[i + 12 * 3] = w->scale_skyCover[i];
		p_MonthlyValues[i + 12 * 4] = w->scale_wind[i];
		p_MonthlyValues[i + 12 * 5] = w->scale_rH[i];
        p_MonthlyValues[i + 12 * 6] = w->scale_shortWaveRad[i];
        p_MonthlyValues[i + 12 * 7] = w->scale_actVapPress[i];
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
    SET_SLOT(SW_WTH, install(cSW_WTH_names[7]), use_cloudCoverMonthly);
    SET_SLOT(SW_WTH, install(cSW_WTH_names[8]), use_windSpeedMonthly);
    SET_SLOT(SW_WTH, install(cSW_WTH_names[9]), use_relHumidityMonthly);
    SET_SLOT(SW_WTH, install(cSW_WTH_names[10]), has_temp2);
    SET_SLOT(SW_WTH, install(cSW_WTH_names[11]), has_ppt);
    SET_SLOT(SW_WTH, install(cSW_WTH_names[12]), has_cloudCover);
    SET_SLOT(SW_WTH, install(cSW_WTH_names[13]), has_sfcWind);
    SET_SLOT(SW_WTH, install(cSW_WTH_names[14]), has_windComp);
    SET_SLOT(SW_WTH, install(cSW_WTH_names[15]), has_hurs);
    SET_SLOT(SW_WTH, install(cSW_WTH_names[16]), has_hurs2);
    SET_SLOT(SW_WTH, install(cSW_WTH_names[17]), has_huss);
    SET_SLOT(SW_WTH, install(cSW_WTH_names[18]), has_tdps);
    SET_SLOT(SW_WTH, install(cSW_WTH_names[19]), has_vp);
    SET_SLOT(SW_WTH, install(cSW_WTH_names[20]), has_rsds);

	UNPROTECT(26);
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
        use_weathergenerator, use_weathergenerator_only, use_cloudCoverMonthly, use_windSpeedMonthly,
        use_relHumidityMonthly, has_temp2, has_ppt, has_cloudCover, has_sfcWind, has_windComp, has_hurs,
        has_hurs2, has_huss, has_tdps, has_vp, has_rsds;
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

    PROTECT(use_cloudCoverMonthly = GET_SLOT(SW_WTH, install(cSW_WTH_names[7])));
    w->use_cloudCoverMonthly = (Bool) *INTEGER(use_cloudCoverMonthly);
    PROTECT(use_windSpeedMonthly = GET_SLOT(SW_WTH, install(cSW_WTH_names[8])));
    w->use_windSpeedMonthly = (Bool) *INTEGER(use_windSpeedMonthly);
    PROTECT(use_relHumidityMonthly = GET_SLOT(SW_WTH, install(cSW_WTH_names[9])));
    w->use_relHumidityMonthly = (Bool) *INTEGER(use_relHumidityMonthly);
    PROTECT(has_temp2 = GET_SLOT(SW_WTH, install(cSW_WTH_names[10])));
    w->has_temp2 = (Bool) *INTEGER(has_temp2);
    PROTECT(has_ppt = GET_SLOT(SW_WTH, install(cSW_WTH_names[11])));
    w->has_ppt = (Bool) *INTEGER(has_ppt) ? 2 : 0;
    PROTECT(has_cloudCover = GET_SLOT(SW_WTH, install(cSW_WTH_names[12])));
    w->has_cloudCover = (Bool) *INTEGER(has_cloudCover);
    PROTECT(has_sfcWind = GET_SLOT(SW_WTH, install(cSW_WTH_names[13])));
    w->has_sfcWind = (Bool) *INTEGER(has_sfcWind);
    PROTECT(has_windComp = GET_SLOT(SW_WTH, install(cSW_WTH_names[14])));
    w->has_windComp = (Bool) *INTEGER(has_windComp);
    PROTECT(has_hurs = GET_SLOT(SW_WTH, install(cSW_WTH_names[15])));
    w->has_hurs = (Bool) *INTEGER(has_hurs);
    PROTECT(has_hurs2 = GET_SLOT(SW_WTH, install(cSW_WTH_names[16])));
    w->has_hurs2 = (Bool) *INTEGER(has_hurs2);
    PROTECT(has_huss = GET_SLOT(SW_WTH, install(cSW_WTH_names[17])));
    w->has_huss = (Bool) *INTEGER(has_huss);
    PROTECT(has_tdps = GET_SLOT(SW_WTH, install(cSW_WTH_names[18])));
    w->has_tdps = (Bool) *INTEGER(has_tdps);
    PROTECT(has_vp = GET_SLOT(SW_WTH, install(cSW_WTH_names[19])));
    w->has_vp = (Bool) *INTEGER(has_vp);
    PROTECT(has_rsds = GET_SLOT(SW_WTH, install(cSW_WTH_names[20])));
    w->has_rsds = (Bool) *INTEGER(has_rsds);
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
        w->scale_actVapPress[i] = p_MonthlyValues[i + 12 * 6];
        w->scale_shortWaveRad[i] = p_MonthlyValues[i + 12 * 7];
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
			MyFileName, SW_Model.startyr, w->startYear
		);
	}
	*/

	UNPROTECT(20);
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

  Called by `onGet_WTH_DATA()`
*/
SEXP onGet_WTH_DATA_YEAR(TimeInt year) {
	int i, days, yearIndex;
	const int nitems = 15;
	SEXP swWeatherData;
	SEXP WeatherData;
	SEXP Year, Year_names, Year_names_y;
	SEXP nYear;
	char *cYear[] = {"DOY", "Tmax_C", "Tmin_C", "PPT_cm", "cloudCov", "windSpeed",
                     "windSpeed_east", "windSpeed_north", "rel_H", "rel_H_max", "rel_H_min",
                     "spec_H", "dewpointTemp_C", "actVP", "shortWR"};
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
        p_Year[i + days * 4] = w->allHist[yearIndex]->cloudcov_daily[i];
        p_Year[i + days * 5] = w->allHist[yearIndex]->windspeed_daily[i];
        p_Year[i + days * 6] = w->allHist[yearIndex]->r_humidity_daily[i];
        p_Year[i + days * 7] = w->allHist[yearIndex]->shortWaveRad[i];
        p_Year[i + days * 8] = w->allHist[yearIndex]->actualVaporPressure[i];
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

        if (nrows(yrWData) != numDaysYear || ncols(yrWData) != MAX_INPUT_COLUMNS + 1) {
          LogError(
            logfp,
            LOGFATAL,
            "Weather data (year %d): wrong dimensions: "
            "expected %d rows (had %d) and 15 columns (had %d).\n",
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

            j = day + numDaysYear * 4;
            allHist[yearIndex]->cloudcov_daily[day] = p_yrWData[j];

            j = day + numDaysYear * 5;
            allHist[yearIndex]->windspeed_daily[day] = p_yrWData[j];

            j = day + numDaysYear * 6;
            allHist[yearIndex]->r_humidity_daily[day] = p_yrWData[j];

            j = day + numDaysYear * 7;
            allHist[yearIndex]->shortWaveRad[day] = p_yrWData[j];

            j = day + numDaysYear * 8;
            allHist[yearIndex]->actualVaporPressure[day] = p_yrWData[j];


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

SEXP rSW2_calc_SiteClimate(SEXP weatherList, SEXP yearStart, SEXP yearEnd,
                           SEXP do_C4vars, SEXP do_Cheatgrass_ClimVars, SEXP latitude) {

    SW_WEATHER_HIST **allHist;

    SW_WEATHER *weather = &SW_Weather;

    SW_CLIMATE_YEARLY climateOutput;
    SW_CLIMATE_CLIM climateAverages;

    int numYears = asInteger(yearEnd) - asInteger(yearStart) + 1, year, calcSiteOutputNum = 10,
    index;

    SEXP res, monthlyMean, monthlyMax, monthlyMin, monthlyPPT, MAT_C, MAP_cm, vectorNames,
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

    allHist = (SW_WEATHER_HIST **)malloc(sizeof(SW_WEATHER_HIST *) * numYears);

    for(year = 0; year < numYears; year++) {
        allHist[year] = (SW_WEATHER_HIST *)malloc(sizeof(SW_WEATHER_HIST));
    }

    Time_init_model();

    // Fill SOILWAT `allHist` with data from weatherList
    rSW2_setAllWeather(weatherList, allHist, asInteger(yearStart), numYears,
                       weather->use_weathergenerator_only);

    // Allocate memory of structs for climate on SOILWAT side
    allocateClimateStructs(numYears, &climateOutput, &climateAverages);

    // Calculate climate variables
    calcSiteClimate(allHist, numYears, asInteger(yearStart), inNorthHem, &climateOutput);

    // Average climate variables
    averageClimateAcrossYears(&climateOutput, numYears, &climateAverages);

    res = PROTECT(allocVector(VECSXP, calcSiteOutputNum));

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

    deallocateClimateStructs(&climateOutput, &climateAverages);

    UNPROTECT(12);

    for(year = 0; year < numYears; year++) {
        free(allHist[year]);
    }
    free(allHist);

    return res;

}
