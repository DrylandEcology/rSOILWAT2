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
static void rSW2_setAllWeather(
  SEXP listAllW,
  SW_WEATHER_HIST **allHist,
  int startYear,
  unsigned int n_years
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
	w->generateWeatherMethod = *INTEGER(use_weathergenerator) ? 2 : 0;
	PROTECT(use_weathergenerator_only = GET_SLOT(SW_WTH, install(cSW_WTH_names[4])));
	w->use_weathergenerator_only = (Bool) *INTEGER(use_weathergenerator_only);
	if (w->use_weathergenerator_only) {
		w->generateWeatherMethod = 2;
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
  @brief Move all weather data from `rSOILWAT2` to `SOILWAT2`, impute
    missing values (weather generator), and apply scaling parameters

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
  deallocateAllWeather();

  // Determine required size of new `allHist`
  SW_Weather.n_years = SW_Model.endyr - SW_Model.startyr + 1;

  // Allocate new `allHist` (based on current `SW_Weather.n_years`)
  allocateAllWeather();


  // Equivalent to `readAllWeather()`:
  // fill `SOILWAT2` `allHist` with values from `rSOILWAT2`
  rSW2_setAllWeather(
    listAllWeather,
    SW_Weather.allHist,
    SW_Model.startyr,
    SW_Weather.n_years
  );


  // Generate missing values
  generateMissingWeather(
    SW_Weather.allHist,
    SW_Model.startyr,
    SW_Weather.n_years,
    SW_Weather.generateWeatherMethod,
    3 // optLOCF_nMax (TODO: make this user input)
  );


  // Scale with monthly additive/multiplicative parameters
  scaleAllWeather(
    SW_Weather.allHist,
    SW_Model.startyr,
    SW_Weather.n_years,
    SW_Weather.scale_temp_max,
    SW_Weather.scale_temp_min,
    SW_Weather.scale_precip
  );
}


// Equivalent to `readAllWeather()`:
// fill `SOILWAT2` `allHist` with values from `rSOILWAT2`
static void rSW2_setAllWeather(
  SEXP listAllW,
  SW_WEATHER_HIST **allHist,
  int startYear,
  unsigned int n_years
) {
  int nList, yearIndex, year, numDaysYear, day, doy, i, j;
  Bool weth_found;
  SEXP tmpW, yrWData;
  double *p_yrWData;

  nList = LENGTH(listAllW);

  // Loop over years and move weather data to `SOILWAT2` `allHist`
  for (yearIndex = 0; yearIndex < n_years; yearIndex++) {

    if (SW_Weather.use_weathergenerator_only) {
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

SEXP rSW2_calc_SiteClimate(SEXP weatherList, SEXP yearStart, SEXP yearEnd,
                           SEXP do_C4vars, SEXP do_Cheatgrass_ClimVars, SEXP latitude) {

    SW_WEATHER_HIST **allHist;

    SW_CLIMATE_YEARLY climateOutput;
    SW_CLIMATE_CLIM climateAverages;

    int numYears = asInteger(yearEnd) - asInteger(yearStart) + 1, year, calcSiteOutputNum = 10,
    deallocate = 0, allocate = 1, index;

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
    rSW2_setAllWeather(weatherList, allHist, asInteger(yearStart), numYears);

    // Allocate memory of structs for climate on SOILWAT side
    allocDeallocClimateStructs(allocate, numYears, &climateOutput, &climateAverages);

    // Calculate climate variables
    calcSiteClimate(allHist, numYears, asInteger(yearStart), &climateOutput);

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
    REAL(C4Variables)[0] = climateAverages.sdC4[0];
    REAL(C4Variables)[1] = climateAverages.sdC4[1];
    REAL(C4Variables)[2] = climateAverages.sdC4[2];

    REAL(C4Variables)[3] = climateAverages.minTempJuly_C;
    REAL(C4Variables)[4] = climateAverages.frostFree_days;
    REAL(C4Variables)[5] = climateAverages.ddAbove65F_degday;

    REAL(Cheatgrass)[0] = climateAverages.sdCheatgrass[0];
    REAL(Cheatgrass)[1] = climateAverages.sdCheatgrass[1];
    REAL(Cheatgrass)[2] = climateAverages.sdCheatgrass[2];

    REAL(Cheatgrass)[3] = climateAverages.PPTJuly_mm;
    REAL(Cheatgrass)[4] = climateAverages.meanTempDriestQtr_C;
    REAL(Cheatgrass)[5] = climateAverages.minTempFeb_C;

    // Set mean average monthly temperature
    SET_VECTOR_ELT(res, 0, monthlyMean);

    // Set mean minimum temperature
    SET_VECTOR_ELT(res, 1, monthlyMax);

    // Set mean maximum temperature
    SET_VECTOR_ELT(res, 2, monthlyMin);

    // Set mean annual precipitation (cm)
    SET_VECTOR_ELT(res, 3, monthlyPPT);

    // Set mean annual temperature (C)
    SET_VECTOR_ELT(res, 4, MAT_C);

    // Set mean annual precipitation (cm)
    SET_VECTOR_ELT(res, 5, MAP_cm);

    // Set values of the two standard deviation categories (C4 and cheatgrass)
    // in result variable
    SET_VECTOR_ELT(res, 8, C4Variables);
    SET_VECTOR_ELT(res, 9, Cheatgrass);

    allocDeallocClimateStructs(deallocate, numYears, &climateOutput, &climateAverages);

    UNPROTECT(12);

    for(year = 0; year < numYears; year++) {
        free(allHist[year]);
    }
    free(allHist);

    return res;

}
