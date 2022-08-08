/*
 * SW_R_lib.h
 *
 *  Created on: Jun 25, 2013
 *      Author: Ryan Murphy
 */
#ifndef SW_R_LIB_H_
#define SW_R_LIB_H_

#include "SOILWAT2/SW_Model.h"
#include "SOILWAT2/SW_Site.h"
#include "SOILWAT2/SW_VegEstab.h"
#include "SOILWAT2/SW_Output.h"
#include "SOILWAT2/SW_Weather.h"
#include "SOILWAT2/SW_Sky.h"
#include "SOILWAT2/SW_VegProd.h"
#include "SOILWAT2/SW_VegEstab.h"
#include "SOILWAT2/SW_SoilWater.h"
#include "SOILWAT2/SW_Markov.h"
#include "SOILWAT2/SW_Control.h"

#include <R.h>
#include <Rdefines.h>
#include <Rconfig.h>
#include <Rinternals.h>



/* =================================================== */
/*            Externed Global Variables                */
/* --------------------------------------------------- */
extern SEXP InputData;
extern SEXP WeatherList;
extern Bool useFiles;
extern Bool bWeatherList;


/* =================================================== */
/*             Global Function Declarations            */
/* --------------------------------------------------- */
SEXP tempError(void);
SEXP onGetInputDataFromFiles(SEXP input);
SEXP start(SEXP inputOptions, SEXP inputData, SEXP weatherList, SEXP quiet);
SEXP rSW2_processAllWeather(SEXP weatherList, SEXP inputData);
SEXP sw_consts(void);

#endif /* SW_R_LIB_H_ */
