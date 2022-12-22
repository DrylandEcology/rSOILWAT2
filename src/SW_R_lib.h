/*
 * SW_R_lib.h
 *
 *  Created on: Jun 25, 2013
 *      Author: Ryan Murphy
 */
#ifndef SW_R_LIB_H_
#define SW_R_LIB_H_

#include "SOILWAT2/include/SW_Control.h"

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
SEXP sw_quiet(SEXP quiet);
SEXP tempError(void);
SEXP onGetInputDataFromFiles(SEXP input, SEXP quiet);
SEXP start(SEXP inputOptions, SEXP inputData, SEXP weatherList, SEXP quiet);
SEXP rSW2_processAllWeather(SEXP weatherList, SEXP inputData);
SEXP sw_consts(void);

SEXP rSW2_SWRC_PTF_estimate_parameters(
  SEXP ptf_type,
  SEXP sand,
  SEXP clay,
  SEXP gravel,
  SEXP bdensity
);

SEXP sw_check_SWRC_vs_PTF(SEXP swrc_type, SEXP swrcp);

SEXP rSW2_SWRC(
  SEXP x,
  SEXP direction,
  SEXP swrc_type,
  SEXP swrcp,
  SEXP gravel,
  SEXP width
);

#endif /* SW_R_LIB_H_ */
