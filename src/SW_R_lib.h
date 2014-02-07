/*
 * SW_R_lib.h
 *
 *  Created on: Jun 25, 2013
 *      Author: Ryan Murphy
 */
#ifdef RSOILWAT

#ifndef SW_R_LIB_H_
#define SW_R_LIB_H_

#include "SW_Model.h"
#include "SW_Site.h"
#include "SW_VegEstab.h"
#include "SW_Output.h"
#include "SW_Weather.h"
#include "SW_Sky.h"
#include "SW_VegProd.h"
#include "SW_VegEstab.h"
#include "SW_SoilWater.h"
#include "SW_Markov.h"

#include <R.h>
#include <Rdefines.h>
#include <Rconfig.h>
#include <Rinterface.h>
#include <Rinternals.h>

void init_args(int argc, char **argv);
void usage(void);
void init_args(int argc, char **argv);
void SW_CTL_main(void);
void SW_CTL_init_model(const char *firstfile);

SEXP onGetInputDataFromFiles(SEXP input);
SEXP start(SEXP inputOptions, SEXP inputData, SEXP weatherList);
SEXP onGetOutput(SEXP inputData);

#endif /* SW_R_LIB_H_ */

#endif
