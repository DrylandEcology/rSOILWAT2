/********************************************************/
/********************************************************/
/*  Source file: Control.c
 *  Type: module
 *  Application: SOILWAT - soilwater dynamics simulator
 *  Purpose: This module controls the flow of the model.
 *           Previously this was done in main() but to
 *           combine the model with other code (eg STEPPE)
 *           there needs to be separate callable routines
 *           for initializing, model flow, and output.
 */
/********************************************************/
/********************************************************/

/* =================================================== */
/*                INCLUDES / DEFINES                   */
/* --------------------------------------------------- */

#include "SOILWAT2/generic.h"  // for `swprintf`
#include "SOILWAT2/SW_Carbon.h"  // for `calculate_CO2_multipliers`
#include "SOILWAT2/SW_Control.h"  // for `SW_CTL_read_inputs_from_disk`

#include "rSW_Files.h"
#include "rSW_Model.h"
#include "rSW_Weather.h"
#include "rSW_VegProd.h"
#include "rSW_Site.h"
#include "rSW_VegEstab.h"
#include "rSW_Output.h"
#include "rSW_Carbon.h"
#include "rSW_SoilWater.h"

#include "rSW_Control.h"

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>


/* =================================================== */
/*                  Global Declarations                */
/* --------------------------------------------------- */

extern Bool useFiles;
extern SEXP InputData;


/** Additional initialization to be performed for rSOILWAT2
    This function is meant to be run immediately after calling SW_CTL_init_model()
 */
void rSW_CTL_init_model2(void) {
  rSW_SWC_construct();
  rSW_OUT_construct();
}



void rSW_CTL_obtain_inputs(void) {
  #ifdef RSWDEBUG
  int debug = 0;
  #endif

  if (useFiles) {
    SW_CTL_read_inputs_from_disk();

  } else { //Use R data to set the data
    #ifdef RSWDEBUG
    if (debug) swprintf("'SW_CTL_obtain_inputs': Copy input from 'InputData':");
    #endif

    onSet_SW_F(GET_SLOT(InputData, install("files")));
    #ifdef RSWDEBUG
    if (debug) swprintf(" 'files'");
    #endif

    onSet_SW_MDL(GET_SLOT(InputData, install("years")));
    #ifdef RSWDEBUG
    if (debug) swprintf(" > 'model'");
    #endif

    onSet_SW_WTH(GET_SLOT(InputData, install("weather")));
    #ifdef RSWDEBUG
    if (debug) swprintf(" > 'weather'");
    #endif

    onSet_SW_VPD(GET_SLOT(InputData, install("prod")));
    #ifdef RSWDEBUG
    if (debug) swprintf(" > 'veg'");
    #endif

    onSet_SW_SIT(GET_SLOT(InputData, install("site")));
    #ifdef RSWDEBUG
    if (debug) swprintf(" > 'site'");
    #endif

    onSet_SW_VES(GET_SLOT(InputData, install("estab")));
    #ifdef RSWDEBUG
    if (debug) swprintf(" > 'establishment'");
    #endif

    onSet_SW_OUT(GET_SLOT(InputData, install("output")));
    #ifdef RSWDEBUG
    if (debug) swprintf(" > 'ouput'");
    #endif

    onSet_swCarbon(GET_SLOT(InputData, install("carbon")));
    #ifdef RSWDEBUG
    if (debug) swprintf(" > 'CO2'");
    #endif

    onSet_SW_SWC(GET_SLOT(InputData, install("swc")));
    #ifdef RSWDEBUG
    if (debug) swprintf(" > 'swc'");
    if (debug) swprintf(" completed.\n");
    #endif
  }

  calculate_CO2_multipliers();
}
