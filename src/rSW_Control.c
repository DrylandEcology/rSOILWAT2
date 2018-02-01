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
}



void rSW_CTL_obtain_inputs(void) {

  if (useFiles) {
    SW_CTL_read_inputs_from_disk();

  } else { //Use R data to set the data
    int debug = 0;

    if (debug) swprintf("'SW_CTL_obtain_inputs': Copy input from 'InputData':");

    onSet_SW_F(GET_SLOT(InputData, install("files")));
    if (debug) swprintf(" 'files'");

    onSet_SW_MDL(GET_SLOT(InputData, install("years")));
    if (debug) swprintf(" > 'model'");

    onSet_SW_WTH(GET_SLOT(InputData, install("weather")));
    if (debug) swprintf(" > 'weather'");

    onSet_SW_VPD(GET_SLOT(InputData, install("prod")));
    if (debug) swprintf(" > 'veg'");

    onSet_SW_SIT(GET_SLOT(InputData, install("site")));
    if (debug) swprintf(" > 'site'");

    onSet_SW_VES(GET_SLOT(InputData, install("estab")));
    if (debug) swprintf(" > 'establishment'");

    onSet_SW_OUT(GET_SLOT(InputData, install("output")));
    if (debug) swprintf(" > 'ouput'");

    onSet_swCarbon(GET_SLOT(InputData, install("carbon")));
    if (debug) swprintf(" > 'CO2'");

    onSet_SW_SWC(GET_SLOT(InputData, install("swc")));
    if (debug) swprintf(" > 'swc'");
    if (debug) swprintf(" completed.\n");
  }

  calculate_CO2_multipliers();
}
