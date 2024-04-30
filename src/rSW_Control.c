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

#include "SOILWAT2/include/generic.h"  // for `swprintf`
#include "SOILWAT2/include/SW_Carbon.h"  // for `calculate_CO2_multipliers`
#include "SOILWAT2/include/SW_Control.h"  // for `SW_CTL_read_inputs_from_disk`
#include "SOILWAT2/include/SW_Files.h"
#include "SOILWAT2/include/SW_Domain.h"

#include "rSW_Files.h"
#include "rSW_Domain.h"
#include "rSW_Weather.h"
#include "rSW_Markov.h"
#include "rSW_Sky.h"
#include "rSW_VegProd.h"
#include "rSW_Site.h"
#include "rSW_VegEstab.h"
#include "rSW_Output.h"
#include "rSW_Carbon.h"
#include "rSW_SoilWater.h"

#include "rSW_Control.h"
#include "rSW_Domain.h"
#include "SW_R_lib.h" // externs `InputData`

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>


/* =================================================== */
/*             Global Function Definitions             */
/* --------------------------------------------------- */

/** Additional initialization to be performed for rSOILWAT2
    This function is meant to be run immediately after calling SW_CTL_init_model()
 */
void rSW_CTL_setup_model2(void) {
  rSW_SWC_construct();
}


// uses global variable `InputData` if not `from_files`
/** Prepare inputs for SOILWAT2

  Side effect is that SOILWAT2 structures contain input values
  (i.e., rSOILWAT2 global variable SoilWatAll).

  @param[in] from_files If TRUE, then read inputs from disk and copy into
      SoilWatAll.
  @param[in] InputData If from_files is FALSE, then copy values from
      InputData to SoilWatAll.
  @param[in] weatherList If from_files is FALSE, then copy values from
      weatherList to SoilWatAll
      (unless weatherList is NULL, then slot weatherHistory of InputData is used).
*/
void rSW_CTL_obtain_inputs(Bool from_files, SEXP InputData, SEXP weatherList, LOG_INFO* LogInfo) {
  #ifdef RSWDEBUG
  int debug = 0;
  #endif

  if (from_files) {
    SW_CTL_read_inputs_from_disk(&SoilWatAll, &SoilWatDomain.PathInfo, LogInfo);

  } else { //Use R data to set the data
    #ifdef RSWDEBUG
    if (debug) {
      swprintf(
        "\n'rSW_CTL_obtain_inputs()': "
        "Copy data from rSOILWAT2 S4 'InputData' object to SOILWAT2 variables:"
      );
    }
    #endif

    onSet_SW_F(GET_SLOT(InputData, install("files")), LogInfo);
    #ifdef RSWDEBUG
    if (debug) swprintf(" 'files'");
    #endif
    if (LogInfo->stopRun) {
        return; // Exit function prematurely due to error
    }

    onSet_SW_MDL(GET_SLOT(InputData, install("years")), LogInfo);
    #ifdef RSWDEBUG
    if (debug) swprintf(" > 'model'");
    #endif
    if (LogInfo->stopRun) {
        return; // Exit function prematurely due to error
    }

    onSet_SW_WTH_setup(GET_SLOT(InputData, install("weather")), LogInfo);
    #ifdef RSWDEBUG
    if (debug) swprintf(" > 'weather-setup'");
    #endif
    if (LogInfo->stopRun) {
        return; // Exit function prematurely due to error
    }

    onSet_SW_SKY(GET_SLOT(InputData, install("cloud")));
    #ifdef RSWDEBUG
    if (debug) swprintf(" > 'climate'");
    #endif

    if (
      LOGICAL(GET_SLOT(GET_SLOT(InputData, install("weather")), install("use_weathergenerator")))[0]
    ) {
      onSet_MKV(GET_SLOT(InputData, install("markov")), LogInfo);
      #ifdef RSWDEBUG
      if (debug) swprintf(" > 'weather generator'");
      #endif
      if (LogInfo->stopRun) {
          return; // Exit function prematurely due to error
      }
    }

    if (isNull(weatherList)) {
      weatherList = GET_SLOT(InputData, install("weatherHistory"));
    }
    onSet_WTH_DATA(weatherList, LogInfo);
    #ifdef RSWDEBUG
    if (debug) swprintf(" > 'weather-history'");
    #endif
    if (LogInfo->stopRun) {
        return; // Exit function prematurely due to error
    }

    onSet_SW_VPD(GET_SLOT(InputData, install("prod")), LogInfo);
    #ifdef RSWDEBUG
    if (debug) swprintf(" > 'veg'");
    #endif
    if (LogInfo->stopRun) {
        return; // Exit function prematurely due to error
    }

    onSet_SW_SIT(GET_SLOT(InputData, install("site")), LogInfo);
    #ifdef RSWDEBUG
    if (debug) swprintf(" > 'site'");
    #endif
    if (LogInfo->stopRun) {
        return; // Exit function prematurely due to error
    }

    onSet_SW_SOILS(GET_SLOT(InputData, install("soils")), LogInfo);
    #ifdef RSWDEBUG
    if (debug) swprintf(" > 'soils' + 'swrc parameters'");
    #endif
    if (LogInfo->stopRun) {
        return; // Exit function prematurely due to error
    }

    onSet_SW_VES(GET_SLOT(InputData, install("estab")), LogInfo);
    #ifdef RSWDEBUG
    if (debug) swprintf(" > 'establishment'");
    #endif
    if (LogInfo->stopRun) {
        return; // Exit function prematurely due to error
    }

    onSet_SW_OUT(GET_SLOT(InputData, install("output")), LogInfo);
    #ifdef RSWDEBUG
    if (debug) swprintf(" > 'ouput'");
    #endif
    if (LogInfo->stopRun) {
        return; // Exit function prematurely due to error
    }

    onSet_swCarbon(GET_SLOT(InputData, install("carbon")), LogInfo);
    #ifdef RSWDEBUG
    if (debug) swprintf(" > 'CO2'");
    #endif
    if (LogInfo->stopRun) {
        return; // Exit function prematurely due to error
    }

    onSet_SW_SWC(GET_SLOT(InputData, install("swc")), LogInfo);
    #ifdef RSWDEBUG
    if (debug) swprintf(" > 'swc'");
    if (debug) swprintf(" completed.\n");
    #endif
    if (LogInfo->stopRun) {
        return; // Exit function prematurely due to error
    }
  }
}
