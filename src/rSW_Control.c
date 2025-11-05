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

#include "SOILWAT2/include/generic.h"  // for `sw_printf`
#include "SOILWAT2/include/SW_Carbon.h"  // for `calculate_CO2_multipliers`
#include "SOILWAT2/include/SW_Control.h"  // for `SW_CTL_read_inputs_from_disk`
#include "SOILWAT2/include/SW_Files.h"
#include "SOILWAT2/include/SW_Domain.h"

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
  (i.e., rSOILWAT2 global variable SoilWatRun).

  @param[in] from_files If TRUE, then read inputs from disk and copy into
      SoilWatRun.
  @param[in] InputData If from_files is FALSE, then copy values from
      InputData to SoilWatRun.
  @param[in] weatherList If from_files is FALSE, then copy values from
      weatherList to SoilWatRun
      (unless weatherList is NULL, then slot weatherHistory of InputData is used).
*/
void rSW_CTL_obtain_inputs(Bool from_files, SEXP InputData, SEXP weatherList, LOG_INFO* LogInfo) {
  #ifdef RSWDEBUG
  int debug = 0;
  #endif

  if (from_files) {
     SW_CTL_read_inputs_from_disk(
        rSW2_rank,
        &SoilWatRun,
        &SoilWatDomain,
        &SoilWatDomain.hasConsistentSoilLayerDepths,
        LogInfo
    );

  } else { //Use R data to set the data
    #ifdef RSWDEBUG
    if (debug) {
      sw_printf(
        "\n'rSW_CTL_obtain_inputs()': "
        "Copy data from rSOILWAT2 S4 'InputData' object to SOILWAT2 variables:"
      );
    }
    #endif

    onSet_SW_MDL(GET_SLOT(InputData, install("years")), LogInfo);
    #ifdef RSWDEBUG
    if (debug) sw_printf(" > 'model'");
    #endif
    if (LogInfo->stopRun) {
        return; // Exit function prematurely due to error
    }

    onSet_SW_WTH_setup(GET_SLOT(InputData, install("weather")), LogInfo);
    #ifdef RSWDEBUG
    if (debug) sw_printf(" > 'weather-setup'");
    #endif
    if (LogInfo->stopRun) {
        return; // Exit function prematurely due to error
    }

    onSet_SW_SKY(GET_SLOT(InputData, install("cloud")));
    #ifdef RSWDEBUG
    if (debug) sw_printf(" > 'climate'");
    #endif

    if (
      LOGICAL(GET_SLOT(GET_SLOT(InputData, install("weather")), install("use_weathergenerator")))[0]
    ) {
      onSet_MKV(GET_SLOT(InputData, install("markov")), LogInfo);
      #ifdef RSWDEBUG
      if (debug) sw_printf(" > 'weather generator'");
      #endif
      if (LogInfo->stopRun) {
          return; // Exit function prematurely due to error
      }
    }

    onSet_SW_VPD(GET_SLOT(InputData, install("prod2")), LogInfo);
    #ifdef RSWDEBUG
    if (debug) sw_printf(" > 'veg'");
    #endif
    if (LogInfo->stopRun) {
        return; // Exit function prematurely due to error
    }

    onSet_SW_SIT(GET_SLOT(InputData, install("site")), LogInfo);
    #ifdef RSWDEBUG
    if (debug) sw_printf(" > 'site'");
    #endif
    if (LogInfo->stopRun) {
        return; // Exit function prematurely due to error
    }

    onSet_SW_SOILS(GET_SLOT(InputData, install("soils")), LogInfo);
    #ifdef RSWDEBUG
    if (debug) sw_printf(" > 'soils' + 'swrc parameters'");
    #endif
    if (LogInfo->stopRun) {
        return; // Exit function prematurely due to error
    }

    onSet_SW_SIT_transp(GET_SLOT(InputData, install("site")), LogInfo);
    #ifdef RSWDEBUG
    if (debug) sw_printf(" > 'tr-regions'");
    #endif
    if (LogInfo->stopRun) {
        return; // Exit function prematurely due to error
    }

    if (isNull(weatherList)) {
      weatherList = GET_SLOT(InputData, install("weatherHistory"));
    }
    onSet_WTH_DATA(weatherList, LogInfo);
    #ifdef RSWDEBUG
    if (debug) sw_printf(" > 'weather-history'");
    #endif
    if (LogInfo->stopRun) {
        return; // Exit function prematurely due to error
    }

    onSet_SW_VES(GET_SLOT(InputData, install("estab")), LogInfo);
    #ifdef RSWDEBUG
    if (debug) sw_printf(" > 'establishment'");
    #endif
    if (LogInfo->stopRun) {
        return; // Exit function prematurely due to error
    }

    onSet_SW_OUT(GET_SLOT(InputData, install("output")), LogInfo);
    #ifdef RSWDEBUG
    if (debug) sw_printf(" > 'ouput'");
    #endif
    if (LogInfo->stopRun) {
        return; // Exit function prematurely due to error
    }

    onSet_swCarbon(
        GET_SLOT(InputData, install("carbon")),
        SoilWatRun.ModelIn.startyr,   // set by onSet_SW_MDL()
        SoilWatRun.ModelIn.endyr,     // set by onSet_SW_MDL()
        SoilWatRun.VegProdIn.vegYear, // set by onSet_SW_VPD()
        LogInfo
    );
    #ifdef RSWDEBUG
    if (debug) sw_printf(" > 'CO2'");
    #endif
    if (LogInfo->stopRun) {
        return; // Exit function prematurely due to error
    }

    onSet_SW_SWC(GET_SLOT(InputData, install("swc")), LogInfo);
    #ifdef RSWDEBUG
    if (debug) sw_printf(" > 'swc'");
    if (debug) sw_printf(" completed.\n");
    #endif
    if (LogInfo->stopRun) {
        return; // Exit function prematurely due to error
    }
  }
}
