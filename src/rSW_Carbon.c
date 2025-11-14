/**
 * @file   SW_Carbon.c
 * @author Zachary Kramer
 * @brief  Contains functions, constants, and variables that deal with the effect of CO2 on transpiration and biomass.
 *
 * Atmospheric carbon dioxide has been observed to affect water-use efficiency
 * and biomass, which is what this code attempts to simulate. The effects can
 * be varied by plant functional type. Most usages of the functions here are
 * in @f SW_VegProd.c and @f SW_Flow_lib.c.
 *
 * @date   7 February 2017
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#include "SOILWAT2/include/generic.h"
#include "SOILWAT2/include/filefuncs.h"
#include "SOILWAT2/include/SW_Defines.h"
#include "SOILWAT2/include/SW_Model.h"

#include "SOILWAT2/include/SW_Carbon.h"
#include "rSW_Carbon.h"
#include "SW_R_lib.h"

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>


/* =================================================== */
/*             Global Function Definitions             */
/* --------------------------------------------------- */



/* A description on how these 'onGet' and 'onSet' functions work...
 * Summary: onGet instantiates the S4 'swCarbon' class and copies the values of the C variable 'SW_Carbon' into a R variable of the S4 'swCarbon' class.
 *          onSet copies the values of the R variable (of the S4 'swCarbon' class) into the C variable 'SW_Carbon'
 * 1) An S4 class is described and generated in rSOILWAT2/R
 * 2) This class needs to be instantiatied, which is done here
 * 3) The object that gets returned here eventually gets inserted into swRunScenariosData
 * 4) Data of the object is then modified with class functions in R (e.g. rSOILWAT2::swCarbon_Scenario(swRUnScenariosData[[1]]) <- "RCP85")
 * 5) The 'onSet' function is used to extract the latest data of the object (e.g. when SOILWAT2 begins modeling the real years)
 */


/**
 * @brief Instantiate the 'swCarbon' class and copies the values of the C variable
 * 'SW_Carbon' into a R variable of the S4 'swCarbon' class.
 * @return An instance of the swCarbon class.
 */
SEXP onGet_SW_CARBON(void) {
  // Create access variables
  SEXP class, object,
    CarbonUseBio, CarbonUseWUE, Scenario, DeltaYear, CO2ppm, CO2ppm_Names,
    cCO2ppm_Names;
  SEXP CO2ppmVegRef;
  char *cCO2ppm[] = {"Year", "CO2ppm"};
  char *cSW_CARBON[] = {"CarbonUseBio", "CarbonUseWUE", "Scenario", "DeltaYear", "CO2ppm", "CO2ppmVegRef"};
  int i, year;
  unsigned int n_sim = SoilWatRun.ModelIn.endyr - SoilWatRun.ModelIn.startyr + 1;
  double *vCO2ppm;

  SW_CARBON_INPUTS *c = &SoilWatRun.CarbonIn;

  // Grab our S4 carbon class as an object
  PROTECT(class  = MAKE_CLASS("swCarbon"));
  PROTECT(object = NEW_OBJECT(class));

  // Copy values from C object 'SW_Carbon' into new S4 object
  PROTECT(CarbonUseBio = NEW_INTEGER(1));
  INTEGER(CarbonUseBio)[0] = c->use_bio_mult;
  SET_SLOT(object, install(cSW_CARBON[0]), CarbonUseBio);

  PROTECT(CarbonUseWUE = NEW_INTEGER(1));
  INTEGER(CarbonUseWUE)[0] = c->use_wue_mult;
  SET_SLOT(object, install(cSW_CARBON[1]), CarbonUseWUE);

  PROTECT(Scenario = NEW_STRING(1));
  SET_STRING_ELT(Scenario, 0, mkChar(c->scenario));
  SET_SLOT(object, install(cSW_CARBON[2]), Scenario);

  PROTECT(DeltaYear = NEW_INTEGER(1));
  INTEGER(DeltaYear)[0] = 0; // addtl_yr was removed with SOILWAT2 v8.3.0
  SET_SLOT(object, install(cSW_CARBON[3]), DeltaYear);

  PROTECT(CO2ppm = allocMatrix(REALSXP, n_sim, 2));
  vCO2ppm = REAL(CO2ppm);
  for (i = 0, year = SoilWatRun.ModelIn.startyr; i < n_sim; i++, year++)
  {
    vCO2ppm[i + n_sim * 0] = year;
    vCO2ppm[i + n_sim * 1] = c->ppm[i];
  }
  PROTECT(CO2ppm_Names = allocVector(VECSXP, 2));
  PROTECT(cCO2ppm_Names = allocVector(STRSXP, 2));
  for (i = 0; i < 2; i++) {
    SET_STRING_ELT(cCO2ppm_Names, i, mkChar(cCO2ppm[i]));
  }

  SET_VECTOR_ELT(CO2ppm_Names, 1, cCO2ppm_Names);
  setAttrib(CO2ppm, R_DimNamesSymbol, CO2ppm_Names);
  SET_SLOT(object, install(cSW_CARBON[4]), CO2ppm);

  PROTECT(CO2ppmVegRef = NEW_NUMERIC(1));
  REAL(CO2ppmVegRef)[0] = c->ppmVegRef;
  SET_SLOT(object, install(cSW_CARBON[5]), CO2ppmVegRef);

  UNPROTECT(10);

  return object;
}


/**
 * @brief Populate the SW_CARBON structure with the values of swCarbon.
 *
 * Extract slots of the swCarbon class:
 *   1. CarbonUseBio - Whether or not to use the biomass multiplier.
 *   2. CarbonUseWUE - Whether or not to use the WUE multiplier.
 *   3. DeltaYear - How many years in the future we are simulating.
 *      (Unused since v6.5.0)
 *   4. Scenario - Scenario name of the CO2 concentration time series.
 *   5. CO2ppm - a vector of length 2 where the first element is the vector of
 *               years and the second element is the CO2 values.
 *   6. ppmVegRef
 *
 * @param object An instance of the swCarbon class.
 */
void onSet_swCarbon(
    SEXP object,
    TimeInt startYr,
    TimeInt endYr,
    TimeInt vegYear,
    LOG_INFO* LogInfo
) {
    SW_CARBON_INPUTS *c = &SoilWatRun.CarbonIn;

    // Extract the slots from our object into our structure
    c->use_bio_mult = INTEGER(GET_SLOT(object, install("CarbonUseBio")))[0];
    c->use_wue_mult = INTEGER(GET_SLOT(object, install("CarbonUseWUE")))[0];
    strcpy(c->scenario, CHAR(STRING_ELT(GET_SLOT(object, install("Scenario")), 0)));

    // If CO2 is not being used, we can run without extracting ppm data
    if (!c->use_bio_mult && !c->use_wue_mult) {
        return;
    }

    // addtl_yr was removed with SOILWAT2 v8.3.0
    int addtl_yr = INTEGER(GET_SLOT(object, install("DeltaYear")))[0];
    if (!R_FINITE(addtl_yr) || addtl_yr != 0) {
        LogError(
            LogInfo,
            LOGERROR,
            "'DeltaYear' for aCO2 is defunct since SOILWAT2 v8.3.0 (value = %d)",
            addtl_yr
        );
        goto cleanMem; // Exit function prematurely due to error
    }

    // Only extract the CO2 values that will be used
    SEXP CO2ppm;
    double *vCO2ppm;
    TimeInt idxrSW;
    TimeInt idxSW = 0;
    TimeInt year;
    TimeInt nSW = endYr - startYr + 1;
    TimeInt nrSW;

    #ifdef RSWDEBUG
    int debug = 0;
    #endif

    c->ppmVegRef = REAL(GET_SLOT(object, install("CO2ppmVegRef")))[0];

    PROTECT(CO2ppm = GET_SLOT(object, install("CO2ppm")));
    nrSW = nrows(CO2ppm);
    vCO2ppm = REAL(CO2ppm);

    // Check that we have enough data
    if (nSW > nrSW) {
        LogError(
            LogInfo,
            LOGERROR,
            "CO2ppm object does not contain data for every year"
        );
        goto cleanMem; // Exit function prematurely due to error
    }

    // Allocate SOILWAT2 memory
    SW_CBN_deconstruct(&SoilWatRun.CarbonIn);
    SW_CBN_alloc_ppm(nSW, &SoilWatRun.CarbonIn.ppm, LogInfo);
    if (LogInfo->stopRun) {
        goto cleanMem; // Exit function prematurely due to error
    }

    // Loop through the rSOILWAT2 data and copy the values that will be used
    for (idxrSW = 0; idxrSW < nrSW; idxrSW++) {
        // Should check for overflow when coercing long int to TimeInt
        year = (TimeInt) lround(vCO2ppm[idxrSW + nrSW * 0]);

        /* Look for requested years */
        if (!((year >= startYr && year <= endYr) || year == vegYear)) {
            continue; // We aren't using this year
        }

        /* Update aCO2 for vegetation reference year if data available */
        if (year == vegYear) {
            c->ppmVegRef = vCO2ppm[idxrSW + nrSW * 1];
        }

        /* Look for sequence of simulation years */
        if ((year < startYr) || (year > endYr)) {
            continue; // We aren't using this year
        }

        if (year != startYr + idxSW) {
            LogError(
                LogInfo,
                LOGERROR,
                "Unexpected sequence of years in CO2ppm object"
            );
            goto cleanMem; // Exit function prematurely due to error
        }

        c->ppm[idxSW] = vCO2ppm[idxrSW + nrSW * 1];
        idxSW++;
    }

    // Check that we extracted the correct number of values
    if (nSW != idxSW) {
        LogError(
            LogInfo,
            LOGERROR,
            "Required %d years but found %d years of data in CO2ppm object",
            nSW,
            idxSW
        );
        goto cleanMem; // Exit function prematurely due to error
    }

cleanMem:
    UNPROTECT(1);
}
