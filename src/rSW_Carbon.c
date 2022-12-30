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
#include "SOILWAT2/include/SW_Model.h" // externs `SW_Model`

#include "SOILWAT2/include/SW_Carbon.h" // externs `SW_Carbon`
#include "rSW_Carbon.h"

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
  char *cCO2ppm[] = {"Year", "CO2ppm"};
  char *cSW_CARBON[] = {"CarbonUseBio", "CarbonUseWUE", "Scenario", "DeltaYear", "CO2ppm"};
  int i, year, n_sim;
  double *vCO2ppm;

  SW_CARBON *c = &SW_Carbon;

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
  INTEGER(DeltaYear)[0] = SW_Model.addtl_yr;
  SET_SLOT(object, install(cSW_CARBON[3]), DeltaYear);

  n_sim = SW_Model.endyr - SW_Model.startyr + 1;
  PROTECT(CO2ppm = allocMatrix(REALSXP, n_sim, 2));
  vCO2ppm = REAL(CO2ppm);
  for (i = 0, year = SW_Model.startyr; i < n_sim; i++, year++)
  {
    vCO2ppm[i + n_sim * 0] = year;
    vCO2ppm[i + n_sim * 1] = c->ppm[year];
  }
  PROTECT(CO2ppm_Names = allocVector(VECSXP, 2));
  PROTECT(cCO2ppm_Names = allocVector(STRSXP, 2));
  for (i = 0; i < 2; i++)
    SET_STRING_ELT(cCO2ppm_Names, i, mkChar(cCO2ppm[i]));
  SET_VECTOR_ELT(CO2ppm_Names, 1, cCO2ppm_Names);
  setAttrib(CO2ppm, R_DimNamesSymbol, CO2ppm_Names);
  SET_SLOT(object, install(cSW_CARBON[4]), CO2ppm);

  UNPROTECT(9);

  return object;
}


/**
 * @brief Populate the SW_CARBON structure with the values of swCarbon.
 *
 * Extract slots of the swCarbon class:
 *   1. CarbonUseBio - Whether or not to use the biomass multiplier.
 *   2. CarbonUseWUE - Whether or not to use the WUE multiplier.
 *   3. DeltaYear - How many years in the future we are simulating.
 *   4. Scenario - Scenario name of the CO2 concentration time series.
 *   5. CO2ppm - a vector of length 2 where the first element is the vector of
 *               years and the second element is the CO2 values.
 *
 * @param object An instance of the swCarbon class.
 */
void onSet_swCarbon(SEXP object) {
  SW_CARBON *c = &SW_Carbon;

  // Extract the slots from our object into our structure
  c->use_bio_mult = INTEGER(GET_SLOT(object, install("CarbonUseBio")))[0];
  c->use_wue_mult = INTEGER(GET_SLOT(object, install("CarbonUseWUE")))[0];
  SW_Model.addtl_yr = INTEGER(GET_SLOT(object, install("DeltaYear")))[0];  // This is needed for output 100% of the time
  strcpy(c->scenario, CHAR(STRING_ELT(GET_SLOT(object, install("Scenario")), 0)));

  // If CO2 is not being used, we can run without extracting ppm data
  if (!c->use_bio_mult && !c->use_wue_mult)
  {
    return;
  }

  // Only extract the CO2 values that will be used
  TimeInt year;
  unsigned int i, n_input, n_sim;
  #ifdef RSWDEBUG
  int debug = 0;
  #endif

  SEXP CO2ppm;
  double *values;

  year = SW_Model.startyr + SW_Model.addtl_yr; // real calendar year when simulation begins
  n_sim = SW_Model.endyr - SW_Model.startyr + 1;
  PROTECT(CO2ppm = GET_SLOT(object, install("CO2ppm")));
  n_input = nrows(CO2ppm);
  values = REAL(CO2ppm);

  // Locate index of first year for which we need CO2 data
  for (i = 1; year != (unsigned int) values[i - 1 + n_input * 0] && i < MAX_NYEAR; i++) {}

  #ifdef RSWDEBUG
  if (debug) {
    swprintf("'onSet_swCarbon': year = %d, n_sim = %d, n_input = %d, i = %d\n",
      year, n_sim, n_input, i);
  }
  #endif

  // Check that we have enough data
  if (i - 1 + n_sim > n_input)
  {
    LogError(logfp, LOGFATAL, "CO2ppm object does not contain data for every year");
  }

  // Copy CO2 concentration values to SOILWAT variable
  for (; i <= n_input && year < MAX_NYEAR; i++, year++)
  {
    c->ppm[year] = values[i - 1 + n_input * 1];  // R's index is 1-based

    #ifdef RSWDEBUG
    if (debug) {
      swprintf("ppm[year = %d] = %3.2f <-> S4[i = %d] = %3.2f\n",
        year, c->ppm[year], i, values[i - 1 + n_input * 1]);
    }
    #endif
  }

  UNPROTECT(1);
}
