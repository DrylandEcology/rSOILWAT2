/********************************************************/
/********************************************************/
/*	Source file: Model.c
 Type: module
 Application: SOILWAT - soilwater dynamics simulator
 Purpose: Read / write and otherwise manage the model's
 parameter file information.  The SOILWAT model
 parameter design makes good objects, and STEPPE
 is almost object-ready and has a similar Model
 component.  So this is where the model-level things
 are kept, such as time (so far that's all).
 */
/********************************************************/
/********************************************************/

/* =================================================== */
/*                INCLUDES / DEFINES                   */
/* --------------------------------------------------- */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stddef.h> // for size_t

#include "SOILWAT2/include/generic.h"
#include "SOILWAT2/include/filefuncs.h"
#include "SOILWAT2/include/Times.h"

#include "SOILWAT2/include/SW_Defines.h"
#include "SOILWAT2/include/SW_Times.h"
#include "SOILWAT2/include/SW_Files.h"
#include "SOILWAT2/include/SW_Model.h"
#include "SOILWAT2/include/SW_Domain.h"

#include "rSW_Files.h"
#include "rSW_Domain.h"
#include "SW_R_lib.h"

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>



/* =================================================== */
/*             Global Function Definitions             */
/* --------------------------------------------------- */

// onGet_SW_DOM() is currently empty and unused because
// rSOILWAT2 doesn't have a swDomain S4 class
// -- see instead onGet_SW_SPINUP()
SEXP onGet_SW_DOM(void) {
    SEXP swDOM = NULL;

    return swDOM ;
}

void onSet_SW_DOM(SEXP InputData, LOG_INFO* LogInfo) {
    // Maintenance note: if `sw_start()` switches from using
    // `SW_CTL_main()` to `SW_CTL_RunSimSet()`, then we would need to
    // copy here complete and correct values into `SoilWatDomain`

    // Currently not implemented in rSOILWAT2 but required in SOILWAT2
    strcpy(SoilWatDomain.DomainType, "s");
    SoilWatDomain.nDimX = 1;
    SoilWatDomain.nDimY = 1;
    SoilWatDomain.nDimS = 1;
    // Currently not implemented in rSOILWAT2 and not utilized in SOILWAT2
    // SoilWatDomain.startyr =
    // SoilWatDomain.endyr =
    // SoilWatDomain.startstart =
    // SoilWatDomain.endend =
    // SoilWatDomain.crs_bbox =
    // SoilWatDomain.min_x =
    // SoilWatDomain.min_y =
    // SoilWatDomain.max_x =
    // SoilWatDomain.max_y =

    // Spinup
    onSet_SW_SPINUP(GET_SLOT(InputData, install("spinup")), LogInfo);
}


SEXP onGet_SW_SPINUP(void) {
	SW_DOMAIN *d = &SoilWatDomain;

	SEXP swSpinup;
	SEXP SW_SPINUP;
	SEXP SpinupMode;
	SEXP SpinupScope;
	SEXP SpinupDuration;
	SEXP SpinupSeed;
	SEXP SpinupActive;

	char *cSW_DOM_names[] = { "SpinupMode", "SpinupScope", "SpinupDuration",
							"SpinupSeed", "SpinupActive" };

	PROTECT(swSpinup = MAKE_CLASS("swSpinup"));
	PROTECT(SW_SPINUP = NEW_OBJECT(swSpinup));

	PROTECT(SpinupMode = NEW_INTEGER(1));
	INTEGER_POINTER(SpinupMode)[0] = d->SW_SpinUp.mode;
	PROTECT(SpinupScope = NEW_INTEGER(1));
	INTEGER_POINTER(SpinupScope)[0] = d->SW_SpinUp.scope;
	PROTECT(SpinupDuration = NEW_INTEGER(1));
	INTEGER_POINTER(SpinupDuration)[0] = d->SW_SpinUp.duration;
	PROTECT(SpinupSeed = NEW_INTEGER(1));
	INTEGER_POINTER(SpinupSeed)[0] = d->SW_SpinUp.rng_seed;
	PROTECT(SpinupActive = NEW_LOGICAL(1));
	LOGICAL_POINTER(SpinupActive)[0] = d->SW_SpinUp.spinup;

	// attaching main's elements
	SET_SLOT(SW_SPINUP, install(cSW_DOM_names[0]), SpinupMode);
	SET_SLOT(SW_SPINUP, install(cSW_DOM_names[1]), SpinupScope);
	SET_SLOT(SW_SPINUP, install(cSW_DOM_names[2]), SpinupDuration);
	SET_SLOT(SW_SPINUP, install(cSW_DOM_names[3]), SpinupSeed);
	SET_SLOT(SW_SPINUP, install(cSW_DOM_names[4]), SpinupActive);

	UNPROTECT(7);
	return SW_SPINUP;
}

SEXP onGet_SW_MDL(void) {
	SW_MODEL_INPUTS *m = &SoilWatRun.ModelIn;
	SW_MODEL_RUN_INPUTS *mr = &SoilWatRun.RunIn.ModelRunIn;

	SEXP swYears;
	SEXP SW_MDL;//, SW_MDL_names;
	SEXP StartYear;
	SEXP EndYear;
	SEXP StartStart;
	SEXP EndEnd;
	//SEXP DayMid;
	SEXP North;
	char *cSW_MDL_names[] = { "StartYear", "EndYear", "FDOFY", "EDOEY", "isNorth" };//"daymid"

	PROTECT(swYears = MAKE_CLASS("swYears"));
	PROTECT(SW_MDL = NEW_OBJECT(swYears));

	PROTECT(StartYear = NEW_INTEGER(1));
	INTEGER_POINTER(StartYear)[0] = m->startyr;
	PROTECT(EndYear = NEW_INTEGER(1));
	INTEGER_POINTER(EndYear)[0] = m->endyr;
	PROTECT(StartStart = NEW_INTEGER(1));
	INTEGER_POINTER(StartStart)[0] = m->startstart;
	PROTECT(EndEnd = NEW_INTEGER(1));
	INTEGER_POINTER(EndEnd)[0] = m->endend;
	//PROTECT(DayMid = NEW_INTEGER(1));
	//INTEGER_POINTER(DayMid)[0] = m->daymid;
	PROTECT(North = NEW_LOGICAL(1));
	LOGICAL_POINTER(North)[0] = mr->isnorth;

	// attaching main's elements
	SET_SLOT(SW_MDL, install(cSW_MDL_names[0]), StartYear);
	SET_SLOT(SW_MDL, install(cSW_MDL_names[1]), EndYear);
	SET_SLOT(SW_MDL, install(cSW_MDL_names[2]), StartStart);
	SET_SLOT(SW_MDL, install(cSW_MDL_names[3]), EndEnd);
	SET_SLOT(SW_MDL, install(cSW_MDL_names[4]), North);

	UNPROTECT(7);
	return SW_MDL;
}



void onSet_SW_SPINUP(SEXP SW_DOM, LOG_INFO* LogInfo) {
    SW_DOMAIN *d = &SoilWatDomain;

    SEXP SpinupMode;
    SEXP SpinupScope;
    SEXP SpinupDuration;
    SEXP SpinupSeed;
    SEXP SpinupActive;

    if (!Rf_isS4(SW_DOM)) {
      LogError(LogInfo, LOGERROR, "onSet_SW_SPINUP: No input.");
          return; // Exit function prematurely due to error
    }

    PROTECT(SpinupMode = GET_SLOT(SW_DOM, install("SpinupMode")));
    if (INTEGER(SpinupMode)[0] != 1 && INTEGER(SpinupMode)[0] != 2) {
        LogError(
            LogInfo,
            LOGERROR,
            "onSet_SW_SPINUP: Invalid Spinup mode (%d). Please select \"1\" or \"2\"",
            INTEGER(SpinupMode)[0]
        );

        UNPROTECT(1);
        return; // Exit function prematurely due to error
    }
    d->SW_SpinUp.mode = INTEGER(SpinupMode)[0];

    PROTECT(SpinupScope = GET_SLOT(SW_DOM, install("SpinupScope")));
    if ( INTEGER(SpinupScope)[0] < 1) {
          LogError(
              LogInfo,
              LOGERROR,
              "onSet_SW_SPINUP: Spinup scope (%d) is less than 1.",
              INTEGER(SpinupScope)[0]
          );

          UNPROTECT(2);
          return; // Exit function prematurely due to error
    }
    d->SW_SpinUp.scope = INTEGER(SpinupScope)[0];

    PROTECT(SpinupDuration = GET_SLOT(SW_DOM, install("SpinupDuration")));
    if (INTEGER(SpinupDuration)[0] < 0 ) {
        LogError(
            LogInfo,
            LOGERROR,
            "onSet_SW_SPINUP: Negative spinup duration (%d)",
            INTEGER(SpinupDuration)[0]
        );

        UNPROTECT(3);
        return; // Exit function prematurely due to error
    }
    d->SW_SpinUp.duration = INTEGER(SpinupDuration)[0];

    PROTECT(SpinupSeed = GET_SLOT(SW_DOM, install("SpinupSeed")));
    d->SW_SpinUp.rng_seed = INTEGER(SpinupSeed)[0];

    PROTECT(SpinupActive = GET_SLOT(SW_DOM, install("SpinupActive")));
    d->SW_SpinUp.spinup = (Bool)LOGICAL(SpinupActive)[0];

    if (d->SW_SpinUp.duration == 0) {
      d->SW_SpinUp.spinup = FALSE;
    }
    else {
      d->SW_SpinUp.spinup = TRUE;
    }

    UNPROTECT(5);
}

void onSet_SW_MDL(SEXP SW_MDL, LOG_INFO* LogInfo) {
	SW_MODEL_INPUTS *m = &SoilWatRun.ModelIn;
	SW_MODEL_RUN_INPUTS *mr = &SoilWatRun.RunIn.ModelRunIn;

	SEXP StartYear;
	SEXP EndYear;
	SEXP StartStart;
	SEXP EndEnd;
	//SEXP DayMid;
	SEXP North;
	Bool fstartdy = FALSE, fenddy = FALSE, fhemi = FALSE;
	TimeInt d;
	char enddyval[6], errstr[MAX_ERROR];

	if (!Rf_isS4(SW_MDL)) {
		LogError(LogInfo, LOGERROR, "modelrun.in: missing input.");
        return; // Exit function prematurely due to error
	}

	PROTECT(StartYear = GET_SLOT(SW_MDL, install("StartYear")));
	if (INTEGER(StartYear)[0] < 0) {
		LogError(LogInfo, LOGERROR, "Negative start year (%d)", INTEGER(StartYear)[0]);

        UNPROTECT(1);
        return; // Exit function prematurely due to error
	}
	m->startyr = INTEGER(StartYear)[0];
	PROTECT(EndYear = GET_SLOT(SW_MDL, install("EndYear")));
	if (isNull(EndYear) || INTEGER(EndYear)[0] == NA_INTEGER) {
		LogError(LogInfo, LOGERROR, "Ending year not found.");

        UNPROTECT(2);
        return; // Exit function prematurely due to error
	}
	if (INTEGER(EndYear)[0] < 0) {
		LogError(LogInfo, LOGERROR, "Negative ending year (%d)", INTEGER(EndYear)[0]);

        UNPROTECT(2);
        return; // Exit function prematurely due to error
	}
	m->endyr = INTEGER(EndYear)[0];
	if (m->endyr < m->startyr) {
		LogError(LogInfo, LOGERROR, "Start Year > End Year");

        UNPROTECT(2);
        return; // Exit function prematurely due to error
	}

	PROTECT(StartStart = GET_SLOT(SW_MDL, install("FDOFY")));
	m->startstart = INTEGER(StartStart)[0];
	fstartdy = TRUE;
	PROTECT(EndEnd = GET_SLOT(SW_MDL, install("EDOEY")));

	d = INTEGER(EndEnd)[0];
	fenddy = TRUE;

	//PROTECT(DayMid = VECTOR_ELT(SW_MDL, 4));
	//m->daymid = INTEGER(DayMid)[0];
	PROTECT(North = GET_SLOT(SW_MDL, install("isNorth")));
	mr->isnorth = (Bool)LOGICAL(North)[0];
	fhemi = TRUE;

	if (!(fstartdy && fenddy && fhemi)) {
		snprintf(errstr, MAX_ERROR, "\nNot found in inputs:\n");
		if (!fstartdy) {
			strcat(errstr, "\tStart Day  - using 1\n");
			m->startstart = 1;
		}
		if (!fenddy) {
			strcat(errstr, "\tEnd Day    - using \"end\"\n");
			strcpy(enddyval, "end");
		}
		if (!fhemi) {
			strcat(errstr, "\tHemisphere - using \"N\"\n");
			mr->isnorth = TRUE;
		}
		strcat(errstr, "Continuing.\n");
		LogError(LogInfo, LOGWARN, errstr);
	}

	m->startstart += ((mr->isnorth) ? DAYFIRST_NORTH : DAYFIRST_SOUTH) - 1;
	//if (strcmp(enddyval, "end") == 0) {
		//m->endend = (m->isnorth) ? Time_get_lastdoy_y(m->endyr) : DAYLAST_SOUTH;
	//} else {
	m->endend = (d == 365) ? Time_get_lastdoy_y(m->endyr) : 365;
	//}

	// m->daymid = (m->isnorth) ? DAYMID_NORTH : DAYMID_SOUTH;

	UNPROTECT(5);
}


// /**
//   @brief Copy weather setup from `rSOILWAT2` S4 `swDomain`
//     to `SOILWAT2` `SW_DOMAIN`

//   Called by `rSW_CTL_obtain_inputs()` if `from_files` is `FALSE`.
// */
// void onSet_SW_DOM_setup(SEXP SW_DOM, LOG_INFO* LogInfo) {


//   // Equivalent to `SW_DOM_read()`:
//   // fill `SOILWAT2` with values from `rSOILWAT2`
//   rSW2_setDomain(weatherList, SoilWatRun.Weather.allHist, LogInfo);
// }


// // Equivalent to `SW_CTL_setup_domain()`:
// // fill `SOILWAT2` with values from `rSOILWAT2` or read from files
void rSW_CTL_setup_domain(
    Bool from_files,
    SEXP InputData,
    size_t userSUID,
    SW_DOMAIN* SW_Domain,
    LOG_INFO* LogInfo
) {
  #ifdef RSWDEBUG
  int debug = 0;
  #endif

    SW_F_construct(&SW_Domain->SW_PathInputs);

    if (from_files) {
        #ifdef RSWDEBUG
        if (debug) {
          sw_printf(
            "\n'rSW_CTL_setup_domain()': "
            "Use SOILWAT2 code to read values from disk:"
          );
        }
        #endif

        SW_F_read(&SW_Domain->SW_PathInputs, LogInfo);
        if(LogInfo->stopRun) {
           return; // Exit function prematurely due to error
        }

        SW_DOM_read(SW_Domain, LogInfo);
        if(LogInfo->stopRun) {
           return;  // Exit function prematurely due to error
        }

    } else {
        #ifdef RSWDEBUG
        if (debug) {
          sw_printf(
            "\n'rSW_CTL_setup_domain()': "
            "Copy data from rSOILWAT2 S4 'InputData' object to SOILWAT2 variables:"
          );
        }
        #endif

        onSet_SW_F(GET_SLOT(InputData, install("files")), LogInfo);
        #ifdef RSWDEBUG
        if (debug) sw_printf(" > 'files'");
        #endif
        if (LogInfo->stopRun) {
            return; // Exit function prematurely due to error
        }

        onSet_SW_DOM(InputData, LogInfo);
        #ifdef RSWDEBUG
        if (debug) sw_printf(" > 'domain'");
        #endif
        if (LogInfo->stopRun) {
            return; // Exit function prematurely due to error
        }
    }

    SW_DOM_construct(SW_Domain->SW_SpinUp.rng_seed, SW_Domain);

    SW_DOM_calc_nSUIDs(SW_Domain);

    SW_DOM_CreateProgress(SW_Domain, LogInfo);
    if(LogInfo->stopRun) {
       return;  // Exit function prematurely due to error
    }

    SW_DOM_SimSet(SW_Domain, userSUID, LogInfo);

    #ifdef RSWDEBUG
    if (debug) sw_printf(" completed.\n");
    #endif
}
