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

#include "SOILWAT2/include/generic.h"
#include "SOILWAT2/include/filefuncs.h"
#include "SOILWAT2/include/Times.h"

#include "SOILWAT2/include/SW_Defines.h"
#include "SOILWAT2/include/SW_Times.h"
#include "SOILWAT2/include/SW_Files.h"
#include "SOILWAT2/include/SW_Model.h"
#include "SOILWAT2/include/SW_Domain.h"

#include "rSW_Domain.h"
#include "SW_R_lib.h"

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>



/* =================================================== */
/*                  Local Variables                    */
/* --------------------------------------------------- */
static char *MyFileName;

/* =================================================== */
/*             Global Function Definitions             */
/* --------------------------------------------------- */

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
	SW_MODEL *m = &SoilWatAll.Model;

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
	LOGICAL_POINTER(North)[0] = m->isnorth;

	// attaching main's elements
	SET_SLOT(SW_MDL, install(cSW_MDL_names[0]), StartYear);
	SET_SLOT(SW_MDL, install(cSW_MDL_names[1]), EndYear);
	SET_SLOT(SW_MDL, install(cSW_MDL_names[2]), StartStart);
	SET_SLOT(SW_MDL, install(cSW_MDL_names[3]), EndEnd);
	SET_SLOT(SW_MDL, install(cSW_MDL_names[4]), North);

	UNPROTECT(7);
	return SW_MDL;
}

/**
   @brief Copy domain setup from `SOILWAT2` `SW_DOMAIN`
   to `rSOILWAT2` S4 `swDomain`

   Called by `onGetInputDataFromFiles()`
*/
void onGet_SW_DOM_setup(void) {


}

void onSet_SW_SPINUP(SEXP SW_DOM, LOG_INFO* LogInfo) {
	SW_DOMAIN *d = &SoilWatDomain;
	SW_MODEL *m = &SoilWatAll.Model;

	SEXP SpinupMode;
	SEXP SpinupScope;
	SEXP SpinupDuration;
	SEXP SpinupSeed;
	SEXP SpinupActive;

	// Bool fstartdy = FALSE, fenddy = FALSE, fhemi = FALSE;
	TimeInt range;
	// char enddyval[6], errstr[MAX_ERROR];

	MyFileName = SoilWatDomain.PathInfo.InFiles[eDomain];
	range = m->endyr - m->startyr;

	if (!IS_S4_OBJECT(SW_DOM)) {
		LogError(LogInfo, LOGERROR, "%s: No input.", MyFileName);
        return; // Exit function prematurely due to error
	}

	PROTECT(SpinupMode = GET_SLOT(SW_DOM, install("SpinupMode")));
	if (INTEGER(SpinupMode)[0] != 1 && INTEGER(SpinupMode)[0] != 2) {
		LogError(LogInfo, LOGERROR, "%s: Invalid Spinup mode (%d). Please select \"1\" or \"2\"",
		MyFileName, INTEGER(SpinupMode)[0]);

        UNPROTECT(1);
        return; // Exit function prematurely due to error
	}
	d->SW_SpinUp.mode = INTEGER(SpinupMode)[0];

	PROTECT(SpinupScope = GET_SLOT(SW_DOM, install("SpinupScope")));
	if ( INTEGER(SpinupScope)[0] < 1 || INTEGER(SpinupScope)[0] > range) {
		LogError(LogInfo, LOGERROR, "%s: Spinup scope (%d) out of range: (%d)", MyFileName,
		INTEGER(SpinupScope)[0], range);

        UNPROTECT(2);
        return; // Exit function prematurely due to error
	}
	d->SW_SpinUp.scope = INTEGER(SpinupScope)[0];

	PROTECT(SpinupDuration = GET_SLOT(SW_DOM, install("SpinupDuration")));
	if (INTEGER(SpinupDuration)[0] < 0 ) {
		LogError(LogInfo, LOGERROR, "%s: Negative spinup duration (%d)", MyFileName, INTEGER(SpinupDuration)[0]);

		UNPROTECT(3);
		return; // Exit function prematurely due to error		
	}
	d->SW_SpinUp.duration = INTEGER(SpinupDuration)[0];

	PROTECT(SpinupSeed = GET_SLOT(SW_DOM, install("SpinupSeed")));
	d->SW_SpinUp.rng_seed = REAL(SpinupSeed)[0];

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
	SW_MODEL *m = &SoilWatAll.Model;

	SEXP StartYear;
	SEXP EndYear;
	SEXP StartStart;
	SEXP EndEnd;
	//SEXP DayMid;
	SEXP North;
	Bool fstartdy = FALSE, fenddy = FALSE, fhemi = FALSE;
	TimeInt d;
	char enddyval[6], errstr[MAX_ERROR];

	MyFileName = SoilWatDomain.PathInfo.InFiles[eModel];

	if (!IS_S4_OBJECT(SW_MDL)) {
		LogError(LogInfo, LOGERROR, "%s: No input.", MyFileName);
        return; // Exit function prematurely due to error
	}

	PROTECT(StartYear = GET_SLOT(SW_MDL, install("StartYear")));
	if (INTEGER(StartYear)[0] < 0) {
		LogError(LogInfo, LOGERROR, "%s: Negative start year (%d)", MyFileName, INTEGER(StartYear)[0]);

        UNPROTECT(1);
        return; // Exit function prematurely due to error
	}
	m->startyr = INTEGER(StartYear)[0];
	PROTECT(EndYear = GET_SLOT(SW_MDL, install("EndYear")));
	if (isNull(EndYear) || INTEGER(EndYear)[0] == NA_INTEGER) {
		LogError(LogInfo, LOGERROR, "%s: Ending year not found.", MyFileName);

        UNPROTECT(2);
        return; // Exit function prematurely due to error
	}
	if (INTEGER(EndYear)[0] < 0) {
		LogError(LogInfo, LOGERROR, "%s: Negative ending year (%d)", MyFileName, INTEGER(EndYear)[0]);

        UNPROTECT(2);
        return; // Exit function prematurely due to error
	}
	m->endyr = INTEGER(EndYear)[0];
	if (m->endyr < m->startyr) {
		LogError(LogInfo, LOGERROR, "%s: Start Year > End Year", MyFileName);

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
	m->isnorth = (Bool)LOGICAL(North)[0];
	fhemi = TRUE;

	if (!(fstartdy && fenddy && fhemi)) {
		snprintf(errstr, MAX_ERROR, "\nNot found in %s:\n", MyFileName);
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
			m->isnorth = TRUE;
		}
		strcat(errstr, "Continuing.\n");
		LogError(LogInfo, LOGWARN, errstr);
	}

	m->startstart += ((m->isnorth) ? DAYFIRST_NORTH : DAYFIRST_SOUTH) - 1;
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
//   rSW2_setDomain(weatherList, SoilWatAll.Weather.allHist, LogInfo);
// }


// // Equivalent to `SW_DOM_read()`:
// // fill `SOILWAT2` with values from `rSOILWAT2`
// static void rSW2_setupDomain(SEXP listAllW, SW_DOMAIN **SW_Domain, LOG_INFO* LogInfo) {
//     unsigned int yearIndex, year;

//     SW_F_construct(
//         SW_Domain->PathInfo.InFiles[eFirst],
//         SW_Domain->PathInfo._ProjDir,
//         LogInfo
//     );
//     if(LogInfo->stopRun) {
//         return; // Exit function prematurely due to error
//     }
//     swprintf("'SW_Control.c: SW_CTL_setup_domain': check 1\n");

//     SW_F_read(&SW_Domain->PathInfo, LogInfo);
//     if(LogInfo->stopRun) {
//         return; // Exit function prematurely due to error
//     }

//     swprintf("'SW_Control.c: SW_CTL_setup_domain': check 2\n");

//     #if defined(SWNETCDF)
//     SW_NC_read(&SW_Domain->netCDFInfo, &SW_Domain->PathInfo, LogInfo);
//     if(LogInfo->stopRun) {
//         return; // Exit function prematurely due to error
//     }
//     #endif

//     SW_DOM_read(SW_Domain, LogInfo);
//     if(LogInfo->stopRun) {
//         return; // Exit function prematurely due to error
//     }

//     SW_DOM_construct(SW_Domain->SW_SpinUp.rng_seed, SW_Domain);

//     SW_DOM_calc_nSUIDs(SW_Domain);

//     #if defined(SWNETCDF)
//     // Create domain template if it does not exist (and exit)
//     if(!FileExists(SW_Domain->netCDFInfo.InFilesNC[vNCdom])) {
//         SW_NC_create_domain_template(SW_Domain, LogInfo);
//         if(LogInfo->stopRun) {
//             return; // Exit prematurely due to error
//         }

//         LogError(LogInfo, LOGERROR, "Domain netCDF template has been created. "
//                                     "Please modify it and rename it to "
//                                     "'domain.nc' when done and try again. "
//                                     "The template path is: %s",
//                                     DOMAIN_TEMP);
//         return; // Exit prematurely so the user can modify the domain template
//     }

//     // Open necessary netCDF input files and check for consistency with domain
//     SW_NC_open_dom_prog_files(&SW_Domain->netCDFInfo, LogInfo);
//     if(LogInfo->stopRun) {
//         return; // Exit function prematurely due to error
//     }

//     SW_NC_check(SW_Domain, SW_Domain->netCDFInfo.ncFileIDs[vNCdom],
//                 SW_Domain->netCDFInfo.InFilesNC[vNCdom], LogInfo);
//     if(LogInfo->stopRun) {
//         return; // Exit function prematurely due to error
//     }
//     #endif

//     SW_DOM_CreateProgress(SW_Domain, LogInfo);
//     if(LogInfo->stopRun) {
//         return; // Exit function prematurely due to error
//     }

//     SW_DOM_SimSet(SW_Domain, userSUID, LogInfo);
// }
