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

#include "rSW_Model.h"
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

void onSet_SW_MDL(SEXP SW_MDL) {
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

	MyFileName = PathInfo.InFiles[eModel];

	if (!IS_S4_OBJECT(SW_MDL)) {
		LogError(&LogInfo, LOGERROR, "%s: No input.", MyFileName);
	}

	PROTECT(StartYear = GET_SLOT(SW_MDL, install("StartYear")));
	if (INTEGER(StartYear)[0] < 0) {
		LogError(&LogInfo, LOGERROR, "%s: Negative start year (%d)", MyFileName, INTEGER(StartYear)[0]);
	}
	m->startyr = INTEGER(StartYear)[0];
	PROTECT(EndYear = GET_SLOT(SW_MDL, install("EndYear")));
	if (isNull(EndYear) || INTEGER(EndYear)[0] == NA_INTEGER) {
		LogError(&LogInfo, LOGERROR, "%s: Ending year not found.", MyFileName);
	}
	if (INTEGER(EndYear)[0] < 0) {
		LogError(&LogInfo, LOGERROR, "%s: Negative ending year (%d)", MyFileName, INTEGER(EndYear)[0]);
	}
	m->endyr = INTEGER(EndYear)[0];
	if (m->endyr < m->startyr) {
		LogError(&LogInfo, LOGERROR, "%s: Start Year > End Year", MyFileName);
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
		LogError(&LogInfo, LOGWARN, errstr);
	}

	m->startstart += ((m->isnorth) ? DAYFIRST_NORTH : DAYFIRST_SOUTH) - 1;
	//if (strcmp(enddyval, "end") == 0) {
		//m->endend = (m->isnorth) ? Time_get_lastdoy_y(m->endyr) : DAYLAST_SOUTH;
	//} else {
	m->endend = (d == 365) ? Time_get_lastdoy_y(m->endyr) : 365;
	//}

	m->daymid = (m->isnorth) ? DAYMID_NORTH : DAYMID_SOUTH;

	UNPROTECT(5);
}
