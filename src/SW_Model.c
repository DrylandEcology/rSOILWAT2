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

 History:
 (8/28/01) -- INITIAL CODING - cwb
 12/02 - IMPORTANT CHANGE - cwb
 refer to comments in Times.h regarding base0
 2/14/03 - cwb - created the common/Times.[ch] code
 and modified this code to reflect that.
 20090826 (drs) stricmp -> strcmp
 03/12/2010	(drs) "DAYLAST_NORTH [==366]  : DAYLAST_SOUTH;" --> "m->endend = (m->isnorth) ? Time_get_lastdoy_y(m->endyr)  : DAYLAST_SOUTH;"
 2011/01/27	(drs) when 'ending day of last year' in years.in was set to 366 and 'last year' was not a leap year, SoilWat would still calculate 366 days for the last (non-leap) year
 improved code to calculate SW_Model.endend in SW_MDL_read() in case strcmp(enddyval, "end")!=0 with
 d = atoi(enddyval);
 m->endend = (d < 365) ? d : Time_get_lastdoy_y(m->endyr);
 06/27/2013	(drs)	closed open files if LogError() with LOGFATAL is called in SW_MDL_read()
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
#include "generic.h"
#include "filefuncs.h"
#include "rands.h"
#include "Times.h"

#include "SW_Defines.h"
#include "SW_Files.h"
#include "SW_Weather.h"
#include "SW_Site.h"
#include "SW_SoilWater.h"  /* for setup_new_year() */
#include "SW_Times.h"
#include "SW_Model.h"

/* =================================================== */
/*                  Global Variables                   */
/* --------------------------------------------------- */
extern SW_SITE SW_Site; /* for reset attribute */
SW_MODEL SW_Model; /* declared here, externed elsewhere */

/* =================================================== */
/*                Module-Level Variables               */
/* --------------------------------------------------- */
static char *MyFileName;

/* these are set in _new_day() */
static TimeInt _prevweek, /* check for new week */
_prevmonth, /* check for new month */
_prevyear, /* check for new year */
_notime = 0xffff; /* init value for _prev* */

/* =================================================== */
/* =================================================== */
/*             Public Function Definitions             */
/* --------------------------------------------------- */

void SW_MDL_construct(void) {
	/* =================================================== */
	/* note that an initializer that is called during
	 * execution (better called clean() or something)
	 * will need to free all allocated memory first
	 * before clearing structure.
	 *
	 */
	SW_MODEL *m = &SW_Model;

	Time_init();
	m->newweek = m->newmonth = m->newyear = FALSE;

#ifndef STEPWAT
	/* already set by user-provided seed in steppe */
	RandSeed(0);
#endif
}

void SW_MDL_read(void) {
	/* =================================================== */
	/*
	 * 1/24/02 - added code for partial start and end years
	 *
	 * 28-Aug-03 (cwb) - N-S hemisphere flag logic changed.
	 *    Can now specify first day of first year, last
	 *    day of last year or hemisphere.  Code checks
	 *    whether the first value is [NnSs] or number to
	 *    make the decision.  If the value is numeric,
	 *    the hemisphere is assumed to be N.  This method
	 *    allows some degree of flexibility on the
	 *    starting year
	 */
	SW_MODEL *m = &SW_Model;
	FILE *f;
	int y, cnt;
	TimeInt d;
	char *p, enddyval[6];
	Bool fstartdy = FALSE, fenddy = FALSE, fhemi = FALSE;

	MyFileName = SW_F_name(eModel);
	f = OpenFile(MyFileName, "r");

	/* ----- beginning year */
	if (!GetALine(f, inbuf)) {
		CloseFile(&f);
		LogError(logfp, LOGFATAL, "%s: No input.", MyFileName);
	}
	y = atoi(inbuf);
	if (y < 0) {
		CloseFile(&f);
		LogError(logfp, LOGFATAL, "%s: Negative start year (%d)", MyFileName, y);
	}
	m->startyr = yearto4digit((TimeInt) y);

	/* ----- ending year */
	if (!GetALine(f, inbuf)) {
		CloseFile(&f);
		LogError(logfp, LOGFATAL, "%s: Ending year not found.", MyFileName);
	}
	y = atoi(inbuf);
	//assert(y > 0);
	if (y < 0) {
		CloseFile(&f);
		LogError(logfp, LOGFATAL, "%s: Negative ending year (%d)", MyFileName, y);
	}
	m->endyr = yearto4digit((TimeInt) y);
	if (m->endyr < m->startyr) {
		CloseFile(&f);
		LogError(logfp, LOGFATAL, "%s: Start Year > End Year", MyFileName);
	}

	/* ----- Start checking for model time parameters */
	/*   input should be in order of startdy, enddy, hemisphere,
	 but if hemisphere occurs first, skip checking for the rest
	 and assume they're not there.
	 */
	cnt = 0;
	while (GetALine(f, inbuf)) {
		cnt++;
		if (isalpha(*inbuf) && strcmp(inbuf, "end")) { /* get hemisphere */
			m->isnorth = (toupper((int) *inbuf) == 'N');
			fhemi = TRUE;
			break;
		}//TODO: SHOULDN'T WE SKIP THIS BELOW IF ABOVE IS TRUE
		switch (cnt) {
		case 1:
			m->startstart = atoi(inbuf);
			fstartdy = TRUE;
			break;
		case 2:
			p = inbuf;
			cnt = 0;
			while (*p && cnt < 6) {
				enddyval[cnt++] = tolower((int) *(p++));
			}
			enddyval[cnt] = enddyval[5] = '\0';
			fenddy = TRUE;
			break;
		case 3:
			m->isnorth = (toupper((int) *inbuf) == 'N');
			fhemi = TRUE;
			break;
		default:
			break; /* skip any extra lines */
		}
	}

	if (!(fstartdy && fenddy && fhemi)) {
		sprintf(errstr, "\nNot found in %s:\n", MyFileName);
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
		LogError(logfp, LOGWARN, errstr);
	}

	m->startstart += ((m->isnorth) ? DAYFIRST_NORTH : DAYFIRST_SOUTH) - 1;
	if (strcmp(enddyval, "end") == 0) {
		m->endend = (m->isnorth) ? Time_get_lastdoy_y(m->endyr) : DAYLAST_SOUTH;
	} else {
		d = atoi(enddyval);
		m->endend = (d == 365) ? Time_get_lastdoy_y(m->endyr) : 365;
	}

	m->daymid = (m->isnorth) ? DAYMID_NORTH : DAYMID_SOUTH;
	CloseFile(&f);

}
#ifdef RSOILWAT
SEXP onGet_SW_MDL() {
	int i;
	SW_MODEL *m = &SW_Model;

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
	SW_MODEL *m = &SW_Model;

	SEXP StartYear;
	SEXP EndYear;
	SEXP StartStart;
	SEXP EndEnd;
	//SEXP DayMid;
	SEXP North;
	Bool fstartdy = FALSE, fenddy = FALSE, fhemi = FALSE;
	TimeInt d;
	char enddyval[6];

	MyFileName = SW_F_name(eModel);

	if (!IS_S4_OBJECT(SW_MDL)) {
		LogError(logfp, LOGFATAL, "%s: No input.", MyFileName);
	}

	PROTECT(StartYear = GET_SLOT(SW_MDL, install("StartYear")));
	if (INTEGER(StartYear)[0] < 0) {
		LogError(logfp, LOGFATAL, "%s: Negative start year (%d)", MyFileName, INTEGER(StartYear)[0]);
	}
	m->startyr = INTEGER(StartYear)[0];
	PROTECT(EndYear = GET_SLOT(SW_MDL, install("EndYear")));
	if (isNull(EndYear) || INTEGER(EndYear)[0] == NA_INTEGER) {
		LogError(logfp, LOGFATAL, "%s: Ending year not found.", MyFileName);
	}
	if (INTEGER(EndYear)[0] < 0) {
		LogError(logfp, LOGFATAL, "%s: Negative ending year (%d)", MyFileName, INTEGER(EndYear)[0]);
	}
	m->endyr = INTEGER(EndYear)[0];
	if (m->endyr < m->startyr) {
		LogError(logfp, LOGFATAL, "%s: Start Year > End Year", MyFileName);
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
		sprintf(errstr, "\nNot found in %s:\n", MyFileName);
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
		LogError(logfp, LOGWARN, errstr);
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
#endif
void SW_MDL_new_year() {
	/* =================================================== */
	/* sets up time structures and calls modules that have
	 * yearly init routines
	 *
	 * 1/24/02 - added code for partial start and end years
	 */

	SW_MODEL *m = &SW_Model;
	TimeInt year = SW_Model.year;

	_prevweek = _prevmonth = _prevyear = _notime;

	Time_new_year(year);

	m->firstdoy = (year == m->startyr) ? m->startstart : 1;
	m->lastdoy = (year == m->endyr) ? m->endend : Time_lastDOY();
}

void SW_MDL_new_day(void) {
	/* =================================================== */
	/* sets the output period elements of SW_Model
	 * based on the current day.
	 */

	SW_Model.month = doy2month(SW_Model.doy);
	SW_Model.week = doy2week(SW_Model.doy); /* more often an index */

	/* in this case, we've finished the daily loop and are about
	 * to flush the output */
	if (SW_Model.doy > SW_Model.lastdoy) {
		SW_Model.newyear = SW_Model.newmonth = SW_Model.newweek = TRUE;
		return;
	}

	if (SW_Model.month != _prevmonth) {
		SW_Model.newmonth = (_prevmonth != _notime) ? TRUE : FALSE;
		_prevmonth = SW_Model.month;
	} else
		SW_Model.newmonth = FALSE;

	/*  if (SW_Model.week != _prevweek || SW_Model.month == NoMonth) { */
	if (SW_Model.week != _prevweek) {
		SW_Model.newweek = (_prevweek != _notime) ? TRUE : FALSE;
		_prevweek = SW_Model.week;
	} else
		SW_Model.newweek = FALSE;

}
