/********************************************************/
/********************************************************/
/*  Source file: SW_Model.h
 *  Type: header
 *  Application: SOILWAT - soilwater dynamics simulator
 *  Purpose: Support for the Model.c routines and any others
 *           that need to reference that module.
 *
 *  History:
 *     (8/28/01) -- INITIAL CODING - cwb
 *    12/02 - IMPORTANT CHANGE - cwb
 *          refer to comments in Times.h regarding base0
 *
 *  2/14/03 - cwb - removed the days_in_month and
 *          cum_month_days arrays to common/Times.[ch]
 */
/********************************************************/
/********************************************************/

#ifndef SW_MODEL_H
#define SW_MODEL_H

#ifdef RSOILWAT
#include <R.h>
#include <Rdefines.h>
#include <Rconfig.h>
#include <Rinterface.h>
#include <Rinternals.h>
#endif
#include "Times.h"

typedef struct {
	TimeInt /* controlling dates for model run */
	startyr, /* beginning year for model run */
	endyr, /* ending year for model run */
	startstart, /* startday in start year */
	endend, /* end day in end year */
	daymid, /* mid year depends on hemisphere */
	/* current year dates */
	firstdoy, /* start day for this year */
	lastdoy, /* 366 if leapyear or endend if endyr */
	doy, week, month, year; /* current model time */
	/* however, week and month are base0 because they
	 * are used as array indices, so take care.
	 * doy and year are base1. */

	/* first day of new week/month is checked for
	 * printing and summing weekly/monthly values */
	Bool newweek, newmonth, newyear;
	Bool isnorth;

} SW_MODEL;

void SW_MDL_read(void);
void SW_MDL_construct(void);
void SW_MDL_new_year(void);
void SW_MDL_new_day(void);

#ifdef RSOILWAT
SEXP onGet_SW_MDL();
void onSet_SW_MDL(SEXP SW_MDL);
#endif

#endif
