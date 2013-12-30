/********************************************************/
/********************************************************/
/*  Source file: Times.h
 *  Type: header
 *
 *  Purpose: Provides a consistent definition for the
 *           time values that might be needed by various
 *           objects.
 *  History:
 *    9/11/01 -- INITIAL CODING - cwb  Originally coded to
 *          be used in SOILWAT.
 *
 *    12/02 -- IMPORTANT CHANGE -- cwb
 *          After modifying the code to integrate with STEPPE
 *          I found that the old system of starting most
 *          arrays from 1 (aka base1) was causing more problems
 *          than it was worth, so I modified all the arrays and
 *          times and other indices to start from 0 (aka base0).
 *          The basic logic is that the user will see base1
 *          numbers while the code will use base0.  Unfortunately
 *          this will require careful attention to the points
 *          at which the conversions must be made, eg, month
 *          indices read from a file will be base1 as will
 *          such times that appear in the output.
 *
 * 2/14/03 - cwb - the features of this code appear to be
 *          rather generally useful, so I took out the
 *          soilwat specific stuff and made a general
 *          codeset.
 *
 * 2/24/03 - cwb - Moved Doy2Month to a function in Times.c
 *
 *    19-Sep-03 (cwb) Imported a bunch of new routines
 *       and added the facility for model time.
 *	  09/26/2011	(drs)	added function interpolate_monthlyValues()
 */
/********************************************************/
/********************************************************/

#ifndef TIMES_H
#define TIMES_H

#include <time.h>
#include "generic.h"

/*---------------------------------------------------------------*/
#define MAX_MONTHS 12
#define MAX_WEEKS 53
#define MAX_DAYS 366

typedef enum {
	Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec, NoMonth
} Months; /* note base0 */

typedef unsigned int TimeInt;

#define WKDAYS 7
/* number of days in each week. unlikely to change, but
 * useful as a readable indicator of usage where it occurs.
 * On the other hand, it is conceivable that one might be
 * interested in 4, 5, or 6 day periods, but redefine it
 * in specific programs and take responsibility there,
 * not here.
 */

/*=======  SUBROUTINES ========*/
void Time_init(void);
void Time_now(void);
void Time_new_year(TimeInt year);
void Time_next_day(void);
void Time_set_year(TimeInt year);
void Time_set_doy(const TimeInt doy);
void Time_set_mday(const TimeInt day);
void Time_set_month(const TimeInt mon);
time_t Time_timestamp(void);
time_t Time_timestamp_now(void);
TimeInt Time_days_in_month(Months month);
TimeInt Time_lastDOY(void);

char *Time_printtime(void);
char *Time_daynmshort(void);
char *Time_daynmshort_d(const TimeInt doy);
char *Time_daynmshort_dm(const TimeInt mday, const TimeInt mon);
char *Time_daynmlong(void);
char *Time_daynmlong_d(const TimeInt doy);
char *Time_daynmlong_dm(const TimeInt mday, const TimeInt mon);

TimeInt Time_get_year(void);
TimeInt Time_get_doy(void);
TimeInt Time_get_month(void);
TimeInt Time_get_week(void);
TimeInt Time_get_mday(void);
TimeInt Time_get_hour(void);
TimeInt Time_get_mins(void);
TimeInt Time_get_secs(void);
TimeInt Time_get_lastdoy(void);
TimeInt Time_get_lastdoy_y(TimeInt year);

TimeInt doy2month(const TimeInt doy);
TimeInt doy2mday(const TimeInt doy);
TimeInt doy2week(TimeInt doy);
TimeInt yearto4digit(TimeInt yr);
Bool isleapyear_now(void);
Bool isleapyear(const TimeInt year);

void interpolate_monthlyValues(double monthlyValues[], double dailyValues[]);

#endif
