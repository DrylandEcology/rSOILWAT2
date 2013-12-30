/********************************************************/
/********************************************************/
/*	Source file: Times.c
 Type: module

 Purpose: Collection of time-oriented functions.
 Allows the parent program (the "model") to maintain
 a separate clock internally, but the library can
 also work off the system clock (real time).

 History:
 (8/28/01) -- INITIAL CODING - cwb
 12/02 - IMPORTANT CHANGE - cwb
 refer to comments in Times.h regarding base0
 19-Sep-03 (cwb) Imported a bunch of new routines
 and added the facility for model time.
 03/12/2010	(drs) "365:366" -> Time_get_lastdoy_y(TimeInt year) {return isleapyear(year) ? 366 : 365; }
 09/26/2011	(drs)	added function interpolate_monthlyValues(): interpolating a record with monthly values and outputs a record with daily values
 */
/********************************************************/
/********************************************************/

/* =================================================== */
/*                INCLUDES / DEFINES                   */
/* --------------------------------------------------- */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "generic.h"
#include "Times.h"

#define MAX_DAYSTR 10
/* =================================================== */
/*                Module-Level Variables               */
/* --------------------------------------------------- */

/* cum_monthdays has one extra for the Doy2Month macro
 * to be able to determine the 12th month.  */
static TimeInt last_doy;
static TimeInt monthdays[12] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
static TimeInt days_in_month[MAX_MONTHS], cum_monthdays[MAX_MONTHS + 1];

static time_t _timestamp;
static struct tm _tym; /* "current" time for the code */

static char _outs[MAX_DAYSTR + 1]; /* for day/month string funcs */

static void _reinit(void);
static TimeInt _yearto4digit_t(void);
static void _remaketime(void);

/* =================================================== */
/* =================================================== */
/*            "Public" Function Definitions            */
/* --------------------------------------------------- */

void Time_init(void) {
	/* =================================================== */

	memcpy(days_in_month, monthdays, sizeof(TimeInt) * MAX_MONTHS);
	memset(cum_monthdays, 0, sizeof(TimeInt) * MAX_MONTHS);
	cum_monthdays[NoMonth] = 1000;

	Time_now(); /* set structure's time to current */
}

void Time_now(void) {
	/* =================================================== */
	time_t x = time(NULL );

	memcpy(&_tym, (struct tm *) localtime(&x), sizeof(struct tm));

	_reinit();
}

void Time_new_year(TimeInt year) {
	/* =================================================== */

	year = yearto4digit(year);
	_tym.tm_year = (int) year - 1900;
	_reinit();
	Time_set_doy(1);

}

void Time_next_day(void) {
	/* =================================================== */
	/* set current day to tomorrow, change year if needed */

	if ((TimeInt) _tym.tm_yday == last_doy) {
		_tym.tm_year++;
		_reinit();
	} else {
		Time_set_doy(++_tym.tm_yday);
	}

}

void Time_set_year(TimeInt year) {
	/* =================================================== */
	/* sets the internal yearday and year to match year */
	/* makes sure the month and month-day are correct for */
	/* yearday.
	 * DOES NOT change current doy.  Use new_year() for that.
	 */

	year = yearto4digit(year);

	if (year == _yearto4digit_t())
		return;

	_tym.tm_year = (int) year - 1900;
	_reinit();
	_tym.tm_mday = (int) doy2mday((TimeInt) _tym.tm_yday);
	_tym.tm_mon = (int) doy2month((TimeInt) _tym.tm_yday);
	_remaketime();

}

void Time_set_doy(const TimeInt doy) {
	/* =================================================== */

	_tym.tm_yday = (int) doy;
	_tym.tm_mday = (int) doy2mday(doy);
	_tym.tm_mon = (int) doy2month(doy);
	_remaketime();
}

void Time_set_mday(const TimeInt day) {
	/* =================================================== */

	_tym.tm_mday = (int) day;
	_remaketime();

}

void Time_set_month(const TimeInt mon) {
	/* =================================================== */

	_tym.tm_mon = (int) mon;
	_remaketime();

}

time_t Time_timestamp(void) {
	/* =================================================== */
	/* returns the timestamp of the "model" time.  to get
	 * actual timestamp, call Time_timestamp_now()
	 */
	return _timestamp;

}

time_t Time_timestamp_now(void) {
	/* =================================================== */
	/* returns the timestamp of the current real time.
	 */
	return time(NULL );

}

TimeInt Time_lastDOY(void) {
	/* =================================================== */

	return cum_monthdays[Dec];
}

TimeInt Time_days_in_month(Months month) {
	/* =================================================== */

	return days_in_month[month];
}

char *Time_printtime(void) {
	/* =================================================== */
	return (asctime(&_tym));
}

char *Time_daynmshort(void) {
	/* =================================================== */

	strftime(_outs, MAX_DAYSTR, "%a", &_tym);
	return _outs;
}

char *Time_daynmshort_d(const TimeInt doy) {
	/* =================================================== */
	struct tm tmp = _tym;

	tmp.tm_mday = doy2mday(doy);
	tmp.tm_mon = doy2month(doy);
	strftime(_outs, MAX_DAYSTR, "%a", &tmp);
	return _outs;
}

char *Time_daynmshort_dm(const TimeInt mday, const TimeInt mon) {
	/* =================================================== */
	struct tm tmp = _tym;

	tmp.tm_mday = mday;
	tmp.tm_mon = mon;
	strftime(_outs, MAX_DAYSTR, "%a", &tmp);
	return _outs;
}

char *Time_daynmlong(void) {
	/* =================================================== */

	strftime(_outs, MAX_DAYSTR, "%A", &_tym);
	return _outs;
}

char *Time_daynmlong_d(const TimeInt doy) {
	/* =================================================== */
	struct tm tmp = _tym;

	tmp.tm_mday = doy2mday(doy);
	tmp.tm_mon = doy2month(doy);
	strftime(_outs, MAX_DAYSTR, "%A", &tmp);
	return _outs;
}

char *Time_daynmlong_dm(const TimeInt mday, const TimeInt mon) {
	/* =================================================== */
	struct tm tmp = _tym;

	tmp.tm_mday = mday;
	tmp.tm_mon = mon;
	strftime(_outs, MAX_DAYSTR, "%A", &tmp);
	return _outs;
}

/* =================================================== */
/* simple methods to return state values */

TimeInt Time_get_year(void) {
	return _yearto4digit_t();
}
TimeInt Time_get_doy(void) {
	return (TimeInt) _tym.tm_yday;
}
TimeInt Time_get_month(void) {
	return (TimeInt) _tym.tm_mon;
}
TimeInt Time_get_week(void) {
	return doy2week(_tym.tm_yday);
}
TimeInt Time_get_mday(void) {
	return (TimeInt) _tym.tm_mday;
}
TimeInt Time_get_hour(void) {
	return (TimeInt) _tym.tm_hour;
}
TimeInt Time_get_mins(void) {
	return (TimeInt) _tym.tm_min;
}
TimeInt Time_get_secs(void) {
	return (TimeInt) _tym.tm_sec;
}
TimeInt Time_get_lastdoy(void) {
	return last_doy;
}
TimeInt Time_get_lastdoy_y(TimeInt year) {
	return isleapyear(year) ? 366 : 365;
}

/* =================================================== */
/* =================================================== */
/* These next six tend to get called a lot and they're
 * pretty independent of the current time, so I'm
 * removing the preceeding Time_ to shorten the calls in
 * the code
 */
TimeInt doy2month(const TimeInt doy) {
	/* =================================================== */
	/* doy is base1, mon becomes base0 month containing doy.
	 * note mon can't become 13, so any day after Nov 30
	 * returns Dec.
	 */
	TimeInt mon;

	for (mon = Jan; mon < Dec && doy > cum_monthdays[mon]; mon++)
		;
	return mon;

}

TimeInt doy2mday(const TimeInt doy) {
	/* =================================================== */
	/* doy is base1, mon becomes base0 month containing doy.
	 * note mon can't become 13, so any day after Nov 30
	 * returns Dec.
	 */

	TimeInt mon = doy2month(doy);
	return (mon == Jan) ? doy : doy - cum_monthdays[mon - 1];

}

TimeInt doy2week(TimeInt doy) {
	/* =================================================== */
	/* Enter with doy base1 and return base0 number of 7 day
	 * periods (weeks) since beginning of year. Note that week
	 * number doesn't necessarily correspond to the calendar week.
	 * Jan 1 starts on different days depending on the year.  In
	 * 2000 it started on Sun: each year later it starts 1 day
	 * later, each year earlier it started one day earlier.
	 */
	return ((TimeInt) (((doy) - 1) / WKDAYS));
}

TimeInt yearto4digit(TimeInt yr) {
	/* =================================================== */
	/* handle the Y2K problems */

	return (TimeInt) ((yr > 100) ? yr : (yr < 50) ? 2000 + yr : 1900 + yr);

}

Bool isleapyear_now(void) {
	/* =================================================== */
	/* check current year from struct tm */
	return isleapyear(_yearto4digit_t());
}

Bool isleapyear(const TimeInt year) {
	/* =================================================== */

	int yr = (year > 100) ? year : yearto4digit(year);
	int t = (yr / 100) * 100;

	return (Bool) (((yr % 4) == 0) && (((t) != yr) || ((yr % 400) == 0)));

}

void interpolate_monthlyValues(double monthlyValues[], double dailyValues[]) {
	/**********************************************************************
	 PURPOSE: linear interpolation of monthly value; monthly values are assumed to representative for the 15th of a month

	 HISTORY:
	 09/22/2011 (drs)

	 INPUTS:
	 monthlyValues - record with values for each month

	 OUTPUTS:
	 dailyValues - linear interpolation for each day
	 **********************************************************************/

	unsigned int doy, mday, month, month2 = NoMonth;
	double sign = 1.;

	for (doy = 1; doy <= MAX_DAYS; doy++) {
		mday = doy2mday(doy);
		month = doy2month(doy);

		if (mday == 15) {
			dailyValues[doy] = monthlyValues[month];
		} else {
			if (mday >= 15) {
				month2 = (month == Dec) ? Jan : month + 1;
				sign = 1;
			} else {
				month2 = (month == Jan) ? Dec : month - 1;
				sign = -1;
			}

			dailyValues[doy] = monthlyValues[month] + sign * (monthlyValues[month2] - monthlyValues[month]) / (monthdays[month]) * (mday - 15.);
		}
	}
}

/* =================================================== */
/* =================================================== */
/*           "Private" Function Definitions            */
/* --------------------------------------------------- */

static void _reinit(void) {
	/* --------------------------------------------------- */
	/* reset the year's month-days arrays */
	TimeInt m;

	days_in_month[Feb] = (isleapyear_now()) ? 29 : 28;
	cum_monthdays[Jan] = days_in_month[Jan];
	for (m = Feb; m < NoMonth; m++)
		cum_monthdays[m] = days_in_month[m] + cum_monthdays[m - 1];

	last_doy = cum_monthdays[Dec];
}

static TimeInt _yearto4digit_t(void) {
	/* --------------------------------------------------- */
	/* convert (struct tm).tm_year to 4 digit */

	return (TimeInt) (_tym.tm_year + 1900);

}

static void _remaketime(void) {
	/* --------------------------------------------------- */

	_timestamp = mktime(&_tym);

}
