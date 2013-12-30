/********************************************************/
/********************************************************/
/*  Source file: SW_Times.h
 *  Type: header
 *  Application: SOILWAT - soilwater dynamics simulator
 *  Purpose: Provides a consistent definition for the
 *           time values that might be needed by various
 *           objects.
 *  History:
 *    9/11/01 -- INITIAL CODING - cwb
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
 *  2/14/03 - cwb - removed generally useful code to
 *          common/Times.[ch].  Only SOILWAT-specific stuff here.
 *
 */
/********************************************************/
/********************************************************/

#ifndef SW_TIMES_H
#define SW_TIMES_H

#include "Times.h"

/*---------------------------------------------------------------*/
typedef enum {
	Yesterday, Today
} TwoDays;

typedef struct {
	TimeInt first, last, total;
} SW_TIMES;

#define DAYFIRST_NORTH 1
#define DAYLAST_NORTH 366
#define DAYFIRST_SOUTH 183
#define DAYLAST_SOUTH 182
#define DAYMID_NORTH 183
#define DAYMID_SOUTH 366
/* The above define the beginning, ending and middle
 * days of the year for northern and southern
 * hemispheres, so there won't be a coding accident.
 * The user need only supply a 0/1 flag in the file
 * containing the start/end years.
 */

#endif
