/********************************************************/
/********************************************************/
/*	Source file: SW_Flow_subs.c
 Type: module
 Application: SOILWAT - soilwater dynamics simulator
 Purpose: Routines called by the water_flow routine
 and the more generic routines found in
 Flow_lib.c to provide a generic interface to
 other routines that access the model data
 structures.  This way, if the Flow_lib.c is
 used in another context, that application
 can provide its own version of these routines
 without requiring modification of the Flow_lib.
 History:

 04/16/2013	(clk)	Changed swpotentl() to SWCbulk2SWPmatric
 the function now requires the fraction of gravel content as a parameter
 Updated the use of swpotentl in other files
 */
/********************************************************/
/********************************************************/

/* =================================================== */
/* =================================================== */
/*                INCLUDES / DEFINES                   */

#include "SW_SoilWater.h"

double SWCbulk2SWPmatric(double fractionGravel, double swcBulk, int n);
/* =================================================== */
/* =================================================== */
/*             Public Function Definitions             */
/* --------------------------------------------------- */

double SWCbulk2SWPmatric(double fractionGravel, double swcBulk, int n) {

	return SW_SWCbulk2SWPmatric(fractionGravel, swcBulk, n);
}

