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
*/
/********************************************************/
/********************************************************/

/* =================================================== */
/* =================================================== */
/*                INCLUDES / DEFINES                   */

#include "SW_SoilWater.h"

float swpotentl( double swc, int n);
/* =================================================== */
/* =================================================== */
/*             Public Function Definitions             */
/* --------------------------------------------------- */

float swpotentl( double swc, int n) {

  return SW_SWC_vol2bars(swc, n);
}

