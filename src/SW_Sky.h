/********************************************************/
/********************************************************/
/*	Source file: Sky.c
	Type: header - used by Weather.c
	Application: SOILWAT - soilwater dynamics simulator
	Purpose: Support definitions for Sky.c, Weather.c, etc.

	History:
	(8/28/01) -- INITIAL CODING - cwb
	(10/12/2009) - (drs) added pressure
	01/12/2010	(drs) removed pressure (used for snow sublimation)
	08/22/2011	(drs) added monthly parameter 'snow_density' to struct SW_SKY to estimate snow depth
	09/26/2011	(drs) added a daily variable for each monthly input in struct SW_SKY: RealD cloudcov_daily, windspeed_daily, r_humidity_daily, transmission_daily, snow_density_daily each of [MAX_DAYS]
*/
/********************************************************/
/********************************************************/

#ifndef SW_SKY_H
#define SW_SKY_H

#ifdef RSOILWAT
#include <R.h>
#include <Rdefines.h>
#include <Rconfig.h>
#include <Rinterface.h>
#include <Rinternals.h>
#endif
#include "SW_Times.h"

typedef struct {
    RealD cloudcov     [MAX_MONTHS], /* monthly cloud cover (frac) */
          windspeed    [MAX_MONTHS], /* windspeed (m/s) */
          r_humidity   [MAX_MONTHS], /* relative humidity (%) */
          transmission [MAX_MONTHS], /* frac light transmitted by atmos. */ /* used as input for petfunc, but algorithm cancels it out */
          snow_density	[MAX_MONTHS];	/* snow density (kg/m3) */

    RealD cloudcov_daily     [MAX_DAYS+1], /* interpolated daily cloud cover (frac) */
          windspeed_daily    [MAX_DAYS+1], /* interpolated daily windspeed (m/s) */
          r_humidity_daily   [MAX_DAYS+1], /* interpolated daily relative humidity (%) */
          transmission_daily [MAX_DAYS+1], /* interpolated daily frac light transmitted by atmos. */ /* used as input for petfunc, but algorithm cancels it out */
          snow_density_daily	[MAX_DAYS+1];	/* interpolated daily snow density (kg/m3) */

} SW_SKY;

void SW_SKY_read(void);
void SW_SKY_init(double scale_sky[], double scale_wind[], double scale_rH[], double scale_transmissivity[]);
void SW_SKY_construct(void);

#ifdef RSOILWAT
SEXP onGet_SW_SKY();
void onSet_SW_SKY(SEXP SW_SKY);
#endif

#endif
