/********************************************************/
/********************************************************/
/*	Source file: Sky.c
 Type: module - used by Weather.c
 Application: SOILWAT - soilwater dynamics simulator
 Purpose: Read / write and otherwise manage the
 information about the sky.
 History:
 (8/28/01) -- INITIAL CODING - cwb
 (10/12/2009) - (drs) added pressure
 01/12/2010	(drs) removed pressure (used for snow sublimation) in SW_SKY_read(void) as case 4
 06/16/2010	(drs) all cloud.in input files contain on line 1 cloud cover, line 2 wind speed, line 3 rel. humidity, and line 4 transmissivity, but SW_SKY_read() was reading rel. humidity from line 1 and cloud cover from line 3 instead -> SW_SKY_read() is now reading as the input files are formatted
 08/22/2011	(drs) new 5th line in cloud.in containing snow densities (kg/m3): read  in SW_SKY_read(void) as case 4
 09/26/2011	(drs) added calls to Times.c:interpolate_monthlyValues() to SW_SKY_init() for each monthly input variable
 06/27/2013	(drs)	closed open files if LogError() with LOGFATAL is called in SW_SKY_read()
 */
/********************************************************/
/********************************************************/

/* =================================================== */
/* =================================================== */
/*                INCLUDES / DEFINES                   */

#include <stdio.h>
#include <stdlib.h>
#include "generic.h"
#include "filefuncs.h"
#include "SW_Defines.h"
#include "SW_Files.h"
#include "SW_Sky.h"

/* =================================================== */
/*                  Global Variables                   */
/* --------------------------------------------------- */

SW_SKY SW_Sky; /* declared here, externed elsewhere */

/* =================================================== */
/*                Module-Level Variables               */
/* --------------------------------------------------- */
static char *MyFileName;

/* =================================================== */
/* =================================================== */
/*             Private Function Definitions            */
/* --------------------------------------------------- */

/* =================================================== */
/* =================================================== */
/*             Public Function Definitions             */
/* --------------------------------------------------- */

void SW_SKY_read(void) {
	/* =================================================== */
	/* 6-Oct-03 (cwb) - all this time I had lines 1 & 3
	 *                  switched!
	 * 06/16/2010	(drs) all cloud.in input files contain on line 1 cloud cover, line 2 wind speed, line 3 rel. humidity, and line 4 transmissivity, but SW_SKY_read() was reading rel. humidity from line 1 and cloud cover from line 3 instead -> SW_SKY_read() is now reading as the input files are formatted
	 */
	SW_SKY *v = &SW_Sky;
	FILE *f;
	int lineno = 0, x = 0;

	MyFileName = SW_F_name(eSky);
	f = OpenFile(MyFileName, "r");

	while (GetALine(f, inbuf)) {
		switch (lineno) {
		case 0:
			x = sscanf(inbuf, "%lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf", &v->cloudcov[0], &v->cloudcov[1], &v->cloudcov[2], &v->cloudcov[3], &v->cloudcov[4],
					&v->cloudcov[5], &v->cloudcov[6], &v->cloudcov[7], &v->cloudcov[8], &v->cloudcov[9], &v->cloudcov[10], &v->cloudcov[11]);
			break;
		case 1:
			x = sscanf(inbuf, "%lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf", &v->windspeed[0], &v->windspeed[1], &v->windspeed[2], &v->windspeed[3], &v->windspeed[4],
					&v->windspeed[5], &v->windspeed[6], &v->windspeed[7], &v->windspeed[8], &v->windspeed[9], &v->windspeed[10], &v->windspeed[11]);
			break;
		case 2:
			x = sscanf(inbuf, "%lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf", &v->r_humidity[0], &v->r_humidity[1], &v->r_humidity[2], &v->r_humidity[3], &v->r_humidity[4],
					&v->r_humidity[5], &v->r_humidity[6], &v->r_humidity[7], &v->r_humidity[8], &v->r_humidity[9], &v->r_humidity[10], &v->r_humidity[11]);
			break;
		case 3:
			x = sscanf(inbuf, "%lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf", &v->transmission[0], &v->transmission[1], &v->transmission[2], &v->transmission[3],
					&v->transmission[4], &v->transmission[5], &v->transmission[6], &v->transmission[7], &v->transmission[8], &v->transmission[9], &v->transmission[10],
					&v->transmission[11]);
			break;
		case 4:
			x = sscanf(inbuf, "%lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf", &v->snow_density[0], &v->snow_density[1], &v->snow_density[2], &v->snow_density[3],
					&v->snow_density[4], &v->snow_density[5], &v->snow_density[6], &v->snow_density[7], &v->snow_density[8], &v->snow_density[9], &v->snow_density[10],
					&v->snow_density[11]);
			break;
		}
		if (x < 12) {
			CloseFile(&f);
			sprintf(errstr, "%s : invalid record %d.\n", MyFileName, lineno);
			LogError(logfp, LOGFATAL, errstr);
		}

		x = 0;
		lineno++;
	}

	CloseFile(&f);

}

#ifdef RSOILWAT
SEXP onGet_SW_SKY() {
	int i;

	SW_SKY *v = &SW_Sky;
	SEXP swCloud,SW_SKY;
	SEXP Cloud;
	SEXP Cloud_names, Cloud_names_x, Cloud_names_y;
	char *x_names[] = { "SkyCoverPCT", "WindSpeed_m/s", "HumidityPCT", "Transmissivity", "SnowDensity_kg/m^3" };
	char *y_names[] = { "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December" };
	RealD *p_Cloud;

	PROTECT(swCloud = MAKE_CLASS("swCloud"));
	PROTECT(SW_SKY = NEW_OBJECT(swCloud));
	PROTECT(Cloud = allocMatrix(REALSXP, 5, 12));
	p_Cloud = REAL(Cloud);
	for (i = 0; i < 12; i++) { //i=columns
		p_Cloud[0 + 5 * i] = v->cloudcov[i];
		p_Cloud[1 + 5 * i] = v->windspeed[i];
		p_Cloud[2 + 5 * i] = v->r_humidity[i];
		p_Cloud[3 + 5 * i] = v->transmission[i];
		p_Cloud[4 + 5 * i] = v->snow_density[i];
	}

	PROTECT(Cloud_names = allocVector(VECSXP, 2));
	PROTECT(Cloud_names_x = allocVector(STRSXP, 5));
	for (i = 0; i < 5; i++) {
		SET_STRING_ELT(Cloud_names_x, i, mkChar(x_names[i]));
	}
	PROTECT(Cloud_names_y = allocVector(STRSXP, 12));
	for (i = 0; i < 12; i++) {
		SET_STRING_ELT(Cloud_names_y, i, mkChar(y_names[i]));
	}
	SET_VECTOR_ELT(Cloud_names, 0, Cloud_names_x);
	SET_VECTOR_ELT(Cloud_names, 1, Cloud_names_y);
	setAttrib(Cloud, R_DimNamesSymbol, Cloud_names);

	SET_SLOT(SW_SKY,install("Cloud"),Cloud);

	UNPROTECT(6);
	return SW_SKY;
}
void onSet_SW_SKY(SEXP sxp_SW_SKY) {
	int i;
	SW_SKY *v = &SW_Sky;
	RealD *p_Cloud;
	PROTECT(sxp_SW_SKY);
	p_Cloud = REAL(GET_SLOT(sxp_SW_SKY,install("Cloud")));

	MyFileName = SW_F_name(eSky);

	for (i = 0; i < 12; i++) { //i=columns
		v->cloudcov[i] = p_Cloud[0 + 5 * i];
		v->windspeed[i] = p_Cloud[1 + 5 * i];
		v->r_humidity[i] = p_Cloud[2 + 5 * i];
		v->transmission[i] = p_Cloud[3 + 5 * i];
		v->snow_density[i] = p_Cloud[4 + 5 * i];
	}
	UNPROTECT(1);
}
#endif
void SW_SKY_init(double scale_sky[], double scale_wind[], double scale_rH[], double scale_transmissivity[]) {
	int i;
	SW_SKY *v = &SW_Sky;
	for(i=0; i<MAX_MONTHS; i++) {
		v->cloudcov[i] = min(100., max(0.0,scale_sky[i]+v->cloudcov[i]));
		v->windspeed[i] = max(0.0, scale_wind[i] * v->windspeed[i]);
		v->r_humidity[i] = min(100., max(0.0, scale_rH[i] + v->r_humidity[i]));
		v->transmission[i] = min(1.0, max(0.0, scale_transmissivity[i]*v->transmission[i]));
	}
	/* interpolate monthly input values to daily records */
	interpolate_monthlyValues(v->cloudcov, v->cloudcov_daily);
	interpolate_monthlyValues(v->windspeed, v->windspeed_daily);
	interpolate_monthlyValues(v->r_humidity, v->r_humidity_daily);
	interpolate_monthlyValues(v->transmission, v->transmission_daily);
	interpolate_monthlyValues(v->snow_density, v->snow_density_daily);
}

void SW_SKY_construct(void) {
	/* note that an initializer that is called during
	 * execution (better called clean() or something)
	 * will need to free all allocated memory first
	 * before clearing structure.
	 */

}
