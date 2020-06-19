/********************************************************/
/********************************************************/
/*	Source file: Sky.c
 Type: module - used by Weather.c
 Application: SOILWAT - soilwater dynamics simulator
 Purpose: Read / write and otherwise manage the
 information about the sky.
 */
/********************************************************/
/********************************************************/

/* =================================================== */
/* =================================================== */
/*                INCLUDES / DEFINES                   */

#include <stdio.h>
#include <stdlib.h>

#include "SOILWAT2/generic.h"
#include "SOILWAT2/filefuncs.h"

#include "SOILWAT2/SW_Defines.h"
#include "SOILWAT2/SW_Files.h"

#include "SOILWAT2/SW_Weather.h"
#include "SOILWAT2/SW_Sky.h"
#include "rSW_Sky.h"

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>


/* =================================================== */
/*                  Global Variables                   */
/* --------------------------------------------------- */

extern SW_SKY SW_Sky;
extern SW_WEATHER SW_Weather;

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

SEXP onGet_SW_SKY() {
	int i;

	SW_SKY *v = &SW_Sky;
	SEXP swCloud,SW_SKY;
	SEXP Cloud;
	SEXP Cloud_names, Cloud_names_x, Cloud_names_y;
	int k = 5;
  char *x_names[] = { "SkyCoverPCT", "WindSpeed_m/s", "HumidityPCT",
    "SnowDensity_kg/m^3", "RainEvents_per_day" };
  char *y_names[] = { "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December" };
	RealD *p_Cloud;

	PROTECT(swCloud = MAKE_CLASS("swCloud"));
	PROTECT(SW_SKY = NEW_OBJECT(swCloud));
	PROTECT(Cloud = allocMatrix(REALSXP, k, 12));
	p_Cloud = REAL(Cloud);
	for (i = 0; i < 12; i++) { //i=columns
		p_Cloud[0 + k * i] = v->cloudcov[i];
		p_Cloud[1 + k * i] = v->windspeed[i];
		p_Cloud[2 + k * i] = v->r_humidity[i];
		p_Cloud[3 + k * i] = v->snow_density[i];
		p_Cloud[4 + k * i] = v->n_rain_per_day[i];
	}

	PROTECT(Cloud_names = allocVector(VECSXP, 2));
	PROTECT(Cloud_names_x = allocVector(STRSXP, k));
	for (i = 0; i < k; i++) {
		SET_STRING_ELT(Cloud_names_x, i, mkChar(x_names[i]));
	}
	PROTECT(Cloud_names_y = allocVector(STRSXP, 12));
	for (i = 0; i < 12; i++) {
		SET_STRING_ELT(Cloud_names_y, i, mkChar(y_names[i]));
	}
	SET_VECTOR_ELT(Cloud_names, 0, Cloud_names_x);
	SET_VECTOR_ELT(Cloud_names, 1, Cloud_names_y);
	setAttrib(Cloud, R_DimNamesSymbol, Cloud_names);

	SET_SLOT(SW_SKY, install("Cloud"), Cloud);

	UNPROTECT(6);
	return SW_SKY;
}

void onSet_SW_SKY(SEXP sxp_SW_SKY) {
	int i, k = 5;
	SW_SKY *v = &SW_Sky;
	RealD *p_Cloud;
	PROTECT(sxp_SW_SKY);
	p_Cloud = REAL(GET_SLOT(sxp_SW_SKY, install("Cloud")));

	MyFileName = SW_F_name(eSky);

	for (i = 0; i < 12; i++) { //i=columns
		v->cloudcov[i] = p_Cloud[0 + k * i];
		v->windspeed[i] = p_Cloud[1 + k * i];
		v->r_humidity[i] = p_Cloud[2 + k * i];
		v->snow_density[i] = p_Cloud[3 + k * i];
		v->n_rain_per_day[i] = p_Cloud[4 + k * i];
	}

	UNPROTECT(1);
}
