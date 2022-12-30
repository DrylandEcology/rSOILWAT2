/********************************************************/
/********************************************************/
/*  Source file: Markov.c
 *  Type: module; used by Weather.c
 *
 *  Application: SOILWAT - soilwater dynamics simulator
 *  Purpose: Read / write and otherwise manage the markov
 *           weather generation information.
 */
/********************************************************/
/********************************************************/

/* =================================================== */
/* =================================================== */
/*                INCLUDES / DEFINES                   */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>

#include "SOILWAT2/include/generic.h"
#include "SOILWAT2/include/filefuncs.h"
#include "SOILWAT2/include/Times.h"

#include "SOILWAT2/include/SW_Files.h"
#include "SOILWAT2/include/SW_Weather.h"
#include "SOILWAT2/include/SW_Markov.h"

#include "rSW_Markov.h" // externs `SW_Markov`

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>


/* =================================================== */
/*                  Local Variables                    */
/* --------------------------------------------------- */

static char *cSW_MKV[] = { "Prob", "Conv" };



/* =================================================== */
/*             Global Function Definitions             */
/* --------------------------------------------------- */


SEXP onGet_MKV(void) {
	SEXP swMarkov;
	SEXP MKV;

	PROTECT(swMarkov = MAKE_CLASS("swMarkov"));
	PROTECT(MKV = NEW_OBJECT(swMarkov));
	SET_SLOT(MKV, install(cSW_MKV[0]), onGet_MKV_prob());
	SET_SLOT(MKV, install(cSW_MKV[1]), onGet_MKV_conv());

	UNPROTECT(2);
	return MKV;
}

void onSet_MKV(SEXP MKV) {
  SEXP MKV_prob, MKV_conv;

  SW_MKV_construct();

  PROTECT(MKV_prob = GET_SLOT(MKV, install(cSW_MKV[0])));
  PROTECT(MKV_conv = GET_SLOT(MKV, install(cSW_MKV[1])));

  if (!onSet_MKV_prob(MKV_prob) && SW_Weather.generateWeatherMethod == 2) {
    LogError(
      logfp,
      LOGFATAL,
      "Markov weather generator: "
      "rSOILWAT2 failed to pass `MKV_prob` values to SOILWAT2.\n"
    );
  }

  if (!onSet_MKV_conv(MKV_conv) && SW_Weather.generateWeatherMethod == 2) {
    LogError(
      logfp,
      LOGFATAL,
      "Markov weather generator: "
      "rSOILWAT2 failed to pass `MKV_conv` values to SOILWAT2.\n"
    );
  }

  UNPROTECT(2);
}


SEXP onGet_MKV_prob(void) {
	int i;
	const int nitems = 5;
	SW_MARKOV *v = &SW_Markov;
	SEXP MKV_prob, MKV_prob_names, MKV_prob_names_y;
	RealD *p_MKV_prob;
	char *cMKC_prob[] = { "DOY", "p_wet_wet", "p_wet_dry", "avg_ppt", "std_ppt" };

	PROTECT(MKV_prob = allocMatrix(REALSXP,MAX_DAYS, nitems));
	p_MKV_prob = REAL(MKV_prob);

	for (i = 0; i < MAX_DAYS; i++) {
		p_MKV_prob[i + MAX_DAYS * 0] = (i + 1);       // Day of year
		p_MKV_prob[i + MAX_DAYS * 1] = v->wetprob[i]; // probability of being wet today given a wet yesterday
		p_MKV_prob[i + MAX_DAYS * 2] = v->dryprob[i]; // probability of being wet today given a dry yesterday
		p_MKV_prob[i + MAX_DAYS * 3] = v->avg_ppt[i]; // mean precip (cm) of wet days
		p_MKV_prob[i + MAX_DAYS * 4] = v->std_ppt[i]; // std dev. for precip of wet days
	}

	PROTECT(MKV_prob_names = allocVector(VECSXP, 2));
	PROTECT(MKV_prob_names_y = allocVector(STRSXP, nitems));

	for (i = 0; i < nitems; i++) {
		SET_STRING_ELT(MKV_prob_names_y, i, mkChar(cMKC_prob[i]));
	}

	SET_VECTOR_ELT(MKV_prob_names, 1, MKV_prob_names_y);
	setAttrib(MKV_prob, R_DimNamesSymbol, MKV_prob_names);

	UNPROTECT(3);
	return MKV_prob;
}

Bool onSet_MKV_prob(SEXP MKV_prob) {
	SW_MARKOV *v = &SW_Markov;
	const int nitems = 5;
	int i;
	RealD *p_MKV_prob;

	if (nrows(MKV_prob) != MAX_DAYS && ncols(MKV_prob) != nitems) {
		return FALSE;
	}

	p_MKV_prob = REAL(MKV_prob);

	for (i = 0; i < MAX_DAYS; i++) {
		v->wetprob[i] = p_MKV_prob[i + MAX_DAYS * 1];
		v->dryprob[i] = p_MKV_prob[i + MAX_DAYS * 2];
		v->avg_ppt[i] = p_MKV_prob[i + MAX_DAYS * 3];
		v->std_ppt[i] = p_MKV_prob[i + MAX_DAYS * 4];
	}
	return TRUE;
}


SEXP onGet_MKV_conv(void) {
	int i;
	const int nitems = 11;
	SW_MARKOV *v = &SW_Markov;
	SEXP MKV_conv, MKV_conv_names, MKV_conv_names_y;
	RealD *p_MKV_conv;
	char *cMKV_conv[] = { "WEEK", "wTmax_C", "wTmin_C", "var_wTmax",
		"cov_wTmaxmin", "cov_wTminmax", "var_wTmin",
		"cfmax_wet", "cfmax_dry", "cfmin_wet", "cfmin_dry" };

	PROTECT(MKV_conv = allocMatrix(REALSXP, MAX_WEEKS, nitems));
	p_MKV_conv = REAL(MKV_conv);

	for (i = 0; i < MAX_WEEKS; i++) {
		p_MKV_conv[i + MAX_WEEKS * 0] = (i + 1);
		p_MKV_conv[i + MAX_WEEKS * 1] = v->u_cov[i][0];       // mean weekly maximum daily temp
		p_MKV_conv[i + MAX_WEEKS * 2] = v->u_cov[i][1];       // mean weekly minimum daily temp
		p_MKV_conv[i + MAX_WEEKS * 3] = v->v_cov[i][0][0];    // mean weekly variance of maximum daily temp
		p_MKV_conv[i + MAX_WEEKS * 4] = v->v_cov[i][0][1];    // mean weekly covariance of min/max daily temp
		p_MKV_conv[i + MAX_WEEKS * 5] = v->v_cov[i][1][0];    // mean weekly covariance of min/max daily temp
		p_MKV_conv[i + MAX_WEEKS * 6] = v->v_cov[i][1][1];    // mean weekly variance of minimum daily temp
		p_MKV_conv[i + MAX_WEEKS * 7] = v->cfxw[i];           // correction factor for tmax for wet days
		p_MKV_conv[i + MAX_WEEKS * 8] = v->cfxd[i];           // correction factor for tmax for dry days
		p_MKV_conv[i + MAX_WEEKS * 9] = v->cfnw[i];           // correction factor for tmin for wet days
		p_MKV_conv[i + MAX_WEEKS * 10] = v->cfnd[i];           // correction factor for tmin for dry days
	}
	PROTECT(MKV_conv_names = allocVector(VECSXP, 2));
	PROTECT(MKV_conv_names_y = allocVector(STRSXP, nitems));
	for (i = 0; i < nitems; i++) {
		SET_STRING_ELT(MKV_conv_names_y, i, mkChar(cMKV_conv[i]));
	}
	SET_VECTOR_ELT(MKV_conv_names, 1, MKV_conv_names_y);
	setAttrib(MKV_conv, R_DimNamesSymbol, MKV_conv_names);

	UNPROTECT(3);
	return MKV_conv;
}

Bool onSet_MKV_conv(SEXP MKV_conv) {
	SW_MARKOV *v = &SW_Markov;
	const int nitems = 11;
	int i;
	RealD *p_MKV_conv;


	if ((nrows(MKV_conv) != MAX_WEEKS) && (ncols(MKV_conv) != nitems)) {
		return FALSE;
	}

	p_MKV_conv = REAL(MKV_conv);

	for (i = 0; i < MAX_WEEKS; i++) {
		v->u_cov[i][0] = p_MKV_conv[i + MAX_WEEKS * 1];    // mean weekly maximum daily temp
		v->u_cov[i][1] = p_MKV_conv[i + MAX_WEEKS * 2];    // mean weekly minimum daily temp
		v->v_cov[i][0][0] = p_MKV_conv[i + MAX_WEEKS * 3]; // mean weekly variance of maximum daily temp
		v->v_cov[i][0][1] = p_MKV_conv[i + MAX_WEEKS * 4]; // mean weekly covariance of min/max daily temp
		v->v_cov[i][1][0] = p_MKV_conv[i + MAX_WEEKS * 5]; // mean weekly covariance of min/max daily temp
		v->v_cov[i][1][1] = p_MKV_conv[i + MAX_WEEKS * 6]; // mean weekly variance of minimum daily temp
		v->cfxw[i] = p_MKV_conv[i + MAX_WEEKS * 7];        // correction factor for tmax for wet days
		v->cfxd[i] = p_MKV_conv[i + MAX_WEEKS * 8];        // correction factor for tmax for dry days
		v->cfnw[i] = p_MKV_conv[i + MAX_WEEKS * 9];        // correction factor for tmin for wet days
		v->cfnd[i] = p_MKV_conv[i + MAX_WEEKS * 10];       // correction factor for tmin for dry days
	}

	return TRUE;
}

