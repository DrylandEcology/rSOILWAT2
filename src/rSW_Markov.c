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

#include "SOILWAT2/SW_Files.h"
#include "SOILWAT2/Times.h"
#include "SOILWAT2/SW_Markov.h"

#include "rSW_Markov.h"

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>


/* =================================================== */
/*                  Global Variables                   */
/* --------------------------------------------------- */
extern SW_MARKOV SW_Markov; /* declared here, externed elsewhere */

/* =================================================== */
/*                Module-Level Variables               */
/* --------------------------------------------------- */

static char *MyFileName;

/* =================================================== */
/* =================================================== */
/*             Public Function Definitions             */
/* --------------------------------------------------- */


SEXP onGet_MKV(void) {
	SEXP swMarkov;
	SEXP MKV;//, MKV_names;

	PROTECT(swMarkov = MAKE_CLASS("swMarkov"));
	PROTECT(MKV = NEW_OBJECT(swMarkov));
	SET_SLOT(MKV,install("Prob"),onGet_MKV_prob());
	SET_SLOT(MKV,install("Conv"),onGet_MKV_conv());
	//PROTECT(MKV_names = allocVector(STRSXP,2));
	//SET_STRING_ELT(MKV_names,0,mkChar("Prob"));
	//SET_STRING_ELT(MKV_names,1,mkChar("Conv"));
	//setAttrib(MKV,R_NamesSymbol,MKV_names);
	UNPROTECT(2);
	return MKV;
}

SEXP onGet_MKV_prob(void) {
	int i;
	SW_MARKOV *v = &SW_Markov;
	SEXP MKV_prob, MKV_prob_names, MKV_prob_names_y;
	RealD *p_MKV_prob;
	char *cMKC_prob[] = { "day", "wet", "dry", "avg_ppt", "std_ppt" };

	PROTECT(MKV_prob = allocMatrix(REALSXP,MAX_DAYS,5));
	p_MKV_prob = REAL(MKV_prob);
	for (i = 0; i < MAX_DAYS; i++) {
		p_MKV_prob[i + MAX_DAYS * 0] = (i + 1);
		p_MKV_prob[i + MAX_DAYS * 1] = v->wetprob[i];
		p_MKV_prob[i + MAX_DAYS * 2] = v->dryprob[i];
		p_MKV_prob[i + MAX_DAYS * 3] = v->avg_ppt[i];
		p_MKV_prob[i + MAX_DAYS * 4] = v->std_ppt[i];
	}
	PROTECT(MKV_prob_names = allocVector(VECSXP,2));
	PROTECT(MKV_prob_names_y = allocVector(STRSXP,5));
	for (i = 0; i < 5; i++)
		SET_STRING_ELT(MKV_prob_names_y, i, mkChar(cMKC_prob[i]));
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

	MyFileName = SW_F_name(eMarkovProb);

	if (nrows(MKV_prob) != MAX_DAYS && ncols(MKV_prob) != nitems)
		return FALSE;
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
	SW_MARKOV *v = &SW_Markov;
	SEXP MKV_conv, MKV_conv_names, MKV_conv_names_y;
	RealD *p_MKV_conv;
	char *cMKV_conv[] = { "week", "t1", "t2", "t3", "t4", "t5", "t6" };

	PROTECT(MKV_conv = allocMatrix(REALSXP, MAX_WEEKS, 7));
	p_MKV_conv = REAL(MKV_conv);
	for (i = 0; i < MAX_WEEKS; i++) {
		p_MKV_conv[i + MAX_WEEKS * 0] = (i + 1);
		p_MKV_conv[i + MAX_WEEKS * 1] = v->u_cov[i][0];
		p_MKV_conv[i + MAX_WEEKS * 2] = v->u_cov[i][1];
		p_MKV_conv[i + MAX_WEEKS * 3] = v->v_cov[i][0][0];
		p_MKV_conv[i + MAX_WEEKS * 4] = v->v_cov[i][0][1];
		p_MKV_conv[i + MAX_WEEKS * 5] = v->v_cov[i][1][0];
		p_MKV_conv[i + MAX_WEEKS * 6] = v->v_cov[i][1][1];
	}
	PROTECT(MKV_conv_names = allocVector(VECSXP,2));
	PROTECT(MKV_conv_names_y = allocVector(STRSXP,7));
	for (i = 0; i < 7; i++)
		SET_STRING_ELT(MKV_conv_names_y, i, mkChar(cMKV_conv[i]));
	SET_VECTOR_ELT(MKV_conv_names, 1, MKV_conv_names_y);
	setAttrib(MKV_conv, R_DimNamesSymbol, MKV_conv_names);

	UNPROTECT(3);
	return MKV_conv;
}

Bool onSet_MKV_conv(SEXP MKV_conv) {
	SW_MARKOV *v = &SW_Markov;
	const int nitems = 7;
	int i;
	RealD *p_MKV_conv;

	MyFileName = SW_F_name(eMarkovCov);

	if ((nrows(MKV_conv) != MAX_WEEKS) && (ncols(MKV_conv) != nitems))
		return FALSE;
	p_MKV_conv = REAL(MKV_conv);
	for (i = 0; i < MAX_WEEKS; i++) {
		p_MKV_conv[i + MAX_WEEKS * 1] = v->u_cov[i][0];
		p_MKV_conv[i + MAX_WEEKS * 2] = v->u_cov[i][1];
		p_MKV_conv[i + MAX_WEEKS * 3] = v->v_cov[i][0][0];
		p_MKV_conv[i + MAX_WEEKS * 4] = v->v_cov[i][0][1];
		p_MKV_conv[i + MAX_WEEKS * 5] = v->v_cov[i][1][0];
		p_MKV_conv[i + MAX_WEEKS * 6] = v->v_cov[i][1][1];
	}
	return TRUE;
}

