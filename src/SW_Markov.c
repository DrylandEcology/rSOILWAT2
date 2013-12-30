/********************************************************/
/********************************************************/
/*  Source file: Markov.c
 *  Type: module; used by Weather.c
 *
 *  Application: SOILWAT - soilwater dynamics simulator
 *  Purpose: Read / write and otherwise manage the markov
 *           weather generation information.
 *  History:
 *     (8/28/01) -- INITIAL CODING - cwb
 *    12/02 - IMPORTANT CHANGE - cwb
 *          refer to comments in Times.h regarding base0
 06/27/2013	(drs)	closed open files if LogError() with LOGFATAL is called in SW_MKV_read_prob(), SW_MKV_read_cov()
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
#include "generic.h"
#include "filefuncs.h"
#include "rands.h"
#include "myMemory.h"
#include "SW_Defines.h"
#include "SW_Files.h"
#include "SW_Weather.h"
#include "SW_Model.h"
#include "SW_Markov.h"

/* =================================================== */
/*                  Global Variables                   */
/* --------------------------------------------------- */
extern SW_MODEL SW_Model;
SW_MARKOV SW_Markov; /* declared here, externed elsewhere */

/* =================================================== */
/*                Module-Level Variables               */
/* --------------------------------------------------- */

static char *MyFileName;
static RealD _vcov[2][2], _ucov[2];

/* =================================================== */
/* =================================================== */
/*             Private Function Definitions            */
/* --------------------------------------------------- */

static void mvnorm(RealD *tmax, RealD *tmin) {
	/* --------------------------------------------------- */
	/* This proc is distilled from a much more general function
	 * in the original fortran version which was prepared to
	 * handle any number of variates.  In our case, there are
	 * only two, tmax and tmin, so there can be many fewer
	 * lines.  The purpose is to compute a random normal tmin
	 * value that covaries with tmax based on the covariance
	 * file read in at startup.
	 *
	 * cwb - 09-Dec-2002 -- This used to be two functions but
	 *       after some extensive debugging in this and the
	 *       RandNorm() function, it seems silly to maintain
	 *       the extra function call.
	 * cwb - 24-Oct-03 -- Note the switch to double (RealD).
	 *       C converts the floats transparently.
	 */
	RealD s, z1, z2, vc00 = _vcov[0][0], vc10 = _vcov[1][0], vc11 = _vcov[1][1];

	vc00 = sqrt(vc00);
	vc10 = (GT(vc00, 0.)) ? vc10 / vc00 : 0;
	s = vc10 * vc10;
	if (GT(s,vc11))
		LogError(logfp, LOGFATAL, "\nBad covariance matrix in mvnorm()");
	vc11 = (EQ(vc11, s)) ? 0. : sqrt(vc11 -s);

	z1 = RandNorm(0., 1.);
	z2 = RandNorm(0., 1.);
	*tmin = (vc10 * z1) + (vc11 * z2) + _ucov[1];
	*tmax = vc00 * z1 + _ucov[0];

}

/* =================================================== */
/* =================================================== */
/*             Public Function Definitions             */
/* --------------------------------------------------- */

void SW_MKV_construct(void) {
	/* =================================================== */
	SW_MARKOV *m = &SW_Markov;
	size_t s = sizeof(RealD);

	m->wetprob = (RealD *) Mem_Calloc(MAX_DAYS, s, "SW_MKV_construct");
	m->dryprob = (RealD *) Mem_Calloc(MAX_DAYS, s, "SW_MKV_construct");
	m->avg_ppt = (RealD *) Mem_Calloc(MAX_DAYS, s, "SW_MKV_construct");
	m->std_ppt = (RealD *) Mem_Calloc(MAX_DAYS, s, "SW_MKV_construct");
}

void SW_MKV_today(TimeInt doy, RealD *tmax, RealD *tmin, RealD *rain) {
	/* =================================================== */
	/* enter with rain == yesterday's ppt, doy as array index
	 * leave with rain == today's ppt
	 */
	TimeInt week;
	RealF prob, p, x;

	/* Calculate Precip */
	prob = (GT(*rain, 0.0)) ? SW_Markov.wetprob[doy] : SW_Markov.dryprob[doy];

	p = RandUni();
	if (LE(p,prob)) {
		x = RandNorm(SW_Markov.avg_ppt[doy], SW_Markov.std_ppt[doy]);
		*rain = max(0., x);
	} else {
		*rain = 0.;
	}

	if (!ZRO(*rain))
		SW_Markov.ppt_events++;

	/* Calculate temperature */
	week = Doy2Week(doy+1);
	memcpy(_vcov, &SW_Markov.v_cov[week], 4 * sizeof(RealD));
	_ucov[0] = SW_Markov.u_cov[week][0];
	_ucov[1] = SW_Markov.u_cov[week][1];
	mvnorm(tmax, tmin);

}

Bool SW_MKV_read_prob(void) {
	/* =================================================== */
	SW_MARKOV *v = &SW_Markov;
	const int nitems = 5;
	FILE *f;
	int lineno = 0, day, x;
	RealF wet, dry, avg, std;

	/* note that Files.read() must be called prior to this. */
	MyFileName = SW_F_name(eMarkovProb);

	if (NULL == (f = fopen(MyFileName, "r")))
		return FALSE;

	while (GetALine(f, inbuf)) {
		if (++lineno == MAX_DAYS)
			break; /* skip extra lines */

		x = sscanf(inbuf, "%d %f %f %f %f", &day, &wet, &dry, &avg, &std);
		if (x < nitems) {
			CloseFile(&f);
			LogError(logfp, LOGFATAL, "\nToo few values in line %d file %s\n", lineno, MyFileName);
		}
		day--;
		v->wetprob[day] = wet;
		v->dryprob[day] = dry;
		v->avg_ppt[day] = avg;
		v->std_ppt[day] = std;

	}
	CloseFile(&f);

	return TRUE;
}
#ifdef RSOILWAT
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
	RealF wet, dry, avg, std;
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
#endif

Bool SW_MKV_read_cov(void) {
	/* =================================================== */
	SW_MARKOV *v = &SW_Markov;
	const int nitems = 7;
	FILE *f;
	int lineno = 0, week, x;
	RealF t1, t2, t3, t4, t5, t6;

	MyFileName = SW_F_name(eMarkovCov);

	if (NULL == (f = fopen(MyFileName, "r")))
		return FALSE;

	while (GetALine(f, inbuf)) {
		if (++lineno == MAX_WEEKS)
			break; /* skip extra lines */

		x = sscanf(inbuf, "%d %f %f %f %f %f %f", &week, &t1, &t2, &t3, &t4, &t5, &t6);
		if (x < nitems) {
			CloseFile(&f);
			LogError(logfp, LOGFATAL, "\nToo few values in line %d file %s\n", lineno, MyFileName);
		}
		week--;
		v->u_cov[week][0] = t1;
		v->u_cov[week][1] = t2;
		v->v_cov[week][0][0] = t3;
		v->v_cov[week][0][1] = t4;
		v->v_cov[week][1][0] = t5;
		v->v_cov[week][1][1] = t6;
	}
	CloseFile(&f);

	return TRUE;
}
#ifdef RSOILWAT
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
	SEXP dim;

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
#endif

#ifdef DEBUG_MEM
#include "myMemory.h"
/*======================================================*/
void SW_MKV_SetMemoryRefs( void) {
	/* when debugging memory problems, use the bookkeeping
	 code in myMemory.c
	 This routine sets the known memory refs in this module
	 so they can be  checked for leaks, etc.  All refs will
	 have been cleared by a call to ClearMemoryRefs() before
	 this, and will be checked via CheckMemoryRefs() after
	 this, most likely in the main() function.
	 */

	NoteMemoryRef(SW_Markov.wetprob);
	NoteMemoryRef(SW_Markov.dryprob);
	NoteMemoryRef(SW_Markov.avg_ppt);
	NoteMemoryRef(SW_Markov.std_ppt);

}

#endif
