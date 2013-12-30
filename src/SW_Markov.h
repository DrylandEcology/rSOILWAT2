/********************************************************/
/********************************************************/
/*	Source file: SW_Markov.h
 Type: header
 Application: SOILWAT - soilwater dynamics simulator
 Purpose: Definitions for Markov-generated weather
 functions.
 History:
 (9/11/01) -- INITIAL CODING - cwb
 */
/********************************************************/
/********************************************************/
#ifndef SW_MARKOV_H
#define SW_MARKOV_H
#ifdef RSOILWAT
#include "SW_Times.h"
#include <R.h>
#include <Rdefines.h>
#include <Rconfig.h>
#include <Rinterface.h>
#include <Rinternals.h>
#endif
typedef struct {

	/* pointers to arrays of probabilities for each day saves some space */
	/* by not being allocated if markov weather not requested by user */
	/* alas, multi-dimensional arrays aren't so convenient */
	RealD *wetprob, /* probability of being wet today */
	*dryprob, /* probability of being dry today */
	*avg_ppt, /* mean precip (cm) */
	*std_ppt, /* std dev. for precip */
	u_cov[MAX_WEEKS][2], /* mean temp (max, min) Celsius */
	v_cov[MAX_WEEKS][2][2]; /* covariance matrix */
	int ppt_events; /* number of ppt events generated this year */

} SW_MARKOV;

void SW_MKV_construct(void);
Bool SW_MKV_read_prob(void);
Bool SW_MKV_read_cov(void);
void SW_MKV_today(TimeInt doy, RealD *tmax, RealD *tmin, RealD *rain);

#ifdef RSOILWAT
SEXP onGet_MKV(void);
SEXP onGet_MKV_prob(void);
Bool onSet_MKV_prob(SEXP MKV_prob);
SEXP onGet_MKV_conv(void);
Bool onSet_MKV_conv(SEXP MKV_conv);
#endif

#ifdef DEBUG_MEM
void SW_MKV_SetMemoryRefs( void);
#endif

#endif
