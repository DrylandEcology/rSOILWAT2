/* rands.h -- contains some random number generators */
/* that could be useful in any program              */
/*
 * USES: rands.c
 *
 * REQUIRES: none
 */
/* Chris Bennett @ LTER-CSU 6/15/2000            */
/*    - 5/19/2001 - split from gen_funcs.c       */

#ifndef RANDS_H

#include <stdio.h>
#include <float.h>

#ifdef RSOILWAT
#include <R.h>
#include <Rdefines.h>
#include <Rconfig.h>
#include <Rinterface.h>
#include <Rinternals.h>
#endif

/***************************************************
 * Basic definitions
 ***************************************************/

/* You can choose to use a shuffled random set
 based on the compiler's rand() (RAND_FAST=1)
 or a compiler-independent version (RAND_FAST=0)
 but the speed of the "fast" version depends
 of course on the compiler.

 Some tests I ran with the GNU compiler from
 Cygwin (for Wintel) showed the FAST version to be
 better distributed, although the time was very
 nearly the same.  I'd suggest comparing the results
 of the two functions again if you use a different
 compiler.
 */
#define RAND_FAST 1
/* #define RAND_FAST 0 */

typedef long RandListType;

/***************************************************
 * Function definitions
 ***************************************************/

void RandSeed(signed long seed);
double RandUni_good(void);
double RandUni_fast(void);
int RandUniRange(const long first, const long last);
double RandNorm(double mean, double stddev);
void RandUniList(long, long, long, RandListType[]);

#if RAND_FAST
#define RandUni RandUni_fast
#else
#define RandUni RandUni_good
#endif

#define RANDS_H
#endif
