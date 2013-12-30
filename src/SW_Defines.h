/********************************************************/
/********************************************************/
/*  Source file: SW_Defines.h
 *  Type: header
 *  Application: SOILWAT - soilwater dynamics simulator
 *  Purpose: Define all the commonly used constants,
 *           looping constructs, and enumeration types
 *           that are used by most of the model code.
 *  History:
 *     (8/28/01) -- INITIAL CODING - cwb
	09/09/2011	(drs) added #define ForEachXXXTranspLayer() for each vegetation type (XXX = tree, shrub, grass)
	05/31/2012  (DLM) added MAX_ST_RGR definition for the maximum amount of soil temperature regressions allowed...
	07/09/2013	(clk) added the ForEachForbTranspLayer(i) function
 */
/********************************************************/
/********************************************************/

#ifndef SOILW_DEF_H
#define SOILW_DEF_H

#ifdef RSOILWAT
#include <R.h>
#include <Rdefines.h>
#include <Rconfig.h>
#include <Rinterface.h>
#include <Rinternals.h>
#endif
#include <math.h>  /* for atan() in tanfunc() below */

/* Not sure if this parameter is variable or a consequence of algebra,
 * but it's different in the FORTRAN version than in the ELM doc.
 * If deemed to need changing, might as well recompile rather than
 * confuse users with an unchanging parameter.
 */
#define SLOW_DRAIN_DEPTH 15. /* numerator over depth in slow drain equation */

/* some basic constants */
#define MAX_LAYERS  25
#define MAX_TRANSP_REGIONS 4
#define MAX_ST_RGR 30

#define SW_MISSING     999.     /* value to use as MISSING */

#ifndef PI
	#define PI          3.141592653589793238462643383279502884197169399375
#endif

#define PI2         6.28318530717958
#define BARCONV     1024.

//was 256 & 1024...
#define MAX_FILENAMESIZE 512
#define MAX_PATHSIZE 2048

/* this could be defined by STEPWAT */
#ifndef DFLT_FIRSTFILE
  #define DFLT_FIRSTFILE "files.in"
#endif

#ifndef STEPWAT
  #define MAX_SPECIESNAMELEN   4  /* for vegestab out of steppe-model context */
#endif

/* convenience indices to arrays in the model */
#define TWO_DAYS   2
#define SW_TOP 0
#define SW_BOT 1
#define SW_MIN 0
#define SW_MAX 1


/*------------ DON'T CHANGE ANYTHING BELOW THIS LINE ------------*/
/* Macros to simplify and add consistency to common tasks */
/* Note the loop var must be declared as LyrIndex */
#define ForEachSoilLayer(i)     for((i)=0; (i) < SW_Site.n_layers;      (i)++)
#define ForEachEvapLayer(i)     for((i)=0; (i) < SW_Site.n_evap_lyrs;   (i)++)
#define ForEachTreeTranspLayer(i)   for((i)=0; (i) < SW_Site.n_transp_lyrs_tree; (i)++)
#define ForEachShrubTranspLayer(i)   for((i)=0; (i) < SW_Site.n_transp_lyrs_shrub; (i)++)
#define ForEachGrassTranspLayer(i)   for((i)=0; (i) < SW_Site.n_transp_lyrs_grass; (i)++)
#define ForEachForbTranspLayer(i)   for((i)=0; (i) < SW_Site.n_transp_lyrs_forb; (i)++)
#define ForEachTranspRegion(r)  for((r)=0; (r) < SW_Site.n_transp_rgn;  (r)++)
/* define m as Months */
#define ForEachMonth(m)         for((m)=Jan; (m) <= Dec;  (m)++)

/* The ARCTANGENT function req'd by the original fortran produces
 * a highly configurable logistic curve. It was unfortunately
 * named tanfunc() in the original model, so I'm keeping it to
 * reduce confusion.  This is a very important function used in
 * lots of places.  It is described in detail in
 *    Parton, W.J., Innis, G.S.  1972 (July).  Some Graphs and Their
 *      Functional Forms.  U.S. International Biological Program,
 *      Grassland Biome, Tech. Rpt. No. 153.
 * The req'd parameters are (from Parton & Innis):
 *   z - the X variable
 *   a - X value of inflection point
 *   b - Y value of inflection point
 *   c - step size (diff of max point to min point)
 *   d - slope of line at inflection point
 */
#define tanfunc(z,a,b,c,d)  ((b)+((c)/PI)*atan(PI*(d)*((z)-(a))) )

/* To facilitate providing parameters to tanfunc() from the model,
 * this typedef can be used.  The parameters are analagous to a-d
 * above.  Some older C versions (of mine) name these differently
 * based on my own experiments with the behavior of the function
 * before I got the documentation.
 */
typedef struct { RealF xinflec, yinflec, range, slope; } tanfunc_t;


/* standardize the test for missing */
#define missing(x)  ( EQ(fabs( (x) ), SW_MISSING) )

/* types to identify the various modules/objects */
typedef enum { eF,   /* file management */
               eMDL, /* model */
               eWTH, /* weather */
               eSIT, /* site */
               eSWC, /* soil water */
               eVES, /* vegetation establishement */
               eVPD, /* vegetation production */
               eOUT  /* output */
} ObjType;

#endif
