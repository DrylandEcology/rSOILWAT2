/********************************************************/
/********************************************************/
/*  Source file: SW_Files.h
 *  Type: header
 *  Application: SOILWAT - soilwater dynamics simulator
 *  Purpose: Define anything needed to support Files.c
 *           including function declarations.
 *  History:
 *     (8/28/01) -- INITIAL CODING - cwb
 *     (1/24/02) -- added logfile entry and rearranged order.
 09/30/2011	(drs)	added function SW_WeatherPrefix()
 09/30/2011	(drs)	added function SW_OutputPrefix()
 */
/********************************************************/
/********************************************************/
#ifndef SW_FILES_H
#define SW_FILES_H

#define SW_NFILES 15

#ifdef RSOILWAT
#include <R.h>
#include <Rdefines.h>
#include <Rconfig.h>
#include <Rinterface.h>
#include <Rinternals.h>
#endif

/* The number of enum elements between eNoFile and
 * eEndFile (not inclusive) must match SW_NFILES.
 * also, these elements must match the order of
 * input from files.in.
 */
typedef enum {
	eNoFile = -1, eFirst = 0, eModel, eLog, eSite, eLayers, eWeather, eMarkovProb, eMarkovCov, eSky, eVegProd, eVegEstab, eSoilwat, eOutput, eEndFile
} SW_FileIndex;

void SW_F_read(const char *s);
char *SW_F_name(SW_FileIndex i);
void SW_F_construct(const char *firstfile);

#ifdef RSOILWAT
SEXP onGet_SW_F();
void onSet_SW_F(SEXP SW_F_construct);
#endif

void SW_WeatherPrefix(char prefix[]);
void SW_OutputPrefix(char prefix[]);

#ifdef DEBUG_MEM
void SW_F_SetMemoryRefs(void);
#endif

#endif
