/********************************************************/
/********************************************************/
/*	Source file: Veg_Estab.h
	Type: header
	Application: SOILWAT - soilwater dynamics simulator
	Purpose: Supports Veg_Estab.c routines.
	History:
	(8/28/01) -- INITIAL CODING - cwb
	
	currently not used.
*/
/********************************************************/
/********************************************************/

#ifndef SW_VEGESTAB_H
#define SW_VEGESTAB_H

#include "SW_Defines.h"
#include "SW_Times.h"
#ifdef RSOILWAT
#include <R.h>
#include <Rdefines.h>
#include <Rconfig.h>
#include <Rinterface.h>
#include <Rinternals.h>
#endif
#ifdef STEPWAT
	#include "ST_defines.h"
#endif

/* indices to bars[] */
#define SW_GERM_BARS 0
#define SW_ESTAB_BARS 1

typedef struct {

	/* see COMMENT-1 below for more information on these vars */

	/* THESE VARIABLES CAN CHANGE VALUE IN THE MODEL */
	TimeInt estab_doy, /* day of establishment for this plant */
			germ_days, /* elapsed days since germination with no estab */
			drydays_postgerm, /* did sprout get too dry for estab? */
			wetdays_for_germ, /* keep track of consecutive wet days */
			wetdays_for_estab;
	Bool germd, /* has this plant germinated yet?  */
		no_estab; /* if TRUE, can't attempt estab for remainder of year */

	/* THESE VARIABLES DO NOT CHANGE DURING THE NORMAL MODEL RUN */
	char sppFileName[MAX_FILENAMESIZE]; /* Store the file Name and Path, Mostly for Rsoilwat */
	char sppname[MAX_SPECIESNAMELEN + 1]; /* one set of parms per species */
	TimeInt min_pregerm_days, /* first possible day of germination */
			max_pregerm_days, /* last possible day of germination */
			min_wetdays_for_germ, 	/* number of consecutive days top layer must be */
									/* "wet" in order for germination to occur. */
			max_drydays_postgerm, 	/* maximum number of consecutive dry days after */
									/* germination before establishment can no longer occur. */
			min_wetdays_for_estab, 	/* minimum number of consecutive days the top layer */
									/* must be "wet" in order to establish */
			min_days_germ2estab, 	/* minimum number of days to wait after germination */
									/* and seminal roots wet before check for estab. */
			max_days_germ2estab; 	/* maximum number of days after germination to wait */
									/* for establishment */

	unsigned int estab_lyrs;	/* estab could conceivably need more than one layer */
								/* swc is averaged over these top layers to compare to */
								/* the converted value from min_swc_estab */
	RealF bars[2], 			/* read from input, saved for reporting */
			min_swc_germ, 	/* wetting point required for germination converted from */
							/* bars to cm per layer for efficiency in the loop */
			min_swc_estab, 	/* same as min_swc_germ but for establishment */
							/* this is the average of the swc of the first estab_lyrs */
			min_temp_germ, 	/* min avg daily temp req't for germination */
			max_temp_germ, 	/* max temp for germ in degC */
			min_temp_estab, /* min avg daily temp req't for establishment */
			max_temp_estab; /* max temp for estab in degC */

} SW_VEGESTAB_INFO;

typedef struct {
	TimeInt *days;	/* only output the day of estab for each species in the input */
					/* this array is allocated via SW_VES_Init() */
					/* each day in the array corresponds to the ordered species list */
} SW_VEGESTAB_OUTPUTS;

typedef struct {
  Bool use;      /* if swTRUE use establishment parms and chkestab() */
  IntU count;  /* number of species to check */
  SW_VEGESTAB_INFO **parms;  /* dynamic array of parms for each species */
  SW_VEGESTAB_OUTPUTS yrsum,  /* conforms to the requirements of the output module */
                      yravg;  /* note that there's only one period for output */
                              /* see also the soilwater and weather modules */
} SW_VEGESTAB;

void SW_VES_read(void);
void SW_VES_construct(void);
void SW_VES_clear(void);
void SW_VES_init(void);
void SW_VES_checkestab(void);
void SW_VES_new_year(void);

#ifdef RSOILWAT
SEXP onGet_SW_VES(void);
void onSet_SW_VES(SEXP VES);
void onGet_SW_VES_spps(SEXP SPP);
void onSet_SW_VES_spp(SEXP SPP, IntU i);
#endif

/* COMMENT-1
 * There are a lot of things to keep track of during the period of
 * possible germination to possible establishment, and the original
 * fortran variables were badly named (effect of 8 char limit?)
 * Here are the current variables and their fortran counterparts
 * in case somebody has to go back to the old code.


 max_drydays_postgerm          ------  numwait
 min_days_germ2estab           ------  minint
 max_days_germ2estab           ------  maxint
 wet_days_before_germ         ------  mingdys
 max_days_4germ                ------  maxgwait
 num_wet_days_for_estab        ------  nestabdy
 num_wet_days_for_germ         ------  ngermdy

 min_temp_germ                 ------  tmpmin
 max_temp_germ                 ------  tmpmax

 wet_swc_germ[]                ------  wetger
 wet_swc_estab[]               ------  wetest
 roots_wet                     ------  rootswet

 */

#endif
