/********************************************************/
/********************************************************/
/*	Source file: Files.c
 Type: module
 Application: SOILWAT - soilwater dynamics simulator
 Purpose: Read / write and otherwise manage the model's
 parameter file information.
 */
/********************************************************/
/********************************************************/

/* =================================================== */
/* =================================================== */
/*                INCLUDES / DEFINES                   */

#include <string.h>
#include <stdio.h>  // for `FILENAME_MAX`
#include <stdlib.h>

#include "SOILWAT2/generic.h"
#include "SOILWAT2/filefuncs.h"
#include "SOILWAT2/myMemory.h"
#include "SOILWAT2/SW_Defines.h"

#include "SOILWAT2/SW_Files.h"
#include "rSW_Files.h"

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>


/* =================================================== */
/*                  Global Variables                   */
/* --------------------------------------------------- */

/* =================================================== */
/*                Module-Level Variables               */
/* --------------------------------------------------- */
extern char *InFiles[SW_NFILES];
extern char _ProjDir[FILENAME_MAX];
extern char weather_prefix[FILENAME_MAX];
extern char output_prefix[FILENAME_MAX];

/* =================================================== */
/* =================================================== */
/*             Private Function Definitions            */
/* --------------------------------------------------- */

SEXP onGet_SW_F() {
	int i = 0;

	SEXP swFiles;
	SEXP SW_F_construct; //, SW_F_construct_names;
	SEXP ProjDir;
	SEXP FilesIn;
	SEXP Rweather_prefix;
	SEXP Routput_prefix;
	char *cSW_F_construct_names[] = { "ProjDir", "InFiles", "WeatherPrefix", "OutputPrefix" };

	PROTECT(swFiles = MAKE_CLASS("swFiles"));
	PROTECT(SW_F_construct = NEW_OBJECT(swFiles));
	PROTECT(ProjDir = allocVector(STRSXP, 1));
	SET_STRING_ELT(ProjDir, 0, mkChar(_ProjDir));

	PROTECT(FilesIn = allocVector(STRSXP, SW_NFILES));
	for (i = 0; i < SW_NFILES; i++) {
		if (InFiles[i] != NULL ) {
			SET_STRING_ELT(FilesIn, i, mkChar(InFiles[i]));
		}
	}

	PROTECT(Rweather_prefix = allocVector(STRSXP, 1));
	SET_STRING_ELT(Rweather_prefix, 0, mkChar(weather_prefix));
	PROTECT(Routput_prefix = allocVector(STRSXP, 1));
	SET_STRING_ELT(Routput_prefix, 0, mkChar(output_prefix));
	// attaching main's elements
	SET_SLOT(SW_F_construct, install(cSW_F_construct_names[0]), ProjDir);
	SET_SLOT(SW_F_construct, install(cSW_F_construct_names[1]), FilesIn);
	SET_SLOT(SW_F_construct, install(cSW_F_construct_names[2]), Rweather_prefix);
	SET_SLOT(SW_F_construct, install(cSW_F_construct_names[3]), Routput_prefix);
	//PROTECT(SW_F_construct_names = allocVector(STRSXP, 4));
	//for (i = 0; i < 4; i++)
	//	SET_STRING_ELT(SW_F_construct_names, i, mkChar(cSW_F_construct_names[i]));
	//setAttrib(SW_F_construct, R_NamesSymbol, SW_F_construct_names);

	UNPROTECT(6);
	return SW_F_construct;
}

void onSet_SW_F(SEXP SW_F_construct) {
	int i, j;
	SEXP ProjDir;
	SEXP FilesIn;
	SEXP Rweather_prefix;
	SEXP Routput_prefix;

	PROTECT(ProjDir = GET_SLOT(SW_F_construct, install("ProjDir")));
	//_ProjDir = R_alloc(strlen(CHAR(STRING_ELT(ProjDir,0))));
	strcpy(_ProjDir, CHAR(STRING_ELT(ProjDir,0)));

	PROTECT(FilesIn = GET_SLOT(SW_F_construct, install("InFiles")));
	j = LENGTH(FilesIn);
	for(i=0;i<SW_NFILES;i++)
		if (!isnull(InFiles[i])) {
			Mem_Free(InFiles[i]);
		}
	for (i = 0; i < j; i++) {
		InFiles[i] = Str_Dup(CHAR(STRING_ELT(FilesIn,i)));
	}

	PROTECT(Rweather_prefix = GET_SLOT(SW_F_construct, install("WeatherPrefix")));
	//weather_prefix = R_alloc(strlen(CHAR(STRING_ELT(Rweather_prefix,0))));
	strcpy(weather_prefix, CHAR(STRING_ELT(Rweather_prefix,0)));

	PROTECT(Routput_prefix = GET_SLOT(SW_F_construct, install("OutputPrefix")));
	//output_prefix = R_alloc(strlen(CHAR(STRING_ELT(Routput_prefix,0))));
	strcpy(output_prefix, CHAR(STRING_ELT(Routput_prefix,0)));
	UNPROTECT(4);
}
