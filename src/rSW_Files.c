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

#include "SOILWAT2/include/generic.h"
#include "SOILWAT2/include/filefuncs.h"
#include "SOILWAT2/include/myMemory.h"
#include "SOILWAT2/include/SW_Defines.h"

#include "SOILWAT2/include/SW_Files.h"
#include "rSW_Files.h"

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>


/* =================================================== */
/*             Global Function Definitions             */
/* --------------------------------------------------- */

SEXP onGet_SW_F(void) {
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
	SET_STRING_ELT(ProjDir, 0, mkChar(PathInfo._ProjDir));

	PROTECT(FilesIn = allocVector(STRSXP, SW_NFILES));
	for (i = 0; i < SW_NFILES; i++) {
		if (PathInfo.InFiles[i] != NULL ) {
			SET_STRING_ELT(FilesIn, i, mkChar(PathInfo.InFiles[i]));
		}
	}

	PROTECT(Rweather_prefix = allocVector(STRSXP, 1));
	SET_STRING_ELT(Rweather_prefix, 0, mkChar(PathInfo.weather_prefix));
	PROTECT(Routput_prefix = allocVector(STRSXP, 1));
	SET_STRING_ELT(Routput_prefix, 0, mkChar(PathInfo.output_prefix));
	// attaching main's elements
	SET_SLOT(SW_F_construct, install(cSW_F_construct_names[0]), ProjDir);
	SET_SLOT(SW_F_construct, install(cSW_F_construct_names[1]), FilesIn);
	SET_SLOT(SW_F_construct, install(cSW_F_construct_names[2]), Rweather_prefix);
	SET_SLOT(SW_F_construct, install(cSW_F_construct_names[3]), Routput_prefix);

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
	strcpy(PathInfo._ProjDir, CHAR(STRING_ELT(ProjDir,0)));

	PROTECT(FilesIn = GET_SLOT(SW_F_construct, install("InFiles")));
	j = LENGTH(FilesIn);
	for(i=0;i<SW_NFILES;i++)
		if (!isnull(PathInfo.InFiles[i])) {
			Mem_Free(PathInfo.InFiles[i]);
		}
	for (i = 0; i < j; i++) {
		PathInfo.InFiles[i] = Str_Dup(CHAR(STRING_ELT(FilesIn,i)));
	}

	PROTECT(Rweather_prefix = GET_SLOT(SW_F_construct, install("WeatherPrefix")));
	strcpy(PathInfo.weather_prefix, CHAR(STRING_ELT(Rweather_prefix,0)));

	PROTECT(Routput_prefix = GET_SLOT(SW_F_construct, install("OutputPrefix")));
	strcpy(PathInfo.output_prefix, CHAR(STRING_ELT(Routput_prefix,0)));
	UNPROTECT(4);
}
