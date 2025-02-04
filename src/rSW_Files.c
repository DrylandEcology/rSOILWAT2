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
#include "SW_R_lib.h"

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
	SET_STRING_ELT(ProjDir, 0, mkChar(SoilWatDomain.SW_PathInputs.SW_ProjDir));

	PROTECT(FilesIn = allocVector(STRSXP, SW_NFILES));
	for (i = 0; i < SW_NFILES; i++) {
		if (SoilWatDomain.SW_PathInputs.txtInFiles[i] != NULL ) {
			SET_STRING_ELT(FilesIn, i, mkChar(SoilWatDomain.SW_PathInputs.txtInFiles[i]));
		}
	}

	PROTECT(Rweather_prefix = allocVector(STRSXP, 1));
	SET_STRING_ELT(Rweather_prefix, 0, mkChar(SoilWatDomain.SW_PathInputs.txtWeatherPrefix));
	PROTECT(Routput_prefix = allocVector(STRSXP, 1));
	SET_STRING_ELT(Routput_prefix, 0, mkChar(SoilWatDomain.SW_PathInputs.outputPrefix));
	// attaching main's elements
	SET_SLOT(SW_F_construct, install(cSW_F_construct_names[0]), ProjDir);
	SET_SLOT(SW_F_construct, install(cSW_F_construct_names[1]), FilesIn);
	SET_SLOT(SW_F_construct, install(cSW_F_construct_names[2]), Rweather_prefix);
	SET_SLOT(SW_F_construct, install(cSW_F_construct_names[3]), Routput_prefix);

	UNPROTECT(6);
	return SW_F_construct;
}

void onSet_SW_F(SEXP SW_F_construct, LOG_INFO* LogInfo) {
	int i, j;
	SEXP ProjDir;
	SEXP FilesIn;
	SEXP Rweather_prefix;
	SEXP Routput_prefix;

	PROTECT(ProjDir = GET_SLOT(SW_F_construct, install("ProjDir")));
	strcpy(SoilWatDomain.SW_PathInputs.SW_ProjDir, CHAR(STRING_ELT(ProjDir,0)));

	PROTECT(FilesIn = GET_SLOT(SW_F_construct, install("InFiles")));
	j = LENGTH(FilesIn);
	for(i=0;i<SW_NFILES;i++)
		if (!isnull(SoilWatDomain.SW_PathInputs.txtInFiles[i])) {
			free(SoilWatDomain.SW_PathInputs.txtInFiles[i]);
		}
	for (i = 0; i < j; i++) {
		// txtInFiles is unused if values set by rSOILWAT2
		// SoilWatDomain.SW_PathInputs.txtInFiles[i] = Str_Dup(CHAR(STRING_ELT(FilesIn,i)), LogInfo);
		SoilWatDomain.SW_PathInputs.txtInFiles[i] = NULL;
        if(LogInfo->stopRun) {
            UNPROTECT(2); // Unprotect the two protected variables before exiting
            return; // Exit function prematurely
        }
	}

	PROTECT(Rweather_prefix = GET_SLOT(SW_F_construct, install("WeatherPrefix")));
	strcpy(SoilWatDomain.SW_PathInputs.txtWeatherPrefix, CHAR(STRING_ELT(Rweather_prefix,0)));

	PROTECT(Routput_prefix = GET_SLOT(SW_F_construct, install("OutputPrefix")));
	strcpy(SoilWatDomain.SW_PathInputs.outputPrefix, CHAR(STRING_ELT(Routput_prefix,0)));
	UNPROTECT(4);
}
