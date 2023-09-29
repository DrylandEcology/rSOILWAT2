/********************************************************/
/********************************************************/
/*	Source file: SoilWater.c
 Type: module
 Application: SOILWAT - soilwater dynamics simulator
 Purpose: Read / write and otherwise manage the
 soil water values.  Includes reading input
 parameters and ordinary daily water flow.
 In addition, generally useful soilwater-
 related functions should go here.
 */
/********************************************************/
/********************************************************/

/* =================================================== */
/* =================================================== */
/*                INCLUDES / DEFINES                   */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include "SOILWAT2/include/generic.h"
#include "SOILWAT2/include/filefuncs.h"
#include "SOILWAT2/include/Times.h"
#include "SOILWAT2/include/myMemory.h"

#include "SOILWAT2/include/SW_Defines.h"
#include "SOILWAT2/include/SW_Files.h"
#include "SOILWAT2/include/SW_Model.h"
#include "SOILWAT2/include/SW_Site.h"

#include "SOILWAT2/include/SW_SoilWater.h"
#include "rSW_SoilWater.h"
#include "SW_R_lib.h" // externs `InputData`

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>


/* =================================================== */
/*                  Local Variables                    */
/* --------------------------------------------------- */
static char *MyFileName;
static int swcdataIndex;



/* =================================================== */
/*             Global Function Definitions             */
/* --------------------------------------------------- */

void rSW_SWC_construct(void) {
	swcdataIndex = 0;
}


SEXP onGet_SW_SWC(void) {
	SW_SOILWAT *v = &SoilWatAll.SoilWat;
	SEXP swSWC;
	SEXP SWC;
	char *cSWC[] = { "UseSWCHistoricData", "DataFilePrefix", "FirstYear", "Method", "History" };
	SEXP swcUseData;
	SEXP swcFilePrefix;
	SEXP swcFirstYear;
	SEXP swcMethod;

	PROTECT(swSWC = MAKE_CLASS("swSWC"));
	PROTECT(SWC = NEW_OBJECT(swSWC));

	PROTECT(swcUseData = NEW_LOGICAL(1));
	LOGICAL(swcUseData)[0] = v->hist_use;
	SET_SLOT(SWC, install(cSWC[0]), swcUseData);

	PROTECT(swcFilePrefix = NEW_CHARACTER(1));
	SET_STRING_ELT(swcFilePrefix, 0, mkChar("swcdata"));//v->hist.file_prefix)
	SET_SLOT(SWC, install(cSWC[1]), swcFilePrefix);

	PROTECT(swcFirstYear = NEW_INTEGER(1));
	INTEGER(swcFirstYear)[0] = v->hist.yr.first;
	SET_SLOT(SWC, install(cSWC[2]), swcFirstYear);

	PROTECT(swcMethod = NEW_INTEGER(1));
	INTEGER(swcMethod)[0] = v->hist.method;
	SET_SLOT(SWC, install(cSWC[3]), swcMethod);

	if(v->hist_use)
		SET_SLOT(SWC,install(cSWC[4]),onGet_SW_SWC_hists());
	else
		SET_SLOT(SWC,install(cSWC[4]),NEW_LIST(0));

	UNPROTECT(6);
	return SWC;
}

void onSet_SW_SWC(SEXP SWC) {
	SW_SOILWAT *v = &SoilWatAll.SoilWat;
	SEXP swcUseData;
	SEXP swcFilePrefix;
	SEXP swcFirstYear;
	SEXP swcMethod;

	MyFileName = PathInfo.InFiles[eSoilwat];
	LyrIndex i;
	ForEachSoilLayer(i, SoilWatAll.Site.n_layers)
		v->avgLyrTemp[i] = SoilWatAll.Site.avgLyrTempInit[i];

	PROTECT(swcUseData = GET_SLOT(SWC, install("UseSWCHistoricData")));
	PROTECT(swcFilePrefix = GET_SLOT(SWC, install("DataFilePrefix")));
	PROTECT(swcFirstYear = GET_SLOT(SWC, install("FirstYear")));
	PROTECT(swcMethod = GET_SLOT(SWC, install("Method")));

	v->hist_use = LOGICAL(swcUseData)[0];
	//if (!isnull(v->hist.file_prefix)) {//Clear memory before setting it
	//	Mem_Free(v->hist.file_prefix);
	//}
	v->hist.file_prefix = (char *) Str_Dup(CHAR(STRING_ELT(swcFilePrefix,0)), &LogInfo);
    if(LogInfo.stopRun) {
        UNPROTECT(4); // Unprotect the four protected variables before exiting
        return; // Exit function prematurely due to error
    }

	v->hist.yr.first = INTEGER(swcFirstYear)[0];
	v->hist.method = INTEGER(swcMethod)[0];

	if (v->hist.method < 1 || v->hist.method > 2) {
		LogError(&LogInfo, LOGERROR, "swcsetup.in : Invalid swc adjustment method.");

        UNPROTECT(4); // Unprotect the four protected variables before exiting
        return; // Exit function prematurely due to error
	}
	v->hist.yr.last = SoilWatAll.Model.endyr;
	v->hist.yr.total = v->hist.yr.last - v->hist.yr.first + 1;
	UNPROTECT(4);
}


SEXP onGet_SW_SWC_hists(void) {
	TimeInt year;
	SEXP SWC_hists, SWC_hists_names;
	int years = ((SoilWatAll.Model.endyr + 1) - SoilWatAll.Model.startyr), i = 0;
	char cYear[5];

	PROTECT(SWC_hists_names = allocVector(STRSXP, years));
	PROTECT(SWC_hists = allocVector(VECSXP,years));

	for (year = SoilWatAll.Model.startyr; year <= SoilWatAll.Model.endyr; year++) {
		if (SoilWatAll.SoilWat.hist_use && year >= SoilWatAll.SoilWat.hist.yr.first) {
			_read_swc_hist(&SoilWatAll.SoilWat.hist, year, &LogInfo);
            if(LogInfo.stopRun) {
                UNPROTECT(2); // Unprotect the two protected variables before exiting
                return NULL; // Exit function prematurely due to error
            }

			SET_VECTOR_ELT(SWC_hists, i, onGet_SW_SWC_hist(year));
			snprintf(cYear, sizeof cYear, "%4d", year);
			SET_STRING_ELT(SWC_hists_names, i, mkChar(cYear));
		}
		i++;
	}

	UNPROTECT(2);
	return SWC_hists;
}

SEXP onGet_SW_SWC_hist(TimeInt year) {
    LogError(&LogInfo, LOGERROR,
             "'onGet_SW_SWC_hist' is currently not functional.\n");
    return NULL; // Exit function prematurely due to error

	int i, j = 0;
	SW_SOILWAT *v = &SoilWatAll.SoilWat;
	SEXP swSWC_hist;
	SEXP hist;
	char *cSWC_hist[] = { "doy", "lyr", "swc", "st_err" };
	SEXP lyrs, lyrs_names, lyrs_names_y;
	RealD *p_lyrs;

	PROTECT(swSWC_hist = MAKE_CLASS("swSWC_hist"));
	PROTECT(hist = NEW_OBJECT(swSWC_hist));

	PROTECT(lyrs = allocMatrix(REALSXP, MAX_LAYERS*MAX_DAYS, 4));
	p_lyrs = REAL(lyrs);

  //TODO: variable j is used as index but not incremented
	for (i = 0; i < MAX_DAYS * MAX_LAYERS; i++) {
		p_lyrs[i + MAX_DAYS * MAX_LAYERS * 0] = (int) (i / MAX_LAYERS);
		p_lyrs[i + MAX_DAYS * MAX_LAYERS * 1] = (int) (j % MAX_LAYERS);
		p_lyrs[i + MAX_DAYS * MAX_LAYERS * 2] = v->hist.swc[(int) (i / MAX_LAYERS)][(int) (j % MAX_LAYERS)];
		p_lyrs[i + MAX_DAYS * MAX_LAYERS * 3] = v->hist.std_err[(int) (i / MAX_LAYERS)][(int) (j % MAX_LAYERS)];
	}
	PROTECT(lyrs_names = allocVector(VECSXP,2));
	PROTECT(lyrs_names_y = allocVector(STRSXP,4));
	for (i = 0; i < 4; i++)
		SET_STRING_ELT(lyrs_names_y, i, mkChar(cSWC_hist[i]));
	SET_VECTOR_ELT(lyrs_names, 1, lyrs_names_y);
	setAttrib(lyrs, R_DimNamesSymbol, lyrs_names);

	SET_SLOT(hist,install("data"),lyrs);

	UNPROTECT(5);
	return lyrs;
}

void onSet_SW_SWC_hist(void) {
    LogError(&LogInfo, LOGERROR,
             "'onSet_SW_SWC_hist' is currently not functional.\n");
    return; // Exit function prematurely due to error

	int i, j = 0;
	SW_SOILWAT *v = &SoilWatAll.SoilWat;
	RealD *p_lyrs;
	SEXP lyrs = VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(InputData,7),4),swcdataIndex);
	swcdataIndex++;

	p_lyrs = REAL(lyrs);
  //TODO: variable j is used as index but not incremented
	for (i = 0; i < MAX_DAYS * MAX_LAYERS; i++) {
		v->hist.swc[(int) (i / MAX_LAYERS)][(int) (j % MAX_LAYERS)] = p_lyrs[i + MAX_DAYS * MAX_LAYERS * 2];
		v->hist.std_err[(int) (i / MAX_LAYERS)][(int) (j % MAX_LAYERS)] = p_lyrs[i + MAX_DAYS * MAX_LAYERS * 3];
	}

}
