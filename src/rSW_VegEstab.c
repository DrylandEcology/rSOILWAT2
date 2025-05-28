/********************************************************/
/********************************************************/
/*	Source file: Veg_Estab.c
 Type: module
 Application: SOILWAT - soilwater dynamics simulator
 Purpose: Reads/writes vegetation establishment info.
 */
/********************************************************/
/********************************************************/

/* =================================================== */
/* =================================================== */
/*                INCLUDES / DEFINES                   */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "SOILWAT2/include/generic.h"
#include "SOILWAT2/include/filefuncs.h"
#include "SOILWAT2/include/Times.h"
#include "SOILWAT2/include/myMemory.h"

#include "SOILWAT2/include/SW_Defines.h"
#include "SOILWAT2/include/SW_Files.h"

#include "SOILWAT2/include/SW_VegEstab.h"
#include "rSW_VegEstab.h"
#include "SW_R_lib.h"

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>



/* =================================================== */
/*                  Local Variables                    */
/* --------------------------------------------------- */



/* =================================================== */
/*             Global Function Definitions             */
/* --------------------------------------------------- */

SEXP onGet_SW_VES(void) {
	SEXP swEstab;
	SEXP VES;
	SEXP use;
	SEXP count;

	PROTECT(swEstab = MAKE_CLASS("swEstab"));
	PROTECT(VES = NEW_OBJECT(swEstab));

	PROTECT(count = NEW_INTEGER(1));
	INTEGER(count)[0] = SoilWatRun.VegEstabIn.count;

	PROTECT(use = NEW_LOGICAL(1));
	LOGICAL(use)[0] = SoilWatRun.VegEstabIn.use;

	SET_SLOT(VES, install("count"), count);
	SET_SLOT(VES, install("useEstab"), use);

	if (SoilWatRun.VegEstabIn.use) {
		onGet_SW_VES_spps(VES);
	}

	UNPROTECT(4);
	return VES;
}

// see SW_VES_read2()
void onSet_SW_VES(SEXP VES, LOG_INFO* LogInfo) {
    IntU i;
    SEXP use, count;

    // Clean out and allocate memory
    SW_VES_deconstruct(
        SoilWatRun.VegEstabIn.count,
        SoilWatRun.ves_p_accu,
        SoilWatRun.ves_p_oagg
    );
    SW_VES_construct(
        &SoilWatRun.VegEstabIn,
        &SoilWatRun.VegEstabSim,
        SoilWatRun.ves_p_oagg,
        SoilWatRun.ves_p_accu
    );
    if(LogInfo->stopRun) {
        return; // Exit function prematurely due to error
    }

    SoilWatRun.VegEstabIn.use = TRUE;


    // Get rSOILWAT2 inputs: use flag and count of species
    PROTECT(use = GET_SLOT(VES, install("useEstab")));
    PROTECT(count = GET_SLOT(VES, install("count")));


    if (LOGICAL(use)[0] == FALSE) {
        SoilWatRun.VegEstabIn.use = FALSE;

    } else {
        SoilWatRun.VegEstabIn.count = INTEGER(count)[0];

        if (SoilWatRun.VegEstabIn.count == 0) {
            LogError(LogInfo, LOGWARN, "Establishment is TRUE but no data. Setting False.");
            SoilWatRun.VegEstabIn.use = FALSE;

        } else {
            for (i = 0; i < SoilWatRun.VegEstabIn.count; i++) {
                onSet_SW_VES_spp(VES, i, LogInfo);

                if (LogInfo->stopRun) {
                    goto report; // Exit function prematurely due to error
                }
              }
          }
    }

    SW_VegEstab_alloc_outptrs(
        SoilWatRun.ves_p_accu,
        SoilWatRun.ves_p_oagg,
        SoilWatRun.VegEstabIn.count,
        LogInfo
    );
    if(LogInfo->stopRun) {
        goto report; // Exit function prematurely due to error
    }

    if (EchoInits) {
        echo_VegEstab(
            SoilWatRun.RunIn.SoilRunIn.width,
            SoilWatRun.VegEstabIn.parms,
            SoilWatRun.VegEstabIn.count,
            LogInfo
        );
    }

    report: {
        UNPROTECT(2);
    }
}

void onGet_SW_VES_spps(SEXP SPP) {
	int i;
	SW_VEGESTAB_INFO_INPUTS *v;
    IntU vcount = SoilWatRun.VegEstabIn.count;
	SEXP fileName, name, vegType, estab_lyrs, barsGERM, barsESTAB, min_pregerm_days, max_pregerm_days, min_wetdays_for_germ, max_drydays_postgerm, min_wetdays_for_estab, min_days_germ2estab,
			max_days_germ2estab, min_temp_germ, max_temp_germ, min_temp_estab, max_temp_estab;

	PROTECT(fileName = allocVector(STRSXP, vcount));
	PROTECT(name = allocVector(STRSXP, vcount));
	PROTECT(vegType = NEW_INTEGER(vcount));
	PROTECT(estab_lyrs = NEW_INTEGER(vcount));
	PROTECT(barsGERM = allocVector(REALSXP, vcount));
	PROTECT(barsESTAB = allocVector(REALSXP, vcount));
	PROTECT(min_pregerm_days = NEW_INTEGER(vcount));
	PROTECT(max_pregerm_days = NEW_INTEGER(vcount));
	PROTECT(min_wetdays_for_germ = NEW_INTEGER(vcount));
	PROTECT(max_drydays_postgerm = NEW_INTEGER(vcount));
	PROTECT(min_wetdays_for_estab = NEW_INTEGER(vcount));
	PROTECT(min_days_germ2estab = NEW_INTEGER(vcount));
	PROTECT(max_days_germ2estab = NEW_INTEGER(vcount));
	PROTECT(min_temp_germ = NEW_NUMERIC(vcount));
	PROTECT(max_temp_germ = NEW_NUMERIC(vcount));
	PROTECT(min_temp_estab = NEW_NUMERIC(vcount));
	PROTECT(max_temp_estab = NEW_NUMERIC(vcount));

	for (i = 0; i < vcount; i++) {
		v = &SoilWatRun.VegEstabIn.parms[i];
		SET_STRING_ELT(fileName, i, mkChar(v->sppFileName));
		SET_STRING_ELT(name, i, mkChar(v->sppname));
		INTEGER(vegType)[i] = v->vegType;
		INTEGER(estab_lyrs)[i] = v->estab_lyrs;
		REAL(barsGERM)[i] = v->bars[0];
		REAL(barsESTAB)[i] = v->bars[1];
		INTEGER(min_pregerm_days)[i] = v->min_pregerm_days;
		INTEGER(max_pregerm_days)[i] = v->max_pregerm_days;
		INTEGER(min_wetdays_for_germ)[i] = v->min_wetdays_for_germ;
		INTEGER(max_drydays_postgerm)[i] = v->max_drydays_postgerm;
		INTEGER(min_wetdays_for_estab)[i] = v->min_wetdays_for_estab;
		INTEGER(min_days_germ2estab)[i] = v->min_days_germ2estab;
		INTEGER(max_days_germ2estab)[i] = v->max_days_germ2estab;
		REAL(min_temp_germ)[i] = v->min_temp_germ;
		REAL(max_temp_germ)[i] = v->max_temp_germ;
		REAL(min_temp_estab)[i] = v->min_temp_estab;
		REAL(max_temp_estab)[i] = v->max_temp_estab;
	}
	SET_SLOT(SPP, install("fileName"), fileName);
	SET_SLOT(SPP, install("Name"), name);
	SET_SLOT(SPP, install("vegType"), vegType);
	SET_SLOT(SPP, install("estab_lyrs"), estab_lyrs);
	SET_SLOT(SPP, install("barsGERM"), barsGERM);
	SET_SLOT(SPP, install("barsESTAB"), barsESTAB);
	SET_SLOT(SPP, install("min_pregerm_days"), min_pregerm_days);
	SET_SLOT(SPP, install("max_pregerm_days"), max_pregerm_days);
	SET_SLOT(SPP, install("min_wetdays_for_germ"), min_wetdays_for_germ);
	SET_SLOT(SPP, install("max_drydays_postgerm"), max_drydays_postgerm);
	SET_SLOT(SPP, install("min_wetdays_for_estab"), min_wetdays_for_estab);
	SET_SLOT(SPP, install("min_days_germ2estab"), min_days_germ2estab);
	SET_SLOT(SPP, install("max_days_germ2estab"), max_days_germ2estab);
	SET_SLOT(SPP, install("min_temp_germ"), min_temp_germ);
	SET_SLOT(SPP, install("max_temp_germ"), max_temp_germ);
	SET_SLOT(SPP, install("min_temp_estab"), min_temp_estab);
	SET_SLOT(SPP, install("max_temp_estab"), max_temp_estab);

	UNPROTECT(17);
}

void onSet_SW_VES_spp(SEXP SPP, IntU i, LOG_INFO* LogInfo) {
	SW_VEGESTAB_INFO_INPUTS *v;
	SEXP fileName, Name;

    if (i >= MAX_NSPECIES) {
        LogError(
            LogInfo,
            LOGERROR,
            "Too many species for establishment module "
            "(maximum = %d, requested = %d).",
            MAX_NSPECIES,
            i
        );
    }
    if(LogInfo->stopRun) {
        return; // Exit function prematurely due to error
    }

    v = &SoilWatRun.VegEstabIn.parms[i];

	v->vegType = INTEGER(GET_SLOT(SPP, install("vegType")))[i];
	v->estab_lyrs = INTEGER(GET_SLOT(SPP, install("estab_lyrs")))[i];
	v->bars[SW_GERM_BARS] = REAL(GET_SLOT(SPP, install("barsGERM")))[i];
	v->bars[SW_ESTAB_BARS] = REAL(GET_SLOT(SPP, install("barsESTAB")))[i];
	v->min_pregerm_days = INTEGER(GET_SLOT(SPP, install("min_pregerm_days")))[i];
	v->max_pregerm_days = INTEGER(GET_SLOT(SPP, install("max_pregerm_days")))[i];
	v->min_wetdays_for_germ = INTEGER(GET_SLOT(SPP, install("min_wetdays_for_germ")))[i];
	v->max_drydays_postgerm = INTEGER(GET_SLOT(SPP, install("max_drydays_postgerm")))[i];
	v->min_wetdays_for_estab = INTEGER(GET_SLOT(SPP, install("min_wetdays_for_estab")))[i];
	v->min_days_germ2estab = INTEGER(GET_SLOT(SPP, install("min_days_germ2estab")))[i];
	v->max_days_germ2estab = INTEGER(GET_SLOT(SPP, install("max_days_germ2estab")))[i];
	v->min_temp_germ = REAL(GET_SLOT(SPP, install("min_temp_germ")))[i];
	v->max_temp_germ = REAL(GET_SLOT(SPP, install("max_temp_germ")))[i];
	v->min_temp_estab = REAL(GET_SLOT(SPP, install("min_temp_estab")))[i];
	v->max_temp_estab = REAL(GET_SLOT(SPP, install("max_temp_estab")))[i];

	PROTECT(fileName = GET_SLOT(SPP, install("fileName")));
	PROTECT(Name = GET_SLOT(SPP, install("Name")));

	strcpy(v->sppFileName, CHAR(STRING_ELT(fileName, i)) );
	/* check for valid name first */
	if (strlen(CHAR(STRING_ELT(Name, i))) > MAX_SPECIESNAMELEN) {
		LogError(LogInfo, LOGERROR, "Species name too long (> 4 chars).");
	} else {
		strcpy(v->sppname, CHAR(STRING_ELT(Name, i)) );
	}
	UNPROTECT(2);
}
