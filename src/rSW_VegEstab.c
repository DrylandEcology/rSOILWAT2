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

#include "SOILWAT2/generic.h" // externs `EchoInits`
#include "SOILWAT2/filefuncs.h"
#include "SOILWAT2/Times.h"
#include "SOILWAT2/myMemory.h"

#include "SOILWAT2/SW_Defines.h"
#include "SOILWAT2/SW_Files.h"

#include "SOILWAT2/SW_VegEstab.h" // externs `SW_VegEstab`
#include "rSW_VegEstab.h"

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>



/* =================================================== */
/*                  Local Variables                    */
/* --------------------------------------------------- */
static char *MyFileName;



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
	INTEGER(count)[0] = SW_VegEstab.count;

	PROTECT(use = NEW_LOGICAL(1));
	LOGICAL(use)[0] = SW_VegEstab.use;

	SET_SLOT(VES, install("count"), count);
	SET_SLOT(VES, install("useEstab"), use);

	if (SW_VegEstab.use) {
		onGet_SW_VES_spps(VES);
	}

	UNPROTECT(4);
	return VES;
}

void onSet_SW_VES(SEXP VES) {
	IntU i;
	int nSPPS;
	SW_VegEstab.use = TRUE;
	SEXP use, count;
	MyFileName = SW_F_name(eVegEstab);

	PROTECT(use = GET_SLOT(VES,install("useEstab")));
	PROTECT(count = GET_SLOT(VES,install("count")));

	if (LOGICAL(use)[0] == FALSE) {
		//LogError(logfp, LOGNOTE, "Establishment not used.\n");
		SW_VegEstab.use = FALSE;
	} else {
		nSPPS = INTEGER(count)[0];
		if (nSPPS == 0) {
			LogError(logfp, LOGWARN, "Establishment is TRUE but no data. Setting False.");
			SW_VegEstab.use = FALSE;
		} else {
			SW_VegEstab.use = TRUE;
			for (i = 0; i < nSPPS; i++)
				onSet_SW_VES_spp(VES, i); // sets `SW_VegEstab.count` incrementally
		}
	}

	if (EchoInits)
		LogError(logfp, LOGNOTE, "Establishment not used.\n");

	SW_VegEstab_construct();

	if (EchoInits)
		_echo_VegEstab();

	UNPROTECT(2);
}

void onGet_SW_VES_spps(SEXP SPP) {
	int i;
	SW_VEGESTAB_INFO *v;
	SEXP fileName, name, vegType, estab_lyrs, barsGERM, barsESTAB, min_pregerm_days, max_pregerm_days, min_wetdays_for_germ, max_drydays_postgerm, min_wetdays_for_estab, min_days_germ2estab,
			max_days_germ2estab, min_temp_germ, max_temp_germ, min_temp_estab, max_temp_estab;

	PROTECT(fileName = allocVector(STRSXP,SW_VegEstab.count));
	PROTECT(name = allocVector(STRSXP,SW_VegEstab.count));
	PROTECT(vegType = NEW_INTEGER(SW_VegEstab.count));
	PROTECT(estab_lyrs = NEW_INTEGER(SW_VegEstab.count));
	PROTECT(barsGERM = allocVector(REALSXP,SW_VegEstab.count));
	PROTECT(barsESTAB = allocVector(REALSXP,SW_VegEstab.count));
	PROTECT(min_pregerm_days = NEW_INTEGER(SW_VegEstab.count));
	PROTECT(max_pregerm_days = NEW_INTEGER(SW_VegEstab.count));
	PROTECT(min_wetdays_for_germ = NEW_INTEGER(SW_VegEstab.count));
	PROTECT(max_drydays_postgerm = NEW_INTEGER(SW_VegEstab.count));
	PROTECT(min_wetdays_for_estab = NEW_INTEGER(SW_VegEstab.count));
	PROTECT(min_days_germ2estab = NEW_INTEGER(SW_VegEstab.count));
	PROTECT(max_days_germ2estab = NEW_INTEGER(SW_VegEstab.count));
	PROTECT(min_temp_germ = NEW_NUMERIC(SW_VegEstab.count));
	PROTECT(max_temp_germ = NEW_NUMERIC(SW_VegEstab.count));
	PROTECT(min_temp_estab = NEW_NUMERIC(SW_VegEstab.count));
	PROTECT(max_temp_estab = NEW_NUMERIC(SW_VegEstab.count));

	for (i = 0; i < SW_VegEstab.count; i++) {
		v = SW_VegEstab.parms[i];
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

void onSet_SW_VES_spp(SEXP SPP, IntU i) {
	SW_VEGESTAB_INFO *v;
	SEXP fileName, Name;
	unsigned int count;

	count = _new_species();
	v = SW_VegEstab.parms[count];

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

	strcpy(v->sppFileName, CHAR(STRING_ELT(fileName,i)) );
	/* check for valid name first */
	if (strlen(CHAR(STRING_ELT(Name,i))) > MAX_SPECIESNAMELEN) {
		LogError(logfp, LOGFATAL, "Species name too long (> 4 chars).\n\tTry again.\n");
	} else {
		strcpy(v->sppname, CHAR(STRING_ELT(Name,i)) );
	}
	UNPROTECT(2);
}
