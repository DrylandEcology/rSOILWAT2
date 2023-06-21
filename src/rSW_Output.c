/********************************************************/
/********************************************************/
/*	Source file: Output.c
 Type: module
 Application: SOILWAT - soilwater dynamics simulator
 Purpose: Read / write and otherwise manage the
 user-specified output flags.
 */
/********************************************************/
/********************************************************/

/* =================================================== */
/*                INCLUDES / DEFINES                   */
/* --------------------------------------------------- */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "SOILWAT2/include/generic.h"
#include "SOILWAT2/include/filefuncs.h"
#include "SOILWAT2/include/Times.h"
#include "SOILWAT2/include/myMemory.h"

#include "SOILWAT2/include/SW_Defines.h"
#include "SOILWAT2/include/SW_Files.h"
#include "SOILWAT2/include/SW_Site.h"

#include "SOILWAT2/include/SW_Output.h" // externs many variables
#include "SOILWAT2/include/SW_Output_outarray.h" // for function `SW_OUT_set_nrow`
#include "rSW_Output.h"
#include "SW_R_lib.h"

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>



/* =================================================== */
/*                Module-Level Variables               */
/* --------------------------------------------------- */
static char *MyFileName;


/* =================================================== */
/*             Global Function Definitions             */
/* --------------------------------------------------- */

/** Copies values from parameter `OUT` of rSOILWAT2 S4 class `swOUT` to SOILWAT2 variables
	"Output": Updated global variables SW_Output, used_OUTNPERIODS, and timeSteps which are
		defined in SW_Output.c
	*/
void onSet_SW_OUT(SEXP OUT) {
	int i, msg_type;
	OutKey k;
	SEXP sep, outfile, tp_convert;
	int *timePeriods, *sumtype, *first_orig, *last_orig;
	// mykey, myobj and use are currently unused:
	// int *use, *mykey, *myobj;
	char msg[200]; // message to print
	#ifdef RSWDEBUG
	int debug = 0;
	#endif

	#ifdef RSWDEBUG
	if (debug) swprintf("onSet_SW_OUT: start ...");
	#endif
	MyFileName = PathInfo.InFiles[eOutput];

	PROTECT(sep = GET_SLOT(OUT, install("outputSeparator")));
	_Sep = '\t';/*TODO Make this work.*/

	// TODO: I don't know why `GET_SLOT(OUT, install("timeSteps"))` is suddenly
	// of type real and not integer any more
	PROTECT(tp_convert = coerceVector(GET_SLOT(OUT, install("timeSteps")), INTSXP));
	timePeriods = INTEGER(tp_convert);
	SoilWatAll.GenOutput.used_OUTNPERIODS = INTEGER(GET_DIM(GET_SLOT(OUT, install("timeSteps"))))[1]; // number of columns

	// mykey, myobj and use are currently unused:
	// mykey = INTEGER(GET_SLOT(OUT, install("mykey")));
	// myobj = INTEGER(GET_SLOT(OUT, install("myobj")));
	// use = LOGICAL(GET_SLOT(OUT, install("use")));
	sumtype = INTEGER(GET_SLOT(OUT, install("sumtype")));
	first_orig = INTEGER(GET_SLOT(OUT, install("first_orig")));
	last_orig = INTEGER(GET_SLOT(OUT, install("last_orig")));
	PROTECT(outfile = GET_SLOT(OUT, install("outfile")));

	ForEachOutKey(k) {
		msg_type = SW_OUT_read_onekey(
			k,
			sumtype[k],
			first_orig[k],
			last_orig[k],
			msg,
			sizeof msg
		);

		if (msg_type > 0) {
			LogError(&LogInfo, msg_type, "%s", msg);
			continue;
		}

		if (SoilWatAll.Output[k].use) {
			SoilWatAll.Output[k].outfile = Str_Dup(CHAR(STRING_ELT(outfile, k)));

			ForEachOutPeriod(i) {
				SoilWatAll.GenOutput.timeSteps[k][i] = timePeriods[k + i * SW_OUTNKEYS];
			}
		}
	}

	if (EchoInits)
		_echo_outputs();

	UNPROTECT(3);

	#ifdef RSWDEBUG
	if (debug) swprintf(" ... done. \n");
	#endif
}


/** Copies values from global SOILWAT2 variables (SW_Output, used_OUTNPERIODS, and timeSteps)
		@return `OUT` of rSOILWAT2 S4 class `swOUT`
	*/
SEXP onGet_SW_OUT(void) {
	int i;
	int *vtimePeriods;
	OutKey k;
	SEXP swOUT, OUT, sep, timePeriods;
	SEXP mykey, myobj, sumtype, use, first_orig, last_orig, outfile;
	char *cKEY[] = {"mykey", "myobj", "sumtype", "use", "first_orig", "last_orig", "outfile"};
  #ifdef RSWDEBUG
  int debug = 0;
  #endif

	#ifdef RSWDEBUG
	if (debug) swprintf("onGet_SW_OUT: start ...");
	#endif

	PROTECT(swOUT = MAKE_CLASS("swOUT"));
	PROTECT(OUT = NEW_OBJECT(swOUT));

	PROTECT(sep = NEW_STRING(1));
	SET_STRING_ELT(sep, 0, mkCharLen(&_Sep, 1));

	PROTECT(timePeriods = allocMatrix(INTSXP, SW_OUTNKEYS, SW_OUTNPERIODS));
	vtimePeriods = INTEGER(timePeriods);
	for (i = 0; i < SW_OUTNKEYS * SW_OUTNPERIODS; i++)
	{
		vtimePeriods[i] = eSW_NoTime; // `allocMatrix` does not initialize
	}

	PROTECT(mykey = NEW_INTEGER(SW_OUTNKEYS));
	PROTECT(myobj = NEW_INTEGER(SW_OUTNKEYS));
	PROTECT(sumtype = NEW_INTEGER(SW_OUTNKEYS));
	PROTECT(use = NEW_LOGICAL(SW_OUTNKEYS));
	PROTECT(first_orig = NEW_INTEGER(SW_OUTNKEYS));
	PROTECT(last_orig = NEW_INTEGER(SW_OUTNKEYS));
	PROTECT(outfile = NEW_STRING(SW_OUTNKEYS));

	ForEachOutKey(k)
	{
		INTEGER(mykey)[k] = SoilWatAll.Output[k].mykey;
		INTEGER(myobj)[k] = SoilWatAll.Output[k].myobj;
		INTEGER(sumtype)[k] = SoilWatAll.Output[k].sumtype;
		LOGICAL(use)[k] = SoilWatAll.Output[k].use;
		INTEGER(first_orig)[k] = SoilWatAll.Output[k].first_orig;
		INTEGER(last_orig)[k] = SoilWatAll.Output[k].last_orig;

		if (SoilWatAll.Output[k].use)
		{
			for (i = 0; i < SoilWatAll.GenOutput.used_OUTNPERIODS; i++) {
				vtimePeriods[k + i * SW_OUTNKEYS] = SoilWatAll.GenOutput.timeSteps[k][i];
			}

			SET_STRING_ELT(outfile, k, mkChar(SoilWatAll.Output[k].outfile));
		} else {
			SET_STRING_ELT(outfile, k, mkChar(""));
		}
	}

	// Copy values into slots of S4 class
	SET_SLOT(OUT, install("outputSeparator"), sep);
	SET_SLOT(OUT, install("timeSteps"), timePeriods);
	SET_SLOT(OUT, install(cKEY[0]), mykey);
	SET_SLOT(OUT, install(cKEY[1]), myobj);
	SET_SLOT(OUT, install(cKEY[2]), sumtype);
	SET_SLOT(OUT, install(cKEY[3]), use);
	SET_SLOT(OUT, install(cKEY[4]), first_orig);
	SET_SLOT(OUT, install(cKEY[5]), last_orig);
	SET_SLOT(OUT, install(cKEY[6]), outfile);

	UNPROTECT(11);

	#ifdef RSWDEBUG
	if (debug) swprintf(" ... done. \n");
	#endif

	return OUT;
}


/** Set global rSOILWAT2 output variables

		@param outputData An object of S4 class `swOutput`, e.g., as returned by function
			`onGetOutput`.

		"Output": Updated number of years/months/weeks/days and
			pointers to pre-configured output data setup. These are used in ifdef RSOILWAT
			portions of SW_Output.c
	*/
void setGlobalrSOILWAT2_OutputVariables(SEXP outputData) {
	int i;
	OutKey k;

	// Get the pointers to the output arrays that were pre-allocated by `onGetOutput`
	ForEachOutKey(k) {
		for (i = 0; i < SoilWatAll.GenOutput.used_OUTNPERIODS; i++) {

			if (SoilWatAll.Output[k].use &&
						SoilWatAll.GenOutput.timeSteps[k][i] != eSW_NoTime)
			{
				SoilWatAll.GenOutput.p_OUT[k][SoilWatAll.GenOutput.timeSteps[k][i]] =
					REAL(GET_SLOT(GET_SLOT(outputData, install(key2str[k])),
					install(pd2longstr[SoilWatAll.GenOutput.timeSteps[k][i]])));
			}
		}
	}
}


/* Experience has shown that generating the Output Data structure in R is slow compared to C
 * This will generate the OUTPUT data Structure and Names*/
SEXP onGetOutput(SEXP inputData) {
	int i, l, h;
	OutKey k;
	OutPeriod pd;
	int *use;
	SEXP swOutput, swOutput_Object, outfile, swOutput_KEY, stemp_KEY,
		rTimeStep, xKEY, xKEY_names, xKEY_cnames;

	char *cSWoutput_Names[] = {"dy_nrow", "wk_nrow", "mo_nrow", "yr_nrow"};

  #ifdef RSWDEBUG
  int debug = 0;
  #endif

	#ifdef RSWDEBUG
	if (debug) swprintf("onGetOutput: start ...\n");
	#endif

	PROTECT(swOutput = MAKE_CLASS("swOutput"));
	PROTECT(swOutput_Object = NEW_OBJECT(swOutput));

	if (SoilWatAll.GenOutput.used_OUTNPERIODS <= 0) {
		UNPROTECT(2);
		return(swOutput_Object);
	}

  // Determine which output is turned on
	use = LOGICAL(GET_SLOT(GET_SLOT(inputData, install("output")), install("use")));

	// Determine which output periods are turned on for at least one output key
	find_OutPeriods_inUse();

	// Determine number of used years/months/weeks/days in simulation period
	SW_OUT_set_nrow();

	ForEachOutPeriod(pd) {
		SET_SLOT(
			swOutput_Object,
			install(cSWoutput_Names[pd]),
			ScalarInteger(nrow_OUT[pd]));
	}

	// KEYS
	PROTECT(swOutput_KEY = MAKE_CLASS("swOutput_KEY"));

	PROTECT(outfile = GET_SLOT(GET_SLOT(inputData, install("output")),
		install("outfile")));

	ForEachOutKey(k) {
		if (use[k]) {
			#ifdef RSWDEBUG
			if (debug) swprintf("%s (ncol = %d):", key2str[k], SoilWatAll.GenOutput.ncol_OUT[k]);
			#endif

			PROTECT(stemp_KEY = NEW_OBJECT(swOutput_KEY));

			SET_SLOT(stemp_KEY, install("Title"), mkString(Str_Dup(CHAR(STRING_ELT(outfile, k)))));
			SET_SLOT(stemp_KEY, install("Columns"), ScalarInteger(SoilWatAll.GenOutput.ncol_OUT[k]));

			PROTECT(rTimeStep = NEW_INTEGER(SoilWatAll.GenOutput.used_OUTNPERIODS));
			for (i = 0; i < SoilWatAll.GenOutput.used_OUTNPERIODS; i++) {
				INTEGER(rTimeStep)[i] = SoilWatAll.GenOutput.timeSteps[k][i];
			}
			SET_SLOT(stemp_KEY, install("TimeStep"), rTimeStep);

			for (i = 0; i < SoilWatAll.GenOutput.used_OUTNPERIODS; i++) {
				if (SoilWatAll.GenOutput.timeSteps[k][i] == eSW_NoTime) {
					continue;
				}

				#ifdef RSWDEBUG
				if (debug) swprintf(" %s (n=%ld = %ld x (%d + %d) alloc'ed) /",
					pd2longstr[SoilWatAll.GenOutput.timeSteps[k][i]],
					SoilWatAll.GenOutput.nrow_OUT[SoilWatAll.GenOutput.timeSteps[k][i]] *
					(SoilWatAll.GenOutput.ncol_OUT[k] + ncol_TimeOUT[SoilWatAll.GenOutput.timeSteps[k][i]]),
					SoilWatAll.GenOutput.nrow_OUT[SoilWatAll.GenOutput.timeSteps[k][i]],
					SoilWatAll.GenOutput.ncol_OUT[k], ncol_TimeOUT[SoilWatAll.GenOutput.timeSteps[k][i]]);
				#endif

				h = ncol_TimeOUT[SoilWatAll.GenOutput.timeSteps[k][i]];

				PROTECT(xKEY = allocMatrix(REALSXP, nrow_OUT[SoilWatAll.GenOutput.timeSteps[k][i]],
					SoilWatAll.GenOutput.ncol_OUT[k] + h)); // future output data matrix

				for (l = 0; l < nrow_OUT[SoilWatAll.GenOutput.timeSteps[k][i]] * (SoilWatAll.GenOutput.ncol_OUT[k] + h); l++) {
					// Initialize to 0:
					// allocMatrix does not initialize and memset appears to not work on
					// `allocMatrix` objects
					REAL(xKEY)[l] = 0.;
				}

				PROTECT(xKEY_names = allocVector(VECSXP, 2)); // list of dimnames
				PROTECT(xKEY_cnames = allocVector(STRSXP, SoilWatAll.GenOutput.ncol_OUT[k] + h)); // vector of column names
				SET_STRING_ELT(xKEY_cnames, 0, mkChar("Year"));
				if (h == 2) {
					SET_STRING_ELT(xKEY_cnames, 1, mkChar(pd2longstr[SoilWatAll.GenOutput.timeSteps[k][i]]));
				}
				for (l = 0; l < SoilWatAll.GenOutput.ncol_OUT[k]; l++) {
					SET_STRING_ELT(xKEY_cnames, l + h,
							mkChar(SoilWatAll.GenOutput.colnames_OUT[k][l]));
				}
				SET_VECTOR_ELT(xKEY_names, 1, xKEY_cnames);
				dimnamesgets(xKEY, xKEY_names);

				SET_SLOT(stemp_KEY, install(pd2longstr[SoilWatAll.GenOutput.timeSteps[k][i]]), xKEY);
				UNPROTECT(3);
			}

			SET_SLOT(swOutput_Object, install(key2str[k]), stemp_KEY);

			#ifdef RSWDEBUG
			if (debug) swprintf(" %s completed.\n", key2str[k]);
			#endif

			UNPROTECT(2);
		}
	}

	UNPROTECT(4);

	#ifdef RSWDEBUG
	if (debug) swprintf(" ... done. \n");
	#endif

	return swOutput_Object;
}
