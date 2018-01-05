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

#include "SOILWAT2/generic.h"
#include "SOILWAT2/filefuncs.h"
#include "SOILWAT2/Times.h"
#include "SOILWAT2/myMemory.h"

#include "SOILWAT2/SW_Defines.h"
#include "SOILWAT2/SW_Files.h"
#include "SOILWAT2/SW_Site.h"

#include "SOILWAT2/SW_Output.h"
#include "rSW_Output.h"

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>



/* =================================================== */
/*                  Global Variables                   */
/* --------------------------------------------------- */
extern SW_SITE SW_Site;
extern SW_OUTPUT SW_Output[];
extern Bool EchoInits;
extern char _Sep; /* output delimiter */
extern char const *key2str[];  // defined in SW_Output.c
extern int used_OUTNPERIODS;  // defined in SW_Output.c
extern OutPeriod timeSteps[SW_OUTNKEYS][SW_OUTNPERIODS]; // defined in SW_Output.c
extern int ncol_OUT[SW_OUTNKEYS]; // defined in SW_Output.c
extern char *colnames_OUT[SW_OUTNKEYS][5 * NVEGTYPES + MAX_LAYERS]; // defined in SW_Output.c

// Pointers to the pre configured output data. These are used in ifdef RSOILWAT portions of SW_Output.c
RealD *p_rOUT[SW_OUTNKEYS][SW_OUTNPERIODS];

// Number of years/months/weeks/days that are used in ifdef RSOILWAT portions of SW_Output.c
unsigned int yr_nrow = 0, mo_nrow = 0, wk_nrow = 0, dy_nrow = 0;


/* =================================================== */
/*                Module-Level Variables               */
/* --------------------------------------------------- */
static char *MyFileName;


/* =================================================== */
/* =================================================== */
/*             Public Function Definitions             */
/* --------------------------------------------------- */

/** Copies values from parameter `OUT` of rSOILWAT2 S4 class `swOUT` to SOILWAT2 variables
	"Output": Updated global variables SW_Output, used_OUTNPERIODS, and timeSteps which are
		defined in SW_Output.c
	*/
void onSet_SW_OUT(SEXP OUT) {
	int i;
	OutKey k;
	SEXP sep, outfile;
	Bool continue1;
	int *use, *timePeriods, *mykey, *myobj, *sumtype, *first_orig, *last_orig;

	MyFileName = SW_F_name(eOutput);

	PROTECT(sep = GET_SLOT(OUT, install("outputSeparator")));
	_Sep = '\t';/*TODO Make this work.*/

	timePeriods = INTEGER(GET_SLOT(OUT, install("timeSteps")));
	used_OUTNPERIODS = INTEGER(GET_DIM(GET_SLOT(OUT, install("timeSteps"))))[1]; // number of columns

	mykey = INTEGER(GET_SLOT(OUT, install("mykey")));
	myobj = INTEGER(GET_SLOT(OUT, install("myobj")));
	sumtype = INTEGER(GET_SLOT(OUT, install("sumtype")));
	use = LOGICAL(GET_SLOT(OUT, install("use")));
	first_orig = INTEGER(GET_SLOT(OUT, install("first_orig")));
	last_orig = INTEGER(GET_SLOT(OUT, install("last_orig")));
	PROTECT(outfile = GET_SLOT(OUT, install("outfile")));

	if (use[eSW_Estab]) {
		sumtype[eSW_Estab] = eSW_Sum;
		first_orig[eSW_Estab] = 1;
		timePeriods[eSW_Estab + 0 * SW_OUTNKEYS] = 3;
		for (i = 1; i < used_OUTNPERIODS; i++) {
			timePeriods[eSW_Estab + i * SW_OUTNKEYS] = SW_MISSING;
		}
		last_orig[eSW_Estab] = 366;
	}

	ForEachOutKey(k) {
		for (i = 0; i < used_OUTNPERIODS; i++) {
			timeSteps[k][i] = timePeriods[k + i * SW_OUTNKEYS];
		}

		continue1 = (k == eSW_AllVeg || k == eSW_ET || k == eSW_AllWthr || k == eSW_AllH2O);
		if (continue1) {
			SW_Output[k].use = FALSE;
			LogError(logfp, LOGNOTE, "Output key %s is currently unimplemented.", key2str[k]);
			continue;
		}

		/* check validity of summary type */
		SW_Output[k].sumtype = sumtype[k];
		if (SW_Output[k].sumtype == eSW_Fnl
				&& !(k == eSW_VWCBulk || k == eSW_VWCMatric
						|| k == eSW_SWPMatric || k == eSW_SWCBulk
						|| k == eSW_SWABulk || k == eSW_SWAMatric
						|| k == eSW_DeepSWC))
		{
			LogError(logfp, LOGWARN, "%s : Summary Type FIN with key %s is meaningless.\n" "  Using type AVG instead.", MyFileName, key2str[k]);
			SW_Output[k].sumtype = eSW_Avg;
		}

		/* verify deep drainage parameters */
		if (k == eSW_DeepSWC && SW_Output[k].sumtype != eSW_Off
				&& !SW_Site.deepdrain)
		{
			LogError(logfp, LOGWARN, "%s : DEEPSWC cannot be output if flag not set in %s.", MyFileName, SW_F_name(eOutput));
			continue;
		}

		/* prepare the remaining structure if use==TRUE */
		SW_Output[k].use = (SW_Output[k].sumtype == eSW_Off || !use[k]) ? FALSE : TRUE;
		if (SW_Output[k].use) {
			SW_Output[k].mykey = mykey[k];
			SW_Output[k].myobj = myobj[k];
			SW_Output[k].first_orig = first_orig[k];
			SW_Output[k].last_orig = last_orig[k];
			if (SW_Output[k].last_orig == 0) {
				LogError(logfp, LOGFATAL, "output.in : Invalid ending day (0) for key=%s.", key2str[k]);
			}
			SW_Output[k].outfile = Str_Dup(CHAR(STRING_ELT(outfile, k)));
		}
	}

	if (EchoInits)
		_echo_outputs();

	UNPROTECT(2);
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


	PROTECT(swOUT = MAKE_CLASS("swOUT"));
	PROTECT(OUT = NEW_OBJECT(swOUT));

	PROTECT(sep = NEW_STRING(1));
	SET_STRING_ELT(sep, 0, mkCharLen(&_Sep, 1));

	PROTECT(timePeriods = allocMatrix(INTSXP, SW_OUTNKEYS, used_OUTNPERIODS));
	vtimePeriods = INTEGER(timePeriods);

	PROTECT(mykey = NEW_INTEGER(SW_OUTNKEYS));
	PROTECT(myobj = NEW_INTEGER(SW_OUTNKEYS));
	PROTECT(sumtype = NEW_INTEGER(SW_OUTNKEYS));
	PROTECT(use = NEW_LOGICAL(SW_OUTNKEYS));
	PROTECT(first_orig = NEW_INTEGER(SW_OUTNKEYS));
	PROTECT(last_orig = NEW_INTEGER(SW_OUTNKEYS));
	PROTECT(outfile = NEW_STRING(SW_OUTNKEYS));

	ForEachOutKey(k) {
		for (i = 0; i < used_OUTNPERIODS; i++) {
			vtimePeriods[k + i * SW_OUTNKEYS] = timeSteps[k][i];
		}

		INTEGER(mykey)[k] = SW_Output[k].mykey;
		INTEGER(myobj)[k] = SW_Output[k].myobj;
		INTEGER(sumtype)[k] = SW_Output[k].sumtype;
		LOGICAL(use)[k] = SW_Output[k].use;
		INTEGER(first_orig)[k] = SW_Output[k].first_orig;
		INTEGER(last_orig)[k] = SW_Output[k].last_orig;

		if (SW_Output[k].use) {
			SET_STRING_ELT(outfile, k, mkChar(SW_Output[k].outfile));
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

	// Number of years/months/weeks/days are used in ifdef RSOILWAT portions of SW_Output.c
	yr_nrow = INTEGER(GET_SLOT(outputData, install("yr_nrow")))[0];
	mo_nrow = INTEGER(GET_SLOT(outputData, install("mo_nrow")))[0];
	wk_nrow = INTEGER(GET_SLOT(outputData, install("wk_nrow")))[0];
	dy_nrow = INTEGER(GET_SLOT(outputData, install("dy_nrow")))[0];

	// Get the pointers to the pre-configured output data setup. These are used in ifdef RSOILWAT portions of SW_Output.c
	ForEachOutKey(k) {
		for (i = 0; i < used_OUTNPERIODS; i++) {
			switch (timeSteps[eSW_CO2Effects][i]) {
				case eSW_Day:
					p_rOUT[k][eSW_Day] = REAL(GET_SLOT(GET_SLOT(outputData, install(key2str[k])), install("Day")));
					break;
				case eSW_Week:
					p_rOUT[k][eSW_Week] = REAL(GET_SLOT(GET_SLOT(outputData, install(key2str[k])), install("Week")));
					break;
				case eSW_Month:
					p_rOUT[k][eSW_Month] = REAL(GET_SLOT(GET_SLOT(outputData, install(key2str[k])), install("Month")));
					break;
				case eSW_Year:
					p_rOUT[k][eSW_Year] = REAL(GET_SLOT(GET_SLOT(outputData, install(key2str[k])), install("Year")));
					break;
			}
		}
	}
}


/* Experience has shown that generating the Output Data structure in R is slow compared to C
 * This will generate the OUTPUT data Structure and Names*/
SEXP onGetOutput(SEXP inputData) {
	int debug = 0;
	int i, l, tYears;
	OutKey k;
	int pOPuse[SW_OUTNPERIODS];
	int *use;
	SEXP swOutput, swOutput_Object, outfile, years, swOutput_KEY, stemp_KEY, rTimeStep,
		xKEY, xKEY_names, xKEY_cnames;

	char *cSWoutput_Names[] = {"yr_nrow", "mo_nrow", "wk_nrow", "dy_nrow"};


	PROTECT(swOutput = MAKE_CLASS("swOutput"));
	PROTECT(swOutput_Object = NEW_OBJECT(swOutput));

  // Determine which output is turned on
	use = LOGICAL(GET_SLOT(GET_SLOT(inputData, install("output")), install("use")));

	// Determine size of output
	//   * number of simulation years
	PROTECT(years = GET_SLOT(inputData, install("years")));
	tYears = (INTEGER(GET_SLOT(years, install("EndYear")))[0] - INTEGER(GET_SLOT(years, install("StartYear")))[0] + 1);

	// Determine name of outfile for output key k
	PROTECT(outfile = GET_SLOT(GET_SLOT(inputData, install("output")), install("outfile")));
	ForEachOutKey(k) {
		// Determine which output periods are turned on for at least one output key
		for (i = 0; i < used_OUTNPERIODS; i++) {
			switch (timeSteps[k][i]) {
				case eSW_Day:
					pOPuse[eSW_Day] = 1;
					break;
				case eSW_Week:
					pOPuse[eSW_Week] = 1;
					break;
				case eSW_Month:
					pOPuse[eSW_Month] = 1;
					break;
				case eSW_Year:
					pOPuse[eSW_Year] = 1;
					break;
			}
		}
	}

	// Determine number of used years/months/weeks/days in simulation period
	yr_nrow = tYears * pOPuse[eSW_Year];
	mo_nrow = tYears * MAX_MONTHS * pOPuse[eSW_Month];
	wk_nrow = tYears * MAX_WEEKS * pOPuse[eSW_Week];

	if (pOPuse[eSW_Day] == 1) {
		dy_nrow = 0;
		for (i = INTEGER(GET_SLOT(years, install("StartYear")))[0]; i <= INTEGER(GET_SLOT(years, install("EndYear")))[0]; i++) {
			if (i == 0) {
				//Need to calculate the starting first day of first year
				dy_nrow += Time_get_lastdoy_y(i) - INTEGER(GET_SLOT(years, install("FDOFY")))[0] + 1;
			} else if (i == (tYears - 1)) {
				//and last day of last year.
				dy_nrow += INTEGER(GET_SLOT(years, install("EDOEY")))[0];
			} else {
				dy_nrow += Time_get_lastdoy_y(i);
			}
		}
	}

	if (debug) swprintf("Year Rows: %d, Month Rows: %d, Week Rows: %d, Day Rows: %d\n",
		yr_nrow, mo_nrow, wk_nrow, dy_nrow);

	SET_SLOT(swOutput_Object, install(cSWoutput_Names[0]), ScalarInteger(yr_nrow));
	SET_SLOT(swOutput_Object, install(cSWoutput_Names[1]), ScalarInteger(mo_nrow));
	SET_SLOT(swOutput_Object, install(cSWoutput_Names[2]), ScalarInteger(wk_nrow));
	SET_SLOT(swOutput_Object, install(cSWoutput_Names[3]), ScalarInteger(dy_nrow));


	// Number and names of columns for output matrices
	SW_OUT_set_ncol();
	SW_OUT_set_colnames();


	// KEYS
	PROTECT(swOutput_KEY = MAKE_CLASS("swOutput_KEY"));

	ForEachOutKey(k) {
		if (use[k]) {
			if (debug) swprintf(" %s: ", key2str[k]);

			PROTECT(stemp_KEY = NEW_OBJECT(swOutput_KEY));

			SET_SLOT(stemp_KEY, install("Title"), STRING_ELT(outfile, k));
			SET_SLOT(stemp_KEY, install("Columns"), ScalarInteger(ncol_OUT[k]));

			PROTECT(rTimeStep = NEW_INTEGER(used_OUTNPERIODS));
			for (i = 0; i < used_OUTNPERIODS; i++)
				INTEGER(rTimeStep)[i] = timeSteps[k][i];
			SET_SLOT(stemp_KEY, install("TimeStep"), rTimeStep);

			for (i = 0; i < used_OUTNPERIODS; i++) {
				if (timeSteps[k][i] == eSW_Day) {
					if (debug) swprintf(" dy ...");
					PROTECT(xKEY = allocMatrix(REALSXP, dy_nrow, ncol_OUT[k] + 2)); // future output data matrix
					for (l = 0; l < dy_nrow * (ncol_OUT[k] + 2); l++)
						REAL(xKEY)[l] = 0.; // Initialize to 0; allocMatrix does not initialize

					PROTECT(xKEY_names = allocVector(VECSXP, 2)); // list of dimnames
					PROTECT(xKEY_cnames = allocVector(STRSXP, ncol_OUT[k] + 2)); // vector of column names
					SET_STRING_ELT(xKEY_cnames, 0, mkChar("Year"));
					SET_STRING_ELT(xKEY_cnames, 1, mkChar("DOY"));
					for (l = 0; l < ncol_OUT[k]; l++)
						SET_STRING_ELT(xKEY_cnames, l + 2, mkChar(colnames_OUT[k][l]));
					SET_VECTOR_ELT(xKEY_names, 1, xKEY_cnames);
					dimnamesgets(xKEY, xKEY_names);

					SET_SLOT(stemp_KEY, install("Day"), xKEY);
					UNPROTECT(3);
				}

				if (timeSteps[k][i] == eSW_Week) {
					if (debug) swprintf(" wk ...");
					PROTECT(xKEY = allocMatrix(REALSXP, wk_nrow, ncol_OUT[k] + 2)); // future output data matrix
					for (l = 0; l < wk_nrow * (ncol_OUT[k] + 2); l++)
						REAL(xKEY)[l] = 0.; // Initialize to 0; allocMatrix does not initialize

					PROTECT(xKEY_names = allocVector(VECSXP, 2)); // list of dimnames
					PROTECT(xKEY_cnames = allocVector(STRSXP, ncol_OUT[k] + 2)); // vector of column names
					SET_STRING_ELT(xKEY_cnames, 0, mkChar("Year"));
					SET_STRING_ELT(xKEY_cnames, 1, mkChar("Week"));
					for (l = 0; l < ncol_OUT[k]; l++)
						SET_STRING_ELT(xKEY_cnames, l + 2, mkChar(colnames_OUT[k][l]));
					SET_VECTOR_ELT(xKEY_names, 1, xKEY_cnames);
					dimnamesgets(xKEY, xKEY_names);

					SET_SLOT(stemp_KEY, install("Week"), xKEY);
					UNPROTECT(3);
				}

				if (timeSteps[k][i] == eSW_Month) {
					if (debug) swprintf(" mo ...");
					PROTECT(xKEY = allocMatrix(REALSXP, mo_nrow, ncol_OUT[k] + 2)); // future output data matrix
					for (l = 0; l < mo_nrow * (ncol_OUT[k] + 2); l++)
						REAL(xKEY)[l] = 0.; // Initialize to 0; allocMatrix does not initialize

					PROTECT(xKEY_names = allocVector(VECSXP, 2)); // list of dimnames
					PROTECT(xKEY_cnames = allocVector(STRSXP, ncol_OUT[k] + 2)); // vector of column names
					SET_STRING_ELT(xKEY_cnames, 0, mkChar("Year"));
					SET_STRING_ELT(xKEY_cnames, 1, mkChar("Month"));
					for (l = 0; l < ncol_OUT[k]; l++)
						SET_STRING_ELT(xKEY_cnames, l + 2, mkChar(colnames_OUT[k][l]));
					SET_VECTOR_ELT(xKEY_names, 1, xKEY_cnames);
					dimnamesgets(xKEY, xKEY_names);

					SET_SLOT(stemp_KEY, install("Month"), xKEY);
					UNPROTECT(3);
				}

				if (timeSteps[k][i] == eSW_Year) {
					if (debug) swprintf(" yr ...");
					PROTECT(xKEY = allocMatrix(REALSXP, yr_nrow, ncol_OUT[k] + 1)); // future output data matrix
					for (l = 0; l < yr_nrow * (ncol_OUT[k] + 1); l++)
						REAL(xKEY)[l] = 0.; // Initialize to 0; allocMatrix does not initialize

					PROTECT(xKEY_names = allocVector(VECSXP, 2)); // list of dimnames
					PROTECT(xKEY_cnames = allocVector(STRSXP, ncol_OUT[k] + 1)); // vector of column names
					SET_STRING_ELT(xKEY_cnames, 0, mkChar("Year"));
					for (l = 0; l < ncol_OUT[k]; l++)
						SET_STRING_ELT(xKEY_cnames, l + 1, mkChar(colnames_OUT[k][l]));
					SET_VECTOR_ELT(xKEY_names, 1, xKEY_cnames);
					dimnamesgets(xKEY, xKEY_names);

					SET_SLOT(stemp_KEY, install("Year"), xKEY);
					UNPROTECT(3);
				}
			}

			SET_SLOT(swOutput_Object, install(key2str[k]), stemp_KEY);
			if (debug) swprintf(" %s completed.\n", key2str[k]);

			UNPROTECT(2);
		}
	}

	UNPROTECT(5);

	return swOutput_Object;
}
