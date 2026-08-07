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
#include "SOILWAT2/include/SW_Main_lib.h"

#include "SOILWAT2/include/SW_Output.h" // externs many variables
#include "SOILWAT2/include/SW_Output_outarray.h" // for function `SW_OUT_set_nrow`
#include "rSW_Output.h"
#include "SW_R_lib.h"

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>



/* =================================================== */
/*             Global Function Definitions             */
/* --------------------------------------------------- */

/** Copies values from parameter `OUT` of rSOILWAT2 S4 class `swOUT` to SOILWAT2 variables
	"Output": Updated global variables SW_Output, used_OUTNPERIODS, and timeSteps which are
		defined in SW_Output.c
	*/
void onSet_SW_OUT(SEXP OUT, LOG_INFO* LogInfo) {
	int i, msg_type;
	OutKey k;
	SEXP sep, outfile, tp_convert;
	int *timePeriods, *sumtype;
	int *use;
	// mykey, myobj and use are currently unused:
	// int *use, *mykey, *myobj;
	char msg[200]; // message to print
	#ifdef RSWDEBUG
	int debug = 0;
	#endif

	#ifdef RSWDEBUG
	if (debug) sw_printf("onSet_SW_OUT: start ...");
	#endif

	PROTECT(sep = GET_SLOT(OUT, install("outputSeparator")));

	// TODO: I don't know why `GET_SLOT(OUT, install("timeSteps"))` is suddenly
	// of type real and not integer any more
	PROTECT(tp_convert = coerceVector(GET_SLOT(OUT, install("timeSteps")), INTSXP));
	timePeriods = INTEGER(tp_convert);
	SoilWatDomain.OutDom.used_OUTNPERIODS = INTEGER(GET_DIM(GET_SLOT(OUT, install("timeSteps"))))[1]; // number of columns

	// mykey, myobj and use are currently unused:
	// mykey = INTEGER(GET_SLOT(OUT, install("mykey")));
	// myobj = INTEGER(GET_SLOT(OUT, install("myobj")));
	use = LOGICAL(GET_SLOT(OUT, install("use")));
	sumtype = INTEGER(GET_SLOT(OUT, install("sumtype")));
	PROTECT(outfile = GET_SLOT(OUT, install("outfile")));

	ForEachOutKey(k) {
		msg_type = SW_OUT_read_onekey(
            &SoilWatDomain.OutDom,
			k,
			(use[k]) ? sumtype[k] : eSW_Off,
			msg,
			sizeof msg,
			&SoilWatRun.VegProdIn.use_SWA,
			SoilWatRun.SiteIn.deepdrain,
			SoilWatDomain.SW_PathInputs.txtInFiles
		);

		if (msg_type > 0) {
			LogError(LogInfo, msg_type, "%s", msg);
            if(LogInfo->stopRun) {
                UNPROTECT(3); // Unprotect the three protected variables before exiting
                return; // Exit function prematurely due to error
            }
			continue;
		}

		if (SoilWatDomain.OutDom.use[k]) {
			SoilWatDomain.OutDom.outfile[k] = Str_Dup(CHAR(STRING_ELT(outfile, k)), LogInfo);
            if(LogInfo->stopRun) {
                UNPROTECT(3); // Unprotect the three protected variables before exiting
                return; // Exit function prematurely due to error
            }

			ForEachOutPeriod(i) {
				SoilWatDomain.OutDom.timeSteps[k][i] = timePeriods[k + i * SW_OUTNKEYS];
			}
		}
	}

	if (EchoInits)
		echo_outputs(&SoilWatDomain.OutDom, LogInfo);

	UNPROTECT(3);

	#ifdef RSWDEBUG
	if (debug) sw_printf(" ... done. \n");
	#endif
}


/** Copies values from global SOILWAT2 variables (SW_Output, used_OUTNPERIODS, and timeSteps)
		@return `OUT` of rSOILWAT2 S4 class `swOUT`
	*/
SEXP onGet_SW_OUT(void) {
    SW_OUT_DOM *OutDom = &SoilWatDomain.OutDom;
	int i;
	int *vtimePeriods;
	OutKey k;
	SEXP swOUT, OUT, sep, timePeriods;
	SEXP mykey, myobj, sumtype, use, first_orig, last_orig, outfile;
	char *cKEY[] = {"mykey", "myobj", "sumtype", "use", "first_orig", "last_orig", "outfile"};
	char _Sep = '\t';
  #ifdef RSWDEBUG
  int debug = 0;
  #endif

	#ifdef RSWDEBUG
	if (debug) sw_printf("onGet_SW_OUT: start ...");
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
		INTEGER(mykey)[k] = OutDom->mykey[k];
		INTEGER(myobj)[k] = OutDom->myobj[k];
		INTEGER(sumtype)[k] = OutDom->sumtype[k];
		LOGICAL(use)[k] = OutDom->use[k];
		INTEGER(first_orig)[k] = 1; // dropped with SOILWAT2 v8.4.0
		INTEGER(last_orig)[k] = 366; // dropped with SOILWAT2 v8.4.0

		if (OutDom->use[k])
		{
			for (i = 0; i < OutDom->used_OUTNPERIODS; i++) {
				vtimePeriods[k + i * SW_OUTNKEYS] = OutDom->timeSteps[k][i];
			}

			SET_STRING_ELT(outfile, k, mkChar(OutDom->outfile[k]));
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
	if (debug) sw_printf(" ... done. \n");
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
		for (i = 0; i < SoilWatDomain.OutDom.used_OUTNPERIODS; i++) {

			if (SoilWatDomain.OutDom.use[k] &&
						SoilWatDomain.OutDom.timeSteps[k][i] != eSW_NoTime)
			{
				SoilWatRun.OutRun.p_OUT[k][SoilWatDomain.OutDom.timeSteps[k][i]] =
					REAL(GET_SLOT(GET_SLOT(outputData, install(key2str[k])),
					install(pd2longstr[SoilWatDomain.OutDom.timeSteps[k][i]])));
			}
		}
	}
}


/* Experience has shown that generating the Output Data structure in R is slow compared to C
 * This will generate the OUTPUT data Structure and Names*/
SEXP onGetOutput(SEXP inputData, LOG_INFO* LogInfo) {
    int i, l, h, numUnprotects = 0;
    OutKey k;
    OutPeriod pd;
    int *use;
    SEXP swOutput, swOutput_Object, outfile, swOutput_KEY, stemp_KEY,
        rTimeStep, xKEY, xKEY_names, xKEY_cnames;

    char *cSWoutput_Names[] = {"dy_nrow", "wk_nrow", "mo_nrow", "yr_nrow"};

    SW_OUT_DOM *OutDom = &SoilWatDomain.OutDom;

    #ifdef RSWDEBUG
    int debug = 0;
    #endif

    #ifdef RSWDEBUG
    if (debug) sw_printf("onGetOutput: start ...\n");
    #endif

    PROTECT(swOutput = MAKE_CLASS("swOutput"));
    PROTECT(swOutput_Object = NEW_OBJECT(swOutput));
    numUnprotects += 2;

    // Determine which output is turned on
    use = LOGICAL(GET_SLOT(GET_SLOT(inputData, install("output")), install("use")));

    // Determine which output periods are turned on for at least one output key
    find_OutPeriods_inUse(&SoilWatDomain.OutDom);

    // Determine number of used years/months/weeks/days in simulation period
    SW_OUT_set_nrow(
        &SoilWatRun.ModelIn, OutDom->use_OutPeriod, OutDom->nrow_OUT
    );

    ForEachOutPeriod(pd) {
        SET_SLOT(
            swOutput_Object,
            install(cSWoutput_Names[pd]),
            ScalarInteger(OutDom->nrow_OUT[pd])
        );
    }

    // KEYS
    PROTECT(swOutput_KEY = MAKE_CLASS("swOutput_KEY"));

    PROTECT(
        outfile = GET_SLOT(
            GET_SLOT(inputData, install("output")),
            install("outfile")
        )
    );

    numUnprotects += 2;

    ForEachOutKey(k) {
        if (use[k]) {
            #ifdef RSWDEBUG
            if (debug) sw_printf("%s (ncol = %d):", key2str[k], OutDom->ncol_OUT[k]);
            #endif

            PROTECT(stemp_KEY = NEW_OBJECT(swOutput_KEY));
            numUnprotects++;

            SET_SLOT(
                stemp_KEY,
                install("Title"),
                mkString(Str_Dup(CHAR(STRING_ELT(outfile, k)), LogInfo))
            );
            if(LogInfo->stopRun) {
                goto report;
            }

            SET_SLOT(
                stemp_KEY,
                install("Columns"),
                ScalarInteger(OutDom->ncol_OUT[k])
            );

            PROTECT(rTimeStep = NEW_INTEGER(OutDom->used_OUTNPERIODS));
            numUnprotects++;

            for (i = 0; i < OutDom->used_OUTNPERIODS; i++) {
                INTEGER(rTimeStep)[i] = OutDom->timeSteps[k][i];
            }
            SET_SLOT(stemp_KEY, install("TimeStep"), rTimeStep);

            for (i = 0; i < OutDom->used_OUTNPERIODS; i++) {
                if (OutDom->timeSteps[k][i] == eSW_NoTime) {
                    continue;
                }

                #ifdef RSWDEBUG
                if (debug) sw_printf(" %s (n=%ld = %ld x (%d + %d) alloc'ed) /",
                    pd2longstr[OutDom->timeSteps[k][i]],
                    OutDom->nrow_OUT[OutDom->timeSteps[k][i]] *
                    (OutDom->ncol_OUT[k] + ncol_TimeOUT[OutDom->timeSteps[k][i]]),
                    OutDom->nrow_OUT[OutDom->timeSteps[k][i]],
                    OutDom->ncol_OUT[k], ncol_TimeOUT[OutDom->timeSteps[k][i]]);
                #endif

                h = ncol_TimeOUT[OutDom->timeSteps[k][i]];

                // output data matrix
                PROTECT(
                    xKEY = allocMatrix(
                        REALSXP,
                        OutDom->nrow_OUT[OutDom->timeSteps[k][i]],
                        OutDom->ncol_OUT[k] + h
                    )
                );
                numUnprotects++;


                for (l = 0; l < OutDom->nrow_OUT[OutDom->timeSteps[k][i]] *
                                                (OutDom->ncol_OUT[k] + h); l++) {
                    // Initialize to 0:
                    // allocMatrix does not initialize and
                    // memset appears to not work on `allocMatrix` objects
                    REAL(xKEY)[l] = 0.;
                }

                // list of dimnames
                PROTECT(xKEY_names = allocVector(VECSXP, 2));
                numUnprotects++;
                // vector of column names
                PROTECT(
                    xKEY_cnames = allocVector(STRSXP, OutDom->ncol_OUT[k] + h)
                );
                numUnprotects++;

                SET_STRING_ELT(xKEY_cnames, 0, mkChar("Year"));
                if (h == 2) {
                    SET_STRING_ELT(
                        xKEY_cnames,
                        1,
                        mkChar(pd2longstr[OutDom->timeSteps[k][i]])
                    );
                }
                for (l = 0; l < OutDom->ncol_OUT[k]; l++) {
                    SET_STRING_ELT(
                        xKEY_cnames, l + h, mkChar(OutDom->colnames_OUT[k][l])
                    );
                }
                SET_VECTOR_ELT(xKEY_names, 1, xKEY_cnames);
                dimnamesgets(xKEY, xKEY_names);

                SET_SLOT(
                    stemp_KEY,
                    install(pd2longstr[OutDom->timeSteps[k][i]]),
                    xKEY
                );

                UNPROTECT(3);
                numUnprotects -= 3;
            }

            SET_SLOT(swOutput_Object, install(key2str[k]), stemp_KEY);

            #ifdef RSWDEBUG
            if (debug) sw_printf(" %s completed.\n", key2str[k]);
            #endif

            UNPROTECT(2);
            numUnprotects -= 2;
        }
    }

    report: {
        UNPROTECT(numUnprotects);
    }

    #ifdef RSWDEBUG
    if (debug) sw_printf(" ... done. \n");
    #endif

    return swOutput_Object;
}
