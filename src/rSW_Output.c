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
extern char const *key2str[];

/* =================================================== */
/*                Module-Level Variables               */
/* --------------------------------------------------- */
static char *MyFileName;

static int numPeriod;// variable to keep track of the number of periods that are listed in the line TIMESTEP
static int timeSteps[SW_OUTNKEYS][SW_OUTNPERIODS];// array to keep track of the periods that will be used for each output


/* =================================================== */
/* =================================================== */
/*             Public Function Definitions             */
/* --------------------------------------------------- */

void onSet_SW_OUT(SEXP OUT)
{
	int i;
	OutKey k;
	SEXP sep, timestep,useTimeStep;
	Bool continue1;
	SEXP mykey, myobj, period, sumtype, use, first, last, first_orig, last_orig, outfile;

	MyFileName = SW_F_name(eOutput);

	PROTECT(sep = GET_SLOT(OUT, install("outputSeparator")));
	_Sep = '\t';/*TODO Make this work.*/
	PROTECT(useTimeStep = GET_SLOT(OUT, install("useTimeStep")));
	PROTECT(timestep = GET_SLOT(OUT, install("timePeriods")));

	PROTECT(mykey = GET_SLOT(OUT, install("mykey")));
	PROTECT(myobj = GET_SLOT(OUT, install("myobj")));
	PROTECT(period = GET_SLOT(OUT, install("period")));
	PROTECT(sumtype = GET_SLOT(OUT, install("sumtype")));
	PROTECT(use = GET_SLOT(OUT, install("use")));
	PROTECT(first = GET_SLOT(OUT, install("first")));
	PROTECT(last = GET_SLOT(OUT, install("last")));
	PROTECT(first_orig = GET_SLOT(OUT, install("first_orig")));
	PROTECT(last_orig = GET_SLOT(OUT, install("last_orig")));
	PROTECT(outfile = GET_SLOT(OUT, install("outfile")));

	ForEachOutKey(k)
	{
		for (i = 0; i < SW_OUTNPERIODS; i++)
		{
			if (i < 1 && !LOGICAL(useTimeStep)[0])
			{
				timeSteps[k][i] = INTEGER(period)[k];
			}
			else if(LOGICAL(useTimeStep)[0] && i<LENGTH(timestep))
			{
				timeSteps[k][i] = INTEGER(timestep)[i];
			}
			else
			{
				timeSteps[k][i] = SW_OUTNPERIODS;
			}
		}
		if (k == eSW_Estab)
		{
			INTEGER(sumtype)[k] = LOGICAL(use)[k]?eSW_Sum:eSW_Off;
			INTEGER(first)[k] = 1;
			INTEGER(period)[k] = 3;
			INTEGER(last)[k] = 366;
		}
		continue1 = (k == eSW_AllVeg || k == eSW_ET || k == eSW_AllWthr || k == eSW_AllH2O);
		if (continue1)
		{
			SW_Output[k].use = FALSE;
			//LogError(logfp, LOGNOTE, "Output key %s is currently unimplemented.", key2str[k]);
		}
		if (!continue1)
		{
			/* check validity of summary type */
			SW_Output[k].sumtype = INTEGER(sumtype)[k];
			if (SW_Output[k].sumtype == eSW_Fnl && !(k == eSW_VWCBulk || k == eSW_VWCMatric || k == eSW_SWPMatric || k == eSW_SWCBulk || k == eSW_SWABulk || k == eSW_SWAMatric || k == eSW_DeepSWC))
			{
				LogError(logfp, LOGWARN, "output.in : Summary Type FIN with key %s is meaningless.\n"
						"  Using type AVG instead.", key2str[k]);
				SW_Output[k].sumtype = eSW_Avg;
			}
			/* verify deep drainage parameters */
			if (k == eSW_DeepSWC && SW_Output[k].sumtype != eSW_Off && !SW_Site.deepdrain)
			{
				LogError(logfp, LOGWARN, "output.in : DEEPSWC cannot be output if flag not set in output.in.");
			}
			else
			{
				/* prepare the remaining structure if use==TRUE */
				SW_Output[k].use = (SW_Output[k].sumtype == eSW_Off) ? FALSE : TRUE;
				if (SW_Output[k].use)
				{
					SW_Output[k].mykey = INTEGER(mykey)[k];
					SW_Output[k].myobj = INTEGER(myobj)[k];
					SW_Output[k].period = INTEGER(period)[k];
					SW_Output[k].first_orig = INTEGER(first_orig)[k];
					SW_Output[k].last_orig = INTEGER(last_orig)[k];
					if (SW_Output[k].last_orig == 0)
					{
						LogError(logfp, LOGFATAL, "output.in : Invalid ending day (%s), key=%s.", INTEGER(last)[k], key2str[k]);
					}
					SW_Output[k].outfile = Str_Dup(CHAR(STRING_ELT(outfile, k)));
				}
			}
		}
	}
	if (EchoInits)
	_echo_outputs();
	UNPROTECT(13);
}

SEXP onGet_SW_OUT(void)
{
	int i, debug = 1;
	Bool doOnce = FALSE;
	OutKey k;
	SEXP swOUT;
	SEXP OUT;
	SEXP sep;
	SEXP useTimeStep;
	SEXP timestep;
	SEXP mykey, myobj, period, sumtype, use, first, last, first_orig, last_orig, outfile;
	char *cKEY[] =
	{	"mykey", "myobj", "period", "sumtype", "use", "first", "last", "first_orig", "last_orig", "outfile"};

	if (debug) swprintf("onGet_SW_OUT begin\n");

	PROTECT(swOUT = MAKE_CLASS("swOUT"));
	PROTECT(OUT = NEW_OBJECT(swOUT));

	PROTECT(sep=NEW_STRING(1));
	SET_STRING_ELT(sep, 0, mkCharLen(&_Sep,1));
	SET_SLOT(OUT, install("outputSeparator"), sep);

	PROTECT(useTimeStep = NEW_LOGICAL(1));
	if(numPeriod == 0)
		LOGICAL(useTimeStep)[0] = FALSE;
	else
		LOGICAL(useTimeStep)[0] = TRUE;

	if (debug) {
		swprintf("useTimeStep after assignment = %d\n", useTimeStep);
		swprintf("	- type of slot (10 = 'logical') %d\n", TYPEOF(useTimeStep));
		swprintf("	- logvalue of slot %d\n", LOGICAL_VALUE(useTimeStep));
		if ( 10 == TYPEOF(useTimeStep) )
			swprintf("	- logdata of slot %d\n", LOGICAL_DATA(useTimeStep));
		else
			swprintf("	- logdata of slot not available because not of type 'logical'\n");
	}
	PROTECT(timestep = NEW_INTEGER(numPeriod));

	PROTECT(mykey = NEW_INTEGER(SW_OUTNKEYS));
	PROTECT(myobj = NEW_INTEGER(SW_OUTNKEYS));
	PROTECT(period = NEW_INTEGER(SW_OUTNKEYS));
	PROTECT(sumtype = NEW_INTEGER(SW_OUTNKEYS));
	PROTECT(use = NEW_LOGICAL(SW_OUTNKEYS));
	PROTECT(first = NEW_INTEGER(SW_OUTNKEYS));
	PROTECT(last = NEW_INTEGER(SW_OUTNKEYS));
	PROTECT(first_orig = NEW_INTEGER(SW_OUTNKEYS));
	PROTECT(last_orig = NEW_INTEGER(SW_OUTNKEYS));
	PROTECT(outfile = NEW_STRING(SW_OUTNKEYS));

	ForEachOutKey(k)
	{
		if(useTimeStep && SW_Output[k].use && !doOnce)
		{
			if (debug) swprintf("length(timestep) = %d, numPeriod = %d\n", GET_LENGTH(timestep), numPeriod);
			for (i = 0; i < numPeriod; i++)
			{
				if (debug) swprintf("timestep, timestep[%d], and timeSteps[%d][%d] before %d assignment = %d, %d, %d\n",
						i, i, k, k, timestep, INTEGER(timestep)[i], timeSteps[k][i]);
				INTEGER(timestep)[i] = timeSteps[k][i];
				if (debug) swprintf("timestep, timestep[%d], and timeSteps[%d][%d] after %d assignment = %d, %d, %d\n",
						i, i, k, k, timestep, INTEGER(timestep)[i], timeSteps[k][i]);
			}
			doOnce=TRUE;
		}

		INTEGER(mykey)[k] = SW_Output[k].mykey;
		INTEGER(myobj)[k] = SW_Output[k].myobj;
		INTEGER(period)[k] = SW_Output[k].period;
		INTEGER(sumtype)[k] = SW_Output[k].sumtype;
		LOGICAL(use)[k] = SW_Output[k].use;
		INTEGER(first)[k] = SW_Output[k].first;
		INTEGER(last)[k] = SW_Output[k].last;
		INTEGER(first_orig)[k] = SW_Output[k].first_orig;
		INTEGER(last_orig)[k] = SW_Output[k].last_orig;
		if(SW_Output[k].use)
		{
			SET_STRING_ELT(outfile, k, mkChar(SW_Output[k].outfile));
		}
		else
		SET_STRING_ELT(outfile, k, mkChar(""));
	}
	SET_SLOT(OUT, install("timePeriods"), timestep);

	if(debug)
	{
		swprintf("useTimeStep slot of OUT before assignment = %d\n", GET_SLOT(OUT, install("useTimeStep")));
		swprintf("	- type of slot %d\n", TYPEOF(GET_SLOT(OUT, install("useTimeStep"))));
		swprintf("	- logvalue of slot %d\n", LOGICAL_VALUE(GET_SLOT(OUT, install("useTimeStep"))));
		if( 10 == TYPEOF(GET_SLOT(OUT, install("useTimeStep"))) )
		swprintf("	- logdata of slot %d\n", LOGICAL_DATA(GET_SLOT(OUT, install("useTimeStep"))));
		else
		swprintf("	- logdata of slot not available because not of type 'logical'\n");
	}
	SET_SLOT(OUT, install("useTimeStep"), useTimeStep);
	if(debug)
	{
		swprintf("useTimeStep slot of OUT after assignment = %d\n", GET_SLOT(OUT, install("useTimeStep")));
		swprintf("	- type of slot (4 = 'environments') %d\n", TYPEOF(GET_SLOT(OUT, install("useTimeStep"))));
		swprintf("	- logvalue of slot %d\n", LOGICAL_VALUE(GET_SLOT(OUT, install("useTimeStep"))));
		if( 10 == TYPEOF(GET_SLOT(OUT, install("useTimeStep"))) )
		swprintf("	- logdata of slot %d\n", LOGICAL_DATA(GET_SLOT(OUT, install("useTimeStep"))));
		else
		swprintf("	- logdata of slot not available because not of type 'logical'\n");
	}

	SET_SLOT(OUT, install(cKEY[0]), mykey);
	SET_SLOT(OUT, install(cKEY[1]), myobj);
	SET_SLOT(OUT, install(cKEY[2]), period);
	SET_SLOT(OUT, install(cKEY[3]), sumtype);
	SET_SLOT(OUT, install(cKEY[4]), use);
	SET_SLOT(OUT, install(cKEY[5]), first);
	SET_SLOT(OUT, install(cKEY[6]), last);
	SET_SLOT(OUT, install(cKEY[7]), first_orig);
	SET_SLOT(OUT, install(cKEY[8]), last_orig);
	SET_SLOT(OUT, install(cKEY[9]), outfile);

	UNPROTECT(15);
	if(debug) swprintf("onGet_SW_OUT end\n");
	return OUT;
}
