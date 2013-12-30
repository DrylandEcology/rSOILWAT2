/********************************************************/
/********************************************************/
/*  Application: SOILWAT - soilwater dynamics simulator
 *  Source file: Main.c
 *  Type: main module
 *  Purpose: Contains the main loops and initializations.
 *
 06/24/2013	(rjm)	included "SW_Site.h" and "SW_Weather.h";
 added calls at end of main() to SW_SIT_clear_layers() and SW_WTH_clear_runavg_list() to free memory
 */
/********************************************************/
/********************************************************/

/* =================================================== */
/*                INCLUDES / DEFINES                   */
/* --------------------------------------------------- */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#ifdef RSOILWAT
#include <R.h>
#include <Rdefines.h>
#include <Rconfig.h>
#include <Rinterface.h>
#include <Rinternals.h>
#endif

#ifdef __BCC__
#include <dir.h>
#else
#include <unistd.h>
#endif
#include "generic.h"
#include "filefuncs.h"
#include "SW_Defines.h"
#include "SW_Control.h"
#include "SW_Site.h"
#include "SW_Weather.h"

/* =================================================== */
/*                  Global Declarations                */
/* externed by other routines elsewhere in the program */
/* --------------------------------------------------- */

/* see generic.h and filefuncs.h for more info on these vars */
char inbuf[1024]; /* buffer used by input statements */
char errstr[MAX_ERROR]; /* used to compose an error msg    */
FILE *logfp; /* file handle for logging messages */
int logged; /* boolean: true = we logged a msg */
/* if true, write indicator to stderr */
#ifdef RSOILWAT
extern int logFatl;
#endif
Bool QuietMode, EchoInits; /* if true, echo inits to logfile */
//function
void init_args(int argc, char **argv);

/* =================================================== */
/*                Module-Level Declarations            */
/* --------------------------------------------------- */

static void check_log(void);
static void usage(void) {
	char *s1 = "Soil water model version 2.2a (SGS-LTER Oct-2003).\n"
			"Usage: soilwat [-d startdir] [-f files.in] [-e] [-q]\n"
			"  -d : operate (chdir) in startdir (default=.)\n"
			"  -f : supply list of input files (default=files.in)\n"
			"       a preceeding path applies to all input files\n"
			"  -e : echo initial values from site and estab to logfile\n"
			"  -q : quiet mode, don't print message to check logfile.\n";
#ifndef RSOILWAT
	fprintf(stderr, "%s", s1);
	exit(0);
#else
	Rprintf("%s", s1);
	Rprintf("EXIT 0");
	warning("");
	//exit(0);
#endif
}

char _firstfile[1024];

#ifndef RSOILWAT
/************  Main() ************************/
int main(int argc, char **argv) {
	/* =================================================== */

	logged = FALSE;
	atexit(check_log);
	logfp = stdout; /* provides a way to inform user that something */
	/* was logged.  can be changed by code (eg init file */
	/* but must be set before init_args().  see generic.h */

	init_args(argc, argv);

	SW_CTL_init_model(_firstfile);

	SW_CTL_main();
	SW_SIT_clear_layers();
	SW_WTH_clear_runavg_list();

	return 0;
}
/*********** End of Main() *******************/

static void check_log(void) {
	/* =================================================== */
	/* function to be called by atexit() so it's the last
	 * to execute before termination.  This is the place to
	 * do any cleanup or progress reporting.
	 */
	if (logfp != stdout && logfp != stderr) {
		if (logged && !QuietMode)
			fprintf(stderr, "\nCheck logfile for error or status messages.\n");
		CloseFile(&logfp);
	}

}
#endif
void init_args(int argc, char **argv) {
	/* =================================================== */
	/* to add an option:
	 *  - include it in opts[]
	 *  - set a flag in valopts indicating no value (0),
	 *    value required (1), or value optional (-1),
	 *  - then tell us what to do in the switch statement
	 *
	 * 3/1/03 - cwb - Current options are
	 *                -d=chg to work dir <opt=dir_name>
	 *                -f=chg deflt first file <opt=file.in>
	 *                -q=quiet, don't print "Check logfile"
	 *                   at end of program.
	 */
	char str[1024], *opts[] = { "-d", "-f", "-e", "-q" }; /* valid options */
	int valopts[] = { 1, 1, 0, 0 }; /* indicates options with values */
	/* 0=none, 1=required, -1=optional */
	int i, /* looper through all cmdline arguments */
	a, /* current valid argument-value position */
	op, /* position number of found option */
	nopts = sizeof(opts) / sizeof(char *);

	/* Defaults */
	strcpy(_firstfile, DFLT_FIRSTFILE);
	QuietMode = EchoInits = FALSE;

	a = 1;
	for (i = 1; i <= nopts; i++) {
		if (a >= argc)
			break;

		/* figure out which option by its position 0-(nopts-1) */
		for (op = 0; op < nopts; op++) {
			if (strncmp(opts[op], argv[a], 2) == 0)
				break; /* found it, move on */
		}
		if (op == nopts) {
#ifndef RSOILWAT
			fprintf(stderr, "Invalid option %s\n", argv[a]);
			usage();
			exit(-1);
#else
			Rprintf("Invalid option %s\n", argv[a]);
			usage();
			Rprintf("EXIT -1");
			error("options");
#endif
		}

		*str = '\0';
		/* extract value part of option-value pair */
		if (valopts[op]) {
			if ('\0' != argv[a][2]) { /* no space betw opt-value */
				strcpy(str, (argv[a] + 2));

			} else if ('-' != *argv[a + 1]) { /* space betw opt-value */
				strcpy(str, argv[++a]);

			} else if (0 < valopts[op]) { /* required opt-val not found */
#ifndef RSOILWAT
				fprintf(stderr, "Incomplete option %s\n", opts[op]);
				usage();
				exit(-1);
#else
				Rprintf("Incomplete option %s\n", opts[op]);
				usage();
				Rprintf("EXIT -1");
				error("options");
#endif
			} /* opt-val not required */
		}

		/* tell us what to do here                   */
		/* set indicators/variables based on results */
		switch (op) {
		case 0: /* -d */
			if (!ChDir(str)) {
				LogError(stderr, LOGFATAL, "Invalid project directory (%s)", str);
			}
			break;
		case 1:
			strcpy(_firstfile, str);
			break; /* -f */
		case 2:
			EchoInits = TRUE;
			break; /* -e */
		case 3:
			QuietMode = TRUE;
			break; /* -q */
		default:
			LogError(logfp, LOGFATAL, "Programmer: bad option in main:init_args:switch");
		}

		a++; /* move to next valid arg-value position */

	} /* end for(i) */

}

