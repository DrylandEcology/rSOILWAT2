/********************************************************/
/********************************************************/
/*	Source file: Files.c
 Type: module
 Application: SOILWAT - soilwater dynamics simulator
 Purpose: Read / write and otherwise manage the model's
 parameter file information.
 History:
 8/28/01 -- INITIAL CODING - cwb
 1/24/02 -- added facility for logfile
 10-May-02 -- Added conditionals for interfacing STEPPE.
 09/30/2011	(drs)	added function SW_WeatherPrefix(): so that SW_Weather can access local variable weather_prefix that is read in now in SW_F_read()
 new module-level variable static char weather_prefix[FILENAME_MAX]; read in in function SW_F_read() from file files.in line 6
 09/30/2011	(drs)	added function SW_OutputPrefix(): so that SW_Output can access local variable output_prefix that is read in now in SW_F_read()
 new module-level variable static char output_prefix[FILENAME_MAX]; read in in function SW_F_read() from file files.in line 12: / for same directory, or e.g., Output/
 */
/********************************************************/
/********************************************************/

/* =================================================== */
/* =================================================== */
/*                INCLUDES / DEFINES                   */

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "generic.h"
#include "filefuncs.h"
#include "myMemory.h"
#include "SW_Defines.h"
#include "SW_Files.h"

/* =================================================== */
/*                  Global Variables                   */
/* --------------------------------------------------- */

/* =================================================== */
/*                Module-Level Variables               */
/* --------------------------------------------------- */
static char *MyFileName;
static char *InFiles[SW_NFILES];
static char _ProjDir[FILENAME_MAX];
static char weather_prefix[FILENAME_MAX];
static char output_prefix[FILENAME_MAX];

/* =================================================== */
/* =================================================== */
/*             Private Function Definitions            */
/* --------------------------------------------------- */
static void init(const char *s) {
	/* --------------------------------------------------- */
	/* sets the name of the first input file. If called
	 * with s==NULL the name is set to "files.in".
	 *
	 * 1/24/02 - replaced [re]alloc with StrDup()
	 */
	char fname[MAX_FILENAMESIZE] = { '\0' };

	if (NULL == InFiles[eFirst])
		strcpy(fname, (s ? s : "files.in"));
	else if (s && strcmp(s, InFiles[eFirst]))
		strcpy(fname, s);

	if (*fname) {
		if (!isnull(InFiles[eFirst]))
			Mem_Free(InFiles[eFirst]);
		InFiles[eFirst] = Str_Dup(fname);
	}

}

/* =================================================== */
/* =================================================== */
/*             Public Function Definitions             */
/* --------------------------------------------------- */

void SW_F_read(const char *s) {
	/* =================================================== */
	/* enter with the name of the first file to read for
	 * the filenames, or NULL.  If null, then read files.in
	 * or whichever filename was set previously. see init().
	 *
	 * 1/24/02 - replaced [re]alloc with StrDup()
	 *         - added facility for log-to-file. logfp depends
	 *             on having executed SW_F_read().
	 */

	FILE *f;
	int lineno = 0, fileno = 0;
	char buf[FILENAME_MAX];

	if (!isnull(s))
		init(s); /* init should be run by SW_F_Construct() */

	MyFileName = SW_F_name(eFirst);
	f = OpenFile(MyFileName, "r");

	while (GetALine(f, inbuf)) {

		switch (lineno) {
		case 5:
			strcpy(weather_prefix, inbuf);
			break;
		case 12:
			strcpy(output_prefix, inbuf);
			break;

		default:
			if (++fileno == SW_NFILES)
				break;

			if (!isnull(InFiles[fileno]))
				Mem_Free(InFiles[fileno]);
			strcpy(buf, _ProjDir);
			strcat(buf, inbuf);
			InFiles[fileno] = Str_Dup(buf);
		}

		lineno++;
	}

	if (fileno < eEndFile - 1) {
		CloseFile(&f);
		LogError(stdout, LOGFATAL, "Too few files (%d) in %s", fileno, MyFileName);
	}

	CloseFile(&f);

#if !defined(STEPWAT) && !defined(RSOILWAT)
	if (0 == strcmp(InFiles[eLog], "stdout")) {
		logfp = stdout;
	} else if (0 == strcmp(InFiles[eLog], "stderr")) {
		logfp = stderr;
	} else {
		logfp = OpenFile(SW_F_name(eLog), "w");
	}
#endif

}

char *SW_F_name(SW_FileIndex i) {
	/* =================================================== */
	return InFiles[i];

}

void SW_F_construct(const char *firstfile) {
	/* =================================================== */
	/* 10-May-02 (cwb) enhancement allows model to be run
	 *    in one directory while getting its input from another.
	 *    This was done mostly in support of STEPWAT but
	 *    it could be useful in a standalone run.
	 */
	char *c, *p;

	init(firstfile);

	if ((c = DirName(firstfile))) {
		strcpy(_ProjDir, c);
		c = (char *) firstfile;
		p = c + strlen(_ProjDir);
		while (*p)
			*(c++) = *(p++);
		*c = '\0';
	} else
		_ProjDir[0] = '\0';

}
#ifdef RSOILWAT
SEXP onGet_SW_F() {
	int i = 0, j = 0;

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
	SET_STRING_ELT(ProjDir, 0, mkChar(_ProjDir));
	for (i = 0; i < 15; i++)
		if (InFiles[i] != NULL )
			j++;
	PROTECT(FilesIn = allocVector(STRSXP, j));

	for (i = 0; i < 15; i++) {
		if (InFiles[i] != NULL ) {
			SET_STRING_ELT(FilesIn, i, mkChar(InFiles[i]));
		}
	}
	PROTECT(Rweather_prefix = allocVector(STRSXP, 1));
	SET_STRING_ELT(Rweather_prefix, 0, mkChar(weather_prefix));
	PROTECT(Routput_prefix = allocVector(STRSXP, 1));
	SET_STRING_ELT(Routput_prefix, 0, mkChar(output_prefix));
	// attaching main's elements
	SET_SLOT(SW_F_construct, install(cSW_F_construct_names[0]), ProjDir);
	SET_SLOT(SW_F_construct, install(cSW_F_construct_names[1]), FilesIn);
	SET_SLOT(SW_F_construct, install(cSW_F_construct_names[2]), Rweather_prefix);
	SET_SLOT(SW_F_construct, install(cSW_F_construct_names[3]), Routput_prefix);
	//PROTECT(SW_F_construct_names = allocVector(STRSXP, 4));
	//for (i = 0; i < 4; i++)
	//	SET_STRING_ELT(SW_F_construct_names, i, mkChar(cSW_F_construct_names[i]));
	//setAttrib(SW_F_construct, R_NamesSymbol, SW_F_construct_names);

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
	//_ProjDir = R_alloc(strlen(CHAR(STRING_ELT(ProjDir,0))));
	strcpy(_ProjDir, CHAR(STRING_ELT(ProjDir,0)));

	PROTECT(FilesIn = GET_SLOT(SW_F_construct, install("InFiles")));
	j = LENGTH(FilesIn);
	for(i=0;i<15;i++)
		if (!isnull(InFiles[i])) {
			Mem_Free(InFiles[i]);
		}
	for (i = 0; i < j; i++) {
		InFiles[i] = Str_Dup(CHAR(STRING_ELT(FilesIn,i)));
	}

	PROTECT(Rweather_prefix = GET_SLOT(SW_F_construct, install("WeatherPrefix")));
	//weather_prefix = R_alloc(strlen(CHAR(STRING_ELT(Rweather_prefix,0))));
	strcpy(weather_prefix, CHAR(STRING_ELT(Rweather_prefix,0)));

	PROTECT(Routput_prefix = GET_SLOT(SW_F_construct, install("OutputPrefix")));
	//output_prefix = R_alloc(strlen(CHAR(STRING_ELT(Routput_prefix,0))));
	strcpy(output_prefix, CHAR(STRING_ELT(Routput_prefix,0)));
	UNPROTECT(4);
}
#endif
void SW_WeatherPrefix(char prefix[]) {
	strcpy(prefix, weather_prefix);
}

void SW_OutputPrefix(char prefix[]) {

	if (strcmp(output_prefix, "/") == 0)
		prefix[0] = '\0';
	else
		strcpy(prefix, output_prefix);
}

#ifdef DEBUG_MEM
#include "myMemory.h"
/*======================================================*/
void SW_F_SetMemoryRefs( void) {
	/* when debugging memory problems, use the bookkeeping
	 code in myMemory.c
	 This routine sets the known memory refs in this module
	 so they can be  checked for leaks, etc.  Includes
	 malloc-ed memory in SOILWAT.  All refs will have been
	 cleared by a call to ClearMemoryRefs() before this, and
	 will be checked via CheckMemoryRefs() after this, most
	 likely in the main() function.
	 */
	SW_FileIndex i;

	for ( i=eFirst; i < eEndFile; i++)
	NoteMemoryRef(InFiles[i]);

}

#endif
