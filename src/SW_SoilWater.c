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
 History:
 (8/28/01) -- INITIAL CODING - cwb
 10/04/2010	(drs) added snowMAUS snow accumulation, sublimation and melt algorithm: Trnka, M., Kocmánková, E., Balek, J., Eitzinger, J., Ruget, F., Formayer, H., Hlavinka, P., Schaumberger, A., Horáková, V., Mozny, M. & Zalud, Z. (2010) Simple snow cover model for agrometeorological applications. Agricultural and Forest Meteorology, 150, 1115-1127.
 replaced SW_SWC_snow_accumulation, SW_SWC_snow_sublimation, and SW_SWC_snow_melt with SW_SWC_adjust_snow (temp_min, temp_max, *ppt, *rain, *snow, *snowmelt)
 10/19/2010	(drs) replaced snowMAUS simulation with SWAT2K routines: Neitsch S, Arnold J, Kiniry J, Williams J. 2005. Soil and water assessment tool (SWAT) theoretical documentation. version 2005. Blackland Research Center, Texas Agricultural Experiment Station: Temple, TX.
 10/25/2010	(drs)	in SW_SWC_water_flow(): replaced test that "swc can't be adjusted on day 1 of year 1" to "swc can't be adjusted on start day of first year of simulation"
 01/04/2011	(drs) added parameter '*snowloss' to function SW_SWC_adjust_snow()
 08/22/2011	(drs) added function RealD SW_SnowDepth( RealD SWE, RealD snowdensity)
 01/20/2012	(drs)	in function 'SW_SnowDepth': catching division by 0 if snowdensity is 0
 02/03/2012	(drs)	added function 'RealD SW_SWC_SWCres(RealD sand, RealD clay, RealD porosity)': which calculates 'Brooks-Corey' residual volumetric soil water based on Rawls & Brakensiek (1985)
 05/25/2012  (DLM) edited SW_SWC_read(void) function to get the initial values for soil temperature from SW_Site
 04/16/2013	(clk)	Changed SW_SWC_vol2bars() to SW_SWCbulk2SWPmatric(), and changed the code to incorporate the fraction of gravel content in the soil
 theta1 = ... / (1. - fractionVolBulk_gravel)
 Changed SW_SWC_bars2vol() to SW_SWPmatric2VWCBulk(), and changed the code to incorporate the fraction of gravel content in the soil
 t = ... * (1. - fractionVolBulk_gravel)
 Changed SW_SWC_SWCres() to SW_VWCBulkRes(), and changed the code to incorporate the fraction of gravel content in the soil
 res = ... * (1. - fractionVolBulk_gravel)
 Updated the use of these three function in all files
 06/24/2013	(rjm)	made temp_snow a module-level static variable (instead of function-level): otherwise it will not get reset to 0 between consecutive calls as a dynamic library
 need to set temp_snow to 0 in function SW_SWC_construct()
 06/26/2013	(rjm)	closed open files at end of functions SW_SWC_read(), _read_hist() or if LogError() with LOGFATAL is called
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
#include "generic.h"
#include "filefuncs.h"
#include "myMemory.h"
#include "SW_Defines.h"
#include "SW_Files.h"
#include "SW_Model.h"
#include "SW_Site.h"
#include "SW_SoilWater.h"
#include "SW_Output.h"

void SW_Water_Flow(void); /* see Water_Flow.c */

/* =================================================== */
/*                  Global Variables                   */
/* --------------------------------------------------- */

extern SW_MODEL SW_Model;
extern SW_SITE SW_Site;
#ifdef RSOILWAT
extern Bool useFiles;
extern SEXP InputData;
#endif
//extern SW_OUTPUT SW_Output[];
SW_SOILWAT SW_Soilwat; /* declared here, externed elsewhere */

/* =================================================== */
/*                Module-Level Variables               */
/* --------------------------------------------------- */
static char *MyFileName;
static RealD temp_snow;
#ifdef RSOILWAT
static int swcdataIndex;
#endif

/* =================================================== */
/* =================================================== */
/*             Private Function Definitions            */
/* --------------------------------------------------- */

static void _read_hist(TimeInt year);

static void _clear_hist(void) {
	/* --------------------------------------------------- */
	TimeInt d;
	LyrIndex z;
	for (d = 0; d < MAX_DAYS; d++) {
		for(z=0; z<MAX_LAYERS; z++)
		{
			SW_Soilwat.hist.swc[d][z] = SW_MISSING;
			SW_Soilwat.hist.std_err[d][z] = SW_MISSING;
		}
	}
}

/* =================================================== */
/* =================================================== */
/*             Public Function Definitions             */
/* --------------------------------------------------- */

void SW_SWC_construct(void) {
	/* =================================================== */
	temp_snow = 0.;
	if (!isnull(SW_Soilwat.hist.file_prefix)) {//Clear memory before setting it
		Mem_Free(SW_Soilwat.hist.file_prefix);
		SW_Soilwat.hist.file_prefix = NULL;
	}
	memset(&SW_Soilwat, 0, sizeof(SW_SOILWAT));
	#ifdef RSOILWAT
	swcdataIndex = 0;
	#endif
}

void SW_SWC_water_flow(void) {
	/* =================================================== */
	/* Adjust SWC according to historical (measured) data
	 * if available, compute water flow, and check if swc
	 * is above threshold for "wet" condition.
	 */

	LyrIndex i;

	/* if there's no swc observation for today,
	 * it shows up as SW_MISSING.  The input must
	 * define historical swc for at least the top
	 * layer to be recognized.
	 * IMPORTANT: swc can't be adjusted on day 1 of first year of simulation.
	 10/25/2010	(drs)	in SW_SWC_water_flow(): replaced test that "swc can't be adjusted on day 1 of year 1" to "swc can't be adjusted on start day of first year of simulation"
	 */

	if (SW_Soilwat.hist_use && !missing( SW_Soilwat.hist.swc[SW_Model.doy-1][1])) {

		if (!(SW_Model.doy == SW_Model.startstart && SW_Model.year == SW_Model.startyr)) {

			SW_SWC_adjust_swc(SW_Model.doy);

		} else {
			LogError(logfp, LOGWARN, "Attempt to set SWC on start day of first year of simulation disallowed.");
		}

	} else {
		SW_Water_Flow();
	}

	ForEachSoilLayer(i)
		SW_Soilwat.is_wet[i] = (GE( SW_Soilwat.swcBulk[Today][i],
				SW_Site.lyr[i]->swcBulk_wet));
}

void SW_SWC_end_day(void) {
	/* =================================================== */
	SW_SOILWAT *v = &SW_Soilwat;
	LyrIndex i;

	ForEachSoilLayer(i)
		v->swcBulk[Yesterday][i] = v->swcBulk[Today][i];

	v->snowpack[Yesterday] = v->snowpack[Today];

}

void SW_SWC_new_year(void) {
	/* =================================================== */
	/* init first doy swc, either by the computed init value
	 * or by the last day of last year, which is also,
	 * coincidentally, Yesterday.
	 */

	LyrIndex lyr;
	TimeInt year = SW_Model.year;
	Bool reset = (SW_Site.reset_yr || SW_Model.year == SW_Model.startyr);

	memset(&SW_Soilwat.yrsum, 0, sizeof(SW_SOILWAT_OUTPUTS));

	/* reset the swc */ForEachSoilLayer(lyr)
	{
		if (reset) {
			SW_Soilwat.swcBulk[Today][lyr] = SW_Soilwat.swcBulk[Yesterday][lyr] = SW_Site.lyr[lyr]->swcBulk_init;
			SW_Soilwat.drain[lyr] = 0.;
		} else {
			SW_Soilwat.swcBulk[Today][lyr] = SW_Soilwat.swcBulk[Yesterday][lyr];
		}
	}

	/* reset the snowpack */
	if (reset) {
		SW_Soilwat.snowpack[Today] = SW_Soilwat.snowpack[Yesterday] = 0.;
	} else {
		SW_Soilwat.snowpack[Today] = SW_Soilwat.snowpack[Yesterday];
	}

	/* reset the historical (measured) values, if needed */
	if (SW_Soilwat.hist_use && year >= SW_Soilwat.hist.yr.first) {
#ifndef RSOILWAT
		_read_hist(year);
#else
		if(useFiles) {
			_read_hist(year);
		} else {
			onSet_SW_SWC_hist(VECTOR_ELT(VECTOR_ELT(VECTOR_ELT(InputData,7),4),swcdataIndex));
			swcdataIndex++;
		}
#endif
	}

	/* always reset deep drainage */
	if (SW_Site.deepdrain)
		SW_Soilwat.swcBulk[Today][SW_Site.deep_lyr] = 0.;
}

void SW_SWC_read(void) {
	/* =================================================== */
	/* Like all of the other "objects", read() reads in the
	 * setup parameters.  See _read_hist() for reading
	 * historical files.
	 *
	 *  1/25/02 - cwb - removed unused records of logfile and
	 *          start and end days.  also removed the SWTIMES dy
	 *          structure element.
	 */

	SW_SOILWAT *v = &SW_Soilwat;
	FILE *f;
	int lineno = 0, nitems = 4;
// gets the soil temperatures from where they are read in the SW_Site struct for use later
// SW_Site.c must call it's read function before this, or it won't work
	LyrIndex i;
	ForEachSoilLayer(i)
		v->sTemp[i] = SW_Site.lyr[i]->sTemp;

	MyFileName = SW_F_name(eSoilwat);
	f = OpenFile(MyFileName, "r");

	while (GetALine(f, inbuf)) {
		switch (lineno) {
		case 0:
			v->hist_use = (atoi(inbuf)) ? TRUE : FALSE;
			break;
		case 1:
			v->hist.file_prefix = (char *) Str_Dup(inbuf);
			break;
		case 2:
			v->hist.yr.first = yearto4digit(atoi(inbuf));
			break;
		case 3:
			v->hist.method = atoi(inbuf);
			break;
		}
		lineno++;
	}
	if(!v->hist_use) {
		CloseFile(&f);
		return;
	}
	if (lineno < nitems) {
		CloseFile(&f);
		LogError(logfp, LOGFATAL, "%s : Insufficient parameters specified.", MyFileName);
	}
	if (v->hist.method < 1 || v->hist.method > 2) {
		CloseFile(&f);
		LogError(logfp, LOGFATAL, "%s : Invalid swc adjustment method.", MyFileName);
	}
	v->hist.yr.last = SW_Model.endyr;
	v->hist.yr.total = v->hist.yr.last - v->hist.yr.first + 1;
	CloseFile(&f);
}
#ifdef RSOILWAT
SEXP onGet_SW_SWC() {
	int i;
	SW_SOILWAT *v = &SW_Soilwat;
	SEXP swSWC;
	SEXP SWC, SWC_names;
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
	SW_SOILWAT *v = &SW_Soilwat;
	SEXP swcUseData;
	SEXP swcFilePrefix;
	SEXP swcFirstYear;
	SEXP swcMethod;

	MyFileName = SW_F_name(eSoilwat);
	LyrIndex i;
	ForEachSoilLayer(i)
		v->sTemp[i] = SW_Site.lyr[i]->sTemp;

	PROTECT(swcUseData = GET_SLOT(SWC, install("UseSWCHistoricData")));
	PROTECT(swcFilePrefix = GET_SLOT(SWC, install("DataFilePrefix")));
	PROTECT(swcFirstYear = GET_SLOT(SWC, install("FirstYear")));
	PROTECT(swcMethod = GET_SLOT(SWC, install("Method")));

	v->hist_use = LOGICAL(swcUseData)[0];
	//if (!isnull(v->hist.file_prefix)) {//Clear memory before setting it
	//	Mem_Free(v->hist.file_prefix);
	//}
	v->hist.file_prefix = (char *) Str_Dup(CHAR(STRING_ELT(swcFilePrefix,0)));
	v->hist.yr.first = INTEGER(swcFirstYear)[0];
	v->hist.method = INTEGER(swcMethod)[0];

	if (v->hist.method < 1 || v->hist.method > 2) {
		LogError(logfp, LOGFATAL, "swcsetup.in : Invalid swc adjustment method.");
	}
	v->hist.yr.last = SW_Model.endyr;
	v->hist.yr.total = v->hist.yr.last - v->hist.yr.first + 1;
	UNPROTECT(4);
}
#endif
static void _read_hist(TimeInt year) {
	/* =================================================== */
	/* read a file containing historical swc measurements.
	 * Enter with year a four digit year number.  This is
	 * appended to the swc prefix to make the input file
	 * name.
	 *
	 *
	 * 1/25/02 - cwb - removed year field from input records.
	 *         This code uses GetALine() which discards comments
	 *         and only one year's data per file is allowed, so
	 *         and the date is part of the file name, but if you
	 *         must, you can add the date as well as other data
	 *         inside comments, preferably at the top of the file.
	 *
	 * Format of the input file is
	 * "doy layer swc stderr"
	 *
	 * for example,
	 *  90 1 1.11658 .1
	 *  90 2 1.11500 .1
	 *    ...
	 * 185 1 2.0330  .23
	 * 185 2 3.1432  .25
	 *    ...
	 * note that missing days or layers will not cause
	 * an error in the input, but missing layers could
	 * cause problems in the flow model.
	 */
	SW_SOILWAT *v = &SW_Soilwat;
	FILE *f;
	TimeInt doy;
	int x, lyr, recno = 0;
	RealF swc, st_err;
	char fname[MAX_FILENAMESIZE];

	sprintf(fname, "%s.%4d", v->hist.file_prefix, year);

	if (!FileExists(fname)) {
		LogError(logfp, LOGWARN, "Historical SWC file %s not found.", fname);
		return;
	}

	f = OpenFile(fname, "r");

	_clear_hist();

	while (GetALine(f, inbuf)) {
		recno++;
		x = sscanf(inbuf, "%d %d %f %f", &doy, &lyr, &swc, &st_err);
		if (x < 4) {
			CloseFile(&f);
			LogError(logfp, LOGFATAL, "%s : Incomplete layer data at record %d\n   Should be DOY LYR SWC STDERR.", fname, recno);
		}
		if (x > 4) {
			CloseFile(&f);
			LogError(logfp, LOGFATAL, "%s : Too many input fields at record %d\n   Should be DOY LYR SWC STDERR.", fname, recno);
		}
		if (doy < 1 || doy > MAX_DAYS) {
			CloseFile(&f);
			LogError(logfp, LOGFATAL, "%s : Day of year out of range at record %d", fname, recno);
		}
		if (lyr < 1 || lyr > MAX_LAYERS) {
			CloseFile(&f);
			LogError(logfp, LOGFATAL, "%s : Layer number out of range (%d > %d), record %d\n", fname, lyr, MAX_LAYERS, recno);
		}

		v->hist.swc[doy - 1][lyr - 1] = swc;
		v->hist.std_err[doy - 1][lyr - 1] = st_err;

	}
	CloseFile(&f);
}
#ifdef RSOILWAT
SEXP onGet_SW_SWC_hists() {
	TimeInt year;
	SEXP SWC_hists, SWC_hists_names;
	int years = ((SW_Model.endyr + 1) - SW_Model.startyr), i = 0;
	char cYear[5];

	PROTECT(SWC_hists_names = allocVector(STRSXP, years));
	PROTECT(SWC_hists = allocVector(VECSXP,years));

	for (year = SW_Model.startyr; year <= SW_Model.endyr; year++) {
		if (SW_Soilwat.hist_use && year >= SW_Soilwat.hist.yr.first) {
			_read_hist(year);
			SET_VECTOR_ELT(SWC_hists, i, onGet_SW_SWC_hist(year));
			sprintf(cYear, "%4d", year);
			SET_STRING_ELT(SWC_hists_names, i, mkChar(cYear));
		}
		i++;
	}

	UNPROTECT(2);
	return SWC_hists;
}
SEXP onGet_SW_SWC_hist(TimeInt year) {
	int i, j;
	SW_SOILWAT *v = &SW_Soilwat;
	SEXP swSWC_hist;
	SEXP hist;
	char *cSWC_hist[] = { "doy", "lyr", "swc", "st_err" };
	SEXP lyrs, lyrs_names, lyrs_names_y;
	RealD *p_lyrs;

	PROTECT(swSWC_hist = MAKE_CLASS("swSWC_hist"));
	PROTECT(hist = NEW_OBJECT(swSWC_hist));

	PROTECT(lyrs = allocMatrix(REALSXP, MAX_LAYERS*MAX_DAYS, 4));
	p_lyrs = REAL(lyrs);

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
void onSet_SW_SWC_hist(SEXP lyrs) {
	int i, j;
	SW_SOILWAT *v = &SW_Soilwat;
	RealD *p_lyrs;

	p_lyrs = REAL(lyrs);
	for (i = 0; i < MAX_DAYS * MAX_LAYERS; i++) {
		v->hist.swc[(int) (i / MAX_LAYERS)][(int) (j % MAX_LAYERS)] = p_lyrs[i + MAX_DAYS * MAX_LAYERS * 2];
		v->hist.std_err[(int) (i / MAX_LAYERS)][(int) (j % MAX_LAYERS)] = p_lyrs[i + MAX_DAYS * MAX_LAYERS * 3];
	}
}
#endif
void SW_SWC_adjust_swc(TimeInt doy) {
	/* =================================================== */
	/* 01/07/02 (cwb) added final loop to guarantee swc > swcBulk_min
	 */

	SW_SOILWAT *v = &SW_Soilwat;
	RealD lower, upper;
	LyrIndex lyr;
	TimeInt dy = doy - 1;

	switch (SW_Soilwat.hist.method) {
	case SW_Adjust_Avg:
		ForEachSoilLayer(lyr)
		{
			v->swcBulk[Today][lyr] += v->hist.swc[dy][lyr];
			v->swcBulk[Today][lyr] /= 2.;
		}
		break;

	case SW_Adjust_StdErr:
		ForEachSoilLayer(lyr)
		{
			upper = v->hist.swc[dy][lyr] + v->hist.std_err[dy][lyr];
			lower = v->hist.swc[dy][lyr] - v->hist.std_err[dy][lyr];
			if (GT(v->swcBulk[Today][lyr], upper))
				v->swcBulk[Today][lyr] = upper;
			else if (LT(v->swcBulk[Today][lyr], lower))
				v->swcBulk[Today][lyr] = lower;
		}
		break;

	default:
		LogError(logfp, LOGFATAL, "%s : Invalid SWC adjustment method.", SW_F_name(eSoilwat));
	}

	/* this will guarantee that any method will not lower swc */
	/* below the minimum defined for the soil layers          */
	ForEachSoilLayer(lyr)
		v->swcBulk[Today][lyr] = fmax(v->swcBulk[Today][lyr], SW_Site.lyr[lyr]->swcBulk_min);

}

void SW_SWC_adjust_snow(RealD temp_min, RealD temp_max, RealD ppt, RealD *rain, RealD *snow, RealD *snowmelt, RealD *snowloss) {
	/*---------------------
	 10/04/2010	(drs) added snowMAUS snow accumulation, sublimation and melt algorithm: Trnka, M., Kocmánková, E., Balek, J., Eitzinger, J., Ruget, F., Formayer, H., Hlavinka, P., Schaumberger, A., Horáková, V., Mozny, M. & Zalud, Z. (2010) Simple snow cover model for agrometeorological applications. Agricultural and Forest Meteorology, 150, 1115-1127.
	 replaced SW_SWC_snow_accumulation, SW_SWC_snow_sublimation, and SW_SWC_snow_melt with SW_SWC_adjust_snow
	 10/19/2010	(drs) replaced snowMAUS simulation with SWAT2K routines: Neitsch S, Arnold J, Kiniry J, Williams J. 2005. Soil and water assessment tool (SWAT) theoretical documentation. version 2005. Blackland Research Center, Texas Agricultural Experiment Station: Temple, TX.
	 Inputs:	temp_min: daily minimum temperature (C)
	 temp_max: daily maximum temperature (C)
	 ppt: daily precipitation (cm)
	 snowpack[Yesterday]: yesterday's snowpack (water-equivalent cm)
	 Outputs:	snowpack[Today], partitioning of ppt into rain and snow, snowmelt and snowloss
	 ---------------------*/

	RealD *snowpack = &SW_Soilwat.snowpack[Today], doy = SW_Model.doy, temp_ave, Rmelt, snow_cov = 1., cov_soil = 0.5, SnowAccu = 0., SnowMelt = 0., SnowLoss = 0.;

	temp_ave = (temp_min + temp_max) / 2.;
	/* snow accumulation */
	if (LE(temp_ave, SW_Site.TminAccu2)) {
		SnowAccu = ppt;
	} else {
		SnowAccu = 0.;
	}
	*rain = fmax(0., ppt - SnowAccu);
	*snow = fmax(0., SnowAccu);
	*snowpack += SnowAccu;

	/* snow melt */
	Rmelt = (SW_Site.RmeltMax + SW_Site.RmeltMin) / 2. + sin((doy - 81.) / 58.09) * (SW_Site.RmeltMax - SW_Site.RmeltMin) / 2.;
	temp_snow = temp_snow * (1 - SW_Site.lambdasnow) + temp_ave * SW_Site.lambdasnow;
	if (GT(temp_snow, SW_Site.TmaxCrit)) {
		SnowMelt = fmin( *snowpack, Rmelt * snow_cov * ((temp_snow + temp_max)/2. - SW_Site.TmaxCrit) );
	} else {
		SnowMelt = 0.;
	}
	if (GT(*snowpack, 0.)) {
		*snowmelt = fmax(0., SnowMelt);
		*snowpack = fmax(0., *snowpack - *snowmelt );
	} else {
		*snowmelt = 0.;
	}

	/* snow loss through sublimation and other processes */
	SnowLoss = fmin( *snowpack, cov_soil * SW_Soilwat.pet );
	if (GT(*snowpack, 0.)) {
		*snowloss = fmax(0., SnowLoss);
		*snowpack = fmax(0., *snowpack - *snowloss );
	} else {
		*snowloss = 0.;
	}

}

RealD SW_SnowDepth(RealD SWE, RealD snowdensity) {
	/*---------------------
	 08/22/2011	(drs)	calculates depth of snowpack
	 Input:	SWE: snow water equivalents (cm = 10kg/m2)
	 snowdensity (kg/m3)
	 Output: snow depth (cm)
	 ---------------------*/
	if (GT(snowdensity, 0.)) {
		return SWE / snowdensity * 10. * 100.;
	} else {
		return 0.;
	}
}

RealD SW_SWCbulk2SWPmatric(RealD fractionGravel, RealD swcBulk, LyrIndex n) {
	/**********************************************************************
	 PURPOSE: Calculate the soil water potential or the soilwater
	 content of the current layer,
	 as a function of soil texture at the layer.

	 DATE:  April 2, 1992

	 HISTORY:
	 9/1/92  (SLC) if swc comes in as zero, set swpotentl to
	 upperbnd.  (Previously, we flagged this
	 as an error, and set swpotentl to zero).

	 27-Aug-03 (cwb) removed the upperbnd business. Except for
	 missing values, swc < 0 is impossible, so it's an error,
	 and the previous limit of swp to 80 seems unreasonable.
	 return 0.0 if input value is MISSING

	 INPUTS:
	 swcBulk - soilwater content of the current layer (cm/layer)
	 n   - layer number to index the **lyr pointer.

	 These are the values for each layer obtained via lyr[n]:
	 width  - width of current soil layer
	 psisMatric   - "saturation" matric potential
	 thetasMatric - saturated moisture content.
	 bMatric       - see equation below.
	 swc_lim - limit for matric potential

	 LOCAL VARIABLES:
	 theta1 - volumetric soil water content

	 DEFINED CONSTANTS:
	 barconv - conversion factor from bars to cm water.  (i.e.
	 1 bar = 1024cm water)

	 COMMENT:
	 See the routine "watreqn" for a description of how the variables
	 psisMatric, bMatric, binverseMatric, thetasMatric are initialized.

	 OUTPUTS:
	 swpotentl - soilwater potential of the current layer
	 (if swflag=TRUE)
	 or
	 soilwater content (if swflag=FALSE)

	 DESCRIPTION: The equation and its coefficients are based on a
	 paper by Cosby,Hornberger,Clapp,Ginn,  in WATER RESOURCES RESEARCH
	 June 1984.  Moisture retention data was fit to the power function

	 **********************************************************************/

	SW_LAYER_INFO *lyr = SW_Site.lyr[n];
	float theta1, swp = 0.;

	if (missing(swcBulk) || ZRO(swcBulk))
		return 0.0;

	if (GT(swcBulk, 0.0)) {
		if (EQ(fractionGravel,1.0))
			theta1 = 0.0;
		else
			theta1 = (swcBulk / lyr->width) * 100. / (1. - fractionGravel);
		swp = lyr->psisMatric / powe(theta1/lyr->thetasMatric, lyr->bMatric) / BARCONV;
	} else {
		LogError(logfp, LOGFATAL, "Invalid SWC value (%.4f) in SW_SWC_swc2potential.\n"
				"    Year = %d, DOY=%d, Layer = %d\n", swcBulk, SW_Model.year, SW_Model.doy, n);
	}

	return swp;
}

RealD SW_SWPmatric2VWCBulk(RealD fractionGravel, RealD swpMatric, LyrIndex n) {
	/* =================================================== */
	/* used to be swfunc in the fortran version */
	/* 27-Aug-03 (cwb) moved from the Site module. */
	/* return the volume as cmH2O/cmSOIL */

	SW_LAYER_INFO *lyr = SW_Site.lyr[n];
	RealD t, p;

	swpMatric *= BARCONV;
	p = powe(lyr->psisMatric / swpMatric, lyr->binverseMatric);
	t = lyr->thetasMatric * p * 0.01 * (1 - fractionGravel);
	return (t);

}

RealD SW_VWCBulkRes(RealD fractionGravel, RealD sand, RealD clay, RealD porosity) {
	/*---------------------
	 02/03/2012	(drs)	calculates 'Brooks-Corey' residual volumetric soil water based on Rawls WJ, Brakensiek DL (1985) Prediction of soil water properties for hydrological modeling. In Watershed management in the Eighties (eds Jones EB, Ward TJ), pp. 293-299. American Society of Civil Engineers, New York.
	 however, equation is only valid if (0.05 < clay < 0.6) & (0.05 < sand < 0.7)

	 Input:	sand: soil texture sand content (fraction)
	 clay: soil texture clay content (fraction)
	 porosity: soil porosity = saturated VWC (fraction)
	 Output: residual volumetric soil water (cm/cm)
	 ---------------------*/
	RealD res;

	sand *= 100.;
	clay *= 100.;

	res = (-0.0182482 + 0.00087269 * sand + 0.00513488 * clay + 0.02939286 * porosity - 0.00015395 * squared(clay) - 0.0010827 * sand * porosity
			- 0.00018233 * squared(clay) * squared(porosity) + 0.00030703 * squared(clay) * porosity - 0.0023584 * squared(porosity) * clay) * (1 - fractionGravel);

	return (fmax(res, 0.));
}

#ifdef DEBUG_MEM
#include "myMemory.h"
/*======================================================*/
void SW_SWC_SetMemoryRefs( void) {
	/* when debugging memory problems, use the bookkeeping
	 code in myMemory.c
	 This routine sets the known memory refs in this module
	 so they can be  checked for leaks, etc.  Includes
	 malloc-ed memory in SOILWAT.  All refs will have been
	 cleared by a call to ClearMemoryRefs() before this, and
	 will be checked via CheckMemoryRefs() after this, most
	 likely in the main() function.
	 */

	/*  NoteMemoryRef(SW_Soilwat.hist.file_prefix); */

}

#endif
