/********************************************************/
/********************************************************/
/*	Source file: Output.c
 Type: module
 Application: SOILWAT - soilwater dynamics simulator
 Purpose: Read / write and otherwise manage the
 user-specified output flags.

 COMMENTS: The algorithm for the summary bookkeeping
 is much more complicated than I'd like, but I don't
 see a cleaner way to address the need to keep
 running tabs without storing daily arrays for each
 output variable.  That might make somewhat
 simpler code, and perhaps slightly more efficient,
 but at a high cost of memory, and the original goal
 was to make this object oriented, so memory should
 be used sparingly.  Plus, much of the code is quite
 general, and the main loops are very simple indeed.

 Generally, adding a new output key is fairly simple,
 and much of the code need not be considered.
 Refer to the comment block at the very end of this
 file for details.


 History:
 9/11/01 cwb -- INITIAL CODING
 10-May-02 cwb -- Added conditionals for interfacing
 with STEPPE.  See SW_OUT_read(), SW_OUT_close_files(),
 SW_OUT_write_today(), get_temp() and get_transp().
 The changes prevent the model from opening, closing,
 and writing to files because SXW only requires periodic
 (eg, weekly) transpiration and yearly temperature
 from get_transp() and get_temp(), resp.
 --The output config file must be prepared by the SXW
 interface code such that only yearly temp and daily,
 weekly, or monthly transpiration are requested for
 periods (1,end).

 12/02 - IMPORTANT CHANGE - cwb
 refer to comments in Times.h regarding base0

 27-Aug-03 (cwb) Just a comment that this code doesn't
 handle missing values in the summaries, especially
 the averages.  This really needs to be addressed
 sometime, but for now it's the user's responsibility
 to make sure there are no missing values.  The
 model doesn't generate any on its own, but it
 still needs to be fixed, although that will take
 a bit of work to keep track of the number of
 missing days, etc.
 20090826 (drs) stricmp -> strcmp -> Str_CompareI; in SW_OUT_sum_today () added break; after default:
 20090827 (drs) changed output-strings str[12] to #define OUTSTRLEN 18; str[OUTSTRLEN]; because of sprintf(str, ...) overflow and memory corruption into for-index i
 20090909 (drs) strcmp -> Str_CompareI (not case-sensitive)
 20090915 (drs) wetdays output was not working: changed in get_wetdays() the output from sprintf(fmt, "%c%%3.0f", _Sep); to sprintf(str, "%c%i", _Sep, val);
 20091013 (drs) output period of static void get_swcm(void) was erroneously set to eSW_SWP instead of eSW_SWCM
 20091015 (drs) ppt is divided into rain and snow and all three values plust snowmelt are output into precip
 20100202 (drs) changed SW_CANOPY to SW_CANOPYEV and SW_LITTER to SW_LITTEREV;
 and eSW_Canopy to eSW_CanopyEv and eSW_Litter to eSW_LitterEv;
 added SWC_CANOPYINT, SW_LITTERINT, SW_SOILINF, SW_LYRDRAIN;
 added eSW_CanopyInt, eSW_LitterInt, eSW_SoilInf, eSW_LyrDrain;
 updated key2str, key2obj;
 added private functions get_canint(), get_litint(), get_soilinf(), get_lyrdrain();
 updated SW_OUT_construct(), sumof_swc() and average_for() with new functions and keys
 for layer drain use only for(i=0; i < SW_Site.n_layers-1; i++)
 04/16/2010 (drs) added SWC_SWA, eSW_SWA; updated key2str, key2obj; added private functions get_swa();
 updated SW_OUT_construct(), sumof_swc()[->make calculation here] and average_for() with new functions and keys
 10/20/2010 (drs) added SW_HYDRED, eSW_HydRed; updated key2str, key2obj; added private functions get_hydred();
 updated SW_OUT_construct(), sumof_swc()[->make calculation here] and average_for() with new functions and keys
 11/16/2010 (drs) added forest intercepted water to canopy interception
 updated get_canint(), SW_OUT_construct(), sumof_swc()[->make calculation here] and average_for()
 01/03/2011	(drs) changed parameter type of str2period(), str2key(), and str2type() from 'const char *' to 'char *' to avoid 'discard qualifiers from pointer target type' in Str_ComparI()
 01/05/2011	(drs) made typecast explicit: changed in SW_OUT_write_today(): SW_Output[k].pfunc() to ((void (*)(void))SW_Output[k].pfunc)(); -> doesn't solve problem under darwin10
 -> 'abort trap' was due to str[OUTSTRLEN] being too short (OUTSTRLEN 18), changed to OUTSTRLEN 100
 01/06/2011	(drs) changed all floating-point output from %7.4f to %7.6f to avoid rounding errors in post-run output calculations
 03/06/2011	(drs) in average_for() changed loop to average hydred from "for(i=0; i < SW_Site.n_layers-1; i++)" to "ForEachSoilLayer(i)" because deepest layer was not averaged
 07/22/2011 (drs) added SW_SURFACEW and SW_EVAPSURFACE, eSW_SurfaceWater and eSW_EvapSurface; updated key2str, key2obj; added private functions get_surfaceWater() and get_evapSurface();
 updated SW_OUT_construct(), sumof_swc()[->make calculation here] and average_for() with new functions and keys
 09/12/2011	(drs)	renamed SW_EVAP to SW_EVAPSOIL, and eSW_Evap to eSW_EvapSoil;
 deleted SW_CANOPYEV, eSW_CanopyEv, SW_LITTEREV, eSW_LitterEv, SW_CANOPYINT, eSW_CanopyInt, SW_LITTERINT, and eSW_LitterInt;
 added SW_INTERCEPTION and eSW_Interception
 09/12/2011	(drs)	renamed get_evap() to get_evapSoil(); renamed get_EvapsurfaceWater() to get_evapSurface()
 deleted get_canevap(), get_litevap(), get_canint(), and get_litint()
 added get_interception()
 09/12/2011	(drs)	increased #define OUTSTRLEN from 100 to 400 (since transpiration and hydraulic redistribution will be 4x as long)
 increased static char outstr[0xff] from 0xff=255 to OUTSTRLEN
 09/12/2011	(drs)	updated SW_OUT_construct(), sumof_swc()[->make calculation here] and average_for() with new functions and keys
 09/12/2011	(drs)	added snowdepth as output to snowpack, i.e., updated updated get_snowpack(), sumof_swc()[->make calculation here] and average_for()
 09/30/2011	(drs)	in SW_OUT_read(): opening of output files: SW_Output[k].outfile is extended by SW_Files.in:SW_OutputPrefix()
 01/09/2012	(drs)	'abort trap' in get_transp due to outstr[OUTSTRLEN] being too short (OUTSTRLEN 400), changed to OUTSTRLEN 1000
 05/25/2012  (DLM) added SW_SOILTEMP to keytostr, updated keytoobj;  wrote get_soiltemp(void) function, added code in the _construct() function to link to get_soiltemp() correctly;  added code to sumof and average_for() to handle the new key
 05/25/2012  (DLM) added a few lines of nonsense to sumof_ves() function in order to get rid of the annoying compiler warnings
 06/12/2012  (DLM) changed OUTSTRLEN from 1000 to 2000
 07/02/2012  (DLM) updated # of chars in keyname & upkey arrays in SW_OUT_READ() function to account for longer keynames... for some reason it would still read them in correctly on OS X without an error, but wouldn't on JANUS.
 11/30/2012	(clk)	changed get_surfaceWater() to ouput amound of surface water, surface runoff, and snowmelt runoff, respectively.
 12/13/2012	(clk, drs)	changed get_surfaceWater() to output amount of surface water
 added get_runoff() to output surface runoff, and snowmelt runoff, respectively, in a separate file -> new version of outsetupin;
 updated key2str and OutKey
 12/14/2012 (drs)	changed OUTSTRLEN from 2000 to 3000 to prevent 'Abort trap: 6' at runtime
 01/10/2013	(clk)	in function SW_OUT_read, when creating the output files, needed to loop through this four times
 for each OutKey, giving us the output files for day, week, month, and year. Also, added
 a switch statement to acqurie the dy, wk, mo, or yr suffix based on the iteration of the for loop.
 When reading in lines from outsetup, removed PERIOD from the read in because it is unneeded in since
 we are doing all time steps.
 Removed the line SW_Output[k].period = str2period(Str_ToUpper(period, ext)), since we are no longer
 reading in a period.
 in function SW_OUT_close_files need to loop through and close all four time step files for each
 OutKey, determined which file to close based on the iteration of the for loop.
 in function SW_OUT_write_today needed to loop through the code four times for each OutKey, changing
 the period of the output for each iteration. With the for loop, I am also able to pick the
 proper FILE pointer to choose when outputting the data.
 in function average_for needed to loop through the code four times for each OutKey, changing
 the period of the output for each iteration.
 the function str2period is no longer used in the code, but will leave it in the code for now.
 01/17/2013	(clk)	in function SW_OUT_read, created an additional condition that if the keyname was TIMESTEP,
 the code would rescan the line looking for up to 5 strings. The first one would be saved
 back into keyname, while the other four would be stored in a matrix. These values would be
 the desired timesteps to output. Then you would convert these strings to periods, and store
 them in an array of integers and keep track of how many timesteps were actually read in.
 The other changes are modifications of the changes made on 01/10/2013. For all the for loops, instead of
 looping through 4 times, we now loop through for as many times as periods that we read in from the
 TIMESTEP line. Also, in all the switch cases where we increased that value which changed the period
 to the next period, we now use the array of integers that stored the period and move through that with
 each iteration.
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
#include "generic.h"
#include "filefuncs.h"
#include "myMemory.h"
#include "Times.h"

#include "SW_Defines.h"

#include "SW_Files.h"
#include "SW_Model.h"
#include "SW_Site.h"
#include "SW_SoilWater.h"
#include "SW_Times.h"
#include "SW_Output.h"
#include "SW_Weather.h"
#include "SW_VegEstab.h"

/* =================================================== */
/*                  Global Variables                   */
/* --------------------------------------------------- */
extern SW_SITE SW_Site;
extern SW_SOILWAT SW_Soilwat;
extern SW_MODEL SW_Model;
extern SW_WEATHER SW_Weather;
extern SW_VEGESTAB SW_VegEstab;
extern Bool EchoInits;

#define OUTSTRLEN 3000 /* max output string length: in get_transp: 4*every soil layer with 14 chars */

SW_OUTPUT SW_Output[SW_OUTNKEYS]; /* declared here, externed elsewhere */

//extern int *p_yr, *p_mo, *p_wk, *p_dy;
extern RealD *p_Raet_yr, *p_Rdeep_drain_yr, *p_Restabs_yr, *p_Revap_soil_yr, *p_Revap_surface_yr, *p_Rhydred_yr, *p_Rinfiltration_yr, *p_Rinterception_yr, *p_Rpercolation_yr,
		*p_Rpet_yr, *p_Rprecip_yr, *p_Rrunoff_yr, *p_Rsnowpack_yr, *p_Rsoil_temp_yr, *p_Rsurface_water_yr, *p_Rsw_pot_yr, *p_Rswa_yr, *p_Rswc_yr, *p_Rtemp_yr, *p_Rtransp_yr,
		*p_Rvwc_yr, *p_Rwetdays_yr;
//extern RealD *p_Restabs_mo, *p_Restabs_wk, *p_Restabs_dy;
extern RealD *p_Raet_mo, *p_Rdeep_drain_mo, *p_Revap_soil_mo, *p_Revap_surface_mo, *p_Rhydred_mo, *p_Rinfiltration_mo, *p_Rinterception_mo, *p_Rpercolation_mo, *p_Rpet_mo,
		*p_Rprecip_mo, *p_Rrunoff_mo, *p_Rsnowpack_mo, *p_Rsoil_temp_mo, *p_Rsurface_water_mo, *p_Rsw_pot_mo, *p_Rswa_mo, *p_Rswc_mo, *p_Rtemp_mo, *p_Rtransp_mo, *p_Rvwc_mo,
		*p_Rwetdays_mo;
extern RealD *p_Raet_wk, *p_Rdeep_drain_wk, *p_Revap_soil_wk, *p_Revap_surface_wk, *p_Rhydred_wk, *p_Rinfiltration_wk, *p_Rinterception_wk, *p_Rpercolation_wk, *p_Rpet_wk,
		*p_Rprecip_wk, *p_Rrunoff_wk, *p_Rsnowpack_wk, *p_Rsoil_temp_wk, *p_Rsurface_water_wk, *p_Rsw_pot_wk, *p_Rswa_wk, *p_Rswc_wk, *p_Rtemp_wk, *p_Rtransp_wk, *p_Rvwc_wk,
		*p_Rwetdays_wk;
extern RealD *p_Raet_dy, *p_Rdeep_drain_dy, *p_Revap_soil_dy, *p_Revap_surface_dy, *p_Rhydred_dy, *p_Rinfiltration_dy, *p_Rinterception_dy, *p_Rpercolation_dy, *p_Rpet_dy,
		*p_Rprecip_dy, *p_Rrunoff_dy, *p_Rsnowpack_dy, *p_Rsoil_temp_dy, *p_Rsurface_water_dy, *p_Rsw_pot_dy, *p_Rswa_dy, *p_Rswc_dy, *p_Rtemp_dy, *p_Rtransp_dy, *p_Rvwc_dy,
		*p_Rwetdays_dy;
extern unsigned int yr_nrow, mo_nrow, wk_nrow, dy_nrow;
int timeSteps[4]; // array to keep track of the periods that will be used
int numPeriod; // variable to keep track of the number of periods to be used

#ifdef STEPWAT
#include "sxw.h"
extern SXW_t SXW;
#endif

/* =================================================== */
/*                Module-Level Variables               */
/* --------------------------------------------------- */
static char *MyFileName;
static char outstr[OUTSTRLEN];
static char _Sep; /* output delimiter */

static Bool bFlush; /* process partial period ? */
static TimeInt tOffset; /* 1 or 0 means we're writing previous or current period */

/* These MUST be in the same order as enum OutKey in
 * SW_Output.h */
static char *key2str[] = { SW_WETHR, SW_TEMP, SW_PRECIP, SW_SOILINF, SW_RUNOFF, SW_ALLH2O, SW_SWC, SW_SWCM, SW_SWP, SW_SWA, SW_SURFACEW, SW_TRANSP, SW_EVAPSOIL,
		SW_EVAPSURFACE, SW_INTERCEPTION, SW_LYRDRAIN, SW_HYDRED, SW_ET, SW_AET, SW_PET, SW_WETDAY, SW_SNOWPACK, SW_DEEPSWC, SW_SOILTEMP, SW_ALLVEG, SW_ESTAB };
/* converts an enum output key (OutKey type) to a module  */
/* or object type. see SW_Output.h for OutKey order.         */
/* MUST be SW_OUTNKEYS of these */
static ObjType key2obj[] = { eWTH, eWTH, eWTH, eWTH, eWTH, eSWC, eSWC, eSWC, eSWC, eSWC, eSWC, eSWC, eSWC, eSWC, eSWC, eSWC, eSWC, eSWC, eSWC, eSWC, eSWC, eSWC, eSWC, eSWC,
		eVES, eVES };
static char *pd2str[] = { SW_DAY, SW_WEEK, SW_MONTH, SW_YEAR };
static char *styp2str[] = { SW_SUM_OFF, SW_SUM_SUM, SW_SUM_AVG, SW_SUM_FNL };

/* =================================================== */
/* =================================================== */
/*             Private Function Definitions            */
/* --------------------------------------------------- */
static void _echo_outputs(void);
static void average_for(ObjType otyp, OutPeriod pd);
//static void get_outstrleader(TimeInt pd);
static void get_temp(void);
static void get_precip(void);
static void get_swc(void);
static void get_swcm(void);
static void get_swp(void);
static void get_swa(void);
static void get_surfaceWater(void);
static void get_runoff(void);
static void get_transp(void);
static void get_evapSoil(void);
static void get_evapSurface(void);
static void get_interception(void);
static void get_soilinf(void);
static void get_lyrdrain(void);
static void get_hydred(void);
static void get_aet(void);
static void get_pet(void);
static void get_wetdays(void);
static void get_snowpack(void);
static void get_deepswc(void);
static void get_estab(void);
static void get_soiltemp(void);
static void get_none(void); /* default until defined */

static void collect_sums(ObjType otyp, OutPeriod op);
static void sumof_wth(SW_WEATHER *v, SW_WEATHER_OUTPUTS *s, OutKey k);
static void sumof_swc(SW_SOILWAT *v, SW_SOILWAT_OUTPUTS *s, OutKey k);
static void sumof_ves(SW_VEGESTAB *v, SW_VEGESTAB_OUTPUTS *s, OutKey k);

static OutPeriod str2period(char *s) {
	/* --------------------------------------------------- */
	IntUS pd;
	for (pd = 0; Str_CompareI(s, pd2str[pd]) && pd < SW_OUTNPERIODS; pd++)
		;
	if (pd == SW_OUTNPERIODS) {
		LogError(logfp, LOGFATAL, "%s : Invalid period (%s)", SW_F_name(eOutput), s);
	}
	return pd;
}

static OutKey str2key(char *s) {
	/* --------------------------------------------------- */
	IntUS key;

	for (key = 0; key < SW_OUTNKEYS && Str_CompareI(s, key2str[key]); key++)
		;
	if (key == SW_OUTNKEYS) {
		LogError(logfp, LOGFATAL, "%s : Invalid key (%s) in %s", SW_F_name(eOutput), s);
	}
	return key;
}

static OutSum str2stype(char *s) {
	/* --------------------------------------------------- */
	OutSum styp;

	for (styp = eSW_Off; Str_CompareI(s, styp2str[styp]) && styp < SW_NSUMTYPES; styp++);
	return styp;
}

/* =================================================== */
/* =================================================== */
/*             Public Function Definitions             */
/* --------------------------------------------------- */
void SW_OUT_construct(void) {
	/* =================================================== */
	OutKey k;

	/* note that an initializer that is called during
	 * execution (better called clean() or something)
	 * will need to free all allocated memory first
	 * before clearing structure.
	 */
	ForEachOutKey(k)
	{
		if (!isnull(SW_Output[k].outfile)) {//Clear memory before setting it
			Mem_Free(SW_Output[k].outfile);
			SW_Output[k].outfile = NULL;
		}
	}
	memset(&SW_Output, 0, sizeof(SW_Output));

	/* attach the printing functions for each output
	 * quantity to the appropriate element in the
	 * output structure.  Using a loop makes it convenient
	 * to simply add a line as new quantities are
	 * implemented and leave the default case for every
	 * thing else.
	 */ForEachOutKey(k)
	{
		SW_Output[k].yr_row = 0;
		SW_Output[k].mo_row = 0;
		SW_Output[k].wk_row = 0;
		SW_Output[k].dy_row = 0;
		switch (k) {
		case eSW_Temp:
			SW_Output[k].pfunc = (void (*)(void)) get_temp;
			break;
		case eSW_Precip:
			SW_Output[k].pfunc = (void (*)(void)) get_precip;
			break;
		case eSW_SWC:
			SW_Output[k].pfunc = (void (*)(void)) get_swc;
			break;
		case eSW_SWCM:
			SW_Output[k].pfunc = (void (*)(void)) get_swcm;
			break;
		case eSW_SWP:
			SW_Output[k].pfunc = (void (*)(void)) get_swp;
			break;
		case eSW_SWA:
			SW_Output[k].pfunc = (void (*)(void)) get_swa;
			break;
		case eSW_SurfaceWater:
			SW_Output[k].pfunc = (void (*)(void)) get_surfaceWater;
			break;
		case eSW_Runoff:
			SW_Output[k].pfunc = (void (*)(void)) get_runoff;
			break;
		case eSW_Transp:
			SW_Output[k].pfunc = (void (*)(void)) get_transp;
			break;
		case eSW_EvapSoil:
			SW_Output[k].pfunc = (void (*)(void)) get_evapSoil;
			break;
		case eSW_EvapSurface:
			SW_Output[k].pfunc = (void (*)(void)) get_evapSurface;
			break;
		case eSW_Interception:
			SW_Output[k].pfunc = (void (*)(void)) get_interception;
			break;
		case eSW_SoilInf:
			SW_Output[k].pfunc = (void (*)(void)) get_soilinf;
			break;
		case eSW_LyrDrain:
			SW_Output[k].pfunc = (void (*)(void)) get_lyrdrain;
			break;
		case eSW_HydRed:
			SW_Output[k].pfunc = (void (*)(void)) get_hydred;
			break;
		case eSW_AET:
			SW_Output[k].pfunc = (void (*)(void)) get_aet;
			break;
		case eSW_PET:
			SW_Output[k].pfunc = (void (*)(void)) get_pet;
			break;
		case eSW_WetDays:
			SW_Output[k].pfunc = (void (*)(void)) get_wetdays;
			break;
		case eSW_SnowPack:
			SW_Output[k].pfunc = (void (*)(void)) get_snowpack;
			break;
		case eSW_DeepSWC:
			SW_Output[k].pfunc = (void (*)(void)) get_deepswc;
			break;
		case eSW_SoilTemp:
			SW_Output[k].pfunc = (void (*)(void)) get_soiltemp;
			break;
		case eSW_Estab:
			SW_Output[k].pfunc = (void (*)(void)) get_estab;
			break;
		default:
			SW_Output[k].pfunc = (void (*)(void)) get_none;
			break;

		}
	}

	bFlush = swFALSE;
	tOffset = 1;

}

void SW_OUT_new_year(void) {
	/* =================================================== */
	/* reset the terminal output days each year  */

	OutKey k;

	ForEachOutKey(k)
	{
		if (!SW_Output[k].use)
			continue;

		if (SW_Output[k].first_orig <= SW_Model.firstdoy)
			SW_Output[k].first = SW_Model.firstdoy;
		else
			SW_Output[k].first = SW_Output[k].first_orig;

		if (SW_Output[k].last_orig >= SW_Model.lastdoy)
			SW_Output[k].last = SW_Model.lastdoy;
		else
			SW_Output[k].last = SW_Output[k].last_orig;

	}

}

void SW_OUT_read(void) {
	/* =================================================== */
	/* read input file for output parameter setup info.
	 * 5-Nov-01 -- now disregard the specified file name's
	 *             extension and instead use the specified
	 *             period as the extension.
	 * 10-May-02 - Added conditional for interfacing to STEPPE.
	 *             We want no output when running from STEPPE
	 *             so the code to open the file is blocked out.
	 *             In fact, the only keys to process are
	 *             TRANSP, PRECIP, and TEMP.
	 */
	FILE *f;
	OutKey k;
	int x, i, itemno;
	char ext[10]; //str[MAX_FILENAMESIZE],

	/* these dims come from the orig format str */
	/* except for the uppercase space. */
	char timeStep[4][10], // matrix to capture all the periods entered in outsetup.in
			keyname[50], upkey[50], /* space for uppercase conversion */
			sumtype[4], upsum[4], period[10], /* should be 2 chars, but we don't want overflow from user typos */
			last[4], /* last doy for output, if "end", ==366 */
			outfile[MAX_FILENAMESIZE]; //prefix[MAX_FILENAMESIZE]
	int first; /* first doy for output */

	MyFileName = SW_F_name(eOutput);
	f = OpenFile(MyFileName, "r");
	itemno = 0;

	_Sep = '\t'; /* default in case it doesn't show up in the file */

	while (GetALine(f, inbuf)) {
		itemno++; /* note extra lines will cause an error */

		x = sscanf(inbuf, "%s %s %d %s %s", keyname, sumtype, &first, last, outfile);
		if (Str_CompareI(keyname, "TIMESTEP") == 0)	// condition to read in the TIMESTEP line in outsetup.in
				{
			numPeriod = sscanf(inbuf, "%s %s %s %s %s", keyname, timeStep[0], timeStep[1], timeStep[2], timeStep[3]);// need to rescan the line because you are looking for all strings, unlike the original scan
			numPeriod--;	// decrement the count to make sure to not count keyname in the number of periods

			for (i = 0; i < numPeriod; i++)	// do this for as many periods are being used
					{
				timeSteps[i] = str2period(Str_ToUpper(timeStep[i], ext));	// takes the entered value for the period and converts it to the period format
			}
			continue;
		} else if (x < 5) {
			if (Str_CompareI(keyname, "OUTSEP") == 0) {
				switch ((int) *sumtype) {
				case 't':
					_Sep = '\t';
					break;
				case 's':
					_Sep = ' ';
					break;
				default:
					_Sep = *sumtype;
				}
				continue;
			} else {
				CloseFile(&f);
				LogError(logfp, LOGFATAL, "%s : Insufficient key parameters for item %d.", MyFileName, itemno);
				continue;
			}
		}

		/* Check validity of output key */
		k = str2key(Str_ToUpper(keyname, upkey));
		if (k == eSW_Estab) {
			strcpy(sumtype, "SUM");
			first = 1;
			strcpy(period, "YR");
			strcpy(last, "end");
		} else if ((k == eSW_AllVeg || k == eSW_ET || k == eSW_AllWthr || k == eSW_AllH2O)) {
			SW_Output[k].use = swFALSE;
			LogError(logfp, LOGNOTE, "%s : Output key %s is currently unimplemented.", MyFileName, key2str[k]);
			continue;
		}

		/* check validity of summary type */
		SW_Output[k].sumtype = str2stype(Str_ToUpper(sumtype, upsum));
		if (SW_Output[k].sumtype == eSW_Fnl && !(k == eSW_SWC || k == eSW_SWP || k == eSW_SWCM || k == eSW_DeepSWC)) {
			LogError(logfp, LOGWARN, "%s : Summary Type FIN with key %s is meaningless.\n"
					"  Using type AVG instead.", MyFileName, key2str[k]);
			SW_Output[k].sumtype = eSW_Avg;
		}

		/* verify deep drainage parameters */
		if (k == eSW_DeepSWC && SW_Output[k].sumtype != eSW_Off && !SW_Site.deepdrain) {
			LogError(logfp, LOGWARN, "%s : DEEPSWC cannot be output if flag not set in %s.", MyFileName, SW_F_name(eOutput));
			continue;
		}

		SW_Output[k].outfile = (char *) Str_Dup(outfile);//NOTE THIS SHOULD NOT BE IN FOR LOOP

		for (i = 0; i < numPeriod; i++) { /* for loop to create files for all the periods that are being used */
			/* prepare the remaining structure if use==swTRUE */
			SW_Output[k].use = (SW_Output[k].sumtype == eSW_Off) ? swFALSE : swTRUE;
			if (SW_Output[k].use) {
				SW_Output[k].mykey = k;
				SW_Output[k].myobj = key2obj[k];
				//SW_Output[k].period = str2period(Str_ToUpper(period, ext));
				SW_Output[k].first_orig = first;
				SW_Output[k].last_orig = !Str_CompareI("END", last) ? 366 : atoi(last);
				if (SW_Output[k].last_orig == 0) {
					CloseFile(&f);
					LogError(logfp, LOGFATAL, "%s : Invalid ending day (%s), key=%s.", MyFileName, last, keyname);
				}

				/*SW_OutputPrefix(prefix);
				 strcpy(str, prefix);
				 strcat(str, outfile);
				 strcat(str, ".");
				 switch (timeSteps[i]) { // depending on iteration through, will determine what period to use from the array of periods
				 case eSW_Day:
				 period[0] = 'd';
				 period[1] = 'y';
				 period[2] = '\0';
				 break;
				 case eSW_Week:
				 period[0] = 'w';
				 period[1] = 'k';
				 period[2] = '\0';
				 break;
				 case eSW_Month:
				 period[0] = 'm';
				 period[1] = 'o';
				 period[2] = '\0';
				 break;
				 case eSW_Year:
				 period[0] = 'y';
				 period[1] = 'r';
				 period[2] = '\0';
				 break;
				 }
				 strcat(str, Str_ToLower(period, ext));*/
				 //Rprintf("str_dup outfile \n");


#ifndef STEPWAT
				/*
				 switch (timeSteps[i]) { // depending on iteration through for loop, chooses the proper FILE pointer to use
				 case eSW_Day:
				 SW_Output[k].fp_dy = OpenFile(SW_Output[k].outfile, "w");
				 break;
				 case eSW_Week:
				 SW_Output[k].fp_wk = OpenFile(SW_Output[k].outfile, "w");
				 break;
				 case eSW_Month:
				 SW_Output[k].fp_mo = OpenFile(SW_Output[k].outfile, "w");
				 break;
				 case eSW_Year:
				 SW_Output[k].fp_yr = OpenFile(SW_Output[k].outfile, "w");
				 break;
				 }
				 */
#endif
			}
		}

	}

	CloseFile(&f);

	if (EchoInits)
		_echo_outputs();
}

void onSet_SW_OUT(SEXP OUT) {
	int i;
	OutKey k;
	SEXP sep, timestep, KEY;
	Bool continue1;
	SEXP mykey, myobj, period, sumtype, use, first, last, first_orig, last_orig, outfile;

	MyFileName = SW_F_name(eOutput);

	PROTECT(sep = GET_SLOT(OUT, install("outputSeparator")));
	_Sep = '\t';/*CHAR(STRING_ELT(sep, 0));*/
	PROTECT(timestep = GET_SLOT(OUT, install("timePeriods")));
	for (i = 0; i < LENGTH(timestep); i++)
		timeSteps[i] = INTEGER(timestep)[i];
	numPeriod = LENGTH(timestep);

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
		if (k == eSW_Estab) {
			INTEGER(sumtype)[k] = 1;
			INTEGER(first)[k] = 1;
			INTEGER(period)[k] = 4;
			INTEGER(last)[k] = 366;
		}
		continue1 = (k == eSW_AllVeg || k == eSW_ET || k == eSW_AllWthr || k == eSW_AllH2O);
		if (continue1) {
			SW_Output[k].use = swFALSE;
			//LogError(logfp, LOGNOTE, "Output key %s is currently unimplemented.", key2str[k]);
		}
		if (!continue1) {
			/* check validity of summary type */
			SW_Output[k].sumtype = INTEGER(sumtype)[k];
			if (SW_Output[k].sumtype == eSW_Fnl && !(k == eSW_SWC || k == eSW_SWP || k == eSW_SWCM || k == eSW_DeepSWC)) {
				LogError(logfp, LOGWARN, "output.in : Summary Type FIN with key %s is meaningless.\n"
						"  Using type AVG instead.", key2str[k]);
				SW_Output[k].sumtype = eSW_Avg;
			}
			/* verify deep drainage parameters */
			if (k == eSW_DeepSWC && SW_Output[k].sumtype != eSW_Off && !SW_Site.deepdrain) {
				LogError(logfp, LOGWARN, "output.in : DEEPSWC cannot be output if flag not set in output.in.");
			} else {
				/* prepare the remaining structure if use==swTRUE */
				SW_Output[k].use = (SW_Output[k].sumtype == eSW_Off) ? swFALSE : swTRUE;
				if (SW_Output[k].use) {
					SW_Output[k].mykey = INTEGER(mykey)[k];
					SW_Output[k].myobj = INTEGER(myobj)[k];
					//SW_Output[k].period = str2period(Str_ToUpper(period, ext));
					SW_Output[k].first_orig = INTEGER(first_orig)[k];
					SW_Output[k].last_orig = INTEGER(last_orig)[k];
					if (SW_Output[k].last_orig == 0) {
						LogError(logfp, LOGFATAL, "output.in : Invalid ending day (%s), key=%s.", INTEGER(last)[k], key2str[k]);
					}
				}
			}
		}
	}
	if (EchoInits)
		_echo_outputs();
	UNPROTECT(12);
}

SEXP onGet_SW_OUT(void) {
	int i;
	OutKey k;
	SEXP swOUT;
	SEXP OUT;
	SEXP sep;
	SEXP timestep;
	SEXP mykey, myobj, period, sumtype, use, first, last, first_orig, last_orig, outfile;
	char *cKEY[] = { "mykey", "myobj", "period", "sumtype", "use", "first", "last", "first_orig", "last_orig", "outfile" };

	PROTECT(swOUT = MAKE_CLASS("swOUT"));
	PROTECT(OUT = NEW_OBJECT(swOUT));		//allocVector(VECSXP,2+SW_OUTNKEYS));

	//PROTECT(OUT_names = allocVector(STRSXP,2+SW_OUTNKEYS));

	PROTECT(sep=NEW_STRING(1));
	SET_STRING_ELT(sep, 0, mkChar(&_Sep));
	SET_SLOT(OUT, install("outputSeparator"), sep);

	PROTECT(timestep = allocVector(INTSXP,numPeriod));
	for (i = 0; i < numPeriod; i++)
		INTEGER(timestep)[i] = timeSteps[i];
	SET_SLOT(OUT, install("timePeriods"), timestep);

	PROTECT(mykey = NEW_INTEGER(26));
	PROTECT(myobj = NEW_INTEGER(26));
	PROTECT(period = NEW_INTEGER(26));
	PROTECT(sumtype = NEW_INTEGER(26));
	PROTECT(use = NEW_LOGICAL(26));
	PROTECT(first = NEW_INTEGER(26));
	PROTECT(last = NEW_INTEGER(26));
	PROTECT(last = NEW_INTEGER(26));
	PROTECT(first_orig = NEW_INTEGER(26));
	PROTECT(last_orig = NEW_INTEGER(26));
	PROTECT(outfile = NEW_STRING(26));

	ForEachOutKey(k)
	{
		INTEGER(mykey)[k] = SW_Output[k].mykey;
		INTEGER(myobj)[k] = SW_Output[k].myobj;
		INTEGER(period)[k] = SW_Output[k].period;
		INTEGER(sumtype)[k] = SW_Output[k].sumtype;
		LOGICAL(use)[k] = SW_Output[k].use;
		INTEGER(first)[k] = SW_Output[k].first;
		INTEGER(last)[k] = SW_Output[k].last;
		INTEGER(first_orig)[k] = SW_Output[k].first_orig;
		INTEGER(last_orig)[k] = SW_Output[k].last_orig;
		if(SW_Output[k].use) {
			SET_STRING_ELT(outfile, k, mkChar(SW_Output[k].outfile));

		} else
			SET_STRING_ELT(outfile, k, mkChar(""));
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
	return OUT;
}

void SW_OUT_close_files(void) {
	/* --------------------------------------------------- */
	/* close all of the user-specified output files.
	 * call this routine at the end of the program run.
	 */
#ifndef STEPWAT
	OutKey k;
	int i;
	ForEachOutKey(k)
	{
		if (SW_Output[k].use)
			for (i = 0; i < numPeriod; i++) /*will loop through for as many periods are being used*/
			{
				switch (timeSteps[i]) { /*depending on iteration through loop, will close one of the time step files */
				case eSW_Day:
					CloseFile(&SW_Output[k].fp_dy);
					break;
				case eSW_Week:
					CloseFile(&SW_Output[k].fp_wk);
					break;
				case eSW_Month:
					CloseFile(&SW_Output[k].fp_mo);
					break;
				case eSW_Year:
					CloseFile(&SW_Output[k].fp_yr);
					break;
				}
			}
	}
#endif
}

void SW_OUT_flush(void) {
	/* --------------------------------------------------- */
	/* called at year end to process the remainder of the output
	 * period.  This sets two module-level flags: bFlush and
	 * tOffset to be used in the appropriate subs.
	 */
	bFlush = swTRUE;
	tOffset = 0;

	SW_OUT_sum_today(eSWC);
	SW_OUT_sum_today(eWTH);
	SW_OUT_sum_today(eVES);
	SW_OUT_write_today();

	bFlush = swFALSE;
	tOffset = 1;

}

void SW_OUT_sum_today(ObjType otyp) {
	/* =================================================== */
	/* adds today's output values to week, month and year
	 * accumulators and puts today's values in yesterday's
	 * registers. This is different from the Weather.c approach
	 * which updates Yesterday's registers during the _new_day()
	 * function. It's more logical to update yesterday just
	 * prior to today's calculations, but there's no logical
	 * need to perform _new_day() on the soilwater.
	 */
	SW_SOILWAT *s = &SW_Soilwat;
	SW_WEATHER *w = &SW_Weather;
	/*  SW_VEGESTAB *v = &SW_VegEstab;  -> we don't need to sum daily for this */

	OutPeriod pd;
	IntU size = 0;

	switch (otyp) {
	case eSWC:
		size = sizeof(SW_SOILWAT_OUTPUTS);
		break;
	case eWTH:
		size = sizeof(SW_WEATHER_OUTPUTS);
		break;
	case eVES:
		return; /* a stub; we don't do anything with ves until get_() */
	default:
		LogError(stdout, LOGFATAL, "Invalid object type in SW_OUT_sum_today().");
	}

	/* do this every day (kinda expensive but more general than before)*/
	switch (otyp) {
	case eSWC:
		memset(&s->dysum, 0, size);
		break;
	case eWTH:
		memset(&w->dysum, 0, size);
		break;
	default:
		break;
	}

	/* the rest only get done if new period */
	if (SW_Model.newweek || bFlush) {
		average_for(otyp, eSW_Week);
		switch (otyp) {
		case eSWC:
			memset(&s->wksum, 0, size);
			break;
		case eWTH:
			memset(&w->wksum, 0, size);
			break;
		default:
			break;
		}
	}

	if (SW_Model.newmonth || bFlush) {
		average_for(otyp, eSW_Month);
		switch (otyp) {
		case eSWC:
			memset(&s->mosum, 0, size);
			break;
		case eWTH:
			memset(&w->mosum, 0, size);
			break;
		default:
			break;
		}
	}

	if (SW_Model.newyear || bFlush) {
		average_for(otyp, eSW_Year);
		switch (otyp) {
		case eSWC:
			memset(&s->yrsum, 0, size);
			break;
		case eWTH:
			memset(&w->yrsum, 0, size);
			break;
		default:
			break;
		}
	}

	if (!bFlush) {
		ForEachOutPeriod(pd)
			collect_sums(otyp, pd);
	}
}

void SW_OUT_write_today(void) {
	/* --------------------------------------------------- */
	/* all output values must have been summed, averaged or
	 * otherwise completed before this is called [now done
	 * by SW_*_sum_*<daily|yearly>()] prior.
	 * This subroutine organizes only the calling loop and
	 * sending the string to output.
	 * Each output quantity must have a print function
	 * defined and linked to SW_Output.pfunc (currently all
	 * starting with 'get_').  Those funcs return a properly
	 * formatted string to be output via the module variable
	 * 'outstr'. Furthermore, those funcs must know their
	 * own time period.  This version of the program only
	 * prints one period for each quantity.
	 *
	 * The t value tests whether the current model time is
	 * outside the output time range requested by the user.
	 * Recall that times are based at 0 rather than 1 for
	 * array indexing purposes but the user request is in
	 * natural numbers, so we add one before testing.
	 */
	/* 10-May-02 (cwb) Added conditional to interface with STEPPE.
	 *           We want no output if running from STEPPE.
	 */
	TimeInt t = 0xffff;
	OutKey k;
	Bool writeit;
	int i;

	ForEachOutKey(k)
	{
		for (i = 0; i < numPeriod; i++) { /* will run through this loop for as many periods are being used */
			if (!SW_Output[k].use)
				continue;
			writeit = swTRUE;
			SW_Output[k].period = timeSteps[i]; /* set the desired period based on the iteration */
			switch (SW_Output[k].period) {
			case eSW_Day:
				t = SW_Model.doy;
				break;
			case eSW_Week:
				writeit = (SW_Model.newweek || bFlush);
				t = (SW_Model.week + 1) - tOffset;
				break;
			case eSW_Month:
				writeit = (SW_Model.newmonth || bFlush);
				t = (SW_Model.month + 1) - tOffset;
				break;
			case eSW_Year:
				writeit = (SW_Model.newyear || bFlush);
				t = SW_Output[k].first; /* always output this period */
				break;
			default:
				LogError(stdout, LOGFATAL, "Invalid period in SW_OUT_write_today().");
			}
			if (!writeit || t < SW_Output[k].first || t > SW_Output[k].last)
				continue;

			((void (*)(void)) SW_Output[k].pfunc)();
		}
	}
}

static void get_none(void) {
	/* --------------------------------------------------- */
	/* output routine for quantities that aren't yet implemented
	 * this just gives the main output loop something to call,
	 * rather than an empty pointer.
	 */
	outstr[0] = '\0';
}

//static void get_outstrleader(TimeInt pd) {
/* --------------------------------------------------- */
/* this is called from each of the remaining get_ funcs
 * to set up the first (date) columns of the output
 * string.  It's the same for all and easier to put in
 * one place.
 * Periodic output for Month and/or Week are actually
 * printing for the PREVIOUS month or week.
 * Also, see note on test value in _write_today() for
 * explanation of the +1.
 */

/*	switch (pd) {
 case eSW_Day:
 //p_dy[dy_row + 0 * dy_nrow] = SW_Model.year;
 //p_dy[dy_row + 1 * dy_nrow] = SW_Model.doy;
 break;
 case eSW_Week:
 //p_wk[wk_row + 0 * wk_nrow] = SW_Model.year;
 //p_wk[wk_row + 1 * wk_nrow] = (SW_Model.week + 1) - tOffset;
 break;
 case eSW_Month:
 //p_mo[mo_row + 0 * mo_nrow] = SW_Model.year;
 //p_mo[mo_row + 1 * mo_nrow] = (SW_Model.month + 1) - tOffset;
 break;
 case eSW_Year:
 //p_yr[yr_row] = SW_Model.year;
 }

 }*/

static void get_estab(void) {
	/* --------------------------------------------------- */
	/* the establishment check produces, for each species in
	 * the given set, a day of year >=0 that the species
	 * established itself in the current year.  The output
	 * will be a single row of numbers for each year.  Each
	 * column represents a species in the order it was entered
	 * in the estabs.in file.  The value will be the day that
	 * the species established, or 0 if it didn't establish
	 * this year.
	 */
	SW_VEGESTAB *v = &SW_VegEstab;
	OutPeriod pd = SW_Output[eSW_Estab].period;
	IntU i;

	switch (pd) {
	case eSW_Day:
	case eSW_Week:
	case eSW_Month:
		break;
	case eSW_Year:
		if (v->count > 0)
			p_Restabs_yr[SW_Output[eSW_Estab].yr_row + yr_nrow * 0] = SW_Model.year;
		for (i = 0; i < v->count; i++)
			p_Restabs_yr[SW_Output[eSW_Estab].yr_row + yr_nrow * v->count + (i + 1)] = v->parms[i]->estab_doy;
		SW_Output[eSW_Estab].yr_row++;
		break;
	}
}

static void get_temp(void) {
	/* --------------------------------------------------- */
	/* each of these get_<envparm> -type funcs return a
	 * formatted string of the appropriate type and are
	 * pointed to by SW_Output[k].pfunc so they can be called
	 * anonymously by looping over the Output[k] list
	 * (see _output_today() for usage.)
	 * they all use the module-level string outstr[].
	 */
	/* 10-May-02 (cwb) Added conditionals for interfacing with STEPPE
	 * 05-Mar-03 (cwb) Added code for max,min,avg.
	 *                 Previously, only avg was output.
	 */
	SW_WEATHER *v = &SW_Weather;
	OutPeriod pd = SW_Output[eSW_Temp].period;

	switch (pd) {
	case eSW_Day:
		p_Rtemp_dy[SW_Output[eSW_Temp].dy_row + dy_nrow * 0] = SW_Model.year;
		p_Rtemp_dy[SW_Output[eSW_Temp].dy_row + dy_nrow * 1] = SW_Model.doy;
		p_Rtemp_dy[SW_Output[eSW_Temp].dy_row + dy_nrow * 2] = v->dysum.temp_max;
		p_Rtemp_dy[SW_Output[eSW_Temp].dy_row + dy_nrow * 3] = v->dysum.temp_min;
		p_Rtemp_dy[SW_Output[eSW_Temp].dy_row + dy_nrow * 4] = v->dysum.temp_avg;
		SW_Output[eSW_Temp].dy_row++;
		break;
	case eSW_Week:
		p_Rtemp_wk[SW_Output[eSW_Temp].wk_row + wk_nrow * 0] = SW_Model.year;
		p_Rtemp_wk[SW_Output[eSW_Temp].wk_row + wk_nrow * 1] = (SW_Model.week + 1) - tOffset;
		p_Rtemp_wk[SW_Output[eSW_Temp].wk_row + wk_nrow * 2] = v->wkavg.temp_max;
		p_Rtemp_wk[SW_Output[eSW_Temp].wk_row + wk_nrow * 3] = v->wkavg.temp_min;
		p_Rtemp_wk[SW_Output[eSW_Temp].wk_row + wk_nrow * 4] = v->wkavg.temp_avg;
		SW_Output[eSW_Temp].wk_row++;
		break;
	case eSW_Month:
		p_Rtemp_mo[SW_Output[eSW_Temp].mo_row + mo_nrow * 0] = SW_Model.year;
		p_Rtemp_mo[SW_Output[eSW_Temp].mo_row + mo_nrow * 1] = (SW_Model.month + 1) - tOffset;
		p_Rtemp_mo[SW_Output[eSW_Temp].mo_row + mo_nrow * 2] = v->moavg.temp_max;
		p_Rtemp_mo[SW_Output[eSW_Temp].mo_row + mo_nrow * 3] = v->moavg.temp_min;
		p_Rtemp_mo[SW_Output[eSW_Temp].mo_row + mo_nrow * 4] = v->moavg.temp_avg;
		SW_Output[eSW_Temp].mo_row++;
		break;
	case eSW_Year:
		p_Rtemp_yr[SW_Output[eSW_Temp].yr_row + yr_nrow * 0] = SW_Model.year;
		p_Rtemp_yr[SW_Output[eSW_Temp].yr_row + yr_nrow * 1] = v->yravg.temp_max;
		p_Rtemp_yr[SW_Output[eSW_Temp].yr_row + yr_nrow * 2] = v->yravg.temp_min;
		p_Rtemp_yr[SW_Output[eSW_Temp].yr_row + yr_nrow * 3] = v->yravg.temp_avg;
		SW_Output[eSW_Temp].yr_row++;
		break;
	}

#ifdef STEPWAT
	if (pd != eSW_Year)
	LogError(logfp, LOGFATAL, "Invalid output period for TEMP; should be YR");
	SXW.temp = v_avg;
#endif
}

static void get_precip(void) {
	/* --------------------------------------------------- */
	/* 	20091015 (drs) ppt is divided into rain and snow and all three values are output into precip */
	SW_WEATHER *v = &SW_Weather;
	OutPeriod pd = SW_Output[eSW_Precip].period;

	switch (pd) {
	case eSW_Day:
		p_Rprecip_dy[SW_Output[eSW_Precip].dy_row + dy_nrow * 0] = SW_Model.year;
		p_Rprecip_dy[SW_Output[eSW_Precip].dy_row + dy_nrow * 1] = SW_Model.doy;
		p_Rprecip_dy[SW_Output[eSW_Precip].dy_row + dy_nrow * 2] = v->dysum.ppt;
		p_Rprecip_dy[SW_Output[eSW_Precip].dy_row + dy_nrow * 3] = v->dysum.rain;
		p_Rprecip_dy[SW_Output[eSW_Precip].dy_row + dy_nrow * 4] = v->dysum.snow;
		p_Rprecip_dy[SW_Output[eSW_Precip].dy_row + dy_nrow * 5] = v->dysum.snowmelt;
		p_Rprecip_dy[SW_Output[eSW_Precip].dy_row + dy_nrow * 6] = v->dysum.snowloss;
		SW_Output[eSW_Precip].dy_row++;
		break;
	case eSW_Week:
		p_Rprecip_wk[SW_Output[eSW_Precip].wk_row + wk_nrow * 0] = SW_Model.year;
		p_Rprecip_wk[SW_Output[eSW_Precip].wk_row + wk_nrow * 1] = (SW_Model.week + 1) - tOffset;
		p_Rprecip_wk[SW_Output[eSW_Precip].wk_row + wk_nrow * 2] = v->wkavg.ppt;
		p_Rprecip_wk[SW_Output[eSW_Precip].wk_row + wk_nrow * 3] = v->wkavg.rain;
		p_Rprecip_wk[SW_Output[eSW_Precip].wk_row + wk_nrow * 4] = v->wkavg.snow;
		p_Rprecip_wk[SW_Output[eSW_Precip].wk_row + wk_nrow * 5] = v->wkavg.snowmelt;
		p_Rprecip_wk[SW_Output[eSW_Precip].wk_row + wk_nrow * 6] = v->wkavg.snowloss;
		SW_Output[eSW_Precip].wk_row++;
		break;
	case eSW_Month:
		p_Rprecip_mo[SW_Output[eSW_Precip].mo_row + mo_nrow * 0] = SW_Model.year;
		p_Rprecip_mo[SW_Output[eSW_Precip].mo_row + mo_nrow * 1] = (SW_Model.month + 1) - tOffset;
		p_Rprecip_mo[SW_Output[eSW_Precip].mo_row + mo_nrow * 2] = v->moavg.ppt;
		p_Rprecip_mo[SW_Output[eSW_Precip].mo_row + mo_nrow * 3] = v->moavg.rain;
		p_Rprecip_mo[SW_Output[eSW_Precip].mo_row + mo_nrow * 4] = v->moavg.snow;
		p_Rprecip_mo[SW_Output[eSW_Precip].mo_row + mo_nrow * 5] = v->moavg.snowmelt;
		p_Rprecip_mo[SW_Output[eSW_Precip].mo_row + mo_nrow * 6] = v->moavg.snowloss;
		SW_Output[eSW_Precip].mo_row++;
		break;
	case eSW_Year:
		p_Rprecip_yr[SW_Output[eSW_Precip].yr_row + yr_nrow * 0] = SW_Model.year;
		p_Rprecip_yr[SW_Output[eSW_Precip].yr_row + yr_nrow * 1] = v->yravg.ppt;
		p_Rprecip_yr[SW_Output[eSW_Precip].yr_row + yr_nrow * 2] = v->yravg.rain;
		p_Rprecip_yr[SW_Output[eSW_Precip].yr_row + yr_nrow * 3] = v->yravg.snow;
		p_Rprecip_yr[SW_Output[eSW_Precip].yr_row + yr_nrow * 4] = v->yravg.snowmelt;
		p_Rprecip_yr[SW_Output[eSW_Precip].yr_row + yr_nrow * 5] = v->yravg.snowloss;
		SW_Output[eSW_Precip].yr_row++;
		break;
	}

#ifdef STEPWAT
	if (pd != eSW_Year)
	LogError(logfp, LOGFATAL, "Invalid output period for PRECIP; should be YR");
	SXW.ppt = val_ppt;
#endif
}

static void get_swc(void) {
	/* --------------------------------------------------- */
	LyrIndex i;
	SW_SOILWAT *v = &SW_Soilwat;
	OutPeriod pd = SW_Output[eSW_SWC].period;

#ifdef STEPWAT
	TimeInt p;
	SW_MODEL *t = &SW_Model;
#endif

	switch (pd) {
	case eSW_Day:
		p_Rswc_dy[SW_Output[eSW_SWC].dy_row + dy_nrow * 0] = SW_Model.year;
		p_Rswc_dy[SW_Output[eSW_SWC].dy_row + dy_nrow * 1] = SW_Model.doy;
		ForEachSoilLayer(i)
		{
			p_Rswc_dy[SW_Output[eSW_SWC].dy_row + dy_nrow * (i + 2)] = v->dysum.swc[i];
		}
		SW_Output[eSW_SWC].dy_row++;
		break;
	case eSW_Week:
		p_Rswc_wk[SW_Output[eSW_SWC].wk_row + wk_nrow * 0] = SW_Model.year;
		p_Rswc_wk[SW_Output[eSW_SWC].wk_row + wk_nrow * 1] = (SW_Model.week + 1) - tOffset;
		ForEachSoilLayer(i)
		{
			p_Rswc_wk[SW_Output[eSW_SWC].wk_row + wk_nrow * (i + 2)] = v->wkavg.swc[i];
		}
		SW_Output[eSW_SWC].wk_row++;
		break;
	case eSW_Month:
		p_Rswc_mo[SW_Output[eSW_SWC].mo_row + mo_nrow * 0] = SW_Model.year;
		p_Rswc_mo[SW_Output[eSW_SWC].mo_row + mo_nrow * 1] = (SW_Model.month + 1) - tOffset;
		ForEachSoilLayer(i)
		{
			p_Rswc_mo[SW_Output[eSW_SWC].mo_row + mo_nrow * (i + 2)] = v->moavg.swc[i];
		}
		SW_Output[eSW_SWC].mo_row++;
		break;
	case eSW_Year:
		p_Rswc_yr[SW_Output[eSW_SWC].yr_row + yr_nrow * 0] = SW_Model.year;
		ForEachSoilLayer(i)
		{
			p_Rswc_yr[SW_Output[eSW_SWC].yr_row + yr_nrow * (i + 1)] = v->yravg.swc[i];
		}
		SW_Output[eSW_SWC].yr_row++;
		break;
	}

#ifdef STEPWAT
	ForEachSoilLayer(i)
	{
		switch (pd) {
			case eSW_Day: p = t->doy-1; break; /* print current but as index */
			case eSW_Week: p = t->week-1; break; /* print previous to current */
			case eSW_Month: p = t->month-1; break; /* print previous to current */
			/* YEAR should never be used with STEPWAT */
		}
		if (bFlush) p++;
		SXW.swc[Ilp(i,p)] = val;
	}
#endif

}

static void get_swcm(void) {
	/* --------------------------------------------------- */
	/* added 21-Oct-03, cwb */

	LyrIndex i;
	SW_SOILWAT *v = &SW_Soilwat;
	OutPeriod pd = SW_Output[eSW_SWCM].period;

	switch (pd) {
	case eSW_Day:
		p_Rvwc_dy[SW_Output[eSW_SWCM].dy_row + dy_nrow * 0] = SW_Model.year;
		p_Rvwc_dy[SW_Output[eSW_SWCM].dy_row + dy_nrow * 1] = SW_Model.doy;
		ForEachSoilLayer(i)
		{
			p_Rvwc_dy[SW_Output[eSW_SWCM].dy_row + dy_nrow * (i + 2)] = v->dysum.swcm[i] / SW_Site.lyr[i]->width;
		}
		SW_Output[eSW_SWCM].dy_row++;
		break;
	case eSW_Week:
		p_Rvwc_wk[SW_Output[eSW_SWCM].wk_row + wk_nrow * 0] = SW_Model.year;
		p_Rvwc_wk[SW_Output[eSW_SWCM].wk_row + wk_nrow * 1] = (SW_Model.week + 1) - tOffset;
		ForEachSoilLayer(i)
		{
			p_Rvwc_wk[SW_Output[eSW_SWCM].wk_row + wk_nrow * (i + 2)] = v->wkavg.swcm[i] / SW_Site.lyr[i]->width;
		}
		SW_Output[eSW_SWCM].wk_row++;
		break;
	case eSW_Month:
		p_Rvwc_mo[SW_Output[eSW_SWCM].mo_row + mo_nrow * 0] = SW_Model.year;
		p_Rvwc_mo[SW_Output[eSW_SWCM].mo_row + mo_nrow * 1] = (SW_Model.month + 1) - tOffset;
		ForEachSoilLayer(i)
		{
			p_Rvwc_mo[SW_Output[eSW_SWCM].mo_row + mo_nrow * (i + 2)] = v->moavg.swcm[i] / SW_Site.lyr[i]->width;
		}
		SW_Output[eSW_SWCM].mo_row++;
		break;
	case eSW_Year:
		p_Rvwc_yr[SW_Output[eSW_SWCM].yr_row + yr_nrow * 0] = SW_Model.year;
		ForEachSoilLayer(i)
		{
			p_Rvwc_yr[SW_Output[eSW_SWCM].yr_row + yr_nrow * (i + 1)] = v->yravg.swcm[i] / SW_Site.lyr[i]->width;
		}
		SW_Output[eSW_SWCM].yr_row++;
		break;
	}

}

static void get_swp(void) {
	/* --------------------------------------------------- */
	/* can't take arithmetic average of swp because it's
	 * exponential.  At this time (until I remember to look
	 * up whether harmonic or some other average is better
	 * and fix this) we're not averaging swp but converting
	 * the averaged swc.  This also avoids converting for
	 * each day.
	 *
	 * added 12-Oct-03, cwb */

	LyrIndex i;
	SW_SOILWAT *v = &SW_Soilwat;
	OutPeriod pd = SW_Output[eSW_SWP].period;

	switch (pd) {
	case eSW_Day:
		p_Rsw_pot_dy[SW_Output[eSW_SWP].dy_row + dy_nrow * 0] = SW_Model.year;
		p_Rsw_pot_dy[SW_Output[eSW_SWP].dy_row + dy_nrow * 1] = SW_Model.doy;
		ForEachSoilLayer(i)
		{
			p_Rsw_pot_dy[SW_Output[eSW_SWP].dy_row + dy_nrow * (i + 2)] = SW_SWC_vol2bars(v->dysum.swp[i], i);
		}
		SW_Output[eSW_SWP].dy_row++;
		break;
	case eSW_Week:
		p_Rsw_pot_wk[SW_Output[eSW_SWP].wk_row + wk_nrow * 0] = SW_Model.year;
		p_Rsw_pot_wk[SW_Output[eSW_SWP].wk_row + wk_nrow * 1] = (SW_Model.week + 1) - tOffset;
		ForEachSoilLayer(i)
		{
			p_Rsw_pot_wk[SW_Output[eSW_SWP].wk_row + wk_nrow * (i + 2)] = SW_SWC_vol2bars(v->wkavg.swp[i], i);
		}
		SW_Output[eSW_SWP].wk_row++;
		break;
	case eSW_Month:
		p_Rsw_pot_mo[SW_Output[eSW_SWP].mo_row + mo_nrow * 0] = SW_Model.year;
		p_Rsw_pot_mo[SW_Output[eSW_SWP].mo_row + mo_nrow * 1] = (SW_Model.month + 1) - tOffset;
		ForEachSoilLayer(i)
		{
			p_Rsw_pot_mo[SW_Output[eSW_SWP].mo_row + mo_nrow * (i + 2)] = SW_SWC_vol2bars(v->moavg.swp[i], i);
		}
		SW_Output[eSW_SWP].mo_row++;
		break;
	case eSW_Year:
		p_Rsw_pot_yr[SW_Output[eSW_SWP].yr_row + yr_nrow * 0] = SW_Model.year;
		ForEachSoilLayer(i)
		{
			p_Rsw_pot_yr[SW_Output[eSW_SWP].yr_row + yr_nrow * (i + 1)] = SW_SWC_vol2bars(v->yravg.swp[i], i);
		}
		SW_Output[eSW_SWP].yr_row++;
		break;
	}
}

static void get_swa(void) {
	/* --------------------------------------------------- */

	LyrIndex i;
	SW_SOILWAT *v = &SW_Soilwat;
	OutPeriod pd = SW_Output[eSW_SWA].period;

	switch (pd) {
	case eSW_Day:
		p_Rswa_dy[SW_Output[eSW_SWA].dy_row + dy_nrow * 0] = SW_Model.year;
		p_Rswa_dy[SW_Output[eSW_SWA].dy_row + dy_nrow * 1] = SW_Model.doy;
		ForEachSoilLayer(i)
		{
			p_Rswa_dy[SW_Output[eSW_SWA].dy_row + dy_nrow * (i + 2)] = v->dysum.swa[i];
		}
		SW_Output[eSW_SWA].dy_row++;
		break;
	case eSW_Week:
		p_Rswa_wk[SW_Output[eSW_SWA].wk_row + wk_nrow * 0] = SW_Model.year;
		p_Rswa_wk[SW_Output[eSW_SWA].wk_row + wk_nrow * 1] = (SW_Model.week + 1) - tOffset;
		ForEachSoilLayer(i)
		{
			p_Rswa_wk[SW_Output[eSW_SWA].wk_row + wk_nrow * (i + 2)] = v->wkavg.swa[i];
		}
		SW_Output[eSW_SWA].wk_row++;
		break;
	case eSW_Month:
		p_Rswa_mo[SW_Output[eSW_SWA].mo_row + mo_nrow * 0] = SW_Model.year;
		p_Rswa_mo[SW_Output[eSW_SWA].mo_row + mo_nrow * 1] = (SW_Model.month + 1) - tOffset;
		ForEachSoilLayer(i)
		{
			p_Rswa_mo[SW_Output[eSW_SWA].mo_row + mo_nrow * (i + 2)] = v->moavg.swa[i];
		}
		SW_Output[eSW_SWA].mo_row++;
		break;
	case eSW_Year:
		p_Rswa_yr[SW_Output[eSW_SWA].yr_row + yr_nrow * 0] = SW_Model.year;
		ForEachSoilLayer(i)
		{
			p_Rswa_yr[SW_Output[eSW_SWA].yr_row + yr_nrow * (i + 1)] = v->yravg.swa[i];
		}
		SW_Output[eSW_SWA].yr_row++;
		break;
	}

}

static void get_surfaceWater(void) {
	/* --------------------------------------------------- */
	SW_SOILWAT *v = &SW_Soilwat;
	OutPeriod pd = SW_Output[eSW_SurfaceWater].period;

	switch (pd) {
	case eSW_Day:
		p_Rsurface_water_dy[SW_Output[eSW_SurfaceWater].dy_row + dy_nrow * 0] = SW_Model.year;
		p_Rsurface_water_dy[SW_Output[eSW_SurfaceWater].dy_row + dy_nrow * 1] = SW_Model.doy;
		p_Rsurface_water_dy[SW_Output[eSW_SurfaceWater].dy_row + dy_nrow * 2] = v->dysum.surfaceWater;
		SW_Output[eSW_SurfaceWater].dy_row++;
		break;
	case eSW_Week:
		p_Rsurface_water_wk[SW_Output[eSW_SurfaceWater].wk_row + wk_nrow * 0] = SW_Model.year;
		p_Rsurface_water_wk[SW_Output[eSW_SurfaceWater].wk_row + wk_nrow * 1] = (SW_Model.week + 1) - tOffset;
		p_Rsurface_water_wk[SW_Output[eSW_SurfaceWater].wk_row + wk_nrow * 2] = v->wkavg.surfaceWater;
		SW_Output[eSW_SurfaceWater].wk_row++;
		break;
	case eSW_Month:
		p_Rsurface_water_mo[SW_Output[eSW_SurfaceWater].mo_row + mo_nrow * 0] = SW_Model.year;
		p_Rsurface_water_mo[SW_Output[eSW_SurfaceWater].mo_row + mo_nrow * 1] = (SW_Model.month + 1) - tOffset;
		p_Rsurface_water_mo[SW_Output[eSW_SurfaceWater].mo_row + mo_nrow * 2] = v->moavg.surfaceWater;
		SW_Output[eSW_SurfaceWater].mo_row++;
		break;
	case eSW_Year:
		p_Rsurface_water_yr[SW_Output[eSW_SurfaceWater].yr_row + yr_nrow * 0] = SW_Model.year;
		p_Rsurface_water_yr[SW_Output[eSW_SurfaceWater].yr_row + yr_nrow * 1] = v->yravg.surfaceWater;
		SW_Output[eSW_SurfaceWater].yr_row++;
		break;
	}
}

static void get_runoff(void) {
	/* --------------------------------------------------- */
	/* (12/13/2012) (clk) Added function to output runoff variables */

	SW_WEATHER *w = &SW_Weather;
	OutPeriod pd = SW_Output[eSW_Runoff].period;

	switch (pd) {
	case eSW_Day:
		p_Rrunoff_dy[SW_Output[eSW_Runoff].dy_row + dy_nrow * 0] = SW_Model.year;
		p_Rrunoff_dy[SW_Output[eSW_Runoff].dy_row + dy_nrow * 1] = SW_Model.doy;
		p_Rrunoff_dy[SW_Output[eSW_Runoff].dy_row + dy_nrow * 2] = (w->dysum.surfaceRunoff + w->dysum.snowRunoff);
		p_Rrunoff_dy[SW_Output[eSW_Runoff].dy_row + dy_nrow * 3] = w->dysum.surfaceRunoff;
		p_Rrunoff_dy[SW_Output[eSW_Runoff].dy_row + dy_nrow * 4] = w->dysum.snowRunoff;
		SW_Output[eSW_Runoff].dy_row++;
		break;
	case eSW_Week:
		p_Rrunoff_wk[SW_Output[eSW_Runoff].wk_row + wk_nrow * 0] = SW_Model.year;
		p_Rrunoff_wk[SW_Output[eSW_Runoff].wk_row + wk_nrow * 1] = (SW_Model.week + 1) - tOffset;
		p_Rrunoff_wk[SW_Output[eSW_Runoff].wk_row + wk_nrow * 2] = (w->wkavg.surfaceRunoff + w->wkavg.snowRunoff);
		p_Rrunoff_wk[SW_Output[eSW_Runoff].wk_row + wk_nrow * 3] = w->wkavg.surfaceRunoff;
		p_Rrunoff_wk[SW_Output[eSW_Runoff].wk_row + wk_nrow * 4] = w->wkavg.snowRunoff;
		SW_Output[eSW_Runoff].wk_row++;
		break;
	case eSW_Month:
		p_Rrunoff_mo[SW_Output[eSW_Runoff].mo_row + mo_nrow * 0] = SW_Model.year;
		p_Rrunoff_mo[SW_Output[eSW_Runoff].mo_row + mo_nrow * 1] = (SW_Model.month + 1) - tOffset;
		p_Rrunoff_mo[SW_Output[eSW_Runoff].mo_row + mo_nrow * 2] = (w->moavg.surfaceRunoff + w->moavg.snowRunoff);
		p_Rrunoff_mo[SW_Output[eSW_Runoff].mo_row + mo_nrow * 3] = w->moavg.surfaceRunoff;
		p_Rrunoff_mo[SW_Output[eSW_Runoff].mo_row + mo_nrow * 4] = w->moavg.snowRunoff;
		SW_Output[eSW_Runoff].mo_row++;
		break;
	case eSW_Year:
		p_Rrunoff_yr[SW_Output[eSW_Runoff].yr_row + yr_nrow * 0] = SW_Model.year;
		p_Rrunoff_yr[SW_Output[eSW_Runoff].yr_row + yr_nrow * 1] = (w->yravg.surfaceRunoff + w->yravg.snowRunoff);
		p_Rrunoff_yr[SW_Output[eSW_Runoff].yr_row + yr_nrow * 2] = w->yravg.surfaceRunoff;
		p_Rrunoff_yr[SW_Output[eSW_Runoff].yr_row + yr_nrow * 3] = w->yravg.snowRunoff;
		SW_Output[eSW_Runoff].yr_row++;
		break;
	}
}

static void get_transp(void) {
	/* --------------------------------------------------- */
	/* 10-May-02 (cwb) Added conditional code to interface
	 *           with STEPPE.
	 */
	LyrIndex i;
	SW_SOILWAT *v = &SW_Soilwat;
	OutPeriod pd = SW_Output[eSW_Transp].period;

	/* Date Info-component transpiration */
	switch (pd) {
	case eSW_Day:
		p_Rtransp_dy[SW_Output[eSW_Transp].dy_row + dy_nrow * 0] = SW_Model.year;
		p_Rtransp_dy[SW_Output[eSW_Transp].dy_row + dy_nrow * 1] = SW_Model.doy;
		break;
	case eSW_Week:
		p_Rtransp_wk[SW_Output[eSW_Transp].wk_row + wk_nrow * 0] = SW_Model.year;
		p_Rtransp_wk[SW_Output[eSW_Transp].wk_row + wk_nrow * 1] = (SW_Model.week + 1) - tOffset;
		break;
	case eSW_Month:
		p_Rtransp_mo[SW_Output[eSW_Transp].mo_row + mo_nrow * 0] = SW_Model.year;
		p_Rtransp_mo[SW_Output[eSW_Transp].mo_row + mo_nrow * 1] = (SW_Model.month + 1) - tOffset;
		break;
	case eSW_Year:
		p_Rtransp_yr[SW_Output[eSW_Transp].yr_row + yr_nrow * 0] = SW_Model.year;
		break;
	}

#ifdef STEPWAT
	TimeInt p;
	SW_MODEL *t = &SW_Model;
#endif

	/* total-component transpiration */
	switch (pd) {
	case eSW_Day:
		ForEachSoilLayer(i)
		{
			p_Rtransp_dy[SW_Output[eSW_Transp].dy_row + dy_nrow * (i + 2)] = v->dysum.transp_total[i];
		}
		break;
	case eSW_Week:
		ForEachSoilLayer(i)
		{
			p_Rtransp_wk[SW_Output[eSW_Transp].wk_row + wk_nrow * (i + 2)] = v->wkavg.transp_total[i];
		}
		break;
	case eSW_Month:
		ForEachSoilLayer(i)
		{
			p_Rtransp_mo[SW_Output[eSW_Transp].mo_row + mo_nrow * (i + 2)] = v->moavg.transp_total[i];
		}
		break;
	case eSW_Year:
		ForEachSoilLayer(i)
		{
			p_Rtransp_yr[SW_Output[eSW_Transp].yr_row + yr_nrow * (i + 1)] = v->yravg.transp_total[i];
		}
		break;
	}

#ifdef STEPWAT
	ForEachSoilLayer(i)
	{
		switch (pd) {
			case eSW_Day: p = t->doy-1; break; /* print current but as index */
			case eSW_Week: p = t->week-1; break; /* print previous to current */
			case eSW_Month: p = t->month-1; break; /* print previous to current */
			/* YEAR should never be used with STEPWAT */
		}
		if (bFlush) p++;
		SXW.transp[Ilp(i,p)] = val;
	}
#endif
	/* Tree-component transpiration */
	switch (pd) {
	case eSW_Day:
		ForEachSoilLayer(i)
		{
			p_Rtransp_dy[SW_Output[eSW_Transp].dy_row + dy_nrow * (i + 2) + (dy_nrow * SW_Site.n_layers * 1)] = v->dysum.transp_tree[i];
		}
		break;
	case eSW_Week:
		ForEachSoilLayer(i)
		{
			p_Rtransp_wk[SW_Output[eSW_Transp].wk_row + wk_nrow * (i + 2) + (wk_nrow * SW_Site.n_layers * 1)] = v->wkavg.transp_tree[i];
		}
		break;
	case eSW_Month:
		ForEachSoilLayer(i)
		{
			p_Rtransp_mo[SW_Output[eSW_Transp].mo_row + mo_nrow * (i + 2) + (mo_nrow * SW_Site.n_layers * 1)] = v->moavg.transp_tree[i];
		}
		break;
	case eSW_Year:
		ForEachSoilLayer(i)
		{
			p_Rtransp_yr[SW_Output[eSW_Transp].yr_row + yr_nrow * (i + 1) + (yr_nrow * SW_Site.n_layers * 1)] = v->yravg.transp_tree[i];
		}
		break;
	}

	/* shrub-component transpiration */
	switch (pd) {
	case eSW_Day:
		ForEachSoilLayer(i)
		{
			p_Rtransp_dy[SW_Output[eSW_Transp].dy_row + dy_nrow * (i + 2) + (dy_nrow * SW_Site.n_layers * 2)] = v->dysum.transp_shrub[i];
		}
		break;
	case eSW_Week:
		ForEachSoilLayer(i)
		{
			p_Rtransp_wk[SW_Output[eSW_Transp].wk_row + wk_nrow * (i + 2) + (wk_nrow * SW_Site.n_layers * 2)] = v->wkavg.transp_shrub[i];
		}
		break;
	case eSW_Month:
		ForEachSoilLayer(i)
		{
			p_Rtransp_mo[SW_Output[eSW_Transp].mo_row + mo_nrow * (i + 2) + (mo_nrow * SW_Site.n_layers * 2)] = v->moavg.transp_shrub[i];
		}
		break;
	case eSW_Year:
		ForEachSoilLayer(i)
		{
			p_Rtransp_yr[SW_Output[eSW_Transp].yr_row + yr_nrow * (i + 1) + (yr_nrow * SW_Site.n_layers * 2)] = v->yravg.transp_shrub[i];
		}
		break;
	}

	/* grass-component transpiration */
	switch (pd) {
	case eSW_Day:
		ForEachSoilLayer(i)
		{
			p_Rtransp_dy[SW_Output[eSW_Transp].dy_row + dy_nrow * (i + 2) + (dy_nrow * SW_Site.n_layers * 3)] = v->dysum.transp_grass[i];
		}
		SW_Output[eSW_Transp].dy_row++;
		break;
	case eSW_Week:
		ForEachSoilLayer(i)
		{
			p_Rtransp_wk[SW_Output[eSW_Transp].wk_row + wk_nrow * (i + 2) + (wk_nrow * SW_Site.n_layers * 3)] = v->wkavg.transp_grass[i];
		}
		SW_Output[eSW_Transp].wk_row++;
		break;
	case eSW_Month:
		ForEachSoilLayer(i)
		{
			p_Rtransp_mo[SW_Output[eSW_Transp].mo_row + mo_nrow * (i + 2) + (mo_nrow * SW_Site.n_layers * 3)] = v->moavg.transp_grass[i];
		}
		SW_Output[eSW_Transp].mo_row++;
		break;
	case eSW_Year:
		ForEachSoilLayer(i)
		{
			p_Rtransp_yr[SW_Output[eSW_Transp].yr_row + yr_nrow * (i + 1) + (yr_nrow * SW_Site.n_layers * 3)] = v->yravg.transp_grass[i];
		}
		SW_Output[eSW_Transp].yr_row++;
		break;
	}
}

static void get_evapSoil(void) {
	/* --------------------------------------------------- */
	LyrIndex i;
	SW_SOILWAT *v = &SW_Soilwat;
	OutPeriod pd = SW_Output[eSW_EvapSoil].period;

	switch (pd) {
	case eSW_Day:
		p_Revap_soil_dy[SW_Output[eSW_EvapSoil].dy_row + dy_nrow * 0] = SW_Model.year;
		p_Revap_soil_dy[SW_Output[eSW_EvapSoil].dy_row + dy_nrow * 1] = SW_Model.doy;
		ForEachEvapLayer(i)
		{
			p_Revap_soil_dy[SW_Output[eSW_EvapSoil].dy_row + dy_nrow * (i + 2)] = v->dysum.evap[i];
		}
		SW_Output[eSW_EvapSoil].dy_row++;
		break;
	case eSW_Week:
		p_Revap_soil_wk[SW_Output[eSW_EvapSoil].wk_row + wk_nrow * 0] = SW_Model.year;
		p_Revap_soil_wk[SW_Output[eSW_EvapSoil].wk_row + wk_nrow * 1] = (SW_Model.week + 1) - tOffset;
		ForEachEvapLayer(i)
		{
			p_Revap_soil_wk[SW_Output[eSW_EvapSoil].wk_row + wk_nrow * (i + 2)] = v->wkavg.evap[i];
		}
		SW_Output[eSW_EvapSoil].wk_row++;
		break;
	case eSW_Month:
		p_Revap_soil_mo[SW_Output[eSW_EvapSoil].mo_row + mo_nrow * 0] = SW_Model.year;
		p_Revap_soil_mo[SW_Output[eSW_EvapSoil].mo_row + mo_nrow * 1] = (SW_Model.month + 1) - tOffset;
		ForEachEvapLayer(i)
		{
			p_Revap_soil_mo[SW_Output[eSW_EvapSoil].mo_row + mo_nrow * (i + 2)] = v->moavg.evap[i];
		}
		SW_Output[eSW_EvapSoil].mo_row++;
		break;
	case eSW_Year:
		p_Revap_soil_yr[SW_Output[eSW_EvapSoil].yr_row + yr_nrow * 0] = SW_Model.year;
		ForEachEvapLayer(i)
		{
			p_Revap_soil_yr[SW_Output[eSW_EvapSoil].yr_row + yr_nrow * (i + 1)] = v->yravg.evap[i];
		}
		SW_Output[eSW_EvapSoil].yr_row++;
		break;
	}

}

static void get_evapSurface(void) {
	/* --------------------------------------------------- */
	SW_SOILWAT *v = &SW_Soilwat;
	OutPeriod pd = SW_Output[eSW_EvapSurface].period;

	switch (pd) {
	case eSW_Day:
		p_Revap_surface_dy[SW_Output[eSW_EvapSurface].dy_row + dy_nrow * 0] = SW_Model.year;
		p_Revap_surface_dy[SW_Output[eSW_EvapSurface].dy_row + dy_nrow * 1] = SW_Model.doy;
		p_Revap_surface_dy[SW_Output[eSW_EvapSurface].dy_row + dy_nrow * 2] = v->dysum.total_evap;
		p_Revap_surface_dy[SW_Output[eSW_EvapSurface].dy_row + dy_nrow * 3] = v->dysum.tree_evap;
		p_Revap_surface_dy[SW_Output[eSW_EvapSurface].dy_row + dy_nrow * 4] = v->dysum.shrub_evap;
		p_Revap_surface_dy[SW_Output[eSW_EvapSurface].dy_row + dy_nrow * 5] = v->dysum.grass_evap;
		p_Revap_surface_dy[SW_Output[eSW_EvapSurface].dy_row + dy_nrow * 6] = v->dysum.litter_evap;
		p_Revap_surface_dy[SW_Output[eSW_EvapSurface].dy_row + dy_nrow * 7] = v->dysum.surfaceWater_evap;
		SW_Output[eSW_EvapSurface].dy_row++;
		break;
	case eSW_Week:
		p_Revap_surface_wk[SW_Output[eSW_EvapSurface].wk_row + wk_nrow * 0] = SW_Model.year;
		p_Revap_surface_wk[SW_Output[eSW_EvapSurface].wk_row + wk_nrow * 1] = (SW_Model.week + 1) - tOffset;
		p_Revap_surface_wk[SW_Output[eSW_EvapSurface].wk_row + wk_nrow * 2] = v->wkavg.total_evap;
		p_Revap_surface_wk[SW_Output[eSW_EvapSurface].wk_row + wk_nrow * 3] = v->wkavg.tree_evap;
		p_Revap_surface_wk[SW_Output[eSW_EvapSurface].wk_row + wk_nrow * 4] = v->wkavg.shrub_evap;
		p_Revap_surface_wk[SW_Output[eSW_EvapSurface].wk_row + wk_nrow * 5] = v->wkavg.grass_evap;
		p_Revap_surface_wk[SW_Output[eSW_EvapSurface].wk_row + wk_nrow * 6] = v->wkavg.litter_evap;
		p_Revap_surface_wk[SW_Output[eSW_EvapSurface].wk_row + wk_nrow * 7] = v->wkavg.surfaceWater_evap;
		SW_Output[eSW_EvapSurface].wk_row++;
		break;
	case eSW_Month:
		p_Revap_surface_mo[SW_Output[eSW_EvapSurface].mo_row + mo_nrow * 0] = SW_Model.year;
		p_Revap_surface_mo[SW_Output[eSW_EvapSurface].mo_row + mo_nrow * 1] = (SW_Model.month + 1) - tOffset;
		p_Revap_surface_mo[SW_Output[eSW_EvapSurface].mo_row + mo_nrow * 2] = v->moavg.total_evap;
		p_Revap_surface_mo[SW_Output[eSW_EvapSurface].mo_row + mo_nrow * 3] = v->moavg.tree_evap;
		p_Revap_surface_mo[SW_Output[eSW_EvapSurface].mo_row + mo_nrow * 4] = v->moavg.shrub_evap;
		p_Revap_surface_mo[SW_Output[eSW_EvapSurface].mo_row + mo_nrow * 5] = v->moavg.grass_evap;
		p_Revap_surface_mo[SW_Output[eSW_EvapSurface].mo_row + mo_nrow * 6] = v->moavg.litter_evap;
		p_Revap_surface_mo[SW_Output[eSW_EvapSurface].mo_row + mo_nrow * 7] = v->moavg.surfaceWater_evap;
		SW_Output[eSW_EvapSurface].mo_row++;
		break;
	case eSW_Year:
		p_Revap_surface_yr[SW_Output[eSW_EvapSurface].yr_row + yr_nrow * 0] = SW_Model.year;
		p_Revap_surface_yr[SW_Output[eSW_EvapSurface].yr_row + yr_nrow * 1] = v->yravg.total_evap;
		p_Revap_surface_yr[SW_Output[eSW_EvapSurface].yr_row + yr_nrow * 2] = v->yravg.tree_evap;
		p_Revap_surface_yr[SW_Output[eSW_EvapSurface].yr_row + yr_nrow * 3] = v->yravg.shrub_evap;
		p_Revap_surface_yr[SW_Output[eSW_EvapSurface].yr_row + yr_nrow * 4] = v->yravg.grass_evap;
		p_Revap_surface_yr[SW_Output[eSW_EvapSurface].yr_row + yr_nrow * 5] = v->yravg.litter_evap;
		p_Revap_surface_yr[SW_Output[eSW_EvapSurface].yr_row + yr_nrow * 6] = v->yravg.surfaceWater_evap;
		SW_Output[eSW_EvapSurface].yr_row++;
		break;
	}
}

static void get_interception(void) {
	/* --------------------------------------------------- */
	SW_SOILWAT *v = &SW_Soilwat;
	OutPeriod pd = SW_Output[eSW_Interception].period;

	switch (pd) {
	case eSW_Day:
		p_Rinterception_dy[SW_Output[eSW_Interception].dy_row + dy_nrow * 0] = SW_Model.year;
		p_Rinterception_dy[SW_Output[eSW_Interception].dy_row + dy_nrow * 1] = SW_Model.doy;
		p_Rinterception_dy[SW_Output[eSW_Interception].dy_row + dy_nrow * 2] = v->dysum.total_int;
		p_Rinterception_dy[SW_Output[eSW_Interception].dy_row + dy_nrow * 3] = v->dysum.tree_int;
		p_Rinterception_dy[SW_Output[eSW_Interception].dy_row + dy_nrow * 4] = v->dysum.shrub_int;
		p_Rinterception_dy[SW_Output[eSW_Interception].dy_row + dy_nrow * 5] = v->dysum.grass_int;
		p_Rinterception_dy[SW_Output[eSW_Interception].dy_row + dy_nrow * 6] = v->dysum.litter_int;
		SW_Output[eSW_Interception].dy_row++;
		break;
	case eSW_Week:
		p_Rinterception_wk[SW_Output[eSW_Interception].wk_row + wk_nrow * 0] = SW_Model.year;
		p_Rinterception_wk[SW_Output[eSW_Interception].wk_row + wk_nrow * 1] = (SW_Model.week + 1) - tOffset;
		p_Rinterception_wk[SW_Output[eSW_Interception].wk_row + wk_nrow * 2] = v->wkavg.total_int;
		p_Rinterception_wk[SW_Output[eSW_Interception].wk_row + wk_nrow * 3] = v->wkavg.tree_int;
		p_Rinterception_wk[SW_Output[eSW_Interception].wk_row + wk_nrow * 4] = v->wkavg.shrub_int;
		p_Rinterception_wk[SW_Output[eSW_Interception].wk_row + wk_nrow * 5] = v->wkavg.grass_int;
		p_Rinterception_wk[SW_Output[eSW_Interception].wk_row + wk_nrow * 6] = v->wkavg.litter_int;
		SW_Output[eSW_Interception].wk_row++;
		break;
	case eSW_Month:
		p_Rinterception_mo[SW_Output[eSW_Interception].mo_row + mo_nrow * 0] = SW_Model.year;
		p_Rinterception_mo[SW_Output[eSW_Interception].mo_row + mo_nrow * 1] = (SW_Model.month + 1) - tOffset;
		p_Rinterception_mo[SW_Output[eSW_Interception].mo_row + mo_nrow * 2] = v->moavg.total_int;
		p_Rinterception_mo[SW_Output[eSW_Interception].mo_row + mo_nrow * 3] = v->moavg.tree_int;
		p_Rinterception_mo[SW_Output[eSW_Interception].mo_row + mo_nrow * 4] = v->moavg.shrub_int;
		p_Rinterception_mo[SW_Output[eSW_Interception].mo_row + mo_nrow * 5] = v->moavg.grass_int;
		p_Rinterception_mo[SW_Output[eSW_Interception].mo_row + mo_nrow * 6] = v->moavg.litter_int;
		SW_Output[eSW_Interception].mo_row++;
		break;
	case eSW_Year:
		p_Rinterception_yr[SW_Output[eSW_Interception].yr_row + yr_nrow * 0] = SW_Model.year;
		p_Rinterception_yr[SW_Output[eSW_Interception].yr_row + yr_nrow * 1] = v->yravg.total_int;
		p_Rinterception_yr[SW_Output[eSW_Interception].yr_row + yr_nrow * 2] = v->yravg.tree_int;
		p_Rinterception_yr[SW_Output[eSW_Interception].yr_row + yr_nrow * 3] = v->yravg.shrub_int;
		p_Rinterception_yr[SW_Output[eSW_Interception].yr_row + yr_nrow * 4] = v->yravg.grass_int;
		p_Rinterception_yr[SW_Output[eSW_Interception].yr_row + yr_nrow * 5] = v->yravg.litter_int;
		SW_Output[eSW_Interception].yr_row++;
		break;
	}
}

static void get_soilinf(void) {
	/* --------------------------------------------------- */
	/* 20100202 (drs) added */
	/* 20110219 (drs) added runoff */
	/* 12/13/2012	(clk)	moved runoff, now named snowRunoff, to get_runoff(); */
	SW_WEATHER *v = &SW_Weather;
	OutPeriod pd = SW_Output[eSW_SoilInf].period;

	switch (pd) {
	case eSW_Day:
		p_Rinfiltration_dy[SW_Output[eSW_SoilInf].dy_row + dy_nrow * 0] = SW_Model.year;
		p_Rinfiltration_dy[SW_Output[eSW_SoilInf].dy_row + dy_nrow * 1] = SW_Model.doy;
		p_Rinfiltration_dy[SW_Output[eSW_SoilInf].dy_row + dy_nrow * 2] = v->dysum.soil_inf;
		SW_Output[eSW_SoilInf].dy_row++;
		break;
	case eSW_Week:
		p_Rinfiltration_wk[SW_Output[eSW_SoilInf].wk_row + wk_nrow * 0] = SW_Model.year;
		p_Rinfiltration_wk[SW_Output[eSW_SoilInf].wk_row + wk_nrow * 1] = (SW_Model.week + 1) - tOffset;
		p_Rinfiltration_wk[SW_Output[eSW_SoilInf].wk_row + wk_nrow * 2] = v->wkavg.soil_inf;
		SW_Output[eSW_SoilInf].wk_row++;
		break;
	case eSW_Month:
		p_Rinfiltration_mo[SW_Output[eSW_SoilInf].mo_row + mo_nrow * 0] = SW_Model.year;
		p_Rinfiltration_mo[SW_Output[eSW_SoilInf].mo_row + mo_nrow * 1] = (SW_Model.month + 1) - tOffset;
		p_Rinfiltration_mo[SW_Output[eSW_SoilInf].mo_row + mo_nrow * 2] = v->moavg.soil_inf;
		SW_Output[eSW_SoilInf].mo_row++;
		break;
	case eSW_Year:
		p_Rinfiltration_yr[SW_Output[eSW_SoilInf].yr_row + yr_nrow * 0] = SW_Model.year;
		p_Rinfiltration_yr[SW_Output[eSW_SoilInf].yr_row + yr_nrow * 1] = v->yravg.soil_inf;
		SW_Output[eSW_SoilInf].yr_row++;
		break;
	}

}

static void get_lyrdrain(void) {
	/* --------------------------------------------------- */
	/* 20100202 (drs) added */
	LyrIndex i;
	SW_SOILWAT *v = &SW_Soilwat;
	OutPeriod pd = SW_Output[eSW_LyrDrain].period;

	switch (pd) {
	case eSW_Day:
		p_Rpercolation_dy[SW_Output[eSW_LyrDrain].dy_row + dy_nrow * 0] = SW_Model.year;
		p_Rpercolation_dy[SW_Output[eSW_LyrDrain].dy_row + dy_nrow * 1] = SW_Model.doy;
		for (i = 0; i < SW_Site.n_layers - 1; i++) {
			p_Rpercolation_dy[SW_Output[eSW_LyrDrain].dy_row + dy_nrow * (i + 2)] = v->dysum.lyrdrain[i];
		}
		SW_Output[eSW_LyrDrain].dy_row++;
		break;
	case eSW_Week:
		p_Rpercolation_wk[SW_Output[eSW_LyrDrain].wk_row + wk_nrow * 0] = SW_Model.year;
		p_Rpercolation_wk[SW_Output[eSW_LyrDrain].wk_row + wk_nrow * 1] = (SW_Model.week + 1) - tOffset;
		for (i = 0; i < SW_Site.n_layers - 1; i++) {
			p_Rpercolation_wk[SW_Output[eSW_LyrDrain].wk_row + wk_nrow * (i + 2)] = v->wkavg.lyrdrain[i];
		}
		SW_Output[eSW_LyrDrain].wk_row++;
		break;
	case eSW_Month:
		p_Rpercolation_mo[SW_Output[eSW_LyrDrain].mo_row + mo_nrow * 0] = SW_Model.year;
		p_Rpercolation_mo[SW_Output[eSW_LyrDrain].mo_row + mo_nrow * 1] = (SW_Model.month + 1) - tOffset;
		for (i = 0; i < SW_Site.n_layers - 1; i++) {
			p_Rpercolation_mo[SW_Output[eSW_LyrDrain].mo_row + mo_nrow * (i + 2)] = v->moavg.lyrdrain[i];
		}
		SW_Output[eSW_LyrDrain].mo_row++;
		break;
	case eSW_Year:
		p_Rpercolation_yr[SW_Output[eSW_LyrDrain].yr_row + yr_nrow * 0] = SW_Model.year;
		for (i = 0; i < SW_Site.n_layers - 1; i++) {
			p_Rpercolation_yr[SW_Output[eSW_LyrDrain].yr_row + yr_nrow * (i + 1)] = v->yravg.lyrdrain[i];
		}
		SW_Output[eSW_LyrDrain].yr_row++;
		break;
	}
}

static void get_hydred(void) {
	/* --------------------------------------------------- */
	/* 20101020 (drs) added */
	LyrIndex i;
	SW_SOILWAT *v = &SW_Soilwat;
	OutPeriod pd = SW_Output[eSW_HydRed].period;

	/* Date Info output */
	switch (pd) {
	case eSW_Day:
		p_Rhydred_dy[SW_Output[eSW_HydRed].dy_row + dy_nrow * 0] = SW_Model.year;
		p_Rhydred_dy[SW_Output[eSW_HydRed].dy_row + dy_nrow * 1] = SW_Model.doy;
		break;
	case eSW_Week:
		p_Rhydred_wk[SW_Output[eSW_HydRed].wk_row + wk_nrow * 0] = SW_Model.year;
		p_Rhydred_wk[SW_Output[eSW_HydRed].wk_row + wk_nrow * 1] = (SW_Model.week + 1) - tOffset;
		break;
	case eSW_Month:
		p_Rhydred_mo[SW_Output[eSW_HydRed].mo_row + mo_nrow * 0] = SW_Model.year;
		p_Rhydred_mo[SW_Output[eSW_HydRed].mo_row + mo_nrow * 1] = (SW_Model.month + 1) - tOffset;
		break;
	case eSW_Year:
		p_Rhydred_yr[SW_Output[eSW_HydRed].yr_row + yr_nrow * 0] = SW_Model.year;
		break;
	}

	/* total output */
	switch (pd) {
	case eSW_Day:
		ForEachSoilLayer(i)
		{
			p_Rhydred_dy[SW_Output[eSW_HydRed].dy_row + dy_nrow * (i + 2) + (dy_nrow * SW_Site.n_layers * 0)] = v->dysum.hydred_total[i];
		}
		break;
	case eSW_Week:
		ForEachSoilLayer(i)
		{
			p_Rhydred_wk[SW_Output[eSW_HydRed].wk_row + wk_nrow * (i + 2) + (wk_nrow * SW_Site.n_layers * 0)] = v->wkavg.hydred_total[i];
		}
		break;
	case eSW_Month:
		ForEachSoilLayer(i)
		{
			p_Rhydred_mo[SW_Output[eSW_HydRed].mo_row + mo_nrow * (i + 2) + (mo_nrow * SW_Site.n_layers * 0)] = v->moavg.hydred_total[i];
		}
		break;
	case eSW_Year:
		ForEachSoilLayer(i)
		{
			p_Rhydred_yr[SW_Output[eSW_HydRed].yr_row + yr_nrow * (i + 1) + (yr_nrow * SW_Site.n_layers * 0)] = v->yravg.hydred_total[i];
		}
		break;
	}

	/* tree output */
	switch (pd) {
	case eSW_Day:
		ForEachSoilLayer(i)
		{
			p_Rhydred_dy[SW_Output[eSW_HydRed].dy_row + dy_nrow * (i + 2) + (dy_nrow * SW_Site.n_layers * 1)] = v->dysum.hydred_tree[i];
		}
		break;
	case eSW_Week:
		ForEachSoilLayer(i)
		{
			p_Rhydred_wk[SW_Output[eSW_HydRed].wk_row + wk_nrow * (i + 2) + (wk_nrow * SW_Site.n_layers * 1)] = v->wkavg.hydred_tree[i];
		}
		break;
	case eSW_Month:
		ForEachSoilLayer(i)
		{
			p_Rhydred_mo[SW_Output[eSW_HydRed].mo_row + mo_nrow * (i + 2) + (mo_nrow * SW_Site.n_layers * 1)] = v->moavg.hydred_tree[i];
		}
		break;
	case eSW_Year:
		ForEachSoilLayer(i)
		{
			p_Rhydred_yr[SW_Output[eSW_HydRed].yr_row + yr_nrow * (i + 1) + (yr_nrow * SW_Site.n_layers * 1)] = v->yravg.hydred_tree[i];
		}
		break;
	}

	/* shrub output */
	switch (pd) {
	case eSW_Day:
		ForEachSoilLayer(i)
		{
			p_Rhydred_dy[SW_Output[eSW_HydRed].dy_row + dy_nrow * (i + 2) + (dy_nrow * SW_Site.n_layers * 2)] = v->dysum.hydred_shrub[i];
		}
		break;
	case eSW_Week:
		ForEachSoilLayer(i)
		{
			p_Rhydred_wk[SW_Output[eSW_HydRed].wk_row + wk_nrow * (i + 2) + (wk_nrow * SW_Site.n_layers * 2)] = v->wkavg.hydred_shrub[i];
		}
		break;
	case eSW_Month:
		ForEachSoilLayer(i)
		{
			p_Rhydred_mo[SW_Output[eSW_HydRed].mo_row + mo_nrow * (i + 2) + (mo_nrow * SW_Site.n_layers * 2)] = v->moavg.hydred_shrub[i];
		}
		break;
	case eSW_Year:
		ForEachSoilLayer(i)
		{
			p_Rhydred_yr[SW_Output[eSW_HydRed].yr_row + yr_nrow * (i + 1) + (yr_nrow * SW_Site.n_layers * 2)] = v->yravg.hydred_shrub[i];
		}
		break;
	}

	/* grass output */
	switch (pd) {
	case eSW_Day:
		ForEachSoilLayer(i)
		{
			p_Rhydred_dy[SW_Output[eSW_HydRed].dy_row + dy_nrow * (i + 2) + (dy_nrow * SW_Site.n_layers * 3)] = v->dysum.hydred_grass[i];
		}
		SW_Output[eSW_HydRed].dy_row++;
		break;
	case eSW_Week:
		ForEachSoilLayer(i)
		{
			p_Rhydred_wk[SW_Output[eSW_HydRed].wk_row + wk_nrow * (i + 2) + (wk_nrow * SW_Site.n_layers * 3)] = v->wkavg.hydred_grass[i];
		}
		SW_Output[eSW_HydRed].wk_row++;
		break;
	case eSW_Month:
		ForEachSoilLayer(i)
		{
			p_Rhydred_mo[SW_Output[eSW_HydRed].mo_row + mo_nrow * (i + 2) + (mo_nrow * SW_Site.n_layers * 3)] = v->moavg.hydred_grass[i];
		}
		SW_Output[eSW_HydRed].mo_row++;
		break;
	case eSW_Year:
		ForEachSoilLayer(i)
		{
			p_Rhydred_yr[SW_Output[eSW_HydRed].yr_row + yr_nrow * (i + 1) + (yr_nrow * SW_Site.n_layers * 3)] = v->yravg.hydred_grass[i];
		}
		SW_Output[eSW_HydRed].yr_row++;
		break;
	}
}

static void get_aet(void) {
	/* --------------------------------------------------- */
	SW_SOILWAT *v = &SW_Soilwat;
	OutPeriod pd = SW_Output[eSW_AET].period;

	switch (pd) {
	case eSW_Day:
		p_Raet_dy[SW_Output[eSW_AET].dy_row + dy_nrow * 0] = SW_Model.year;
		p_Raet_dy[SW_Output[eSW_AET].dy_row + dy_nrow * 1] = SW_Model.doy;
		p_Raet_dy[SW_Output[eSW_AET].dy_row + dy_nrow * 2] = v->dysum.aet;
		SW_Output[eSW_AET].dy_row++;
		break;
	case eSW_Week:
		p_Raet_wk[SW_Output[eSW_AET].wk_row + wk_nrow * 0] = SW_Model.year;
		p_Raet_wk[SW_Output[eSW_AET].wk_row + wk_nrow * 1] = (SW_Model.week + 1) - tOffset;
		p_Raet_wk[SW_Output[eSW_AET].wk_row + wk_nrow * 2] = v->wkavg.aet;
		SW_Output[eSW_AET].wk_row++;
		break;
	case eSW_Month:
		p_Raet_mo[SW_Output[eSW_AET].mo_row + mo_nrow * 0] = SW_Model.year;
		p_Raet_mo[SW_Output[eSW_AET].mo_row + mo_nrow * 1] = (SW_Model.month + 1) - tOffset;
		p_Raet_mo[SW_Output[eSW_AET].mo_row + mo_nrow * 2] = v->moavg.aet;
		SW_Output[eSW_AET].mo_row++;
		break;
	case eSW_Year:
		p_Raet_yr[SW_Output[eSW_AET].yr_row + yr_nrow * 0] = SW_Model.year;
		p_Raet_yr[SW_Output[eSW_AET].yr_row + yr_nrow * 1] = v->yravg.aet;
		SW_Output[eSW_AET].yr_row++;
		break;
	}

#ifdef STEPWAT
	SXW.aet += val;
#endif
}

static void get_pet(void) {
	/* --------------------------------------------------- */
	SW_SOILWAT *v = &SW_Soilwat;
	OutPeriod pd = SW_Output[eSW_PET].period;

	switch (pd) {
	case eSW_Day:
		p_Rpet_dy[SW_Output[eSW_PET].dy_row + dy_nrow * 0] = SW_Model.year;
		p_Rpet_dy[SW_Output[eSW_PET].dy_row + dy_nrow * 1] = SW_Model.doy;
		p_Rpet_dy[SW_Output[eSW_PET].dy_row + dy_nrow * 2] = v->dysum.pet;
		SW_Output[eSW_PET].dy_row++;
		break;
	case eSW_Week:
		p_Rpet_wk[SW_Output[eSW_PET].wk_row + wk_nrow * 0] = SW_Model.year;
		p_Rpet_wk[SW_Output[eSW_PET].wk_row + wk_nrow * 1] = (SW_Model.week + 1) - tOffset;
		p_Rpet_wk[SW_Output[eSW_PET].wk_row + wk_nrow * 2] = v->wkavg.pet;
		SW_Output[eSW_PET].wk_row++;
		break;
	case eSW_Month:
		p_Rpet_mo[SW_Output[eSW_PET].mo_row + mo_nrow * 0] = SW_Model.year;
		p_Rpet_mo[SW_Output[eSW_PET].mo_row + mo_nrow * 1] = (SW_Model.month + 1) - tOffset;
		p_Rpet_mo[SW_Output[eSW_PET].mo_row + mo_nrow * 2] = v->moavg.pet;
		SW_Output[eSW_PET].mo_row++;
		break;
	case eSW_Year:
		p_Rpet_yr[SW_Output[eSW_PET].yr_row + yr_nrow * 0] = SW_Model.year;
		p_Rpet_yr[SW_Output[eSW_PET].yr_row + yr_nrow * 1] = v->yravg.pet;
		SW_Output[eSW_PET].yr_row++;
		break;
	}
}

static void get_wetdays(void) {
	/* --------------------------------------------------- */
	LyrIndex i;
	SW_SOILWAT *v = &SW_Soilwat;
	OutPeriod pd = SW_Output[eSW_WetDays].period;

	switch (pd) {
	case eSW_Day:
		p_Rwetdays_dy[SW_Output[eSW_WetDays].dy_row + dy_nrow * 0] = SW_Model.year;
		p_Rwetdays_dy[SW_Output[eSW_WetDays].dy_row + dy_nrow * 1] = SW_Model.doy;
		ForEachSoilLayer(i)
		{
			p_Rwetdays_dy[SW_Output[eSW_WetDays].dy_row + dy_nrow * (i + 2)] = (v->is_wet[i]) ? 1 : 0;
		}
		SW_Output[eSW_WetDays].dy_row++;
		break;
	case eSW_Week:
		p_Rwetdays_wk[SW_Output[eSW_WetDays].wk_row + wk_nrow * 0] = SW_Model.year;
		p_Rwetdays_wk[SW_Output[eSW_WetDays].wk_row + wk_nrow * 1] = (SW_Model.week + 1) - tOffset;
		ForEachSoilLayer(i)
		{
			p_Rwetdays_wk[SW_Output[eSW_WetDays].wk_row + wk_nrow * (i + 2)] = (int) v->wkavg.wetdays[i];
		}
		SW_Output[eSW_WetDays].wk_row++;
		break;
	case eSW_Month:
		p_Rwetdays_mo[SW_Output[eSW_WetDays].mo_row + mo_nrow * 0] = SW_Model.year;
		p_Rwetdays_mo[SW_Output[eSW_WetDays].mo_row + mo_nrow * 1] = (SW_Model.month + 1) - tOffset;
		ForEachSoilLayer(i)
		{
			p_Rwetdays_mo[SW_Output[eSW_WetDays].mo_row + mo_nrow * (i + 2)] = (int) v->moavg.wetdays[i];
		}
		SW_Output[eSW_WetDays].mo_row++;
		break;
	case eSW_Year:
		p_Rwetdays_yr[SW_Output[eSW_WetDays].yr_row + yr_nrow * 0] = SW_Model.year;
		ForEachSoilLayer(i)
		{
			p_Rwetdays_yr[SW_Output[eSW_WetDays].yr_row + yr_nrow * (i + 1)] = (int) v->yravg.wetdays[i];
		}
		SW_Output[eSW_WetDays].yr_row++;
		break;
	}

}

static void get_snowpack(void) {
	/* --------------------------------------------------- */
	SW_SOILWAT *v = &SW_Soilwat;
	OutPeriod pd = SW_Output[eSW_SnowPack].period;

	switch (pd) {
	case eSW_Day:
		p_Rsnowpack_dy[SW_Output[eSW_SnowPack].dy_row + dy_nrow * 0] = SW_Model.year;
		p_Rsnowpack_dy[SW_Output[eSW_SnowPack].dy_row + dy_nrow * 1] = SW_Model.doy;
		p_Rsnowpack_dy[SW_Output[eSW_SnowPack].dy_row + dy_nrow * 2] = v->dysum.snowpack;
		p_Rsnowpack_dy[SW_Output[eSW_SnowPack].dy_row + dy_nrow * 3] = v->dysum.snowdepth;
		SW_Output[eSW_SnowPack].dy_row++;
		break;
	case eSW_Week:
		p_Rsnowpack_wk[SW_Output[eSW_SnowPack].wk_row + wk_nrow * 0] = SW_Model.year;
		p_Rsnowpack_wk[SW_Output[eSW_SnowPack].wk_row + wk_nrow * 1] = (SW_Model.week + 1) - tOffset;
		p_Rsnowpack_wk[SW_Output[eSW_SnowPack].wk_row + wk_nrow * 2] = v->wkavg.snowpack;
		p_Rsnowpack_wk[SW_Output[eSW_SnowPack].wk_row + wk_nrow * 3] = v->wkavg.snowdepth;
		SW_Output[eSW_SnowPack].wk_row++;
		break;
	case eSW_Month:
		p_Rsnowpack_mo[SW_Output[eSW_SnowPack].mo_row + mo_nrow * 0] = SW_Model.year;
		p_Rsnowpack_mo[SW_Output[eSW_SnowPack].mo_row + mo_nrow * 1] = (SW_Model.month + 1) - tOffset;
		p_Rsnowpack_mo[SW_Output[eSW_SnowPack].mo_row + mo_nrow * 2] = v->moavg.snowpack;
		p_Rsnowpack_mo[SW_Output[eSW_SnowPack].mo_row + mo_nrow * 3] = v->moavg.snowdepth;
		SW_Output[eSW_SnowPack].mo_row++;
		break;
	case eSW_Year:
		p_Rsnowpack_yr[SW_Output[eSW_SnowPack].yr_row + yr_nrow * 0] = SW_Model.year;
		p_Rsnowpack_yr[SW_Output[eSW_SnowPack].yr_row + yr_nrow * 1] = v->yravg.snowpack;
		p_Rsnowpack_yr[SW_Output[eSW_SnowPack].yr_row + yr_nrow * 2] = v->yravg.snowdepth;
		SW_Output[eSW_SnowPack].yr_row++;
		break;
	}

}

static void get_deepswc(void) {
	/* --------------------------------------------------- */
	SW_SOILWAT *v = &SW_Soilwat;
	OutPeriod pd = SW_Output[eSW_DeepSWC].period;

	switch (pd) {
	case eSW_Day:
		p_Rdeep_drain_dy[SW_Output[eSW_DeepSWC].dy_row + dy_nrow * 0] = SW_Model.year;
		p_Rdeep_drain_dy[SW_Output[eSW_DeepSWC].dy_row + dy_nrow * 1] = SW_Model.doy;
		p_Rdeep_drain_dy[SW_Output[eSW_DeepSWC].dy_row + dy_nrow * 2] = v->dysum.deep;
		SW_Output[eSW_DeepSWC].dy_row++;
		break;
	case eSW_Week:
		p_Rdeep_drain_wk[SW_Output[eSW_DeepSWC].wk_row + wk_nrow * 0] = SW_Model.year;
		p_Rdeep_drain_wk[SW_Output[eSW_DeepSWC].wk_row + wk_nrow * 1] = (SW_Model.week + 1) - tOffset;
		p_Rdeep_drain_wk[SW_Output[eSW_DeepSWC].wk_row + wk_nrow * 2] = v->wkavg.deep;
		SW_Output[eSW_DeepSWC].wk_row++;
		break;
	case eSW_Month:
		p_Rdeep_drain_mo[SW_Output[eSW_DeepSWC].mo_row + mo_nrow * 0] = SW_Model.year;
		p_Rdeep_drain_mo[SW_Output[eSW_DeepSWC].mo_row + mo_nrow * 1] = (SW_Model.month + 1) - tOffset;
		p_Rdeep_drain_mo[SW_Output[eSW_DeepSWC].mo_row + mo_nrow * 2] = v->moavg.deep;
		SW_Output[eSW_DeepSWC].mo_row++;
		break;
	case eSW_Year:
		p_Rdeep_drain_yr[SW_Output[eSW_DeepSWC].yr_row + yr_nrow * 0] = SW_Model.year;
		p_Rdeep_drain_yr[SW_Output[eSW_DeepSWC].yr_row + yr_nrow * 1] = v->yravg.deep;
		SW_Output[eSW_DeepSWC].yr_row++;
		break;
	}
}

static void get_soiltemp(void) {
	/* --------------------------------------------------- */
	LyrIndex i;
	SW_SOILWAT *v = &SW_Soilwat;
	OutPeriod pd = SW_Output[eSW_SoilTemp].period;

	switch (pd) {
	case eSW_Day:
		p_Rsoil_temp_dy[SW_Output[eSW_SoilTemp].dy_row + dy_nrow * 0] = SW_Model.year;
		p_Rsoil_temp_dy[SW_Output[eSW_SoilTemp].dy_row + dy_nrow * 1] = SW_Model.doy;
		ForEachSoilLayer(i)
		{
			p_Rsoil_temp_dy[SW_Output[eSW_SoilTemp].dy_row + dy_nrow * (i + 2)] = v->dysum.sTemp[i];
		}
		SW_Output[eSW_SoilTemp].dy_row++;
		break;
	case eSW_Week:
		p_Rsoil_temp_wk[SW_Output[eSW_SoilTemp].wk_row + wk_nrow * 0] = SW_Model.year;
		p_Rsoil_temp_wk[SW_Output[eSW_SoilTemp].wk_row + wk_nrow * 1] = (SW_Model.week + 1) - tOffset;
		ForEachSoilLayer(i)
		{
			p_Rsoil_temp_wk[SW_Output[eSW_SoilTemp].wk_row + wk_nrow * (i + 2)] = v->wkavg.sTemp[i];
		}
		SW_Output[eSW_SoilTemp].wk_row++;
		break;
	case eSW_Month:
		p_Rsoil_temp_mo[SW_Output[eSW_SoilTemp].mo_row + mo_nrow * 0] = SW_Model.year;
		p_Rsoil_temp_mo[SW_Output[eSW_SoilTemp].mo_row + mo_nrow * 1] = (SW_Model.month + 1) - tOffset;
		ForEachSoilLayer(i)
		{
			p_Rsoil_temp_mo[SW_Output[eSW_SoilTemp].mo_row + mo_nrow * (i + 2)] = v->moavg.sTemp[i];
		}
		SW_Output[eSW_SoilTemp].mo_row++;
		break;
	case eSW_Year:
		p_Rsoil_temp_yr[SW_Output[eSW_SoilTemp].yr_row + yr_nrow * 0] = SW_Model.year;
		ForEachSoilLayer(i)
		{
			p_Rsoil_temp_yr[SW_Output[eSW_SoilTemp].yr_row + yr_nrow * (i + 1)] = v->yravg.sTemp[i];
		}
		SW_Output[eSW_SoilTemp].yr_row++;
		break;
	}
}

static void sumof_ves(SW_VEGESTAB *v, SW_VEGESTAB_OUTPUTS *s, OutKey k) {
	/* --------------------------------------------------- */
	/* k is always eSW_Estab, and this only gets called yearly */
	/* in fact, there's nothing to do here as the get_estab()
	 * function does everything needed.  This stub is here only
	 * to facilitate the loop everything else uses.
	 * That is, until we need to start outputting as-yet-unknown
	 * establishment variables.
	 */

// just a few lines of nonsense to supress the compile warnings, doesn't actually do anything
	if (&v == &v)
		if (&s == &s)
			if (k != 0)
				return;

}

static void sumof_wth(SW_WEATHER *v, SW_WEATHER_OUTPUTS *s, OutKey k) {
	/* --------------------------------------------------- */
	/* 	20091015 (drs) ppt is divided into rain and snow and all three values are output into precip */

	switch (k) {

	case eSW_Temp:
		s->temp_max += v->now.temp_max[Today];
		s->temp_min += v->now.temp_min[Today];
		s->temp_avg += v->now.temp_avg[Today];
		break;

	case eSW_Precip:
		s->ppt += v->now.ppt[Today];
		s->rain += v->now.rain[Today];
		s->snow += v->now.snow[Today];
		s->snowmelt += v->now.snowmelt[Today];
		s->snowloss += v->now.snowloss[Today];
		break;

	case eSW_SoilInf:
		s->soil_inf += v->soil_inf;
		break;

	case eSW_Runoff:
		s->snowRunoff += v->snowRunoff;
		s->surfaceRunoff += v->surfaceRunoff;
		break;

	default:
		LogError(stderr, LOGFATAL, "PGMR: Invalid key in sumof_wth(%s)", key2str[k]);
	}

}

static void sumof_swc(SW_SOILWAT *v, SW_SOILWAT_OUTPUTS *s, OutKey k) {
	/* --------------------------------------------------- */
	LyrIndex i;

	switch (k) {

	case eSW_SWC:
		ForEachSoilLayer(i)
			s->swc[i] += v->swc[Today][i];
		break;

	case eSW_SWCM:
		/* like with SWP, simply accumulate swc and cvt later */
		/* otherwise if you specify both swc and swcm it double counts */
		ForEachSoilLayer(i)
			s->swcm[i] += v->swc[Today][i];
		break;

	case eSW_SWP:
		/* can't avg swp so avg swc and convert later */
		ForEachSoilLayer(i)
			s->swp[i] += v->swc[Today][i];
		break;

	case eSW_SWA:
		ForEachSoilLayer(i)
			s->swa[i] += fmax (v->swc[Today][i] - SW_Site.lyr[i]->swc_wiltpt, 0.);
		break;

	case eSW_SurfaceWater:
		s->surfaceWater += v->surfaceWater;
		break;

	case eSW_Transp:
		ForEachSoilLayer(i)
		{
			s->transp_total[i] += v->transpiration_tree[i] + v->transpiration_shrub[i] + v->transpiration_grass[i];
			s->transp_tree[i] += v->transpiration_tree[i];
			s->transp_shrub[i] += v->transpiration_shrub[i];
			s->transp_grass[i] += v->transpiration_grass[i];
		}
		break;
	case eSW_EvapSoil:
		ForEachEvapLayer(i)
			s->evap[i] += v->evaporation[i];
		break;

	case eSW_EvapSurface:
		s->total_evap += v->tree_evap + v->shrub_evap + v->grass_evap + v->litter_evap + v->surfaceWater_evap;
		s->tree_evap += v->tree_evap;
		s->shrub_evap += v->shrub_evap;
		s->grass_evap += v->grass_evap;
		s->litter_evap += v->litter_evap;
		s->surfaceWater_evap += v->surfaceWater_evap;
		break;

	case eSW_Interception:
		s->total_int += v->tree_int + v->shrub_int + v->grass_int + v->litter_int;
		s->tree_int += v->tree_int;
		s->shrub_int += v->shrub_int;
		s->grass_int += v->grass_int;
		s->litter_int += v->litter_int;
		break;

	case eSW_LyrDrain:
		for (i = 0; i < SW_Site.n_layers - 1; i++)
			s->lyrdrain[i] += v->drain[i];
		break;

	case eSW_HydRed:
		ForEachSoilLayer(i)
		{
			s->hydred_total[i] += v->hydred_tree[i] + v->hydred_shrub[i] + v->hydred_grass[i];
			s->hydred_tree[i] += v->hydred_tree[i];
			s->hydred_shrub[i] += v->hydred_shrub[i];
			s->hydred_grass[i] += v->hydred_grass[i];
		}
		break;

	case eSW_AET:
		s->aet += v->aet;
		break;

	case eSW_PET:
		s->pet += v->pet;
		break;

	case eSW_WetDays:
		ForEachSoilLayer(i)
			if (v->is_wet[i])
				s->wetdays[i]++;
		break;

	case eSW_SnowPack:
		s->snowpack += v->snowpack[Today];
		s->snowdepth += v->snowdepth;
		break;

	case eSW_DeepSWC:
		s->deep += v->swc[Today][SW_Site.deep_lyr];
		break;

	case eSW_SoilTemp:
		ForEachSoilLayer(i)
		{
			s->sTemp[i] += v->sTemp[i];
		}
		break;

	default:
		LogError(stderr, LOGFATAL, "PGMR: Invalid key in sumof_swc(%s)", key2str[k]);
	}

}
static void average_for(ObjType otyp, OutPeriod pd) {
	/* --------------------------------------------------- */
	/* separates the task of obtaining a periodic average.
	 * no need to average days, so this should never be
	 * called with eSW_Day.
	 * Enter this routine just after the summary period
	 * is completed, so the current week and month will be
	 * one greater than the period being summarized.
	 */
	/* 	20091015 (drs) ppt is divided into rain and snow and all three values are output into precip */
	SW_SOILWAT_OUTPUTS *savg = NULL, *ssumof = NULL;
	SW_WEATHER_OUTPUTS *wavg = NULL, *wsumof = NULL;
	TimeInt curr_pd = 0;
	RealD div = 0.; /* if sumtype=AVG, days in period; if sumtype=SUM, 1 */
	OutKey k;
	LyrIndex i;
	int j;

	if (!(otyp == eSWC || otyp == eWTH))
		LogError(stdout, LOGFATAL, "Invalid object type in OUT_averagefor().");

	ForEachOutKey(k)
	{
		for (j = 0; j < numPeriod; j++) { /* loop through this code for as many periods that are being used */
			if (!SW_Output[k].use)
				continue;
			SW_Output[k].period = timeSteps[j]; /* set the period to use based on the iteration of the for loop */
			switch (pd) {
			case eSW_Week:
				curr_pd = (SW_Model.week + 1) - tOffset;
				savg = (SW_SOILWAT_OUTPUTS *) &SW_Soilwat.wkavg;
				ssumof = (SW_SOILWAT_OUTPUTS *) &SW_Soilwat.wksum;
				wavg = (SW_WEATHER_OUTPUTS *) &SW_Weather.wkavg;
				wsumof = (SW_WEATHER_OUTPUTS *) &SW_Weather.wksum;
				div = (bFlush) ? SW_Model.lastdoy % WKDAYS : WKDAYS;
				break;

			case eSW_Month:
				curr_pd = (SW_Model.month + 1) - tOffset;
				savg = (SW_SOILWAT_OUTPUTS *) &SW_Soilwat.moavg;
				ssumof = (SW_SOILWAT_OUTPUTS *) &SW_Soilwat.mosum;
				wavg = (SW_WEATHER_OUTPUTS *) &SW_Weather.moavg;
				wsumof = (SW_WEATHER_OUTPUTS *) &SW_Weather.mosum;
				div = Time_days_in_month(SW_Model.month - tOffset);
				break;

			case eSW_Year:
				curr_pd = SW_Output[k].first;
				savg = (SW_SOILWAT_OUTPUTS *) &SW_Soilwat.yravg;
				ssumof = (SW_SOILWAT_OUTPUTS *) &SW_Soilwat.yrsum;
				wavg = (SW_WEATHER_OUTPUTS *) &SW_Weather.yravg;
				wsumof = (SW_WEATHER_OUTPUTS *) &SW_Weather.yrsum;
				div = SW_Output[k].last - SW_Output[k].first + 1;
				break;

			default:
				LogError(stdout, LOGFATAL, "Programmer: Invalid period in average_for().");
			} /* end switch(pd) */

			if (SW_Output[k].period != pd || SW_Output[k].myobj != otyp || curr_pd < SW_Output[k].first || curr_pd > SW_Output[k].last)
				continue;

			if (SW_Output[k].sumtype == eSW_Sum)
				div = 1.;

			/* notice that all valid keys are in this switch */
			switch (k) {

			case eSW_Temp:
				wavg->temp_max = wsumof->temp_max / div;
				wavg->temp_min = wsumof->temp_min / div;
				wavg->temp_avg = wsumof->temp_avg / div;
				break;

			case eSW_Precip:
				wavg->ppt = wsumof->ppt / div;
				wavg->rain = wsumof->rain / div;
				wavg->snow = wsumof->snow / div;
				wavg->snowmelt = wsumof->snowmelt / div;
				wavg->snowloss = wsumof->snowloss / div;
				break;

			case eSW_SoilInf:
				wavg->soil_inf = wsumof->soil_inf / div;
				break;

			case eSW_Runoff:
				wavg->snowRunoff = wsumof->snowRunoff / div;
				wavg->surfaceRunoff = wsumof->surfaceRunoff / div;
				break;

			case eSW_SoilTemp:
				ForEachSoilLayer(i)
					savg->sTemp[i] = (SW_Output[k].sumtype == eSW_Fnl) ? SW_Soilwat.sTemp[i] : ssumof->sTemp[i] / div;
				break;

			case eSW_SWC:
				ForEachSoilLayer(i)
					savg->swc[i] = (SW_Output[k].sumtype == eSW_Fnl) ? SW_Soilwat.swc[Yesterday][i] : ssumof->swc[i] / div;
				break;

			case eSW_SWCM:
				/* like with SWP, I think it's better to simply
				 * accumulate swc and convert at the get_() function
				 */
				ForEachSoilLayer(i)
					savg->swcm[i] = (SW_Output[k].sumtype == eSW_Fnl) ? SW_Soilwat.swc[Yesterday][i] : ssumof->swc[i] / div;
				break;

			case eSW_SWP:
				/* this one's weird because of the problem averaging
				 * the exponential results, etc.  Note we're summing
				 * or averaging the swc.  We don't convert to swp
				 * until writing.
				 */
				ForEachSoilLayer(i)
					savg->swp[i] = (SW_Output[k].sumtype == eSW_Fnl) ? SW_Soilwat.swc[Yesterday][i] : ssumof->swp[i] / div;
				break;

			case eSW_SWA:
				ForEachSoilLayer(i)
					savg->swa[i] = (SW_Output[k].sumtype == eSW_Fnl) ? fmax (SW_Soilwat.swc[Yesterday][i] - SW_Site.lyr[i]->swc_wiltpt, 0.) : ssumof->swa[i] / div;
				break;

			case eSW_DeepSWC:
				savg->deep = (SW_Output[k].sumtype == eSW_Fnl) ? SW_Soilwat.swc[Yesterday][SW_Site.deep_lyr] : ssumof->deep / div;
				break;

			case eSW_SurfaceWater:
				savg->surfaceWater = ssumof->surfaceWater / div;
				break;

			case eSW_Transp:
				ForEachSoilLayer(i)
				{
					savg->transp_total[i] = ssumof->transp_total[i] / div;
					savg->transp_tree[i] = ssumof->transp_tree[i] / div;
					savg->transp_shrub[i] = ssumof->transp_shrub[i] / div;
					savg->transp_grass[i] = ssumof->transp_grass[i] / div;
				}
				break;

			case eSW_EvapSoil:
				ForEachEvapLayer(i)
					savg->evap[i] = ssumof->evap[i] / div;
				break;

			case eSW_EvapSurface:
				savg->total_evap = ssumof->total_evap / div;
				savg->tree_evap = ssumof->tree_evap / div;
				savg->shrub_evap = ssumof->shrub_evap / div;
				savg->grass_evap = ssumof->grass_evap / div;
				savg->litter_evap = ssumof->litter_evap / div;
				savg->surfaceWater_evap = ssumof->surfaceWater_evap / div;
				break;

			case eSW_Interception:
				savg->total_int = ssumof->total_int / div;
				savg->tree_int = ssumof->tree_int / div;
				savg->shrub_int = ssumof->shrub_int / div;
				savg->grass_int = ssumof->grass_int / div;
				savg->litter_int = ssumof->litter_int / div;
				break;

			case eSW_AET:
				savg->aet = ssumof->aet / div;
				break;

			case eSW_LyrDrain:
				for (i = 0; i < SW_Site.n_layers - 1; i++)
					savg->lyrdrain[i] = ssumof->lyrdrain[i] / div;
				break;

			case eSW_HydRed:
				ForEachSoilLayer(i)
				{
					savg->hydred_total[i] = ssumof->hydred_total[i] / div;
					savg->hydred_tree[i] = ssumof->hydred_tree[i] / div;
					savg->hydred_shrub[i] = ssumof->hydred_shrub[i] / div;
					savg->hydred_grass[i] = ssumof->hydred_grass[i] / div;
				}
				break;

			case eSW_PET:
				savg->pet = ssumof->pet / div;
				break;

			case eSW_WetDays:
				ForEachSoilLayer(i)
					savg->wetdays[i] = ssumof->wetdays[i] / div;
				break;

			case eSW_SnowPack:
				savg->snowpack = ssumof->snowpack / div;
				savg->snowdepth = ssumof->snowdepth / div;
				break;

			case eSW_Estab: /* do nothing, no averaging required */
				break;

			default:
				LogError(stderr, LOGFATAL, "PGMR: Invalid key in average_for(%s)", key2str[k]);
			}
		} /* end of for loop */
	} /* end ForEachKey */
}

static void collect_sums(ObjType otyp, OutPeriod op) {
	/* --------------------------------------------------- */

	SW_SOILWAT *s = &SW_Soilwat;
	SW_SOILWAT_OUTPUTS *ssum = NULL;
	SW_WEATHER *w = &SW_Weather;
	SW_WEATHER_OUTPUTS *wsum = NULL;
	SW_VEGESTAB *v = &SW_VegEstab; /* vegestab only gets summed yearly */
	SW_VEGESTAB_OUTPUTS *vsum = NULL;

	TimeInt pd = 0;
	OutKey k;

	ForEachOutKey(k)
	{
		if (otyp != SW_Output[k].myobj || !SW_Output[k].use)
			continue;
		switch (op) {
		case eSW_Day:
			pd = SW_Model.doy;
			ssum = &s->dysum;
			wsum = &w->dysum;
			break;
		case eSW_Week:
			pd = SW_Model.week + 1;
			ssum = &s->wksum;
			wsum = &w->wksum;
			break;
		case eSW_Month:
			pd = SW_Model.month + 1;
			ssum = &s->mosum;
			wsum = &w->mosum;
			break;
		case eSW_Year:
			pd = SW_Model.doy;
			ssum = &s->yrsum;
			wsum = &w->yrsum;
			vsum = &v->yrsum; /* yearly, y'see */
			break;
		default:
			LogError(logfp, LOGFATAL, "PGMR: Invalid outperiod in collect_sums()");
		}

		if (pd >= SW_Output[k].first && pd <= SW_Output[k].last) {
			switch (otyp) {
			case eSWC:
				sumof_swc(s, ssum, k);
				break;
			case eWTH:
				sumof_wth(w, wsum, k);
				break;
			case eVES:
				sumof_ves(v, vsum, k);
				break;
			default:
				break;
			}
		}

	} /* end ForEachOutKey */
}

static void _echo_outputs(void) {
	/* --------------------------------------------------- */

	OutKey k;
	strcpy(errstr, "\n===============================================\n"
			"  Output Configuration:\n");
	ForEachOutKey(k)
	{
		if (!SW_Output[k].use)
			continue;
		strcat(errstr, "---------------------------\nKey ");
		strcat(errstr, key2str[k]);
		strcat(errstr, "\n\tSummary Type: ");
		strcat(errstr, styp2str[SW_Output[k].sumtype]);
		strcat(errstr, "\n\tOutput Period: ");
		strcat(errstr, pd2str[SW_Output[k].period]);
		sprintf(outstr, "\n\tStart period: %d", SW_Output[k].first_orig);
		strcat(errstr, outstr);
		sprintf(outstr, "\n\tEnd period  : %d", SW_Output[k].last_orig);
		strcat(errstr, outstr);
		strcat(errstr, "\n\tOutput File: NA");
		//strcat(errstr, SW_Output[k].outfile);
		strcat(errstr, "\n");
	}

	strcat(errstr, "\n----------  End of Output Configuration ---------- \n");
	LogError(logfp, LOGNOTE, errstr);

}

#ifdef DEBUG_MEM
#include "myMemory.h"
/*======================================================*/
void SW_OUT_SetMemoryRefs( void) {
	/* when debugging memory problems, use the bookkeeping
	 code in myMemory.c
	 This routine sets the known memory refs in this module
	 so they can be  checked for leaks, etc.  Includes
	 malloc-ed memory in SOILWAT.  All refs will have been
	 cleared by a call to ClearMemoryRefs() before this, and
	 will be checked via CheckMemoryRefs() after this, most
	 likely in the main() function.
	 */
	OutKey k;

	ForEachOutKey(k) {
		if (SW_Output[k].use)
		NoteMemoryRef(SW_Output[k].outfile);
	}

}

#endif

/*==================================================================

 Description of the algorithm.

 There is a structure array (SW_OUTPUT) that contains the
 information from the outsetup.in file. This structure is filled in
 the initialization process by matching defined macros of valid keys
 with enumeration variables used as indices into the structure
 array.  A similar combination of text macros and enumeration
 constants handles the TIMEPERIOD conversion from text to numeric
 index.

 Each structure element of the array contains the output period
 code, start and end values, output file name, opened file pointer
 for output, on/off status, and a pointer to the function that
 prepares a complete line of formatted output per output period.

 A _construct() function clears the entire structure array to set
 values and flags to zero, and then assigns each specific print
 function name to the associated element's print function pointer.
 This allows the print function to be called via a simple loop that
 runs through all of the output keys.  Those output objects that are
 turned off are ignored and the print function is not called.  Thus,
 to add a new output variable, a new print function must be added to
 the loop in addition to adding the new macro and enumeration keys
 for it.  Oh, and a line or two of summarizing code.

 After initialization, each valid output key has an element in the
 structure array that "knows" its parameters and whether it is on or
 off.  There is still space allocated for the "off" keys but they
 are ignored by the use flag.

 During the daily execution loop of the model, values for each of
 the output objects are accumulated via a call to
 SW_OUT_sum_today(x) function with x being a special enumeration
 code that defines the actual module object to be summed (see
 SW_Output.h).  This enumeration code breaks up the many output
 variables into a few simple types so that adding a new output
 variable is simplified by putting it into its proper category.

 When the _sum_today() function is called, it calls the averaging
 function which puts the sum, average, etc into the output
 accumulators--(dy|wk|mo|yr)avg--then conditionally clears the
 summary accumulators--(dy|wk|mo|yr)sum--if a new period has
 occurred (in preparation for the new period), then calls the
 function to handle collecting the summaries called collect_sums().

 The collect_sums() function needs the object type (eg, eSWC, eWTH)
 and the output period (eg, dy, wk, etc) and then, for each valid
 output key, it assigns a pointer to the appropriate object's
 summary sub-structure.  (This is where the complexity of this
 approach starts to become a bit clumsy, but it nonetheless tends to
 keep the overall code size down.) After assigning the pointer to
 the summary structure, the pointers are passed to a routine to
 actually do the accumulation for the various output objects
 (currently SWC and WTH).  No other arithmetic is performed here.
 This routine is only called, however, if the current day or period
 falls within the range specified by the user.  Otherwise, the
 accumulators will remain zero.  Also, the period check is used in
 other places to determine whether to bother with averaging and
 printing.

 Once a period other than daily has passed, the accumulated values
 are averaged or summed as appropriate within the average_for()
 subroutine as mentioned above.

 After the averaging function, the values are ready to format for
 output.  The SW_OUT_write_today() routine is called from the
 end_day() function in main().  Any quantities that have finished
 their period by the current day are written out.  This requires
 testing of all of the output quantities periods each day but makes
 the code quite simple.  This is a reasonable tradeoff since there
 are only a few quantities to test; this should outweight the costs
 of having to read and understand ugly code.

 So to summarize, adding another output quantity requires several steps.
 - Add an appropriate element to the SW_*_OUTPUTS substructure of the
 main object (eg SW_Soilwat) to hold the output value.
 - Define a new key string and add a macro definition and enumeration
 to the appropriate list in Output.h.  Be sure the new key's position
 in the list doesn't interfere with the ForEach*() loops.
 - Increase the value of SW_OUTNKEYS macro in Output.h.
 - Add the macro and enum keys to the key2str and key2obj lists in
 SW_Output.c as appropriate, IN THE SAME LIST POSITION.
 - Create and declare a get_*() function that returns the correctly
 formatted string for output.
 - Add a line to link the get_ function to the appropriate element in
 the SW_OUTPUT array in _construct().
 - Add new code to the switch statement in sumof_*() to handle the new
 key.
 - Add new code to the switch statement in average_for() to do the
 summarizing.

 That should do it.  However, new code is about to be added to Output.c
 and outsetup.in that will allow quantities to be summarized by summing
 or averaging.  Possibly in the future, more types of options will be
 added (eg, geometric average, stddev, who knows).  Thus, new keys will
 be needed to handle those operations within the average_for()
 function, but the rest of the code will be the same.

 */
