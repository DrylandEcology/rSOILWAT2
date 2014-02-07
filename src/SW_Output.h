/********************************************************/
/********************************************************/
/*  Source file: SW_Output.h
	Type: header
	Application: SOILWAT - soilwater dynamics simulator
	Purpose: Support for Output.c
	History:
	(9/11/01) -- INITIAL CODING - cwb
	2010/02/02	(drs) changed SW_CANOPY to SW_CANOPYEV and SW_LITTER to SW_LITTEREV
			and eSW_Canopy to eSW_CanopyEv and eSW_Litter to eSW_LitterEv;
			added SWC_CANOPYINT, SW_LITTERINT, SW_SOILINF, SW_LYRDRAIN;
			updated SW_OUTNKEYS from 19 to 23;
			added eSW_CanopyInt, eSW_LitterInt, eSW_SoilInf, eSW_LyrDrain to enum OutKey;
	04/16/2010	(drs) added SW_SWA, updated SW_OUTNKEYS to 24, added eSW_SWA to enum OutKey
	10/20/2010	(drs) added SW_HYDRED, updated SW_OUTNKEYS to 25, added eSW_HydRed to enum OutKey
	02/19/2011	(drs) moved soil_inf output from swc- to weather-output: updated enum Outkey
	07/22/2011	(drs) added SW_SURFACEW and SW_EVAPSURFACE, updated SW_OUTNKEYS to 27, added eSW_SurfaceWater and eSW_EvapSurface to enum
	09/12/2011	(drs)	renamed SW_EVAP to SW_EVAPSOIL, and eSW_Evap to eSW_EvapSoil;
			deleted SW_CANOPYEV, eSW_CanopyEv, SW_LITTEREV, eSW_LitterEv, SW_CANOPYINT, eSW_CanopyInt, SW_LITTERINT, and eSW_LitterInt;
			added SW_INTERCEPTION and eSW_Interception
	05/25/2012	(DLM) added SW_SOILTEMP, updated SW_OUTNKEYs to 28, added eSW_SoilTemp to enum
	12/13/2012	(clk) added SW_RUNOFF, updated SW_OUTNKEYs to 29, added eSW_Runoff to enum
	12/14/2012	(drs) updated SW_OUTNKEYs from 29 to 26 [incorrect number probably introduced 09/12/2011]
	01/10/2013	(clk)	instead of using one FILE pointer named fp, created four new
			FILE pointers; fp_dy, fp_wk, fp_mo, and fp_yr. This allows us to keep track
			of all time steps for each OutKey.
*/
/********************************************************/
/********************************************************/

#ifndef SW_OUTPUT_H
#define SW_OUTPUT_H

#ifdef RSOILWAT
#include <R.h>
#include <Rdefines.h>
#include <Rconfig.h>
#include <Rinterface.h>
#include <Rinternals.h>
#endif

/* These are the keywords to be found in the output setup file */
/* some of them are from the old fortran model and are no longer */
/* implemented, but are retained for some tiny measure of backward */
/* compatibility */
						//KEY			INDEX	OBJT	SUMTYPE
#define SW_WETHR		"WTHR"			//0		2		0/* position and variable marker, not an output key */
#define SW_TEMP			"TEMP"			//1		2		2
#define SW_PRECIP		"PRECIP"		//2		2		1
#define SW_SOILINF		"SOILINFILT"	//3		2		1
#define SW_RUNOFF		"RUNOFF"		//4		2		1
#define SW_ALLH2O   	"ALLH2O"		//5		4		0/* position and variable marker, not an output key */
#define SW_VWCBULK  	"VWCBULK"		//6		4		2
#define SW_VWCMATRIC	"VWCMATRIC"		//7		4		2
#define SW_SWCBULK    	"SWCBULK"		//8		4		2
#define SW_SWABULK	 	"SWABULK"		//9		4		2
#define SW_SWAMATRIC	"SWAMATRIC"		//10	4		2
#define SW_SWPMATRIC    "SWPMATRIC"		//11	4		2
#define SW_SURFACEW		"SURFACEWATER"	//12	4		2
#define SW_TRANSP		"TRANSP"		//13	4		1
#define SW_EVAPSOIL		"EVAPSOIL"		//14	4		1
#define SW_EVAPSURFACE	"EVAPSURFACE"	//15	4		1
#define SW_INTERCEPTION	"INTERCEPTION"	//16	4		1
#define SW_LYRDRAIN		"LYRDRAIN"		//17	4		1
#define SW_HYDRED		"HYDRED"		//18	4		1
#define SW_ET			"ET"			//19	4		0/* position and variable marker, not an output key */
#define SW_AET			"AET"			//20	4		1
#define SW_PET			"PET"			//21	4		1
#define SW_WETDAY		"WETDAY"		//22	4		1
#define SW_SNOWPACK		"SNOWPACK"		//23	4		2
#define SW_DEEPSWC		"DEEPSWC"		//24	4		1
#define SW_SOILTEMP		"SOILTEMP"		//25	4		2
#define SW_ALLVEG		"ALLVEG"		//26	5		0/* position and variable marker, not an output key */
#define SW_ESTAB		"ESTABL"		//27	5		0

#define SW_OUTNKEYS 28 /* must also match number of items in enum (minus eSW_NoKey and eSW_LastKey) */

/* these are the code analog of the above */
/* see also key2str[] in Output.c */
/* take note of boundary conditions in ForEach...() loops below */
typedef enum {
	eSW_NoKey = -1,
	/* weather/atmospheric quantities */
	eSW_AllWthr, /* includes all weather vars */
	eSW_Temp,
	eSW_Precip,
	eSW_SoilInf,
	eSW_Runoff,
	/* soil related water quantities */
	eSW_AllH2O,
	eSW_VWCBulk,
	eSW_VWCMatric,
	eSW_SWCBulk,
	eSW_SWABulk,
	eSW_SWAMatric,
	eSW_SWPMatric,
	eSW_SurfaceWater,
	eSW_Transp,
	eSW_EvapSoil,
	eSW_EvapSurface,
	eSW_Interception,
	eSW_LyrDrain,
	eSW_HydRed,
	eSW_ET,
	eSW_AET,
	eSW_PET, /* really belongs in wth, but for historical reasons we'll keep it here */
	eSW_WetDays,
	eSW_SnowPack,
	eSW_DeepSWC,
	eSW_SoilTemp,
	/* vegetation quantities */
	eSW_AllVeg,
	eSW_Estab, /* make sure this is the last one */
	eSW_LastKey
} OutKey;

/* output period specifiers found in input file */
#define SW_DAY   "DY"
#define SW_WEEK  "WK"
#define SW_MONTH "MO"
#define SW_YEAR  "YR"
#define SW_OUTNPERIODS 4  /* must match with enum */

typedef enum {
	eSW_Day, eSW_Week, eSW_Month, eSW_Year
} OutPeriod;

/* summary methods */
#define SW_SUM_OFF "OFF"  /* don't output */
#define SW_SUM_SUM "SUM"  /* sum for period */
#define SW_SUM_AVG "AVG"  /* arith. avg for period */
#define SW_SUM_FNL "FIN"  /* value on last day in period */
#define SW_NSUMTYPES 4

typedef enum {
	eSW_Off, eSW_Sum, eSW_Avg, eSW_Fnl
} OutSum;

typedef struct {
	OutKey mykey;
	ObjType myobj;
	OutPeriod period;
	OutSum sumtype;
	Bool use;
	TimeInt first, last, 			/* updated for each year */
			first_orig, last_orig;
#ifdef RSOILWAT
	int yr_row, mo_row, wk_row, dy_row;
#endif
	char *outfile; /* point to name of output file */
	FILE *fp_dy; /* opened output file pointer for day*/
	FILE *fp_wk; /* opened output file pointer for week*/
	FILE *fp_mo; /* opened output file pointer for month*/
	FILE *fp_yr; /* opened output file pointer for year*/
	void (*pfunc)(void); /* pointer to output routine */
} SW_OUTPUT;

/* convenience loops for consistency.
 * k must be a defined variable, either of OutKey type
 * or int (IntU is better).
 */
#define ForEachOutKey(k)     for((k)=eSW_NoKey+1; (k)<eSW_LastKey;   (k)++)
#define ForEachSWC_OutKey(k) for((k)=eSW_AllH2O;  (k)<=eSW_SnowPack; (k)++)
#define ForEachWTH_OutKey(k) for((k)=eSW_AllWthr; (k)<=eSW_Precip;   (k)++)
#define ForEachVES_OutKey(k) for((k)=eSW_AllVeg;  (k)<=eSW_Estab;    (k)++)
#define ForEachOutPeriod(k)  for((k)=eSW_Day;     (k)<=eSW_Year;     (k)++)

void SW_OUT_construct(void);
void SW_OUT_new_year(void);
void SW_OUT_read(void);
void SW_OUT_sum_today(ObjType otyp);
void SW_OUT_write_today(void);
void SW_OUT_write_year(void);
void SW_OUT_close_files(void);
void SW_OUT_flush(void);

#ifdef RSOILWAT
SEXP onGet_SW_OUT(void);
void onSet_SW_OUT(SEXP OUT);
#endif

#ifdef DEBUG_MEM
void SW_OUT_SetMemoryRefs(void);
#endif

#endif
