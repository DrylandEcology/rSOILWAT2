/*
 * SW_R_lib.c
 *
 *  Created on: Jun 25, 2013
 *      Author: Ryan Murphy
 */
#define RSOILWAT
#ifdef RSOILWAT

#include "SW_R_lib.h"
#include "SW_Files.h"

/* =================================================== */
/*                  Global Declarations                */
/* external by other routines elsewhere in the program */
/* --------------------------------------------------- */

int logNote = 1;
int logWarn = 1;
int logFatl = 1;
int RlogIndex;
SEXP Rlogfile;
SEXP InputData;
SEXP WeatherList;
Bool useFiles;
Bool collectInData;
Bool bWeatherList;

int *p_yr, *p_mo, *p_wk, *p_dy;
RealD *p_Raet_yr, *p_Rdeep_drain_yr, *p_Restabs_yr, *p_Revap_soil_yr, *p_Revap_surface_yr, *p_Rhydred_yr, *p_Rinfiltration_yr, *p_Rinterception_yr, *p_Rpercolation_yr,
		*p_Rpet_yr, *p_Rprecip_yr, *p_Rrunoff_yr, *p_Rsnowpack_yr, *p_Rsoil_temp_yr, *p_Rsurface_water_yr, *p_RvwcBulk_yr, *p_RvwcMatric_yr, *p_RswcBulk_yr, *p_RswpMatric_yr,
		*p_RswaBulk_yr, *p_RswaMatric_yr, *p_Rtemp_yr, *p_Rtransp_yr, *p_Rwetdays_yr;
RealD *p_Raet_mo, *p_Rdeep_drain_mo, *p_Restabs_mo, *p_Revap_soil_mo, *p_Revap_surface_mo, *p_Rhydred_mo, *p_Rinfiltration_mo, *p_Rinterception_mo, *p_Rpercolation_mo,
		*p_Rpet_mo, *p_Rprecip_mo, *p_Rrunoff_mo, *p_Rsnowpack_mo, *p_Rsoil_temp_mo, *p_Rsurface_water_mo, *p_RvwcBulk_mo, *p_RvwcMatric_mo, *p_RswcBulk_mo, *p_RswpMatric_mo,
		*p_RswaBulk_mo, *p_RswaMatric_mo, *p_Rtemp_mo, *p_Rtransp_mo, *p_Rwetdays_mo;
RealD *p_Raet_wk, *p_Rdeep_drain_wk, *p_Restabs_wk, *p_Revap_soil_wk, *p_Revap_surface_wk, *p_Rhydred_wk, *p_Rinfiltration_wk, *p_Rinterception_wk, *p_Rpercolation_wk,
		*p_Rpet_wk, *p_Rprecip_wk, *p_Rrunoff_wk, *p_Rsnowpack_wk, *p_Rsoil_temp_wk, *p_Rsurface_water_wk, *p_RvwcBulk_wk, *p_RvwcMatric_wk, *p_RswcBulk_wk, *p_RswpMatric_wk,
		*p_RswaBulk_wk, *p_RswaMatric_wk, *p_Rtemp_wk, *p_Rtransp_wk, *p_Rwetdays_wk;
RealD *p_Raet_dy, *p_Rdeep_drain_dy, *p_Restabs_dy, *p_Revap_soil_dy, *p_Revap_surface_dy, *p_Rhydred_dy, *p_Rinfiltration_dy, *p_Rinterception_dy, *p_Rpercolation_dy,
		*p_Rpet_dy, *p_Rprecip_dy, *p_Rrunoff_dy, *p_Rsnowpack_dy, *p_Rsoil_temp_dy, *p_Rsurface_water_dy, *p_RvwcBulk_dy, *p_RvwcMatric_dy, *p_RswcBulk_dy, *p_RswpMatric_dy,
		*p_RswaBulk_dy, *p_RswaMatric_dy, *p_Rtemp_dy, *p_Rtransp_dy, *p_Rwetdays_dy;
unsigned int yr_nrow = 0, mo_nrow = 0, wk_nrow = 0, dy_nrow = 0;

extern char _firstfile[1024];
extern int timeSteps[4];
extern int numPeriod;

/* =================================================== */
/*                Module-Level Declarations            */
/* --------------------------------------------------- */

void SW_FLW_construct(void);

SEXP onGetInputDataFromFiles(SEXP inputOptions) {
	int i;
	SEXP swInputData;
	SEXP SW_DataList;
	SEXP swLog;
	SEXP oRlogfile;
	char *ListNames[] = {"files.in", "years.in", "weathersetup.in", "prod.in", "site.in","estab.in","outsetup.in","swcsetup.in","LogFile"};

	logged = FALSE;
	logfp = stdout;
	int argc = length(inputOptions);
	char *argv[7];
	collectInData = TRUE;
	PROTECT(inputOptions = AS_CHARACTER(inputOptions));
	for (i = 0; i < argc; i++) {
		argv[i] = R_alloc(strlen(CHAR(STRING_ELT(inputOptions, i))), sizeof(char));
	}
	for (i = 0; i < argc; i++) {
		strcpy(argv[i], CHAR(STRING_ELT(inputOptions, i)));
	}
	//Rprintf("set Args\n");
	PROTECT(swLog = MAKE_CLASS("swLog"));
	PROTECT(oRlogfile = NEW_OBJECT(swLog));
	PROTECT(Rlogfile = GET_SLOT(oRlogfile,install("LogData")));
	//Rprintf("swLog\n");
	init_args(argc, argv);
	SW_F_construct(_firstfile);
	SW_MDL_construct();
	SW_WTH_construct();
	SW_SIT_construct();
	SW_VES_construct();
	SW_VPD_construct();
	SW_OUT_construct();
	SW_SWC_construct();
	SW_FLW_construct();
	//Rprintf("Construct\n");
	SW_F_read(NULL);
	//Rprintf("FilesRead\n");
	SW_MDL_read();
	//Rprintf("mdlRead\n");
	SW_WTH_read();
	//Rprintf("wthRead\n");
	SW_VPD_read();
	//Rprintf("vpdRead\n");
	SW_SIT_read();
	//Rprintf("sitRead\n");
	SW_VES_read();
	//Rprintf("vesRead\n");
	SW_OUT_read();
	//Rprintf("outRead\n");
	SW_SWC_read();
	//Rprintf("Read\n");
	PROTECT(swInputData = MAKE_CLASS("swInputData"));
	PROTECT(SW_DataList = NEW_OBJECT(swInputData));
	SET_SLOT(SW_DataList,install("files"),onGet_SW_F());
	//Rprintf("swFiles\n");
	SET_SLOT(SW_DataList,install("years"),onGet_SW_MDL());
	//Rprintf("swYears\n");
	SET_SLOT(SW_DataList,install("weather"),onGet_SW_WTH());
	//Rprintf("swWeather\n");
	SET_SLOT(SW_DataList, install("cloud"), onGet_SW_SKY());
	//Rprintf("swSky\n");
	SET_SLOT(SW_DataList, install("weatherHistory"), onGet_WTH_DATA());
	//Rprintf("swWeatherHistory\n");
	if (LOGICAL(GET_SLOT(GET_SLOT(SW_DataList,install("weather")),install("use_Markov")))[0]) {
		SET_SLOT(SW_DataList, install("markov"), onGet_MKV());
		//Rprintf("swMarkov\n");
	}
	SET_SLOT(SW_DataList,install("prod"),onGet_SW_VPD());
	//Rprintf("swProd\n");
	SET_SLOT(SW_DataList,install("site"),onGet_SW_SIT());
	//Rprintf("swSite\n");
	SET_SLOT(SW_DataList,install("soils"),onGet_SW_LYR());
	//Rprintf("swSoils\n");
	SET_SLOT(SW_DataList,install("estab"),onGet_SW_VES());
	//Rprintf("swEstab\n");
	SET_SLOT(SW_DataList,install("output"),onGet_SW_OUT());
	//Rprintf("swOUT\n");
	SET_SLOT(SW_DataList,install("swc"),onGet_SW_SWC());
	//Rprintf("swSWC\n");
	SET_SLOT(SW_DataList,install("log"),oRlogfile);

	SW_SIT_clear_layers();
	SW_WTH_clear_runavg_list();
	SW_VES_clear();

	UNPROTECT(6);
	return SW_DataList;
}

SEXP start(SEXP inputOptions, SEXP inputData, SEXP weatherList, SEXP outputData) {
	int tYears = 0, tevapLayers = 0, tVegEstabCount = 0, pYearUse = 0, pMonthUse = 0, pWeekUse = 0, pDayUse = 0;
	int i;

	//Main Output
	SEXP swLog;
	SEXP oRlogfile;

	logged = FALSE;
	logfp = stdout;
	int argc = length(inputOptions);
	char *argv[7];
	collectInData = FALSE;
	if(isNull(inputData))
		useFiles = TRUE;
	else {
		useFiles = FALSE;
		InputData = inputData;
	}
	//This is used to minimize copying weather data between similiar runs.
	if(isNull(weatherList)) {
		bWeatherList = FALSE;
	} else {
		bWeatherList = TRUE;
		WeatherList = weatherList;
	}

	PROTECT(inputOptions = AS_CHARACTER(inputOptions));
	for (i = 0; i < argc; i++) {
		argv[i] = R_alloc(strlen(CHAR(STRING_ELT(inputOptions, i))), sizeof(char));
	}
	for (i = 0; i < argc; i++) {
		strcpy(argv[i], CHAR(STRING_ELT(inputOptions, i)));
	}
	RlogIndex = 0;
	// logfile
	PROTECT(swLog = MAKE_CLASS("swLog"));
	PROTECT(oRlogfile = NEW_OBJECT(swLog));
	PROTECT(Rlogfile = GET_SLOT(oRlogfile,install("LogData")));

	//Set the input data either from files or from memory
	init_args(argc, argv);
	SW_CTL_init_model(_firstfile);

	yr_nrow = INTEGER(GET_SLOT(outputData, install("yr_nrow")))[0];
	mo_nrow = INTEGER(GET_SLOT(outputData, install("mo_nrow")))[0];
	wk_nrow = INTEGER(GET_SLOT(outputData, install("wk_nrow")))[0];
	dy_nrow = INTEGER(GET_SLOT(outputData, install("dy_nrow")))[0];

	//Get the pointers to the pre configured output data setup. These are used in output.c
	//RealD Year:
	p_Raet_yr = REAL(GET_SLOT(GET_SLOT(outputData, install("AET")),install("Year")));
	p_Rdeep_drain_yr = REAL(GET_SLOT(GET_SLOT(outputData, install("DEEPSWC")),install("Year")));
	p_Restabs_yr = REAL(GET_SLOT(GET_SLOT(outputData, install("ESTABL")),install("Year")));
	p_Revap_soil_yr = REAL(GET_SLOT(GET_SLOT(outputData, install("EVAPSOIL")),install("Year")));
	p_Revap_surface_yr = REAL(GET_SLOT(GET_SLOT(outputData, install("EVAPSURFACE")),install("Year")));
	p_Rhydred_yr = REAL(GET_SLOT(GET_SLOT(outputData, install("HYDRED")),install("Year")));
	p_Rinfiltration_yr = REAL(GET_SLOT(GET_SLOT(outputData, install("SOILINFILT")),install("Year")));
	p_Rinterception_yr = REAL(GET_SLOT(GET_SLOT(outputData, install("INTERCEPTION")),install("Year")));
	p_Rpercolation_yr = REAL(GET_SLOT(GET_SLOT(outputData, install("LYRDRAIN")),install("Year")));
	p_Rpet_yr = REAL(GET_SLOT(GET_SLOT(outputData, install("PET")),install("Year")));
	p_Rprecip_yr = REAL(GET_SLOT(GET_SLOT(outputData, install("PRECIP")),install("Year")));
	p_Rrunoff_yr = REAL(GET_SLOT(GET_SLOT(outputData, install("RUNOFF")),install("Year")));
	p_Rsnowpack_yr = REAL(GET_SLOT(GET_SLOT(outputData, install("SNOWPACK")),install("Year")));
	p_Rsoil_temp_yr = REAL(GET_SLOT(GET_SLOT(outputData, install("SOILTEMP")),install("Year")));
	p_Rsurface_water_yr = REAL(GET_SLOT(GET_SLOT(outputData, install("SURFACEWATER")),install("Year")));
	p_RvwcBulk_yr = REAL(GET_SLOT(GET_SLOT(outputData, install("VWCBULK")),install("Year")));
	p_RvwcMatric_yr = REAL(GET_SLOT(GET_SLOT(outputData, install("VWCMATRIC")),install("Year")));
	p_RswcBulk_yr = REAL(GET_SLOT(GET_SLOT(outputData, install("SWCBULK")),install("Year")));
	p_RswpMatric_yr = REAL(GET_SLOT(GET_SLOT(outputData, install("SWPMATRIC")),install("Year")));
	p_RswaBulk_yr = REAL(GET_SLOT(GET_SLOT(outputData, install("SWABULK")),install("Year")));
	p_RswaMatric_yr = REAL(GET_SLOT(GET_SLOT(outputData, install("SWAMATRIC")),install("Year")));
	p_Rtemp_yr = REAL(GET_SLOT(GET_SLOT(outputData, install("TEMP")),install("Year")));
	p_Rtransp_yr = REAL(GET_SLOT(GET_SLOT(outputData, install("TRANSP")),install("Year")));
	p_Rwetdays_yr = REAL(GET_SLOT(GET_SLOT(outputData, install("WETDAY")),install("Year")));
	//Rprintf("Year Pointers Set\n");
	//RealD mo
	p_Raet_mo = REAL(GET_SLOT(GET_SLOT(outputData, install("AET")),install("Month")));
	p_Rdeep_drain_mo = REAL(GET_SLOT(GET_SLOT(outputData, install("DEEPSWC")),install("Month")));
	p_Restabs_mo = REAL(GET_SLOT(GET_SLOT(outputData, install("ESTABL")),install("Month")));
	p_Revap_soil_mo = REAL(GET_SLOT(GET_SLOT(outputData, install("EVAPSOIL")),install("Month")));
	p_Revap_surface_mo = REAL(GET_SLOT(GET_SLOT(outputData, install("EVAPSURFACE")),install("Month")));
	p_Rhydred_mo = REAL(GET_SLOT(GET_SLOT(outputData, install("HYDRED")),install("Month")));
	p_Rinfiltration_mo = REAL(GET_SLOT(GET_SLOT(outputData, install("SOILINFILT")),install("Month")));
	p_Rinterception_mo = REAL(GET_SLOT(GET_SLOT(outputData, install("INTERCEPTION")),install("Month")));
	p_Rpercolation_mo = REAL(GET_SLOT(GET_SLOT(outputData, install("LYRDRAIN")),install("Month")));
	p_Rpet_mo = REAL(GET_SLOT(GET_SLOT(outputData, install("PET")),install("Month")));
	p_Rprecip_mo = REAL(GET_SLOT(GET_SLOT(outputData, install("PRECIP")),install("Month")));
	p_Rrunoff_mo = REAL(GET_SLOT(GET_SLOT(outputData, install("RUNOFF")),install("Month")));
	p_Rsnowpack_mo = REAL(GET_SLOT(GET_SLOT(outputData, install("SNOWPACK")),install("Month")));
	p_Rsoil_temp_mo = REAL(GET_SLOT(GET_SLOT(outputData, install("SOILTEMP")),install("Month")));
	p_Rsurface_water_mo = REAL(GET_SLOT(GET_SLOT(outputData, install("SURFACEWATER")),install("Month")));
	p_RvwcBulk_mo = REAL(GET_SLOT(GET_SLOT(outputData, install("VWCBULK")),install("Month")));
	p_RvwcMatric_mo = REAL(GET_SLOT(GET_SLOT(outputData, install("VWCMATRIC")),install("Month")));
	p_RswcBulk_mo = REAL(GET_SLOT(GET_SLOT(outputData, install("SWCBULK")),install("Month")));
	p_RswpMatric_mo = REAL(GET_SLOT(GET_SLOT(outputData, install("SWPMATRIC")),install("Month")));
	p_RswaBulk_mo = REAL(GET_SLOT(GET_SLOT(outputData, install("SWABULK")),install("Month")));
	p_RswaMatric_mo = REAL(GET_SLOT(GET_SLOT(outputData, install("SWAMATRIC")),install("Month")));
	p_Rtemp_mo = REAL(GET_SLOT(GET_SLOT(outputData, install("TEMP")),install("Month")));
	p_Rtransp_mo = REAL(GET_SLOT(GET_SLOT(outputData, install("TRANSP")),install("Month")));
	p_Rwetdays_mo = REAL(GET_SLOT(GET_SLOT(outputData, install("WETDAY")),install("Month")));
	//Rprintf("Month Pointers Set\n");
	//RealD wk
	p_Raet_wk = REAL(GET_SLOT(GET_SLOT(outputData, install("AET")),install("Week")));
	p_Rdeep_drain_wk = REAL(GET_SLOT(GET_SLOT(outputData, install("DEEPSWC")),install("Week")));
	p_Restabs_wk = REAL(GET_SLOT(GET_SLOT(outputData, install("ESTABL")),install("Week")));
	p_Revap_soil_wk = REAL(GET_SLOT(GET_SLOT(outputData, install("EVAPSOIL")),install("Week")));
	p_Revap_surface_wk = REAL(GET_SLOT(GET_SLOT(outputData, install("EVAPSURFACE")),install("Week")));
	p_Rhydred_wk = REAL(GET_SLOT(GET_SLOT(outputData, install("HYDRED")),install("Week")));
	p_Rinfiltration_wk = REAL(GET_SLOT(GET_SLOT(outputData, install("SOILINFILT")),install("Week")));
	p_Rinterception_wk = REAL(GET_SLOT(GET_SLOT(outputData, install("INTERCEPTION")),install("Week")));
	p_Rpercolation_wk = REAL(GET_SLOT(GET_SLOT(outputData, install("LYRDRAIN")),install("Week")));
	p_Rpet_wk = REAL(GET_SLOT(GET_SLOT(outputData, install("PET")),install("Week")));
	p_Rprecip_wk = REAL(GET_SLOT(GET_SLOT(outputData, install("PRECIP")),install("Week")));
	p_Rrunoff_wk = REAL(GET_SLOT(GET_SLOT(outputData, install("RUNOFF")),install("Week")));
	p_Rsnowpack_wk = REAL(GET_SLOT(GET_SLOT(outputData, install("SNOWPACK")),install("Week")));
	p_Rsoil_temp_wk = REAL(GET_SLOT(GET_SLOT(outputData, install("SOILTEMP")),install("Week")));
	p_Rsurface_water_wk = REAL(GET_SLOT(GET_SLOT(outputData, install("SURFACEWATER")),install("Week")));
	p_RvwcBulk_wk = REAL(GET_SLOT(GET_SLOT(outputData, install("VWCBULK")),install("Week")));
	p_RvwcMatric_wk = REAL(GET_SLOT(GET_SLOT(outputData, install("VWCMATRIC")),install("Week")));
	p_RswcBulk_wk = REAL(GET_SLOT(GET_SLOT(outputData, install("SWCBULK")),install("Week")));
	p_RswpMatric_wk = REAL(GET_SLOT(GET_SLOT(outputData, install("SWPMATRIC")),install("Week")));
	p_RswaBulk_wk = REAL(GET_SLOT(GET_SLOT(outputData, install("SWABULK")),install("Week")));
	p_RswaMatric_wk = REAL(GET_SLOT(GET_SLOT(outputData, install("SWAMATRIC")),install("Week")));
	p_Rtemp_wk = REAL(GET_SLOT(GET_SLOT(outputData, install("TEMP")),install("Week")));
	p_Rtransp_wk = REAL(GET_SLOT(GET_SLOT(outputData, install("TRANSP")),install("Week")));
	p_Rwetdays_wk = REAL(GET_SLOT(GET_SLOT(outputData, install("WETDAY")),install("Week")));
	//Rprintf("Week Pointers Set\n");
	//RealD dy
	p_Raet_dy = REAL(GET_SLOT(GET_SLOT(outputData, install("AET")),install("Day")));
	p_Rdeep_drain_dy = REAL(GET_SLOT(GET_SLOT(outputData, install("DEEPSWC")),install("Day")));
	p_Restabs_dy = REAL(GET_SLOT(GET_SLOT(outputData, install("ESTABL")),install("Day")));
	p_Revap_soil_dy = REAL(GET_SLOT(GET_SLOT(outputData, install("EVAPSOIL")),install("Day")));
	p_Revap_surface_dy = REAL(GET_SLOT(GET_SLOT(outputData, install("EVAPSURFACE")),install("Day")));
	p_Rhydred_dy = REAL(GET_SLOT(GET_SLOT(outputData, install("HYDRED")),install("Day")));
	p_Rinfiltration_dy = REAL(GET_SLOT(GET_SLOT(outputData, install("SOILINFILT")),install("Day")));
	p_Rinterception_dy = REAL(GET_SLOT(GET_SLOT(outputData, install("INTERCEPTION")),install("Day")));
	p_Rpercolation_dy = REAL(GET_SLOT(GET_SLOT(outputData, install("LYRDRAIN")),install("Day")));
	p_Rpet_dy = REAL(GET_SLOT(GET_SLOT(outputData, install("PET")),install("Day")));
	p_Rprecip_dy = REAL(GET_SLOT(GET_SLOT(outputData, install("PRECIP")),install("Day")));
	p_Rrunoff_dy = REAL(GET_SLOT(GET_SLOT(outputData, install("RUNOFF")),install("Day")));
	p_Rsnowpack_dy = REAL(GET_SLOT(GET_SLOT(outputData, install("SNOWPACK")),install("Day")));
	p_Rsoil_temp_dy = REAL(GET_SLOT(GET_SLOT(outputData, install("SOILTEMP")),install("Day")));
	p_Rsurface_water_dy = REAL(GET_SLOT(GET_SLOT(outputData, install("SURFACEWATER")),install("Day")));
	p_RvwcBulk_dy = REAL(GET_SLOT(GET_SLOT(outputData, install("VWCBULK")),install("Day")));
	p_RvwcMatric_dy = REAL(GET_SLOT(GET_SLOT(outputData, install("VWCMATRIC")),install("Day")));
	p_RswcBulk_dy = REAL(GET_SLOT(GET_SLOT(outputData, install("SWCBULK")),install("Day")));
	p_RswpMatric_dy = REAL(GET_SLOT(GET_SLOT(outputData, install("SWPMATRIC")),install("Day")));
	p_RswaBulk_dy = REAL(GET_SLOT(GET_SLOT(outputData, install("SWABULK")),install("Day")));
	p_RswaMatric_dy = REAL(GET_SLOT(GET_SLOT(outputData, install("SWAMATRIC")),install("Day")));
	p_Rtemp_dy = REAL(GET_SLOT(GET_SLOT(outputData, install("TEMP")),install("Day")));
	p_Rtransp_dy = REAL(GET_SLOT(GET_SLOT(outputData, install("TRANSP")),install("Day")));
	p_Rwetdays_dy = REAL(GET_SLOT(GET_SLOT(outputData, install("WETDAY")),install("Day")));
	//Rprintf("Day Pointers Set\n");
	SW_CTL_main();


	SW_SIT_clear_layers();
	SW_WTH_clear_runavg_list();

	UNPROTECT(4);

	return(R_NilValue);
}

/* Experience has shown that generating the Output Data structure in R is slow compared to C
 * This will generate the OUTPUT data Structure and Names*/
SEXP onGetOutput(void) {
	int tLayers=0, tYears = 0, tevapLayers = 0, tVegEstabCount = 0, pYearUse = 0, pMonthUse = 0, pWeekUse = 0, pDayUse = 0;
	unsigned int Raet_columns, Rdeedrain_columns, Restabs_columns, Revasoil_columns, Revasurface_columns, Rhydred_columns, Rinfiltration_columns, Rinterception_columns, Rpercolation_columns,
					Rpet_columns, Rprecip_columns, Rrunoff_columns, Rsnowpack_columns, Rsoil_temp_columns, Rsurface_water_columns, RvwcBulk_columns, RvwcMatric_columns, RswcBulk_columns, RswpMatric_columns,
					RswaBulk_columns, RswaMatric_columns, Rtemp_columns, Rtransp_columns, Rwetdays_columns, /*NOT USED ->*/ Rwthr_columns,RallH2O_columns,Ret_columns,Rallveg_columns;
	int i;
	Bool useTimeStep;

	SEXP swOutput, swOutput_Object;
	char *cSWoutput_Names[] = {"yr_nrow","mo_nrow","wk_nrow","dy_nrow","WTHR","TEMP","PRECIP","SOILINFILT","RUNOFF","ALLH2O","VWCBULK","VWCMATRIC","SWCBULK","SWABULK","SWAMATRIC","SWPMATRIC","SURFACEWATER",
			"TRANSP","EVAPSOIL","EVAPSURFACE","INTERCEPTION","LYRDRAIN","HYDRED","ET","AET","PET","WETDAY","SNOWPACK","DEEPSWC","SOILTEMP","ALLVEG","ESTABL"};

	SEXP swOutput_KEY;
	char *cSWoutput_KEY_Names[] = {"Title","TimeStep","Columns","Day","Week","Month","Year"};
	SEXP swOutput_KEY_WTHR, swOutput_KEY_TEMP, swOutput_KEY_PRECIP, swOutput_KEY_SOILINFILT, swOutput_KEY_RUNOFF, swOutput_KEY_ALLH2O, swOutput_KEY_VWCBULK, swOutput_KEY_VWCMATRIC, swOutput_KEY_SWCBULK,
		swOutput_KEY_SWPMATRIC, swOutput_KEY_SWABULK, swOutput_KEY_SWAMATRIC, swOutput_KEY_SURFACEWATER, swOutput_KEY_TRANSP, swOutput_KEY_EVAPSOIL, swOutput_KEY_EVAPSURFACE, swOutput_KEY_INTERCEPTION,
		swOutput_KEY_LYRDRAIN, swOutput_KEY_HYDRED, swOutput_KEY_ET, swOutput_KEY_AET, swOutput_KEY_PET, swOutput_KEY_WETDAY, swOutput_KEY_SNOWPACK, swOutput_KEY_DEEPSWC,
		swOutput_KEY_SOILTEMP, swOutput_KEY_ALLVEG, swOutput_KEY_ESTABL;
	char *cSWoutput_KEY_Titles[] = {"","temp_air","precip","infiltration","runoff","","vwc_bulk","vwc_matric","swc_bulk","swp_matric","swa_bulk","swa_matric","surface_water","transp","evap_soil","evap_surface",
		"interception","percolation","hydred","","aet","pet","wetdays","snowpack","deep_drain","temp_soil","","estabs"};

	SEXP Periods, TimeSteps;
/*
	SEXP Raet_yr, Rdeedrain_yr, Restabs_yr, Revasoil_yr, Revasurface_yr, Rhydred_yr, Rinfiltration_yr, Rinterception_yr, Rpercolation_yr,
			Rpet_yr, Rprecip_yr, Rrunoff_yr, Rsnowpack_yr, Rsoil_temp_yr, Rsurface_water_yr, RvwcBulk_yr, RvwcMatric_yr, RswcBulk_yr, RswpMatric_yr,
			RswaBulk_yr, RswaMatric_yr, Rtemp_yr, Rtransp_yr, Rwetdays_yr;
	SEXP Raet_mo, Rdeedrain_mo, Restabs_mo, Revasoil_mo, Revasurface_mo, Rhydred_mo, Rinfiltration_mo, Rinterception_mo, Rpercolation_mo,
			Rpet_mo, Rprecip_mo, Rrunoff_mo, Rsnowpack_mo, Rsoil_temp_mo, Rsurface_water_mo, RvwcBulk_mo, RvwcMatric_mo, RswcBulk_mo, RswpMatric_mo,
			RswaBulk_mo, RswaMatric_mo, Rtemp_mo, Rtransp_mo, Rwetdays_mo;
	SEXP Raet_wk, Rdeedrain_wk, Restabs_wk, Revasoil_wk, Revasurface_wk, Rhydred_wk, Rinfiltration_wk, Rinterception_wk, Rpercolation_wk,
			Rpet_wk, Rprecip_wk, Rrunoff_wk, Rsnowpack_wk, Rsoil_temp_wk, Rsurface_water_wk, RvwcBulk_wk, RvwcMatric_wk, RswcBulk_wk, RswpMatric_wk,
			RswaBulk_wk, RswaMatric_wk, Rtemp_wk, Rtransp_wk, Rwetdays_wk;
	SEXP Raet_dy, Rdeedrain_dy, Restabs_dy, Revasoil_dy, Revasurface_dy, Rhydred_dy, Rinfiltration_dy, Rinterception_dy, Rpercolation_dy,
			Rpet_dy, Rprecip_dy, Rrunoff_dy, Rsnowpack_dy, Rsoil_temp_dy, Rsurface_water_dy, RvwcBulk_dy, RvwcMatric_dy, RswcBulk_dy, RswpMatric_dy,
			RswaBulk_dy, RswaMatric_dy, Rtemp_dy, Rtransp_dy, Rwetdays_dy;
*/
	tLayers = 0;

	tYears = (SW_Model.endyr - SW_Model.startyr + 1);
	tLayers = SW_Site.n_layers;
	tevapLayers = SW_Site.n_evap_lyrs;
	for (i = 0; i < numPeriod; i++) {
		switch (timeSteps[i]) {
			case eSW_Day:
			pDayUse = 1;
			break;
			case eSW_Week:
			pWeekUse = 1;
			break;
			case eSW_Month:
			pMonthUse = 1;
			break;
			case eSW_Year:
			pYearUse = 1;
			break;
		}
	}

	if (SW_VegEstab.count > 0)
	tVegEstabCount = SW_VegEstab.count;

	yr_nrow = tYears * pYearUse;
	mo_nrow = tYears * 12 * pMonthUse;
	wk_nrow = tYears * 53 * pWeekUse;
	if (pDayUse == 1) {
		dy_nrow = 0;
		for (i = SW_Model.startyr; i <= SW_Model.endyr; i++) {
			dy_nrow += Time_get_lastdoy_y(i);
		}
	}

	PROTECT(Periods = GETSLOT(GETSLOT(InputData, install("output")),install("period")));
	PROTECT(TimeSteps = GETSLOT(GETSLOT(InputData, install("output")),install("timePeriods")));
	useTimeStep = LOGICAL(GETSLOT(GETSLOT(InputData, install("output")),install("useTimeStep")))[0];

	// Number of Columns for outputs
	Rwthr_columns = 0;
	Rtemp_columns = 3;
	Rprecip_columns = 5;
	Rinfiltration_columns = 1;
	Rrunoff_columns = 3;
	RallH2O_columns = 0;
	RvwcBulk_columns = tLayers;
	RvwcMatric_columns = tLayers;
	RswcBulk_columns = tLayers;
	RswpMatric_columns = tLayers;
	RswaBulk_columns = tLayers;
	RswaMatric_columns = tLayers;
	Rsurface_water_columns = 1;
	Rtransp_columns = tLayers*5;
	Revasoil_columns = tevapLayers;
	Revasurface_columns = 7;
	Rinterception_columns = 6;
	Rpercolation_columns = tLayers-1;
	Rhydred_columns = tLayers*5;
	Ret_columns = 0;
	Raet_columns = 1;
	Rpet_columns = 1;
	Rwetdays_columns = tLayers;
	Rsnowpack_columns = 2;
	Rdeedrain_columns = 1;
	Rsoil_temp_columns = tLayers;
	Rallveg_columns = 0;
	Restabs_columns = tVegEstabCount;

	PROTECT(swOutput = MAKE_CLASS("swOutput"));
	PROTECT(swOutput_Object = NEW_OBJECT(swOutput));
	SET_SLOT(swOutput_Object, install(cSWoutput_Names[0]), REAL(PROTECT(allocVector(REALSXP,1)))[0]=yr_nrow);
	SET_SLOT(swOutput_Object, install(cSWoutput_Names[1]), REAL(PROTECT(allocVector(REALSXP,1)))[0]=mo_nrow);
	SET_SLOT(swOutput_Object, install(cSWoutput_Names[2]), REAL(PROTECT(allocVector(REALSXP,1)))[0]=wk_nrow);
	SET_SLOT(swOutput_Object, install(cSWoutput_Names[3]), REAL(PROTECT(allocVector(REALSXP,1)))[0]=dy_nrow);

//KEYS//
	PROTECT(swOutput_KEY = MAKE_CLASS("swOutput_KEY"));

	//WTHR - NOTUSED
	PROTECT(swOutput_KEY_WTHR = NEW_OBJECT(swOutput_KEY));
	SET_SLOT(swOutput_KEY_WTHR, install("Title"), SET_STRING_ELT(PROTECT(NEW_STRING(1)), 0, mkChar(cSWoutput_KEY_Titles[0])));
	if(useTimeStep) {
		SET_SLOT(swOutput_KEY_WTHR, install("TimeStep"), Periods);
	} else {
		SET_SLOT(swOutput_KEY_WTHR, install("TimeStep"), INTEGER(PROTECT(NEW_INTEGER(1)))[0]=INTEGER(Periods)[0]);
	}
	SET_SLOT(swOutput_KEY_WTHR, install("Columns"), INTEGER(PROTECT(NEW_INTEGER(1)))[0]=Rwthr_columns);
	SET_SLOT(swOutput_KEY_WTHR, install("Day"), PROTECT(allocMatrix(REALSXP,dy_nrow,Rwthr_columns)));
	SET_SLOT(swOutput_KEY_WTHR, install("Week"), PROTECT(allocMatrix(REALSXP,wk_nrow,Rwthr_columns)));
	SET_SLOT(swOutput_KEY_WTHR, install("Month"), PROTECT(allocMatrix(REALSXP,mo_nrow,Rwthr_columns)));
	SET_SLOT(swOutput_KEY_WTHR, install("Year"), PROTECT(allocMatrix(REALSXP,yr_nrow,Rwthr_columns)));

	//TEMP
	PROTECT(swOutput_KEY_TEMP = NEW_OBJECT(swOutput_KEY));
	SET_SLOT(swOutput_KEY_TEMP, install("Title"), SET_STRING_ELT(PROTECT(NEW_STRING(1)), 0, mkChar(cSWoutput_KEY_Titles[1])));
	if (useTimeStep) {
		SET_SLOT(swOutput_KEY_TEMP, install("TimeStep"), Periods);
	} else {
		SET_SLOT(swOutput_KEY_TEMP, install("TimeStep"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = INTEGER(Periods)[1]);
	}
	SET_SLOT(swOutput_KEY_TEMP, install("Columns"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = Rtemp_columns);
	SET_SLOT(swOutput_KEY_TEMP, install("Day"), PROTECT(allocMatrix(REALSXP, dy_nrow, Rtemp_columns+2)));
	SET_SLOT(swOutput_KEY_TEMP, install("Week"), PROTECT(allocMatrix(REALSXP, wk_nrow, Rtemp_columns+2)));
	SET_SLOT(swOutput_KEY_TEMP, install("Month"), PROTECT(allocMatrix(REALSXP, mo_nrow, Rtemp_columns+2)));
	SET_SLOT(swOutput_KEY_TEMP, install("Year"), PROTECT(allocMatrix(REALSXP, yr_nrow, Rtemp_columns+1)));

	//PRECIP
	PROTECT(swOutput_KEY_PRECIP = NEW_OBJECT(swOutput_KEY));
	SET_SLOT(swOutput_KEY_PRECIP, install("Title"), SET_STRING_ELT(PROTECT(NEW_STRING(1)), 0, mkChar(cSWoutput_KEY_Titles[2])));
	if (useTimeStep) {
		SET_SLOT(swOutput_KEY_PRECIP, install("TimeStep"), Periods);
	} else {
		SET_SLOT(swOutput_KEY_PRECIP, install("TimeStep"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = INTEGER(Periods)[2]);
	}
	SET_SLOT(swOutput_KEY_PRECIP, install("Columns"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = Rprecip_columns);
	SET_SLOT(swOutput_KEY_PRECIP, install("Day"), PROTECT(allocMatrix(REALSXP, dy_nrow, Rprecip_columns+2)));
	SET_SLOT(swOutput_KEY_PRECIP, install("Week"), PROTECT(allocMatrix(REALSXP, wk_nrow, Rprecip_columns+2)));
	SET_SLOT(swOutput_KEY_PRECIP, install("Month"), PROTECT(allocMatrix(REALSXP, mo_nrow, Rprecip_columns+2)));
	SET_SLOT(swOutput_KEY_PRECIP, install("Year"), PROTECT(allocMatrix(REALSXP, yr_nrow, Rprecip_columns+1)));

	//SoilInf
	PROTECT(swOutput_KEY_SOILINFILT = NEW_OBJECT(swOutput_KEY));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Title"), SET_STRING_ELT(PROTECT(NEW_STRING(1)), 0, mkChar(cSWoutput_KEY_Titles[3])));
	if (useTimeStep) {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), Periods);
	} else {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = INTEGER(Periods)[3]);
	}
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Columns"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = Rinfiltration_columns);
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Day"), PROTECT(allocMatrix(REALSXP, dy_nrow, Rinfiltration_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Week"), PROTECT(allocMatrix(REALSXP, wk_nrow, Rinfiltration_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Month"), PROTECT(allocMatrix(REALSXP, mo_nrow, Rinfiltration_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Year"), PROTECT(allocMatrix(REALSXP, yr_nrow, Rinfiltration_columns+1)));

	//Runoff
	PROTECT(swOutput_KEY_RUNOFF = NEW_OBJECT(swOutput_KEY));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Title"), SET_STRING_ELT(PROTECT(NEW_STRING(1)), 0, mkChar(cSWoutput_KEY_Titles[4])));
	if (useTimeStep) {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), Periods);
	} else {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = INTEGER(Periods)[4]);
	}
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Columns"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = Rrunoff_columns);
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Day"), PROTECT(allocMatrix(REALSXP, dy_nrow, Rrunoff_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Week"), PROTECT(allocMatrix(REALSXP, wk_nrow, Rrunoff_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Month"), PROTECT(allocMatrix(REALSXP, mo_nrow, Rrunoff_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Year"), PROTECT(allocMatrix(REALSXP, yr_nrow, Rrunoff_columns+1)));

	//AllH2O - NOT USED
	PROTECT(swOutput_KEY_ALLH2O = NEW_OBJECT(swOutput_KEY));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Title"), SET_STRING_ELT(PROTECT(NEW_STRING(1)), 0, mkChar(cSWoutput_KEY_Titles[5])));
	if (useTimeStep) {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), Periods);
	} else {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = INTEGER(Periods)[5]);
	}
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Columns"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = RallH2O_columns);
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Day"), PROTECT(allocMatrix(REALSXP, dy_nrow, RallH2O_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Week"), PROTECT(allocMatrix(REALSXP, wk_nrow, RallH2O_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Month"), PROTECT(allocMatrix(REALSXP, mo_nrow, RallH2O_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Year"), PROTECT(allocMatrix(REALSXP, yr_nrow, RallH2O_columns+1)));

	//VWCBulk
	PROTECT(swOutput_KEY_VWCBULK = NEW_OBJECT(swOutput_KEY));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Title"), SET_STRING_ELT(PROTECT(NEW_STRING(1)), 0, mkChar(cSWoutput_KEY_Titles[6])));
	if (useTimeStep) {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), Periods);
	} else {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = INTEGER(Periods)[6]);
	}
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Columns"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = RvwcBulk_columns);
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Day"), PROTECT(allocMatrix(REALSXP, dy_nrow, RvwcBulk_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Week"), PROTECT(allocMatrix(REALSXP, wk_nrow, RvwcBulk_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Month"), PROTECT(allocMatrix(REALSXP, mo_nrow, RvwcBulk_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Year"), PROTECT(allocMatrix(REALSXP, yr_nrow, RvwcBulk_columns+1)));

	//VWCMatrix
	PROTECT(swOutput_KEY_VWCMATRIC = NEW_OBJECT(swOutput_KEY));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Title"), SET_STRING_ELT(PROTECT(NEW_STRING(1)), 0, mkChar(cSWoutput_KEY_Titles[7])));
	if (useTimeStep) {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), Periods);
	} else {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = INTEGER(Periods)[7]);
	}
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Columns"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = RvwcMatric_columns);
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Day"), PROTECT(allocMatrix(REALSXP, dy_nrow, RvwcMatric_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Week"), PROTECT(allocMatrix(REALSXP, wk_nrow, RvwcMatric_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Month"), PROTECT(allocMatrix(REALSXP, mo_nrow, RvwcMatric_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Year"), PROTECT(allocMatrix(REALSXP, yr_nrow, RvwcMatric_columns+1)));

	//SWCBulk
	PROTECT(swOutput_KEY_SWCBULK = NEW_OBJECT(swOutput_KEY));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Title"), SET_STRING_ELT(PROTECT(NEW_STRING(1)), 0, mkChar(cSWoutput_KEY_Titles[8])));
	if (useTimeStep) {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), Periods);
	} else {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = INTEGER(Periods)[8]);
	}
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Columns"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = RswcBulk_columns);
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Day"), PROTECT(allocMatrix(REALSXP, dy_nrow, RswcBulk_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Week"), PROTECT(allocMatrix(REALSXP, wk_nrow, RswcBulk_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Month"), PROTECT(allocMatrix(REALSXP, mo_nrow, RswcBulk_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Year"), PROTECT(allocMatrix(REALSXP, yr_nrow, RswcBulk_columns+1)));

	//SWP Matric
	PROTECT(swOutput_KEY_SWPMATRIC = NEW_OBJECT(swOutput_KEY));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Title"), SET_STRING_ELT(PROTECT(NEW_STRING(1)), 0, mkChar(cSWoutput_KEY_Titles[9])));
	if (useTimeStep) {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), Periods);
	} else {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = INTEGER(Periods)[9]);
	}
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Columns"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = RswpMatric_columns);
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Day"), PROTECT(allocMatrix(REALSXP, dy_nrow, RswpMatric_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Week"), PROTECT(allocMatrix(REALSXP, wk_nrow, RswpMatric_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Month"), PROTECT(allocMatrix(REALSXP, mo_nrow, RswpMatric_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Year"), PROTECT(allocMatrix(REALSXP, yr_nrow, RswpMatric_columns+1)));

	//SWABULK
	PROTECT(swOutput_KEY_SWABULK = NEW_OBJECT(swOutput_KEY));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Title"), SET_STRING_ELT(PROTECT(NEW_STRING(1)), 0, mkChar(cSWoutput_KEY_Titles[10])));
	if (useTimeStep) {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), Periods);
	} else {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = INTEGER(Periods)[10]);
	}
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Columns"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = RswaBulk_columns);
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Day"), PROTECT(allocMatrix(REALSXP, dy_nrow, RswaBulk_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Week"), PROTECT(allocMatrix(REALSXP, wk_nrow, RswaBulk_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Month"), PROTECT(allocMatrix(REALSXP, mo_nrow, RswaBulk_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Year"), PROTECT(allocMatrix(REALSXP, yr_nrow, RswaBulk_columns+1)));

	//SWAMATRIC
	PROTECT(swOutput_KEY_SWAMATRIC = NEW_OBJECT(swOutput_KEY));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Title"), SET_STRING_ELT(PROTECT(NEW_STRING(1)), 0, mkChar(cSWoutput_KEY_Titles[11])));
	if (useTimeStep) {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), Periods);
	} else {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = INTEGER(Periods)[11]);
	}
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Columns"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = RswaMatric_columns);
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Day"), PROTECT(allocMatrix(REALSXP, dy_nrow, RswaMatric_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Week"), PROTECT(allocMatrix(REALSXP, wk_nrow, RswaMatric_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Month"), PROTECT(allocMatrix(REALSXP, mo_nrow, RswaMatric_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Year"), PROTECT(allocMatrix(REALSXP, yr_nrow, RswaMatric_columns+1)));

	//SURFACE WATER
	PROTECT(swOutput_KEY_SURFACEWATER = NEW_OBJECT(swOutput_KEY));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Title"), SET_STRING_ELT(PROTECT(NEW_STRING(1)), 0, mkChar(cSWoutput_KEY_Titles[12])));
	if (useTimeStep) {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), Periods);
	} else {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = INTEGER(Periods)[12]);
	}
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Columns"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = Rsurface_water_columns);
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Day"), PROTECT(allocMatrix(REALSXP, dy_nrow, Rsurface_water_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Week"), PROTECT(allocMatrix(REALSXP, wk_nrow, Rsurface_water_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Month"), PROTECT(allocMatrix(REALSXP, mo_nrow, Rsurface_water_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Year"), PROTECT(allocMatrix(REALSXP, yr_nrow, Rsurface_water_columns+1)));

	//TRANSP
	PROTECT(swOutput_KEY_TRANSP = NEW_OBJECT(swOutput_KEY));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Title"), SET_STRING_ELT(PROTECT(NEW_STRING(1)), 0, mkChar(cSWoutput_KEY_Titles[13])));
	if (useTimeStep) {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), Periods);
	} else {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = INTEGER(Periods)[13]);
	}
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Columns"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = Rtransp_columns);
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Day"), PROTECT(allocMatrix(REALSXP, dy_nrow, Rtransp_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Week"), PROTECT(allocMatrix(REALSXP, wk_nrow, Rtransp_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Month"), PROTECT(allocMatrix(REALSXP, mo_nrow, Rtransp_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Year"), PROTECT(allocMatrix(REALSXP, yr_nrow, Rtransp_columns+1)));

	//EVAP SOIL
	PROTECT(swOutput_KEY_EVAPSOIL = NEW_OBJECT(swOutput_KEY));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Title"), SET_STRING_ELT(PROTECT(NEW_STRING(1)), 0, mkChar(cSWoutput_KEY_Titles[14])));
	if (useTimeStep) {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), Periods);
	} else {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = INTEGER(Periods)[14]);
	}
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Columns"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = Revasoil_columns);
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Day"), PROTECT(allocMatrix(REALSXP, dy_nrow, Revasoil_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Week"), PROTECT(allocMatrix(REALSXP, wk_nrow, Revasoil_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Month"), PROTECT(allocMatrix(REALSXP, mo_nrow, Revasoil_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Year"), PROTECT(allocMatrix(REALSXP, yr_nrow, Revasoil_columns+1)));

	//EVAP SURFACE
	PROTECT(swOutput_KEY_EVAPSURFACE = NEW_OBJECT(swOutput_KEY));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Title"), SET_STRING_ELT(PROTECT(NEW_STRING(1)), 0, mkChar(cSWoutput_KEY_Titles[15])));
	if (useTimeStep) {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), Periods);
	} else {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = INTEGER(Periods)[15]);
	}
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Columns"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = Revasurface_columns);
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Day"), PROTECT(allocMatrix(REALSXP, dy_nrow, Revasurface_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Week"), PROTECT(allocMatrix(REALSXP, wk_nrow, Revasurface_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Month"), PROTECT(allocMatrix(REALSXP, mo_nrow, Revasurface_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Year"), PROTECT(allocMatrix(REALSXP, yr_nrow, Revasurface_columns+1)));

	//SOIL INFILT
	PROTECT(swOutput_KEY_INTERCEPTION = NEW_OBJECT(swOutput_KEY));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Title"), SET_STRING_ELT(PROTECT(NEW_STRING(1)), 0, mkChar(cSWoutput_KEY_Titles[16])));
	if (useTimeStep) {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), Periods);
	} else {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = INTEGER(Periods)[16]);
	}
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Columns"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = Rinfiltration_columns);
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Day"), PROTECT(allocMatrix(REALSXP, dy_nrow, Rinfiltration_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Week"), PROTECT(allocMatrix(REALSXP, wk_nrow, Rinfiltration_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Month"), PROTECT(allocMatrix(REALSXP, mo_nrow, Rinfiltration_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Year"), PROTECT(allocMatrix(REALSXP, yr_nrow, Rinfiltration_columns+1)));

	//Percolation
	PROTECT(swOutput_KEY_LYRDRAIN = NEW_OBJECT(swOutput_KEY));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Title"), SET_STRING_ELT(PROTECT(NEW_STRING(1)), 0, mkChar(cSWoutput_KEY_Titles[17])));
	if (useTimeStep) {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), Periods);
	} else {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = INTEGER(Periods)[17]);
	}
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Columns"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = Rpercolation_columns);
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Day"), PROTECT(allocMatrix(REALSXP, dy_nrow, Rpercolation_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Week"), PROTECT(allocMatrix(REALSXP, wk_nrow, Rpercolation_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Month"), PROTECT(allocMatrix(REALSXP, mo_nrow, Rpercolation_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Year"), PROTECT(allocMatrix(REALSXP, yr_nrow, Rpercolation_columns+1)));

	//Hydred
	PROTECT(swOutput_KEY_HYDRED = NEW_OBJECT(swOutput_KEY));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Title"), SET_STRING_ELT(PROTECT(NEW_STRING(1)), 0, mkChar(cSWoutput_KEY_Titles[18])));
	if (useTimeStep) {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), Periods);
	} else {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = INTEGER(Periods)[18]);
	}
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Columns"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = Rhydred_columns);
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Day"), PROTECT(allocMatrix(REALSXP, dy_nrow, Rhydred_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Week"), PROTECT(allocMatrix(REALSXP, wk_nrow, Rhydred_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Month"), PROTECT(allocMatrix(REALSXP, mo_nrow, Rhydred_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Year"), PROTECT(allocMatrix(REALSXP, yr_nrow, Rhydred_columns+1)));

	//ET
	PROTECT(swOutput_KEY_ET = NEW_OBJECT(swOutput_KEY));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Title"), SET_STRING_ELT(PROTECT(NEW_STRING(1)), 0, mkChar(cSWoutput_KEY_Titles[19])));
	if (useTimeStep) {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), Periods);
	} else {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = INTEGER(Periods)[19]);
	}
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Columns"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = Ret_columns);
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Day"), PROTECT(allocMatrix(REALSXP, dy_nrow, Ret_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Week"), PROTECT(allocMatrix(REALSXP, wk_nrow, Ret_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Month"), PROTECT(allocMatrix(REALSXP, mo_nrow, Ret_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Year"), PROTECT(allocMatrix(REALSXP, yr_nrow, Ret_columns+1)));

	//AET
	PROTECT(swOutput_KEY_AET = NEW_OBJECT(swOutput_KEY));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Title"), SET_STRING_ELT(PROTECT(NEW_STRING(1)), 0, mkChar(cSWoutput_KEY_Titles[20])));
	if (useTimeStep) {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), Periods);
	} else {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = INTEGER(Periods)[20]);
	}
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Columns"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = Raet_columns);
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Day"), PROTECT(allocMatrix(REALSXP, dy_nrow, Raet_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Week"), PROTECT(allocMatrix(REALSXP, wk_nrow, Raet_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Month"), PROTECT(allocMatrix(REALSXP, mo_nrow, Raet_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Year"), PROTECT(allocMatrix(REALSXP, yr_nrow, Raet_columns+1)));

	//PET
	PROTECT(swOutput_KEY_PET = NEW_OBJECT(swOutput_KEY));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Title"), SET_STRING_ELT(PROTECT(NEW_STRING(1)), 0, mkChar(cSWoutput_KEY_Titles[21])));
	if (useTimeStep) {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), Periods);
	} else {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = INTEGER(Periods)[21]);
	}
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Columns"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = Rpet_columns);
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Day"), PROTECT(allocMatrix(REALSXP, dy_nrow, Rpet_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Week"), PROTECT(allocMatrix(REALSXP, wk_nrow, Rpet_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Month"), PROTECT(allocMatrix(REALSXP, mo_nrow, Rpet_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Year"), PROTECT(allocMatrix(REALSXP, yr_nrow, Rpet_columns+1)));

	//WET DAYS
	PROTECT(swOutput_KEY_WETDAY = NEW_OBJECT(swOutput_KEY));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Title"), SET_STRING_ELT(PROTECT(NEW_STRING(1)), 0, mkChar(cSWoutput_KEY_Titles[22])));
	if (useTimeStep) {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), Periods);
	} else {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = INTEGER(Periods)[22]);
	}
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Columns"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = Rwetdays_columns);
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Day"), PROTECT(allocMatrix(REALSXP, dy_nrow, Rwetdays_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Week"), PROTECT(allocMatrix(REALSXP, wk_nrow, Rwetdays_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Month"), PROTECT(allocMatrix(REALSXP, mo_nrow, Rwetdays_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Year"), PROTECT(allocMatrix(REALSXP, yr_nrow, Rwetdays_columns+1)));

	//SNOW PACK
	PROTECT(swOutput_KEY_SNOWPACK = NEW_OBJECT(swOutput_KEY));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Title"), SET_STRING_ELT(PROTECT(NEW_STRING(1)), 0, mkChar(cSWoutput_KEY_Titles[23])));
	if (useTimeStep) {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), Periods);
	} else {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = INTEGER(Periods)[23]);
	}
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Columns"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = Rsnowpack_columns);
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Day"), PROTECT(allocMatrix(REALSXP, dy_nrow, Rsnowpack_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Week"), PROTECT(allocMatrix(REALSXP, wk_nrow, Rsnowpack_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Month"), PROTECT(allocMatrix(REALSXP, mo_nrow, Rsnowpack_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Year"), PROTECT(allocMatrix(REALSXP, yr_nrow, Rsnowpack_columns+1)));

	//DEEP SWC
	PROTECT(swOutput_KEY_DEEPSWC = NEW_OBJECT(swOutput_KEY));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Title"), SET_STRING_ELT(PROTECT(NEW_STRING(1)), 0, mkChar(cSWoutput_KEY_Titles[24])));
	if (useTimeStep) {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), Periods);
	} else {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = INTEGER(Periods)[24]);
	}
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Columns"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = Rdeedrain_columns);
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Day"), PROTECT(allocMatrix(REALSXP, dy_nrow, Rdeedrain_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Week"), PROTECT(allocMatrix(REALSXP, wk_nrow, Rdeedrain_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Month"), PROTECT(allocMatrix(REALSXP, mo_nrow, Rdeedrain_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Year"), PROTECT(allocMatrix(REALSXP, yr_nrow, Rdeedrain_columns+1)));

	//Soil Temp
	PROTECT(swOutput_KEY_SOILTEMP = NEW_OBJECT(swOutput_KEY));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Title"), SET_STRING_ELT(PROTECT(NEW_STRING(1)), 0, mkChar(cSWoutput_KEY_Titles[25])));
	if (useTimeStep) {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), Periods);
	} else {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = INTEGER(Periods)[25]);
	}
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Columns"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = Rsoil_temp_columns);
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Day"), PROTECT(allocMatrix(REALSXP, dy_nrow, Rsoil_temp_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Week"), PROTECT(allocMatrix(REALSXP, wk_nrow, Rsoil_temp_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Month"), PROTECT(allocMatrix(REALSXP, mo_nrow, Rsoil_temp_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Year"), PROTECT(allocMatrix(REALSXP, yr_nrow, Rsoil_temp_columns+1)));

	//ALL VEG
	PROTECT(swOutput_KEY_ALLVEG = NEW_OBJECT(swOutput_KEY));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Title"), SET_STRING_ELT(PROTECT(NEW_STRING(1)), 0, mkChar(cSWoutput_KEY_Titles[26])));
	if (useTimeStep) {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), Periods);
	} else {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = INTEGER(Periods)[26]);
	}
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Columns"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = Rallveg_columns);
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Day"), PROTECT(allocMatrix(REALSXP, dy_nrow, Rallveg_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Week"), PROTECT(allocMatrix(REALSXP, wk_nrow, Rallveg_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Month"), PROTECT(allocMatrix(REALSXP, mo_nrow, Rallveg_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Year"), PROTECT(allocMatrix(REALSXP, yr_nrow, Rallveg_columns+1)));

	//ESTABL
	PROTECT(swOutput_KEY_ESTABL = NEW_OBJECT(swOutput_KEY));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Title"), SET_STRING_ELT(PROTECT(NEW_STRING(1)), 0, mkChar(cSWoutput_KEY_Titles[27])));
	if (useTimeStep) {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), Periods);
	} else {
		SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = INTEGER(Periods)[27]);
	}
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Columns"), INTEGER(PROTECT(NEW_INTEGER(1)))[0] = Restabs_columns);
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Day"), PROTECT(allocMatrix(REALSXP, dy_nrow, Restabs_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Week"), PROTECT(allocMatrix(REALSXP, wk_nrow, Restabs_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Month"), PROTECT(allocMatrix(REALSXP, mo_nrow, Restabs_columns+2)));
	SET_SLOT(swOutput_KEY_SOILINFILT, install("Year"), PROTECT(allocMatrix(REALSXP, yr_nrow, Restabs_columns+1)));


	SET_SLOT(swOutput_Object, install(cSWoutput_Names[4]), swOutput_KEY_WTHR);
	SET_SLOT(swOutput_Object, install(cSWoutput_Names[5]), swOutput_KEY_TEMP);
	SET_SLOT(swOutput_Object, install(cSWoutput_Names[6]), swOutput_KEY_PRECIP);
	SET_SLOT(swOutput_Object, install(cSWoutput_Names[7]), swOutput_KEY_SOILINFILT);
	SET_SLOT(swOutput_Object, install(cSWoutput_Names[8]), swOutput_KEY_RUNOFF);
	SET_SLOT(swOutput_Object, install(cSWoutput_Names[9]), swOutput_KEY_ALLH2O);
	SET_SLOT(swOutput_Object, install(cSWoutput_Names[10]), swOutput_KEY_VWCBULK);
	SET_SLOT(swOutput_Object, install(cSWoutput_Names[11]), swOutput_KEY_VWCMATRIC);
	SET_SLOT(swOutput_Object, install(cSWoutput_Names[12]), swOutput_KEY_SWCBULK);
	SET_SLOT(swOutput_Object, install(cSWoutput_Names[13]), swOutput_KEY_SWPMATRIC);
	SET_SLOT(swOutput_Object, install(cSWoutput_Names[14]), swOutput_KEY_SWABULK);
	SET_SLOT(swOutput_Object, install(cSWoutput_Names[15]), swOutput_KEY_SWAMATRIC);
	SET_SLOT(swOutput_Object, install(cSWoutput_Names[16]), swOutput_KEY_SURFACEWATER);
	SET_SLOT(swOutput_Object, install(cSWoutput_Names[17]), swOutput_KEY_TRANSP);
	SET_SLOT(swOutput_Object, install(cSWoutput_Names[18]), swOutput_KEY_EVAPSOIL);
	SET_SLOT(swOutput_Object, install(cSWoutput_Names[19]), swOutput_KEY_EVAPSURFACE);
	SET_SLOT(swOutput_Object, install(cSWoutput_Names[20]), swOutput_KEY_INTERCEPTION);
	SET_SLOT(swOutput_Object, install(cSWoutput_Names[21]), swOutput_KEY_LYRDRAIN);
	SET_SLOT(swOutput_Object, install(cSWoutput_Names[22]), swOutput_KEY_HYDRED);
	SET_SLOT(swOutput_Object, install(cSWoutput_Names[23]), swOutput_KEY_ET);
	SET_SLOT(swOutput_Object, install(cSWoutput_Names[24]), swOutput_KEY_AET);
	SET_SLOT(swOutput_Object, install(cSWoutput_Names[25]), swOutput_KEY_PET);
	SET_SLOT(swOutput_Object, install(cSWoutput_Names[26]), swOutput_KEY_WETDAY);
	SET_SLOT(swOutput_Object, install(cSWoutput_Names[27]), swOutput_KEY_SNOWPACK);
	SET_SLOT(swOutput_Object, install(cSWoutput_Names[28]), swOutput_KEY_DEEPSWC);
	SET_SLOT(swOutput_Object, install(cSWoutput_Names[29]), swOutput_KEY_SOILTEMP);
	SET_SLOT(swOutput_Object, install(cSWoutput_Names[30]), swOutput_KEY_ALLVEG);
	SET_SLOT(swOutput_Object, install(cSWoutput_Names[31]), swOutput_KEY_ESTABL);

}

#endif
