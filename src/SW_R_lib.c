/*
 * SW_R_lib.c
 *
 *  Created on: Jun 25, 2013
 *      Author: Ryan Murphy
 */
//#define RSOILWAT
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
//extern int timeSteps[SW_OUTNKEYS][4];
//extern int numPeriod;
extern SW_MODEL SW_Model;
//extern SW_SITE SW_Site;
extern SW_VEGESTAB SW_VegEstab;


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

SEXP start(SEXP inputOptions, SEXP inputData, SEXP weatherList) {
	int tYears = 0, tevapLayers = 0, tVegEstabCount = 0, pYearUse = 0, pMonthUse = 0, pWeekUse = 0, pDayUse = 0;
	int i;
	SEXP outputData;

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

	PROTECT(outputData = onGetOutput(inputData));

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

	UNPROTECT(5);

	return(outputData);
}

/* Experience has shown that generating the Output Data structure in R is slow compared to C
 * This will generate the OUTPUT data Structure and Names*/
SEXP onGetOutput(SEXP inputData) {
	int debug = 0;

	int tLayers=0, tYears = 0, tevapLayers = 0, tVegEstabCount = 0, pYearUse = 0, pMonthUse = 0, pWeekUse = 0, pDayUse = 0;
	unsigned int Raet_columns, Rdeedrain_columns, Restabs_columns, Revasoil_columns, Revasurface_columns, Rhydred_columns, Rinfiltration_columns, Rinterception_columns, Rpercolation_columns,
					Rpet_columns, Rprecip_columns, Rrunoff_columns, Rsnowpack_columns, Rsoil_temp_columns, Rsurface_water_columns, RvwcBulk_columns, RvwcMatric_columns, RswcBulk_columns, RswpMatric_columns, RswaBulk_columns,
					RswaMatric_columns, Rtemp_columns, Rtransp_columns, Rwetdays_columns, /*NOT USED ->*/ Rwthr_columns,RallH2O_columns,Ret_columns,Rallveg_columns;
	int i,j, pCount=0;
	int use[28];
	int periodUse[28][4];
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
	char *cSWoutput_KEY_Titles[] = {"","temp_air","precip","infiltration","runoff","","vwc_bulk","vwc_matric","swc_bulk","swa_bulk","swa_matric","swp_matric","surface_water","transp","evap_soil","evap_surface",
		"interception","percolation","hydred","","aet","pet","wetdays","snowpack","deep_drain","temp_soil","","estabs"};

	SEXP Periods, TimeSteps;
	SEXP r_dy_nrow, r_wk_nrow, r_mo_nrow, r_yr_nrow;
	SEXP r_WTHR_NAME, r_TEMP_NAME, r_PRECIP_NAME, r_SOILINFILT_NAME, r_RUNOFF_NAME, r_ALLH2O_NAME, r_VWCBULK_NAME, r_VWCMATRIC_NAME, r_SWCBULK_NAME, r_SWPMATRIC_NAME, r_SWABULK_NAME, r_SWAMATRIC_NAME, r_SURFACEWATER_NAME, r_TRANSP_NAME, r_EVAPSOIL_NAME, r_EVAPSURFACE_NAME, r_INTERCEPTION_NAME, r_LYRDRAIN_NAME, r_HYDRED_NAME, r_ET_NAME, r_AET_NAME, r_PET_NAME, r_WETDAY_NAME, r_SNOWPACK_NAME, r_DEEPSWC_NAME, r_SOILTEMP_NAME, r_ALLVEG_NAME, r_ESTABL_NAME;
	SEXP r_WTHR_PERIOD, r_TEMP_PERIOD, r_PRECIP_PERIOD, r_SOILINFILT_PERIOD, r_RUNOFF_PERIOD, r_ALLH2O_PERIOD, r_VWCBULK_PERIOD, r_VWCMATRIC_PERIOD, r_SWCBULK_PERIOD, r_SWPMATRIC_PERIOD, r_SWABULK_PERIOD, r_SWAMATRIC_PERIOD, r_SURFACEWATER_PERIOD, r_TRANSP_PERIOD, r_EVAPSOIL_PERIOD, r_EVAPSURFACE_PERIOD, r_INTERCEPTION_PERIOD, r_LYRDRAIN_PERIOD, r_HYDRED_PERIOD, r_ET_PERIOD, r_AET_PERIOD, r_PET_PERIOD, r_WETDAY_PERIOD, r_SNOWPACK_PERIOD, r_DEEPSWC_PERIOD, r_SOILTEMP_PERIOD, r_ALLVEG_PERIOD, r_ESTABL_PERIOD;
	SEXP r_WTHR_COLUMNS, r_TEMP_COLUMNS, r_PRECIP_COLUMNS, r_SOILINFILT_COLUMNS, r_RUNOFF_COLUMNS, r_ALLH2O_COLUMNS, r_VWCBULK_COLUMNS, r_VWCMATRIC_COLUMNS, r_SWCBULK_COLUMNS, r_SWPMATRIC_COLUMNS, r_SWABULK_COLUMNS, r_SWAMATRIC_COLUMNS, r_SURFACEWATER_COLUMNS, r_TRANSP_COLUMNS, r_EVAPSOIL_COLUMNS, r_EVAPSURFACE_COLUMNS, r_INTERCEPTION_COLUMNS, r_LYRDRAIN_COLUMNS, r_HYDRED_COLUMNS, r_ET_COLUMNS, r_AET_COLUMNS, r_PET_COLUMNS, r_WETDAY_COLUMNS, r_SNOWPACK_COLUMNS, r_DEEPSWC_COLUMNS, r_SOILTEMP_COLUMNS, r_ALLVEG_COLUMNS, r_ESTABL_COLUMNS;

	SEXP Rallveg_yr, Ret_yr, RallH2O_yr, Rwthr_yr, Raet_yr, Rdeedrain_yr, Restabs_yr, Revasoil_yr, Revasurface_yr, Rhydred_yr, Rinfiltration_yr, Rinterception_yr, Rpercolation_yr,
			Rpet_yr, Rprecip_yr, Rrunoff_yr, Rsnowpack_yr, Rsoil_temp_yr, Rsurface_water_yr, RvwcBulk_yr, RvwcMatric_yr, RswcBulk_yr, RswpMatric_yr,
			RswaBulk_yr, RswaMatric_yr, Rtemp_yr, Rtransp_yr, Rwetdays_yr;
	SEXP Rallveg_mo, Ret_mo, RallH2O_mo, Rwthr_mo, Raet_mo, Rdeedrain_mo, Restabs_mo, Revasoil_mo, Revasurface_mo, Rhydred_mo, Rinfiltration_mo, Rinterception_mo, Rpercolation_mo,
			Rpet_mo, Rprecip_mo, Rrunoff_mo, Rsnowpack_mo, Rsoil_temp_mo, Rsurface_water_mo, RvwcBulk_mo, RvwcMatric_mo, RswcBulk_mo, RswpMatric_mo,
			RswaBulk_mo, RswaMatric_mo, Rtemp_mo, Rtransp_mo, Rwetdays_mo;
	SEXP Rallveg_wk, Ret_wk, RallH2O_wk, Rwthr_wk, Raet_wk, Rdeedrain_wk, Restabs_wk, Revasoil_wk, Revasurface_wk, Rhydred_wk, Rinfiltration_wk, Rinterception_wk, Rpercolation_wk,
			Rpet_wk, Rprecip_wk, Rrunoff_wk, Rsnowpack_wk, Rsoil_temp_wk, Rsurface_water_wk, RvwcBulk_wk, RvwcMatric_wk, RswcBulk_wk, RswpMatric_wk,
			RswaBulk_wk, RswaMatric_wk, Rtemp_wk, Rtransp_wk, Rwetdays_wk;
	SEXP Rallveg_dy, Ret_dy, RallH2O_dy, Rwthr_dy, Raet_dy, Rdeedrain_dy, Restabs_dy, Revasoil_dy, Revasurface_dy, Rhydred_dy, Rinfiltration_dy, Rinterception_dy, Rpercolation_dy,
			Rpet_dy, Rprecip_dy, Rrunoff_dy, Rsnowpack_dy, Rsoil_temp_dy, Rsurface_water_dy, RvwcBulk_dy, RvwcMatric_dy, RswcBulk_dy, RswpMatric_dy,
			RswaBulk_dy, RswaMatric_dy, Rtemp_dy, Rtransp_dy, Rwetdays_dy;

	/************ NAMES ****************/
	SEXP Ret_names_yr, Ret_names_y_yr, Raet_names_yr, Raet_names_y_yr, Rdeep_drain_names_yr, Rdeep_drain_names_y_yr, Restabs_names_yr, Restabs_names_y_yr, Revap_soil_names_yr, Revap_soil_names_y_yr,
	Revap_surface_names_yr, Revap_surface_names_y_yr, Rhydred_names_yr, Rhydred_names_y_yr, Rinfiltration_names_yr, Rinfiltration_names_y_yr, Rinterception_names_yr,
	Rinterception_names_y_yr, Rpercolation_names_yr, Rpercolation_names_y_yr, Rpet_names_yr, Rpet_names_y_yr, Rprecip_names_yr, Rprecip_names_y_yr, Rrunoff_names_yr,
	Rrunoff_names_y_yr, Rsnowpack_names_yr, Rsnowpack_names_y_yr, Rsoil_temp_names_yr, Rsoil_temp_names_y_yr, Rsurface_water_names_yr, Rsurface_water_names_y_yr,
	Rsw_pot_names_yr, Rsw_pot_names_y_yr, RswaBulk_names_yr, RswaBulk_names_y_yr, RswaMatric_names_yr, RswaMatric_names_y_yr, RswcBulk_names_yr, RswcBulk_names_y_yr, Rtemp_names_yr, Rtemp_names_y_yr, Rtransp_names_yr,
	Rtransp_names_y_yr, RvwcBulk_names_yr, RvwcBulk_names_y_yr, RvwcMatric_names_yr, RvwcMatric_names_y_yr, Rwetdays_names_yr, Rwetdays_names_y_yr, RswpMatric_names_yr, RswpMatric_names_y_yr;
	SEXP Ret_names_mo, Ret_names_y_mo, Raet_names_mo, Raet_names_y_mo, Rdeep_drain_names_mo, Rdeep_drain_names_y_mo, Restabs_names_mo, Restabs_names_y_mo, Revap_soil_names_mo, Revap_soil_names_y_mo,
	Revap_surface_names_mo, Revap_surface_names_y_mo, Rhydred_names_mo, Rhydred_names_y_mo, Rinfiltration_names_mo, Rinfiltration_names_y_mo, Rinterception_names_mo,
	Rinterception_names_y_mo, Rpercolation_names_mo, Rpercolation_names_y_mo, Rpet_names_mo, Rpet_names_y_mo, Rprecip_names_mo, Rprecip_names_y_mo, Rrunoff_names_mo,
	Rrunoff_names_y_mo, Rsnowpack_names_mo, Rsnowpack_names_y_mo, Rsoil_temp_names_mo, Rsoil_temp_names_y_mo, Rsurface_water_names_mo, Rsurface_water_names_y_mo,
	Rsw_pot_names_mo, Rsw_pot_names_y_mo, RswaBulk_names_mo, RswaBulk_names_y_mo, RswaMatric_names_mo, RswaMatric_names_y_mo, RswcBulk_names_mo, RswcBulk_names_y_mo, Rtemp_names_mo, Rtemp_names_y_mo, Rtransp_names_mo,
	Rtransp_names_y_mo, RvwcBulk_names_mo, RvwcBulk_names_y_mo, RvwcMatric_names_mo, RvwcMatric_names_y_mo, Rwetdays_names_mo, Rwetdays_names_y_mo, RswpMatric_names_mo, RswpMatric_names_y_mo;
	SEXP Ret_names_wk, Ret_names_y_wk, Raet_names_wk, Raet_names_y_wk, Rdeep_drain_names_wk, Rdeep_drain_names_y_wk, Restabs_names_wk, Restabs_names_y_wk, Revap_soil_names_wk, Revap_soil_names_y_wk,
	Revap_surface_names_wk, Revap_surface_names_y_wk, Rhydred_names_wk, Rhydred_names_y_wk, Rinfiltration_names_wk, Rinfiltration_names_y_wk, Rinterception_names_wk,
	Rinterception_names_y_wk, Rpercolation_names_wk, Rpercolation_names_y_wk, Rpet_names_wk, Rpet_names_y_wk, Rprecip_names_wk, Rprecip_names_y_wk, Rrunoff_names_wk,
	Rrunoff_names_y_wk, Rsnowpack_names_wk, Rsnowpack_names_y_wk, Rsoil_temp_names_wk, Rsoil_temp_names_y_wk, Rsurface_water_names_wk, Rsurface_water_names_y_wk,
	Rsw_pot_names_wk, Rsw_pot_names_y_wk, RswaBulk_names_wk, RswaBulk_names_y_wk, RswaMatric_names_wk, RswaMatric_names_y_wk, RswcBulk_names_wk, RswcBulk_names_y_wk, Rtemp_names_wk, Rtemp_names_y_wk, Rtransp_names_wk,
	Rtransp_names_y_wk, RvwcBulk_names_wk, RvwcBulk_names_y_wk, RvwcMatric_names_wk, RvwcMatric_names_y_wk, Rwetdays_names_wk, Rwetdays_names_y_wk, RswpMatric_names_wk, RswpMatric_names_y_wk;
	SEXP Ret_names_dy, Ret_names_y_dy, Raet_names_dy, Raet_names_y_dy, Rdeep_drain_names_dy, Rdeep_drain_names_y_dy, Restabs_names_dy, Restabs_names_y_dy, Revap_soil_names_dy, Revap_soil_names_y_dy,
	Revap_surface_names_dy, Revap_surface_names_y_dy, Rhydred_names_dy, Rhydred_names_y_dy, Rinfiltration_names_dy, Rinfiltration_names_y_dy, Rinterception_names_dy,
	Rinterception_names_y_dy, Rpercolation_names_dy, Rpercolation_names_y_dy, Rpet_names_dy, Rpet_names_y_dy, Rprecip_names_dy, Rprecip_names_y_dy, Rrunoff_names_dy,
	Rrunoff_names_y_dy, Rsnowpack_names_dy, Rsnowpack_names_y_dy, Rsoil_temp_names_dy, Rsoil_temp_names_y_dy, Rsurface_water_names_dy, Rsurface_water_names_y_dy,
	Rsw_pot_names_dy, Rsw_pot_names_y_dy, RswaBulk_names_dy, RswaBulk_names_y_dy, RswaMatric_names_dy, RswaMatric_names_y_dy, RswcBulk_names_dy, RswcBulk_names_y_dy, Rtemp_names_dy, Rtemp_names_y_dy, Rtransp_names_dy,
	Rtransp_names_y_dy, RvwcBulk_names_dy, RvwcBulk_names_y_dy, RvwcMatric_names_dy, RvwcMatric_names_y_dy, Rwetdays_names_dy, Rwetdays_names_y_dy, RswpMatric_names_dy, RswpMatric_names_y_dy;

	char *Layers_names[] = { "Lyr_1", "Lyr_2", "Lyr_3", "Lyr_4", "Lyr_5", "Lyr_6", "Lyr_7", "Lyr_8", "Lyr_9", "Lyr_10", "Lyr_11", "Lyr_12", "Lyr_13", "Lyr_14", "Lyr_15",
			"Lyr_16", "Lyr_17", "Lyr_18", "Lyr_19", "Lyr_20", "Lyr_21", "Lyr_22", "Lyr_23", "Lyr_24", "Lyr_25", "Lyr_26", "Lyr_27", "Lyr_28", "Lyr_29", "Lyr_30" };
	char *Cevap_surface_names[] = { "total_evap", "tree_evap", "shrub_evap","forbs_evap", "grass_evap", "litter_evap", "surfaceWater_evap" };
	char *Chydred_names[] = { "total_", "tree_", "shrub_", "forbs_", "grass_" };
	char *Cinterception_names[] = { "total", "tree", "shrub", "forbs", "grass", "litter" };
	char *Cprecip_names[] = { "ppt", "rain", "snow_fall", "snowmelt", "snowloss" };
	char *Crunoff_names[] = { "total", "ponded", "snowmelt" };
	char *Csnowpack_names[] = { "snowpackWaterEquivalent_cm", "snowdepth_cm" };
	char *Ctemp_names[] = { "max_C", "min_C", "avg_C" };
	char *Ctransp_names[] = { "transp_total_", "transp_tree_", "transp_shrub_", "transp_forbs_", "transp_grass_" };
	char Ctemp[50];
	/****************************************************************************************/

	tLayers = 0;
	tevapLayers=0;

	for(i=0; i<length(GET_SLOT(GET_SLOT(inputData, install("output")), install("use")));i++) {
		use[i] = LOGICAL(GET_SLOT(GET_SLOT(inputData, install("output")), install("use")))[i];
		//pCount+=4;
	}

	tYears = (INTEGER(GET_SLOT(GET_SLOT(inputData, install("years")), install("EndYear")))[0] - INTEGER(GET_SLOT(GET_SLOT(inputData, install("years")), install("StartYear")))[0] + 1);
	tLayers = nrows(GET_SLOT(GET_SLOT(inputData, install("soils")), install("Layers")));
	for(i=0; i<tLayers; i++) {
		if(REAL(GET_SLOT(GET_SLOT(inputData, install("soils")), install("Layers")))[i + (tLayers) * 3] > 0)
			tevapLayers++;
	}

	if(debug) Rprintf("tYears: %d, tLayers: %d, tEvapLayers: %d \n", tYears, tLayers, tevapLayers);

	PROTECT(TimeSteps = GET_SLOT(GET_SLOT(inputData, install("output")),install("timePeriods")));
	useTimeStep = LOGICAL(GET_SLOT(GET_SLOT(inputData, install("output")),install("useTimeStep")))[0];
	for (i = 0; i < LENGTH(TimeSteps); i++) {
		switch (INTEGER(TimeSteps)[i]) {
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

	tVegEstabCount = INTEGER(GET_SLOT(GET_SLOT(inputData, install("estab")), install("count")))[0];

	//tVegEstabCount = SW_VegEstab.count;

	yr_nrow = tYears * pYearUse;
	mo_nrow = tYears * 12 * pMonthUse;
	wk_nrow = tYears * 53 * pWeekUse;
	if (pDayUse == 1) {
		dy_nrow = 0;
		for (i = INTEGER(GET_SLOT(GET_SLOT(inputData, install("years")), install("StartYear")))[0]; i <= INTEGER(GET_SLOT(GET_SLOT(inputData, install("years")), install("EndYear")))[0]; i++) {
			if(i==0) {//Need to calculate the starting first day of first year
				dy_nrow += Time_get_lastdoy_y(i) - INTEGER(GET_SLOT(GET_SLOT(inputData, install("years")), install("FDOFY")))[0] + 1;
				if(debug) Rprintf("Year: %d DAYSINYEAR: %d\n",i,Time_get_lastdoy_y(i) - INTEGER(GET_SLOT(GET_SLOT(inputData, install("years")), install("FDOFY")))[0] + 1);
			} else if(i==(tYears-1)) {//and last day of last year.
				dy_nrow += INTEGER(GET_SLOT(GET_SLOT(inputData, install("years")), install("EDOEY")))[0];
				if(debug) Rprintf("Year: %d DAYSINYEAR: %d\n",i,INTEGER(GET_SLOT(GET_SLOT(inputData, install("years")), install("EDOEY")))[0]);
			} else {
				dy_nrow += Time_get_lastdoy_y(i);
				if(debug) Rprintf("Year: %d DAYSINYEAR: %d\n",i,Time_get_lastdoy_y(i));
			}
		}
	}

	if(debug) Rprintf("Year Rows: %d, Month Rows: %d, Week Rows: %d, Day Rows: %d\n",yr_nrow, mo_nrow, wk_nrow, dy_nrow);

	if(useTimeStep)
		PROTECT(Periods = GET_SLOT(GET_SLOT(inputData, install("output")),install("timePeriods")));
	else
		PROTECT(Periods = GET_SLOT(GET_SLOT(inputData, install("output")),install("period")));

	for(i=0; i<28; i++) {
		periodUse[i][0]=periodUse[i][1]=periodUse[i][2]=periodUse[i][3]=0;
		if(useTimeStep) {
			for(j=0; j<length(Periods); j++) {
				periodUse[i][INTEGER(Periods)[j]] = 1;
				//pCount+=3;
			}
		} else {
			periodUse[i][INTEGER(Periods)[0]] = 1;
			//pCount+=3;
		}
	}

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

	PROTECT(r_dy_nrow = allocVector(INTSXP,1));
	INTEGER(r_dy_nrow)[0]=dy_nrow;
	PROTECT(r_wk_nrow = allocVector(INTSXP,1));
	INTEGER(r_wk_nrow)[0]=wk_nrow;
	PROTECT(r_mo_nrow = allocVector(INTSXP,1));
	INTEGER(r_mo_nrow)[0]=mo_nrow;
	PROTECT(r_yr_nrow = allocVector(INTSXP,1));
	INTEGER(r_yr_nrow)[0]=yr_nrow;

	SET_SLOT(swOutput_Object, install(cSWoutput_Names[0]), r_yr_nrow);
	SET_SLOT(swOutput_Object, install(cSWoutput_Names[1]), r_mo_nrow);
	SET_SLOT(swOutput_Object, install(cSWoutput_Names[2]), r_wk_nrow);
	SET_SLOT(swOutput_Object, install(cSWoutput_Names[3]), r_dy_nrow);

//KEYS//
	PROTECT(swOutput_KEY = MAKE_CLASS("swOutput_KEY"));

	pCount+=9;
	//WTHR - NOTUSED
	if(use[0]) {
		if(debug) Rprintf("%s\n",cSWoutput_KEY_Titles[0]);
		PROTECT(swOutput_KEY_WTHR = NEW_OBJECT(swOutput_KEY));
		PROTECT(r_WTHR_NAME = NEW_STRING(1));
		SET_STRING_ELT(r_WTHR_NAME, 0, mkChar(cSWoutput_KEY_Titles[0]));
		SET_SLOT(swOutput_KEY_WTHR, install("Title"), r_WTHR_NAME);
		if(useTimeStep) {
			PROTECT(r_WTHR_PERIOD = NEW_INTEGER(length(Periods)));
			for(i=0; i<length(Periods); i++) {
				INTEGER(r_WTHR_PERIOD)[i]=INTEGER(Periods)[i];
			}
			SET_SLOT(swOutput_KEY_WTHR, install("TimeStep"), r_WTHR_PERIOD);
			UNPROTECT(1);
		} else {
			PROTECT(r_WTHR_PERIOD = NEW_INTEGER(1));
			INTEGER(r_WTHR_PERIOD)[0]=INTEGER(Periods)[0];
			SET_SLOT(swOutput_KEY_WTHR, install("TimeStep"), r_WTHR_PERIOD);
			UNPROTECT(1);
		}
		if(periodUse[0][0]) {
			PROTECT(Rwthr_dy = allocMatrix(REALSXP,dy_nrow,Rwthr_columns));
			SET_SLOT(swOutput_KEY_WTHR, install("Day"), Rwthr_dy);
			UNPROTECT(1);
		}
		if(periodUse[0][1]) {
			PROTECT(Rwthr_wk = allocMatrix(REALSXP,wk_nrow,Rwthr_columns));
			SET_SLOT(swOutput_KEY_WTHR, install("Week"), Rwthr_wk);
			UNPROTECT(1);
		}
		if(periodUse[0][2]) {
			PROTECT(Rwthr_mo = allocMatrix(REALSXP,mo_nrow,Rwthr_columns));
			SET_SLOT(swOutput_KEY_WTHR, install("Month"), Rwthr_mo);
			UNPROTECT(1);
		}
		if(periodUse[0][3]) {
			PROTECT(Rwthr_yr = allocMatrix(REALSXP,yr_nrow,Rwthr_columns));
			SET_SLOT(swOutput_KEY_WTHR, install("Year"), Rwthr_yr);
			UNPROTECT(1);
		}
		PROTECT(r_WETDAY_COLUMNS = NEW_INTEGER(1));
		INTEGER(r_WETDAY_COLUMNS)[0]=Rwthr_columns;
		SET_SLOT(swOutput_KEY_WTHR, install("Columns"), r_WETDAY_COLUMNS);
		SET_SLOT(swOutput_Object, install(cSWoutput_Names[4]), swOutput_KEY_WTHR);
		UNPROTECT(3);
	}
	//TEMP
	if(use[1]) {
		if(debug) Rprintf("%s\n",cSWoutput_KEY_Titles[1]);
		PROTECT(swOutput_KEY_TEMP = NEW_OBJECT(swOutput_KEY));
		PROTECT(r_TEMP_NAME = NEW_STRING(1));
		SET_STRING_ELT(r_TEMP_NAME, 0, mkChar(cSWoutput_KEY_Titles[1]));
		SET_SLOT(swOutput_KEY_TEMP, install("Title"), r_TEMP_NAME);
		if (useTimeStep) {
			PROTECT(r_TEMP_PERIOD = NEW_INTEGER(length(Periods)));
			for(i=0; i<length(Periods); i++)
				INTEGER(r_TEMP_PERIOD)[i]=INTEGER(Periods)[i];
			SET_SLOT(swOutput_KEY_TEMP, install("TimeStep"), r_TEMP_PERIOD);
			UNPROTECT(1);
		} else {
			PROTECT(r_TEMP_PERIOD = NEW_INTEGER(1));
			INTEGER(r_TEMP_PERIOD)[0]=INTEGER(Periods)[1];
			SET_SLOT(swOutput_KEY_TEMP, install("TimeStep"), r_TEMP_PERIOD);
			UNPROTECT(1);
		}
		if(periodUse[1][0]) {
			if(debug) Rprintf("\tdy\n");
			PROTECT(Rtemp_dy = allocMatrix(REALSXP, dy_nrow, Rtemp_columns+2));
			PROTECT(Rtemp_names_dy = allocVector(VECSXP, 2));
			PROTECT(Rtemp_names_y_dy = allocVector(STRSXP, Rtemp_columns + 2));
			SET_STRING_ELT(Rtemp_names_y_dy, 0, mkChar("Year"));
			SET_STRING_ELT(Rtemp_names_y_dy, 1, mkChar("DOY"));
			for (i = 0; i < Rtemp_columns; i++)
				SET_STRING_ELT(Rtemp_names_y_dy, i + 2, mkChar(Ctemp_names[i]));
			SET_VECTOR_ELT(Rtemp_names_dy, 1, Rtemp_names_y_dy);
			setAttrib(Rtemp_dy, R_DimNamesSymbol, Rtemp_names_dy);
			SET_SLOT(swOutput_KEY_TEMP, install("Day"), Rtemp_dy);
			UNPROTECT(3);
		}
		if(periodUse[1][1]) {
			if(debug) Rprintf("\twk\n");
			PROTECT(Rtemp_wk = allocMatrix(REALSXP, wk_nrow, Rtemp_columns+2));
			PROTECT(Rtemp_names_wk = allocVector(VECSXP, 2));
			PROTECT(Rtemp_names_y_wk = allocVector(STRSXP, Rtemp_columns + 2));
			SET_STRING_ELT(Rtemp_names_y_wk, 0, mkChar("Year"));
			SET_STRING_ELT(Rtemp_names_y_wk, 1, mkChar("Week"));
			for (i = 0; i < Rtemp_columns; i++)
				SET_STRING_ELT(Rtemp_names_y_wk, i + 2, mkChar(Ctemp_names[i]));
			SET_VECTOR_ELT(Rtemp_names_wk, 1, Rtemp_names_y_wk);
			setAttrib(Rtemp_wk, R_DimNamesSymbol, Rtemp_names_wk);
			SET_SLOT(swOutput_KEY_TEMP, install("Week"), Rtemp_wk);
			UNPROTECT(3);
		}
		if(periodUse[1][2]) {
			if(debug) Rprintf("\tmo\n");
			PROTECT(Rtemp_mo = allocMatrix(REALSXP, mo_nrow, Rtemp_columns+2));
			PROTECT(Rtemp_names_mo = allocVector(VECSXP, 2));
			PROTECT(Rtemp_names_y_mo = allocVector(STRSXP, Rtemp_columns + 2));
			SET_STRING_ELT(Rtemp_names_y_mo, 0, mkChar("Year"));
			SET_STRING_ELT(Rtemp_names_y_mo, 1, mkChar("Month"));
			for (i = 0; i < Rtemp_columns; i++)
				SET_STRING_ELT(Rtemp_names_y_mo, i + 2, mkChar(Ctemp_names[i]));
			SET_VECTOR_ELT(Rtemp_names_mo, 1, Rtemp_names_y_mo);
			setAttrib(Rtemp_mo, R_DimNamesSymbol, Rtemp_names_mo);
			SET_SLOT(swOutput_KEY_TEMP, install("Month"), Rtemp_mo);
			UNPROTECT(3);
		}
		if(periodUse[1][3]) {
			if(debug) Rprintf("\tyr\n");
			PROTECT(Rtemp_yr = allocMatrix(REALSXP, yr_nrow, Rtemp_columns+1));
			PROTECT(Rtemp_names_yr = allocVector(VECSXP, 2));
			PROTECT(Rtemp_names_y_yr = allocVector(STRSXP, Rtemp_columns + 1));
			SET_STRING_ELT(Rtemp_names_y_yr, 0, mkChar("Year"));
			for (i = 0; i < Rtemp_columns; i++)
				SET_STRING_ELT(Rtemp_names_y_yr, i + 1, mkChar(Ctemp_names[i]));
			SET_VECTOR_ELT(Rtemp_names_yr, 1, Rtemp_names_y_yr);
			setAttrib(Rtemp_yr, R_DimNamesSymbol, Rtemp_names_yr);
			SET_SLOT(swOutput_KEY_TEMP, install("Year"), Rtemp_yr);
			UNPROTECT(3);
		}
		PROTECT(r_TEMP_COLUMNS = NEW_INTEGER(1));
		INTEGER(r_TEMP_COLUMNS)[0]=Rtemp_columns;
		SET_SLOT(swOutput_KEY_TEMP, install("Columns"), r_TEMP_COLUMNS);
		SET_SLOT(swOutput_Object, install(cSWoutput_Names[5]), swOutput_KEY_TEMP);
		UNPROTECT(3);
	}
	//PRECIP
	if(use[2]) {
		if(debug) Rprintf("%s\n",cSWoutput_KEY_Titles[2]);
		PROTECT(swOutput_KEY_PRECIP = NEW_OBJECT(swOutput_KEY));
		PROTECT(r_PRECIP_NAME = NEW_STRING(1));
		SET_STRING_ELT(r_PRECIP_NAME, 0, mkChar(cSWoutput_KEY_Titles[2]));
		SET_SLOT(swOutput_KEY_PRECIP, install("Title"), r_PRECIP_NAME);
		if (useTimeStep) {
			PROTECT(r_PRECIP_PERIOD = NEW_INTEGER(length(Periods)));
			for(i=0; i<length(Periods); i++)
				INTEGER(r_PRECIP_PERIOD)[i]=INTEGER(Periods)[i];
			SET_SLOT(swOutput_KEY_PRECIP, install("TimeStep"), r_PRECIP_PERIOD);
			UNPROTECT(1);
		} else {
			PROTECT(r_PRECIP_PERIOD = NEW_INTEGER(1));
			INTEGER(r_PRECIP_PERIOD)[0]=INTEGER(Periods)[2];
			SET_SLOT(swOutput_KEY_PRECIP, install("TimeStep"), r_PRECIP_PERIOD);
			UNPROTECT(1);
		}
		if(periodUse[2][0]) {
			if(debug) Rprintf("\tdy\n");
			PROTECT(Rprecip_dy = allocMatrix(REALSXP, dy_nrow, Rprecip_columns+2));
			PROTECT(Rprecip_names_dy = allocVector(VECSXP, 2));
			PROTECT(Rprecip_names_y_dy = allocVector(STRSXP, Rprecip_columns + 2));
			SET_STRING_ELT(Rprecip_names_y_dy, 0, mkChar("Year"));
			SET_STRING_ELT(Rprecip_names_y_dy, 1, mkChar("DOY"));
			for (i = 0; i < Rprecip_columns; i++)
				SET_STRING_ELT(Rprecip_names_y_dy, i + 2, mkChar(Cprecip_names[i]));
			SET_VECTOR_ELT(Rprecip_names_dy, 1, Rprecip_names_y_dy); //17
			setAttrib(Rprecip_dy, R_DimNamesSymbol, Rprecip_names_dy);
			SET_SLOT(swOutput_KEY_PRECIP, install("Day"), Rprecip_dy);
			UNPROTECT(3);
		}
		if(periodUse[2][1]) {
			if(debug) Rprintf("\twk\n");
			PROTECT(Rprecip_wk = allocMatrix(REALSXP, wk_nrow, Rprecip_columns+2));
			PROTECT(Rprecip_names_wk = allocVector(VECSXP, 2));
			PROTECT(Rprecip_names_y_wk = allocVector(STRSXP, Rprecip_columns + 2));
			SET_STRING_ELT(Rprecip_names_y_wk, 0, mkChar("Year"));
			SET_STRING_ELT(Rprecip_names_y_wk, 1, mkChar("Week"));
			for (i = 0; i < Rprecip_columns; i++)
				SET_STRING_ELT(Rprecip_names_y_wk, i + 2, mkChar(Cprecip_names[i]));
			SET_VECTOR_ELT(Rprecip_names_wk, 1, Rprecip_names_y_wk); //17
			setAttrib(Rprecip_wk, R_DimNamesSymbol, Rprecip_names_wk);
			SET_SLOT(swOutput_KEY_PRECIP, install("Week"), Rprecip_wk);
			UNPROTECT(3);
		}
		if(periodUse[2][2]) {
			if(debug) Rprintf("\tmo\n");
			PROTECT(Rprecip_mo = allocMatrix(REALSXP, mo_nrow, Rprecip_columns+2));
			PROTECT(Rprecip_names_mo = allocVector(VECSXP, 2));
			PROTECT(Rprecip_names_y_mo = allocVector(STRSXP, Rprecip_columns + 2));
			SET_STRING_ELT(Rprecip_names_y_mo, 0, mkChar("Year"));
			SET_STRING_ELT(Rprecip_names_y_mo, 1, mkChar("Month"));
			for (i = 0; i < Rprecip_columns; i++)
				SET_STRING_ELT(Rprecip_names_y_mo, i + 2, mkChar(Cprecip_names[i]));
			SET_VECTOR_ELT(Rprecip_names_mo, 1, Rprecip_names_y_mo); //17
			setAttrib(Rprecip_mo, R_DimNamesSymbol, Rprecip_names_mo);
			SET_SLOT(swOutput_KEY_PRECIP, install("Month"), Rprecip_mo);
			UNPROTECT(3);
		}
		if(periodUse[2][3]) {
			if(debug) Rprintf("\tyr\n");
			PROTECT(Rprecip_yr = allocMatrix(REALSXP, yr_nrow, Rprecip_columns+1));
			PROTECT(Rprecip_names_yr = allocVector(VECSXP, 2));
			PROTECT(Rprecip_names_y_yr = allocVector(STRSXP, Rprecip_columns + 1));
			SET_STRING_ELT(Rprecip_names_y_yr, 0, mkChar("Year"));
			for (i = 0; i < Rprecip_columns; i++)
				SET_STRING_ELT(Rprecip_names_y_yr, i + 1, mkChar(Cprecip_names[i]));
			SET_VECTOR_ELT(Rprecip_names_yr, 1, Rprecip_names_y_yr); //17
			setAttrib(Rprecip_yr, R_DimNamesSymbol, Rprecip_names_yr);
			SET_SLOT(swOutput_KEY_PRECIP, install("Year"), Rprecip_yr);
			UNPROTECT(3);
		}
		PROTECT(r_PRECIP_COLUMNS = NEW_INTEGER(1));
		INTEGER(r_PRECIP_COLUMNS)[0]=Rprecip_columns;
		SET_SLOT(swOutput_KEY_PRECIP, install("Columns"), r_PRECIP_COLUMNS);
		SET_SLOT(swOutput_Object, install(cSWoutput_Names[6]), swOutput_KEY_PRECIP);
		UNPROTECT(3);
	}
	//SoilInf
	if(use[3]) {
		if(debug) Rprintf("%s\n",cSWoutput_KEY_Titles[3]);
		PROTECT(swOutput_KEY_SOILINFILT = NEW_OBJECT(swOutput_KEY));
		PROTECT(r_SOILINFILT_NAME = NEW_STRING(1));
		SET_STRING_ELT(r_SOILINFILT_NAME, 0, mkChar(cSWoutput_KEY_Titles[3]));
		SET_SLOT(swOutput_KEY_SOILINFILT, install("Title"), r_SOILINFILT_NAME);
		if (useTimeStep) {
			PROTECT(r_SOILINFILT_PERIOD = NEW_INTEGER(length(Periods)));
			for(i=0; i<length(Periods); i++)
				INTEGER(r_SOILINFILT_PERIOD)[i]=INTEGER(Periods)[i];
			SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), r_SOILINFILT_PERIOD);
			UNPROTECT(1);
		} else {
			PROTECT(r_SOILINFILT_PERIOD = NEW_INTEGER(1));
			INTEGER(r_SOILINFILT_PERIOD)[0]=INTEGER(Periods)[3];
			SET_SLOT(swOutput_KEY_SOILINFILT, install("TimeStep"), r_SOILINFILT_PERIOD);
			UNPROTECT(1);
		}
		if(periodUse[3][0]) {
			if(debug) Rprintf("\tdy\n");
			PROTECT(Rinfiltration_dy = allocMatrix(REALSXP, dy_nrow, Rinfiltration_columns+2));
			PROTECT(Rinfiltration_names_dy = allocVector(VECSXP, 2));
			PROTECT(Rinfiltration_names_y_dy = allocVector(STRSXP, Rinfiltration_columns + 2));
			SET_STRING_ELT(Rinfiltration_names_y_dy, 0, mkChar("Year"));
			SET_STRING_ELT(Rinfiltration_names_y_dy, 1, mkChar("DOY"));
			SET_STRING_ELT(Rinfiltration_names_y_dy, 2, mkChar("soil_inf"));
			SET_VECTOR_ELT(Rinfiltration_names_dy, 1, Rinfiltration_names_y_dy);
			setAttrib(Rinfiltration_dy, R_DimNamesSymbol, Rinfiltration_names_dy);
			SET_SLOT(swOutput_KEY_SOILINFILT, install("Day"), Rinfiltration_dy);
			UNPROTECT(3);
		}
		if(periodUse[3][1]) {
			if(debug) Rprintf("\twk\n");
			PROTECT(Rinfiltration_wk = allocMatrix(REALSXP, wk_nrow, Rinfiltration_columns+2));
			PROTECT(Rinfiltration_names_wk = allocVector(VECSXP, 2));
			PROTECT(Rinfiltration_names_y_wk = allocVector(STRSXP, Rinfiltration_columns + 2));
			SET_STRING_ELT(Rinfiltration_names_y_wk, 0, mkChar("Year"));
			SET_STRING_ELT(Rinfiltration_names_y_wk, 1, mkChar("Week"));
			SET_STRING_ELT(Rinfiltration_names_y_wk, 2, mkChar("soil_inf"));
			SET_VECTOR_ELT(Rinfiltration_names_wk, 1, Rinfiltration_names_y_wk);
			setAttrib(Rinfiltration_wk, R_DimNamesSymbol, Rinfiltration_names_wk);
			SET_SLOT(swOutput_KEY_SOILINFILT, install("Week"), Rinfiltration_wk);
			UNPROTECT(3);
		}
		if(periodUse[3][2]) {
			if(debug) Rprintf("\tmo\n");
			PROTECT(Rinfiltration_mo = allocMatrix(REALSXP, mo_nrow, Rinfiltration_columns+2));
			PROTECT(Rinfiltration_names_mo = allocVector(VECSXP, 2));
			PROTECT(Rinfiltration_names_y_mo = allocVector(STRSXP, Rinfiltration_columns + 2));
			SET_STRING_ELT(Rinfiltration_names_y_mo, 0, mkChar("Year"));
			SET_STRING_ELT(Rinfiltration_names_y_mo, 1, mkChar("Month"));
			SET_STRING_ELT(Rinfiltration_names_y_mo, 2, mkChar("soil_inf"));
			SET_VECTOR_ELT(Rinfiltration_names_mo, 1, Rinfiltration_names_y_mo);
			setAttrib(Rinfiltration_mo, R_DimNamesSymbol, Rinfiltration_names_mo);
			SET_SLOT(swOutput_KEY_SOILINFILT, install("Month"), Rinfiltration_mo);
			UNPROTECT(3);
		}
		if(periodUse[3][3]) {
			if(debug) Rprintf("\tyr\n");
			PROTECT(Rinfiltration_yr = allocMatrix(REALSXP, yr_nrow, Rinfiltration_columns+1));
			PROTECT(Rinfiltration_names_yr = allocVector(VECSXP, 2));
			PROTECT(Rinfiltration_names_y_yr = allocVector(STRSXP, Rinfiltration_columns + 1));
			SET_STRING_ELT(Rinfiltration_names_y_yr, 0, mkChar("Year"));
			SET_STRING_ELT(Rinfiltration_names_y_yr, 1, mkChar("soil_inf"));
			SET_VECTOR_ELT(Rinfiltration_names_yr, 1, Rinfiltration_names_y_yr);
			setAttrib(Rinfiltration_yr, R_DimNamesSymbol, Rinfiltration_names_yr);
			SET_SLOT(swOutput_KEY_SOILINFILT, install("Year"), Rinfiltration_yr);
			UNPROTECT(3);
		}
		PROTECT(r_SOILINFILT_COLUMNS = NEW_INTEGER(1));
		INTEGER(r_SOILINFILT_COLUMNS)[0]=Rinfiltration_columns;
		SET_SLOT(swOutput_KEY_SOILINFILT, install("Columns"), r_SOILINFILT_COLUMNS);
		SET_SLOT(swOutput_Object, install(cSWoutput_Names[7]), swOutput_KEY_SOILINFILT);
		UNPROTECT(3);
	}
	//Runoff
	if(use[4]) {
		if(debug) Rprintf("%s\n",cSWoutput_KEY_Titles[4]);
		PROTECT(swOutput_KEY_RUNOFF = NEW_OBJECT(swOutput_KEY));
		PROTECT(r_RUNOFF_NAME = NEW_STRING(1));
		SET_STRING_ELT(r_RUNOFF_NAME, 0, mkChar(cSWoutput_KEY_Titles[4]));
		SET_SLOT(swOutput_KEY_RUNOFF, install("Title"), r_RUNOFF_NAME);
		if (useTimeStep) {
			PROTECT(r_RUNOFF_PERIOD = NEW_INTEGER(length(Periods)));
			for(i=0; i<length(Periods); i++)
				INTEGER(r_RUNOFF_PERIOD)[i]=INTEGER(Periods)[i];
			SET_SLOT(swOutput_KEY_RUNOFF, install("TimeStep"), r_RUNOFF_PERIOD);
			UNPROTECT(1);
		} else {
			PROTECT(r_RUNOFF_PERIOD = NEW_INTEGER(1));
			INTEGER(r_RUNOFF_PERIOD)[0]=INTEGER(Periods)[4];
			SET_SLOT(swOutput_KEY_RUNOFF, install("TimeStep"), r_RUNOFF_PERIOD);
			UNPROTECT(1);
		}
		if(periodUse[4][0]) {
			if(debug) Rprintf("\tdy\n");
			PROTECT(Rrunoff_dy = allocMatrix(REALSXP, dy_nrow, Rrunoff_columns+2));
			PROTECT(Rrunoff_names_dy = allocVector(VECSXP, 2));
			PROTECT(Rrunoff_names_y_dy = allocVector(STRSXP, Rrunoff_columns + 2));
			SET_STRING_ELT(Rrunoff_names_y_dy, 0, mkChar("Year"));
			SET_STRING_ELT(Rrunoff_names_y_dy, 1, mkChar("DOY"));
			for (i = 0; i < Rrunoff_columns; i++)
				SET_STRING_ELT(Rrunoff_names_y_dy, i + 2, mkChar(Crunoff_names[i]));
			SET_VECTOR_ELT(Rrunoff_names_dy, 1, Rrunoff_names_y_dy);
			setAttrib(Rrunoff_dy, R_DimNamesSymbol, Rrunoff_names_dy);
			SET_SLOT(swOutput_KEY_RUNOFF, install("Day"), Rrunoff_dy);
			UNPROTECT(3);
		}
		if(periodUse[4][1]) {
			if(debug) Rprintf("\twk\n");
			PROTECT(Rrunoff_wk = allocMatrix(REALSXP, wk_nrow, Rrunoff_columns+2));
			PROTECT(Rrunoff_names_wk = allocVector(VECSXP, 2));
			PROTECT(Rrunoff_names_y_wk = allocVector(STRSXP, Rrunoff_columns + 2));
			SET_STRING_ELT(Rrunoff_names_y_wk, 0, mkChar("Year"));
			SET_STRING_ELT(Rrunoff_names_y_wk, 1, mkChar("Week"));
			for (i = 0; i < Rrunoff_columns; i++)
				SET_STRING_ELT(Rrunoff_names_y_wk, i + 2, mkChar(Crunoff_names[i]));
			SET_VECTOR_ELT(Rrunoff_names_wk, 1, Rrunoff_names_y_wk);
			setAttrib(Rrunoff_wk, R_DimNamesSymbol, Rrunoff_names_wk);
			SET_SLOT(swOutput_KEY_RUNOFF, install("Week"), Rrunoff_wk);
			UNPROTECT(3);
		}
		if(periodUse[4][2]) {
			if(debug) Rprintf("\tmo\n");
			PROTECT(Rrunoff_mo = allocMatrix(REALSXP, mo_nrow, Rrunoff_columns+2));
			PROTECT(Rrunoff_names_mo = allocVector(VECSXP, 2));
			PROTECT(Rrunoff_names_y_mo = allocVector(STRSXP, Rrunoff_columns + 2));
			SET_STRING_ELT(Rrunoff_names_y_mo, 0, mkChar("Year"));
			SET_STRING_ELT(Rrunoff_names_y_mo, 1, mkChar("Month"));
			for (i = 0; i < Rrunoff_columns; i++)
				SET_STRING_ELT(Rrunoff_names_y_mo, i + 2, mkChar(Crunoff_names[i]));
			SET_VECTOR_ELT(Rrunoff_names_mo, 1, Rrunoff_names_y_mo);
			setAttrib(Rrunoff_mo, R_DimNamesSymbol, Rrunoff_names_mo);
			SET_SLOT(swOutput_KEY_RUNOFF, install("Month"), Rrunoff_mo);
			UNPROTECT(3);
		}
		if(periodUse[4][3]) {
			if(debug) Rprintf("\tyr\n");
			PROTECT(Rrunoff_yr = allocMatrix(REALSXP, yr_nrow, Rrunoff_columns+1));
			PROTECT(Rrunoff_names_yr = allocVector(VECSXP, 2));
			PROTECT(Rrunoff_names_y_yr = allocVector(STRSXP, Rrunoff_columns + 1));
			SET_STRING_ELT(Rrunoff_names_y_yr, 0, mkChar("Year"));
			for (i = 0; i < Rrunoff_columns; i++)
				SET_STRING_ELT(Rrunoff_names_y_yr, i + 1, mkChar(Crunoff_names[i]));
			SET_VECTOR_ELT(Rrunoff_names_yr, 1, Rrunoff_names_y_yr);
			setAttrib(Rrunoff_yr, R_DimNamesSymbol, Rrunoff_names_yr);
			SET_SLOT(swOutput_KEY_RUNOFF, install("Year"), Rrunoff_yr);
			UNPROTECT(3);
		}
		PROTECT(r_RUNOFF_COLUMNS = NEW_INTEGER(1));
		INTEGER(r_RUNOFF_COLUMNS)[0]=Rrunoff_columns;
		SET_SLOT(swOutput_KEY_RUNOFF, install("Columns"), r_RUNOFF_COLUMNS);
		SET_SLOT(swOutput_Object, install(cSWoutput_Names[8]), swOutput_KEY_RUNOFF);
		UNPROTECT(3);
	}
	//AllH2O - NOT USED
	if(use[5]) {
		if(debug) Rprintf("%s\n",cSWoutput_KEY_Titles[5]);
		PROTECT(swOutput_KEY_ALLH2O = NEW_OBJECT(swOutput_KEY));
		PROTECT(r_ALLH2O_NAME = NEW_STRING(1));
		SET_STRING_ELT(r_ALLH2O_NAME, 0, mkChar(cSWoutput_KEY_Titles[5]));
		SET_SLOT(swOutput_KEY_ALLH2O, install("Title"), r_ALLH2O_NAME);
		if (useTimeStep) {
			PROTECT(r_ALLH2O_PERIOD = NEW_INTEGER(length(Periods)));
			for(i=0; i<length(Periods); i++)
				INTEGER(r_ALLH2O_PERIOD)[i]=INTEGER(Periods)[i];
			SET_SLOT(swOutput_KEY_ALLH2O, install("TimeStep"), r_ALLH2O_PERIOD);
			UNPROTECT(1);
		} else {
			PROTECT(r_ALLH2O_PERIOD = NEW_INTEGER(1));
			INTEGER(r_ALLH2O_PERIOD)[0]=INTEGER(Periods)[5];
			SET_SLOT(swOutput_KEY_ALLH2O, install("TimeStep"), r_ALLH2O_PERIOD);
			UNPROTECT(1);
		}
		if(periodUse[5][0]) {
			PROTECT(RallH2O_dy = allocMatrix(REALSXP, dy_nrow, RallH2O_columns+2));
			SET_SLOT(swOutput_KEY_ALLH2O, install("Day"), RallH2O_dy);
			UNPROTECT(1);
		}
		if(periodUse[5][1]) {
			PROTECT(RallH2O_wk = allocMatrix(REALSXP, wk_nrow, RallH2O_columns+2));
			SET_SLOT(swOutput_KEY_ALLH2O, install("Week"), RallH2O_wk);
			UNPROTECT(1);
		}
		if(periodUse[5][2]) {
			PROTECT(RallH2O_mo = allocMatrix(REALSXP, mo_nrow, RallH2O_columns+2));
			SET_SLOT(swOutput_KEY_ALLH2O, install("Month"), RallH2O_mo);
			UNPROTECT(1);
		}
		if(periodUse[5][3]) {
			PROTECT(RallH2O_yr = allocMatrix(REALSXP, yr_nrow, RallH2O_columns+1));
			SET_SLOT(swOutput_KEY_ALLH2O, install("Year"), RallH2O_yr);
			UNPROTECT(1);
		}
		PROTECT(r_ALLH2O_COLUMNS = NEW_INTEGER(1));
		INTEGER(r_ALLH2O_COLUMNS)[0]=RallH2O_columns;
		SET_SLOT(swOutput_KEY_ALLH2O, install("Columns"), r_ALLH2O_COLUMNS);
		SET_SLOT(swOutput_Object, install(cSWoutput_Names[9]), swOutput_KEY_ALLH2O);
		UNPROTECT(3);
	}
	//VWCBulk
	if(use[6]) {
		if(debug) Rprintf("%s\n",cSWoutput_KEY_Titles[6]);
		PROTECT(swOutput_KEY_VWCBULK = NEW_OBJECT(swOutput_KEY));
		PROTECT(r_VWCBULK_NAME = NEW_STRING(1));
		SET_STRING_ELT(r_VWCBULK_NAME, 0, mkChar(cSWoutput_KEY_Titles[6]));
		SET_SLOT(swOutput_KEY_VWCBULK, install("Title"), r_VWCBULK_NAME);
		if (useTimeStep) {
			PROTECT(r_VWCBULK_PERIOD = NEW_INTEGER(length(Periods)));
			for(i=0; i<length(Periods); i++)
				INTEGER(r_VWCBULK_PERIOD)[i]=INTEGER(Periods)[i];
			SET_SLOT(swOutput_KEY_VWCBULK, install("TimeStep"), r_VWCBULK_PERIOD);
			UNPROTECT(1);
		} else {
			PROTECT(r_VWCBULK_PERIOD = NEW_INTEGER(1));
			INTEGER(r_VWCBULK_PERIOD)[0]=INTEGER(Periods)[6];
			SET_SLOT(swOutput_KEY_VWCBULK, install("TimeStep"), r_VWCBULK_PERIOD);
			UNPROTECT(1);
		}
		if(periodUse[6][0]) {
			if(debug) Rprintf("\tdy\n");
			PROTECT(RvwcBulk_dy = allocMatrix(REALSXP, dy_nrow, RvwcBulk_columns+2));
			PROTECT(RvwcBulk_names_dy = allocVector(VECSXP, 2));
			PROTECT(RvwcBulk_names_y_dy = allocVector(STRSXP, RvwcBulk_columns + 2));
			SET_STRING_ELT(RvwcBulk_names_y_dy, 0, mkChar("Year"));
			SET_STRING_ELT(RvwcBulk_names_y_dy, 1, mkChar("DOY"));
			for (i = 0; i < tLayers; i++)
				SET_STRING_ELT(RvwcBulk_names_y_dy, i + 2, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(RvwcBulk_names_dy, 1, RvwcBulk_names_y_dy);
			setAttrib(RvwcBulk_dy, R_DimNamesSymbol, RvwcBulk_names_dy);
			SET_SLOT(swOutput_KEY_VWCBULK, install("Day"), RvwcBulk_dy);
			UNPROTECT(3);
		}
		if(periodUse[6][1]) {
			if(debug) Rprintf("\twk\n");
			PROTECT(RvwcBulk_wk = allocMatrix(REALSXP, wk_nrow, RvwcBulk_columns+2));
			PROTECT(RvwcBulk_names_wk = allocVector(VECSXP, 2));
			PROTECT(RvwcBulk_names_y_wk = allocVector(STRSXP, RvwcBulk_columns + 2));
			SET_STRING_ELT(RvwcBulk_names_y_wk, 0, mkChar("Year"));
			SET_STRING_ELT(RvwcBulk_names_y_wk, 1, mkChar("Week"));
			for (i = 0; i < tLayers; i++)
				SET_STRING_ELT(RvwcBulk_names_y_wk, i + 2, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(RvwcBulk_names_wk, 1, RvwcBulk_names_y_wk);
			setAttrib(RvwcBulk_wk, R_DimNamesSymbol, RvwcBulk_names_wk);
			SET_SLOT(swOutput_KEY_VWCBULK, install("Week"), RvwcBulk_wk);
			UNPROTECT(3);
		}
		if(periodUse[6][2]) {
			if(debug) Rprintf("\tmo\n");
			PROTECT(RvwcBulk_mo = allocMatrix(REALSXP, mo_nrow, RvwcBulk_columns+2));
			PROTECT(RvwcBulk_names_mo = allocVector(VECSXP, 2));
			PROTECT(RvwcBulk_names_y_mo = allocVector(STRSXP, RvwcBulk_columns + 2));
			SET_STRING_ELT(RvwcBulk_names_y_mo, 0, mkChar("Year"));
			SET_STRING_ELT(RvwcBulk_names_y_mo, 1, mkChar("Month"));
			for (i = 0; i < tLayers; i++)
				SET_STRING_ELT(RvwcBulk_names_y_mo, i + 2, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(RvwcBulk_names_mo, 1, RvwcBulk_names_y_mo);
			setAttrib(RvwcBulk_mo, R_DimNamesSymbol, RvwcBulk_names_mo);
			SET_SLOT(swOutput_KEY_VWCBULK, install("Month"), RvwcBulk_mo);
			UNPROTECT(3);
		}
		if(periodUse[6][3]) {
			if(debug) Rprintf("\tyr\n");
			PROTECT(RvwcBulk_yr = allocMatrix(REALSXP, yr_nrow, RvwcBulk_columns+1));
			PROTECT(RvwcBulk_names_yr = allocVector(VECSXP, 2));
			PROTECT(RvwcBulk_names_y_yr = allocVector(STRSXP, RvwcBulk_columns + 1));
			SET_STRING_ELT(RvwcBulk_names_y_yr, 0, mkChar("Year"));
			for (i = 0; i < tLayers; i++)
				SET_STRING_ELT(RvwcBulk_names_y_yr, i + 1, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(RvwcBulk_names_yr, 1, RvwcBulk_names_y_yr);
			setAttrib(RvwcBulk_yr, R_DimNamesSymbol, RvwcBulk_names_yr);
			SET_SLOT(swOutput_KEY_VWCBULK, install("Year"), RvwcBulk_yr);
			UNPROTECT(3);
		}
		PROTECT(r_VWCBULK_COLUMNS = NEW_INTEGER(1));
		INTEGER(r_VWCBULK_COLUMNS)[0]=RvwcBulk_columns;
		SET_SLOT(swOutput_KEY_VWCBULK, install("Columns"), r_VWCBULK_COLUMNS);
		SET_SLOT(swOutput_Object, install(cSWoutput_Names[10]), swOutput_KEY_VWCBULK);
		UNPROTECT(3);
	}
	//VWCMatrix
	if(use[7]) {
		if(debug) Rprintf("%s\n",cSWoutput_KEY_Titles[7]);
		PROTECT(swOutput_KEY_VWCMATRIC = NEW_OBJECT(swOutput_KEY));
		PROTECT(r_VWCMATRIC_NAME = NEW_STRING(1));
		SET_STRING_ELT(r_VWCMATRIC_NAME, 0, mkChar(cSWoutput_KEY_Titles[7]));
		SET_SLOT(swOutput_KEY_VWCMATRIC, install("Title"), r_VWCMATRIC_NAME);
		if (useTimeStep) {
			PROTECT(r_VWCMATRIC_PERIOD = NEW_INTEGER(length(Periods)));
			for(i=0; i<length(Periods); i++)
				INTEGER(r_VWCMATRIC_PERIOD)[i]=INTEGER(Periods)[i];
			SET_SLOT(swOutput_KEY_VWCMATRIC, install("TimeStep"), r_VWCMATRIC_PERIOD);
			UNPROTECT(1);
		} else {
			PROTECT(r_VWCMATRIC_PERIOD = NEW_INTEGER(1));
			INTEGER(r_VWCMATRIC_PERIOD)[0]=INTEGER(Periods)[7];
			SET_SLOT(swOutput_KEY_VWCMATRIC, install("TimeStep"), r_VWCMATRIC_PERIOD);
			UNPROTECT(1);
		}
		if(periodUse[7][0]) {
			if(debug) Rprintf("\tdy\n");
			PROTECT(RvwcMatric_dy = allocMatrix(REALSXP, dy_nrow, RvwcMatric_columns+2));
			PROTECT(RvwcMatric_names_dy = allocVector(VECSXP, 2));
			PROTECT(RvwcMatric_names_y_dy = allocVector(STRSXP, RvwcMatric_columns + 2));
			SET_STRING_ELT(RvwcMatric_names_y_dy, 0, mkChar("Year"));
			SET_STRING_ELT(RvwcMatric_names_y_dy, 1, mkChar("DOY"));
			for (i = 0; i < tLayers; i++)
				SET_STRING_ELT(RvwcMatric_names_y_dy, i + 2, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(RvwcMatric_names_dy, 1, RvwcMatric_names_y_dy);
			setAttrib(RvwcMatric_dy, R_DimNamesSymbol, RvwcMatric_names_dy);
			SET_SLOT(swOutput_KEY_VWCMATRIC, install("Day"), RvwcMatric_dy);
			UNPROTECT(3);
		}
		if(periodUse[7][1]) {
			if(debug) Rprintf("\twk\n");
			PROTECT(RvwcMatric_wk = allocMatrix(REALSXP, wk_nrow, RvwcMatric_columns+2));
			PROTECT(RvwcMatric_names_wk = allocVector(VECSXP, 2));
			PROTECT(RvwcMatric_names_y_wk = allocVector(STRSXP, RvwcMatric_columns + 2));
			SET_STRING_ELT(RvwcMatric_names_y_wk, 0, mkChar("Year"));
			SET_STRING_ELT(RvwcMatric_names_y_wk, 1, mkChar("Week"));
			for (i = 0; i < tLayers; i++)
				SET_STRING_ELT(RvwcMatric_names_y_wk, i + 2, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(RvwcMatric_names_wk, 1, RvwcMatric_names_y_wk);
			setAttrib(RvwcMatric_wk, R_DimNamesSymbol, RvwcMatric_names_wk);
			SET_SLOT(swOutput_KEY_VWCMATRIC, install("Week"), RvwcMatric_wk);
			UNPROTECT(3);
		}
		if(periodUse[7][2]) {
			if(debug) Rprintf("\tmo\n");
			PROTECT(RvwcMatric_mo = allocMatrix(REALSXP, mo_nrow, RvwcMatric_columns+2));
			PROTECT(RvwcMatric_names_mo = allocVector(VECSXP, 2));
			PROTECT(RvwcMatric_names_y_mo = allocVector(STRSXP, RvwcMatric_columns + 2));
			SET_STRING_ELT(RvwcMatric_names_y_mo, 0, mkChar("Year"));
			SET_STRING_ELT(RvwcMatric_names_y_mo, 1, mkChar("Month"));
			for (i = 0; i < tLayers; i++)
				SET_STRING_ELT(RvwcMatric_names_y_mo, i + 2, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(RvwcMatric_names_mo, 1, RvwcMatric_names_y_mo);
			setAttrib(RvwcMatric_mo, R_DimNamesSymbol, RvwcMatric_names_mo);
			SET_SLOT(swOutput_KEY_VWCMATRIC, install("Month"), RvwcMatric_mo);
			UNPROTECT(3);
		}
		if(periodUse[7][3]) {
			if(debug) Rprintf("\tyr\n");
			PROTECT(RvwcMatric_yr = allocMatrix(REALSXP, yr_nrow, RvwcMatric_columns+1));
			PROTECT(RvwcMatric_names_yr = allocVector(VECSXP, 2));
			PROTECT(RvwcMatric_names_y_yr = allocVector(STRSXP, RvwcMatric_columns + 1));
			SET_STRING_ELT(RvwcMatric_names_y_yr, 0, mkChar("Year"));
			for (i = 0; i < tLayers; i++)
				SET_STRING_ELT(RvwcMatric_names_y_yr, i + 1, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(RvwcMatric_names_yr, 1, RvwcMatric_names_y_yr);
			setAttrib(RvwcMatric_yr, R_DimNamesSymbol, RvwcMatric_names_yr);
			SET_SLOT(swOutput_KEY_VWCMATRIC, install("Year"), RvwcMatric_yr);
			UNPROTECT(3);
		}
		PROTECT(r_VWCMATRIC_COLUMNS = NEW_INTEGER(1));
		INTEGER(r_VWCMATRIC_COLUMNS)[0]=RvwcMatric_columns;
		SET_SLOT(swOutput_KEY_VWCMATRIC, install("Columns"), r_VWCMATRIC_COLUMNS);
		SET_SLOT(swOutput_Object, install(cSWoutput_Names[11]), swOutput_KEY_VWCMATRIC);
		UNPROTECT(3);
	}
	//SWCBulk
	if(use[8]) {
		if(debug) Rprintf("%s\n",cSWoutput_KEY_Titles[8]);
		PROTECT(swOutput_KEY_SWCBULK = NEW_OBJECT(swOutput_KEY));
		PROTECT(r_SWCBULK_NAME = NEW_STRING(1));
		SET_STRING_ELT(r_SWCBULK_NAME, 0, mkChar(cSWoutput_KEY_Titles[8]));
		SET_SLOT(swOutput_KEY_SWCBULK, install("Title"), r_SWCBULK_NAME);
		if (useTimeStep) {
			PROTECT(r_SWCBULK_PERIOD = NEW_INTEGER(length(Periods)));
			for(i=0; i<length(Periods); i++)
				INTEGER(r_SWCBULK_PERIOD)[i]=INTEGER(Periods)[i];
			SET_SLOT(swOutput_KEY_SWCBULK, install("TimeStep"), r_SWCBULK_PERIOD);
			UNPROTECT(1);
		} else {
			PROTECT(r_SWCBULK_PERIOD = NEW_INTEGER(1));
			INTEGER(r_SWCBULK_PERIOD)[0]=INTEGER(Periods)[8];
			SET_SLOT(swOutput_KEY_SWCBULK, install("TimeStep"), r_SWCBULK_PERIOD);
			UNPROTECT(1);
		}
		if(periodUse[8][0]) {
			if(debug) Rprintf("\tdy\n");
			PROTECT(RswcBulk_dy = allocMatrix(REALSXP, dy_nrow, RswcBulk_columns+2));
			PROTECT(RswcBulk_names_dy = allocVector(VECSXP, 2));
			PROTECT(RswcBulk_names_y_dy = allocVector(STRSXP, RswcBulk_columns + 2));
			SET_STRING_ELT(RswcBulk_names_y_dy, 0, mkChar("Year"));
			SET_STRING_ELT(RswcBulk_names_y_dy, 1, mkChar("DOY"));
			for (i = 0; i < tLayers; i++)
				SET_STRING_ELT(RswcBulk_names_y_dy, i + 2, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(RswcBulk_names_dy, 1, RswcBulk_names_y_dy);
			setAttrib(RswcBulk_dy, R_DimNamesSymbol, RswcBulk_names_dy);
			SET_SLOT(swOutput_KEY_SWCBULK, install("Day"), RswcBulk_dy);
			UNPROTECT(3);
		}
		if(periodUse[8][1]) {
			if(debug) Rprintf("\twk\n");
			PROTECT(RswcBulk_wk = allocMatrix(REALSXP, wk_nrow, RswcBulk_columns+2));
			PROTECT(RswcBulk_names_wk = allocVector(VECSXP, 2));
			PROTECT(RswcBulk_names_y_wk = allocVector(STRSXP, RswcBulk_columns + 2));
			SET_STRING_ELT(RswcBulk_names_y_wk, 0, mkChar("Year"));
			SET_STRING_ELT(RswcBulk_names_y_wk, 1, mkChar("Week"));
			for (i = 0; i < tLayers; i++)
				SET_STRING_ELT(RswcBulk_names_y_wk, i + 2, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(RswcBulk_names_wk, 1, RswcBulk_names_y_wk);
			setAttrib(RswcBulk_wk, R_DimNamesSymbol, RswcBulk_names_wk);
			SET_SLOT(swOutput_KEY_SWCBULK, install("Week"), RswcBulk_wk);
			UNPROTECT(3);
		}
		if(periodUse[8][2]) {
			if(debug) Rprintf("\tmo\n");
			PROTECT(RswcBulk_mo = allocMatrix(REALSXP, mo_nrow, RswcBulk_columns+2));
			PROTECT(RswcBulk_names_mo = allocVector(VECSXP, 2));
			PROTECT(RswcBulk_names_y_mo = allocVector(STRSXP, RswcBulk_columns + 2));
			SET_STRING_ELT(RswcBulk_names_y_mo, 0, mkChar("Year"));
			SET_STRING_ELT(RswcBulk_names_y_mo, 1, mkChar("Month"));
			for (i = 0; i < tLayers; i++)
				SET_STRING_ELT(RswcBulk_names_y_mo, i + 2, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(RswcBulk_names_mo, 1, RswcBulk_names_y_mo);
			setAttrib(RswcBulk_mo, R_DimNamesSymbol, RswcBulk_names_mo);
			SET_SLOT(swOutput_KEY_SWCBULK, install("Month"), RswcBulk_mo);
			UNPROTECT(3);
		}
		if(periodUse[8][3]) {
			if(debug) Rprintf("\tyr\n");
			PROTECT(RswcBulk_yr = allocMatrix(REALSXP, yr_nrow, RswcBulk_columns+1));
			PROTECT(RswcBulk_names_yr = allocVector(VECSXP, 2));
			PROTECT(RswcBulk_names_y_yr = allocVector(STRSXP, RswcBulk_columns + 1));
			SET_STRING_ELT(RswcBulk_names_y_yr, 0, mkChar("Year"));
			for (i = 0; i < tLayers; i++)
				SET_STRING_ELT(RswcBulk_names_y_yr, i + 1, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(RswcBulk_names_yr, 1, RswcBulk_names_y_yr);
			setAttrib(RswcBulk_yr, R_DimNamesSymbol, RswcBulk_names_yr);
			SET_SLOT(swOutput_KEY_SWCBULK, install("Year"), RswcBulk_yr);
			UNPROTECT(3);
		}
		PROTECT(r_SWCBULK_COLUMNS = NEW_INTEGER(1));
		INTEGER(r_SWCBULK_COLUMNS)[0]=RswcBulk_columns;
		SET_SLOT(swOutput_KEY_SWCBULK, install("Columns"), r_SWCBULK_COLUMNS);
		SET_SLOT(swOutput_Object, install(cSWoutput_Names[12]), swOutput_KEY_SWCBULK);
		UNPROTECT(3);
	}
	//SWABULK
	if(use[9]) {
		if(debug) Rprintf("%s\n",cSWoutput_KEY_Titles[9]);
		PROTECT(swOutput_KEY_SWABULK = NEW_OBJECT(swOutput_KEY));
		PROTECT(r_SWABULK_NAME = NEW_STRING(1));
		SET_STRING_ELT(r_SWABULK_NAME, 0, mkChar(cSWoutput_KEY_Titles[9]));
		SET_SLOT(swOutput_KEY_SWABULK, install("Title"), r_SWABULK_NAME);
		if (useTimeStep) {
			PROTECT(r_SWABULK_PERIOD = NEW_INTEGER(length(Periods)));
			for(i=0; i<length(Periods); i++)
				INTEGER(r_SWABULK_PERIOD)[i]=INTEGER(Periods)[i];
			SET_SLOT(swOutput_KEY_SWABULK, install("TimeStep"), r_SWABULK_PERIOD);
			UNPROTECT(1);
		} else {
			PROTECT(r_SWABULK_PERIOD = NEW_INTEGER(1));
			INTEGER(r_SWABULK_PERIOD)[0]=INTEGER(Periods)[10];
			SET_SLOT(swOutput_KEY_SWABULK, install("TimeStep"), r_SWABULK_PERIOD);
			UNPROTECT(1);
		}
		if(periodUse[9][0]) {
			if(debug) Rprintf("\tdy\n");
			PROTECT(RswaBulk_dy = allocMatrix(REALSXP, dy_nrow, RswaBulk_columns+2));
			PROTECT(RswaBulk_names_dy = allocVector(VECSXP, 2));
			PROTECT(RswaBulk_names_y_dy = allocVector(STRSXP, RswaBulk_columns + 2));
			SET_STRING_ELT(RswaBulk_names_y_dy, 0, mkChar("Year"));
			SET_STRING_ELT(RswaBulk_names_y_dy, 1, mkChar("DOY"));
			for (i = 0; i < tLayers; i++)
				SET_STRING_ELT(RswaBulk_names_y_dy, i + 2, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(RswaBulk_names_dy, 1, RswaBulk_names_y_dy);
			setAttrib(RswaBulk_dy, R_DimNamesSymbol, RswaBulk_names_dy);
			SET_SLOT(swOutput_KEY_SWABULK, install("Day"), RswaBulk_dy);
			UNPROTECT(3);
		}
		if(periodUse[9][1]) {
			if(debug) Rprintf("\twk\n");
			PROTECT(RswaBulk_wk = allocMatrix(REALSXP, wk_nrow, RswaBulk_columns+2));
			PROTECT(RswaBulk_names_wk = allocVector(VECSXP, 2));
			PROTECT(RswaBulk_names_y_wk = allocVector(STRSXP, RswaBulk_columns + 2));
			SET_STRING_ELT(RswaBulk_names_y_wk, 0, mkChar("Year"));
			SET_STRING_ELT(RswaBulk_names_y_wk, 1, mkChar("Week"));
			for (i = 0; i < tLayers; i++)
				SET_STRING_ELT(RswaBulk_names_y_wk, i + 2, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(RswaBulk_names_wk, 1, RswaBulk_names_y_wk);
			setAttrib(RswaBulk_wk, R_DimNamesSymbol, RswaBulk_names_wk);
			SET_SLOT(swOutput_KEY_SWABULK, install("Week"), RswaBulk_wk);
			UNPROTECT(3);
		}
		if(periodUse[9][2]) {
			if(debug) Rprintf("\tmo\n");
			PROTECT(RswaBulk_mo = allocMatrix(REALSXP, mo_nrow, RswaBulk_columns+2));
			PROTECT(RswaBulk_names_mo = allocVector(VECSXP, 2));
			PROTECT(RswaBulk_names_y_mo = allocVector(STRSXP, RswaBulk_columns + 2));
			SET_STRING_ELT(RswaBulk_names_y_mo, 0, mkChar("Year"));
			SET_STRING_ELT(RswaBulk_names_y_mo, 1, mkChar("Month"));
			for (i = 0; i < tLayers; i++)
				SET_STRING_ELT(RswaBulk_names_y_mo, i + 2, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(RswaBulk_names_mo, 1, RswaBulk_names_y_mo);
			setAttrib(RswaBulk_mo, R_DimNamesSymbol, RswaBulk_names_mo);
			SET_SLOT(swOutput_KEY_SWABULK, install("Month"), RswaBulk_mo);
			UNPROTECT(3);
		}
		if(periodUse[9][3]) {
			if(debug) Rprintf("\twk\n");
			PROTECT(RswaBulk_yr = allocMatrix(REALSXP, yr_nrow, RswaBulk_columns+1));
			PROTECT(RswaBulk_names_yr = allocVector(VECSXP, 2));
			PROTECT(RswaBulk_names_y_yr = allocVector(STRSXP, RswaBulk_columns + 1));
			SET_STRING_ELT(RswaBulk_names_y_yr, 0, mkChar("Year"));
			for (i = 0; i < tLayers; i++)
				SET_STRING_ELT(RswaBulk_names_y_yr, i + 1, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(RswaBulk_names_yr, 1, RswaBulk_names_y_yr);
			setAttrib(RswaBulk_yr, R_DimNamesSymbol, RswaBulk_names_yr);
			SET_SLOT(swOutput_KEY_SWABULK, install("Year"), RswaBulk_yr);
			UNPROTECT(3);
		}
		PROTECT(r_SWABULK_COLUMNS = NEW_INTEGER(1));
		INTEGER(r_SWABULK_COLUMNS)[0]=RswaBulk_columns;
		SET_SLOT(swOutput_KEY_SWABULK, install("Columns"), r_SWABULK_COLUMNS);
		SET_SLOT(swOutput_Object, install(cSWoutput_Names[13]), swOutput_KEY_SWABULK);
		UNPROTECT(3);
	}
	//SWAMATRIC
	if(use[10]) {
		if(debug) Rprintf("%s\n",cSWoutput_KEY_Titles[10]);
		PROTECT(swOutput_KEY_SWAMATRIC = NEW_OBJECT(swOutput_KEY));
		PROTECT(r_SWAMATRIC_NAME = NEW_STRING(1));
		SET_STRING_ELT(r_SWAMATRIC_NAME, 0, mkChar(cSWoutput_KEY_Titles[10]));
		SET_SLOT(swOutput_KEY_SWAMATRIC, install("Title"), r_SWAMATRIC_NAME);
		if (useTimeStep) {
			PROTECT(r_SWAMATRIC_PERIOD = NEW_INTEGER(length(Periods)));
			for(i=0; i<length(Periods); i++)
				INTEGER(r_SWAMATRIC_PERIOD)[i]=INTEGER(Periods)[i];
			SET_SLOT(swOutput_KEY_SWAMATRIC, install("TimeStep"), r_SWAMATRIC_PERIOD);
			UNPROTECT(1);
		} else {
			PROTECT(r_SWAMATRIC_PERIOD = NEW_INTEGER(1));
			INTEGER(r_SWAMATRIC_PERIOD)[0]=INTEGER(Periods)[11];
			SET_SLOT(swOutput_KEY_SWAMATRIC, install("TimeStep"), r_SWAMATRIC_PERIOD);
			UNPROTECT(1);
		}
		if(periodUse[10][0]) {
			if(debug) Rprintf("\tdy\n");
			PROTECT(RswaMatric_dy = allocMatrix(REALSXP, dy_nrow, RswaMatric_columns+2));
			PROTECT(RswaMatric_names_dy = allocVector(VECSXP, 2));
			PROTECT(RswaMatric_names_y_dy = allocVector(STRSXP, RswaMatric_columns + 2));
			SET_STRING_ELT(RswaMatric_names_y_dy, 0, mkChar("Year"));
			SET_STRING_ELT(RswaMatric_names_y_dy, 1, mkChar("DOY"));
			for (i = 0; i < tLayers; i++)
				SET_STRING_ELT(RswaMatric_names_y_dy, i + 2, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(RswaMatric_names_dy, 1, RswaMatric_names_y_dy);
			setAttrib(RswaMatric_dy, R_DimNamesSymbol, RswaMatric_names_dy);
			SET_SLOT(swOutput_KEY_SWAMATRIC, install("Day"), RswaMatric_dy);
			UNPROTECT(3);
		}
		if(periodUse[10][1]) {
			if(debug) Rprintf("\twk\n");
			PROTECT(RswaMatric_wk = allocMatrix(REALSXP, wk_nrow, RswaMatric_columns+2));
			PROTECT(RswaMatric_names_wk = allocVector(VECSXP, 2));
			PROTECT(RswaMatric_names_y_wk = allocVector(STRSXP, RswaMatric_columns + 2));
			SET_STRING_ELT(RswaMatric_names_y_wk, 0, mkChar("Year"));
			SET_STRING_ELT(RswaMatric_names_y_wk, 1, mkChar("Week"));
			for (i = 0; i < tLayers; i++)
				SET_STRING_ELT(RswaMatric_names_y_wk, i + 2, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(RswaMatric_names_wk, 1, RswaMatric_names_y_wk);
			setAttrib(RswaMatric_wk, R_DimNamesSymbol, RswaMatric_names_wk);
			SET_SLOT(swOutput_KEY_SWAMATRIC, install("Week"), RswaMatric_wk);
			UNPROTECT(3);
		}
		if(periodUse[10][2]) {
			if(debug) Rprintf("\tmo\n");
			PROTECT(RswaMatric_mo = allocMatrix(REALSXP, mo_nrow, RswaMatric_columns+2));
			PROTECT(RswaMatric_names_mo = allocVector(VECSXP, 2));
			PROTECT(RswaMatric_names_y_mo = allocVector(STRSXP, RswaMatric_columns + 2));
			SET_STRING_ELT(RswaMatric_names_y_mo, 0, mkChar("Year"));
			SET_STRING_ELT(RswaMatric_names_y_mo, 1, mkChar("Month"));
			for (i = 0; i < tLayers; i++)
				SET_STRING_ELT(RswaMatric_names_y_mo, i + 2, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(RswaMatric_names_mo, 1, RswaMatric_names_y_mo);
			setAttrib(RswaMatric_mo, R_DimNamesSymbol, RswaMatric_names_mo);
			SET_SLOT(swOutput_KEY_SWAMATRIC, install("Month"), RswaMatric_mo);
			UNPROTECT(3);
		}
		if(periodUse[10][3]) {
			if(debug) Rprintf("\tyr\n");
			PROTECT(RswaMatric_yr = allocMatrix(REALSXP, yr_nrow, RswaMatric_columns+1));
			PROTECT(RswaMatric_names_yr = allocVector(VECSXP, 2));
			PROTECT(RswaMatric_names_y_yr = allocVector(STRSXP, RswaMatric_columns + 1));
			SET_STRING_ELT(RswaMatric_names_y_yr, 0, mkChar("Year"));
			for (i = 0; i < tLayers; i++)
				SET_STRING_ELT(RswaMatric_names_y_yr, i + 1, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(RswaMatric_names_yr, 1, RswaMatric_names_y_yr);
			setAttrib(RswaMatric_yr, R_DimNamesSymbol, RswaMatric_names_yr);
			SET_SLOT(swOutput_KEY_SWAMATRIC, install("Year"), RswaMatric_yr);
			UNPROTECT(3);
		}
		PROTECT(r_SWAMATRIC_COLUMNS = NEW_INTEGER(1));
		INTEGER(r_SWAMATRIC_COLUMNS)[0]=RswaMatric_columns;
		SET_SLOT(swOutput_KEY_SWAMATRIC, install("Columns"), r_SWAMATRIC_COLUMNS);
		SET_SLOT(swOutput_Object, install(cSWoutput_Names[14]), swOutput_KEY_SWAMATRIC);
		UNPROTECT(3);
	}
	//SWP Matric
	if(use[11]) {
		if(debug) Rprintf("%s\n",cSWoutput_KEY_Titles[11]);
		PROTECT(swOutput_KEY_SWPMATRIC = NEW_OBJECT(swOutput_KEY));
		PROTECT(r_SWPMATRIC_NAME = NEW_STRING(1));
		SET_STRING_ELT(r_SWPMATRIC_NAME, 0, mkChar(cSWoutput_KEY_Titles[11]));
		SET_SLOT(swOutput_KEY_SWPMATRIC, install("Title"), r_SWPMATRIC_NAME);
		if (useTimeStep) {
			if(debug) Rprintf("\tUseTimeStep - %d\n", length(Periods));
			PROTECT(r_SWPMATRIC_PERIOD = NEW_INTEGER(length(Periods)));
			if(debug) Rprintf("\tr_SWPMATRIC_PERIOD - %d\n", length(r_SWPMATRIC_PERIOD));
			for(i=0; i<length(Periods); i++)
				INTEGER(r_SWPMATRIC_PERIOD)[i]=INTEGER(Periods)[i];
			SET_SLOT(swOutput_KEY_SWPMATRIC, install("TimeStep"), r_SWPMATRIC_PERIOD);
			UNPROTECT(1);
		} else {
			if(debug) Rprintf("\t ! UseTimeStep\n");
			PROTECT(r_SWPMATRIC_PERIOD = NEW_INTEGER(1));
			INTEGER(r_SWPMATRIC_PERIOD)[0]=INTEGER(Periods)[9];
			SET_SLOT(swOutput_KEY_SWPMATRIC, install("TimeStep"), r_SWPMATRIC_PERIOD);
			UNPROTECT(1);
		}
		if(periodUse[11][0]) {
			if(debug) Rprintf("\tdy\n");
			PROTECT(RswpMatric_dy = allocMatrix(REALSXP, dy_nrow, RswpMatric_columns+2));
			PROTECT(RswpMatric_names_dy = allocVector(VECSXP, 2));
			PROTECT(RswpMatric_names_y_dy = allocVector(STRSXP, RswpMatric_columns + 2));
			SET_STRING_ELT(RswpMatric_names_y_dy, 0, mkChar("Year"));
			SET_STRING_ELT(RswpMatric_names_y_dy, 1, mkChar("DOY"));
			for (i = 0; i < tLayers; i++)
				SET_STRING_ELT(RswpMatric_names_y_dy, i + 2, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(RswpMatric_names_dy, 1, RswpMatric_names_y_dy);
			setAttrib(RswpMatric_dy, R_DimNamesSymbol, RswpMatric_names_dy);
			SET_SLOT(swOutput_KEY_SWPMATRIC, install("Day"), RswpMatric_dy);
			UNPROTECT(3);
		}
		if(periodUse[11][1]) {
			if(debug) Rprintf("\twk\n");
			PROTECT(RswpMatric_wk = allocMatrix(REALSXP, wk_nrow, RswpMatric_columns+2));
			PROTECT(RswpMatric_names_wk = allocVector(VECSXP, 2));
			PROTECT(RswpMatric_names_y_wk = allocVector(STRSXP, RswpMatric_columns + 2));
			SET_STRING_ELT(RswpMatric_names_y_wk, 0, mkChar("Year"));
			SET_STRING_ELT(RswpMatric_names_y_wk, 1, mkChar("Week"));
			for (i = 0; i < tLayers; i++)
				SET_STRING_ELT(RswpMatric_names_y_wk, i + 2, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(RswpMatric_names_wk, 1, RswpMatric_names_y_wk);
			setAttrib(RswpMatric_wk, R_DimNamesSymbol, RswpMatric_names_wk);
			SET_SLOT(swOutput_KEY_SWPMATRIC, install("Week"), RswpMatric_wk);
			UNPROTECT(3);
		}
		if(periodUse[11][2]) {
			if(debug) Rprintf("\tmo\n");
			PROTECT(RswpMatric_mo = allocMatrix(REALSXP, mo_nrow, RswpMatric_columns+2));
			PROTECT(RswpMatric_names_mo = allocVector(VECSXP, 2));
			PROTECT(RswpMatric_names_y_mo = allocVector(STRSXP, RswpMatric_columns + 2));
			SET_STRING_ELT(RswpMatric_names_y_mo, 0, mkChar("Year"));
			SET_STRING_ELT(RswpMatric_names_y_mo, 1, mkChar("Month"));
			for (i = 0; i < tLayers; i++)
				SET_STRING_ELT(RswpMatric_names_y_mo, i + 2, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(RswpMatric_names_mo, 1, RswpMatric_names_y_mo);
			setAttrib(RswpMatric_mo, R_DimNamesSymbol, RswpMatric_names_mo);
			SET_SLOT(swOutput_KEY_SWPMATRIC, install("Month"), RswpMatric_mo);
			UNPROTECT(3);
		}
		if(periodUse[11][3]) {
			if(debug) Rprintf("\tyr\n");
			PROTECT(RswpMatric_yr = allocMatrix(REALSXP, yr_nrow, RswpMatric_columns+1));
			PROTECT(RswpMatric_names_yr = allocVector(VECSXP, 2));
			PROTECT(RswpMatric_names_y_yr = allocVector(STRSXP, RswpMatric_columns + 1));
			SET_STRING_ELT(RswpMatric_names_y_yr, 0, mkChar("Year"));
			for (i = 0; i < tLayers; i++)
				SET_STRING_ELT(RswpMatric_names_y_yr, i + 1, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(RswpMatric_names_yr, 1, RswpMatric_names_y_yr);
			setAttrib(RswpMatric_yr, R_DimNamesSymbol, RswpMatric_names_yr);
			SET_SLOT(swOutput_KEY_SWPMATRIC, install("Year"), RswpMatric_yr);
			UNPROTECT(3);
		}
		PROTECT(r_SWPMATRIC_COLUMNS = NEW_INTEGER(1));
		INTEGER(r_SWPMATRIC_COLUMNS)[0]=RswpMatric_columns;
		SET_SLOT(swOutput_KEY_SWPMATRIC, install("Columns"), r_SWPMATRIC_COLUMNS);
		SET_SLOT(swOutput_Object, install(cSWoutput_Names[15]), swOutput_KEY_SWPMATRIC);
		UNPROTECT(3);
	}
	//SURFACE WATER
	if(use[12]) {
		if(debug) Rprintf("%s\n",cSWoutput_KEY_Titles[12]);
		PROTECT(swOutput_KEY_SURFACEWATER = NEW_OBJECT(swOutput_KEY));
		PROTECT(r_SURFACEWATER_NAME = NEW_STRING(1));
		SET_STRING_ELT(r_SURFACEWATER_NAME, 0, mkChar(cSWoutput_KEY_Titles[12]));
		SET_SLOT(swOutput_KEY_SURFACEWATER, install("Title"), r_SURFACEWATER_NAME);
		if (useTimeStep) {
			PROTECT(r_SURFACEWATER_PERIOD = NEW_INTEGER(length(Periods)));
			for(i=0; i<length(Periods); i++)
				INTEGER(r_SURFACEWATER_PERIOD)[i]=INTEGER(Periods)[i];
			SET_SLOT(swOutput_KEY_SURFACEWATER, install("TimeStep"), r_SURFACEWATER_PERIOD);
			UNPROTECT(1);
		} else {
			PROTECT(r_SURFACEWATER_PERIOD = NEW_INTEGER(1));
			INTEGER(r_SURFACEWATER_PERIOD)[0]=INTEGER(Periods)[12];
			SET_SLOT(swOutput_KEY_SURFACEWATER, install("TimeStep"), r_SURFACEWATER_PERIOD);
			UNPROTECT(1);
		}
		if(periodUse[12][0]) {
			if(debug) Rprintf("\tdy\n");
			PROTECT(Rsurface_water_dy = allocMatrix(REALSXP, dy_nrow, Rsurface_water_columns+2));
			PROTECT(Rsurface_water_names_dy = allocVector(VECSXP, 2));
			PROTECT(Rsurface_water_names_y_dy = allocVector(STRSXP, Rsurface_water_columns + 2));
			SET_STRING_ELT(Rsurface_water_names_y_dy, 0, mkChar("Year"));
			SET_STRING_ELT(Rsurface_water_names_y_dy, 1, mkChar("DOY"));
			SET_STRING_ELT(Rsurface_water_names_y_dy, 2, mkChar("surfaceWater_cm"));
			SET_VECTOR_ELT(Rsurface_water_names_dy, 1, Rsurface_water_names_y_dy);
			setAttrib(Rsurface_water_dy, R_DimNamesSymbol, Rsurface_water_names_dy);
			SET_SLOT(swOutput_KEY_SURFACEWATER, install("Day"), Rsurface_water_dy);
			UNPROTECT(3);
		}
		if(periodUse[12][1]) {
			if(debug) Rprintf("\twk\n");
			PROTECT(Rsurface_water_wk = allocMatrix(REALSXP, wk_nrow, Rsurface_water_columns+2));
			PROTECT(Rsurface_water_names_wk = allocVector(VECSXP, 2));
			PROTECT(Rsurface_water_names_y_wk = allocVector(STRSXP, Rsurface_water_columns + 2));
			SET_STRING_ELT(Rsurface_water_names_y_wk, 0, mkChar("Year"));
			SET_STRING_ELT(Rsurface_water_names_y_wk, 1, mkChar("Week"));
			SET_STRING_ELT(Rsurface_water_names_y_wk, 2, mkChar("surfaceWater_cm"));
			SET_VECTOR_ELT(Rsurface_water_names_wk, 1, Rsurface_water_names_y_wk);
			setAttrib(Rsurface_water_wk, R_DimNamesSymbol, Rsurface_water_names_wk);
			SET_SLOT(swOutput_KEY_SURFACEWATER, install("Week"), Rsurface_water_wk);
			UNPROTECT(3);
		}
		if(periodUse[12][2]) {
			if(debug) Rprintf("\tmo\n");
			PROTECT(Rsurface_water_mo = allocMatrix(REALSXP, mo_nrow, Rsurface_water_columns+2));
			PROTECT(Rsurface_water_names_mo = allocVector(VECSXP, 2));
			PROTECT(Rsurface_water_names_y_mo = allocVector(STRSXP, Rsurface_water_columns + 2));
			SET_STRING_ELT(Rsurface_water_names_y_mo, 0, mkChar("Year"));
			SET_STRING_ELT(Rsurface_water_names_y_mo, 1, mkChar("Month"));
			SET_STRING_ELT(Rsurface_water_names_y_mo, 2, mkChar("surfaceWater_cm"));
			SET_VECTOR_ELT(Rsurface_water_names_mo, 1, Rsurface_water_names_y_mo);
			setAttrib(Rsurface_water_mo, R_DimNamesSymbol, Rsurface_water_names_mo);
			SET_SLOT(swOutput_KEY_SURFACEWATER, install("Month"), Rsurface_water_mo);
			UNPROTECT(3);
		}
		if(periodUse[12][3]) {
			if(debug) Rprintf("\tyr\n");
			PROTECT(Rsurface_water_yr = allocMatrix(REALSXP, yr_nrow, Rsurface_water_columns+1));
			PROTECT(Rsurface_water_names_yr = allocVector(VECSXP, 2));
			PROTECT(Rsurface_water_names_y_yr = allocVector(STRSXP, Rsurface_water_columns + 1));
			SET_STRING_ELT(Rsurface_water_names_y_yr, 0, mkChar("Year"));
			SET_STRING_ELT(Rsurface_water_names_y_yr, 1, mkChar("surfaceWater_cm"));
			SET_VECTOR_ELT(Rsurface_water_names_yr, 1, Rsurface_water_names_y_yr);
			setAttrib(Rsurface_water_yr, R_DimNamesSymbol, Rsurface_water_names_yr);
			SET_SLOT(swOutput_KEY_SURFACEWATER, install("Year"), Rsurface_water_yr);
			UNPROTECT(3);
		}
		PROTECT(r_SURFACEWATER_COLUMNS = NEW_INTEGER(1));
		INTEGER(r_SURFACEWATER_COLUMNS)[0]=Rsurface_water_columns;
		SET_SLOT(swOutput_KEY_SURFACEWATER, install("Columns"), r_SURFACEWATER_COLUMNS);
		SET_SLOT(swOutput_Object, install(cSWoutput_Names[16]), swOutput_KEY_SURFACEWATER);
		UNPROTECT(3);
	}
	//TRANSP
	if(use[13]) {
		if(debug) Rprintf("%s\n",cSWoutput_KEY_Titles[13]);
		PROTECT(swOutput_KEY_TRANSP = NEW_OBJECT(swOutput_KEY));
		PROTECT(r_TRANSP_NAME = NEW_STRING(1));
		SET_STRING_ELT(r_TRANSP_NAME, 0, mkChar(cSWoutput_KEY_Titles[13]));
		SET_SLOT(swOutput_KEY_TRANSP, install("Title"), r_TRANSP_NAME);
		if (useTimeStep) {
			PROTECT(r_TRANSP_PERIOD = NEW_INTEGER(length(Periods)));
			for(i=0; i<length(Periods); i++)
				INTEGER(r_TRANSP_PERIOD)[i]=INTEGER(Periods)[i];
			SET_SLOT(swOutput_KEY_TRANSP, install("TimeStep"), r_TRANSP_PERIOD);
			UNPROTECT(1);
		} else {
			PROTECT(r_TRANSP_PERIOD = NEW_INTEGER(1));
			INTEGER(r_TRANSP_PERIOD)[0]=INTEGER(Periods)[13];
			SET_SLOT(swOutput_KEY_TRANSP, install("TimeStep"), r_TRANSP_PERIOD);
			UNPROTECT(1);
		}
		if(periodUse[13][0]) {
			if(debug) Rprintf("\tdy\n");
			PROTECT(Rtransp_dy = allocMatrix(REALSXP, dy_nrow, Rtransp_columns+2));
			PROTECT(Rtransp_names_dy = allocVector(VECSXP, 2));
			PROTECT(Rtransp_names_y_dy = allocVector(STRSXP, Rtransp_columns + 2));
			SET_STRING_ELT(Rtransp_names_y_dy, 0, mkChar("Year"));
			SET_STRING_ELT(Rtransp_names_y_dy, 1, mkChar("DOY"));
			for (i = 0; i < 5; i++) {
				for (j = 0; j < tLayers; j++) {
					strcpy(Ctemp, Ctransp_names[i]);
					strcat(Ctemp, Layers_names[j]);
					SET_STRING_ELT(Rtransp_names_y_dy, (i * tLayers + j) + 2, mkChar(Ctemp));
				}
			}
			SET_VECTOR_ELT(Rtransp_names_dy, 1, Rtransp_names_y_dy);
			setAttrib(Rtransp_dy, R_DimNamesSymbol, Rtransp_names_dy);
			SET_SLOT(swOutput_KEY_TRANSP, install("Day"), Rtransp_dy);
			UNPROTECT(3);
		}
		if(periodUse[13][1]) {
			if(debug) Rprintf("\twk\n");
			PROTECT(Rtransp_wk = allocMatrix(REALSXP, wk_nrow, Rtransp_columns+2));
			PROTECT(Rtransp_names_wk = allocVector(VECSXP, 2));
			PROTECT(Rtransp_names_y_wk = allocVector(STRSXP, Rtransp_columns + 2));
			SET_STRING_ELT(Rtransp_names_y_wk, 0, mkChar("Year"));
			SET_STRING_ELT(Rtransp_names_y_wk, 1, mkChar("Week"));
			for (i = 0; i < 5; i++) {
				for (j = 0; j < tLayers; j++) {
					strcpy(Ctemp, Ctransp_names[i]);
					strcat(Ctemp, Layers_names[j]);
					SET_STRING_ELT(Rtransp_names_y_wk, (i * tLayers + j) + 2, mkChar(Ctemp));
				}
			}
			SET_VECTOR_ELT(Rtransp_names_wk, 1, Rtransp_names_y_wk);
			setAttrib(Rtransp_wk, R_DimNamesSymbol, Rtransp_names_wk);
			SET_SLOT(swOutput_KEY_TRANSP, install("Week"), Rtransp_wk);
			UNPROTECT(3);
		}
		if(periodUse[13][2]) {
			if(debug) Rprintf("\tmo\n");
			PROTECT(Rtransp_mo = allocMatrix(REALSXP, mo_nrow, Rtransp_columns+2));
			PROTECT(Rtransp_names_mo = allocVector(VECSXP, 2));
			PROTECT(Rtransp_names_y_mo = allocVector(STRSXP, Rtransp_columns + 2));
			SET_STRING_ELT(Rtransp_names_y_mo, 0, mkChar("Year"));
			SET_STRING_ELT(Rtransp_names_y_mo, 1, mkChar("Month"));
			for (i = 0; i < 5; i++) {
				for (j = 0; j < tLayers; j++) {
					strcpy(Ctemp, Ctransp_names[i]);
					strcat(Ctemp, Layers_names[j]);
					SET_STRING_ELT(Rtransp_names_y_mo, (i * tLayers + j) + 2, mkChar(Ctemp));
				}
			}
			SET_VECTOR_ELT(Rtransp_names_mo, 1, Rtransp_names_y_mo);
			setAttrib(Rtransp_mo, R_DimNamesSymbol, Rtransp_names_mo);
			SET_SLOT(swOutput_KEY_TRANSP, install("Month"), Rtransp_mo);
			UNPROTECT(3);
		}
		if(periodUse[13][3]) {
			if(debug) Rprintf("\tyr\n");
			PROTECT(Rtransp_yr = allocMatrix(REALSXP, yr_nrow, Rtransp_columns+1));
			PROTECT(Rtransp_names_yr = allocVector(VECSXP, 2));
			PROTECT(Rtransp_names_y_yr = allocVector(STRSXP, Rtransp_columns + 1));
			SET_STRING_ELT(Rtransp_names_y_yr, 0, mkChar("Year"));
			for (i = 0; i < 5; i++) {
				for (j = 0; j < tLayers; j++) {
					strcpy(Ctemp, Ctransp_names[i]);
					strcat(Ctemp, Layers_names[j]);
					SET_STRING_ELT(Rtransp_names_y_yr, (i * tLayers + j) + 1, mkChar(Ctemp));
				}
			}
			SET_VECTOR_ELT(Rtransp_names_yr, 1, Rtransp_names_y_yr);
			setAttrib(Rtransp_yr, R_DimNamesSymbol, Rtransp_names_yr);
			SET_SLOT(swOutput_KEY_TRANSP, install("Year"), Rtransp_yr);
			UNPROTECT(3);
		}
		PROTECT(r_TRANSP_COLUMNS = NEW_INTEGER(1));
		INTEGER(r_TRANSP_COLUMNS)[0]=Rtransp_columns;
		SET_SLOT(swOutput_KEY_TRANSP, install("Columns"), r_TRANSP_COLUMNS);
		SET_SLOT(swOutput_Object, install(cSWoutput_Names[17]), swOutput_KEY_TRANSP);
		UNPROTECT(3);
	}
	//EVAP SOIL
	if(use[14]) {
		if(debug) Rprintf("%s\n",cSWoutput_KEY_Titles[14]);
		PROTECT(swOutput_KEY_EVAPSOIL = NEW_OBJECT(swOutput_KEY));
		PROTECT(r_EVAPSOIL_NAME = NEW_STRING(1));
		SET_STRING_ELT(r_EVAPSOIL_NAME , 0, mkChar(cSWoutput_KEY_Titles[14]));
		SET_SLOT(swOutput_KEY_EVAPSOIL, install("Title"), r_EVAPSOIL_NAME);
		if (useTimeStep) {
			PROTECT(r_EVAPSOIL_PERIOD = NEW_INTEGER(length(Periods)));
			for(i=0; i<length(Periods); i++)
				INTEGER(r_EVAPSOIL_PERIOD)[i]=INTEGER(Periods)[i];
			SET_SLOT(swOutput_KEY_EVAPSOIL, install("TimeStep"), r_EVAPSOIL_PERIOD);
			UNPROTECT(1);
		} else {
			PROTECT(r_EVAPSOIL_PERIOD = NEW_INTEGER(1));
			INTEGER(r_EVAPSOIL_PERIOD)[0]=INTEGER(Periods)[14];
			SET_SLOT(swOutput_KEY_EVAPSOIL, install("TimeStep"), r_EVAPSOIL_PERIOD);
			UNPROTECT(1);
		}
		if(periodUse[14][0]) {
			if(debug) Rprintf("\tdy\n");
			PROTECT(Revasoil_dy = allocMatrix(REALSXP, dy_nrow, Revasoil_columns+2));
			PROTECT(Revap_soil_names_dy = allocVector(VECSXP, 2));
			PROTECT(Revap_soil_names_y_dy = allocVector(STRSXP, Revasoil_columns + 2));
			SET_STRING_ELT(Revap_soil_names_y_dy, 0, mkChar("Year"));
			SET_STRING_ELT(Revap_soil_names_y_dy, 1, mkChar("DOY"));
			for (i = 0; i < Revasoil_columns; i++)
				SET_STRING_ELT(Revap_soil_names_y_dy, i + 2, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(Revap_soil_names_dy, 1, Revap_soil_names_y_dy);
			setAttrib(Revasoil_dy, R_DimNamesSymbol, Revap_soil_names_dy);
			SET_SLOT(swOutput_KEY_EVAPSOIL, install("Day"), Revasoil_dy);
			UNPROTECT(3);
		}
		if(periodUse[14][1]) {
			if(debug) Rprintf("\twk\n");
			PROTECT(Revasoil_wk = allocMatrix(REALSXP, wk_nrow, Revasoil_columns+2));
			PROTECT(Revap_soil_names_wk = allocVector(VECSXP, 2));
			PROTECT(Revap_soil_names_y_wk = allocVector(STRSXP, Revasoil_columns + 2));
			SET_STRING_ELT(Revap_soil_names_y_wk, 0, mkChar("Year"));
			SET_STRING_ELT(Revap_soil_names_y_wk, 1, mkChar("Week"));
			for (i = 0; i < Revasoil_columns; i++)
				SET_STRING_ELT(Revap_soil_names_y_wk, i + 2, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(Revap_soil_names_wk, 1, Revap_soil_names_y_wk);
			setAttrib(Revasoil_wk, R_DimNamesSymbol, Revap_soil_names_wk);
			SET_SLOT(swOutput_KEY_EVAPSOIL, install("Week"), Revasoil_wk);
			UNPROTECT(3);
		}
		if(periodUse[14][2]) {
			if(debug) Rprintf("\tmo\n");
			PROTECT(Revasoil_mo = allocMatrix(REALSXP, mo_nrow, Revasoil_columns+2));
			PROTECT(Revap_soil_names_mo = allocVector(VECSXP, 2));
			PROTECT(Revap_soil_names_y_mo = allocVector(STRSXP, Revasoil_columns + 2));
			SET_STRING_ELT(Revap_soil_names_y_mo, 0, mkChar("Year"));
			SET_STRING_ELT(Revap_soil_names_y_mo, 1, mkChar("Month"));
			for (i = 0; i < Revasoil_columns; i++)
				SET_STRING_ELT(Revap_soil_names_y_mo, i + 2, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(Revap_soil_names_mo, 1, Revap_soil_names_y_mo);
			setAttrib(Revasoil_mo, R_DimNamesSymbol, Revap_soil_names_mo);
			SET_SLOT(swOutput_KEY_EVAPSOIL, install("Month"), Revasoil_mo);
			UNPROTECT(3);
		}
		if(periodUse[14][3]) {
			if(debug) Rprintf("\tyr\n");
			PROTECT(Revasoil_yr = allocMatrix(REALSXP, yr_nrow, Revasoil_columns+1));
			PROTECT(Revap_soil_names_yr = allocVector(VECSXP, 2));
			PROTECT(Revap_soil_names_y_yr = allocVector(STRSXP, Revasoil_columns + 1));
			SET_STRING_ELT(Revap_soil_names_y_yr, 0, mkChar("Year"));
			for (i = 0; i < Revasoil_columns; i++)
				SET_STRING_ELT(Revap_soil_names_y_yr, i + 1, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(Revap_soil_names_yr, 1, Revap_soil_names_y_yr);
			setAttrib(Revasoil_yr, R_DimNamesSymbol, Revap_soil_names_yr);
			SET_SLOT(swOutput_KEY_EVAPSOIL, install("Year"), Revasoil_yr);
			UNPROTECT(3);
		}
		PROTECT(r_EVAPSOIL_COLUMNS = NEW_INTEGER(1));
		INTEGER(r_EVAPSOIL_COLUMNS)[0]=Revasoil_columns;
		SET_SLOT(swOutput_KEY_EVAPSOIL, install("Columns"), r_EVAPSOIL_COLUMNS);
		SET_SLOT(swOutput_Object, install(cSWoutput_Names[18]), swOutput_KEY_EVAPSOIL);
		UNPROTECT(3);
	}
	//EVAP SURFACE
	if(use[15]) {
		if(debug) Rprintf("%s\n",cSWoutput_KEY_Titles[15]);
		PROTECT(swOutput_KEY_EVAPSURFACE = NEW_OBJECT(swOutput_KEY));
		PROTECT(r_EVAPSURFACE_NAME = NEW_STRING(1));
		SET_STRING_ELT(r_EVAPSURFACE_NAME, 0, mkChar(cSWoutput_KEY_Titles[15]));
		SET_SLOT(swOutput_KEY_EVAPSURFACE, install("Title"), r_EVAPSURFACE_NAME);
		if (useTimeStep) {
			PROTECT(r_EVAPSURFACE_PERIOD = NEW_INTEGER(length(Periods)));
			for(i=0; i<length(Periods); i++)
				INTEGER(r_EVAPSURFACE_PERIOD)[i]=INTEGER(Periods)[i];
			SET_SLOT(swOutput_KEY_EVAPSURFACE, install("TimeStep"), r_EVAPSURFACE_PERIOD);
			UNPROTECT(1);
		} else {
			PROTECT(r_EVAPSURFACE_PERIOD = NEW_INTEGER(1));
			INTEGER(r_EVAPSURFACE_PERIOD)[0]=INTEGER(Periods)[15];
			SET_SLOT(swOutput_KEY_EVAPSURFACE, install("TimeStep"), r_EVAPSURFACE_PERIOD);
			UNPROTECT(1);
		}
		if(periodUse[15][0]) {
			if(debug) Rprintf("\tdy\n");
			PROTECT(Revasurface_dy = allocMatrix(REALSXP, dy_nrow, Revasurface_columns+2));
			PROTECT(Revap_surface_names_dy = allocVector(VECSXP, 2));
			PROTECT(Revap_surface_names_y_dy = allocVector(STRSXP, Revasurface_columns + 2));
			SET_STRING_ELT(Revap_surface_names_y_dy, 0, mkChar("Year"));
			SET_STRING_ELT(Revap_surface_names_y_dy, 1, mkChar("DOY"));
			for (i = 0; i < Revasurface_columns; i++)
				SET_STRING_ELT(Revap_surface_names_y_dy, i + 2, mkChar(Cevap_surface_names[i]));
			SET_VECTOR_ELT(Revap_surface_names_dy, 1, Revap_surface_names_y_dy);
			setAttrib(Revasurface_dy, R_DimNamesSymbol, Revap_surface_names_dy);
			SET_SLOT(swOutput_KEY_EVAPSURFACE, install("Day"), Revasurface_dy);
			UNPROTECT(3);
		}
		if(periodUse[15][1]) {
			if(debug) Rprintf("\twk\n");
			PROTECT(Revasurface_wk = allocMatrix(REALSXP, wk_nrow, Revasurface_columns+2));
			PROTECT(Revap_surface_names_wk = allocVector(VECSXP, 2));
			PROTECT(Revap_surface_names_y_wk = allocVector(STRSXP, Revasurface_columns + 2));
			SET_STRING_ELT(Revap_surface_names_y_wk, 0, mkChar("Year"));
			SET_STRING_ELT(Revap_surface_names_y_wk, 1, mkChar("Week"));
			for (i = 0; i < Revasurface_columns; i++)
				SET_STRING_ELT(Revap_surface_names_y_wk, i + 2, mkChar(Cevap_surface_names[i]));
			SET_VECTOR_ELT(Revap_surface_names_wk, 1, Revap_surface_names_y_wk);
			setAttrib(Revasurface_wk, R_DimNamesSymbol, Revap_surface_names_wk);
			SET_SLOT(swOutput_KEY_EVAPSURFACE, install("Week"), Revasurface_wk);
			UNPROTECT(3);
		}
		if(periodUse[15][2]) {
			if(debug) Rprintf("\tmo\n");
			PROTECT(Revasurface_mo = allocMatrix(REALSXP, mo_nrow, Revasurface_columns+2));
			PROTECT(Revap_surface_names_mo = allocVector(VECSXP, 2));
			PROTECT(Revap_surface_names_y_mo = allocVector(STRSXP, Revasurface_columns + 2));
			SET_STRING_ELT(Revap_surface_names_y_mo, 0, mkChar("Year"));
			SET_STRING_ELT(Revap_surface_names_y_mo, 1, mkChar("Month"));
			for (i = 0; i < Revasurface_columns; i++)
				SET_STRING_ELT(Revap_surface_names_y_mo, i + 2, mkChar(Cevap_surface_names[i]));
			SET_VECTOR_ELT(Revap_surface_names_mo, 1, Revap_surface_names_y_mo);
			setAttrib(Revasurface_mo, R_DimNamesSymbol, Revap_surface_names_mo);
			SET_SLOT(swOutput_KEY_EVAPSURFACE, install("Month"), Revasurface_mo);
			UNPROTECT(3);
		}
		if(periodUse[15][3]) {
			if(debug) Rprintf("\tyr\n");
			PROTECT(Revasurface_yr = allocMatrix(REALSXP, yr_nrow, Revasurface_columns+1));
			PROTECT(Revap_surface_names_yr = allocVector(VECSXP, 2));
			PROTECT(Revap_surface_names_y_yr = allocVector(STRSXP, Revasurface_columns + 1));
			SET_STRING_ELT(Revap_surface_names_y_yr, 0, mkChar("Year"));
			for (i = 0; i < Revasurface_columns; i++)
				SET_STRING_ELT(Revap_surface_names_y_yr, i + 1, mkChar(Cevap_surface_names[i]));
			SET_VECTOR_ELT(Revap_surface_names_yr, 1, Revap_surface_names_y_yr);
			setAttrib(Revasurface_yr, R_DimNamesSymbol, Revap_surface_names_yr);
			SET_SLOT(swOutput_KEY_EVAPSURFACE, install("Year"), Revasurface_yr);
			UNPROTECT(3);
		}

		PROTECT(r_EVAPSURFACE_COLUMNS = NEW_INTEGER(1));
		INTEGER(r_EVAPSURFACE_COLUMNS)[0]=Revasurface_columns;
		SET_SLOT(swOutput_KEY_EVAPSURFACE, install("Columns"), r_EVAPSURFACE_COLUMNS);
		SET_SLOT(swOutput_Object, install(cSWoutput_Names[19]), swOutput_KEY_EVAPSURFACE);
		UNPROTECT(3);
	}
	//SOIL INTERCEPTION
	if(use[16]) {
		if(debug) Rprintf("%s\n",cSWoutput_KEY_Titles[16]);
		PROTECT(swOutput_KEY_INTERCEPTION = NEW_OBJECT(swOutput_KEY));
		PROTECT(r_INTERCEPTION_NAME = NEW_STRING(1));
		SET_STRING_ELT(r_INTERCEPTION_NAME, 0, mkChar(cSWoutput_KEY_Titles[16]));
		SET_SLOT(swOutput_KEY_INTERCEPTION, install("Title"), r_INTERCEPTION_NAME);
		if (useTimeStep) {
			PROTECT(r_INTERCEPTION_PERIOD = NEW_INTEGER(length(Periods)));
			for(i=0; i<length(Periods); i++)
				INTEGER(r_INTERCEPTION_PERIOD)[i]=INTEGER(Periods)[i];
			SET_SLOT(swOutput_KEY_INTERCEPTION, install("TimeStep"), r_INTERCEPTION_PERIOD);
			UNPROTECT(1);
		} else {
			PROTECT(r_INTERCEPTION_PERIOD = NEW_INTEGER(1));
			INTEGER(r_INTERCEPTION_PERIOD)[0]=INTEGER(Periods)[16];
			SET_SLOT(swOutput_KEY_INTERCEPTION, install("TimeStep"), r_INTERCEPTION_PERIOD);
			UNPROTECT(1);
		}
		if(periodUse[16][0]) {
			if(debug) Rprintf("\tdy\n");
			PROTECT(Rinterception_dy = allocMatrix(REALSXP, dy_nrow, Rinterception_columns+2));
			PROTECT(Rinterception_names_dy = allocVector(VECSXP, 2));
			PROTECT(Rinterception_names_y_dy = allocVector(STRSXP, Rinterception_columns + 2));
			SET_STRING_ELT(Rinterception_names_y_dy, 0, mkChar("Year"));
			SET_STRING_ELT(Rinterception_names_y_dy, 1, mkChar("DOY"));
			for (i = 0; i < Rinterception_columns; i++)
				SET_STRING_ELT(Rinterception_names_y_dy, i + 2, mkChar(Cinterception_names[i]));
			SET_VECTOR_ELT(Rinterception_names_dy, 1, Rinterception_names_y_dy);
			setAttrib(Rinterception_dy, R_DimNamesSymbol, Rinterception_names_dy);
			SET_SLOT(swOutput_KEY_INTERCEPTION, install("Day"), Rinterception_dy);
			UNPROTECT(3);
		}
		if(periodUse[16][1]) {
			if(debug) Rprintf("\twk\n");
			PROTECT(Rinterception_wk = allocMatrix(REALSXP, wk_nrow, Rinterception_columns+2));
			PROTECT(Rinterception_names_wk = allocVector(VECSXP, 2));
			PROTECT(Rinterception_names_y_wk = allocVector(STRSXP, Rinterception_columns + 2));
			SET_STRING_ELT(Rinterception_names_y_wk, 0, mkChar("Year"));
			SET_STRING_ELT(Rinterception_names_y_wk, 1, mkChar("Week"));
			for (i = 0; i < Rinterception_columns; i++)
				SET_STRING_ELT(Rinterception_names_y_wk, i + 2, mkChar(Cinterception_names[i]));
			SET_VECTOR_ELT(Rinterception_names_wk, 1, Rinterception_names_y_wk);
			setAttrib(Rinterception_wk, R_DimNamesSymbol, Rinterception_names_wk);
			SET_SLOT(swOutput_KEY_INTERCEPTION, install("Week"), Rinterception_wk);
			UNPROTECT(3);
		}
		if(periodUse[16][2]) {
			if(debug) Rprintf("\tmo\n");
			PROTECT(Rinterception_mo = allocMatrix(REALSXP, mo_nrow, Rinterception_columns+2));
			PROTECT(Rinterception_names_mo = allocVector(VECSXP, 2));
			PROTECT(Rinterception_names_y_mo = allocVector(STRSXP, Rinterception_columns + 2));
			SET_STRING_ELT(Rinterception_names_y_mo, 0, mkChar("Year"));
			SET_STRING_ELT(Rinterception_names_y_mo, 1, mkChar("Month"));
			for (i = 0; i < Rinterception_columns; i++)
				SET_STRING_ELT(Rinterception_names_y_mo, i + 2, mkChar(Cinterception_names[i]));
			SET_VECTOR_ELT(Rinterception_names_mo, 1, Rinterception_names_y_mo);
			setAttrib(Rinterception_mo, R_DimNamesSymbol, Rinterception_names_mo);
			SET_SLOT(swOutput_KEY_INTERCEPTION, install("Month"), Rinterception_mo);
			UNPROTECT(3);
		}
		if(periodUse[16][3]) {
			if(debug) Rprintf("\tyr\n");
			PROTECT(Rinterception_yr = allocMatrix(REALSXP, yr_nrow, Rinterception_columns+1));
			PROTECT(Rinterception_names_yr = allocVector(VECSXP, 2));
			PROTECT(Rinterception_names_y_yr = allocVector(STRSXP, Rinterception_columns + 1));
			SET_STRING_ELT(Rinterception_names_y_yr, 0, mkChar("Year"));
			for (i = 0; i < Rinterception_columns; i++)
				SET_STRING_ELT(Rinterception_names_y_yr, i + 1, mkChar(Cinterception_names[i]));
			SET_VECTOR_ELT(Rinterception_names_yr, 1, Rinterception_names_y_yr);
			setAttrib(Rinterception_yr, R_DimNamesSymbol, Rinterception_names_yr);
			SET_SLOT(swOutput_KEY_INTERCEPTION, install("Year"), Rinterception_yr);
			UNPROTECT(3);
		}
		PROTECT(r_INTERCEPTION_COLUMNS = NEW_INTEGER(1));
		INTEGER(r_INTERCEPTION_COLUMNS)[0]=Rinterception_columns;
		SET_SLOT(swOutput_KEY_INTERCEPTION, install("Columns"), r_INTERCEPTION_COLUMNS);
		SET_SLOT(swOutput_Object, install(cSWoutput_Names[20]), swOutput_KEY_INTERCEPTION);
		UNPROTECT(3);
	}
	//Percolation
	if(use[17]) {
		if(debug) Rprintf("%s\n",cSWoutput_KEY_Titles[17]);
		PROTECT(swOutput_KEY_LYRDRAIN = NEW_OBJECT(swOutput_KEY));
		PROTECT(r_LYRDRAIN_NAME = NEW_STRING(1));
		SET_STRING_ELT(r_LYRDRAIN_NAME, 0, mkChar(cSWoutput_KEY_Titles[17]));
		SET_SLOT(swOutput_KEY_LYRDRAIN, install("Title"), r_LYRDRAIN_NAME);
		if (useTimeStep) {
			PROTECT(r_LYRDRAIN_PERIOD = NEW_INTEGER(length(Periods)));
			for(i=0; i<length(Periods); i++)
				INTEGER(r_LYRDRAIN_PERIOD)[i]=INTEGER(Periods)[i];
			SET_SLOT(swOutput_KEY_LYRDRAIN, install("TimeStep"), r_LYRDRAIN_PERIOD);
			UNPROTECT(1);
		} else {
			PROTECT(r_LYRDRAIN_PERIOD = NEW_INTEGER(1));
			INTEGER(r_LYRDRAIN_PERIOD)[0]=INTEGER(Periods)[17];
			SET_SLOT(swOutput_KEY_LYRDRAIN, install("TimeStep"), r_LYRDRAIN_PERIOD);
			UNPROTECT(1);
		}
		if(periodUse[17][0]) {
			if(debug) Rprintf("\tdy\n");
			PROTECT(Rpercolation_dy = allocMatrix(REALSXP, dy_nrow, Rpercolation_columns+2));
			PROTECT(Rpercolation_names_dy = allocVector(VECSXP, 2));
			PROTECT(Rpercolation_names_y_dy = allocVector(STRSXP, Rpercolation_columns + 2));
			SET_STRING_ELT(Rpercolation_names_y_dy, 0, mkChar("Year"));
			SET_STRING_ELT(Rpercolation_names_y_dy, 1, mkChar("DOY"));
			for (i = 0; i < (tLayers - 1); i++)
				SET_STRING_ELT(Rpercolation_names_y_dy, i + 2, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(Rpercolation_names_dy, 1, Rpercolation_names_y_dy);
			setAttrib(Rpercolation_dy, R_DimNamesSymbol, Rpercolation_names_dy);
			SET_SLOT(swOutput_KEY_LYRDRAIN, install("Day"), Rpercolation_dy);
			UNPROTECT(3);
		}
		if(periodUse[17][1]) {
			if(debug) Rprintf("\twk\n");
			PROTECT(Rpercolation_wk = allocMatrix(REALSXP, wk_nrow, Rpercolation_columns+2));
			PROTECT(Rpercolation_names_wk = allocVector(VECSXP, 2));
			PROTECT(Rpercolation_names_y_wk = allocVector(STRSXP, Rpercolation_columns + 2));
			SET_STRING_ELT(Rpercolation_names_y_wk, 0, mkChar("Year"));
			SET_STRING_ELT(Rpercolation_names_y_wk, 1, mkChar("Week"));
			for (i = 0; i < (tLayers - 1); i++)
				SET_STRING_ELT(Rpercolation_names_y_wk, i + 2, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(Rpercolation_names_wk, 1, Rpercolation_names_y_wk);
			setAttrib(Rpercolation_wk, R_DimNamesSymbol, Rpercolation_names_wk);
			SET_SLOT(swOutput_KEY_LYRDRAIN, install("Week"), Rpercolation_wk);
			UNPROTECT(3);
		}
		if(periodUse[17][2]) {
			if(debug) Rprintf("\tmo\n");
			PROTECT(Rpercolation_mo = allocMatrix(REALSXP, mo_nrow, Rpercolation_columns+2));
			PROTECT(Rpercolation_names_mo = allocVector(VECSXP, 2));
			PROTECT(Rpercolation_names_y_mo = allocVector(STRSXP, Rpercolation_columns + 2));
			SET_STRING_ELT(Rpercolation_names_y_mo, 0, mkChar("Year"));
			SET_STRING_ELT(Rpercolation_names_y_mo, 1, mkChar("Month"));
			for (i = 0; i < (tLayers - 1); i++)
				SET_STRING_ELT(Rpercolation_names_y_mo, i + 2, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(Rpercolation_names_mo, 1, Rpercolation_names_y_mo);
			setAttrib(Rpercolation_mo, R_DimNamesSymbol, Rpercolation_names_mo);
			SET_SLOT(swOutput_KEY_LYRDRAIN, install("Month"), Rpercolation_mo);
			UNPROTECT(3);
		}
		if(periodUse[17][3]) {
			if(debug) Rprintf("\tyr\n");
			PROTECT(Rpercolation_yr = allocMatrix(REALSXP, yr_nrow, Rpercolation_columns+1));
			PROTECT(Rpercolation_names_yr = allocVector(VECSXP, 2));
			PROTECT(Rpercolation_names_y_yr = allocVector(STRSXP, Rpercolation_columns + 1));
			SET_STRING_ELT(Rpercolation_names_y_yr, 0, mkChar("Year"));
			for (i = 0; i < (tLayers - 1); i++)
				SET_STRING_ELT(Rpercolation_names_y_yr, i + 1, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(Rpercolation_names_yr, 1, Rpercolation_names_y_yr);
			setAttrib(Rpercolation_yr, R_DimNamesSymbol, Rpercolation_names_yr);
			SET_SLOT(swOutput_KEY_LYRDRAIN, install("Year"), Rpercolation_yr);
			UNPROTECT(3);
		}
		PROTECT(r_LYRDRAIN_COLUMNS = NEW_INTEGER(1));
		INTEGER(r_LYRDRAIN_COLUMNS)[0]=Rpercolation_columns;
		SET_SLOT(swOutput_KEY_LYRDRAIN, install("Columns"), r_LYRDRAIN_COLUMNS);
		SET_SLOT(swOutput_Object, install(cSWoutput_Names[21]), swOutput_KEY_LYRDRAIN);
		UNPROTECT(3);
	}
	//Hydred
	if(use[18]) {
		if(debug) Rprintf("%s\n",cSWoutput_KEY_Titles[18]);
		PROTECT(swOutput_KEY_HYDRED = NEW_OBJECT(swOutput_KEY));
		PROTECT(r_HYDRED_NAME = NEW_STRING(1));
		SET_STRING_ELT(r_HYDRED_NAME, 0, mkChar(cSWoutput_KEY_Titles[18]));
		SET_SLOT(swOutput_KEY_HYDRED, install("Title"), r_HYDRED_NAME);
		if (useTimeStep) {
			PROTECT(r_HYDRED_PERIOD = NEW_INTEGER(length(Periods)));
			for(i=0; i<length(Periods); i++)
				INTEGER(r_HYDRED_PERIOD)[i]=INTEGER(Periods)[i];
			SET_SLOT(swOutput_KEY_HYDRED, install("TimeStep"), r_HYDRED_PERIOD);
			UNPROTECT(1);
		} else {
			PROTECT(r_HYDRED_PERIOD = NEW_INTEGER(1));
			INTEGER(r_HYDRED_PERIOD)[0]=INTEGER(Periods)[18];
			SET_SLOT(swOutput_KEY_HYDRED, install("TimeStep"), r_HYDRED_PERIOD);
			UNPROTECT(1);
		}
		if(periodUse[18][0]) {
			if(debug) Rprintf("\tdy\n");
			if(debug) Rprintf("\tRows dy_nrow %d Columns %d \n", dy_nrow, Rhydred_columns + 2);
			PROTECT(Rhydred_dy = allocMatrix(REALSXP, dy_nrow, Rhydred_columns+2));
			PROTECT(Rhydred_names_dy = allocVector(VECSXP, 2));
			PROTECT(Rhydred_names_y_dy = allocVector(STRSXP, Rhydred_columns + 2));
			SET_STRING_ELT(Rhydred_names_y_dy, 0, mkChar("Year"));
			SET_STRING_ELT(Rhydred_names_y_dy, 1, mkChar("DOY"));
			for (i = 0; i < 5; i++) {
				for (j = 0; j < tLayers; j++) {
					strcpy(Ctemp, Chydred_names[i]);
					strcat(Ctemp, Layers_names[j]);
					SET_STRING_ELT(Rhydred_names_y_dy, (i * tLayers + j) + 2, mkChar(Ctemp));
				}
			}
			SET_VECTOR_ELT(Rhydred_names_dy, 1, Rhydred_names_y_dy);
			setAttrib(Rhydred_dy, R_DimNamesSymbol, Rhydred_names_dy);
			SET_SLOT(swOutput_KEY_HYDRED, install("Day"), Rhydred_dy);
			UNPROTECT(3);
		}
		if(periodUse[18][1]) {
			if(debug) Rprintf("\twk\n");
			PROTECT(Rhydred_wk = allocMatrix(REALSXP, wk_nrow, Rhydred_columns+2));
			PROTECT(Rhydred_names_wk = allocVector(VECSXP, 2));
			PROTECT(Rhydred_names_y_wk = allocVector(STRSXP, Rhydred_columns + 2));
			SET_STRING_ELT(Rhydred_names_y_wk, 0, mkChar("Year"));
			SET_STRING_ELT(Rhydred_names_y_wk, 1, mkChar("Week"));
			for (i = 0; i < 5; i++) {
				for (j = 0; j < tLayers; j++) {
					strcpy(Ctemp, Chydred_names[i]);
					strcat(Ctemp, Layers_names[j]);
					SET_STRING_ELT(Rhydred_names_y_wk, (i * tLayers + j) + 2, mkChar(Ctemp));
				}
			}
			SET_VECTOR_ELT(Rhydred_names_wk, 1, Rhydred_names_y_wk);
			setAttrib(Rhydred_wk, R_DimNamesSymbol, Rhydred_names_wk);
			SET_SLOT(swOutput_KEY_HYDRED, install("Week"), Rhydred_wk);
			UNPROTECT(3);
		}
		if(periodUse[18][2]) {
			if(debug) Rprintf("\tmo\n");
			PROTECT(Rhydred_mo = allocMatrix(REALSXP, mo_nrow, Rhydred_columns+2));
			PROTECT(Rhydred_names_mo = allocVector(VECSXP, 2));
			PROTECT(Rhydred_names_y_mo = allocVector(STRSXP, Rhydred_columns + 2));
			SET_STRING_ELT(Rhydred_names_y_mo, 0, mkChar("Year"));
			SET_STRING_ELT(Rhydred_names_y_mo, 1, mkChar("Month"));
			for (i = 0; i < 5; i++) {
				for (j = 0; j < tLayers; j++) {
					strcpy(Ctemp, Chydred_names[i]);
					strcat(Ctemp, Layers_names[j]);
					SET_STRING_ELT(Rhydred_names_y_mo, (i * tLayers + j) + 2, mkChar(Ctemp));
				}
			}
			SET_VECTOR_ELT(Rhydred_names_mo, 1, Rhydred_names_y_mo);
			setAttrib(Rhydred_mo, R_DimNamesSymbol, Rhydred_names_mo);
			SET_SLOT(swOutput_KEY_HYDRED, install("Month"), Rhydred_mo);
			UNPROTECT(3);
		}
		if(periodUse[18][3]) {
			if(debug) Rprintf("\tyr\n");
			PROTECT(Rhydred_yr = allocMatrix(REALSXP, yr_nrow, Rhydred_columns+1));
			PROTECT(Rhydred_names_yr = allocVector(VECSXP, 2));
			PROTECT(Rhydred_names_y_yr = allocVector(STRSXP, Rhydred_columns + 1));
			SET_STRING_ELT(Rhydred_names_y_yr, 0, mkChar("Year"));
			for (i = 0; i < 5; i++) {
				for (j = 0; j < tLayers; j++) {
					strcpy(Ctemp, Chydred_names[i]);
					strcat(Ctemp, Layers_names[j]);
					SET_STRING_ELT(Rhydred_names_y_yr, (i * tLayers + j) + 1, mkChar(Ctemp));
				}
			}
			SET_VECTOR_ELT(Rhydred_names_yr, 1, Rhydred_names_y_yr);
			setAttrib(Rhydred_yr, R_DimNamesSymbol, Rhydred_names_yr);
			SET_SLOT(swOutput_KEY_HYDRED, install("Year"), Rhydred_yr);
			UNPROTECT(3);
		}
		PROTECT(r_HYDRED_COLUMNS = NEW_INTEGER(1));
		INTEGER(r_HYDRED_COLUMNS)[0]=Rhydred_columns;
		SET_SLOT(swOutput_KEY_HYDRED, install("Columns"), r_HYDRED_COLUMNS);
		SET_SLOT(swOutput_Object, install(cSWoutput_Names[22]), swOutput_KEY_HYDRED);
		UNPROTECT(3);
	}
	//ET - NOT USED
	if(use[19]) {
		if(debug) Rprintf("%s\n",cSWoutput_KEY_Titles[19]);
		PROTECT(swOutput_KEY_ET = NEW_OBJECT(swOutput_KEY));
		PROTECT(r_ET_NAME = NEW_STRING(1));
		SET_STRING_ELT(r_ET_NAME, 0, mkChar(cSWoutput_KEY_Titles[19]));
		SET_SLOT(swOutput_KEY_ET, install("Title"), r_ET_NAME);
		if (useTimeStep) {
			PROTECT(r_ET_PERIOD = NEW_INTEGER(length(Periods)));
			for(i=0; i<length(Periods); i++)
				INTEGER(r_ET_PERIOD)[i]=INTEGER(Periods)[i];
			SET_SLOT(swOutput_KEY_ET, install("TimeStep"), r_ET_PERIOD);
			UNPROTECT(1);
		} else {
			PROTECT(r_ET_PERIOD = NEW_INTEGER(1));
			INTEGER(r_ET_PERIOD)[0]=INTEGER(Periods)[19];
			SET_SLOT(swOutput_KEY_ET, install("TimeStep"), r_ET_PERIOD);
			UNPROTECT(1);
		}
		if(periodUse[19][0]) {
			PROTECT(Ret_dy = allocMatrix(REALSXP, dy_nrow, Ret_columns+2));

			setAttrib(Ret_dy, R_DimNamesSymbol, Ret_names_dy);
			SET_SLOT(swOutput_KEY_ET, install("Day"), Ret_dy);
			UNPROTECT(1);
		}
		if(periodUse[19][1]) {
			PROTECT(Ret_wk = allocMatrix(REALSXP, wk_nrow, Ret_columns+2));

			setAttrib(Ret_wk, R_DimNamesSymbol, Ret_names_dy);
			SET_SLOT(swOutput_KEY_ET, install("Week"), Ret_wk);
			UNPROTECT(1);
		}
		if(periodUse[19][2]) {
			PROTECT(Ret_mo = allocMatrix(REALSXP, mo_nrow, Ret_columns+2));

			setAttrib(Ret_mo, R_DimNamesSymbol, Ret_names_dy);
			SET_SLOT(swOutput_KEY_ET, install("Month"), Ret_mo);
			UNPROTECT(1);
		}
		if(periodUse[19][3]) {
			PROTECT(Ret_yr = allocMatrix(REALSXP, yr_nrow, Ret_columns+1));

			setAttrib(Ret_yr, R_DimNamesSymbol, Ret_names_dy);
			SET_SLOT(swOutput_KEY_ET, install("Year"), Ret_yr);
			UNPROTECT(1);
		}
		PROTECT(r_ET_COLUMNS = NEW_INTEGER(1));
		INTEGER(r_ET_COLUMNS)[0]=Ret_columns;
		SET_SLOT(swOutput_KEY_ET, install("Columns"), r_ET_COLUMNS);
		SET_SLOT(swOutput_Object, install(cSWoutput_Names[23]), swOutput_KEY_ET);
		UNPROTECT(3);
	}
	//AET
	if(use[20]) {
		if(debug) Rprintf("%s\n",cSWoutput_KEY_Titles[20]);
		PROTECT(swOutput_KEY_AET = NEW_OBJECT(swOutput_KEY));
		PROTECT(r_AET_NAME = NEW_STRING(1));
		SET_STRING_ELT(r_AET_NAME, 0, mkChar(cSWoutput_KEY_Titles[20]));
		SET_SLOT(swOutput_KEY_AET, install("Title"), r_AET_NAME);
		if (useTimeStep) {
			PROTECT(r_AET_PERIOD = NEW_INTEGER(length(Periods)));
			for(i=0; i<length(Periods); i++)
				INTEGER(r_AET_PERIOD)[i]=INTEGER(Periods)[i];
			SET_SLOT(swOutput_KEY_AET, install("TimeStep"), r_AET_PERIOD);
			UNPROTECT(1);
		} else {
			PROTECT(r_AET_PERIOD = NEW_INTEGER(1));
			INTEGER(r_AET_PERIOD)[0]=INTEGER(Periods)[20];
			SET_SLOT(swOutput_KEY_AET, install("TimeStep"), r_AET_PERIOD);
			UNPROTECT(1);
		}
		if(periodUse[20][0]) {
			if(debug) Rprintf("\tdy\n");
			PROTECT(Raet_dy = allocMatrix(REALSXP, dy_nrow, Raet_columns+2));
			PROTECT(Raet_names_dy = allocVector(VECSXP, 2));
			PROTECT(Raet_names_y_dy = allocVector(STRSXP, Raet_columns + 2));
			SET_STRING_ELT(Raet_names_y_dy, 0, mkChar("Year"));
			SET_STRING_ELT(Raet_names_y_dy, 1, mkChar("DOY"));
			SET_STRING_ELT(Raet_names_y_dy, 2, mkChar("evapotr_cm"));
			SET_VECTOR_ELT(Raet_names_dy, 1, Raet_names_y_dy);
			setAttrib(Raet_dy, R_DimNamesSymbol, Raet_names_dy);
			SET_SLOT(swOutput_KEY_AET, install("Day"), Raet_dy);
			UNPROTECT(3);
		}
		if(periodUse[20][1]) {
			if(debug) Rprintf("\twk\n");
			PROTECT(Raet_wk = allocMatrix(REALSXP, wk_nrow, Raet_columns+2));
			PROTECT(Raet_names_wk = allocVector(VECSXP, 2));
			PROTECT(Raet_names_y_wk = allocVector(STRSXP, Raet_columns + 2));
			SET_STRING_ELT(Raet_names_y_wk, 0, mkChar("Year"));
			SET_STRING_ELT(Raet_names_y_wk, 1, mkChar("Week"));
			SET_STRING_ELT(Raet_names_y_wk, 2, mkChar("evapotr_cm"));
			SET_VECTOR_ELT(Raet_names_wk, 1, Raet_names_y_wk);
			setAttrib(Raet_wk, R_DimNamesSymbol, Raet_names_wk);
			SET_SLOT(swOutput_KEY_AET, install("Week"), Raet_wk);
			UNPROTECT(3);
		}
		if(periodUse[20][2]) {
			if(debug) Rprintf("\tmo\n");
			PROTECT(Raet_mo = allocMatrix(REALSXP, mo_nrow, Raet_columns+2));
			PROTECT(Raet_names_mo = allocVector(VECSXP, 2));
			PROTECT(Raet_names_y_mo = allocVector(STRSXP, Raet_columns + 2));
			SET_STRING_ELT(Raet_names_y_mo, 0, mkChar("Year"));
			SET_STRING_ELT(Raet_names_y_mo, 1, mkChar("Month"));
			SET_STRING_ELT(Raet_names_y_mo, 2, mkChar("evapotr_cm"));
			SET_VECTOR_ELT(Raet_names_mo, 1, Raet_names_y_mo);
			setAttrib(Raet_mo, R_DimNamesSymbol, Raet_names_mo);
			SET_SLOT(swOutput_KEY_AET, install("Month"), Raet_mo);
			UNPROTECT(3);
		}
		if(periodUse[20][3]) {
			if(debug) Rprintf("\tyr\n");
			PROTECT(Raet_yr = allocMatrix(REALSXP, yr_nrow, Raet_columns+1));
			PROTECT(Raet_names_yr = allocVector(VECSXP, 2));
			PROTECT(Raet_names_y_yr = allocVector(STRSXP, Raet_columns + 1));
			SET_STRING_ELT(Raet_names_y_yr, 0, mkChar("Year"));
			SET_STRING_ELT(Raet_names_y_yr, 1, mkChar("evapotr_cm"));
			SET_VECTOR_ELT(Raet_names_yr, 1, Raet_names_y_yr);
			setAttrib(Raet_yr, R_DimNamesSymbol, Raet_names_yr);
			SET_SLOT(swOutput_KEY_AET, install("Year"), Raet_yr);
			UNPROTECT(3);
		}
		PROTECT(r_AET_COLUMNS = NEW_INTEGER(1));
		INTEGER(r_AET_COLUMNS)[0]=Raet_columns;
		SET_SLOT(swOutput_KEY_AET, install("Columns"), r_AET_COLUMNS);
		SET_SLOT(swOutput_Object, install(cSWoutput_Names[24]), swOutput_KEY_AET);
		UNPROTECT(3);
	}
	//PET
	if(use[21]) {
		if(debug) Rprintf("%s\n",cSWoutput_KEY_Titles[21]);
		PROTECT(swOutput_KEY_PET = NEW_OBJECT(swOutput_KEY));
		PROTECT(r_PET_NAME = NEW_STRING(1));
		SET_STRING_ELT(r_PET_NAME, 0, mkChar(cSWoutput_KEY_Titles[21]));
		SET_SLOT(swOutput_KEY_PET, install("Title"), r_PET_NAME);
		if (useTimeStep) {
			PROTECT(r_PET_PERIOD = NEW_INTEGER(length(Periods)));
			for(i=0; i<length(Periods); i++)
				INTEGER(r_PET_PERIOD)[i]=INTEGER(Periods)[i];
			SET_SLOT(swOutput_KEY_PET, install("TimeStep"), r_PET_PERIOD);
			UNPROTECT(1);
		} else {
			PROTECT(r_PET_PERIOD = NEW_INTEGER(1));
			INTEGER(r_PET_PERIOD)[0]=INTEGER(Periods)[21];
			SET_SLOT(swOutput_KEY_PET, install("TimeStep"), r_PET_PERIOD);
			UNPROTECT(1);
		}
		if(periodUse[21][0]) {
			if(debug) Rprintf("\tdy\n");
			PROTECT(Rpet_dy = allocMatrix(REALSXP, dy_nrow, Rpet_columns+2));
			PROTECT(Rpet_names_dy = allocVector(VECSXP, 2));
			PROTECT(Rpet_names_y_dy = allocVector(STRSXP, Rpet_columns + 2));
			SET_STRING_ELT(Rpet_names_y_dy, 0, mkChar("Year"));
			SET_STRING_ELT(Rpet_names_y_dy, 1, mkChar("DOY"));
			SET_STRING_ELT(Rpet_names_y_dy, 2, mkChar("pet_cm"));
			SET_VECTOR_ELT(Rpet_names_dy, 1, Rpet_names_y_dy);
			setAttrib(Rpet_dy, R_DimNamesSymbol, Rpet_names_dy);
			SET_SLOT(swOutput_KEY_PET, install("Day"), Rpet_dy);
			UNPROTECT(3);
		}
		if(periodUse[21][1]) {
			if(debug) Rprintf("\twk\n");
			PROTECT(Rpet_wk = allocMatrix(REALSXP, wk_nrow, Rpet_columns+2));
			PROTECT(Rpet_names_wk = allocVector(VECSXP, 2));
			PROTECT(Rpet_names_y_wk = allocVector(STRSXP, Rpet_columns + 2));
			SET_STRING_ELT(Rpet_names_y_wk, 0, mkChar("Year"));
			SET_STRING_ELT(Rpet_names_y_wk, 1, mkChar("Week"));
			SET_STRING_ELT(Rpet_names_y_wk, 2, mkChar("pet_cm"));
			SET_VECTOR_ELT(Rpet_names_wk, 1, Rpet_names_y_wk);
			setAttrib(Rpet_wk, R_DimNamesSymbol, Rpet_names_wk);
			SET_SLOT(swOutput_KEY_PET, install("Week"), Rpet_wk);
			UNPROTECT(3);
		}
		if(periodUse[21][2]) {
			if(debug) Rprintf("\tmo\n");
			PROTECT(Rpet_mo = allocMatrix(REALSXP, mo_nrow, Rpet_columns+2));
			PROTECT(Rpet_names_mo = allocVector(VECSXP, 2));
			PROTECT(Rpet_names_y_mo = allocVector(STRSXP, Rpet_columns + 2));
			SET_STRING_ELT(Rpet_names_y_mo, 0, mkChar("Year"));
			SET_STRING_ELT(Rpet_names_y_mo, 1, mkChar("Month"));
			SET_STRING_ELT(Rpet_names_y_mo, 2, mkChar("pet_cm"));
			SET_VECTOR_ELT(Rpet_names_mo, 1, Rpet_names_y_mo);
			setAttrib(Rpet_mo, R_DimNamesSymbol, Rpet_names_mo);
			SET_SLOT(swOutput_KEY_PET, install("Month"), Rpet_mo);
			UNPROTECT(3);
		}
		if(periodUse[21][3]) {
			if(debug) Rprintf("\tyr\n");
			PROTECT(Rpet_yr = allocMatrix(REALSXP, yr_nrow, Rpet_columns+1));
			PROTECT(Rpet_names_yr = allocVector(VECSXP, 2));
			PROTECT(Rpet_names_y_yr = allocVector(STRSXP, Rpet_columns + 1));
			SET_STRING_ELT(Rpet_names_y_yr, 0, mkChar("Year"));
			SET_STRING_ELT(Rpet_names_y_yr, 1, mkChar("pet_cm"));
			SET_VECTOR_ELT(Rpet_names_yr, 1, Rpet_names_y_yr);
			setAttrib(Rpet_yr, R_DimNamesSymbol, Rpet_names_yr);
			SET_SLOT(swOutput_KEY_PET, install("Year"), Rpet_yr);
			UNPROTECT(3);
		}
		PROTECT(r_PET_COLUMNS = NEW_INTEGER(1));
		INTEGER(r_PET_COLUMNS)[0]=Rpet_columns;
		SET_SLOT(swOutput_KEY_PET, install("Columns"), r_PET_COLUMNS);
		SET_SLOT(swOutput_Object, install(cSWoutput_Names[25]), swOutput_KEY_PET);
		UNPROTECT(3);
	}
	//WET DAYS
	if(use[22]) {
		if(debug) Rprintf("%s\n",cSWoutput_KEY_Titles[22]);
		PROTECT(swOutput_KEY_WETDAY = NEW_OBJECT(swOutput_KEY));
		PROTECT(r_WETDAY_NAME = NEW_STRING(1));
		SET_STRING_ELT(r_WETDAY_NAME, 0, mkChar(cSWoutput_KEY_Titles[22]));
		SET_SLOT(swOutput_KEY_WETDAY, install("Title"), r_WETDAY_NAME);
		if (useTimeStep) {
			PROTECT(r_WETDAY_PERIOD = NEW_INTEGER(length(Periods)));
			for(i=0; i<length(Periods); i++)
				INTEGER(r_WETDAY_PERIOD)[i]=INTEGER(Periods)[i];
			SET_SLOT(swOutput_KEY_WETDAY, install("TimeStep"), r_WETDAY_PERIOD);
			UNPROTECT(1);
		} else {
			PROTECT(r_WETDAY_PERIOD = NEW_INTEGER(1));
			INTEGER(r_WETDAY_PERIOD)[0]=INTEGER(Periods)[22];
			SET_SLOT(swOutput_KEY_WETDAY, install("TimeStep"), r_WETDAY_PERIOD);
			UNPROTECT(1);
		}
		if(periodUse[22][0]) {
			if(debug) Rprintf("\tdy\n");
			PROTECT(Rwetdays_dy = allocMatrix(REALSXP, dy_nrow, Rwetdays_columns+2));
			PROTECT(Rwetdays_names_dy = allocVector(VECSXP, 2));
			PROTECT(Rwetdays_names_y_dy = allocVector(STRSXP, Rwetdays_columns + 2));
			SET_STRING_ELT(Rwetdays_names_y_dy, 0, mkChar("Year"));
			SET_STRING_ELT(Rwetdays_names_y_dy, 1, mkChar("DOY"));
			for (i = 0; i < tLayers; i++)
				SET_STRING_ELT(Rwetdays_names_y_dy, i + 2, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(Rwetdays_names_dy, 1, Rwetdays_names_y_dy);
			setAttrib(Rwetdays_dy, R_DimNamesSymbol, Rwetdays_names_dy);
			SET_SLOT(swOutput_KEY_WETDAY, install("Day"), Rwetdays_dy);
			UNPROTECT(3);
		}
		if(periodUse[22][1]) {
			if(debug) Rprintf("\twk\n");
			PROTECT(Rwetdays_wk = allocMatrix(REALSXP, wk_nrow, Rwetdays_columns+2));
			PROTECT(Rwetdays_names_wk = allocVector(VECSXP, 2));
			PROTECT(Rwetdays_names_y_wk = allocVector(STRSXP, Rwetdays_columns + 2));
			SET_STRING_ELT(Rwetdays_names_y_wk, 0, mkChar("Year"));
			SET_STRING_ELT(Rwetdays_names_y_wk, 1, mkChar("Week"));
			for (i = 0; i < tLayers; i++)
				SET_STRING_ELT(Rwetdays_names_y_wk, i + 2, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(Rwetdays_names_wk, 1, Rwetdays_names_y_wk);
			setAttrib(Rwetdays_wk, R_DimNamesSymbol, Rwetdays_names_wk);
			SET_SLOT(swOutput_KEY_WETDAY, install("Week"), Rwetdays_wk);
			UNPROTECT(3);
		}
		if(periodUse[22][2]) {
			if(debug) Rprintf("\tmo\n");
			PROTECT(Rwetdays_mo = allocMatrix(REALSXP, mo_nrow, Rwetdays_columns+2));
			PROTECT(Rwetdays_names_mo = allocVector(VECSXP, 2));
			PROTECT(Rwetdays_names_y_mo = allocVector(STRSXP, Rwetdays_columns + 2));
			SET_STRING_ELT(Rwetdays_names_y_mo, 0, mkChar("Year"));
			SET_STRING_ELT(Rwetdays_names_y_mo, 1, mkChar("Month"));
			for (i = 0; i < tLayers; i++)
				SET_STRING_ELT(Rwetdays_names_y_mo, i + 2, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(Rwetdays_names_mo, 1, Rwetdays_names_y_mo);
			setAttrib(Rwetdays_mo, R_DimNamesSymbol, Rwetdays_names_mo);
			SET_SLOT(swOutput_KEY_WETDAY, install("Month"), Rwetdays_mo);
			UNPROTECT(3);
		}
		if(periodUse[22][3]) {
			if(debug) Rprintf("\tyr\n");
			PROTECT(Rwetdays_yr = allocMatrix(REALSXP, yr_nrow, Rwetdays_columns+1));
			PROTECT(Rwetdays_names_yr = allocVector(VECSXP, 2));
			PROTECT(Rwetdays_names_y_yr = allocVector(STRSXP, Rwetdays_columns + 1));
			SET_STRING_ELT(Rwetdays_names_y_yr, 0, mkChar("Year"));
			for (i = 0; i < tLayers; i++)
				SET_STRING_ELT(Rwetdays_names_y_yr, i + 1, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(Rwetdays_names_yr, 1, Rwetdays_names_y_yr);
			setAttrib(Rwetdays_yr, R_DimNamesSymbol, Rwetdays_names_yr);
			SET_SLOT(swOutput_KEY_WETDAY, install("Year"), Rwetdays_yr);
			UNPROTECT(3);
		}
		PROTECT(r_WETDAY_COLUMNS = NEW_INTEGER(1));
		INTEGER(r_WETDAY_COLUMNS)[0]=Rwetdays_columns;
		SET_SLOT(swOutput_KEY_WETDAY, install("Columns"), r_WETDAY_COLUMNS);
		SET_SLOT(swOutput_Object, install(cSWoutput_Names[26]), swOutput_KEY_WETDAY);
		UNPROTECT(3);
	}
	//SNOW PACK
	if(use[23]) {
		if(debug) Rprintf("%s\n",cSWoutput_KEY_Titles[23]);
		PROTECT(swOutput_KEY_SNOWPACK = NEW_OBJECT(swOutput_KEY));
		PROTECT(r_SNOWPACK_NAME = NEW_STRING(1));
		SET_STRING_ELT(r_SNOWPACK_NAME, 0, mkChar(cSWoutput_KEY_Titles[23]));
		SET_SLOT(swOutput_KEY_SNOWPACK, install("Title"), r_SNOWPACK_NAME);
		if (useTimeStep) {
			PROTECT(r_SNOWPACK_PERIOD = NEW_INTEGER(length(Periods)));
			for(i=0; i<length(Periods); i++)
				INTEGER(r_SNOWPACK_PERIOD)[i]=INTEGER(Periods)[i];
			SET_SLOT(swOutput_KEY_SNOWPACK, install("TimeStep"), r_SNOWPACK_PERIOD);
			UNPROTECT(1);
		} else {
			PROTECT(r_SNOWPACK_PERIOD = NEW_INTEGER(1));
			INTEGER(r_SNOWPACK_PERIOD)[0]=INTEGER(Periods)[23];
			SET_SLOT(swOutput_KEY_SNOWPACK, install("TimeStep"), r_SNOWPACK_PERIOD);
			UNPROTECT(1);
		}
		if(periodUse[23][0]) {
			if(debug) Rprintf("\tdy\n");
			PROTECT(Rsnowpack_dy = allocMatrix(REALSXP, dy_nrow, Rsnowpack_columns+2));
			PROTECT(Rsnowpack_names_dy = allocVector(VECSXP, 2));
			PROTECT(Rsnowpack_names_y_dy = allocVector(STRSXP, Rsnowpack_columns + 2));
			SET_STRING_ELT(Rsnowpack_names_y_dy, 0, mkChar("Year"));
			SET_STRING_ELT(Rsnowpack_names_y_dy, 1, mkChar("DOY"));
			for (i = 0; i < Rsnowpack_columns; i++)
				SET_STRING_ELT(Rsnowpack_names_y_dy, i + 2, mkChar(Csnowpack_names[i]));
			SET_VECTOR_ELT(Rsnowpack_names_dy, 1, Rsnowpack_names_y_dy);
			setAttrib(Rsnowpack_dy, R_DimNamesSymbol, Rsnowpack_names_dy);
			SET_SLOT(swOutput_KEY_SNOWPACK, install("Day"), Rsnowpack_dy);
			UNPROTECT(3);
		}
		if(periodUse[23][1]) {
			if(debug) Rprintf("\twk\n");
			PROTECT(Rsnowpack_wk = allocMatrix(REALSXP, wk_nrow, Rsnowpack_columns+2));
			PROTECT(Rsnowpack_names_wk = allocVector(VECSXP, 2));
			PROTECT(Rsnowpack_names_y_wk = allocVector(STRSXP, Rsnowpack_columns + 2));
			SET_STRING_ELT(Rsnowpack_names_y_wk, 0, mkChar("Year"));
			SET_STRING_ELT(Rsnowpack_names_y_wk, 1, mkChar("Week"));
			for (i = 0; i < Rsnowpack_columns; i++)
				SET_STRING_ELT(Rsnowpack_names_y_wk, i + 2, mkChar(Csnowpack_names[i]));
			SET_VECTOR_ELT(Rsnowpack_names_wk, 1, Rsnowpack_names_y_wk);
			setAttrib(Rsnowpack_wk, R_DimNamesSymbol, Rsnowpack_names_wk);
			SET_SLOT(swOutput_KEY_SNOWPACK, install("Week"), Rsnowpack_wk);
			UNPROTECT(3);
		}
		if(periodUse[23][2]) {
			if(debug) Rprintf("\tmo\n");
			PROTECT(Rsnowpack_mo = allocMatrix(REALSXP, mo_nrow, Rsnowpack_columns+2));
			PROTECT(Rsnowpack_names_mo = allocVector(VECSXP, 2));
			PROTECT(Rsnowpack_names_y_mo = allocVector(STRSXP, Rsnowpack_columns + 2));
			SET_STRING_ELT(Rsnowpack_names_y_mo, 0, mkChar("Year"));
			SET_STRING_ELT(Rsnowpack_names_y_mo, 1, mkChar("Month"));
			for (i = 0; i < Rsnowpack_columns; i++)
				SET_STRING_ELT(Rsnowpack_names_y_mo, i + 2, mkChar(Csnowpack_names[i]));
			SET_VECTOR_ELT(Rsnowpack_names_mo, 1, Rsnowpack_names_y_mo);
			setAttrib(Rsnowpack_mo, R_DimNamesSymbol, Rsnowpack_names_mo);
			SET_SLOT(swOutput_KEY_SNOWPACK, install("Month"), Rsnowpack_mo);
			UNPROTECT(3);
		}
		if(periodUse[23][3]) {
			if(debug) Rprintf("\tyr\n");
			PROTECT(Rsnowpack_yr = allocMatrix(REALSXP, yr_nrow, Rsnowpack_columns+1));
			PROTECT(Rsnowpack_names_yr = allocVector(VECSXP, 2));
			PROTECT(Rsnowpack_names_y_yr = allocVector(STRSXP, Rsnowpack_columns + 1));
			SET_STRING_ELT(Rsnowpack_names_y_yr, 0, mkChar("Year"));
			for (i = 0; i < Rsnowpack_columns; i++)
				SET_STRING_ELT(Rsnowpack_names_y_yr, i + 1, mkChar(Csnowpack_names[i]));
			SET_VECTOR_ELT(Rsnowpack_names_yr, 1, Rsnowpack_names_y_yr);
			setAttrib(Rsnowpack_yr, R_DimNamesSymbol, Rsnowpack_names_yr);
			SET_SLOT(swOutput_KEY_SNOWPACK, install("Year"), Rsnowpack_yr);
			UNPROTECT(3);
		}
		PROTECT(r_SNOWPACK_COLUMNS = NEW_INTEGER(1));
		INTEGER(r_SNOWPACK_COLUMNS)[0]=Rsnowpack_columns;
		SET_SLOT(swOutput_KEY_SNOWPACK, install("Columns"), r_SNOWPACK_COLUMNS);
		SET_SLOT(swOutput_Object, install(cSWoutput_Names[27]), swOutput_KEY_SNOWPACK);
		UNPROTECT(3);
	}
	//DEEP SWC
	if(use[24]) {
		if(debug) Rprintf("%s\n",cSWoutput_KEY_Titles[24]);
		PROTECT(swOutput_KEY_DEEPSWC = NEW_OBJECT(swOutput_KEY));
		PROTECT(r_DEEPSWC_NAME = NEW_STRING(1));
		SET_STRING_ELT(r_DEEPSWC_NAME, 0, mkChar(cSWoutput_KEY_Titles[24]));
		SET_SLOT(swOutput_KEY_DEEPSWC, install("Title"), r_DEEPSWC_NAME);
		if (useTimeStep) {
			PROTECT(r_DEEPSWC_PERIOD = NEW_INTEGER(length(Periods)));
			for(i=0; i<length(Periods); i++)
				INTEGER(r_DEEPSWC_PERIOD)[i]=INTEGER(Periods)[i];
			SET_SLOT(swOutput_KEY_DEEPSWC, install("TimeStep"), r_DEEPSWC_PERIOD);
			UNPROTECT(1);
		} else {
			PROTECT(r_DEEPSWC_PERIOD = NEW_INTEGER(1));
			INTEGER(r_DEEPSWC_PERIOD)[0]=INTEGER(Periods)[24];
			SET_SLOT(swOutput_KEY_DEEPSWC, install("TimeStep"), r_DEEPSWC_PERIOD);
			UNPROTECT(1);
		}
		if(periodUse[24][0]) {
			if(debug) Rprintf("\tdy\n");
			PROTECT(Rdeedrain_dy = allocMatrix(REALSXP, dy_nrow, Rdeedrain_columns+2));
			PROTECT(Rdeep_drain_names_dy = allocVector(VECSXP, 2));
			PROTECT(Rdeep_drain_names_y_dy = allocVector(STRSXP, Rdeedrain_columns + 2));
			SET_STRING_ELT(Rdeep_drain_names_y_dy, 0, mkChar("Year"));
			SET_STRING_ELT(Rdeep_drain_names_y_dy, 1, mkChar("DOY"));
			SET_STRING_ELT(Rdeep_drain_names_y_dy, 2, mkChar("lowLayerDrain_cm"));
			SET_VECTOR_ELT(Rdeep_drain_names_dy, 1, Rdeep_drain_names_y_dy);
			setAttrib(Rdeedrain_dy, R_DimNamesSymbol, Rdeep_drain_names_dy);
			SET_SLOT(swOutput_KEY_DEEPSWC, install("Day"), Rdeedrain_dy);
			UNPROTECT(3);
		}
		if(periodUse[24][1]) {
			if(debug) Rprintf("\twk\n");
			PROTECT(Rdeedrain_wk = allocMatrix(REALSXP, wk_nrow, Rdeedrain_columns+2));
			PROTECT(Rdeep_drain_names_wk = allocVector(VECSXP, 2));
			PROTECT(Rdeep_drain_names_y_wk = allocVector(STRSXP, Rdeedrain_columns + 2));
			SET_STRING_ELT(Rdeep_drain_names_y_wk, 0, mkChar("Year"));
			SET_STRING_ELT(Rdeep_drain_names_y_wk, 1, mkChar("Week"));
			SET_STRING_ELT(Rdeep_drain_names_y_wk, 2, mkChar("lowLayerDrain_cm"));
			SET_VECTOR_ELT(Rdeep_drain_names_wk, 1, Rdeep_drain_names_y_wk);
			setAttrib(Rdeedrain_wk, R_DimNamesSymbol, Rdeep_drain_names_wk);
			SET_SLOT(swOutput_KEY_DEEPSWC, install("Week"), Rdeedrain_wk);
			UNPROTECT(3);
		}
		if(periodUse[24][2]) {
			if(debug) Rprintf("\tmo\n");
			PROTECT(Rdeedrain_mo = allocMatrix(REALSXP, mo_nrow, Rdeedrain_columns+2));
			PROTECT(Rdeep_drain_names_mo = allocVector(VECSXP, 2));
			PROTECT(Rdeep_drain_names_y_mo = allocVector(STRSXP, Rdeedrain_columns + 2));
			SET_STRING_ELT(Rdeep_drain_names_y_mo, 0, mkChar("Year"));
			SET_STRING_ELT(Rdeep_drain_names_y_mo, 1, mkChar("Month"));
			SET_STRING_ELT(Rdeep_drain_names_y_mo, 2, mkChar("lowLayerDrain_cm"));
			SET_VECTOR_ELT(Rdeep_drain_names_mo, 1, Rdeep_drain_names_y_mo);
			setAttrib(Rdeedrain_mo, R_DimNamesSymbol, Rdeep_drain_names_mo);
			SET_SLOT(swOutput_KEY_DEEPSWC, install("Month"), Rdeedrain_mo);
			UNPROTECT(3);
		}
		if(periodUse[24][3]) {
			if(debug) Rprintf("\tyr\n");
			PROTECT(Rdeedrain_yr = allocMatrix(REALSXP, yr_nrow, Rdeedrain_columns+1));
			PROTECT(Rdeep_drain_names_yr = allocVector(VECSXP, 2));
			PROTECT(Rdeep_drain_names_y_yr = allocVector(STRSXP, Rdeedrain_columns + 1));
			SET_STRING_ELT(Rdeep_drain_names_y_yr, 0, mkChar("Year"));
			SET_STRING_ELT(Rdeep_drain_names_y_yr, 1, mkChar("lowLayerDrain_cm"));
			SET_VECTOR_ELT(Rdeep_drain_names_yr, 1, Rdeep_drain_names_y_yr);
			setAttrib(Rdeedrain_yr, R_DimNamesSymbol, Rdeep_drain_names_yr);
			SET_SLOT(swOutput_KEY_DEEPSWC, install("Year"), Rdeedrain_yr);
			UNPROTECT(3);
		}
		PROTECT(r_DEEPSWC_COLUMNS = NEW_INTEGER(1));
		INTEGER(r_DEEPSWC_COLUMNS)[0]=Rdeedrain_columns;
		SET_SLOT(swOutput_KEY_DEEPSWC, install("Columns"), r_DEEPSWC_COLUMNS);
		SET_SLOT(swOutput_Object, install(cSWoutput_Names[28]), swOutput_KEY_DEEPSWC);
		UNPROTECT(3);
	}
	//Soil Temp
	if(use[25]) {
		if(debug) Rprintf("%s\n",cSWoutput_KEY_Titles[25]);
		PROTECT(swOutput_KEY_SOILTEMP = NEW_OBJECT(swOutput_KEY));
		PROTECT(r_SOILTEMP_NAME = NEW_STRING(1));
		SET_STRING_ELT(r_SOILTEMP_NAME, 0, mkChar(cSWoutput_KEY_Titles[25]));
		SET_SLOT(swOutput_KEY_SOILTEMP, install("Title"), r_SOILTEMP_NAME);
		if (useTimeStep) {
			PROTECT(r_SOILTEMP_PERIOD = NEW_INTEGER(length(Periods)));
			for(i=0; i<length(Periods); i++)
				INTEGER(r_SOILTEMP_PERIOD)[i]=INTEGER(Periods)[i];
			SET_SLOT(swOutput_KEY_SOILTEMP, install("TimeStep"), r_SOILTEMP_PERIOD);
			UNPROTECT(1);
		} else {
			PROTECT(r_SOILTEMP_PERIOD = NEW_INTEGER(1));
			INTEGER(r_SOILTEMP_PERIOD)[0]=INTEGER(Periods)[25];
			SET_SLOT(swOutput_KEY_SOILTEMP, install("TimeStep"), r_SOILTEMP_PERIOD);
			UNPROTECT(1);
		}
		if(periodUse[25][0]) {
			if(debug) Rprintf("\tdy\n");
			PROTECT(Rsoil_temp_dy = allocMatrix(REALSXP, dy_nrow, Rsoil_temp_columns+2));
			PROTECT(Rsoil_temp_names_dy = allocVector(VECSXP, 2));
			PROTECT(Rsoil_temp_names_y_dy = allocVector(STRSXP, Rsoil_temp_columns + 2));
			SET_STRING_ELT(Rsoil_temp_names_y_dy, 0, mkChar("Year"));
			SET_STRING_ELT(Rsoil_temp_names_y_dy, 1, mkChar("DOY"));
			for (i = 0; i < tLayers; i++)
				SET_STRING_ELT(Rsoil_temp_names_y_dy, i + 2, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(Rsoil_temp_names_dy, 1, Rsoil_temp_names_y_dy);
			setAttrib(Rsoil_temp_dy, R_DimNamesSymbol, Rsoil_temp_names_dy);
			SET_SLOT(swOutput_KEY_SOILTEMP, install("Day"), Rsoil_temp_dy);
			UNPROTECT(3);
		}
		if(periodUse[25][1]) {
			if(debug) Rprintf("\twk\n");
			PROTECT(Rsoil_temp_wk = allocMatrix(REALSXP, wk_nrow, Rsoil_temp_columns+2));
			PROTECT(Rsoil_temp_names_wk = allocVector(VECSXP, 2));
			PROTECT(Rsoil_temp_names_y_wk = allocVector(STRSXP, Rsoil_temp_columns + 2));
			SET_STRING_ELT(Rsoil_temp_names_y_wk, 0, mkChar("Year"));
			SET_STRING_ELT(Rsoil_temp_names_y_wk, 1, mkChar("Week"));
			for (i = 0; i < tLayers; i++)
				SET_STRING_ELT(Rsoil_temp_names_y_wk, i + 2, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(Rsoil_temp_names_wk, 1, Rsoil_temp_names_y_wk);
			setAttrib(Rsoil_temp_wk, R_DimNamesSymbol, Rsoil_temp_names_wk);
			SET_SLOT(swOutput_KEY_SOILTEMP, install("Week"), Rsoil_temp_wk);
			UNPROTECT(3);
		}
		if(periodUse[25][2]) {
			if(debug) Rprintf("\tmo\n");
			PROTECT(Rsoil_temp_mo = allocMatrix(REALSXP, mo_nrow, Rsoil_temp_columns+2));
			PROTECT(Rsoil_temp_names_mo = allocVector(VECSXP, 2));
			PROTECT(Rsoil_temp_names_y_mo = allocVector(STRSXP, Rsoil_temp_columns + 2));
			SET_STRING_ELT(Rsoil_temp_names_y_mo, 0, mkChar("Year"));
			SET_STRING_ELT(Rsoil_temp_names_y_mo, 1, mkChar("Month"));
			for (i = 0; i < tLayers; i++)
				SET_STRING_ELT(Rsoil_temp_names_y_mo, i + 2, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(Rsoil_temp_names_mo, 1, Rsoil_temp_names_y_mo);
			setAttrib(Rsoil_temp_mo, R_DimNamesSymbol, Rsoil_temp_names_mo);
			SET_SLOT(swOutput_KEY_SOILTEMP, install("Month"), Rsoil_temp_mo);
			UNPROTECT(3);
		}
		if(periodUse[25][3]) {
			if(debug) Rprintf("\tyr\n");
			PROTECT(Rsoil_temp_yr = allocMatrix(REALSXP, yr_nrow, Rsoil_temp_columns+1));
			PROTECT(Rsoil_temp_names_yr = allocVector(VECSXP, 2));
			PROTECT(Rsoil_temp_names_y_yr = allocVector(STRSXP, Rsoil_temp_columns + 1));
			SET_STRING_ELT(Rsoil_temp_names_y_yr, 0, mkChar("Year"));
			for (i = 0; i < tLayers; i++)
				SET_STRING_ELT(Rsoil_temp_names_y_yr, i + 1, mkChar(Layers_names[i]));
			SET_VECTOR_ELT(Rsoil_temp_names_yr, 1, Rsoil_temp_names_y_yr);
			setAttrib(Rsoil_temp_yr, R_DimNamesSymbol, Rsoil_temp_names_yr);
			SET_SLOT(swOutput_KEY_SOILTEMP, install("Year"), Rsoil_temp_yr);
			UNPROTECT(3);
		}

		PROTECT(r_SOILTEMP_COLUMNS = NEW_INTEGER(1));
		INTEGER(r_SOILTEMP_COLUMNS)[0]=Rsoil_temp_columns;
		SET_SLOT(swOutput_KEY_SOILTEMP, install("Columns"), r_SOILTEMP_COLUMNS);
		SET_SLOT(swOutput_Object, install(cSWoutput_Names[29]), swOutput_KEY_SOILTEMP);
		UNPROTECT(3);
	}
	//ALL VEG
	if(use[26]) {
		if(debug) Rprintf("%s\n",cSWoutput_KEY_Titles[26]);
		PROTECT(swOutput_KEY_ALLVEG = NEW_OBJECT(swOutput_KEY));
		PROTECT(r_ALLVEG_NAME = NEW_STRING(1));
		SET_STRING_ELT(r_ALLVEG_NAME, 0, mkChar(cSWoutput_KEY_Titles[26]));
		SET_SLOT(swOutput_KEY_ALLVEG, install("Title"), r_ALLVEG_NAME);
		if (useTimeStep) {
			PROTECT(r_ALLVEG_PERIOD = NEW_INTEGER(length(Periods)));
			for(i=0; i<length(Periods); i++)
				INTEGER(r_ALLVEG_PERIOD)[i]=INTEGER(Periods)[i];
			SET_SLOT(swOutput_KEY_ALLVEG, install("TimeStep"), r_ALLVEG_PERIOD);
			UNPROTECT(1);
		} else {
			PROTECT(r_ALLVEG_PERIOD = NEW_INTEGER(1));
			INTEGER(r_ALLVEG_PERIOD)[0]=INTEGER(Periods)[26];
			SET_SLOT(swOutput_KEY_ALLVEG, install("TimeStep"), r_ALLVEG_PERIOD);
			UNPROTECT(1);
		}
		if(periodUse[26][0]) {
			PROTECT(Rallveg_dy = allocMatrix(REALSXP, dy_nrow, Rallveg_columns+2));

			SET_SLOT(swOutput_KEY_ALLVEG, install("Day"), Rallveg_dy);
			UNPROTECT(1);
		}
		if(periodUse[26][1]) {
			PROTECT(Rallveg_wk = allocMatrix(REALSXP, wk_nrow, Rallveg_columns+2));

			SET_SLOT(swOutput_KEY_ALLVEG, install("Week"), Rallveg_wk);
			UNPROTECT(1);
		}
		if(periodUse[26][2]) {
			PROTECT(Rallveg_mo = allocMatrix(REALSXP, mo_nrow, Rallveg_columns+2));

			SET_SLOT(swOutput_KEY_ALLVEG, install("Month"), Rallveg_mo);
			UNPROTECT(1);
		}
		if(periodUse[26][3]) {
			PROTECT(Rallveg_yr = allocMatrix(REALSXP, yr_nrow, Rallveg_columns+1));

			SET_SLOT(swOutput_KEY_ALLVEG, install("Year"), Rallveg_yr);
			UNPROTECT(1);
		}
		PROTECT(r_ALLVEG_COLUMNS = NEW_INTEGER(1));
		INTEGER(r_ALLVEG_COLUMNS)[0]=Rallveg_columns;
		SET_SLOT(swOutput_KEY_ALLVEG, install("Columns"), r_ALLVEG_COLUMNS);
		SET_SLOT(swOutput_Object, install(cSWoutput_Names[30]), swOutput_KEY_ALLVEG);
		UNPROTECT(3);
	}
	//ESTABL
	if(use[27]) {
		if(debug) Rprintf("%s\n",cSWoutput_KEY_Titles[27]);
		PROTECT(swOutput_KEY_ESTABL = NEW_OBJECT(swOutput_KEY));
		PROTECT(r_ESTABL_NAME = NEW_STRING(1));
		SET_STRING_ELT(r_ESTABL_NAME, 0, mkChar(cSWoutput_KEY_Titles[27]));
		SET_SLOT(swOutput_KEY_ESTABL, install("Title"), r_ESTABL_NAME);
		if (useTimeStep) {
			PROTECT(r_ESTABL_PERIOD = NEW_INTEGER(length(Periods)));
			for(i=0; i<length(Periods); i++)
				INTEGER(r_ESTABL_PERIOD)[i]=INTEGER(Periods)[i];
			SET_SLOT(swOutput_KEY_ESTABL, install("TimeStep"), r_ESTABL_PERIOD);
			UNPROTECT(1);
		} else {
			PROTECT(r_ESTABL_PERIOD = NEW_INTEGER(1));
			INTEGER(r_ESTABL_PERIOD)[0]=INTEGER(Periods)[27];
			SET_SLOT(swOutput_KEY_ESTABL, install("TimeStep"), r_ESTABL_PERIOD);
			UNPROTECT(1);
		}
		if(periodUse[27][0]) {
			if(debug) Rprintf("\tdy\n");
			PROTECT(Restabs_dy = allocMatrix(REALSXP, dy_nrow, Restabs_columns+2));
			PROTECT(Restabs_names_dy = allocVector(VECSXP, 2));
			PROTECT(Restabs_names_y_dy = allocVector(STRSXP, Restabs_columns+2));
			SET_STRING_ELT(Restabs_names_y_dy, 0, mkChar("Year"));
			SET_STRING_ELT(Restabs_names_y_dy, 1, mkChar("DOY"));
			SET_STRING_ELT(Restabs_names_y_dy, 2, mkChar("YearlyEstabResults"));
			SET_VECTOR_ELT(Restabs_names_dy, 1, Restabs_names_y_dy);
			setAttrib(Restabs_dy, R_DimNamesSymbol, Restabs_names_dy);
			SET_SLOT(swOutput_KEY_ESTABL, install("Day"), Restabs_dy);
			UNPROTECT(3);
		}
		if(periodUse[27][1]) {
			if(debug) Rprintf("\twk\n");
			PROTECT(Restabs_wk = allocMatrix(REALSXP, wk_nrow, Restabs_columns+2));
			 PROTECT(Restabs_names_wk = allocVector(VECSXP, 2));
			 PROTECT(Restabs_names_y_wk = allocVector(STRSXP, Restabs_columns+2));
			 SET_STRING_ELT(Restabs_names_y_wk, 0, mkChar("Year"));
			 SET_STRING_ELT(Restabs_names_y_wk, 1, mkChar("Week"));
			 SET_STRING_ELT(Restabs_names_y_wk, 2, mkChar("YearlyEstabResults"));
			 SET_VECTOR_ELT(Restabs_names_wk, 1, Restabs_names_y_wk);
			setAttrib(Restabs_wk, R_DimNamesSymbol, Restabs_names_wk);
			SET_SLOT(swOutput_KEY_ESTABL, install("Week"), Restabs_wk);
			UNPROTECT(3);
		}
		if(periodUse[27][2]) {
			if(debug) Rprintf("\tmo\n");
			PROTECT(Restabs_mo = allocMatrix(REALSXP, mo_nrow, Restabs_columns+2));
			PROTECT(Restabs_names_mo = allocVector(VECSXP, 2));
			PROTECT(Restabs_names_y_mo = allocVector(STRSXP, Restabs_columns+2));
			SET_STRING_ELT(Restabs_names_y_mo, 0, mkChar("Year"));
			SET_STRING_ELT(Restabs_names_y_mo, 1, mkChar("Month"));
			SET_STRING_ELT(Restabs_names_y_mo, 2, mkChar("YearlyEstabResults"));
			SET_VECTOR_ELT(Restabs_names_mo, 1, Restabs_names_y_mo);
			setAttrib(Restabs_mo, R_DimNamesSymbol, Restabs_names_mo);
			SET_SLOT(swOutput_KEY_ESTABL, install("Month"), Restabs_mo);
			UNPROTECT(3);
		}
		if(periodUse[27][3]) {
			if(debug) Rprintf("\tyr\n");
			PROTECT(Restabs_yr = allocMatrix(REALSXP, yr_nrow, Restabs_columns+1));
			PROTECT(Restabs_names_yr = allocVector(VECSXP, 2));
			PROTECT(Restabs_names_y_yr = allocVector(STRSXP, Restabs_columns+1));
			SET_STRING_ELT(Restabs_names_y_yr, 0, mkChar("Year"));
			SET_STRING_ELT(Restabs_names_y_yr, 1, mkChar("YearlyEstabResults"));
			SET_VECTOR_ELT(Restabs_names_yr, 1, Restabs_names_y_yr);
			setAttrib(Restabs_yr, R_DimNamesSymbol, Restabs_names_yr);
			SET_SLOT(swOutput_KEY_ESTABL, install("Year"), Restabs_yr);
			UNPROTECT(3);
		}
		PROTECT(r_ESTABL_COLUMNS = NEW_INTEGER(1));
		INTEGER(r_ESTABL_COLUMNS)[0]=Restabs_columns;
		SET_SLOT(swOutput_KEY_ESTABL, install("Columns"), r_ESTABL_COLUMNS);
		SET_SLOT(swOutput_Object, install(cSWoutput_Names[31]), swOutput_KEY_ESTABL);
		UNPROTECT(3);
	}

	UNPROTECT(pCount);
	return swOutput_Object;
}

#endif
