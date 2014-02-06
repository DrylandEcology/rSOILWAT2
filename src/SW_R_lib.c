/*
 * SW_R_lib.c
 *
 *  Created on: Jun 25, 2013
 *      Author: Ryan Murphy
 */

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
		*p_Rpet_yr, *p_Rprecip_yr, *p_Rrunoff_yr, *p_Rsnowpack_yr, *p_Rsoil_temp_yr, *p_Rsurface_water_yr, *p_Rsw_pot_yr, *p_Rswa_yr, *p_Rswc_yr, *p_Rtemp_yr, *p_Rtransp_yr,
		*p_Rvwc_yr, *p_Rwetdays_yr;
RealD *p_Raet_mo, *p_Rdeep_drain_mo, *p_Restabs_mo, *p_Revap_soil_mo, *p_Revap_surface_mo, *p_Rhydred_mo, *p_Rinfiltration_mo, *p_Rinterception_mo, *p_Rpercolation_mo,
		*p_Rpet_mo, *p_Rprecip_mo, *p_Rrunoff_mo, *p_Rsnowpack_mo, *p_Rsoil_temp_mo, *p_Rsurface_water_mo, *p_Rsw_pot_mo, *p_Rswa_mo, *p_Rswc_mo, *p_Rtemp_mo, *p_Rtransp_mo,
		*p_Rvwc_mo, *p_Rwetdays_mo;
RealD *p_Raet_wk, *p_Rdeep_drain_wk, *p_Restabs_wk, *p_Revap_soil_wk, *p_Revap_surface_wk, *p_Rhydred_wk, *p_Rinfiltration_wk, *p_Rinterception_wk, *p_Rpercolation_wk,
		*p_Rpet_wk, *p_Rprecip_wk, *p_Rrunoff_wk, *p_Rsnowpack_wk, *p_Rsoil_temp_wk, *p_Rsurface_water_wk, *p_Rsw_pot_wk, *p_Rswa_wk, *p_Rswc_wk, *p_Rtemp_wk, *p_Rtransp_wk,
		*p_Rvwc_wk, *p_Rwetdays_wk;
RealD *p_Raet_dy, *p_Rdeep_drain_dy, *p_Restabs_dy, *p_Revap_soil_dy, *p_Revap_surface_dy, *p_Rhydred_dy, *p_Rinfiltration_dy, *p_Rinterception_dy, *p_Rpercolation_dy,
		*p_Rpet_dy, *p_Rprecip_dy, *p_Rrunoff_dy, *p_Rsnowpack_dy, *p_Rsoil_temp_dy, *p_Rsurface_water_dy, *p_Rsw_pot_dy, *p_Rswa_dy, *p_Rswc_dy, *p_Rtemp_dy, *p_Rtransp_dy,
		*p_Rvwc_dy, *p_Rwetdays_dy;
unsigned int yr_nrow = 0, mo_nrow = 0, wk_nrow = 0, dy_nrow = 0;

extern SW_MODEL SW_Model;
extern SW_SITE SW_Site;
extern SW_VEGESTAB SW_VegEstab;
extern int timeSteps[4];
extern int numPeriod;
extern char _firstfile[1024];

/* =================================================== */
/*                Module-Level Declarations            */
/* --------------------------------------------------- */

static int p_Raet_columns, p_Rdeep_drain_columns, p_Restabs_columns, p_Revap_soil_columns, p_Revap_surface_columns, p_Rhydred_columns, p_Rinfiltration_columns,
		p_Rinterception_columns, p_Rpercolation_columns, p_Rpet_columns, p_Rprecip_columns, p_Rrunoff_columns, p_Rsnowpack_columns, p_Rsoil_temp_columns,
		p_Rsurface_water_columns, p_Rsw_pot_columns, p_Rswa_columns, p_Rswc_columns, p_Rtemp_columns, p_Rtransp_columns, p_Rvwc_columns, p_Rwetdays_columns;

static int tLayers;

void SW_FLW_construct(void);

SEXP onGetInputDataFromFiles(SEXP inputOptions) {
	int i;
	SEXP swInputData;
	SEXP SW_DataList;
	SEXP swLog;
	SEXP oRlogfile;
	char *ListNames[] = {"files.in", "years.in", "weathersetup.in", "prod.in", "site.in","estab.in","outsetup.in","swcsetup.in","LogFile"};

	logged = swFALSE;
	logfp = stdout;
	int argc = length(inputOptions);
	char *argv[7];
	collectInData = swTRUE;
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

	UNPROTECT(6);
	return SW_DataList;
}

SEXP start(SEXP inputOptions, SEXP inputData, SEXP weatherList) {
	int tYears = 0, tevapLayers = 0, tVegEstabCount = 0, pYearUse = 0, pMonthUse = 0, pWeekUse = 0, pDayUse = 0;
	int i;

	tLayers = 0;
	//Main Output
	SEXP OutputList;
	SEXP swLog;
	SEXP oRlogfile;

	//SUB LIST
	SEXP Raet, Rdeep_drain, Restabs, Revap_soil, Revap_surface, Rhydred, Rinfiltration, Rinterception, Rpercolation, Rpet, Rprecip, Rrunoff, Rsnowpack, Rsoil_temp,
			Rsurface_water, Rsw_pot, Rswa, Rswc, Rtemp, Rtransp, Rvwc, Rwetdays; //List Elements

	//SUB LIST NUMERICS
	SEXP Raet_dy, Rdeep_drain_dy, Restabs_dy, Revap_soil_dy, Revap_surface_dy, Rhydred_dy, Rinfiltration_dy, Rinterception_dy, Rpercolation_dy, Rpet_dy, Rprecip_dy,
			Rrunoff_dy, Rsnowpack_dy, Rsoil_temp_dy, Rsurface_water_dy, Rsw_pot_dy, Rswa_dy, Rswc_dy, Rtemp_dy, Rtransp_dy, Rvwc_dy, Rwetdays_dy;
	SEXP Raet_wk, Rdeep_drain_wk, Restabs_wk, Revap_soil_wk, Revap_surface_wk, Rhydred_wk, Rinfiltration_wk, Rinterception_wk, Rpercolation_wk, Rpet_wk, Rprecip_wk,
			Rrunoff_wk, Rsnowpack_wk, Rsoil_temp_wk, Rsurface_water_wk, Rsw_pot_wk, Rswa_wk, Rswc_wk, Rtemp_wk, Rtransp_wk, Rvwc_wk, Rwetdays_wk;
	SEXP Raet_mo, Rdeep_drain_mo, Restabs_mo, Revap_soil_mo, Revap_surface_mo, Rhydred_mo, Rinfiltration_mo, Rinterception_mo, Rpercolation_mo, Rpet_mo, Rprecip_mo,
			Rrunoff_mo, Rsnowpack_mo, Rsoil_temp_mo, Rsurface_water_mo, Rsw_pot_mo, Rswa_mo, Rswc_mo, Rtemp_mo, Rtransp_mo, Rvwc_mo, Rwetdays_mo;
	SEXP Raet_yr, Rdeep_drain_yr, Restabs_yr, Revap_soil_yr, Revap_surface_yr, Rhydred_yr, Rinfiltration_yr, Rinterception_yr, Rpercolation_yr, Rpet_yr, Rprecip_yr,
			Rrunoff_yr, Rsnowpack_yr, Rsoil_temp_yr, Rsurface_water_yr, Rsw_pot_yr, Rswa_yr, Rswc_yr, Rtemp_yr, Rtransp_yr, Rvwc_yr, Rwetdays_yr;

	logged = swFALSE;
	logfp = stdout;
	int argc = length(inputOptions);
	char *argv[7];
	collectInData = swFALSE;
	if(isNull(inputData))
		useFiles = swTRUE;
	else {
		useFiles = swFALSE;
		InputData = inputData;
	}
	//This is used to minimize copying weather data between similiar runs.
	if(isNull(weatherList)) {
		bWeatherList = swFALSE;
	} else {
		bWeatherList = swTRUE;
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

	init_args(argc, argv);
	SW_CTL_init_model(_firstfile);

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

	// Number of Columns for outputs;
	p_Raet_columns = 1;
	p_Rdeep_drain_columns = 1;
	p_Restabs_columns = tVegEstabCount;
	p_Revap_soil_columns = tevapLayers;
	p_Revap_surface_columns = 6;
	p_Rhydred_columns = tLayers * 4;
	p_Rinfiltration_columns = 1;
	p_Rinterception_columns = 5;
	p_Rpercolation_columns = tLayers - 1;
	p_Rpet_columns = 1;
	p_Rprecip_columns = 5;
	p_Rrunoff_columns = 3;
	p_Rsnowpack_columns = 2;
	p_Rsoil_temp_columns = tLayers;
	p_Rsurface_water_columns = 1;
	p_Rsw_pot_columns = tLayers;
	p_Rswa_columns = tLayers;
	p_Rswc_columns = tLayers;
	p_Rtemp_columns = 3;
	p_Rtransp_columns = tLayers * 4;
	p_Rvwc_columns = tLayers;
	p_Rwetdays_columns = tLayers;

	//RealD Year:
	PROTECT(Raet_yr = allocMatrix(REALSXP, yr_nrow, p_Raet_columns + 1));
	p_Raet_yr = REAL(Raet_yr);
	PROTECT(Rdeep_drain_yr = allocMatrix(REALSXP, yr_nrow, p_Rdeep_drain_columns + 1));
	p_Rdeep_drain_yr = REAL(Rdeep_drain_yr);
	if (p_Restabs_columns == 0)
		PROTECT(Restabs_yr = allocMatrix(REALSXP, yr_nrow * 0, p_Restabs_columns + 1));
	else
		PROTECT(Restabs_yr = allocMatrix(REALSXP, yr_nrow, p_Restabs_columns + 1));
	p_Restabs_yr = REAL(Restabs_yr);
	PROTECT(Revap_soil_yr = allocMatrix(REALSXP, yr_nrow, p_Revap_soil_columns + 1));
	p_Revap_soil_yr = REAL(Revap_soil_yr);
	PROTECT(Revap_surface_yr = allocMatrix(REALSXP, yr_nrow, p_Revap_surface_columns + 1));
	p_Revap_surface_yr = REAL(Revap_surface_yr);
	PROTECT(Rhydred_yr = allocMatrix(REALSXP, yr_nrow, p_Rhydred_columns + 1));
	p_Rhydred_yr = REAL(Rhydred_yr);
	PROTECT(Rinfiltration_yr = allocMatrix(REALSXP, yr_nrow, p_Rinfiltration_columns + 1));
	p_Rinfiltration_yr = REAL(Rinfiltration_yr);
	PROTECT(Rinterception_yr = allocMatrix(REALSXP, yr_nrow, p_Rinterception_columns + 1));
	p_Rinterception_yr = REAL(Rinterception_yr);
	PROTECT(Rpercolation_yr = allocMatrix(REALSXP, yr_nrow, p_Rpercolation_columns + 1));
	p_Rpercolation_yr = REAL(Rpercolation_yr);
	PROTECT(Rpet_yr = allocMatrix(REALSXP, yr_nrow, p_Rpet_columns + 1));
	p_Rpet_yr = REAL(Rpet_yr);
	PROTECT(Rprecip_yr = allocMatrix(REALSXP, yr_nrow, p_Rprecip_columns + 1));
	p_Rprecip_yr = REAL(Rprecip_yr);
	PROTECT(Rrunoff_yr = allocMatrix(REALSXP, yr_nrow, p_Rrunoff_columns + 1));
	p_Rrunoff_yr = REAL(Rrunoff_yr);
	PROTECT(Rsnowpack_yr = allocMatrix(REALSXP, yr_nrow, p_Rsnowpack_columns + 1));
	p_Rsnowpack_yr = REAL(Rsnowpack_yr);
	PROTECT(Rsoil_temp_yr = allocMatrix(REALSXP, yr_nrow, p_Rsoil_temp_columns + 1));
	p_Rsoil_temp_yr = REAL(Rsoil_temp_yr);
	PROTECT(Rsurface_water_yr = allocMatrix(REALSXP, yr_nrow, p_Rsurface_water_columns + 1));
	p_Rsurface_water_yr = REAL(Rsurface_water_yr);
	PROTECT(Rsw_pot_yr = allocMatrix(REALSXP, yr_nrow, p_Rsw_pot_columns + 1));
	p_Rsw_pot_yr = REAL(Rsw_pot_yr);
	PROTECT(Rswa_yr = allocMatrix(REALSXP, yr_nrow, p_Rswa_columns + 1));
	p_Rswa_yr = REAL(Rswa_yr);
	PROTECT(Rswc_yr = allocMatrix(REALSXP, yr_nrow, p_Rswc_columns + 1));
	p_Rswc_yr = REAL(Rswc_yr);
	PROTECT(Rtemp_yr = allocMatrix(REALSXP, yr_nrow, p_Rtemp_columns + 1));
	p_Rtemp_yr = REAL(Rtemp_yr);
	PROTECT(Rtransp_yr = allocMatrix(REALSXP, yr_nrow, p_Rtransp_columns + 1));
	p_Rtransp_yr = REAL(Rtransp_yr);
	PROTECT(Rvwc_yr = allocMatrix(REALSXP, yr_nrow, p_Rvwc_columns + 1));
	p_Rvwc_yr = REAL(Rvwc_yr);
	PROTECT(Rwetdays_yr = allocMatrix(REALSXP, yr_nrow, p_Rwetdays_columns + 1));
	p_Rwetdays_yr = REAL(Rwetdays_yr);

	//RealD mo
	PROTECT(Raet_mo = allocMatrix(REALSXP, mo_nrow, p_Raet_columns + 2));
	p_Raet_mo = REAL(Raet_mo);
	PROTECT(Rdeep_drain_mo = allocMatrix(REALSXP, mo_nrow, p_Rdeep_drain_columns + 2));
	p_Rdeep_drain_mo = REAL(Rdeep_drain_mo);
	PROTECT(Restabs_mo = allocMatrix(REALSXP, mo_nrow * 0, p_Restabs_columns + 2));
	p_Restabs_mo = REAL(Restabs_mo);
	PROTECT(Revap_soil_mo = allocMatrix(REALSXP, mo_nrow, p_Revap_soil_columns + 2));
	p_Revap_soil_mo = REAL(Revap_soil_mo);
	PROTECT(Revap_surface_mo = allocMatrix(REALSXP, mo_nrow, p_Revap_surface_columns + 2));
	p_Revap_surface_mo = REAL(Revap_surface_mo);
	PROTECT(Rhydred_mo = allocMatrix(REALSXP, mo_nrow, p_Rhydred_columns + 2));
	p_Rhydred_mo = REAL(Rhydred_mo);
	PROTECT(Rinfiltration_mo = allocMatrix(REALSXP, mo_nrow, p_Rinfiltration_columns + 2));
	p_Rinfiltration_mo = REAL(Rinfiltration_mo);
	PROTECT(Rinterception_mo = allocMatrix(REALSXP, mo_nrow, p_Rinterception_columns + 2));
	p_Rinterception_mo = REAL(Rinterception_mo);
	PROTECT(Rpercolation_mo = allocMatrix(REALSXP, mo_nrow, p_Rpercolation_columns + 2));
	p_Rpercolation_mo = REAL(Rpercolation_mo);
	PROTECT(Rpet_mo = allocMatrix(REALSXP, mo_nrow, p_Rpet_columns + 2));
	p_Rpet_mo = REAL(Rpet_mo);
	PROTECT(Rprecip_mo = allocMatrix(REALSXP, mo_nrow, p_Rprecip_columns + 2));
	p_Rprecip_mo = REAL(Rprecip_mo);
	PROTECT(Rrunoff_mo = allocMatrix(REALSXP, mo_nrow, p_Rrunoff_columns + 2));
	p_Rrunoff_mo = REAL(Rrunoff_mo);
	PROTECT(Rsnowpack_mo = allocMatrix(REALSXP, mo_nrow, p_Rsnowpack_columns + 2));
	p_Rsnowpack_mo = REAL(Rsnowpack_mo);
	PROTECT(Rsoil_temp_mo = allocMatrix(REALSXP, mo_nrow, p_Rsoil_temp_columns + 2));
	p_Rsoil_temp_mo = REAL(Rsoil_temp_mo);
	PROTECT(Rsurface_water_mo = allocMatrix(REALSXP, mo_nrow, p_Rsurface_water_columns + 2));
	p_Rsurface_water_mo = REAL(Rsurface_water_mo);
	PROTECT(Rsw_pot_mo = allocMatrix(REALSXP, mo_nrow, p_Rsw_pot_columns + 2));
	p_Rsw_pot_mo = REAL(Rsw_pot_mo);
	PROTECT(Rswa_mo = allocMatrix(REALSXP, mo_nrow, p_Rswa_columns + 2));
	p_Rswa_mo = REAL(Rswa_mo);
	PROTECT(Rswc_mo = allocMatrix(REALSXP, mo_nrow, p_Rswc_columns + 2));
	p_Rswc_mo = REAL(Rswc_mo);
	PROTECT(Rtemp_mo = allocMatrix(REALSXP, mo_nrow, p_Rtemp_columns + 2));
	p_Rtemp_mo = REAL(Rtemp_mo);
	PROTECT(Rtransp_mo = allocMatrix(REALSXP, mo_nrow, p_Rtransp_columns + 2));
	p_Rtransp_mo = REAL(Rtransp_mo);
	PROTECT(Rvwc_mo = allocMatrix(REALSXP, mo_nrow, p_Rvwc_columns + 2));
	p_Rvwc_mo = REAL(Rvwc_mo);
	PROTECT(Rwetdays_mo = allocMatrix(REALSXP, mo_nrow, p_Rwetdays_columns + 2));
	p_Rwetdays_mo = REAL(Rwetdays_mo);

	//RealD wk
	PROTECT(Raet_wk = allocMatrix(REALSXP, wk_nrow, p_Raet_columns + 2));
	p_Raet_wk = REAL(Raet_wk);
	PROTECT(Rdeep_drain_wk = allocMatrix(REALSXP, wk_nrow, p_Rdeep_drain_columns + 2));
	p_Rdeep_drain_wk = REAL(Rdeep_drain_wk);
	PROTECT(Restabs_wk = allocMatrix(REALSXP, wk_nrow * 0, p_Restabs_columns + 2));
	p_Restabs_wk = REAL(Restabs_wk);
	PROTECT(Revap_soil_wk = allocMatrix(REALSXP, wk_nrow, p_Revap_soil_columns + 2));
	p_Revap_soil_wk = REAL(Revap_soil_wk);
	PROTECT(Revap_surface_wk = allocMatrix(REALSXP, wk_nrow, p_Revap_surface_columns + 2));
	p_Revap_surface_wk = REAL(Revap_surface_wk);
	PROTECT(Rhydred_wk = allocMatrix(REALSXP, wk_nrow, p_Rhydred_columns + 2));
	p_Rhydred_wk = REAL(Rhydred_wk);
	PROTECT(Rinfiltration_wk = allocMatrix(REALSXP, wk_nrow, p_Rinfiltration_columns + 2));
	p_Rinfiltration_wk = REAL(Rinfiltration_wk);
	PROTECT(Rinterception_wk = allocMatrix(REALSXP, wk_nrow, p_Rinterception_columns + 2));
	p_Rinterception_wk = REAL(Rinterception_wk);
	PROTECT(Rpercolation_wk = allocMatrix(REALSXP, wk_nrow, p_Rpercolation_columns + 2));
	p_Rpercolation_wk = REAL(Rpercolation_wk);
	PROTECT(Rpet_wk = allocMatrix(REALSXP, wk_nrow, p_Rpet_columns + 2));
	p_Rpet_wk = REAL(Rpet_wk);
	PROTECT(Rprecip_wk = allocMatrix(REALSXP, wk_nrow, p_Rprecip_columns + 2));
	p_Rprecip_wk = REAL(Rprecip_wk);
	PROTECT(Rrunoff_wk = allocMatrix(REALSXP, wk_nrow, p_Rrunoff_columns + 2));
	p_Rrunoff_wk = REAL(Rrunoff_wk);
	PROTECT(Rsnowpack_wk = allocMatrix(REALSXP, wk_nrow, p_Rsnowpack_columns + 2));
	p_Rsnowpack_wk = REAL(Rsnowpack_wk);
	PROTECT(Rsoil_temp_wk = allocMatrix(REALSXP, wk_nrow, p_Rsoil_temp_columns + 2));
	p_Rsoil_temp_wk = REAL(Rsoil_temp_wk);
	PROTECT(Rsurface_water_wk = allocMatrix(REALSXP, wk_nrow, p_Rsurface_water_columns + 2));
	p_Rsurface_water_wk = REAL(Rsurface_water_wk);
	PROTECT(Rsw_pot_wk = allocMatrix(REALSXP, wk_nrow, p_Rsw_pot_columns + 2));
	p_Rsw_pot_wk = REAL(Rsw_pot_wk);
	PROTECT(Rswa_wk = allocMatrix(REALSXP, wk_nrow, p_Rswa_columns + 2));
	p_Rswa_wk = REAL(Rswa_wk);
	PROTECT(Rswc_wk = allocMatrix(REALSXP, wk_nrow, p_Rswc_columns + 2));
	p_Rswc_wk = REAL(Rswc_wk);
	PROTECT(Rtemp_wk = allocMatrix(REALSXP, wk_nrow, p_Rtemp_columns + 2));
	p_Rtemp_wk = REAL(Rtemp_wk);
	PROTECT(Rtransp_wk = allocMatrix(REALSXP, wk_nrow, p_Rtransp_columns + 2));
	p_Rtransp_wk = REAL(Rtransp_wk);
	PROTECT(Rvwc_wk = allocMatrix(REALSXP, wk_nrow, p_Rvwc_columns + 2));
	p_Rvwc_wk = REAL(Rvwc_wk);
	PROTECT(Rwetdays_wk = allocMatrix(REALSXP, wk_nrow, p_Rwetdays_columns + 2));
	p_Rwetdays_wk = REAL(Rwetdays_wk);

	//RealD dy
	PROTECT(Raet_dy = allocMatrix(REALSXP, dy_nrow, p_Raet_columns + 2));
	p_Raet_dy = REAL(Raet_dy);
	PROTECT(Rdeep_drain_dy = allocMatrix(REALSXP, dy_nrow, p_Rdeep_drain_columns + 2));
	p_Rdeep_drain_dy = REAL(Rdeep_drain_dy);
	PROTECT(Restabs_dy = allocMatrix(REALSXP, dy_nrow * 0, p_Restabs_columns + 2));
	p_Restabs_dy = REAL(Restabs_dy);
	PROTECT(Revap_soil_dy = allocMatrix(REALSXP, dy_nrow, p_Revap_soil_columns + 2));
	p_Revap_soil_dy = REAL(Revap_soil_dy);
	PROTECT(Revap_surface_dy = allocMatrix(REALSXP, dy_nrow, p_Revap_surface_columns + 2));
	p_Revap_surface_dy = REAL(Revap_surface_dy);
	PROTECT(Rhydred_dy = allocMatrix(REALSXP, dy_nrow, p_Rhydred_columns + 2));
	p_Rhydred_dy = REAL(Rhydred_dy);
	PROTECT(Rinfiltration_dy = allocMatrix(REALSXP, dy_nrow, p_Rinfiltration_columns + 2));
	p_Rinfiltration_dy = REAL(Rinfiltration_dy);
	PROTECT(Rinterception_dy = allocMatrix(REALSXP, dy_nrow, p_Rinterception_columns + 2));
	p_Rinterception_dy = REAL(Rinterception_dy);
	PROTECT(Rpercolation_dy = allocMatrix(REALSXP, dy_nrow, p_Rpercolation_columns + 2));
	p_Rpercolation_dy = REAL(Rpercolation_dy);
	PROTECT(Rpet_dy = allocMatrix(REALSXP, dy_nrow, p_Rpet_columns + 2));
	p_Rpet_dy = REAL(Rpet_dy);
	PROTECT(Rprecip_dy = allocMatrix(REALSXP, dy_nrow, p_Rprecip_columns + 2));
	p_Rprecip_dy = REAL(Rprecip_dy);
	PROTECT(Rrunoff_dy = allocMatrix(REALSXP, dy_nrow, p_Rrunoff_columns + 2));
	p_Rrunoff_dy = REAL(Rrunoff_dy);
	PROTECT(Rsnowpack_dy = allocMatrix(REALSXP, dy_nrow, p_Rsnowpack_columns + 2));
	p_Rsnowpack_dy = REAL(Rsnowpack_dy);
	PROTECT(Rsoil_temp_dy = allocMatrix(REALSXP, dy_nrow, p_Rsoil_temp_columns + 2));
	p_Rsoil_temp_dy = REAL(Rsoil_temp_dy);
	PROTECT(Rsurface_water_dy = allocMatrix(REALSXP, dy_nrow, p_Rsurface_water_columns + 2));
	p_Rsurface_water_dy = REAL(Rsurface_water_dy);
	PROTECT(Rsw_pot_dy = allocMatrix(REALSXP, dy_nrow, p_Rsw_pot_columns + 2));
	p_Rsw_pot_dy = REAL(Rsw_pot_dy);
	PROTECT(Rswa_dy = allocMatrix(REALSXP, dy_nrow, p_Rswa_columns + 2));
	p_Rswa_dy = REAL(Rswa_dy);
	PROTECT(Rswc_dy = allocMatrix(REALSXP, dy_nrow, p_Rswc_columns + 2));
	p_Rswc_dy = REAL(Rswc_dy);
	PROTECT(Rtemp_dy = allocMatrix(REALSXP, dy_nrow, p_Rtemp_columns + 2));
	p_Rtemp_dy = REAL(Rtemp_dy);
	PROTECT(Rtransp_dy = allocMatrix(REALSXP, dy_nrow, p_Rtransp_columns + 2));
	p_Rtransp_dy = REAL(Rtransp_dy);
	PROTECT(Rvwc_dy = allocMatrix(REALSXP, dy_nrow, p_Rvwc_columns + 2));
	p_Rvwc_dy = REAL(Rvwc_dy);
	PROTECT(Rwetdays_dy = allocMatrix(REALSXP, dy_nrow, p_Rwetdays_columns + 2));
	p_Rwetdays_dy = REAL(Rwetdays_dy);

	PROTECT(Raet = allocVector(VECSXP, 4));
	SET_VECTOR_ELT(Raet, 0, Raet_yr);
	SET_VECTOR_ELT(Raet, 1, Raet_mo);
	SET_VECTOR_ELT(Raet, 2, Raet_wk);
	SET_VECTOR_ELT(Raet, 3, Raet_dy);

	PROTECT(Rdeep_drain = allocVector(VECSXP, 4));
	SET_VECTOR_ELT(Rdeep_drain, 0, Rdeep_drain_yr);
	SET_VECTOR_ELT(Rdeep_drain, 1, Rdeep_drain_mo);
	SET_VECTOR_ELT(Rdeep_drain, 2, Rdeep_drain_wk);
	SET_VECTOR_ELT(Rdeep_drain, 3, Rdeep_drain_dy);

	PROTECT(Restabs = allocVector(VECSXP, 4));
	SET_VECTOR_ELT(Restabs, 0, Restabs_yr);
	SET_VECTOR_ELT(Restabs, 1, Restabs_mo);
	SET_VECTOR_ELT(Restabs, 2, Restabs_wk);
	SET_VECTOR_ELT(Restabs, 3, Restabs_dy);

	PROTECT(Revap_soil = allocVector(VECSXP, 4));
	SET_VECTOR_ELT(Revap_soil, 0, Revap_soil_yr);
	SET_VECTOR_ELT(Revap_soil, 1, Revap_soil_mo);
	SET_VECTOR_ELT(Revap_soil, 2, Revap_soil_wk);
	SET_VECTOR_ELT(Revap_soil, 3, Revap_soil_dy);

	PROTECT(Revap_surface = allocVector(VECSXP, 4));
	SET_VECTOR_ELT(Revap_surface, 0, Revap_surface_yr);
	SET_VECTOR_ELT(Revap_surface, 1, Revap_surface_mo);
	SET_VECTOR_ELT(Revap_surface, 2, Revap_surface_wk);
	SET_VECTOR_ELT(Revap_surface, 3, Revap_surface_dy);

	PROTECT(Rhydred = allocVector(VECSXP, 4));
	SET_VECTOR_ELT(Rhydred, 0, Rhydred_yr);
	SET_VECTOR_ELT(Rhydred, 1, Rhydred_mo);
	SET_VECTOR_ELT(Rhydred, 2, Rhydred_wk);
	SET_VECTOR_ELT(Rhydred, 3, Rhydred_dy);

	PROTECT(Rinfiltration = allocVector(VECSXP, 4));
	SET_VECTOR_ELT(Rinfiltration, 0, Rinfiltration_yr);
	SET_VECTOR_ELT(Rinfiltration, 1, Rinfiltration_mo);
	SET_VECTOR_ELT(Rinfiltration, 2, Rinfiltration_wk);
	SET_VECTOR_ELT(Rinfiltration, 3, Rinfiltration_dy);

	PROTECT(Rinterception = allocVector(VECSXP, 4));
	SET_VECTOR_ELT(Rinterception, 0, Rinterception_yr);
	SET_VECTOR_ELT(Rinterception, 1, Rinterception_mo);
	SET_VECTOR_ELT(Rinterception, 2, Rinterception_wk);
	SET_VECTOR_ELT(Rinterception, 3, Rinterception_dy);

	PROTECT(Rpercolation = allocVector(VECSXP, 4));
	SET_VECTOR_ELT(Rpercolation, 0, Rpercolation_yr);
	SET_VECTOR_ELT(Rpercolation, 1, Rpercolation_mo);
	SET_VECTOR_ELT(Rpercolation, 2, Rpercolation_wk);
	SET_VECTOR_ELT(Rpercolation, 3, Rpercolation_dy);

	PROTECT(Rpet = allocVector(VECSXP, 4));
	SET_VECTOR_ELT(Rpet, 0, Rpet_yr);
	SET_VECTOR_ELT(Rpet, 1, Rpet_mo);
	SET_VECTOR_ELT(Rpet, 2, Rpet_wk);
	SET_VECTOR_ELT(Rpet, 3, Rpet_dy);

	PROTECT(Rprecip = allocVector(VECSXP, 4));
	SET_VECTOR_ELT(Rprecip, 0, Rprecip_yr);
	SET_VECTOR_ELT(Rprecip, 1, Rprecip_mo);
	SET_VECTOR_ELT(Rprecip, 2, Rprecip_wk);
	SET_VECTOR_ELT(Rprecip, 3, Rprecip_dy);

	PROTECT(Rrunoff = allocVector(VECSXP, 4));
	SET_VECTOR_ELT(Rrunoff, 0, Rrunoff_yr);
	SET_VECTOR_ELT(Rrunoff, 1, Rrunoff_mo);
	SET_VECTOR_ELT(Rrunoff, 2, Rrunoff_wk);
	SET_VECTOR_ELT(Rrunoff, 3, Rrunoff_dy);

	PROTECT(Rsnowpack = allocVector(VECSXP, 4));
	SET_VECTOR_ELT(Rsnowpack, 0, Rsnowpack_yr);
	SET_VECTOR_ELT(Rsnowpack, 1, Rsnowpack_mo);
	SET_VECTOR_ELT(Rsnowpack, 2, Rsnowpack_wk);
	SET_VECTOR_ELT(Rsnowpack, 3, Rsnowpack_dy);

	PROTECT(Rsoil_temp = allocVector(VECSXP, 4));
	SET_VECTOR_ELT(Rsoil_temp, 0, Rsoil_temp_yr);
	SET_VECTOR_ELT(Rsoil_temp, 1, Rsoil_temp_mo);
	SET_VECTOR_ELT(Rsoil_temp, 2, Rsoil_temp_wk);
	SET_VECTOR_ELT(Rsoil_temp, 3, Rsoil_temp_dy);

	PROTECT(Rsurface_water = allocVector(VECSXP, 4));
	SET_VECTOR_ELT(Rsurface_water, 0, Rsurface_water_yr);
	SET_VECTOR_ELT(Rsurface_water, 1, Rsurface_water_mo);
	SET_VECTOR_ELT(Rsurface_water, 2, Rsurface_water_wk);
	SET_VECTOR_ELT(Rsurface_water, 3, Rsurface_water_dy);

	PROTECT(Rsw_pot = allocVector(VECSXP, 4));
	SET_VECTOR_ELT(Rsw_pot, 0, Rsw_pot_yr);
	SET_VECTOR_ELT(Rsw_pot, 1, Rsw_pot_mo);
	SET_VECTOR_ELT(Rsw_pot, 2, Rsw_pot_wk);
	SET_VECTOR_ELT(Rsw_pot, 3, Rsw_pot_dy);

	PROTECT(Rswa = allocVector(VECSXP, 4));
	SET_VECTOR_ELT(Rswa, 0, Rswa_yr);
	SET_VECTOR_ELT(Rswa, 1, Rswa_mo);
	SET_VECTOR_ELT(Rswa, 2, Rswa_wk);
	SET_VECTOR_ELT(Rswa, 3, Rswa_dy);

	PROTECT(Rswc = allocVector(VECSXP, 4));
	SET_VECTOR_ELT(Rswc, 0, Rswc_yr);
	SET_VECTOR_ELT(Rswc, 1, Rswc_mo);
	SET_VECTOR_ELT(Rswc, 2, Rswc_wk);
	SET_VECTOR_ELT(Rswc, 3, Rswc_dy);

	PROTECT(Rtemp = allocVector(VECSXP, 4));
	SET_VECTOR_ELT(Rtemp, 0, Rtemp_yr);
	SET_VECTOR_ELT(Rtemp, 1, Rtemp_mo);
	SET_VECTOR_ELT(Rtemp, 2, Rtemp_wk);
	SET_VECTOR_ELT(Rtemp, 3, Rtemp_dy);

	PROTECT(Rtransp = allocVector(VECSXP, 4));
	SET_VECTOR_ELT(Rtransp, 0, Rtransp_yr);
	SET_VECTOR_ELT(Rtransp, 1, Rtransp_mo);
	SET_VECTOR_ELT(Rtransp, 2, Rtransp_wk);
	SET_VECTOR_ELT(Rtransp, 3, Rtransp_dy);

	PROTECT(Rvwc = allocVector(VECSXP, 4));
	SET_VECTOR_ELT(Rvwc, 0, Rvwc_yr);
	SET_VECTOR_ELT(Rvwc, 1, Rvwc_mo);
	SET_VECTOR_ELT(Rvwc, 2, Rvwc_wk);
	SET_VECTOR_ELT(Rvwc, 3, Rvwc_dy);

	PROTECT(Rwetdays = allocVector(VECSXP, 4));
	SET_VECTOR_ELT(Rwetdays, 0, Rwetdays_yr);
	SET_VECTOR_ELT(Rwetdays, 1, Rwetdays_mo);
	SET_VECTOR_ELT(Rwetdays, 2, Rwetdays_wk);
	SET_VECTOR_ELT(Rwetdays, 3, Rwetdays_dy);

	// Creating a list with 2 vector elements:
	PROTECT(OutputList = allocVector(VECSXP, 23));
	// attaching main's elements
	SET_VECTOR_ELT(OutputList, 0, Raet);
	SET_VECTOR_ELT(OutputList, 1, Rdeep_drain);
	SET_VECTOR_ELT(OutputList, 2, Restabs);
	SET_VECTOR_ELT(OutputList, 3, Revap_soil);
	SET_VECTOR_ELT(OutputList, 4, Revap_surface);
	SET_VECTOR_ELT(OutputList, 5, Rhydred);
	SET_VECTOR_ELT(OutputList, 6, Rinfiltration);
	SET_VECTOR_ELT(OutputList, 7, Rinterception);
	SET_VECTOR_ELT(OutputList, 8, Rpercolation);
	SET_VECTOR_ELT(OutputList, 9, Rpet);
	SET_VECTOR_ELT(OutputList, 10, Rprecip);
	SET_VECTOR_ELT(OutputList, 11, Rrunoff);
	SET_VECTOR_ELT(OutputList, 12, Rsnowpack);
	SET_VECTOR_ELT(OutputList, 13, Rsoil_temp);
	SET_VECTOR_ELT(OutputList, 14, Rsurface_water);
	SET_VECTOR_ELT(OutputList, 15, Rsw_pot);
	SET_VECTOR_ELT(OutputList, 16, Rswa);
	SET_VECTOR_ELT(OutputList, 17, Rswc);
	SET_VECTOR_ELT(OutputList, 18, Rtemp);
	SET_VECTOR_ELT(OutputList, 19, Rtransp);
	SET_VECTOR_ELT(OutputList, 20, Rvwc);
	SET_VECTOR_ELT(OutputList, 21, Rwetdays);
	SET_VECTOR_ELT(OutputList, 22, oRlogfile);
	// and attaching the vector names:

	SW_CTL_main();

	SW_SIT_clear_layers();
	SW_WTH_clear_runavg_list();

	UNPROTECT(115);

	return OutputList;
}

SEXP onSetNames(SEXP SW_R_Data) {
	int i, j;

	SEXP OutputList_names;

	SEXP SubListNames;
	char *OutSubColNames[] = { "yr", "mo", "wk", "dy" };
	//names
	SEXP Raet_names_yr, Raet_names_y_yr, Rdeep_drain_names_yr, Rdeep_drain_names_y_yr, /*Restabs_names_yr, Restabs_names_y_yr,*/Revap_soil_names_yr, Revap_soil_names_y_yr,
			Revap_surface_names_yr, Revap_surface_names_y_yr, Rhydred_names_yr, Rhydred_names_y_yr, Rinfiltration_names_yr, Rinfiltration_names_y_yr, Rinterception_names_yr,
			Rinterception_names_y_yr, Rpercolation_names_yr, Rpercolation_names_y_yr, Rpet_names_yr, Rpet_names_y_yr, Rprecip_names_yr, Rprecip_names_y_yr, Rrunoff_names_yr,
			Rrunoff_names_y_yr, Rsnowpack_names_yr, Rsnowpack_names_y_yr, Rsoil_temp_names_yr, Rsoil_temp_names_y_yr, Rsurface_water_names_yr, Rsurface_water_names_y_yr,
			Rsw_pot_names_yr, Rsw_pot_names_y_yr, Rswa_names_yr, Rswa_names_y_yr, Rswc_names_yr, Rswc_names_y_yr, Rtemp_names_yr, Rtemp_names_y_yr, Rtransp_names_yr,
			Rtransp_names_y_yr, Rvwc_names_yr, Rvwc_names_y_yr, Rwetdays_names_yr, Rwetdays_names_y_yr;
	SEXP Raet_names_mo, Raet_names_y_mo, Rdeep_drain_names_mo, Rdeep_drain_names_y_mo, /*Restabs_names_mo, Restabs_names_y_mo,*/Revap_soil_names_mo, Revap_soil_names_y_mo,
			Revap_surface_names_mo, Revap_surface_names_y_mo, Rhydred_names_mo, Rhydred_names_y_mo, Rinfiltration_names_mo, Rinfiltration_names_y_mo, Rinterception_names_mo,
			Rinterception_names_y_mo, Rpercolation_names_mo, Rpercolation_names_y_mo, Rpet_names_mo, Rpet_names_y_mo, Rprecip_names_mo, Rprecip_names_y_mo, Rrunoff_names_mo,
			Rrunoff_names_y_mo, Rsnowpack_names_mo, Rsnowpack_names_y_mo, Rsoil_temp_names_mo, Rsoil_temp_names_y_mo, Rsurface_water_names_mo, Rsurface_water_names_y_mo,
			Rsw_pot_names_mo, Rsw_pot_names_y_mo, Rswa_names_mo, Rswa_names_y_mo, Rswc_names_mo, Rswc_names_y_mo, Rtemp_names_mo, Rtemp_names_y_mo, Rtransp_names_mo,
			Rtransp_names_y_mo, Rvwc_names_mo, Rvwc_names_y_mo, Rwetdays_names_mo, Rwetdays_names_y_mo;
	SEXP Raet_names_wk, Raet_names_y_wk, Rdeep_drain_names_wk, Rdeep_drain_names_y_wk, /*Restabs_names_wk, Restabs_names_y_wk,*/Revap_soil_names_wk, Revap_soil_names_y_wk,
			Revap_surface_names_wk, Revap_surface_names_y_wk, Rhydred_names_wk, Rhydred_names_y_wk, Rinfiltration_names_wk, Rinfiltration_names_y_wk, Rinterception_names_wk,
			Rinterception_names_y_wk, Rpercolation_names_wk, Rpercolation_names_y_wk, Rpet_names_wk, Rpet_names_y_wk, Rprecip_names_wk, Rprecip_names_y_wk, Rrunoff_names_wk,
			Rrunoff_names_y_wk, Rsnowpack_names_wk, Rsnowpack_names_y_wk, Rsoil_temp_names_wk, Rsoil_temp_names_y_wk, Rsurface_water_names_wk, Rsurface_water_names_y_wk,
			Rsw_pot_names_wk, Rsw_pot_names_y_wk, Rswa_names_wk, Rswa_names_y_wk, Rswc_names_wk, Rswc_names_y_wk, Rtemp_names_wk, Rtemp_names_y_wk, Rtransp_names_wk,
			Rtransp_names_y_wk, Rvwc_names_wk, Rvwc_names_y_wk, Rwetdays_names_wk, Rwetdays_names_y_wk;
	SEXP Raet_names_dy, Raet_names_y_dy, Rdeep_drain_names_dy, Rdeep_drain_names_y_dy, /*Restabs_names_dy, Restabs_names_y_dy,*/Revap_soil_names_dy, Revap_soil_names_y_dy,
			Revap_surface_names_dy, Revap_surface_names_y_dy, Rhydred_names_dy, Rhydred_names_y_dy, Rinfiltration_names_dy, Rinfiltration_names_y_dy, Rinterception_names_dy,
			Rinterception_names_y_dy, Rpercolation_names_dy, Rpercolation_names_y_dy, Rpet_names_dy, Rpet_names_y_dy, Rprecip_names_dy, Rprecip_names_y_dy, Rrunoff_names_dy,
			Rrunoff_names_y_dy, Rsnowpack_names_dy, Rsnowpack_names_y_dy, Rsoil_temp_names_dy, Rsoil_temp_names_y_dy, Rsurface_water_names_dy, Rsurface_water_names_y_dy,
			Rsw_pot_names_dy, Rsw_pot_names_y_dy, Rswa_names_dy, Rswa_names_y_dy, Rswc_names_dy, Rswc_names_y_dy, Rtemp_names_dy, Rtemp_names_y_dy, Rtransp_names_dy,
			Rtransp_names_y_dy, Rvwc_names_dy, Rvwc_names_y_dy, Rwetdays_names_dy, Rwetdays_names_y_dy;

	char *OutColNames[] = { "aet", "deep_drain", "estabs", "evap_soil", "evap_surface", "hydred", "infiltration", "interception", "percolation", "pet", "precip", "runoff",
			"snowpack", "soil_temp", "surface_water", "sw_pot", "swa", "swc", "temp", "transp", "vwc", "wetdays", "logfile" };

	char *Layers_names[] = { "Lyr_1", "Lyr_2", "Lyr_3", "Lyr_4", "Lyr_5", "Lyr_6", "Lyr_7", "Lyr_8", "Lyr_9", "Lyr_10", "Lyr_11", "Lyr_12", "Lyr_13", "Lyr_14", "Lyr_15",
			"Lyr_16", "Lyr_17", "Lyr_18", "Lyr_19", "Lyr_20", "Lyr_21", "Lyr_22", "Lyr_23", "Lyr_24", "Lyr_25", "Lyr_26", "Lyr_27", "Lyr_28", "Lyr_29", "Lyr_30" };
	char *Cevap_surface_names[] = { "total_evap", "tree_evap", "shrub_evap", "grass_evap", "litter_evap", "surfaceWater_evap" };
	char *Chydred_names[] = { "total_", "tree_", "shrub_", "grass_" };
	char *Cinterception_names[] = { "total", "tree", "shrub", "grass", "litter" };
	char *Cprecip_names[] = { "ppt", "rain", "snow", "snowmelt", "snowloss" };
	char *Crunoff_names[] = { "surfaceRunoff", "snowRunoff", "total" };
	char *Csnowpack_names[] = { "snowpack", "snowdepth" };
	char *Ctemp_names[] = { "max_C", "min_C", "avg_C" };
	char *Ctransp_names[] = { "transp_total_", "transp_tree_", "transp_shrub_", "transp_grass_" };
	char Ctemp[50];

	//Year Names
	PROTECT(Raet_names_yr = allocVector(VECSXP, 2));
	PROTECT(Raet_names_y_yr = allocVector(STRSXP, p_Raet_columns + 1));
	SET_STRING_ELT(Raet_names_y_yr, 0, mkChar("Year"));
	SET_STRING_ELT(Raet_names_y_yr, 1, mkChar("evapotr_cm"));
	SET_VECTOR_ELT(Raet_names_yr, 1, Raet_names_y_yr);

	PROTECT(Rdeep_drain_names_yr = allocVector(VECSXP, 2));
	PROTECT(Rdeep_drain_names_y_yr = allocVector(STRSXP, p_Rdeep_drain_columns + 1));
	SET_STRING_ELT(Rdeep_drain_names_y_yr, 0, mkChar("Year"));
	SET_STRING_ELT(Rdeep_drain_names_y_yr, 1, mkChar("lowLayerDrain_cm"));
	SET_VECTOR_ELT(Rdeep_drain_names_yr, 1, Rdeep_drain_names_y_yr);
	/*
	 PROTECT(Restabs_names_yr = allocVector(VECSXP, 2));
	 PROTECT(Restabs_names_y_yr = allocVector(STRSXP, p_Restabs_columns+1));
	 SET_STRING_ELT(Restabs_names_y_yr, 0, mkChar("Year"));
	 SET_STRING_ELT(Restabs_names_y_yr, 1, mkChar("YearlyEstabResults"));
	 SET_VECTOR_ELT(Restabs_names_yr, 1, Restabs_names_y_yr);
	 */
	PROTECT(Revap_soil_names_yr = allocVector(VECSXP, 2));
	PROTECT(Revap_soil_names_y_yr = allocVector(STRSXP, p_Revap_soil_columns + 1));
	SET_STRING_ELT(Revap_soil_names_y_yr, 0, mkChar("Year"));
	for (i = 0; i < p_Revap_soil_columns; i++)
		SET_STRING_ELT(Revap_soil_names_y_yr, i + 1, mkChar(Layers_names[i]));
	SET_VECTOR_ELT(Revap_soil_names_yr, 1, Revap_soil_names_y_yr);

	PROTECT(Revap_surface_names_yr = allocVector(VECSXP, 2));
	PROTECT(Revap_surface_names_y_yr = allocVector(STRSXP, p_Revap_surface_columns + 1));
	SET_STRING_ELT(Revap_surface_names_y_yr, 0, mkChar("Year"));
	for (i = 0; i < p_Revap_surface_columns; i++)
		SET_STRING_ELT(Revap_surface_names_y_yr, i + 1, mkChar(Cevap_surface_names[i]));
	SET_VECTOR_ELT(Revap_surface_names_yr, 1, Revap_surface_names_y_yr);

	PROTECT(Rhydred_names_yr = allocVector(VECSXP, 2));
	PROTECT(Rhydred_names_y_yr = allocVector(STRSXP, p_Rhydred_columns + 1));
	SET_STRING_ELT(Rhydred_names_y_yr, 0, mkChar("Year"));
	for (i = 0; i < 4; i++) {
		for (j = 0; j < tLayers; j++) {
			strcpy(Ctemp, Chydred_names[i]);
			strcat(Ctemp, Layers_names[j]);
			SET_STRING_ELT(Rhydred_names_y_yr, (i * tLayers + j) + 1, mkChar(Ctemp));
		}
	}
	SET_VECTOR_ELT(Rhydred_names_yr, 1, Rhydred_names_y_yr);

	PROTECT(Rinfiltration_names_yr = allocVector(VECSXP, 2));
	PROTECT(Rinfiltration_names_y_yr = allocVector(STRSXP, p_Rinfiltration_columns + 1));
	SET_STRING_ELT(Rinfiltration_names_y_yr, 0, mkChar("Year"));
	SET_STRING_ELT(Rinfiltration_names_y_yr, 1, mkChar("soil_inf"));
	SET_VECTOR_ELT(Rinfiltration_names_yr, 1, Rinfiltration_names_y_yr);

	PROTECT(Rinterception_names_yr = allocVector(VECSXP, 2));
	PROTECT(Rinterception_names_y_yr = allocVector(STRSXP, p_Rinterception_columns + 1));
	SET_STRING_ELT(Rinterception_names_y_yr, 0, mkChar("Year"));
	for (i = 0; i < p_Rinterception_columns; i++)
		SET_STRING_ELT(Rinterception_names_y_yr, i + 1, mkChar(Cinterception_names[i]));
	SET_VECTOR_ELT(Rinterception_names_yr, 1, Rinterception_names_y_yr);

	PROTECT(Rpercolation_names_yr = allocVector(VECSXP, 2));
	PROTECT(Rpercolation_names_y_yr = allocVector(STRSXP, p_Rpercolation_columns + 1));
	SET_STRING_ELT(Rpercolation_names_y_yr, 0, mkChar("Year"));
	for (i = 0; i < (tLayers - 1); i++)
		SET_STRING_ELT(Rpercolation_names_y_yr, i + 1, mkChar(Layers_names[i]));
	SET_VECTOR_ELT(Rpercolation_names_yr, 1, Rpercolation_names_y_yr);

	PROTECT(Rpet_names_yr = allocVector(VECSXP, 2));
	PROTECT(Rpet_names_y_yr = allocVector(STRSXP, p_Rpet_columns + 1));
	SET_STRING_ELT(Rpet_names_y_yr, 0, mkChar("Year"));
	SET_STRING_ELT(Rpet_names_y_yr, 1, mkChar("pet_cm"));
	SET_VECTOR_ELT(Rpet_names_yr, 1, Rpet_names_y_yr);

	PROTECT(Rprecip_names_yr = allocVector(VECSXP, 2));
	PROTECT(Rprecip_names_y_yr = allocVector(STRSXP, p_Rprecip_columns + 1));
	SET_STRING_ELT(Rprecip_names_y_yr, 0, mkChar("Year"));
	for (i = 0; i < p_Rprecip_columns; i++)
		SET_STRING_ELT(Rprecip_names_y_yr, i + 1, mkChar(Cprecip_names[i]));
	SET_VECTOR_ELT(Rprecip_names_yr, 1, Rprecip_names_y_yr); //17

	PROTECT(Rrunoff_names_yr = allocVector(VECSXP, 2));
	PROTECT(Rrunoff_names_y_yr = allocVector(STRSXP, p_Rrunoff_columns + 1));
	SET_STRING_ELT(Rrunoff_names_y_yr, 0, mkChar("Year"));
	for (i = 0; i < p_Rrunoff_columns; i++)
		SET_STRING_ELT(Rrunoff_names_y_yr, i + 1, mkChar(Crunoff_names[i]));
	SET_VECTOR_ELT(Rrunoff_names_yr, 1, Rrunoff_names_y_yr);

	PROTECT(Rsnowpack_names_yr = allocVector(VECSXP, 2));
	PROTECT(Rsnowpack_names_y_yr = allocVector(STRSXP, p_Rsnowpack_columns + 1));
	SET_STRING_ELT(Rsnowpack_names_y_yr, 0, mkChar("Year"));
	for (i = 0; i < p_Rsnowpack_columns; i++)
		SET_STRING_ELT(Rsnowpack_names_y_yr, i + 1, mkChar(Csnowpack_names[i]));
	SET_VECTOR_ELT(Rsnowpack_names_yr, 1, Rsnowpack_names_y_yr);

	PROTECT(Rsoil_temp_names_yr = allocVector(VECSXP, 2));
	PROTECT(Rsoil_temp_names_y_yr = allocVector(STRSXP, p_Rsoil_temp_columns + 1));
	SET_STRING_ELT(Rsoil_temp_names_y_yr, 0, mkChar("Year"));
	for (i = 0; i < tLayers; i++)
		SET_STRING_ELT(Rsoil_temp_names_y_yr, i + 1, mkChar(Layers_names[i]));
	SET_VECTOR_ELT(Rsoil_temp_names_yr, 1, Rsoil_temp_names_y_yr);

	PROTECT(Rsurface_water_names_yr = allocVector(VECSXP, 2));
	PROTECT(Rsurface_water_names_y_yr = allocVector(STRSXP, p_Rsurface_water_columns + 1));
	SET_STRING_ELT(Rsurface_water_names_y_yr, 0, mkChar("Year"));
	SET_STRING_ELT(Rsurface_water_names_y_yr, 1, mkChar("surfaceWater_cm"));
	SET_VECTOR_ELT(Rsurface_water_names_yr, 1, Rsurface_water_names_y_yr);

	PROTECT(Rsw_pot_names_yr = allocVector(VECSXP, 2));
	PROTECT(Rsw_pot_names_y_yr = allocVector(STRSXP, p_Rsw_pot_columns + 1));
	SET_STRING_ELT(Rsw_pot_names_y_yr, 0, mkChar("Year"));
	for (i = 0; i < tLayers; i++)
		SET_STRING_ELT(Rsw_pot_names_y_yr, i + 1, mkChar(Layers_names[i]));
	SET_VECTOR_ELT(Rsw_pot_names_yr, 1, Rsw_pot_names_y_yr);

	PROTECT(Rswa_names_yr = allocVector(VECSXP, 2));
	PROTECT(Rswa_names_y_yr = allocVector(STRSXP, p_Rswa_columns + 1));
	SET_STRING_ELT(Rswa_names_y_yr, 0, mkChar("Year"));
	for (i = 0; i < tLayers; i++)
		SET_STRING_ELT(Rswa_names_y_yr, i + 1, mkChar(Layers_names[i]));
	SET_VECTOR_ELT(Rswa_names_yr, 1, Rswa_names_y_yr); //28

	PROTECT(Rswc_names_yr = allocVector(VECSXP, 2));
	PROTECT(Rswc_names_y_yr = allocVector(STRSXP, p_Rswc_columns + 1));
	SET_STRING_ELT(Rswc_names_y_yr, 0, mkChar("Year"));
	for (i = 0; i < tLayers; i++)
		SET_STRING_ELT(Rswc_names_y_yr, i + 1, mkChar(Layers_names[i]));
	SET_VECTOR_ELT(Rswc_names_yr, 1, Rswc_names_y_yr);

	PROTECT(Rtemp_names_yr = allocVector(VECSXP, 2));
	PROTECT(Rtemp_names_y_yr = allocVector(STRSXP, p_Rtemp_columns + 1));
	SET_STRING_ELT(Rtemp_names_y_yr, 0, mkChar("Year"));
	for (i = 0; i < p_Rtemp_columns; i++)
		SET_STRING_ELT(Rtemp_names_y_yr, i + 1, mkChar(Ctemp_names[i]));
	SET_VECTOR_ELT(Rtemp_names_yr, 1, Rtemp_names_y_yr);

	PROTECT(Rtransp_names_yr = allocVector(VECSXP, 2));
	PROTECT(Rtransp_names_y_yr = allocVector(STRSXP, p_Rtransp_columns + 1));
	SET_STRING_ELT(Rtransp_names_y_yr, 0, mkChar("Year"));
	for (i = 0; i < 4; i++) {
		for (j = 0; j < tLayers; j++) {
			strcpy(Ctemp, Ctransp_names[i]);
			strcat(Ctemp, Layers_names[j]);
			SET_STRING_ELT(Rtransp_names_y_yr, (i * tLayers + j) + 1, mkChar(Ctemp));
		}
	}
	SET_VECTOR_ELT(Rtransp_names_yr, 1, Rtransp_names_y_yr);

	PROTECT(Rvwc_names_yr = allocVector(VECSXP, 2));
	PROTECT(Rvwc_names_y_yr = allocVector(STRSXP, p_Rvwc_columns + 1));
	SET_STRING_ELT(Rvwc_names_y_yr, 0, mkChar("Year"));
	for (i = 0; i < tLayers; i++)
		SET_STRING_ELT(Rvwc_names_y_yr, i + 1, mkChar(Layers_names[i]));
	SET_VECTOR_ELT(Rvwc_names_yr, 1, Rvwc_names_y_yr);

	PROTECT(Rwetdays_names_yr = allocVector(VECSXP, 2));
	PROTECT(Rwetdays_names_y_yr = allocVector(STRSXP, p_Rwetdays_columns + 1));
	SET_STRING_ELT(Rwetdays_names_y_yr, 0, mkChar("Year"));
	for (i = 0; i < tLayers; i++)
		SET_STRING_ELT(Rwetdays_names_y_yr, i + 1, mkChar(Layers_names[i]));
	SET_VECTOR_ELT(Rwetdays_names_yr, 1, Rwetdays_names_y_yr);
	//
	//Month Names
	//
	PROTECT(Raet_names_mo = allocVector(VECSXP, 2));
	PROTECT(Raet_names_y_mo = allocVector(STRSXP, p_Raet_columns + 2));
	SET_STRING_ELT(Raet_names_y_mo, 0, mkChar("Year"));
	SET_STRING_ELT(Raet_names_y_mo, 1, mkChar("Month"));
	SET_STRING_ELT(Raet_names_y_mo, 2, mkChar("evapotr_cm"));
	SET_VECTOR_ELT(Raet_names_mo, 1, Raet_names_y_mo);

	PROTECT(Rdeep_drain_names_mo = allocVector(VECSXP, 2));
	PROTECT(Rdeep_drain_names_y_mo = allocVector(STRSXP, p_Rdeep_drain_columns + 2));
	SET_STRING_ELT(Rdeep_drain_names_y_mo, 0, mkChar("Year"));
	SET_STRING_ELT(Rdeep_drain_names_y_mo, 1, mkChar("Month"));
	SET_STRING_ELT(Rdeep_drain_names_y_mo, 2, mkChar("lowLayerDrain_cm"));
	SET_VECTOR_ELT(Rdeep_drain_names_mo, 1, Rdeep_drain_names_y_mo);
	/*
	 PROTECT(Restabs_names_mo = allocVector(VECSXP, 2));
	 PROTECT(Restabs_names_y_mo = allocVector(STRSXP, p_Restabs_columns+2));
	 SET_STRING_ELT(Restabs_names_y_mo, 0, mkChar("Year"));
	 SET_STRING_ELT(Restabs_names_y_mo, 1, mkChar("Month"));
	 SET_STRING_ELT(Restabs_names_y_mo, 2, mkChar("YearlyEstabResults"));
	 SET_VECTOR_ELT(Restabs_names_mo, 1, Restabs_names_y_mo);
	 */
	PROTECT(Revap_soil_names_mo = allocVector(VECSXP, 2));
	PROTECT(Revap_soil_names_y_mo = allocVector(STRSXP, p_Revap_soil_columns + 2));
	SET_STRING_ELT(Revap_soil_names_y_mo, 0, mkChar("Year"));
	SET_STRING_ELT(Revap_soil_names_y_mo, 1, mkChar("Month"));
	for (i = 0; i < p_Revap_soil_columns; i++)
		SET_STRING_ELT(Revap_soil_names_y_mo, i + 2, mkChar(Layers_names[i]));
	SET_VECTOR_ELT(Revap_soil_names_mo, 1, Revap_soil_names_y_mo);

	PROTECT(Revap_surface_names_mo = allocVector(VECSXP, 2));
	PROTECT(Revap_surface_names_y_mo = allocVector(STRSXP, p_Revap_surface_columns + 2));
	SET_STRING_ELT(Revap_surface_names_y_mo, 0, mkChar("Year"));
	SET_STRING_ELT(Revap_surface_names_y_mo, 1, mkChar("Month"));
	for (i = 0; i < p_Revap_surface_columns; i++)
		SET_STRING_ELT(Revap_surface_names_y_mo, i + 2, mkChar(Cevap_surface_names[i]));
	SET_VECTOR_ELT(Revap_surface_names_mo, 1, Revap_surface_names_y_mo);

	PROTECT(Rhydred_names_mo = allocVector(VECSXP, 2));
	PROTECT(Rhydred_names_y_mo = allocVector(STRSXP, p_Rhydred_columns + 2));
	SET_STRING_ELT(Rhydred_names_y_mo, 0, mkChar("Year"));
	SET_STRING_ELT(Rhydred_names_y_mo, 1, mkChar("Month"));
	for (i = 0; i < 4; i++) {
		for (j = 0; j < tLayers; j++) {
			strcpy(Ctemp, Chydred_names[i]);
			strcat(Ctemp, Layers_names[j]);
			SET_STRING_ELT(Rhydred_names_y_mo, (i * tLayers + j) + 2, mkChar(Ctemp));
		}
	}
	SET_VECTOR_ELT(Rhydred_names_mo, 1, Rhydred_names_y_mo);

	PROTECT(Rinfiltration_names_mo = allocVector(VECSXP, 2));
	PROTECT(Rinfiltration_names_y_mo = allocVector(STRSXP, p_Rinfiltration_columns + 2));
	SET_STRING_ELT(Rinfiltration_names_y_mo, 0, mkChar("Year"));
	SET_STRING_ELT(Rinfiltration_names_y_mo, 1, mkChar("Month"));
	SET_STRING_ELT(Rinfiltration_names_y_mo, 2, mkChar("soil_inf"));
	SET_VECTOR_ELT(Rinfiltration_names_mo, 1, Rinfiltration_names_y_mo);

	PROTECT(Rinterception_names_mo = allocVector(VECSXP, 2));
	PROTECT(Rinterception_names_y_mo = allocVector(STRSXP, p_Rinterception_columns + 2));
	SET_STRING_ELT(Rinterception_names_y_mo, 0, mkChar("Year"));
	SET_STRING_ELT(Rinterception_names_y_mo, 1, mkChar("Month"));
	for (i = 0; i < p_Rinterception_columns; i++)
		SET_STRING_ELT(Rinterception_names_y_mo, i + 2, mkChar(Cinterception_names[i]));
	SET_VECTOR_ELT(Rinterception_names_mo, 1, Rinterception_names_y_mo);

	PROTECT(Rpercolation_names_mo = allocVector(VECSXP, 2));
	PROTECT(Rpercolation_names_y_mo = allocVector(STRSXP, p_Rpercolation_columns + 2));
	SET_STRING_ELT(Rpercolation_names_y_mo, 0, mkChar("Year"));
	SET_STRING_ELT(Rpercolation_names_y_mo, 1, mkChar("Month"));
	for (i = 0; i < (tLayers - 1); i++)
		SET_STRING_ELT(Rpercolation_names_y_mo, i + 2, mkChar(Layers_names[i]));
	SET_VECTOR_ELT(Rpercolation_names_mo, 1, Rpercolation_names_y_mo);

	PROTECT(Rpet_names_mo = allocVector(VECSXP, 2));
	PROTECT(Rpet_names_y_mo = allocVector(STRSXP, p_Rpet_columns + 2));
	SET_STRING_ELT(Rpet_names_y_mo, 0, mkChar("Year"));
	SET_STRING_ELT(Rpet_names_y_mo, 1, mkChar("Month"));
	SET_STRING_ELT(Rpet_names_y_mo, 2, mkChar("pet_cm"));
	SET_VECTOR_ELT(Rpet_names_mo, 1, Rpet_names_y_mo);

	PROTECT(Rprecip_names_mo = allocVector(VECSXP, 2));
	PROTECT(Rprecip_names_y_mo = allocVector(STRSXP, p_Rprecip_columns + 2));
	SET_STRING_ELT(Rprecip_names_y_mo, 0, mkChar("Year"));
	SET_STRING_ELT(Rprecip_names_y_mo, 1, mkChar("Month"));
	for (i = 0; i < p_Rprecip_columns; i++)
		SET_STRING_ELT(Rprecip_names_y_mo, i + 2, mkChar(Cprecip_names[i]));
	SET_VECTOR_ELT(Rprecip_names_mo, 1, Rprecip_names_y_mo); //17

	PROTECT(Rrunoff_names_mo = allocVector(VECSXP, 2));
	PROTECT(Rrunoff_names_y_mo = allocVector(STRSXP, p_Rrunoff_columns + 2));
	SET_STRING_ELT(Rrunoff_names_y_mo, 0, mkChar("Year"));
	SET_STRING_ELT(Rrunoff_names_y_mo, 1, mkChar("Month"));
	for (i = 0; i < p_Rrunoff_columns; i++)
		SET_STRING_ELT(Rrunoff_names_y_mo, i + 2, mkChar(Crunoff_names[i]));
	SET_VECTOR_ELT(Rrunoff_names_mo, 1, Rrunoff_names_y_mo);

	PROTECT(Rsnowpack_names_mo = allocVector(VECSXP, 2));
	PROTECT(Rsnowpack_names_y_mo = allocVector(STRSXP, p_Rsnowpack_columns + 2));
	SET_STRING_ELT(Rsnowpack_names_y_mo, 0, mkChar("Year"));
	SET_STRING_ELT(Rsnowpack_names_y_mo, 1, mkChar("Month"));
	for (i = 0; i < p_Rsnowpack_columns; i++)
		SET_STRING_ELT(Rsnowpack_names_y_mo, i + 2, mkChar(Csnowpack_names[i]));
	SET_VECTOR_ELT(Rsnowpack_names_mo, 1, Rsnowpack_names_y_mo);

	PROTECT(Rsoil_temp_names_mo = allocVector(VECSXP, 2));
	PROTECT(Rsoil_temp_names_y_mo = allocVector(STRSXP, p_Rsoil_temp_columns + 2));
	SET_STRING_ELT(Rsoil_temp_names_y_mo, 0, mkChar("Year"));
	SET_STRING_ELT(Rsoil_temp_names_y_mo, 1, mkChar("Month"));
	for (i = 0; i < tLayers; i++)
		SET_STRING_ELT(Rsoil_temp_names_y_mo, i + 2, mkChar(Layers_names[i]));
	SET_VECTOR_ELT(Rsoil_temp_names_mo, 1, Rsoil_temp_names_y_mo);

	PROTECT(Rsurface_water_names_mo = allocVector(VECSXP, 2));
	PROTECT(Rsurface_water_names_y_mo = allocVector(STRSXP, p_Rsurface_water_columns + 2));
	SET_STRING_ELT(Rsurface_water_names_y_mo, 0, mkChar("Year"));
	SET_STRING_ELT(Rsurface_water_names_y_mo, 1, mkChar("Month"));
	SET_STRING_ELT(Rsurface_water_names_y_mo, 2, mkChar("surfaceWater_cm"));
	SET_VECTOR_ELT(Rsurface_water_names_mo, 1, Rsurface_water_names_y_mo);

	PROTECT(Rsw_pot_names_mo = allocVector(VECSXP, 2));
	PROTECT(Rsw_pot_names_y_mo = allocVector(STRSXP, p_Rsw_pot_columns + 2));
	SET_STRING_ELT(Rsw_pot_names_y_mo, 0, mkChar("Year"));
	SET_STRING_ELT(Rsw_pot_names_y_mo, 1, mkChar("Month"));
	for (i = 0; i < tLayers; i++)
		SET_STRING_ELT(Rsw_pot_names_y_mo, i + 2, mkChar(Layers_names[i]));
	SET_VECTOR_ELT(Rsw_pot_names_mo, 1, Rsw_pot_names_y_mo);

	PROTECT(Rswa_names_mo = allocVector(VECSXP, 2));
	PROTECT(Rswa_names_y_mo = allocVector(STRSXP, p_Rswa_columns + 2));
	SET_STRING_ELT(Rswa_names_y_mo, 0, mkChar("Year"));
	SET_STRING_ELT(Rswa_names_y_mo, 1, mkChar("Month"));
	for (i = 0; i < tLayers; i++)
		SET_STRING_ELT(Rswa_names_y_mo, i + 2, mkChar(Layers_names[i]));
	SET_VECTOR_ELT(Rswa_names_mo, 1, Rswa_names_y_mo); //28

	PROTECT(Rswc_names_mo = allocVector(VECSXP, 2));
	PROTECT(Rswc_names_y_mo = allocVector(STRSXP, p_Rswc_columns + 2));
	SET_STRING_ELT(Rswc_names_y_mo, 0, mkChar("Year"));
	SET_STRING_ELT(Rswc_names_y_mo, 1, mkChar("Month"));
	for (i = 0; i < tLayers; i++)
		SET_STRING_ELT(Rswc_names_y_mo, i + 2, mkChar(Layers_names[i]));
	SET_VECTOR_ELT(Rswc_names_mo, 1, Rswc_names_y_mo);

	PROTECT(Rtemp_names_mo = allocVector(VECSXP, 2));
	PROTECT(Rtemp_names_y_mo = allocVector(STRSXP, p_Rtemp_columns + 2));
	SET_STRING_ELT(Rtemp_names_y_mo, 0, mkChar("Year"));
	SET_STRING_ELT(Rtemp_names_y_mo, 1, mkChar("Month"));
	for (i = 0; i < p_Rtemp_columns; i++)
		SET_STRING_ELT(Rtemp_names_y_mo, i + 2, mkChar(Ctemp_names[i]));
	SET_VECTOR_ELT(Rtemp_names_mo, 1, Rtemp_names_y_mo);

	PROTECT(Rtransp_names_mo = allocVector(VECSXP, 2));
	PROTECT(Rtransp_names_y_mo = allocVector(STRSXP, p_Rtransp_columns + 2));
	SET_STRING_ELT(Rtransp_names_y_mo, 0, mkChar("Year"));
	SET_STRING_ELT(Rtransp_names_y_mo, 1, mkChar("Month"));
	for (i = 0; i < 4; i++) {
		for (j = 0; j < tLayers; j++) {
			strcpy(Ctemp, Ctransp_names[i]);
			strcat(Ctemp, Layers_names[j]);
			SET_STRING_ELT(Rtransp_names_y_mo, (i * tLayers + j) + 2, mkChar(Ctemp));
		}
	}
	SET_VECTOR_ELT(Rtransp_names_mo, 1, Rtransp_names_y_mo);

	PROTECT(Rvwc_names_mo = allocVector(VECSXP, 2));
	PROTECT(Rvwc_names_y_mo = allocVector(STRSXP, p_Rvwc_columns + 2));
	SET_STRING_ELT(Rvwc_names_y_mo, 0, mkChar("Year"));
	SET_STRING_ELT(Rvwc_names_y_mo, 1, mkChar("Month"));
	for (i = 0; i < tLayers; i++)
		SET_STRING_ELT(Rvwc_names_y_mo, i + 2, mkChar(Layers_names[i]));
	SET_VECTOR_ELT(Rvwc_names_mo, 1, Rvwc_names_y_mo);

	PROTECT(Rwetdays_names_mo = allocVector(VECSXP, 2));
	PROTECT(Rwetdays_names_y_mo = allocVector(STRSXP, p_Rwetdays_columns + 2));
	SET_STRING_ELT(Rwetdays_names_y_mo, 0, mkChar("Year"));
	SET_STRING_ELT(Rwetdays_names_y_mo, 1, mkChar("Month"));
	for (i = 0; i < tLayers; i++)
		SET_STRING_ELT(Rwetdays_names_y_mo, i + 2, mkChar(Layers_names[i]));
	SET_VECTOR_ELT(Rwetdays_names_mo, 1, Rwetdays_names_y_mo);

	//
	//Week Names
	//
	PROTECT(Raet_names_wk = allocVector(VECSXP, 2));
	PROTECT(Raet_names_y_wk = allocVector(STRSXP, p_Raet_columns + 2));
	SET_STRING_ELT(Raet_names_y_wk, 0, mkChar("Year"));
	SET_STRING_ELT(Raet_names_y_wk, 1, mkChar("Week"));
	SET_STRING_ELT(Raet_names_y_wk, 2, mkChar("evapotr_cm"));
	SET_VECTOR_ELT(Raet_names_wk, 1, Raet_names_y_wk);

	PROTECT(Rdeep_drain_names_wk = allocVector(VECSXP, 2));
	PROTECT(Rdeep_drain_names_y_wk = allocVector(STRSXP, p_Rdeep_drain_columns + 2));
	SET_STRING_ELT(Rdeep_drain_names_y_wk, 0, mkChar("Year"));
	SET_STRING_ELT(Rdeep_drain_names_y_wk, 1, mkChar("Week"));
	SET_STRING_ELT(Rdeep_drain_names_y_wk, 2, mkChar("lowLayerDrain_cm"));
	SET_VECTOR_ELT(Rdeep_drain_names_wk, 1, Rdeep_drain_names_y_wk);
	/*
	 PROTECT(Restabs_names_wk = allocVector(VECSXP, 2));
	 PROTECT(Restabs_names_y_wk = allocVector(STRSXP, p_Restabs_columns+2));
	 SET_STRING_ELT(Restabs_names_y_wk, 0, mkChar("Year"));
	 SET_STRING_ELT(Restabs_names_y_wk, 1, mkChar("Week"));
	 SET_STRING_ELT(Restabs_names_y_wk, 2, mkChar("YearlyEstabResults"));
	 SET_VECTOR_ELT(Restabs_names_wk, 1, Restabs_names_y_wk);
	 */
	PROTECT(Revap_soil_names_wk = allocVector(VECSXP, 2));
	PROTECT(Revap_soil_names_y_wk = allocVector(STRSXP, p_Revap_soil_columns + 2));
	SET_STRING_ELT(Revap_soil_names_y_wk, 0, mkChar("Year"));
	SET_STRING_ELT(Revap_soil_names_y_wk, 1, mkChar("Week"));
	for (i = 0; i < p_Revap_soil_columns; i++)
		SET_STRING_ELT(Revap_soil_names_y_wk, i + 2, mkChar(Layers_names[i]));
	SET_VECTOR_ELT(Revap_soil_names_wk, 1, Revap_soil_names_y_wk);

	PROTECT(Revap_surface_names_wk = allocVector(VECSXP, 2));
	PROTECT(Revap_surface_names_y_wk = allocVector(STRSXP, p_Revap_surface_columns + 2));
	SET_STRING_ELT(Revap_surface_names_y_wk, 0, mkChar("Year"));
	SET_STRING_ELT(Revap_surface_names_y_wk, 1, mkChar("Week"));
	for (i = 0; i < p_Revap_surface_columns; i++)
		SET_STRING_ELT(Revap_surface_names_y_wk, i + 2, mkChar(Cevap_surface_names[i]));
	SET_VECTOR_ELT(Revap_surface_names_wk, 1, Revap_surface_names_y_wk);

	PROTECT(Rhydred_names_wk = allocVector(VECSXP, 2));
	PROTECT(Rhydred_names_y_wk = allocVector(STRSXP, p_Rhydred_columns + 2));
	SET_STRING_ELT(Rhydred_names_y_wk, 0, mkChar("Year"));
	SET_STRING_ELT(Rhydred_names_y_wk, 1, mkChar("Week"));
	for (i = 0; i < 4; i++) {
		for (j = 0; j < tLayers; j++) {
			strcpy(Ctemp, Chydred_names[i]);
			strcat(Ctemp, Layers_names[j]);
			SET_STRING_ELT(Rhydred_names_y_wk, (i * tLayers + j) + 2, mkChar(Ctemp));
		}
	}
	SET_VECTOR_ELT(Rhydred_names_wk, 1, Rhydred_names_y_wk);

	PROTECT(Rinfiltration_names_wk = allocVector(VECSXP, 2));
	PROTECT(Rinfiltration_names_y_wk = allocVector(STRSXP, p_Rinfiltration_columns + 2));
	SET_STRING_ELT(Rinfiltration_names_y_wk, 0, mkChar("Year"));
	SET_STRING_ELT(Rinfiltration_names_y_wk, 1, mkChar("Week"));
	SET_STRING_ELT(Rinfiltration_names_y_wk, 2, mkChar("soil_inf"));
	SET_VECTOR_ELT(Rinfiltration_names_wk, 1, Rinfiltration_names_y_wk);

	PROTECT(Rinterception_names_wk = allocVector(VECSXP, 2));
	PROTECT(Rinterception_names_y_wk = allocVector(STRSXP, p_Rinterception_columns + 2));
	SET_STRING_ELT(Rinterception_names_y_wk, 0, mkChar("Year"));
	SET_STRING_ELT(Rinterception_names_y_wk, 1, mkChar("Week"));
	for (i = 0; i < p_Rinterception_columns; i++)
		SET_STRING_ELT(Rinterception_names_y_wk, i + 2, mkChar(Cinterception_names[i]));
	SET_VECTOR_ELT(Rinterception_names_wk, 1, Rinterception_names_y_wk);

	PROTECT(Rpercolation_names_wk = allocVector(VECSXP, 2));
	PROTECT(Rpercolation_names_y_wk = allocVector(STRSXP, p_Rpercolation_columns + 2));
	SET_STRING_ELT(Rpercolation_names_y_wk, 0, mkChar("Year"));
	SET_STRING_ELT(Rpercolation_names_y_wk, 1, mkChar("Week"));
	for (i = 0; i < (tLayers - 1); i++)
		SET_STRING_ELT(Rpercolation_names_y_wk, i + 2, mkChar(Layers_names[i]));
	SET_VECTOR_ELT(Rpercolation_names_wk, 1, Rpercolation_names_y_wk);

	PROTECT(Rpet_names_wk = allocVector(VECSXP, 2));
	PROTECT(Rpet_names_y_wk = allocVector(STRSXP, p_Rpet_columns + 2));
	SET_STRING_ELT(Rpet_names_y_wk, 0, mkChar("Year"));
	SET_STRING_ELT(Rpet_names_y_wk, 1, mkChar("Week"));
	SET_STRING_ELT(Rpet_names_y_wk, 2, mkChar("pet_cm"));
	SET_VECTOR_ELT(Rpet_names_wk, 1, Rpet_names_y_wk);

	PROTECT(Rprecip_names_wk = allocVector(VECSXP, 2));
	PROTECT(Rprecip_names_y_wk = allocVector(STRSXP, p_Rprecip_columns + 2));
	SET_STRING_ELT(Rprecip_names_y_wk, 0, mkChar("Year"));
	SET_STRING_ELT(Rprecip_names_y_wk, 1, mkChar("Week"));
	for (i = 0; i < p_Rprecip_columns; i++)
		SET_STRING_ELT(Rprecip_names_y_wk, i + 2, mkChar(Cprecip_names[i]));
	SET_VECTOR_ELT(Rprecip_names_wk, 1, Rprecip_names_y_wk); //17

	PROTECT(Rrunoff_names_wk = allocVector(VECSXP, 2));
	PROTECT(Rrunoff_names_y_wk = allocVector(STRSXP, p_Rrunoff_columns + 2));
	SET_STRING_ELT(Rrunoff_names_y_wk, 0, mkChar("Year"));
	SET_STRING_ELT(Rrunoff_names_y_wk, 1, mkChar("Week"));
	for (i = 0; i < p_Rrunoff_columns; i++)
		SET_STRING_ELT(Rrunoff_names_y_wk, i + 2, mkChar(Crunoff_names[i]));
	SET_VECTOR_ELT(Rrunoff_names_wk, 1, Rrunoff_names_y_wk);

	PROTECT(Rsnowpack_names_wk = allocVector(VECSXP, 2));
	PROTECT(Rsnowpack_names_y_wk = allocVector(STRSXP, p_Rsnowpack_columns + 2));
	SET_STRING_ELT(Rsnowpack_names_y_wk, 0, mkChar("Year"));
	SET_STRING_ELT(Rsnowpack_names_y_wk, 1, mkChar("Week"));
	for (i = 0; i < p_Rsnowpack_columns; i++)
		SET_STRING_ELT(Rsnowpack_names_y_wk, i + 2, mkChar(Csnowpack_names[i]));
	SET_VECTOR_ELT(Rsnowpack_names_wk, 1, Rsnowpack_names_y_wk);

	PROTECT(Rsoil_temp_names_wk = allocVector(VECSXP, 2));
	PROTECT(Rsoil_temp_names_y_wk = allocVector(STRSXP, p_Rsoil_temp_columns + 2));
	SET_STRING_ELT(Rsoil_temp_names_y_wk, 0, mkChar("Year"));
	SET_STRING_ELT(Rsoil_temp_names_y_wk, 1, mkChar("Week"));
	for (i = 0; i < tLayers; i++)
		SET_STRING_ELT(Rsoil_temp_names_y_wk, i + 2, mkChar(Layers_names[i]));
	SET_VECTOR_ELT(Rsoil_temp_names_wk, 1, Rsoil_temp_names_y_wk);

	PROTECT(Rsurface_water_names_wk = allocVector(VECSXP, 2));
	PROTECT(Rsurface_water_names_y_wk = allocVector(STRSXP, p_Rsurface_water_columns + 2));
	SET_STRING_ELT(Rsurface_water_names_y_wk, 0, mkChar("Year"));
	SET_STRING_ELT(Rsurface_water_names_y_wk, 1, mkChar("Week"));
	SET_STRING_ELT(Rsurface_water_names_y_wk, 2, mkChar("surfaceWater_cm"));
	SET_VECTOR_ELT(Rsurface_water_names_wk, 1, Rsurface_water_names_y_wk);

	PROTECT(Rsw_pot_names_wk = allocVector(VECSXP, 2));
	PROTECT(Rsw_pot_names_y_wk = allocVector(STRSXP, p_Rsw_pot_columns + 2));
	SET_STRING_ELT(Rsw_pot_names_y_wk, 0, mkChar("Year"));
	SET_STRING_ELT(Rsw_pot_names_y_wk, 1, mkChar("Week"));
	for (i = 0; i < tLayers; i++)
		SET_STRING_ELT(Rsw_pot_names_y_wk, i + 2, mkChar(Layers_names[i]));
	SET_VECTOR_ELT(Rsw_pot_names_wk, 1, Rsw_pot_names_y_wk);

	PROTECT(Rswa_names_wk = allocVector(VECSXP, 2));
	PROTECT(Rswa_names_y_wk = allocVector(STRSXP, p_Rswa_columns + 2));
	SET_STRING_ELT(Rswa_names_y_wk, 0, mkChar("Year"));
	SET_STRING_ELT(Rswa_names_y_wk, 1, mkChar("Week"));
	for (i = 0; i < tLayers; i++)
		SET_STRING_ELT(Rswa_names_y_wk, i + 2, mkChar(Layers_names[i]));
	SET_VECTOR_ELT(Rswa_names_wk, 1, Rswa_names_y_wk); //28

	PROTECT(Rswc_names_wk = allocVector(VECSXP, 2));
	PROTECT(Rswc_names_y_wk = allocVector(STRSXP, p_Rswc_columns + 2));
	SET_STRING_ELT(Rswc_names_y_wk, 0, mkChar("Year"));
	SET_STRING_ELT(Rswc_names_y_wk, 1, mkChar("Week"));
	for (i = 0; i < tLayers; i++)
		SET_STRING_ELT(Rswc_names_y_wk, i + 2, mkChar(Layers_names[i]));
	SET_VECTOR_ELT(Rswc_names_wk, 1, Rswc_names_y_wk);

	PROTECT(Rtemp_names_wk = allocVector(VECSXP, 2));
	PROTECT(Rtemp_names_y_wk = allocVector(STRSXP, p_Rtemp_columns + 2));
	SET_STRING_ELT(Rtemp_names_y_wk, 0, mkChar("Year"));
	SET_STRING_ELT(Rtemp_names_y_wk, 1, mkChar("Week"));
	for (i = 0; i < p_Rtemp_columns; i++)
		SET_STRING_ELT(Rtemp_names_y_wk, i + 2, mkChar(Ctemp_names[i]));
	SET_VECTOR_ELT(Rtemp_names_wk, 1, Rtemp_names_y_wk);

	PROTECT(Rtransp_names_wk = allocVector(VECSXP, 2));
	PROTECT(Rtransp_names_y_wk = allocVector(STRSXP, p_Rtransp_columns + 2));
	SET_STRING_ELT(Rtransp_names_y_wk, 0, mkChar("Year"));
	SET_STRING_ELT(Rtransp_names_y_wk, 1, mkChar("Week"));
	for (i = 0; i < 4; i++) {
		for (j = 0; j < tLayers; j++) {
			strcpy(Ctemp, Ctransp_names[i]);
			strcat(Ctemp, Layers_names[j]);
			SET_STRING_ELT(Rtransp_names_y_wk, (i * tLayers + j) + 2, mkChar(Ctemp));
		}
	}
	SET_VECTOR_ELT(Rtransp_names_wk, 1, Rtransp_names_y_wk);

	PROTECT(Rvwc_names_wk = allocVector(VECSXP, 2));
	PROTECT(Rvwc_names_y_wk = allocVector(STRSXP, p_Rvwc_columns + 2));
	SET_STRING_ELT(Rvwc_names_y_wk, 0, mkChar("Year"));
	SET_STRING_ELT(Rvwc_names_y_wk, 1, mkChar("Week"));
	for (i = 0; i < tLayers; i++)
		SET_STRING_ELT(Rvwc_names_y_wk, i + 2, mkChar(Layers_names[i]));
	SET_VECTOR_ELT(Rvwc_names_wk, 1, Rvwc_names_y_wk);

	PROTECT(Rwetdays_names_wk = allocVector(VECSXP, 2));
	PROTECT(Rwetdays_names_y_wk = allocVector(STRSXP, p_Rwetdays_columns + 2));
	SET_STRING_ELT(Rwetdays_names_y_wk, 0, mkChar("Year"));
	SET_STRING_ELT(Rwetdays_names_y_wk, 1, mkChar("Week"));
	for (i = 0; i < tLayers; i++)
		SET_STRING_ELT(Rwetdays_names_y_wk, i + 2, mkChar(Layers_names[i]));
	SET_VECTOR_ELT(Rwetdays_names_wk, 1, Rwetdays_names_y_wk);

	//
	//Day Names
	//
	PROTECT(Raet_names_dy = allocVector(VECSXP, 2));
	PROTECT(Raet_names_y_dy = allocVector(STRSXP, p_Raet_columns + 2));
	SET_STRING_ELT(Raet_names_y_dy, 0, mkChar("Year"));
	SET_STRING_ELT(Raet_names_y_dy, 1, mkChar("DayOfYear"));
	SET_STRING_ELT(Raet_names_y_dy, 2, mkChar("evapotr_cm"));
	SET_VECTOR_ELT(Raet_names_dy, 1, Raet_names_y_dy);

	PROTECT(Rdeep_drain_names_dy = allocVector(VECSXP, 2));
	PROTECT(Rdeep_drain_names_y_dy = allocVector(STRSXP, p_Rdeep_drain_columns + 2));
	SET_STRING_ELT(Rdeep_drain_names_y_dy, 0, mkChar("Year"));
	SET_STRING_ELT(Rdeep_drain_names_y_dy, 1, mkChar("DayOfYear"));
	SET_STRING_ELT(Rdeep_drain_names_y_dy, 2, mkChar("lowLayerDrain_cm"));
	SET_VECTOR_ELT(Rdeep_drain_names_dy, 1, Rdeep_drain_names_y_dy);
	/*
	 PROTECT(Restabs_names_dy = allocVector(VECSXP, 2));
	 PROTECT(Restabs_names_y_dy = allocVector(STRSXP, p_Restabs_columns+2));
	 SET_STRING_ELT(Restabs_names_y_dy, 0, mkChar("Year"));
	 SET_STRING_ELT(Restabs_names_y_dy, 1, mkChar("DayOfYear"));
	 SET_STRING_ELT(Restabs_names_y_dy, 2, mkChar("YearlyEstabResults"));
	 SET_VECTOR_ELT(Restabs_names_dy, 1, Restabs_names_y_dy);
	 */
	PROTECT(Revap_soil_names_dy = allocVector(VECSXP, 2));
	PROTECT(Revap_soil_names_y_dy = allocVector(STRSXP, p_Revap_soil_columns + 2));
	SET_STRING_ELT(Revap_soil_names_y_dy, 0, mkChar("Year"));
	SET_STRING_ELT(Revap_soil_names_y_dy, 1, mkChar("DayOfYear"));
	for (i = 0; i < p_Revap_soil_columns; i++)
		SET_STRING_ELT(Revap_soil_names_y_dy, i + 2, mkChar(Layers_names[i]));
	SET_VECTOR_ELT(Revap_soil_names_dy, 1, Revap_soil_names_y_dy);

	PROTECT(Revap_surface_names_dy = allocVector(VECSXP, 2));
	PROTECT(Revap_surface_names_y_dy = allocVector(STRSXP, p_Revap_surface_columns + 2));
	SET_STRING_ELT(Revap_surface_names_y_dy, 0, mkChar("Year"));
	SET_STRING_ELT(Revap_surface_names_y_dy, 1, mkChar("DayOfYear"));
	for (i = 0; i < p_Revap_surface_columns; i++)
		SET_STRING_ELT(Revap_surface_names_y_dy, i + 2, mkChar(Cevap_surface_names[i]));
	SET_VECTOR_ELT(Revap_surface_names_dy, 1, Revap_surface_names_y_dy);

	PROTECT(Rhydred_names_dy = allocVector(VECSXP, 2));
	PROTECT(Rhydred_names_y_dy = allocVector(STRSXP, p_Rhydred_columns + 2));
	SET_STRING_ELT(Rhydred_names_y_dy, 0, mkChar("Year"));
	SET_STRING_ELT(Rhydred_names_y_dy, 1, mkChar("DayOfYear"));
	for (i = 0; i < 4; i++) {
		for (j = 0; j < tLayers; j++) {
			strcpy(Ctemp, Chydred_names[i]);
			strcat(Ctemp, Layers_names[j]);
			SET_STRING_ELT(Rhydred_names_y_dy, (i * tLayers + j) + 2, mkChar(Ctemp));
		}
	}
	SET_VECTOR_ELT(Rhydred_names_dy, 1, Rhydred_names_y_dy);

	PROTECT(Rinfiltration_names_dy = allocVector(VECSXP, 2));
	PROTECT(Rinfiltration_names_y_dy = allocVector(STRSXP, p_Rinfiltration_columns + 2));
	SET_STRING_ELT(Rinfiltration_names_y_dy, 0, mkChar("Year"));
	SET_STRING_ELT(Rinfiltration_names_y_dy, 1, mkChar("DayOfYear"));
	SET_STRING_ELT(Rinfiltration_names_y_dy, 2, mkChar("soil_inf"));
	SET_VECTOR_ELT(Rinfiltration_names_dy, 1, Rinfiltration_names_y_dy);

	PROTECT(Rinterception_names_dy = allocVector(VECSXP, 2));
	PROTECT(Rinterception_names_y_dy = allocVector(STRSXP, p_Rinterception_columns + 2));
	SET_STRING_ELT(Rinterception_names_y_dy, 0, mkChar("Year"));
	SET_STRING_ELT(Rinterception_names_y_dy, 1, mkChar("DayOfYear"));
	for (i = 0; i < p_Rinterception_columns; i++)
		SET_STRING_ELT(Rinterception_names_y_dy, i + 2, mkChar(Cinterception_names[i]));
	SET_VECTOR_ELT(Rinterception_names_dy, 1, Rinterception_names_y_dy);

	PROTECT(Rpercolation_names_dy = allocVector(VECSXP, 2));
	PROTECT(Rpercolation_names_y_dy = allocVector(STRSXP, p_Rpercolation_columns + 2));
	SET_STRING_ELT(Rpercolation_names_y_dy, 0, mkChar("Year"));
	SET_STRING_ELT(Rpercolation_names_y_dy, 1, mkChar("DayOfYear"));
	for (i = 0; i < (tLayers - 1); i++)
		SET_STRING_ELT(Rpercolation_names_y_dy, i + 2, mkChar(Layers_names[i]));
	SET_VECTOR_ELT(Rpercolation_names_dy, 1, Rpercolation_names_y_dy);

	PROTECT(Rpet_names_dy = allocVector(VECSXP, 2));
	PROTECT(Rpet_names_y_dy = allocVector(STRSXP, p_Rpet_columns + 2));
	SET_STRING_ELT(Rpet_names_y_dy, 0, mkChar("Year"));
	SET_STRING_ELT(Rpet_names_y_dy, 1, mkChar("DayOfYear"));
	SET_STRING_ELT(Rpet_names_y_dy, 2, mkChar("pet_cm"));
	SET_VECTOR_ELT(Rpet_names_dy, 1, Rpet_names_y_dy);

	PROTECT(Rprecip_names_dy = allocVector(VECSXP, 2));
	PROTECT(Rprecip_names_y_dy = allocVector(STRSXP, p_Rprecip_columns + 2));
	SET_STRING_ELT(Rprecip_names_y_dy, 0, mkChar("Year"));
	SET_STRING_ELT(Rprecip_names_y_dy, 1, mkChar("DayOfYear"));
	for (i = 0; i < p_Rprecip_columns; i++)
		SET_STRING_ELT(Rprecip_names_y_dy, i + 2, mkChar(Cprecip_names[i]));
	SET_VECTOR_ELT(Rprecip_names_dy, 1, Rprecip_names_y_dy); //17

	PROTECT(Rrunoff_names_dy = allocVector(VECSXP, 2));
	PROTECT(Rrunoff_names_y_dy = allocVector(STRSXP, p_Rrunoff_columns + 2));
	SET_STRING_ELT(Rrunoff_names_y_dy, 0, mkChar("Year"));
	SET_STRING_ELT(Rrunoff_names_y_dy, 1, mkChar("DayOfYear"));
	for (i = 0; i < p_Rrunoff_columns; i++)
		SET_STRING_ELT(Rrunoff_names_y_dy, i + 2, mkChar(Crunoff_names[i]));
	SET_VECTOR_ELT(Rrunoff_names_dy, 1, Rrunoff_names_y_dy);

	PROTECT(Rsnowpack_names_dy = allocVector(VECSXP, 2));
	PROTECT(Rsnowpack_names_y_dy = allocVector(STRSXP, p_Rsnowpack_columns + 2));
	SET_STRING_ELT(Rsnowpack_names_y_dy, 0, mkChar("Year"));
	SET_STRING_ELT(Rsnowpack_names_y_dy, 1, mkChar("DayOfYear"));
	for (i = 0; i < p_Rsnowpack_columns; i++)
		SET_STRING_ELT(Rsnowpack_names_y_dy, i + 2, mkChar(Csnowpack_names[i]));
	SET_VECTOR_ELT(Rsnowpack_names_dy, 1, Rsnowpack_names_y_dy);

	PROTECT(Rsoil_temp_names_dy = allocVector(VECSXP, 2));
	PROTECT(Rsoil_temp_names_y_dy = allocVector(STRSXP, p_Rsoil_temp_columns + 2));
	SET_STRING_ELT(Rsoil_temp_names_y_dy, 0, mkChar("Year"));
	SET_STRING_ELT(Rsoil_temp_names_y_dy, 1, mkChar("DayOfYear"));
	for (i = 0; i < tLayers; i++)
		SET_STRING_ELT(Rsoil_temp_names_y_dy, i + 2, mkChar(Layers_names[i]));
	SET_VECTOR_ELT(Rsoil_temp_names_dy, 1, Rsoil_temp_names_y_dy);

	PROTECT(Rsurface_water_names_dy = allocVector(VECSXP, 2));
	PROTECT(Rsurface_water_names_y_dy = allocVector(STRSXP, p_Rsurface_water_columns + 2));
	SET_STRING_ELT(Rsurface_water_names_y_dy, 0, mkChar("Year"));
	SET_STRING_ELT(Rsurface_water_names_y_dy, 1, mkChar("DayOfYear"));
	SET_STRING_ELT(Rsurface_water_names_y_dy, 2, mkChar("surfaceWater_cm"));
	SET_VECTOR_ELT(Rsurface_water_names_dy, 1, Rsurface_water_names_y_dy);

	PROTECT(Rsw_pot_names_dy = allocVector(VECSXP, 2));
	PROTECT(Rsw_pot_names_y_dy = allocVector(STRSXP, p_Rsw_pot_columns + 2));
	SET_STRING_ELT(Rsw_pot_names_y_dy, 0, mkChar("Year"));
	SET_STRING_ELT(Rsw_pot_names_y_dy, 1, mkChar("DayOfYear"));
	for (i = 0; i < tLayers; i++)
		SET_STRING_ELT(Rsw_pot_names_y_dy, i + 2, mkChar(Layers_names[i]));
	SET_VECTOR_ELT(Rsw_pot_names_dy, 1, Rsw_pot_names_y_dy);

	PROTECT(Rswa_names_dy = allocVector(VECSXP, 2));
	PROTECT(Rswa_names_y_dy = allocVector(STRSXP, p_Rswa_columns + 2));
	SET_STRING_ELT(Rswa_names_y_dy, 0, mkChar("Year"));
	SET_STRING_ELT(Rswa_names_y_dy, 1, mkChar("DayOfYear"));
	for (i = 0; i < tLayers; i++)
		SET_STRING_ELT(Rswa_names_y_dy, i + 2, mkChar(Layers_names[i]));
	SET_VECTOR_ELT(Rswa_names_dy, 1, Rswa_names_y_dy); //28

	PROTECT(Rswc_names_dy = allocVector(VECSXP, 2));
	PROTECT(Rswc_names_y_dy = allocVector(STRSXP, p_Rswc_columns + 2));
	SET_STRING_ELT(Rswc_names_y_dy, 0, mkChar("Year"));
	SET_STRING_ELT(Rswc_names_y_dy, 1, mkChar("DayOfYear"));
	for (i = 0; i < tLayers; i++)
		SET_STRING_ELT(Rswc_names_y_dy, i + 2, mkChar(Layers_names[i]));
	SET_VECTOR_ELT(Rswc_names_dy, 1, Rswc_names_y_dy);

	PROTECT(Rtemp_names_dy = allocVector(VECSXP, 2));
	PROTECT(Rtemp_names_y_dy = allocVector(STRSXP, p_Rtemp_columns + 2));
	SET_STRING_ELT(Rtemp_names_y_dy, 0, mkChar("Year"));
	SET_STRING_ELT(Rtemp_names_y_dy, 1, mkChar("DayOfYear"));
	for (i = 0; i < p_Rtemp_columns; i++)
		SET_STRING_ELT(Rtemp_names_y_dy, i + 2, mkChar(Ctemp_names[i]));
	SET_VECTOR_ELT(Rtemp_names_dy, 1, Rtemp_names_y_dy);

	PROTECT(Rtransp_names_dy = allocVector(VECSXP, 2));
	PROTECT(Rtransp_names_y_dy = allocVector(STRSXP, p_Rtransp_columns + 2));
	SET_STRING_ELT(Rtransp_names_y_dy, 0, mkChar("Year"));
	SET_STRING_ELT(Rtransp_names_y_dy, 1, mkChar("DayOfYear"));
	for (i = 0; i < 4; i++) {
		for (j = 0; j < tLayers; j++) {
			strcpy(Ctemp, Ctransp_names[i]);
			strcat(Ctemp, Layers_names[j]);
			SET_STRING_ELT(Rtransp_names_y_dy, (i * tLayers + j) + 2, mkChar(Ctemp));
		}
	}
	SET_VECTOR_ELT(Rtransp_names_dy, 1, Rtransp_names_y_dy);

	PROTECT(Rvwc_names_dy = allocVector(VECSXP, 2));
	PROTECT(Rvwc_names_y_dy = allocVector(STRSXP, p_Rvwc_columns + 2));
	SET_STRING_ELT(Rvwc_names_y_dy, 0, mkChar("Year"));
	SET_STRING_ELT(Rvwc_names_y_dy, 1, mkChar("DayOfYear"));
	for (i = 0; i < tLayers; i++)
		SET_STRING_ELT(Rvwc_names_y_dy, i + 2, mkChar(Layers_names[i]));
	SET_VECTOR_ELT(Rvwc_names_dy, 1, Rvwc_names_y_dy);

	PROTECT(Rwetdays_names_dy = allocVector(VECSXP, 2));
	PROTECT(Rwetdays_names_y_dy = allocVector(STRSXP, p_Rwetdays_columns + 2));
	SET_STRING_ELT(Rwetdays_names_y_dy, 0, mkChar("Year"));
	SET_STRING_ELT(Rwetdays_names_y_dy, 1, mkChar("DayOfYear"));
	for (i = 0; i < tLayers; i++)
		SET_STRING_ELT(Rwetdays_names_y_dy, i + 2, mkChar(Layers_names[i]));
	SET_VECTOR_ELT(Rwetdays_names_dy, 1, Rwetdays_names_y_dy);

	//year
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 0), 0), R_DimNamesSymbol, Raet_names_yr);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 1), 0), R_DimNamesSymbol, Rdeep_drain_names_yr);
	//setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 2), 0), R_DimNamesSymbol, Restabs_names_yr);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 3), 0), R_DimNamesSymbol, Revap_soil_names_yr);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 4), 0), R_DimNamesSymbol, Revap_surface_names_yr);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 5), 0), R_DimNamesSymbol, Rhydred_names_yr);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 6), 0), R_DimNamesSymbol, Rinfiltration_names_yr);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 7), 0), R_DimNamesSymbol, Rinterception_names_yr);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 8), 0), R_DimNamesSymbol, Rpercolation_names_yr);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 9), 0), R_DimNamesSymbol, Rpet_names_yr);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 10), 0), R_DimNamesSymbol, Rprecip_names_yr);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 11), 0), R_DimNamesSymbol, Rrunoff_names_yr);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 12), 0), R_DimNamesSymbol, Rsnowpack_names_yr);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 13), 0), R_DimNamesSymbol, Rsoil_temp_names_yr);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 14), 0), R_DimNamesSymbol, Rsurface_water_names_yr);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 15), 0), R_DimNamesSymbol, Rsw_pot_names_yr);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 16), 0), R_DimNamesSymbol, Rswa_names_yr);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 17), 0), R_DimNamesSymbol, Rswc_names_yr);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 18), 0), R_DimNamesSymbol, Rtemp_names_yr);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 19), 0), R_DimNamesSymbol, Rtransp_names_yr);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 20), 0), R_DimNamesSymbol, Rvwc_names_yr);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 21), 0), R_DimNamesSymbol, Rwetdays_names_yr);
	//month
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 0), 1), R_DimNamesSymbol, Raet_names_mo);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 1), 1), R_DimNamesSymbol, Rdeep_drain_names_mo);
	//setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 2), 1), R_DimNamesSymbol, Restabs_names_mo);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 3), 1), R_DimNamesSymbol, Revap_soil_names_mo);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 4), 1), R_DimNamesSymbol, Revap_surface_names_mo);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 5), 1), R_DimNamesSymbol, Rhydred_names_mo);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 6), 1), R_DimNamesSymbol, Rinfiltration_names_mo);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 7), 1), R_DimNamesSymbol, Rinterception_names_mo);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 8), 1), R_DimNamesSymbol, Rpercolation_names_mo);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 9), 1), R_DimNamesSymbol, Rpet_names_mo);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 10), 1), R_DimNamesSymbol, Rprecip_names_mo);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 11), 1), R_DimNamesSymbol, Rrunoff_names_mo);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 12), 1), R_DimNamesSymbol, Rsnowpack_names_mo);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 13), 1), R_DimNamesSymbol, Rsoil_temp_names_mo);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 14), 1), R_DimNamesSymbol, Rsurface_water_names_mo);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 15), 1), R_DimNamesSymbol, Rsw_pot_names_mo);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 16), 1), R_DimNamesSymbol, Rswa_names_mo);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 17), 1), R_DimNamesSymbol, Rswc_names_mo);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 18), 1), R_DimNamesSymbol, Rtemp_names_mo);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 19), 1), R_DimNamesSymbol, Rtransp_names_mo);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 20), 1), R_DimNamesSymbol, Rvwc_names_mo);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 21), 1), R_DimNamesSymbol, Rwetdays_names_mo);
	//wk
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 0), 2), R_DimNamesSymbol, Raet_names_wk);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 1), 2), R_DimNamesSymbol, Rdeep_drain_names_wk);
	//setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 2), 2), R_DimNamesSymbol, Restabs_names_wk);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 3), 2), R_DimNamesSymbol, Revap_soil_names_wk);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 4), 2), R_DimNamesSymbol, Revap_surface_names_wk);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 5), 2), R_DimNamesSymbol, Rhydred_names_wk);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 6), 2), R_DimNamesSymbol, Rinfiltration_names_wk);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 7), 2), R_DimNamesSymbol, Rinterception_names_wk);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 8), 2), R_DimNamesSymbol, Rpercolation_names_wk);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 9), 2), R_DimNamesSymbol, Rpet_names_wk);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 10), 2), R_DimNamesSymbol, Rprecip_names_wk);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 11), 2), R_DimNamesSymbol, Rrunoff_names_wk);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 12), 2), R_DimNamesSymbol, Rsnowpack_names_wk);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 13), 2), R_DimNamesSymbol, Rsoil_temp_names_wk);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 14), 2), R_DimNamesSymbol, Rsurface_water_names_wk);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 15), 2), R_DimNamesSymbol, Rsw_pot_names_wk);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 16), 2), R_DimNamesSymbol, Rswa_names_wk);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 17), 2), R_DimNamesSymbol, Rswc_names_wk);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 18), 2), R_DimNamesSymbol, Rtemp_names_wk);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 19), 2), R_DimNamesSymbol, Rtransp_names_wk);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 20), 2), R_DimNamesSymbol, Rvwc_names_wk);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 21), 2), R_DimNamesSymbol, Rwetdays_names_wk);
	//day
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 0), 3), R_DimNamesSymbol, Raet_names_dy);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 1), 3), R_DimNamesSymbol, Rdeep_drain_names_dy);
	//setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 2), 3), R_DimNamesSymbol, Restabs_names_dy);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 3), 3), R_DimNamesSymbol, Revap_soil_names_dy);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 4), 3), R_DimNamesSymbol, Revap_surface_names_dy);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 5), 3), R_DimNamesSymbol, Rhydred_names_dy);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 6), 3), R_DimNamesSymbol, Rinfiltration_names_dy);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 7), 3), R_DimNamesSymbol, Rinterception_names_dy);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 8), 3), R_DimNamesSymbol, Rpercolation_names_dy);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 9), 3), R_DimNamesSymbol, Rpet_names_dy);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 10), 3), R_DimNamesSymbol, Rprecip_names_dy);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 11), 3), R_DimNamesSymbol, Rrunoff_names_dy);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 12), 3), R_DimNamesSymbol, Rsnowpack_names_dy);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 13), 3), R_DimNamesSymbol, Rsoil_temp_names_dy);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 14), 3), R_DimNamesSymbol, Rsurface_water_names_dy);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 15), 3), R_DimNamesSymbol, Rsw_pot_names_dy);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 16), 3), R_DimNamesSymbol, Rswa_names_dy);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 17), 3), R_DimNamesSymbol, Rswc_names_dy);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 18), 3), R_DimNamesSymbol, Rtemp_names_dy);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 19), 3), R_DimNamesSymbol, Rtransp_names_dy);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 20), 3), R_DimNamesSymbol, Rvwc_names_dy);
	setAttrib(VECTOR_ELT(VECTOR_ELT(SW_R_Data, 21), 3), R_DimNamesSymbol, Rwetdays_names_dy);

	PROTECT(SubListNames = allocVector(STRSXP, 4));
	for (i = 0; i < 4; i++)
		SET_STRING_ELT(SubListNames, i, mkChar(OutSubColNames[i]));

	setAttrib(VECTOR_ELT(SW_R_Data, 0), R_NamesSymbol, SubListNames);
	setAttrib(VECTOR_ELT(SW_R_Data, 1), R_NamesSymbol, SubListNames);
	setAttrib(VECTOR_ELT(SW_R_Data, 2), R_NamesSymbol, SubListNames);
	setAttrib(VECTOR_ELT(SW_R_Data, 3), R_NamesSymbol, SubListNames);
	setAttrib(VECTOR_ELT(SW_R_Data, 4), R_NamesSymbol, SubListNames);
	setAttrib(VECTOR_ELT(SW_R_Data, 5), R_NamesSymbol, SubListNames);
	setAttrib(VECTOR_ELT(SW_R_Data, 6), R_NamesSymbol, SubListNames);
	setAttrib(VECTOR_ELT(SW_R_Data, 7), R_NamesSymbol, SubListNames);
	setAttrib(VECTOR_ELT(SW_R_Data, 8), R_NamesSymbol, SubListNames);
	setAttrib(VECTOR_ELT(SW_R_Data, 9), R_NamesSymbol, SubListNames);
	setAttrib(VECTOR_ELT(SW_R_Data, 10), R_NamesSymbol, SubListNames);
	setAttrib(VECTOR_ELT(SW_R_Data, 11), R_NamesSymbol, SubListNames);
	setAttrib(VECTOR_ELT(SW_R_Data, 12), R_NamesSymbol, SubListNames);
	setAttrib(VECTOR_ELT(SW_R_Data, 13), R_NamesSymbol, SubListNames);
	setAttrib(VECTOR_ELT(SW_R_Data, 14), R_NamesSymbol, SubListNames);
	setAttrib(VECTOR_ELT(SW_R_Data, 15), R_NamesSymbol, SubListNames);
	setAttrib(VECTOR_ELT(SW_R_Data, 16), R_NamesSymbol, SubListNames);
	setAttrib(VECTOR_ELT(SW_R_Data, 17), R_NamesSymbol, SubListNames);
	setAttrib(VECTOR_ELT(SW_R_Data, 18), R_NamesSymbol, SubListNames);
	setAttrib(VECTOR_ELT(SW_R_Data, 19), R_NamesSymbol, SubListNames);
	setAttrib(VECTOR_ELT(SW_R_Data, 20), R_NamesSymbol, SubListNames);
	setAttrib(VECTOR_ELT(SW_R_Data, 21), R_NamesSymbol, SubListNames);

	PROTECT(OutputList_names = allocVector(STRSXP, 23));
	for (i = 0; i < 23; i++)
		SET_STRING_ELT(OutputList_names, i, mkChar(OutColNames[i]));

	setAttrib(SW_R_Data, R_NamesSymbol, OutputList_names);

	UNPROTECT(170);
	return (R_NilValue);
}
