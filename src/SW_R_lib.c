/*
 * SW_R_lib.c
 *
 *  Created on: Jun 25, 2013
 *      Author: Ryan Murphy
 */

// externs `*logfp`, `errstr`, `logged`, `QuietMode`, `EchoInits`
#include "SOILWAT2/generic.h"
#include "SOILWAT2/filefuncs.h" // externs `_firstfile`
#include "SOILWAT2/Times.h"
#include "SOILWAT2/SW_Defines.h"

#include "SOILWAT2/SW_Files.h"
#include "SOILWAT2/SW_Carbon.h" // externs `SW_Carbon`
#include "SOILWAT2/SW_SoilWater.h" // externs `SW_Soilwat`
#include "SOILWAT2/SW_VegEstab.h" // externs `SW_VegEstab`
#include "SOILWAT2/SW_Output.h"
#include "SOILWAT2/SW_Main_lib.h"

#include "rSW_Files.h"
#include "rSW_Model.h"
#include "rSW_Weather.h"
#include "rSW_Markov.h"
#include "rSW_Sky.h"
#include "rSW_VegProd.h"
#include "rSW_Site.h"
#include "rSW_VegEstab.h"
#include "rSW_Output.h"
#include "rSW_Carbon.h"
#include "rSW_SoilWater.h"
#include "rSW_Control.h"

#include "SW_R_lib.h"

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>



/* =================================================== */
/*                  Global Variables                   */
/* --------------------------------------------------- */

SEXP InputData;
SEXP WeatherList;
Bool useFiles;
Bool bWeatherList;



/* =================================================== */
/*                  Local Variables                    */
/* --------------------------------------------------- */
static SEXP Rlogfile;



/* =================================================== */
/*             Global Function Definitions             */
/* --------------------------------------------------- */

/**
 * Determines if a constant in the Parton equation 2.21 is invalid and would
 * thus cause extreme soil temperature values (see SW_Flow_lib.c ~1770)
 *
 * @param  none
 * @return an R boolean that denotes an error (TRUE) or lack of (FALSE)
 *
 */
SEXP tempError(void) {
	SEXP swR_temp_error;
	PROTECT(swR_temp_error = NEW_LOGICAL(1));
	LOGICAL_POINTER(swR_temp_error)[0] = SW_Soilwat.soiltempError;
	UNPROTECT(1);
	return swR_temp_error;
}


/** Setup and construct model (independent of inputs)
*/
void setupSOILWAT2(SEXP inputOptions) {
	int i, argc;
	char *argv[7];
  #ifdef RSWDEBUG
  int debug = 0;
  #endif


  #ifdef RSWDEBUG
  if (debug) swprintf("Set args\n");
  #endif

	argc = length(inputOptions);
	if (argc > 7) {
		// fatal condition because argv is hard-coded to be of length 7; increase size of
		// argv if more command-line options are added to SOILWAT2 in the future
		sw_error(-1, "length(inputOptions) must be <= 7.");
	}
	for (i = 0; i < argc; i++) {
		argv[i] = (char *) CHAR(STRING_ELT(inputOptions, i));
	}

  #ifdef RSWDEBUG
	if (debug) swprintf("Set call arguments\n");
  #endif

	sw_init_args(argc, argv);

  #ifdef RSWDEBUG
  if (debug) swprintf("Initialize SOILWAT ...");
	#endif

	SW_CTL_setup_model(_firstfile);
	rSW_CTL_setup_model2();
}


SEXP onGetInputDataFromFiles(SEXP inputOptions) {
  SEXP swInputData, SW_DataList, swLog, oRlogfile;
  #ifdef RSWDEBUG
  int debug = 0;
  #endif

  logged = FALSE;
  logfp = NULL;

  #ifdef RSWDEBUG
  if (debug) swprintf("Set log\n");
  #endif
  PROTECT(swLog = MAKE_CLASS("swLog"));
  PROTECT(oRlogfile = NEW_OBJECT(swLog));
  PROTECT(Rlogfile = GET_SLOT(oRlogfile,install("LogData")));

  // setup and construct model (independent of inputs)
  setupSOILWAT2(inputOptions);

  // read user inputs: from files
  #ifdef RSWDEBUG
  if (debug) swprintf("Read input from disk files into SOILWAT2 variables\n");
  #endif
  rSW_CTL_obtain_inputs(TRUE);

  // initialize simulation run (based on user inputs)
  #ifdef RSWDEBUG
  if (debug) swprintf(" init simulation run ...\n");
  #endif
  SW_CTL_init_run();

  #ifdef RSWDEBUG
  if (debug) swprintf("onGetInputDataFromFiles: copy data from SOILWAT2 "
    "variables to rSOILWAT2 S4 classes: ");
  #endif

  PROTECT(swInputData = MAKE_CLASS("swInputData"));
  PROTECT(SW_DataList = NEW_OBJECT(swInputData));


  SET_SLOT(SW_DataList, install("files"), onGet_SW_F());
  #ifdef RSWDEBUG
  if (debug) swprintf(" 'files'");
  #endif

  SET_SLOT(SW_DataList, install("years"), onGet_SW_MDL());
  #ifdef RSWDEBUG
  if (debug) swprintf(" > 'model'");
  #endif

  SET_SLOT(SW_DataList, install("weather"), onGet_SW_WTH_setup());
  #ifdef RSWDEBUG
  if (debug) swprintf(" > 'weather-setup'");
  #endif

  SET_SLOT(SW_DataList, install("weatherHistory"), onGet_WTH_DATA());
  #ifdef RSWDEBUG
  if (debug) swprintf(" > 'weather-data'");
  #endif

  SET_SLOT(SW_DataList, install("cloud"), onGet_SW_SKY());
  #ifdef RSWDEBUG
  if (debug) swprintf(" > 'climate'");
  #endif

  if (LOGICAL(GET_SLOT(GET_SLOT(SW_DataList, install("weather")), install("use_weathergenerator")))[0]) {
    SET_SLOT(SW_DataList, install("markov"), onGet_MKV());
    #ifdef RSWDEBUG
    if (debug) swprintf(" > 'mwgen'");
    #endif
  }

  SET_SLOT(SW_DataList, install("prod"), onGet_SW_VPD());
  #ifdef RSWDEBUG
  if (debug) swprintf(" > 'veg'");
  #endif

  SET_SLOT(SW_DataList, install("site"), onGet_SW_SIT());
  #ifdef RSWDEBUG
  if (debug) swprintf(" > 'site'");
  #endif

  SET_SLOT(SW_DataList, install("soils"), onGet_SW_LYR());
  #ifdef RSWDEBUG
  if (debug) swprintf(" > 'soils'");
  #endif

  SET_SLOT(SW_DataList, install("estab"), onGet_SW_VES());
  #ifdef RSWDEBUG
  if (debug) swprintf(" > 'establishment'");
  #endif

  SET_SLOT(SW_DataList, install("output"), onGet_SW_OUT());
  #ifdef RSWDEBUG
  if (debug) swprintf(" > 'ouput'");
  #endif

  SET_SLOT(SW_DataList, install("carbon"), onGet_SW_CARBON());
  #ifdef RSWDEBUG
  if (debug) swprintf(" > 'CO2'");
  #endif

  SET_SLOT(SW_DataList, install("swc"), onGet_SW_SWC());
  #ifdef RSWDEBUG
  if (debug) swprintf(" > 'swc'");
  #endif

  SET_SLOT(SW_DataList, install("log"), oRlogfile);

  // de-allocate all memory, but `p_OUT`
  #ifdef RSWDEBUG
  if (debug) swprintf(" > de-allocate most memory; \n");
  #endif
  SW_CTL_clear_model(FALSE);

  #ifdef RSWDEBUG
  if (debug) swprintf(" onGetInputDataFromFiles completed.\n");
  #endif

  UNPROTECT(5);
  return SW_DataList;
}

SEXP start(SEXP inputOptions, SEXP inputData, SEXP weatherList, SEXP quiet) {
	SEXP outputData, swLog, oRlogfile;
  #ifdef RSWDEBUG
  int debug = 0;
  #endif

	logged = FALSE;
	if (LOGICAL(coerceVector(quiet, LGLSXP))[0]) {
		// tell 'LogError' that R should NOT print messages to the console
		logfp = NULL;
	} else {
		// tell 'LogError' that R should print messages to the console
		logfp = (FILE *) swTRUE; // any non-NULL file pointer
	}

	if (isNull(inputData)) {
		useFiles = TRUE;
	} else {
		useFiles = FALSE;
		InputData = inputData;
	}

	//This is used to minimize copying weather data between similiar runs.
	if (isNull(weatherList)) {
		bWeatherList = FALSE;
	} else {
		bWeatherList = TRUE;
		WeatherList = weatherList;
	}

  #ifdef RSWDEBUG
  if (debug) swprintf("'start': create log ...");
  #endif
	PROTECT(swLog = MAKE_CLASS("swLog"));
	PROTECT(oRlogfile = NEW_OBJECT(swLog));
	PROTECT(Rlogfile = GET_SLOT(oRlogfile,install("LogData")));

  // setup and construct model (independent of inputs)
  #ifdef RSWDEBUG
  if (debug) swprintf(" input arguments & setup model ...");
  #endif
	setupSOILWAT2(inputOptions);

	// read user inputs: either from files or from memory (depending on useFiles)
	#ifdef RSWDEBUG
	if (debug) swprintf(" obtain inputs ...");
	#endif
	rSW_CTL_obtain_inputs(useFiles);

	// initialize simulation run (based on user inputs)
	#ifdef RSWDEBUG
	if (debug) swprintf(" init simulation run ...");
	#endif
	SW_CTL_init_run();

  // initialize output
  #ifdef RSWDEBUG
  if (debug) swprintf(" setup output variables ...");
  #endif
	SW_OUT_set_ncol();
	SW_OUT_set_colnames();
	PROTECT(outputData = onGetOutput(inputData));
	setGlobalrSOILWAT2_OutputVariables(outputData);

  // run simulation: loop through each year
  #ifdef RSWDEBUG
  if (debug) swprintf(" run SOILWAT2 ...");
  #endif
	SW_CTL_main();

   // de-allocate all memory, but let R handle `p_OUT`
  #ifdef RSWDEBUG
  if (debug) swprintf(" clean up ...");
  #endif
	SW_CTL_clear_model(FALSE);

  #ifdef RSWDEBUG
  if (debug) swprintf(" completed.\n");
  #endif

	UNPROTECT(4);

	return(outputData);
}


/** Expose SOILWAT2 constants and defines to internal R code of rSOILWAT2
  @return A list with six elements: one element `kINT` for integer constants;
    other elements contain vegetation keys, `VegTypes`; output keys, `OutKeys`;
    output periods, `OutPeriods`; output aggregation types, `OutAggs`; and names of
    input files, `InFiles`.
 */
SEXP sw_consts(void) {
  #ifdef RSWDEBUG
  int debug = 0;
  #endif

  const int nret = 7; // length of cret
  const int nINT = 10; // length of vINT and cINT
  const int nNUM = 1; // length of vNUM and cNUM

  #ifdef RSWDEBUG
  if (debug) swprintf("sw_consts: define variables ... ");
  #endif

  SEXP ret, cnames, ret_num, ret_int, ret_int2, ret_str1, ret_str2, ret_str3,
    ret_infiles;
  int i;
  int *pvINT;
  double *pvNUM;
  char *cret[] = {"kNUM", "kINT", "VegTypes", "OutKeys", "OutPeriods",
    "OutAggs", "InFiles"};

  double vNUM[] = {SW_MISSING};
  char *cNUM[] = {"SW_MISSING"};

  int vINT[] = {SW_NFILES, MAX_LAYERS, MAX_TRANSP_REGIONS, MAX_NYEAR, eSW_NoTime,
    SW_OUTNPERIODS, SW_OUTNKEYS, SW_NSUMTYPES, NVEGTYPES, OUT_DIGITS};
  char *cINT[] = {"SW_NFILES", "MAX_LAYERS", "MAX_TRANSP_REGIONS", "MAX_NYEAR",
    "eSW_NoTime", "SW_OUTNPERIODS", "SW_OUTNKEYS", "SW_NSUMTYPES", "NVEGTYPES",
    "OUT_DIGITS"};
  int vINT2[] = {SW_TREES, SW_SHRUB, SW_FORBS, SW_GRASS};
  char *cINT2[] = {"SW_TREES", "SW_SHRUB", "SW_FORBS", "SW_GRASS"};

  char *vSTR1[] = { SW_WETHR, SW_TEMP, SW_PRECIP, SW_SOILINF, SW_RUNOFF, SW_ALLH2O, SW_VWCBULK,
			SW_VWCMATRIC, SW_SWCBULK, SW_SWABULK, SW_SWAMATRIC, SW_SWA, SW_SWPMATRIC,
			SW_SURFACEW, SW_TRANSP, SW_EVAPSOIL, SW_EVAPSURFACE, SW_INTERCEPTION,
			SW_LYRDRAIN, SW_HYDRED, SW_ET, SW_AET, SW_PET, SW_WETDAY, SW_SNOWPACK,
			SW_DEEPSWC, SW_SOILTEMP, SW_FROZEN,
			SW_ALLVEG, SW_ESTAB, SW_CO2EFFECTS, SW_BIOMASS };  // TODO: this is identical to SW_Output.c/key2str
  char *cSTR1[] = {"SW_WETHR", "SW_TEMP", "SW_PRECIP", "SW_SOILINF", "SW_RUNOFF",
    "SW_ALLH2O", "SW_VWCBULK", "SW_VWCMATRIC", "SW_SWCBULK", "SW_SWABULK",
    "SW_SWAMATRIC", "SW_SWA", "SW_SWPMATRIC", "SW_SURFACEW", "SW_TRANSP", "SW_EVAPSOIL",
    "SW_EVAPSURFACE", "SW_INTERCEPTION", "SW_LYRDRAIN", "SW_HYDRED", "SW_ET", "SW_AET",
    "SW_PET", "SW_WETDAY", "SW_SNOWPACK", "SW_DEEPSWC", "SW_SOILTEMP", "SW_FROZEN", "SW_ALLVEG",
    "SW_ESTAB", "SW_CO2EFFECTS", "SW_BIOMASS"};
  char *vSTR2[] = {SW_DAY, SW_WEEK, SW_MONTH, SW_YEAR}; // TODO: this is identical to SW_Output.c/pd2str
  char *cSTR2[] = {"SW_DAY", "SW_WEEK", "SW_MONTH", "SW_YEAR"};
  char *vSTR3[] = {SW_SUM_OFF, SW_SUM_SUM, SW_SUM_AVG, SW_SUM_FNL}; // TODO: this is identical to SW_Output.c/styp2str
  char *cSTR3[] = {"SW_SUM_OFF", "SW_SUM_SUM", "SW_SUM_AVG", "SW_SUM_FNL"};
  char *cInF[] = {"eFirst", "eModel", "eLog", "eSite", "eLayers", "eWeather",
    "eMarkovProb",  "eMarkovCov", "eSky", "eVegProd", "eVegEstab", "eCarbon", "eSoilwat",
    "eOutput", "eOutputDaily","eOutputWeekly","eOutputMonthly","eOutputYearly",
    "eOutputDaily_soil","eOutputWeekly_soil","eOutputMonthly_soil","eOutputYearly_soil"}; // TODO: this must match SW_Files.h/SW_FileIndex

  // create vector of numeric/real/double constants
  #ifdef RSWDEBUG
  if (debug) swprintf(" create ret_num ...");
  #endif
  PROTECT(ret_num = allocVector(REALSXP, nNUM));
  pvNUM = REAL(ret_num);
  PROTECT(cnames = allocVector(STRSXP, nNUM));
  for (i = 0; i < nNUM; i++) {
    pvNUM[i] = vNUM[i];
    SET_STRING_ELT(cnames, i, mkChar(cNUM[i]));
  }
  namesgets(ret_num, cnames);

  // create vector of integer constants
  #ifdef RSWDEBUG
  if (debug) swprintf(" create ret_int ...");
  #endif
  PROTECT(ret_int = allocVector(INTSXP, nINT));
  pvINT = INTEGER(ret_int);
  PROTECT(cnames = allocVector(STRSXP, nINT));
  for (i = 0; i < nINT; i++) {
    pvINT[i] = vINT[i];
    SET_STRING_ELT(cnames, i, mkChar(cINT[i]));
  }
  namesgets(ret_int, cnames);

  // create vector of vegetation types
  #ifdef RSWDEBUG
  if (debug) swprintf(" create ret_int2 ...");
  #endif
  PROTECT(ret_int2 = allocVector(INTSXP, NVEGTYPES));
  pvINT = INTEGER(ret_int2);
  PROTECT(cnames = allocVector(STRSXP, NVEGTYPES));
  for (i = 0; i < NVEGTYPES; i++) {
    pvINT[i] = vINT2[i];
    SET_STRING_ELT(cnames, i, mkChar(cINT2[i]));
  }
  namesgets(ret_int2, cnames);

  // create vector of output key constants
  #ifdef RSWDEBUG
  if (debug) swprintf(" create ret_str1 ...");
  #endif
  PROTECT(ret_str1 = allocVector(STRSXP, SW_OUTNKEYS));
  PROTECT(cnames = allocVector(STRSXP, SW_OUTNKEYS));
  for (i = 0; i < SW_OUTNKEYS; i++) {
    SET_STRING_ELT(ret_str1, i, mkChar(vSTR1[i]));
    SET_STRING_ELT(cnames, i, mkChar(cSTR1[i]));
  }
  namesgets(ret_str1, cnames);

  // create vector of output period constants
  #ifdef RSWDEBUG
  if (debug) swprintf(" create ret_str2 ...");
  #endif
  PROTECT(ret_str2 = allocVector(STRSXP, SW_OUTNPERIODS));
  PROTECT(cnames = allocVector(STRSXP, SW_OUTNPERIODS));
  for (i = 0; i < SW_OUTNPERIODS; i++) {
    SET_STRING_ELT(ret_str2, i, mkChar(vSTR2[i]));
    SET_STRING_ELT(cnames, i, mkChar(cSTR2[i]));
  }
  namesgets(ret_str2, cnames);

  // create vector of output summary constants
  #ifdef RSWDEBUG
  if (debug) swprintf(" create ret_str3 ...");
  #endif
  PROTECT(ret_str3 = allocVector(STRSXP, SW_NSUMTYPES));
  PROTECT(cnames = allocVector(STRSXP, SW_NSUMTYPES));
  for (i = 0; i < SW_NSUMTYPES; i++) {
    SET_STRING_ELT(ret_str3, i, mkChar(vSTR3[i]));
    SET_STRING_ELT(cnames, i, mkChar(cSTR3[i]));
  }
  namesgets(ret_str3, cnames);

  // create vector of input file descriptors
  #ifdef RSWDEBUG
  if (debug) swprintf(" create ret_infiles ...");
  #endif
  PROTECT(ret_infiles = allocVector(INTSXP, SW_NFILES));
  pvINT = INTEGER(ret_infiles);
  PROTECT(cnames = allocVector(STRSXP, SW_NFILES));
  for (i = 0; i < SW_NFILES; i++) {
    pvINT[i] = i;
    SET_STRING_ELT(cnames, i, mkChar(cInF[i]));
  }
  namesgets(ret_infiles, cnames);


  // combine vectors into a list and return
  #ifdef RSWDEBUG
  if (debug) swprintf(" create ret ...");
  #endif
  PROTECT(ret = allocVector(VECSXP, nret));
  PROTECT(cnames = allocVector(STRSXP, nret));
  for (i = 0; i < nret; i++)
    SET_STRING_ELT(cnames, i, mkChar(cret[i]));
  namesgets(ret, cnames);
  SET_VECTOR_ELT(ret, 0, ret_num);
  SET_VECTOR_ELT(ret, 1, ret_int);
  SET_VECTOR_ELT(ret, 2, ret_int2);
  SET_VECTOR_ELT(ret, 3, ret_str1);
  SET_VECTOR_ELT(ret, 4, ret_str2);
  SET_VECTOR_ELT(ret, 5, ret_str3);
  SET_VECTOR_ELT(ret, 6, ret_infiles);

  UNPROTECT(nret * 2 + 2);
  #ifdef RSWDEBUG
  if (debug) swprintf(" ... done.\n");
  #endif

  return ret;
}
