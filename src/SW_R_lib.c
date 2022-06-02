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
static Bool current_sw_quiet = swFALSE;



/* =================================================== */
/*             Global Function Definitions             */
/* --------------------------------------------------- */


/**
 * Turn on/off `SOILWAT2` messages including errors, notes, and warnings
 *
 * @param verbose A logical value.
 * @return The previous logical value.
 */
SEXP sw_quiet(SEXP quiet) {
	SEXP prev_quiet;

	PROTECT(prev_quiet = NEW_LOGICAL(1));
	LOGICAL_POINTER(prev_quiet)[0] = current_sw_quiet;

	if (LOGICAL(coerceVector(quiet, LGLSXP))[0]) {
		// tell `LogError()` that R should NOT print messages to the console
		logfp = NULL;
		current_sw_quiet = swTRUE;
	} else {
		// tell `LogError()` that R should print messages to the console
		logfp = (FILE *) swTRUE; // any non-NULL file pointer
		current_sw_quiet = swFALSE;
	}

	UNPROTECT(1);
	return prev_quiet;
}


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


SEXP onGetInputDataFromFiles(SEXP inputOptions, SEXP quiet) {
  SEXP swInputData, SW_DataList, swLog, oRlogfile;
  #ifdef RSWDEBUG
  int debug = 0;
  #endif

  logged = FALSE;
  sw_quiet(quiet);

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
  if (debug) {
    swprintf(
      "onGetInputDataFromFiles: copy data from SOILWAT2 variables "
      "to rSOILWAT2 S4 classes: "
    );
  }
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

  SET_SLOT(SW_DataList, install("weather"), onGet_SW_WTH());
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

  SET_SLOT(SW_DataList, install("soils"), onGet_SW_SOILS());
  #ifdef RSWDEBUG
  if (debug) swprintf(" > 'soils' + 'swrc parameters'");
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
	sw_quiet(quiet);

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
  @return A list with six elements:
    one element `kINT` for integer constants;
    other elements contain vegetation keys, `VegTypes`;
    output keys, `OutKeys`;
    output periods, `OutPeriods`;
    output aggregation types, `OutAggs`;
    and indices of input files, `InFiles`.
 */
SEXP sw_consts(void) {
  #ifdef RSWDEBUG
  int debug = 0;
  #endif

  const int nret = 9; // length of cret
  const int nINT = 13; // length of vINT and cINT
  const int nNUM = 1; // length of vNUM and cNUM

  #ifdef RSWDEBUG
  if (debug) swprintf("sw_consts: define variables ... ");
  #endif

  SEXP
    ret,
    cnames,
    ret_num,
    ret_int,
    ret_int2,
    ret_str1, ret_str2, ret_str3,
    ret_infiles,
    ret_swrc,
    ret_pdf;
  int i;
  int *pvINT;
  double *pvNUM;
  char *cret[] = {
    "kNUM",
    "kINT",
    "VegTypes",
    "OutKeys", "OutPeriods", "OutAggs",
    "InFiles",
    "SWRC_types",
    "PDF_types"
  };

  // Miscellaneous numerical constants
  double vNUM[] = {SW_MISSING};
  char *cNUM[] = {"SW_MISSING"};

  // Miscellaneous integer constants
  int vINT[] = {
    SW_NFILES, MAX_LAYERS, MAX_TRANSP_REGIONS, MAX_NYEAR,
    SWRC_PARAM_NMAX,
    eSW_NoTime, SW_OUTNPERIODS, SW_OUTNKEYS, SW_NSUMTYPES, NVEGTYPES,
    OUT_DIGITS,
    N_SWRCs, N_PDFs
  };
  char *cINT[] = {
    "SW_NFILES", "MAX_LAYERS", "MAX_TRANSP_REGIONS", "MAX_NYEAR",
    "SWRC_PARAM_NMAX",
    "eSW_NoTime", "SW_OUTNPERIODS", "SW_OUTNKEYS", "SW_NSUMTYPES", "NVEGTYPES",
    "OUT_DIGITS",
    "N_SWRCs", "N_PDFs"
  };

  // Vegetation types
  // NOTE: order must match their numeric values, i.e., how SOILWAT2 uses them
  int vINT2[] = {SW_TREES, SW_SHRUB, SW_FORBS, SW_GRASS};
  char *cINT2[] = {"SW_TREES", "SW_SHRUB", "SW_FORBS", "SW_GRASS"};

  // Output categories
  // NOTE: `cSTR1` must agree with SW_Output.c/key2str[]
  char *cSTR1[] = {
    "SW_WETHR", "SW_TEMP", "SW_PRECIP", "SW_SOILINF", "SW_RUNOFF",
    "SW_ALLH2O", "SW_VWCBULK", "SW_VWCMATRIC", "SW_SWCBULK", "SW_SWABULK",
    "SW_SWAMATRIC", "SW_SWA", "SW_SWPMATRIC", "SW_SURFACEW", "SW_TRANSP", "SW_EVAPSOIL",
    "SW_EVAPSURFACE", "SW_INTERCEPTION", "SW_LYRDRAIN", "SW_HYDRED", "SW_ET", "SW_AET",
    "SW_PET", "SW_WETDAY", "SW_SNOWPACK", "SW_DEEPSWC", "SW_SOILTEMP", "SW_FROZEN", "SW_ALLVEG",
    "SW_ESTAB", "SW_CO2EFFECTS", "SW_BIOMASS"
  };

  // Output time steps
  // Note: `cSTR2` must agree with SW_Output.c/pd2longstr[]
  char *cSTR2[] = {"SW_DAY", "SW_WEEK", "SW_MONTH", "SW_YEAR"};

  // Output aggregation types
  // Note: `cSTR3` must agree with SW_Output.c/styp2str
  char *cSTR3[] = {"SW_SUM_OFF", "SW_SUM_SUM", "SW_SUM_AVG", "SW_SUM_FNL"};

  // SOILWAT2 input files
  // Note: `cInF` must agree with SW_Files.h/SW_FileIndex
  char *cInF[] = {
    "eFirst",
    "eModel", "eLog",
    "eSite", "eLayers", "eSWRCp",
    "eWeather", "eMarkovProb", "eMarkovCov", "eSky",
    "eVegProd", "eVegEstab",
    "eCarbon",
    "eSoilwat",
    "eOutput", "eOutputDaily", "eOutputWeekly", "eOutputMonthly", "eOutputYearly",
    "eOutputDaily_soil", "eOutputWeekly_soil", "eOutputMonthly_soil", "eOutputYearly_soil"
  };


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
    SET_STRING_ELT(ret_str1, i, mkChar(key2str[i]));
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
    SET_STRING_ELT(ret_str2, i, mkChar(pd2longstr[i]));
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
    SET_STRING_ELT(ret_str3, i, mkChar(styp2str[i]));
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

  // create vector of SWRC types
  #ifdef RSWDEBUG
  if (debug) swprintf(" create ret_swrc ...");
  #endif
  PROTECT(ret_swrc = allocVector(INTSXP, N_SWRCs));
  pvINT = INTEGER(ret_swrc);
  PROTECT(cnames = allocVector(STRSXP, N_SWRCs));
  for (i = 0; i < N_SWRCs; i++) {
    pvINT[i] = i;
    SET_STRING_ELT(cnames, i, mkChar(swrc2str[i]));
  }
  namesgets(ret_swrc, cnames);

  // create vector of PDF types
  #ifdef RSWDEBUG
  if (debug) swprintf(" create ret_pdf ...");
  #endif
  PROTECT(ret_pdf = allocVector(INTSXP, N_PDFs));
  pvINT = INTEGER(ret_pdf);
  PROTECT(cnames = allocVector(STRSXP, N_PDFs));
  for (i = 0; i < N_PDFs; i++) {
    pvINT[i] = i;
    SET_STRING_ELT(cnames, i, mkChar(pdf2str[i]));
  }
  namesgets(ret_pdf, cnames);


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
  SET_VECTOR_ELT(ret, 7, ret_swrc);
  SET_VECTOR_ELT(ret, 8, ret_pdf);

  // clean up
  UNPROTECT(nret * 2 + 2);
  #ifdef RSWDEBUG
  if (debug) swprintf(" ... done.\n");
  #endif

  return ret;
}



/**
  @brief Estimate parameters of selected soil water retention curve (SWRC)
    using selected pedotransfer function (PDF)

  See SOILWAT2's `SWRC_PDF_estimate_parameters()`, `swrc2str[]` and `pdf2str[]`.

  @param[in] pdf_type Identification number of selected PDF
  @param[in] sand Sand content of the matric soil (< 2 mm fraction) [g/g]
  @param[in] clay Clay content of the matric soil (< 2 mm fraction) [g/g]
  @param[in] fcoarse Coarse fragments (> 2 mm; e.g., gravel)
    of the whole soil [m3/m3]

  @return Matrix of estimated SWRC parameters
*/
SEXP rSW2_SWRC_PDF_estimate_parameters(
  SEXP pdf_type,
  SEXP sand,
  SEXP clay,
  SEXP fcoarse
) {
  int nlyrs = length(sand);

  /* Check inputs */
  if (
    nlyrs != length(clay) ||
    nlyrs != length(fcoarse) ||
    nlyrs != length(pdf_type)
  ) {
    error("inputs are not of the same length.");
  }

  /* Convert inputs to correct type */
  pdf_type = PROTECT(coerceVector(pdf_type, INTSXP));
  sand = PROTECT(coerceVector(sand, REALSXP));
  clay = PROTECT(coerceVector(clay, REALSXP));
  fcoarse = PROTECT(coerceVector(fcoarse, REALSXP));

  /* Allocate memory for SWRC parameters */
  SEXP
    swrcpk = PROTECT(allocVector(REALSXP, SWRC_PARAM_NMAX)),
    res_swrcp = PROTECT(allocMatrix(REALSXP, nlyrs, SWRC_PARAM_NMAX));

  /* Create convenience pointers */
  unsigned int
    *xpdf_type = (unsigned int *) INTEGER(pdf_type);

  double
    *xsand = REAL(sand),
    *xclay = REAL(clay),
    *xcoarse = REAL(fcoarse),
    *xres = REAL(res_swrcp);

  /* Loop over soil layers */
  /* Ideally, SOILWAT2's `SWRC_PDF_estimate_parameters()`
     would loop over soil layers internally,
     but SOILWAT2 uses a list of soil layer structures instead of an array
  */
  int k1, k2;

  for (k1 = 0; k1 < nlyrs; k1++) {
    SWRC_PDF_estimate_parameters(
      xpdf_type[k1],
      REAL(swrcpk),
      xsand[k1],
      xclay[k1],
      xcoarse[k1]
    );

    for (k2 = 0; k2 < SWRC_PARAM_NMAX; k2++) {
      xres[k1 + nlyrs * k2] = REAL(swrcpk)[k2];
    }
  }

  UNPROTECT(6);

  return res_swrcp;
}


/**
  @brief Check whether selected PDF and SWRC are compatible

  @param[in] swrc_name Name selected SWRC
  @param[in] pdf_name Name selected PDF

  @return A logical value indicating if SWRC and PDF are compatible.
*/
SEXP rSW2_check_SWRC_vs_PDF(SEXP swrc_name, SEXP pdf_name) {
	SEXP res;
	PROTECT(res = NEW_LOGICAL(1));
	LOGICAL(res)[0] = swFALSE;

	PROTECT(swrc_name = AS_CHARACTER(swrc_name));
	PROTECT(pdf_name = AS_CHARACTER(pdf_name));

	if (
		!isNull(swrc_name) &&
		!isNull(pdf_name) &&
		strlen(CHAR(STRING_ELT(swrc_name, 0))) < 64 &&
		strlen(CHAR(STRING_ELT(pdf_name, 0))) < 64
	) {
		char
			sw_swrc_name[64],
			sw_pdf_name[64];

		strcpy(sw_swrc_name, CHAR(STRING_ELT(swrc_name, 0)));
		strcpy(sw_pdf_name, CHAR(STRING_ELT(pdf_name, 0)));

		LOGICAL(res)[0] = check_SWRC_vs_PDF(sw_swrc_name, sw_pdf_name, swFALSE);
	}

	UNPROTECT(3);
	return res;
}


/**
  @brief Check Soil Water Retention Curve (SWRC) parameters

  See SOILWAT2 function `SWRC_check_parameters()`.

  @param[in] swrc_type Identification number of selected SWRC
  @param[in] *swrcp SWRC parameters;
    matrix (one row per set of parameters) or vector (treated as one set)

  @return A logical vector indicating if parameters passed the checks.
*/
SEXP rSW2_SWRC_check_parameters(SEXP swrc_type, SEXP swrcp) {
  /* Convert inputs to correct type */
  swrcp = PROTECT(coerceVector(swrcp, REALSXP));
  swrc_type = PROTECT(coerceVector(swrc_type, INTSXP));


  /* Check SWRC parameters */
  int
    nrp, ncp,
    nlyrs = length(swrc_type);

  if (isMatrix(swrcp)) {
    nrp = nrows(swrcp);
    ncp = ncols(swrcp);
  } else if (isVector(swrcp)) {
    nrp = 1;
    ncp = length(swrcp);
  } else {
    nrp = 0;
    ncp = 0;
  }

  if (nlyrs != nrp) {
    UNPROTECT(2); /* unprotect: swrcp, swrc_type */
    error("`nrows(swrcp)` disagrees with length of `swrc_type`.");
  }

  if (ncp != SWRC_PARAM_NMAX) {
    UNPROTECT(2); /* unprotect: swrcp, swrc_type */
    error("`ncols(swrcp)` disagrees with required number of SWRC parameters.");
  }


  /* Allocate memory for result */
  SEXP res = PROTECT(allocVector(LGLSXP, nlyrs));


  /* Create convenience pointers */
  unsigned int *xswrc_type = (unsigned int *) INTEGER(swrc_type);
  int *xres = LOGICAL(res); /* LGLSXP are internally coded as int */
  double *xswrcp = REAL(swrcp);


  /* Loop over soil layers */
  /* Ideally, SOILWAT2's `SWRC_check_parameters()`
     would loop over soil layers internally,
     but SOILWAT2 uses a list of soil layer structures instead of an array
  */
  int k1, k2;
  double swrcpk[SWRC_PARAM_NMAX];

  for (k1 = 0; k1 < nlyrs; k1++) {
    for (k2 = 0; k2 < SWRC_PARAM_NMAX; k2++) {
      swrcpk[k2] = xswrcp[k1 + nlyrs * k2];
    }

    xres[k1] = SWRC_check_parameters(xswrc_type[k1], swrcpk);
  }

  UNPROTECT(3);

  return res;
}



/**
  @brief Convert between soil water content and soil water potential using
      specified soil water retention curve (SWRC)

  See SOILWAT2 function `SWRC_SWCtoSWP()` and `SWRC_SWPtoSWC()`.

  @param[in] x
    Soil water content in the layer [cm] or soil water potential [-bar]\
  @param[in] direction Direction of conversion, 1: SWP->SWC; 2: SWC->SWP
  @param[in] swrc_type Identification number of selected SWRC
  @param[in] *swrcp Vector or matrix of SWRC parameters
  @param[in] fcoarse Coarse fragments (> 2 mm; e.g., gravel)
    of the whole soil [m3/m3]
  @param[in] width Soil layer width [cm]

  @return Vector of soil water potential [-bar] or soil water content [cm]
**/
SEXP rSW2_SWRC(
  SEXP x,
  SEXP direction,
  SEXP swrc_type,
  SEXP swrcp,
  SEXP fcoarse,
  SEXP width
) {
  int xdirection = asInteger(direction);

  if (xdirection != 1 && xdirection != 2) {
    error("`direction` must be either SWP->SWC(1) or SWC->SWP(2).");
  }

  /* Check dimensions */
  int nlyrs = length(width);

  if (nlyrs != length(fcoarse)) {
    error("`width` and `fcoarse` are not of the same length.");
  }

  if (nlyrs != length(x)) {
    error("`length(x)` is not equal to the number of soil layers.");
  }

  if (nlyrs != length(swrc_type)) {
    error("`swrc_type` is not equal to the number of soil layers.");
  }


  /* Convert inputs to correct type */
  x = PROTECT(coerceVector(x, REALSXP));
  fcoarse = PROTECT(coerceVector(fcoarse, REALSXP));
  width = PROTECT(coerceVector(width, REALSXP));
  swrcp = PROTECT(coerceVector(swrcp, REALSXP));
  swrc_type = PROTECT(coerceVector(swrc_type, INTSXP));


  /* Check SWRC parameters */
  int nrp, ncp;

  if (isMatrix(swrcp)) {
    nrp = nrows(swrcp);
    ncp = ncols(swrcp);
  } else if (isVector(swrcp)) {
    nrp = 1;
    ncp = length(swrcp);
  } else {
    nrp = 0;
    ncp = 0;
  }

  if (nlyrs != nrp) {
    UNPROTECT(5); /* unprotect: swrcp, width, fcoarse, x, swrc_type */
    error("`nrows(swrcp)` disagrees with number of soil layers.");
  }

  if (ncp != SWRC_PARAM_NMAX) {
    UNPROTECT(5); /* unprotect: swrcp, width, fcoarse, x, swrc_type */
    error("`ncols(swrcp)` disagrees with required number of SWRC parameters.");
  }


  /* Allocate memory for result */
  SEXP res = PROTECT(allocVector(REALSXP, nlyrs));


  /* Create convenience pointers */
  unsigned int
    *xswrc_type = (unsigned int *) INTEGER(swrc_type);

  double
    *xres = REAL(res),
    *xx = REAL(x),
    *xswrcp = REAL(swrcp),
    *xcoarse = REAL(fcoarse),
    *xwidth = REAL(width);


  /* Loop over soil layers */
  /* Ideally, SOILWAT2's `SWRC_SWPtoSWC()` and `SWRC_SWCtoSWP()`
     would loop over soil layers internally,
     but SOILWAT2 uses a list of soil layer structures instead of an array
  */
  int k1, k2;
  double swrcpk[SWRC_PARAM_NMAX];

  for (k1 = 0; k1 < nlyrs; k1++) {
    for (k2 = 0; k2 < SWRC_PARAM_NMAX; k2++) {
      swrcpk[k2] = xswrcp[k1 + nlyrs * k2];
    }

    if (R_FINITE(xx[k1]) && R_FINITE(xcoarse[k1]) && R_FINITE(xwidth[k1])) {
      switch (xdirection) {
        case 1:
          /* SWP->SWC: [-bar] to [cm] */
          xres[k1] = SWRC_SWPtoSWC(
            xx[k1],
            xswrc_type[k1],
            swrcpk,
            xcoarse[k1],
            xwidth[k1],
            LOGWARN
          );
          break;

        case 2:
          /* SWC->SWP: [cm] to [-bar] */
          xres[k1] = SWRC_SWCtoSWP(
            xx[k1],
            xswrc_type[k1],
            swrcpk,
            xcoarse[k1],
            xwidth[k1],
            LOGWARN
          );
          break;
      }

      // Translate SOILWAT2 missing to R missing value
      if (EQ(xres[k1], SW_MISSING)) {
        xres[k1] = NA_REAL;
      }

    } else {
      // Input values are not finite
      xres[k1] = NA_REAL;
    }
  }

  UNPROTECT(6);

  return res;
}
