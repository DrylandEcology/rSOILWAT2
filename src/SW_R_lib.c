/*
 * SW_R_lib.c
 *
 *  Created on: Jun 25, 2013
 *      Author: Ryan Murphy
 */

#include "SOILWAT2/include/generic.h"
#include "SOILWAT2/include/filefuncs.h"
#include "SOILWAT2/include/Times.h"
#include "SOILWAT2/include/SW_Defines.h"

#include "SOILWAT2/include/SW_Files.h"
#include "SOILWAT2/include/SW_Carbon.h"
#include "SOILWAT2/include/SW_Model.h"
#include "SOILWAT2/include/SW_Weather.h"
#include "SOILWAT2/include/SW_Sky.h"
#include "SOILWAT2/include/SW_SoilWater.h"
#include "SOILWAT2/include/SW_VegEstab.h"
#include "SOILWAT2/include/SW_VegProd.h"
#include "SOILWAT2/include/SW_Output.h"
#include "SOILWAT2/include/SW_Main_lib.h"
#include "SOILWAT2/include/SW_Site.h"
#include "SOILWAT2/include/SW_Domain.h"


#include "rSW_Files.h"
#include "rSW_Domain.h"
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

#include <stddef.h> // for size_t


/* =================================================== */
/*                  Global Variables                   */
/* --------------------------------------------------- */

// rSOILWAT2 does not utilize SOILWAT2's MPI-based parallelization
const int rSW2_rank = 0;

// rSOILWAT2 is currently set up to have a fixed domain size of 1
SW_DOMAIN SoilWatDomain;

SW_RUN SoilWatRun;

Bool EchoInits;

/* dummyLogFile is a marker to indicate that current_sw_verbosity is not NULL */
static FILE dummyLogFile;

/* rSOILWAT2 writes warnings and error messages to the console; thus
RSOILWAT does not use logfp other than checking
if it's NULL or not NULL (where NULL represents silent mode).
Not NULL is represented by dummyLogFile */
FILE *current_sw_verbosity = &dummyLogFile;



/* =================================================== */
/*                  Local Variables                    */
/* --------------------------------------------------- */
static SEXP sw_has_soiltemp_error = FALSE;


/* =================================================== */
/*             Global Function Definitions             */
/* --------------------------------------------------- */


/**
 Turn on/off `SOILWAT2` messages including errors, notes, and warnings

  `sw_verbosity()` is R interface to sw_verbose().

 @param verbose A logical value.
 @return The previous logical value.
*/
SEXP sw_verbose(SEXP verbose) {
	SEXP prev_verbose;

	PROTECT(prev_verbose = NEW_LOGICAL(1));
	LOGICAL_POINTER(prev_verbose)[0] = !isnull(current_sw_verbosity);

	if (LOGICAL(coerceVector(verbose, LGLSXP))[0]) {
		// verbose: tell `LogError()` that R should print messages to the console
		current_sw_verbosity = &dummyLogFile; // any non-NULL file pointer
	} else {
		// quiet: tell `LogError()` that R should NOT print messages to the console
		current_sw_verbosity = NULL;
	}

	UNPROTECT(1);
	return prev_verbose;
}


/**
 * Queries error status of soil temperature from the most previous simulation run (`sw_start()`)
 *
 * `has_soilTemp_failed()` is R interface to tempError()
 *
 * @param  none
 * @return an R boolean that denotes an error (TRUE) or lack of (FALSE)
 *
 */
SEXP tempError(void) {
	return sw_has_soiltemp_error;
}

static void setGlobal_soiltempError(Bool soiltempError) {
	PROTECT(sw_has_soiltemp_error = NEW_LOGICAL(1));
	LOGICAL_POINTER(sw_has_soiltemp_error)[0] = soiltempError;
	UNPROTECT(1);
}



/** Setup and construct global variables for SOILWAT2

  Global variables managed by rSOILWAT2: SoilWatDomain, SoilWatRun, and PathInfo.
*/
static void setupSOILWAT2(Bool from_files, SEXP InputData, SEXP inputOptions, LOG_INFO* LogInfo) {
    int i, argc;
    char *argv[7];
    size_t userSUID = 0; // keep userSUID fixed at 0
    #ifdef RSWDEBUG
    int debug = 0;
    #endif
    Bool renameDomainTemplateNC = swFALSE;
    Bool prepareFiles = swFALSE;
    Bool endQuietly = swFALSE;
    int rank = 0; // unused


  #ifdef RSWDEBUG
  if (debug) sw_printf("setupSOILWAT2: set args\n");
  #endif


	argc = length(inputOptions);
	if (argc > 7) {
		// fatal condition because argv is hard-coded to be of length 7; increase size of
		// argv if more command-line options are added to SOILWAT2 in the future
		LogError(LogInfo, LOGERROR, "length(inputOptions) must be <= 7.");
        return; // Exit function prematurely due to error
	}
	for (i = 0; i < argc; i++) {
		argv[i] = (char *) CHAR(STRING_ELT(inputOptions, i));
	}

    #ifdef RSWDEBUG
    if (debug) sw_printf("Set call arguments\n");
    #endif

    SW_DOM_init_ptrs(&SoilWatDomain);
    SW_CTL_init_ptrs(&SoilWatRun);

    sw_init_args(
        argc,
        argv,
        rank,
        &EchoInits,
        &SoilWatDomain.SW_PathInputs.txtInFiles[eFirst],
        &userSUID,
        NULL,
        &renameDomainTemplateNC,
        &prepareFiles,
        &endQuietly,
        LogInfo
    );
    if(endQuietly || LogInfo->stopRun) {
        return; // Exit function prematurely due to error
    }

    #ifdef RSWDEBUG
    if (debug) sw_printf("Initialize SOILWAT ...");
    #endif

    rSW_CTL_setup_domain(from_files, InputData, userSUID, &SoilWatDomain, LogInfo);
    if(LogInfo->stopRun) {
      return; // Exit function prematurely due to error
    }

    if (SoilWatDomain.nSUIDs != 1) {
        LogError(
            LogInfo,
            LOGERROR,
            "Size of domain = %lu but this version of rSOILWAT2 cannot "
            "handle a domain of size other than 1.",
            SoilWatDomain.nSUIDs
        );
        return; // Exit function prematurely due to error
    }

    SW_CTL_setup_model(&SoilWatRun, &SoilWatDomain.OutDom, TRUE, LogInfo);
    if(LogInfo->stopRun) {
        return; // Exit function prematurely due to error
    }

    SW_MDL_get_ModelRun(&SoilWatRun.ModelIn, &SoilWatDomain, NULL, LogInfo);
    if(LogInfo->stopRun) {
      return; // Exit function prematurely due to error
    }

    rSW_CTL_setup_model2();
}


/**
  @brief Read inputs from SOILWAT2 input files on disk using SOILWAT2 code

  `sw_inputDataFromFiles()` is R interface to onGetInputDataFromFiles()
*/
SEXP onGetInputDataFromFiles(SEXP inputOptions) {
  SEXP swInputData, SW_DataList = NULL, swLog, oRlogfile;
  SEXP swProdOld1;
  SEXP prodold1;

  int numUnprotects = 0;

  #ifdef RSWDEBUG
  int debug = 0;
  #endif

  LOG_INFO local_LogInfo;
  sw_init_logs(current_sw_verbosity, &local_LogInfo);

  #ifdef RSWDEBUG
  if (debug) sw_printf("Set log\n");
  #endif
  PROTECT(swLog = MAKE_CLASS("swLog"));
  PROTECT(oRlogfile = NEW_OBJECT(swLog));
  numUnprotects += 2;

  // read user inputs: from files
  // setup and construct global variables
  #ifdef RSWDEBUG
  if (debug) sw_printf("Read input from disk files into SOILWAT2 variables (part 1)\n");
  #endif
  setupSOILWAT2(TRUE, NULL, inputOptions, &local_LogInfo);
  if(local_LogInfo.stopRun) {
    goto report;
  }

  // read user inputs: from files
  #ifdef RSWDEBUG
  if (debug) sw_printf("Read input from disk files into SOILWAT2 variables (part 2)\n");
  #endif
  rSW_CTL_obtain_inputs(TRUE, NULL, NULL, &local_LogInfo);
  if(local_LogInfo.stopRun) {
    goto report;
  }

    // finalize daily weather
    #ifdef RSWDEBUG
    if (debug) sw_printf(" finalize daily weather ...\n");
    #endif
    SW_WTH_finalize_all_weather(
        &SoilWatRun.MarkovIn,
        &SoilWatRun.WeatherIn,
        SoilWatRun.RunIn.weathRunAllHist,
        SoilWatRun.ModelSim.cum_monthdays,
        SoilWatRun.ModelSim.days_in_month,
        NULL,
        swFALSE,
        &local_LogInfo
    );
    if(local_LogInfo.stopRun) {
        goto report;
    }

  // initialize simulation run (based on user inputs)
  #ifdef RSWDEBUG
  if (debug) sw_printf(" init simulation run ...\n");
  #endif
  SW_CTL_init_run(&SoilWatRun, &local_LogInfo);
  if(local_LogInfo.stopRun) {
    goto report;
  }

    // identify domain-wide soil profile information
    // value of hasConsistentSoilLayerDepths does not matter
    SW_DOM_soilProfile(
        &SoilWatDomain.netCDFInput,
        &SoilWatDomain.SW_PathInputs,
        SoilWatDomain.hasConsistentSoilLayerDepths,
        &SoilWatDomain.nMaxSoilLayers,
        &SoilWatDomain.nMaxEvapLayers,
        SoilWatDomain.depthsAllSoilLayers,
        SoilWatRun.RunIn.SiteRunIn.n_layers,
        SoilWatRun.SiteSim.n_evap_lyrs,
        SoilWatRun.RunIn.SoilRunIn.depths,
        &local_LogInfo
    );
    if(local_LogInfo.stopRun) {
        goto report;
    }

  #ifdef RSWDEBUG
  if (debug) {
    sw_printf(
      "\n'onGetInputDataFromFiles()': "
      "copy data from SOILWAT2 variables to rSOILWAT2 S4 classes: "
    );
  }
  #endif

  PROTECT(swInputData = MAKE_CLASS("swInputData"));
  PROTECT(SW_DataList = NEW_OBJECT(swInputData));
  numUnprotects += 2;


  SET_SLOT(SW_DataList, install("files"), onGet_SW_F());
  #ifdef RSWDEBUG
  if (debug) sw_printf(" 'files'");
  #endif

  SET_SLOT(SW_DataList, install("spinup"), onGet_SW_SPINUP());
  #ifdef RSWDEBUG
  if (debug) sw_printf(" > 'domain'");
  #endif

  SET_SLOT(SW_DataList, install("years"), onGet_SW_MDL());
  #ifdef RSWDEBUG
  if (debug) sw_printf(" > 'model'");
  #endif

  SET_SLOT(SW_DataList, install("weather"), onGet_SW_WTH_setup());
  #ifdef RSWDEBUG
  if (debug) sw_printf(" > 'weather-setup'");
  #endif

  SET_SLOT(SW_DataList, install("weatherHistory"), onGet_WTH_DATA());
  #ifdef RSWDEBUG
  if (debug) sw_printf(" > 'weather-data'");
  #endif

  SET_SLOT(SW_DataList, install("cloud"), onGet_SW_SKY());
  #ifdef RSWDEBUG
  if (debug) sw_printf(" > 'climate'");
  #endif

  if (
    LOGICAL(
      GET_SLOT(
        GET_SLOT(
          SW_DataList,
          install("weather")
        ),
        install("use_weathergenerator")
      )
    )[0]
  ) {
    SET_SLOT(SW_DataList, install("markov"), onGet_MKV());
    #ifdef RSWDEBUG
    if (debug) sw_printf(" > 'weather generator'");
    #endif
  }

  PROTECT(swProdOld1 = MAKE_CLASS("swProd"));
  PROTECT(prodold1 = NEW_OBJECT(swProdOld1));
  numUnprotects += 2;
  SET_SLOT(SW_DataList, install("prod"), prodold1);

  SET_SLOT(SW_DataList, install("prod2"), onGet_SW_VPD());
  #ifdef RSWDEBUG
  if (debug) sw_printf(" > 'veg'");
  #endif

  SET_SLOT(SW_DataList, install("site"), onGet_SW_SIT());
  #ifdef RSWDEBUG
  if (debug) sw_printf(" > 'site'");
  #endif

  SET_SLOT(SW_DataList, install("soils"), onGet_SW_SOILS());
  #ifdef RSWDEBUG
  if (debug) sw_printf(" > 'soils' + 'swrc parameters'");
  #endif

  SET_SLOT(SW_DataList, install("estab"), onGet_SW_VES());
  #ifdef RSWDEBUG
  if (debug) sw_printf(" > 'establishment'");
  #endif

  SET_SLOT(SW_DataList, install("output"), onGet_SW_OUT());
  #ifdef RSWDEBUG
  if (debug) sw_printf(" > 'ouput'");
  #endif

  SET_SLOT(SW_DataList, install("carbon"), onGet_SW_CARBON());
  #ifdef RSWDEBUG
  if (debug) sw_printf(" > 'CO2'");
  #endif

  SET_SLOT(SW_DataList, install("swc"), onGet_SW_SWC(&local_LogInfo));
  #ifdef RSWDEBUG
  if (debug) sw_printf(" > 'swc'");
  #endif
  if(local_LogInfo.stopRun) {
    goto report;
  }

  SET_SLOT(SW_DataList, install("log"), oRlogfile);

  #ifdef RSWDEBUG
  if (debug) sw_printf(" > de-allocate most memory; \n");
  #endif

  report: {
    UNPROTECT(numUnprotects);

    // de-allocate SOILWAT2 memory, but let R handle `p_OUT`
    SW_DOM_deconstruct(&SoilWatDomain);
    SW_CTL_clear_model(FALSE, &SoilWatRun);

    sw_write_warnings("(rlib) ", &local_LogInfo);
    sw_fail_on_error(&local_LogInfo);
  }
  #ifdef RSWDEBUG
  if (debug) sw_printf(" onGetInputDataFromFiles completed.\n");
  #endif

  return SW_DataList;
}


/**
  @brief Run a SOILWAT2 simulation

  - Copies R inputs to C variables
  - Executes a SOILWAT2 simulation
  - Copies output to R output variable

  `sw_exec()` is R interface to sw_start()
*/
SEXP sw_start(SEXP inputOptions, SEXP inputData, SEXP weatherList) {
	SEXP outputData = NULL, swLog, oRlogfile;
//  SW_WALLTIME local_WallTime;
  LOG_INFO local_LogInfo;

  #ifdef RSWDEBUG
  int debug = 0;
  #endif
    // Start overall wall time
//    local_WallTime.has_walltime = swFALSE; // rSOILWAT2 currently does not do wall-time

    // Initialize logs and pointer objects
    sw_init_logs(current_sw_verbosity, &local_LogInfo);


  #ifdef RSWDEBUG
  if (debug) sw_printf("'start': create log ...");
  #endif
	PROTECT(swLog = MAKE_CLASS("swLog"));
	PROTECT(oRlogfile = NEW_OBJECT(swLog));

  // setup and construct model (via inputData)
  #ifdef RSWDEBUG
  if (debug) sw_printf(" input arguments & setup model ...");
  #endif
  setupSOILWAT2(FALSE, inputData, inputOptions, &local_LogInfo);
  if(local_LogInfo.stopRun) {
    goto report;
  }

	// read user inputs: either from files or from memory (depending on useFiles)

	#ifdef RSWDEBUG
	if (debug) sw_printf(" obtain inputs ...");
	#endif

	rSW_CTL_obtain_inputs(FALSE, inputData, weatherList, &local_LogInfo);
  if(local_LogInfo.stopRun) {
    goto report;
  }

    // finalize daily weather
    #ifdef RSWDEBUG
    if (debug) sw_printf(" finalize daily weather ...\n");
    #endif
    SW_WTH_finalize_all_weather(
        &SoilWatRun.MarkovIn,
        &SoilWatRun.WeatherIn,
        SoilWatRun.RunIn.weathRunAllHist,
        SoilWatRun.ModelSim.cum_monthdays,
        SoilWatRun.ModelSim.days_in_month,
        NULL,
        swFALSE,
        &local_LogInfo
    );
    if(local_LogInfo.stopRun) {
        goto report;
    }

	// initialize simulation run (based on user inputs)
	#ifdef RSWDEBUG
	if (debug) sw_printf(" init simulation run ...");
	#endif
	SW_CTL_init_run(&SoilWatRun, &local_LogInfo);
    if(local_LogInfo.stopRun) {
        goto report;
    }

    // identify domain-wide soil profile information
    // value of hasConsistentSoilLayerDepths does not matter
    SW_DOM_soilProfile(
        &SoilWatDomain.netCDFInput,
        &SoilWatDomain.SW_PathInputs,
        SoilWatDomain.hasConsistentSoilLayerDepths,
        &SoilWatDomain.nMaxSoilLayers,
        &SoilWatDomain.nMaxEvapLayers,
        SoilWatDomain.depthsAllSoilLayers,
        SoilWatRun.RunIn.SiteRunIn.n_layers,
        SoilWatRun.SiteSim.n_evap_lyrs,
        SoilWatRun.RunIn.SoilRunIn.depths,
        &local_LogInfo
    );
    if(local_LogInfo.stopRun) {
        goto report;
    }

    // initialize output
    #ifdef RSWDEBUG
    if (debug) sw_printf(" setup output variables ...");
    #endif

    SW_OUT_setup_output(
        SoilWatDomain.nMaxSoilLayers,
        SoilWatDomain.nMaxEvapLayers,
        SoilWatRun.VegEstabIn.count,
        SoilWatRun.VegEstabIn.parms,
        &SoilWatDomain.OutDom,
        &local_LogInfo
    );
    if(local_LogInfo.stopRun) {
        goto report;
    }

	PROTECT(outputData = onGetOutput(inputData, &local_LogInfo));
  if(local_LogInfo.stopRun) {
      goto report;
  }
	setGlobalrSOILWAT2_OutputVariables(outputData);

  // run simulation: loop through each year
  #ifdef RSWDEBUG
  if (debug) sw_printf(" run SOILWAT2 ...");
  #endif
    // Ideally, we call here SW_CTL_RunSimSet() -- equivalently to SOILWAT2;
    // however, rSOILWAT2's output memory is handled by R instead of SOILWAT2,
    // i.e., we cannot take a local deep copy (and free all after the run).
    // Thus, we mimic here SW_CTL_run_sw() instead
    // and are using rSOILWAT2's global variables
    if (SoilWatDomain.SW_SpinUp.spinup) {
      SW_CTL_run_spinup(&SoilWatRun, &SoilWatDomain.OutDom, &local_LogInfo);
      if (local_LogInfo.stopRun) {
          goto report;
      }
    }

    SW_CTL_main(&SoilWatRun, &SoilWatDomain.OutDom, &local_LogInfo);


  #ifdef RSWDEBUG
  if (debug) sw_printf(" clean up ...");
  #endif

  report: {
    UNPROTECT(2);

    if (local_LogInfo.stopRun) {
        setGlobal_soiltempError(TRUE);
    } else {
        setGlobal_soiltempError(SoilWatRun.SoilWatSim.soiltempError);
    }
    // de-allocate SOILWAT2 memory, but let R handle `p_OUT`
    SW_DOM_deconstruct(&SoilWatDomain);
    SW_CTL_clear_model(FALSE, &SoilWatRun);

    sw_write_warnings("(rlib) ", &local_LogInfo);
    sw_fail_on_error(&local_LogInfo);
  }

  #ifdef RSWDEBUG
  if (debug) sw_printf(" completed.\n");
  #endif

	return(outputData);
}




// `sw_outputData()` is R interface to onGetOutputDeprecated()
SEXP onGetOutputDeprecated(SEXP inputData) {
  SEXP inputOptions, swOutput_Object = NULL;
  int numUnprotects = 0;

  LOG_INFO local_LogInfo;
  sw_init_logs(current_sw_verbosity, &local_LogInfo);
  if(local_LogInfo.stopRun) {
    goto report;
  }

  /* Setup global variables including SoilWatRun */
  PROTECT(inputOptions = allocVector(STRSXP, 1));
  numUnprotects++;
  SET_STRING_ELT(inputOptions, 0, mkChar("SOILWAT2"));

  setupSOILWAT2(FALSE, inputData, inputOptions, &local_LogInfo);
  if(local_LogInfo.stopRun) {
    goto report;
  }

  // Create output object
  swOutput_Object = onGetOutput(inputData, &local_LogInfo);


  report: {
    UNPROTECT(numUnprotects);

    // de-allocate SOILWAT2 memory, but let R handle `p_OUT`
    SW_DOM_deconstruct(&SoilWatDomain);
    SW_CTL_clear_model(FALSE, &SoilWatRun);

    sw_write_warnings("(rlib) ", &local_LogInfo);
    sw_fail_on_error(&local_LogInfo);
  }

  return swOutput_Object;
}




/**
  @brief Process daily driving (weather) variables using SOILWAT2 code

  Applies additive/multiplicative scaling parameters and
  uses imputation/weather generator to fill missing values

  `dbW_generateWeather()` is R interface to rSW2_processAllWeather()
*/
SEXP rSW2_processAllWeather(SEXP weatherList, SEXP inputData) {
  SEXP res = NULL, inputOptions;
  SEXP IntrinsicSiteParams;
  int numUnprotects = 0;
  #ifdef RSWDEBUG
  int debug = 0;
  #endif

  #ifdef RSWDEBUG
  if (debug) sw_printf("\n'rSW2_processAllWeather': data preparation: ");
  #endif

  if (isNull(weatherList)) {
    error("'weatherList' is NULL.");
  }

  LOG_INFO local_LogInfo;
  sw_init_logs(current_sw_verbosity, &local_LogInfo);


  // setup and construct model
  #ifdef RSWDEBUG
  if (debug) sw_printf("'setup' > ");
  #endif

  // values of `inputOptions` are not used
  PROTECT(inputOptions = allocVector(STRSXP, 1));
  numUnprotects++;
  SET_STRING_ELT(inputOptions, 0, mkChar("SOILWAT2"));

  setupSOILWAT2(FALSE, inputData, inputOptions, &local_LogInfo);
  if(local_LogInfo.stopRun) {
    goto report;
  }


  // rSW_CTL_obtain_inputs():
  // `onSet_WTH_DATA()` requires `endyr`, `startyr`, `elevation` from `SW_Model`
  #ifdef RSWDEBUG
  if (debug) sw_printf("'model' > ");
  #endif
  onSet_SW_MDL(GET_SLOT(inputData, install("years")), &local_LogInfo);
  if (local_LogInfo.stopRun) {
    goto report;
  }

  PROTECT(
    IntrinsicSiteParams = GET_SLOT(
      GET_SLOT(inputData, install("site")), install("IntrinsicSiteParams")
    )
  );
  numUnprotects++;
  SoilWatRun.RunIn.ModelRunIn.elevation = REAL(IntrinsicSiteParams)[2];

  // `onSet_WTH_DATA()` requires additive/multiplicative scaling parameters
  #ifdef RSWDEBUG
  if (debug) sw_printf(" > 'weather-setup'");
  #endif
  onSet_SW_WTH_setup(GET_SLOT(inputData, install("weather")), &local_LogInfo);
  if(local_LogInfo.stopRun) {
    goto report;
  }

  // `onSet_WTH_DATA()` requires ready-to-go weather generator
  if (
    LOGICAL(
      GET_SLOT(
        GET_SLOT(
          inputData,
          install("weather")
        ),
        install("use_weathergenerator")
      )
    )[0]
  ) {
    onSet_MKV(GET_SLOT(inputData, install("markov")), &local_LogInfo);
    #ifdef RSWDEBUG
    if (debug) sw_printf(" > 'weather generator'.\n");
    #endif
    if(local_LogInfo.stopRun) {
      goto report;
    }
  }


  // Process weather data
  #ifdef RSWDEBUG
  if (debug) sw_printf("'rSW2_processAllWeather': process weather data");
  #endif
  onSet_WTH_DATA(weatherList, &local_LogInfo);
  if(local_LogInfo.stopRun) {
    goto report;
  }

    // Finalize daily weather (weather generator & monthly scaling)
    #ifdef RSWDEBUG
    if (debug) sw_printf(" > finalize daily weather.\n");
    #endif
    SW_WTH_finalize_all_weather(
        &SoilWatRun.MarkovIn,
        &SoilWatRun.WeatherIn,
        SoilWatRun.RunIn.weathRunAllHist,
        SoilWatRun.ModelSim.cum_monthdays,
        SoilWatRun.ModelSim.days_in_month,
        NULL,
        swFALSE,
        &local_LogInfo
    );
    if(local_LogInfo.stopRun) {
        goto report;
    }

  // Return processed weather data
  PROTECT(res = onGet_WTH_DATA());
  numUnprotects++;


  report: {
    UNPROTECT(numUnprotects);

    // de-allocate SOILWAT2 memory, but let R handle `p_OUT`
    SW_DOM_deconstruct(&SoilWatDomain);
    SW_CTL_clear_model(FALSE, &SoilWatRun);

    sw_write_warnings("(rlib) ", &local_LogInfo);
    sw_fail_on_error(&local_LogInfo);
  }


  return res;
}




/**
  @brief Read daily driving (weather) variables from disk using SOILWAT2 code

  `getWeatherData_folders()` is R interface to rSW2_readAllWeatherFromDisk()
*/
SEXP rSW2_readAllWeatherFromDisk(
  SEXP path,
  SEXP name_prefix,
  SEXP startYear,
  SEXP endYear,
  SEXP elevation,
  SEXP dailyInputFlags,
  SEXP fixWeatherData,
  SEXP sw_template
) {
  SEXP res = NULL, inputOptions;
  int i, numUnprotects = 0;

  #ifdef RSWDEBUG
  int debug = 0;
  #endif

  #ifdef RSWDEBUG
  if (debug) sw_printf("\n'rSW2_readAllWeatherFromDisk': data preparation: ");
  #endif

  LOG_INFO local_LogInfo;
  sw_init_logs(current_sw_verbosity, &local_LogInfo);

  /* Convert inputs to correct type */
  path = PROTECT(AS_CHARACTER(path));
  name_prefix = PROTECT(AS_CHARACTER(name_prefix));
  startYear = PROTECT(coerceVector(startYear, INTSXP));
  endYear = PROTECT(coerceVector(endYear, INTSXP));
  elevation = PROTECT(coerceVector(elevation, REALSXP));
  dailyInputFlags = PROTECT(coerceVector(dailyInputFlags, LGLSXP));
  fixWeatherData = PROTECT(coerceVector(fixWeatherData, LGLSXP));
  numUnprotects += 7;


  /* Create convenience pointers */
  int *xdif = LOGICAL(dailyInputFlags); /* LGLSXP are internally coded as int */
  int *xfix = LOGICAL(fixWeatherData); /* LGLSXP are internally coded as int */


  /* Setup global variables including SoilWatRun */
  PROTECT(inputOptions = allocVector(STRSXP, 1));
  numUnprotects++;
  SET_STRING_ELT(inputOptions, 0, mkChar("SOILWAT2"));

  setupSOILWAT2(FALSE, sw_template, inputOptions, &local_LogInfo);
  if(local_LogInfo.stopRun) {
    goto report;
  }

  /* Copy relevant data to global variable SoilWatRun */
  SoilWatRun.ModelIn.startyr = INTEGER(startYear)[0];
  SoilWatRun.ModelIn.endyr = INTEGER(endYear)[0];

  SoilWatRun.RunIn.ModelRunIn.elevation = REAL(elevation)[0];

  strcpy(SoilWatRun.WeatherIn.name_prefix, CHAR(STRING_ELT(path, 0)));
  strcat(SoilWatRun.WeatherIn.name_prefix, "/");
  strcat(SoilWatRun.WeatherIn.name_prefix, CHAR(STRING_ELT(name_prefix, 0)));

  // read only from files
  SoilWatRun.WeatherIn.use_weathergenerator_only = FALSE; // no weather generator
  SoilWatRun.WeatherIn.generateWeatherMethod = 0;

  SoilWatRun.WeatherIn.use_cloudCoverMonthly = FALSE; // don't interpolate monthly values
  SoilWatRun.WeatherIn.use_windSpeedMonthly = FALSE; // don't interpolate monthly values
  SoilWatRun.WeatherIn.use_humidityMonthly = FALSE; // don't interpolate monthly values
  for (i = 0; i < MAX_MONTHS; i++) {
    SoilWatRun.RunIn.SkyRunIn.cloudcov[i] = SW_MISSING;
    SoilWatRun.RunIn.SkyRunIn.windspeed[i] = SW_MISSING;
    SoilWatRun.RunIn.SkyRunIn.r_humidity[i] = SW_MISSING;
  }

  for (i = 0; i < MAX_INPUT_COLUMNS; i++) {
    SoilWatRun.WeatherIn.dailyInputFlags[i] = xdif[i] ? swTRUE : swFALSE;
  };

  set_dailyInputIndices(
    SoilWatRun.WeatherIn.dailyInputFlags,
    SoilWatRun.WeatherIn.dailyInputIndices,
    &SoilWatRun.WeatherIn.n_input_forcings
  );

  check_and_update_dailyInputFlags(
    SoilWatRun.WeatherIn.use_cloudCoverMonthly,
    SoilWatRun.WeatherIn.use_humidityMonthly,
    SoilWatRun.WeatherIn.use_windSpeedMonthly,
    SoilWatRun.WeatherIn.dailyInputFlags,
    &local_LogInfo
  );
  if(local_LogInfo.stopRun) {
    goto report; // Exit function prematurely due to error
  }

  // no monthly scaling
  for (i = 0; i < MAX_MONTHS; i++) {
    SoilWatRun.WeatherIn.scale_precip[i] = 1;
    SoilWatRun.WeatherIn.scale_temp_max[i] = 0;
    SoilWatRun.WeatherIn.scale_temp_min[i] = 0;
    SoilWatRun.WeatherIn.scale_skyCover[i] = 0;
    SoilWatRun.WeatherIn.scale_wind[i] = 1;
    SoilWatRun.WeatherIn.scale_rH[i] = 0;
    SoilWatRun.WeatherIn.scale_actVapPress[i] = 1;
    SoilWatRun.WeatherIn.scale_shortWaveRad[i] = 1;
  }

  // Requested fixes of weather values
  for (i = 0; i < NFIXWEATHER; i++) {
    SoilWatRun.WeatherIn.fixWeatherData[i] = xfix[i] ? swTRUE : swFALSE;
  }


    // Read weather data
    #ifdef RSWDEBUG
    if (debug) sw_printf("'rSW2_readAllWeatherFromDisk': read weather data");
    #endif
    SW_WTH_read(
        &SoilWatRun.WeatherIn,
        &SoilWatRun.RunIn.weathRunAllHist,
        &SoilWatRun.RunIn.SkyRunIn,
        &SoilWatRun.ModelIn,
        SoilWatRun.RunIn.ModelRunIn.elevation,
        swTRUE,
        SoilWatRun.ModelSim.cum_monthdays,
        SoilWatRun.ModelSim.days_in_month,
        &local_LogInfo
    );
    if(local_LogInfo.stopRun) {
        goto report; // Exit function prematurely due to error
    }

    // Finalize daily weather (weather generator & monthly scaling)
    // we do not "finalize" weather because we do not want generated weather
    // or monthly scaling values applied

    // Return processed weather data
    // using global variable SoilWatRun.RunIn.weathRunAllHist
    res = PROTECT(onGet_WTH_DATA());
    numUnprotects++;


  report: {
    UNPROTECT(numUnprotects);

    // de-allocate SOILWAT2 memory, but let R handle `p_OUT`
    SW_DOM_deconstruct(&SoilWatDomain);
    SW_CTL_clear_model(FALSE, &SoilWatRun);

    sw_write_warnings("(rlib) ", &local_LogInfo);
    sw_fail_on_error(&local_LogInfo);
  }

  return res;
}




/** Expose SOILWAT2 constants and defines to internal R code of rSOILWAT2

  `C_sw_consts()` is R interface to sw_consts()

  @return A list with elements:
    one element `kINT` for integer constants;
    other elements contain vegetation keys, `VegTypes1`, `VegTypes2`,
    `VegTypeNames1`, `VegTypeNames2`;
    output keys, `OutKeys`;
    output periods, `OutPeriods`;
    output aggregation types, `OutAggs`;
    and indices of input files, `InFiles`.
 */
SEXP sw_consts(void) {
  #ifdef RSWDEBUG
  int debug = 0;
  #endif

  const int nret = 12; // length of cret
  const int nINT = 16; // length of vINT and cINT
  const int nNUM = 1; // length of vNUM and cNUM

  #ifdef RSWDEBUG
  if (debug) sw_printf("sw_consts: define variables ... ");
  #endif

  SEXP
    ret,
    cnames,
    ret_num,
    ret_int,
    res_int2v1,
    res_int2v2,
    ret_str1, ret_str2, ret_str3, ret_str4, ret_str5,
    ret_infiles,
    ret_swrc,
    ret_ptf;
  int i;
  int *pvINT;
  double *pvNUM;
  char *cret[] = {
    "kNUM",
    "kINT",
    "VegTypes1", "VegTypes2",
    "VegTypeNames1", "VegTypeNames2",
    "OutKeys", "OutPeriods", "OutAggs",
    "InFiles",
    "SWRC_types",
    "PTF_types"
  };

  // Miscellaneous numerical constants
  double vNUM[] = {SW_MISSING};
  char *cNUM[] = {"SW_MISSING"};

  // Vegetation types (old v1)
  // NOTE: order must match their numeric values, i.e., SOILWAT2 < v8.4.0
  int NVEGTYPESv1 = 4;
  char *cINT2v1[4] = {"SW_TREES", "SW_SHRUB", "SW_FORBS", "SW_GRASS"};
  char *cINT3v1[4] = {"Trees", "Shrubs", "Forbs", "Grasses"};

  // Vegetation types (v2)
  // NOTE: order must match their numeric values, i.e., SOILWAT2 >= v8.4.0
  int vINT2v2[NVEGTYPES] = {
      SW_TREENL, SW_TREEBL, SW_SHRUB, SW_FORBS, SW_GRASS3, SW_GRASS4
  };
  char *cINT2v2[NVEGTYPES] = {
      "SW_TREENL", "SW_TREEBL", "SW_SHRUB", "SW_FORBS", "SW_GRASS3", "SW_GRASS4"
  };

  // Miscellaneous integer constants
  int vINT[] = {
    SW_NFILES, MAX_LAYERS, MAX_TRANSP_REGIONS, 2500,
    SWRC_PARAM_NMAX,
    eSW_NoTime, SW_OUTNPERIODS, SW_OUTNKEYS, SW_NSUMTYPES,
    NVEGTYPESv1, NVEGTYPES,
    OUT_DIGITS,
    N_SWRCs, N_PTFs, MAX_INPUT_COLUMNS, NFIXWEATHER
  };
  char *cINT[] = {
    "SW_NFILES", "MAX_LAYERS", "MAX_TRANSP_REGIONS", "MAX_NYEAR",
    "SWRC_PARAM_NMAX",
    "eSW_NoTime", "SW_OUTNPERIODS", "SW_OUTNKEYS", "SW_NSUMTYPES",
    "NVEGTYPESv1", "NVEGTYPES",
    "OUT_DIGITS",
    "N_SWRCs", "N_PTFs", "MAX_INPUT_COLUMNS", "NFIXWEATHER"
  };

  // Output categories
  // NOTE: `cSTR1` must agree with SW_Output.c/key2str[]
  char *cSTR1[] = {
    "SW_WETHR", "SW_TEMP", "SW_PRECIP", "SW_SOILINF", "SW_RUNOFF",
    "SW_ALLH2O", "SW_VWCBULK", "SW_VWCMATRIC", "SW_SWCBULK", "SW_SWABULK",
    "SW_SWAMATRIC", "SW_SWA", "SW_SWPMATRIC", "SW_SURFACEW", "SW_TRANSP", "SW_EVAPSOIL",
    "SW_EVAPSURFACE", "SW_INTERCEPTION", "SW_LYRDRAIN", "SW_HYDRED", "SW_ET", "SW_AET",
    "SW_PET", "SW_WETDAY", "SW_SNOWPACK", "SW_DEEPSWC", "SW_SOILTEMP", "SW_FROZEN", "SW_ALLVEG",
    "SW_ESTAB", "SW_CO2EFFECTS", "SW_BIOMASS", "SW_DERIVEDSUM", "SW_DERIVEDAVG"
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
    "eNCIn", "eNCInAtt", "eNCOutVars",
    "eDomain",
    "eModel",
    "eLog",
    "eSite", "eLayers", "eSWRCp",
    "eWeather", "eMarkovProb", "eMarkovCov", "eSky",
    "eVegProd", "eVegEstab",
    "eCarbon",
    "eSoilwat",
    "eOutput",
    "eOutputDaily", "eOutputWeekly", "eOutputMonthly", "eOutputYearly",
    "eOutputDaily_soil", "eOutputWeekly_soil", "eOutputMonthly_soil", "eOutputYearly_soil"
  };


  // create vector of numeric/real/double constants
  #ifdef RSWDEBUG
  if (debug) sw_printf(" create ret_num ...");
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
  if (debug) sw_printf(" create ret_int ...");
  #endif
  PROTECT(ret_int = allocVector(INTSXP, nINT));
  pvINT = INTEGER(ret_int);
  PROTECT(cnames = allocVector(STRSXP, nINT));
  for (i = 0; i < nINT; i++) {
    pvINT[i] = vINT[i];
    SET_STRING_ELT(cnames, i, mkChar(cINT[i]));
  }
  namesgets(ret_int, cnames);

  // create vector of vegetation types (old1)
  #ifdef RSWDEBUG
  if (debug) sw_printf(" create res_int2v1 ...");
  #endif
  PROTECT(res_int2v1 = allocVector(INTSXP, NVEGTYPESv1));
  pvINT = INTEGER(res_int2v1);
  PROTECT(cnames = allocVector(STRSXP, NVEGTYPESv1));
  for (i = 0; i < NVEGTYPESv1; i++) {
    pvINT[i] = i;
    SET_STRING_ELT(cnames, i, mkChar(cINT2v1[i]));
  }
  namesgets(res_int2v1, cnames);

  // create vector of vegetation types (v2)
  #ifdef RSWDEBUG
  if (debug) sw_printf(" create res_int2v2 ...");
  #endif
  PROTECT(res_int2v2 = allocVector(INTSXP, NVEGTYPES));
  pvINT = INTEGER(res_int2v2);
  PROTECT(cnames = allocVector(STRSXP, NVEGTYPES));
  for (i = 0; i < NVEGTYPES; i++) {
    pvINT[i] = vINT2v2[i];
    SET_STRING_ELT(cnames, i, mkChar(cINT2v2[i]));
  }
  namesgets(res_int2v2, cnames);

  // create vector of vegetation type names (v1)
  #ifdef RSWDEBUG
  if (debug) sw_printf(" create ret_str4 ...");
  #endif
  PROTECT(ret_str4 = allocVector(STRSXP, NVEGTYPESv1));
  PROTECT(cnames = allocVector(STRSXP, NVEGTYPESv1));
  for (i = 0; i < NVEGTYPESv1; i++) {
    SET_STRING_ELT(ret_str4, i, mkChar(cINT3v1[i]));
    SET_STRING_ELT(cnames, i, mkChar(cINT2v1[i]));
  }
  namesgets(ret_str4, cnames);

  // create vector of vegetation type names (v2)
  #ifdef RSWDEBUG
  if (debug) sw_printf(" create ret_str5 ...");
  #endif
  PROTECT(ret_str5 = allocVector(STRSXP, NVEGTYPES));
  PROTECT(cnames = allocVector(STRSXP, NVEGTYPES));
  for (i = 0; i < NVEGTYPES; i++) {
    SET_STRING_ELT(ret_str5, i, mkChar(key2veg[i]));
    SET_STRING_ELT(cnames, i, mkChar(cINT2v2[i]));
  }
  namesgets(ret_str5, cnames);

  // create vector of output key constants
  #ifdef RSWDEBUG
  if (debug) sw_printf(" create ret_str1 ...");
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
  if (debug) sw_printf(" create ret_str2 ...");
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
  if (debug) sw_printf(" create ret_str3 ...");
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
  if (debug) sw_printf(" create ret_infiles ...");
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
  if (debug) sw_printf(" create ret_swrc ...");
  #endif
  PROTECT(ret_swrc = allocVector(INTSXP, N_SWRCs));
  pvINT = INTEGER(ret_swrc);
  PROTECT(cnames = allocVector(STRSXP, N_SWRCs));
  for (i = 0; i < N_SWRCs; i++) {
    pvINT[i] = i;
    SET_STRING_ELT(cnames, i, mkChar(swrc2str[i]));
  }
  namesgets(ret_swrc, cnames);

  // create vector of PTF types
  #ifdef RSWDEBUG
  if (debug) sw_printf(" create ret_ptf ...");
  #endif
  PROTECT(ret_ptf = allocVector(INTSXP, N_PTFs));
  pvINT = INTEGER(ret_ptf);
  PROTECT(cnames = allocVector(STRSXP, N_PTFs));
  for (i = 0; i < N_PTFs; i++) {
    pvINT[i] = i;
    SET_STRING_ELT(cnames, i, mkChar(ptf2str[i]));
  }
  namesgets(ret_ptf, cnames);


  // combine vectors into a list and return
  #ifdef RSWDEBUG
  if (debug) sw_printf(" create ret ...");
  #endif
  PROTECT(ret = allocVector(VECSXP, nret));
  PROTECT(cnames = allocVector(STRSXP, nret));
  for (i = 0; i < nret; i++) {
    SET_STRING_ELT(cnames, i, mkChar(cret[i]));
  }
  namesgets(ret, cnames);
  SET_VECTOR_ELT(ret, 0, ret_num);
  SET_VECTOR_ELT(ret, 1, ret_int);
  SET_VECTOR_ELT(ret, 2, res_int2v1);
  SET_VECTOR_ELT(ret, 3, res_int2v2);
  SET_VECTOR_ELT(ret, 4, ret_str4);
  SET_VECTOR_ELT(ret, 5, ret_str5);
  SET_VECTOR_ELT(ret, 6, ret_str1);
  SET_VECTOR_ELT(ret, 7, ret_str2);
  SET_VECTOR_ELT(ret, 8, ret_str3);
  SET_VECTOR_ELT(ret, 9, ret_infiles);
  SET_VECTOR_ELT(ret, 10, ret_swrc);
  SET_VECTOR_ELT(ret, 11, ret_ptf);

  // clean up
  UNPROTECT(nret * 2 + 2);
  #ifdef RSWDEBUG
  if (debug) sw_printf(" ... done.\n");
  #endif

  return ret;
}



/**
  @brief Estimate parameters of selected soil water retention curve (SWRC)
    using selected pedotransfer function (PTF)

  See SOILWAT2's `SWRC_PTF_estimate_parameters()`, `swrc2str[]` and `ptf2str[]`.

  `ptf_estimate()` is R interface to rSW2_SWRC_PTF_estimate_parameters().

  @param[in] ptf_type Identification number of selected PTF
  @param[in] sand Sand content of the matric soil (< 2 mm fraction) [g/g]
  @param[in] clay Clay content of the matric soil (< 2 mm fraction) [g/g]
  @param[in] fcoarse Coarse fragments (> 2 mm; e.g., gravel)
    of the whole soil [m3/m3]
  @param[in] bdensity Density of the whole soil
    (matric soil plus coarse fragments) [g/cm3];
    accepts `NULL` if not used by `PTF`

  @return Matrix of estimated SWRC parameters
*/
SEXP rSW2_SWRC_PTF_estimate_parameters(
  SEXP ptf_type,
  SEXP sand,
  SEXP clay,
  SEXP fcoarse,
  SEXP bdensity
) {
  LOG_INFO local_LogInfo;
  sw_init_logs(current_sw_verbosity, &local_LogInfo);

  int nlyrs = length(sand);
  Rboolean has_bd = !isNull(bdensity);

  /* Check inputs */
  if (
    nlyrs != length(clay) ||
    nlyrs != length(fcoarse) ||
    nlyrs != length(ptf_type) ||
    (has_bd && nlyrs != length(bdensity))
  ) {
    error("inputs are not of the same length.");
  }

  /* Convert inputs to correct type */
  ptf_type = PROTECT(coerceVector(ptf_type, INTSXP));
  sand = PROTECT(coerceVector(sand, REALSXP));
  clay = PROTECT(coerceVector(clay, REALSXP));
  fcoarse = PROTECT(coerceVector(fcoarse, REALSXP));
  if (has_bd) {
    bdensity = PROTECT(coerceVector(bdensity, REALSXP));
  } else {
    // Set `bdensity` from `NULL` to array of `SW_MISSING` of appropriate length
    // `SW_MISSING` is the expected value by SOILWAT2
    bdensity = PROTECT(allocVector(REALSXP, nlyrs));
    for (int i = 0; i < nlyrs; i++) {
      REAL(bdensity)[i] = SW_MISSING;
    }
  }

  /* Allocate memory for SWRC parameters */
  SEXP
    swrcpk = PROTECT(allocVector(REALSXP, SWRC_PARAM_NMAX)),
    res_swrcp = PROTECT(allocMatrix(REALSXP, nlyrs, SWRC_PARAM_NMAX));

  /* Create convenience pointers */
  unsigned int
    *xptf_type = (unsigned int *) INTEGER(ptf_type);

  double
    *xsand = REAL(sand),
    *xclay = REAL(clay),
    *xcoarse = REAL(fcoarse),
    *xbd = REAL(bdensity),
    *xres = REAL(res_swrcp);


  /* Loop over soil layers */
  /* Ideally, SOILWAT2's `SWRC_PTF_estimate_parameters()`
     would loop over soil layers internally,
     but SOILWAT2 uses a list of soil layer structures instead of an array
  */
  int k1, k2;

  for (k1 = 0; k1 < nlyrs; k1++) {
    SWRC_PTF_estimate_parameters(
      xptf_type[k1],
      REAL(swrcpk),
      xsand[k1],
      xclay[k1],
      xcoarse[k1],
      xbd[k1],
      &local_LogInfo
    );

    if (local_LogInfo.stopRun) {
        goto report;
    }


    for (k2 = 0; k2 < SWRC_PARAM_NMAX; k2++) {
      xres[k1 + nlyrs * k2] = REAL(swrcpk)[k2];
    }
  }

  report: {
      // Note: no SOILWAT2 memory was allocated
      UNPROTECT(7);

      sw_write_warnings("(rlib) ", &local_LogInfo);
      sw_fail_on_error(&local_LogInfo);
  }

  return res_swrcp;
}


/**
  @brief Check whether PTF and SWRC are compatible and implemented in `SOILWAT2`

  `check_SWRC_vs_PTF()` is R interface to sw_check_SWRC_vs_PTF().

  @param[in] swrc_name Name of SWRC
  @param[in] ptf_name Name of PTF

  @return A logical value indicating if SWRC and PTF are compatible.
*/
SEXP sw_check_SWRC_vs_PTF(SEXP swrc_name, SEXP ptf_name) {
	SEXP res;
	PROTECT(res = NEW_LOGICAL(1));
	LOGICAL(res)[0] = swFALSE;

	PROTECT(swrc_name = AS_CHARACTER(swrc_name));
	PROTECT(ptf_name = AS_CHARACTER(ptf_name));

	if (
		!isNull(swrc_name) &&
		!isNull(ptf_name) &&
		strlen(CHAR(STRING_ELT(swrc_name, 0))) < 64 &&
		strlen(CHAR(STRING_ELT(ptf_name, 0))) < 64
	) {
		char
			sw_swrc_name[64],
			sw_ptf_name[64];

		strcpy(sw_swrc_name, CHAR(STRING_ELT(swrc_name, 0)));
		strcpy(sw_ptf_name, CHAR(STRING_ELT(ptf_name, 0)));

		LOGICAL(res)[0] = check_SWRC_vs_PTF(sw_swrc_name, sw_ptf_name);
	}

	UNPROTECT(3);
	return res;
}


/**
  @brief Check Soil Water Retention Curve (SWRC) parameters

  See SOILWAT2 function `SWRC_check_parameters()`.

  `check_swrcp()` is R interface to rSW2_SWRC_check_parameters().

  @param[in] swrc_type Identification number of selected SWRC
  @param[in] *swrcp SWRC parameters;
    matrix (one row per set of parameters) or vector (treated as one set)

  @return A logical vector indicating if parameters passed the checks.
*/
SEXP rSW2_SWRC_check_parameters(SEXP swrc_type, SEXP swrcp) {
  int numUnprotects = 0;
  LOG_INFO local_LogInfo;
  sw_init_logs(current_sw_verbosity, &local_LogInfo);

  /* Convert inputs to correct type */
  swrcp = PROTECT(coerceVector(swrcp, REALSXP));
  swrc_type = PROTECT(coerceVector(swrc_type, INTSXP));
  numUnprotects += 2;

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
    UNPROTECT(numUnprotects); /* unprotect: swrcp, swrc_type */
    error("`nrows(swrcp)` disagrees with length of `swrc_type`.");
  }

  if (ncp != SWRC_PARAM_NMAX) {
    UNPROTECT(numUnprotects); /* unprotect: swrcp, swrc_type */
    error("`ncols(swrcp)` disagrees with required number of SWRC parameters.");
  }


  /* Allocate memory for result */
  SEXP res = PROTECT(allocVector(LGLSXP, nlyrs));
  numUnprotects++;

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

    xres[k1] = SWRC_check_parameters(xswrc_type[k1], swrcpk, &local_LogInfo);

    if (local_LogInfo.stopRun) {
        goto report;
    }
  }

  report: {
      // Note: no SOILWAT2 memory was allocated
      UNPROTECT(numUnprotects);

      sw_write_warnings("(rlib) ", &local_LogInfo);
      sw_fail_on_error(&local_LogInfo);
  }

  return res;
}



/**
  @brief Convert between soil water content and soil water potential using
      specified soil water retention curve (SWRC)

  See SOILWAT2 function `SWRC_SWCtoSWP()` and `SWRC_SWPtoSWC()`.

  `swrc_conversion()` via `swrc_conversion_1d()` is R interface to rSW2_SWRC().

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
  LOG_INFO local_LogInfo;
  sw_init_logs(current_sw_verbosity, &local_LogInfo);

  int
    xdirection = asInteger(direction),
    numUnprotects = 0;

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
  numUnprotects += 5;

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
    UNPROTECT(numUnprotects); /* unprotect: swrcp, width, fcoarse, x, swrc_type */
    error("`nrows(swrcp)` disagrees with number of soil layers.");
  }

  if (ncp != SWRC_PARAM_NMAX) {
    UNPROTECT(numUnprotects); /* unprotect: swrcp, width, fcoarse, x, swrc_type */
    error("`ncols(swrcp)` disagrees with required number of SWRC parameters.");
  }


  /* Allocate memory for result */
  SEXP res = PROTECT(allocVector(REALSXP, nlyrs));
  numUnprotects++;

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
            LOGWARN,
            &local_LogInfo
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
            LOGWARN,
            &local_LogInfo
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

    if (local_LogInfo.stopRun) {
        goto report; // Exit function prematurely due to error
    }
  }


  report: {
      // Note: no SOILWAT2 memory was allocated
      UNPROTECT(numUnprotects);

      sw_write_warnings("(rlib) ", &local_LogInfo);
      sw_fail_on_error(&local_LogInfo);
  }

  return res;
}
