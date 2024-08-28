# rSOILWAT2 v6.1.1-9000


# rSOILWAT2 v6.1.0-9000
* This version produces the same output as the previous version.
* `SOILWAT2` updated to v8.0.0 which now includes a simulation domain;
  however, this has no impact on `rSOILWAT2`.

## New features
* SOILWAT2 gained spin-up functionality (@niteflyunicorns, @dschlaep).
  A user-requested sequence of (random) years is simulated (without output)
  before the actual simulation run to provide better starting values
  (e.g., soil moisture, soil temperature).


# rSOILWAT2 v6.0.4
* This version produces the same output as the previous version.

## New features
* New `upgrade_weatherDF()` adds requested weather columns to a data frame
  (@dschlaep).
* Improved rounding of weather functionality (@dschlaep):
    * `dbW_weatherData_round()` now rounds both `"weatherList"` and
      `"weatherDF"` objects; argument `"digits"` can now also be logical
      (if `TRUE`, then digits takes the default value of 4) or not finite
      (e.g., `NA`; not finite values return the input without rounding).
    * Argument `"round"` of `dbW_dataframe_to_weatherData()` is deprecated and
      changed the default value from rounding to 2 digits to no rounding (`NA`);
      recommended replacement is a separate call to `dbW_weatherData_round()`.
    * Argument `"digits"` of `dbW_generateWeather()` changed the default value
      from rounding to 4 digits to no rounding (`NA`).
* `dbW_generateWeather()` gained `"return_weatherDF"` and now returns a
  user requested weather object type (@dschlaep).
  If `return_weatherDF` is `TRUE`, then the result is converted to a
  data frame where columns represent weather variables; otherwise,
  a list of elements of class `swWeatherData` is returned (as previously).
* New `dbW_imputeWeather()` replaces missing weather values using
  using the weather generator and
  using functionality by `rSW2utils::impute_df()` (@dschlaep).
* New `dbW_substituteWeather()` replaces missing weather values in one
  weather data object with values from a second weather data object (@dschlaep).
* New `dbW_fixWeather()` fixes missing weather values using a sequence of
  approaches including linear interpolation for short missing spells,
  a fixed value for short spells of missing precipitation (optionally),
  substitution from a second weather data object, and
  replacement with long term daily mean values (@dschlaep).
* New family of functions `sw_meteo_obtain` that obtain (download) weather
  data from external sources and prepare for use by `"rSOILWAT2"` (@dschlaep):
      * New `sw_meteo_obtain_DayMet()` obtains and formats data from `"Daymet"`
      * New `sw_meteo_obtain_SCAN()` obtains and formats data from `"SCAN"`


# rSOILWAT2 v6.0.3
* This version produces the same output as the previous version.
* Update `SOILWAT2` to v7.2.0 which improves error handling and fixes
  memory leaks on error (#239; @dschlaep, @N1ckP3rsl3y).
* Code no longer requires (unused) `pcg` header (@dschlaep).


# rSOILWAT2 v6.0.2
* This version produces the same output as the previous version.
* Update `SOILWAT2` to v7.1.0 which prepares for thread-safety and reentrancy
  (@N1ckP3rsl3y, @dschlaep).
* C-side of `rSOILWAT2` gains new globals of the four main abstract types in
  `SOILWAT2` - SW_ALL, SW_OUTPUT_POINTERS, LOG_INFO, and PATH_INFO -
  to interact with `SOILWAT2`(@N1ckP3rsl3y, @dschlaep).


# rSOILWAT2 v6.0.1

## Bugfix
* `dbW_dataframe_to_weatherData()` is now compatible with data frames that
  contain a subset of all possible weather variables, e.g.,
  weather data frames from previous versions (#236; @dschlaep).



# rSOILWAT2 v6.0.0

## Breaking changes
* `SOILWAT2` updated to v7.0.0
* This version produces nearly identical simulation output
  as the previous release under default values.
  Small deviations arise due to
    1. a fix in the handling of soil moisture values
       between field capacity and saturation;
    2. a fix in the calculation of potential natural vegetation; and
    3. a fix in the calculation of climate variables (if used).

## New features
* New method `sw_upgrade()` upgrades objects with
  outdated `rSOILWAT2` S4 classes to the current version;
  new `upgrade_weatherHistory()` upgrades outdated weather history objects.
* New `get_soilmoisture()` to consistently extract soil moisture content,
  volumetric water content (bulk soil), or
  volumetric water content for the matric component.
  The function calculates the requested type if not stored in the output
  from those that are available.
* Derived output functions `get_XXX()` gain new argument `keep_time`;
  if `keep_time` is requested (`TRUE`), then year and sub-year time step values
  are added as first one or two columns to the returned matrix.
* New `time_columns()` returns the output column indices with time information.
* New `nrow_output()` returns the number of time steps in output.

* Daily weather inputs, in addition to the previous variables
  maximum air temperature, minimum air temperature, and precipitation amount,
  can now process the following variables (issue #229; @dschlaep, @N1ckP3rsl3y):
    * Cloud cover (can be replaced by shortwave radiation)
    * Wind speed (can be replaced by wind components)
    * Wind speed eastward component (optional)
    * Wind speed northward component (optional)
    * Relative humidity (can be replaced by max/min humidity, specific humidity
      dew point temperature, or vapor pressure)
    * Maximum relative humidity (optional)
    * Minimum relative humidity (optional)
    * Specific humidity (optional)
    * Dew point temperature (optional)
    * Actual vapor pressure (optional)
    * Downward surface shortwave radiation (optional)

* This version now handles a variety of soil water retention curves `SWRC`
  and pedotransfer functions `PTF` (issue #207, @dschlaep).
    * New inputs are required to select a `SWRC` and `PTF` as well as to provide
      parameter values of the selected `SWRC` for each soil layer.
      Default values are backwards compatible, i.e.,
      default `SWRC` is `"Campbell1974"` and
      default `PTF` is `"Cosby1984AndOthers"`.
    * If these new inputs are missing in an `rSOILWAT2` `swInputData` object,
      then they are automatically set to their default values.
    * New functionality for working with `SWRCs` and `PTFs` include
        * `check_SWRC_vs_PTF()`
          checks if `PTF` and `SWRC` are compatible and implemented.
        * `check_ptf_availability()` checks availability of `PTFs`.
        * `list_matched_swrcs_ptfs()` lists matching pairs of
          implemented `SWRCs` and `PTFs`.
        * `ptf_estimate()` estimates `SWRC` parameters from soil texture
          with a pedotransfer function.
        * `ptf_names()` lists pedotransfer functions `PTFs`.
        * `swrc_conversion()`, `swrc_swp_to_vwc()`, and `swrc_vwc_to_swp()`
          convert between bulk soil water content and soil water potential.
        * `swrc_names()` lists soil water retention curves `SWRCs`.
    * Documentation for code developers can be found in comment sections
      `"Notes for implementing a new PTF"` and
      `"Notes for implementing a new SWRC"`.

* Soil density inputs can now represent either matric or bulk density
  (issue #280; @dschlaep).
    * Automatic conversion by `SOILWAT2` between matric and bulk density
      as needed using the new slot `"SoilDensityInputType"`.
* `calc_SiteClimate()` is now implemented via `SOILWAT2`
  (issue #205; @N1ckP3rsl3y, @dschlaep).
  The old implementation in R is still available as non-exported and deprecated
  `calc_SiteClimate_old()`.
    * This version fixes issues from the previous R version:
       * Mean annual temperature is now the mean across years of
         means across days within year of mean daily temperature.
       * Years at locations in the southern hemisphere are now adjusted to start
         on July 1 of the previous calendar year.
       * The cheatgrass-related variables, i.e., `Month7th_PPT_mm`,
         `MeanTemp_ofDriestQuarter_C`, and `MinTemp_of2ndMonth_C`,
         are now adjusted for location by hemisphere.
* `estimate_PotNatVeg_composition()` is now implemented via `SOILWAT2`
  (issues #206, #218, #219; @N1ckP3rsl3y, @dschlaep).
  The old implementation in R is still available as non-exported and deprecated
  `estimate_PotNatVeg_composition_old()`.
    * This version fixes issues from the previous R version:
       * The `C4` grass correction based on Teeri & Stowe 1976 is now applied
         as documented (issue #218).
       * The sum of all grass components, if fixed, is now incorporated into
         the total sum of all fixed components (issue #219).


## Changes to interface
* Class `swSite` gains new slots `"swrc_flags"` and `"has_swrcp"` and associated
  methods `swSite_SWRCflags()` and `swSite_hasSWRCp()`
  for names of selected `SWRC` and `PTF` as well as indicating
  whether `SWRC` parameters are provided as inputs or to be calculated
  at run time (issue #207, @dschlaep).
* Class `swSoils` gains new slot `"SWRCp"` and associated methods
  `swSoils_SWRCp()` for `SWRC` parameters by soil layer (issue #207, @dschlaep).
* Class `swFiles` gains a new file name for the `SWRC` parameter input file and
  associated methods `swFiles_SWRCp()` (issue #207, @dschlaep).
* Class `swSite` gains new slot `"SoilDensityInputType"` and associated
  methods `swSite_SoilDensityInputType()` (issue #209, @dschlaep).
  This encodes whether soil density inputs represent
  matric soil or bulk soil values.
* Class `swProd` gains new slot `"veg_method"` (issue #206, @N1ckP3rsl3y).
  This encodes if land cover is estimated at run-time by `SOILWAT2` via
  `estimatePotNatVegComposition()` (value 1) or if land cover values are passed
  as inputs (value 0, as previously).
* `SWPtoVWC()` and `VWCtoSWP()` are deprecated in favor of
  `swrc_swp_to_vwc()` and `swrc_vwc_to_swp()` respectively.
* Class `swWeather` gains new slots (issue #229)
    * `"use_cloudCoverMonthly"`, `"use_windSpeedMonthly"`, and
      `"use_humidityMonthly"` which determine whether mean monthly values
      (from `swCloud`) or daily values (from `swWeatherData`) are utilized;
    * `"dailyInputFlags"` which indicates which of the 14 possible daily
      weather variables are present in the inputs;
    * `"desc_rsds"` which describes units of input shortwave radiation.
* Class `swWeatherData` gains new columns in slot `"data"` that accommodate
  all 14 possible daily weather variables (issue #229).

# rSOILWAT2 v5.4.1
* This version produces identical simulation output as the previous release.
* `get_transpiration()` and `get_evaporation()` now also work with
  `rSOILWAT2` output objects produced before `v5.0.0` (#230; @dschlaep).


# rSOILWAT2 v5.4.0
* `SOILWAT2` is updated to v6.7.0 which fixed vegetation establishment.
* This version produces identical simulation output
  as the previous release under default values
  (i.e., vegetation establishment is turned off).

* Functionality to calculate and output establishment/recruitment of species
  now works and is covered by tests (issue #225, @dschlaep).
  Note that this functionality assesses yearly the chances of
  species to recruit/establish based on simulated daily conditions;
  however, establishment/recruitment outcomes are not utilized to inform the
  simulation.

## Changes to interface
* Class `swEstabSpecies` gains new slot `"vegType"` to specify vegetation type
  of a species establishment parameters (issue #225, @dschlaep).


# rSOILWAT2 v5.3.3
* This version produces identical simulation output as the previous release.
* `get_soiltemp()` now correctly locates soil temperature output for all
  `rSOILWAT2` output objects
  (even if created with `rSOILWAT2` before `v5.3.0` and
  `soillayers` is specified; issue #221, @dschlaep).
* `r-lib` Github Actions updated to `v2`;
  separate workflows for `R-CMD-check` and `test-coverage`
  (issue #202; @dschlaep).


# rSOILWAT2 v5.3.2
* This version produces identical simulation output as the previous release.
* `SOILWAT2` is updated to `v6.6.0` which updated random number functionality;
  however, none of those updates affect `rSOILWAT2` because `rSOILWAT2` utilizes
  R API random number functionality which has not changed.



# rSOILWAT2 v5.3.1
* This version fixes a bug in soil temperature output that was
  introduced with version `3.5.0` (#194).
* New `get_soiltemp()` extracts soil temperature at surface and/or at
  requested depths of soil layers from simulation output.
  This function works with simulation output from
  rSOILWAT2 versions before and after `v5.3.0`.


# rSOILWAT2 v5.3.0
* This version adds new output to otherwise identical simulation output.
* `SOILWAT2` is updated to `v6.5.0` which provides
  the estimated minimum/maximum soil temperature for every layer and
  at the surface.
* Surface temperature is provided in slot `"TEMP"` in columns
  `surfaceTemp_min_C`, `surfaceTemp_avg_C` (previously `surfaceTemp_C`),
  and `surfaceTemp_max_C`.
* Soil temperature at depths of soil layers is provided in slot `"SOILTEMP"`
  in columns `Lyr_X_min_C`, `Lyr_X_avg_C` (previously `Lyr_X`),
  and `Lyr_X_max_C` where `X` stands for layer number 1, 2, ...
* Package linting updated to `lintr` >= 3 and
  lint workflow switched from package tests to Github Action.


# rSOILWAT2 v5.2.0
## Breaking changes
* This version adds new output to otherwise identical simulation output.
* `SOILWAT2` is updated to `v6.4.0` which provides
  the phase of soil moisture (frozen or not) in each soil layer.
* The new output is provided in slot `"FROZEN"` of class `swOutput` (#101).


# rSOILWAT2 v5.1.3
* This version produces identical simulation output as the previous release.
* `.dbW_setConnection()` is a bare-bones version of `dbW_setConnection()` that
  quickly and without any error checking connects to a weather database.


# rSOILWAT2 v5.1.2
* This version produces identical simulation output as the previous release.
* `dbW_delete_duplicated_weatherData()` gains arguments
  with backwards-compatible defaults:
    * `site_id`: delete duplicates only for one selected site
    * `carefully`: in combination with `site_id`, thoroughly identify and
      delete duplicated entries only if quick counts suggests that there
      might be duplicates, i.e, this safes time if there are no duplicates.


# rSOILWAT2 v5.1.1
* This version produces identical simulation output as the previous release.
* `check_version()` now checks for the possible presence of
  (an arbitrary number of) development version levels and the argument `level`
  can now also take the value "devel".
* `dbW_getWeatherData()` is now vectorized over sites and scenarios and
  gained the argument `stop_if_missing`.
* `dbW_getSiteId()` is now correctly vectorized over `Labels`.
* `dbW_getIDs()` is now correctly vectorized for all arguments
  that identify sites: `site_id`, `site_label`, `long`/`lat`.


# rSOILWAT2 v5.1.0
## Breaking changes
* This version produces slightly different simulation output from previous
  versions.
* `SOILWAT2` is updated to `v6.3.0` with
   improved percolation and behavior at minimum soil water content
   (https://github.com/DrylandEcology/SOILWAT2/releases/tag/v6.3.0) including:
    * Unsaturated percolation rate is now adjusted for `swc_min`,
      i.e., percolation rate is smaller at very low moisture levels.
    * Bare-soil evaporation, transpiration, and hydraulic redistribution
      no longer remove soil moisture held below `swc_min`.
    * Lower limit of `swc_min` is now set at -30 MPa.


# rSOILWAT2 v5.0.4
* This version produces identical simulation output as the previous release.
* Improved version checks (with backwards compatible defaults).


# rSOILWAT2 v5.0.3
* This version produces identical simulation output as the previous release.
* Improved concurrent reading and writing to a weather database.
* `dbW_updateSites()` and `dbW_getSiteId()` are faster.
* New `dbW_have_sites_all_weatherData()` checks if the weather database
  contains weather data objects of requested sites and scenarios.
* New `dbW_delete_duplicated_weatherData()` removes duplicated weather data.
* `dbW_has_weatherData()` now counts unique (instead of all)
  weather data entries as documented.


# rSOILWAT2 v5.0.2
* This version produces identical simulation output as the previous release.
* Changes of the internal code organization affect developers only.
* `SOILWAT2` updated to v6.2.2


# rSOILWAT2 v5.0.1
* Installation via `remotes::install_github("DrylandEcology/rSOILWAT2")`
  works again (#177).


# rSOILWAT2 v5.0.0

## Breaking changes
* Reorganization of `SOILWAT2`-related functionality,
  i.e., creation of a family of `rSW2`-related packages
  (see https://github.com/DrylandEcology/rSOILWAT2#rSW2):
    * `rSW2utils` provides miscellaneous utility tools
    * `rSW2st` provides spatiotemporal tools
      including functions to create and interact with `netCDF` files
    * `rSW2data` provides input data preparation
    * `rSW2exter` provides access to external data
    * `rSOILWAT2` is a R package that directly connects to `SOILWAT2`
      in memory,
      i.e., without writing/reading input and output files to/from disk
    * `rSW2funs` calculates new response variables from `rSOILWAT2` output
    * `rSFSW2` manages large `rSOILWAT2` simulation experiment
    * `rSFSTEP2` manages large `STEPWAT2` simulation experiment
* New dependency on new packages `rSW2utils` and `rSW2data`.
* Some functions marked as defunct (moved to `rSW2` appropriate packages):
    * package `rSW2funs`: `calc_SMTRs()`, `SMR_names()`, `SMRq_names()`,
      `STR_names()`, `calc_RRs_Chambers2014()`, `calc_RRs_Maestas2016()`
    * package `rSW2data`: `getStartYear()`, `setup_time_simulation_run()`,
      `simTiming_ForEachUsedTimeUnit()`, `update_requested_years()`

## New features
* New functions/methods:
  `adjust_TranspirationRegions()`, `check_TranspirationRegions()`,
  `prepare_TranspirationRegions()`, `dbW_check_weatherData()`,
  `estimate_PotNatVeg_roots()`, `lookup_annual_CO2a()`,
  `activate_swOUT_OutKey<-()`, `deactivate_swOUT_OutKey<-()`
* New datasets:
  `sw2_tr_VegBiom`, `sw2_trco_table`, `sw2_tr_CO2a`

## Minor improvements and fixes
* `SOILWAT2` updated to v6.2.1
* Many improvements in documentation and unit tests.
* Closed issues and bug reports, including #58, #164, #170, #171, #176.
* Moved CI from `travis` and `appveyor` to Github Actions.
