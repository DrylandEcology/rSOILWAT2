# rSOILWAT2 v6.0.0-9000

## Breaking changes
* `SOILWAT2` updated to v7.0.0
* This version now handles a variety of soil water retention curves `SWRC`
  and pedotransfer functions `PDF`.
* New inputs are required to select a `SWRC` and `PDF` as well as to provide
  parameter values of the selected `SWRC` for each soil layer.
  Default values are backwards compatible, i.e.,
  default `SWRC` is "Campbell1974" and default `PDF` is "Cosby1984AndOthers".
  If these new inputs are missing in an `rSOILWAT2` "swInputData" object,
  then they are automatically set to their default values.
* This version produces nearly identical simulation output
  as the previous release under default values.
  Small deviations arise due to a fix in the handling of soil moisture values
  between field capacity and saturation.

## New features
* New method `sw_upgrade()` upgrades objects with
  outdated `rSOILWAT2` S4 classes to the current version.
* New `get_soilmoisture()` to consistently extract soil moisture content,
  volumetric water content (bulk soil), or
  volumetric water content for the matric component.
  The function calculates the requested type if not stored in the output
  from those that are available.


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
* Surface temperature is provided in slot "TEMP" in columns
  `surfaceTemp_min_C`, `surfaceTemp_avg_C` (previously `surfaceTemp_C`),
  and `surfaceTemp_max_C`.
* Soil temperature at depths of soil layers is provided in slot "SOILTEMP"
  in columns `Lyr_X_min_C`, `Lyr_X_avg_C` (previously `Lyr_X`),
  and `Lyr_X_max_C` where `X` stands for layer number 1, 2, ...
* Package linting updated to `lintr` >= 3 and
  lint workflow switched from package tests to Github Action.


# rSOILWAT2 v5.2.0
## Breaking changes
* This version adds new output to otherwise identical simulation output.
* `SOILWAT2` is updated to `v6.4.0` which provides
  the phase of soil moisture (frozen or not) in each soil layer.
* The new output is provided in slot "FROZEN" of class `swOutput` (#101).


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
      no longer remove soil moisture held below swc_min.
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
* Moved CI from travis and appveyor to Github Actions.
