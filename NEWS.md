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
