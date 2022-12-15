# rSOILWAT2: An Ecohydrological Ecosystem-Scale Water Balance Simulation Model

<!-- badges: start -->
[ ![R build status][1]][2] [ ![github release][5]][6] [![DOI][12]][13] [![license][7]][8] [![codecov status][9]][10]
<!-- badges: end -->


[1]: https://github.com/DrylandEcology/rSOILWAT2/actions/workflows/check-standard.yml/badge.svg?branch=main
[2]: https://github.com/DrylandEcology/rSOILWAT2/actions
[5]: https://img.shields.io/github/release/DrylandEcology/rSOILWAT2.svg?label=current+release
[6]: https://github.com/DrylandEcology/rSOILWAT2/releases
[7]: https://img.shields.io/github/license/DrylandEcology/rSOILWAT2.svg
[8]: https://www.gnu.org/licenses/gpl.html
[9]: https://codecov.io/gh/DrylandEcology/rSOILWAT2/branch/main/graph/badge.svg
[10]: https://codecov.io/gh/DrylandEcology/rSOILWAT2
[11]: https://img.shields.io/github/downloads/DrylandEcology/rSOILWAT2/total.svg
[12]: https://zenodo.org/badge/15520848.svg
[13]: https://doi.org/10.5281/zenodo.5495139
[SOILWAT2]: https://github.com/DrylandEcology/SOILWAT2
[STEPWAT2]: https://github.com/DrylandEcology/STEPWAT2
[rSFSTEP2]: https://github.com/DrylandEcology/rSFSTEP2
[rSW2utils]: https://github.com/DrylandEcology/rSW2utils
[rSW2st]: https://github.com/DrylandEcology/rSW2st
[rSW2data]: https://github.com/DrylandEcology/rSW2data
[rSW2exter]: https://github.com/DrylandEcology/rSW2exter
[rSW2funs]: https://github.com/DrylandEcology/rSW2funs
[rSFSTEP2]: https://github.com/DrylandEcology/rSFSTEP2
[rSOILWAT2]: https://github.com/DrylandEcology/rSOILWAT2
[rSFSW2]: https://github.com/DrylandEcology/rSFSW2
[issues]: https://github.com/DrylandEcology/rSOILWAT2/issues
[pull request]: https://github.com/DrylandEcology/rSOILWAT2/pulls
[guidelines]: https://github.com/DrylandEcology/DrylandEcologyProtocols
[semantic versioning]: https://semver.org/
[testthat]: https://github.com/r-lib/testthat
[roxygen2]: https://cran.r-project.org/package=roxygen2
[r-pkgs]: https://r-pkgs.org/


<br>


## Table of contents

1. [How to get started](#get_started)
    1. [Installation](#install)
    2. [Documentation](#get_documentation)
2. [Overview of rSW2](#rSW2)
3. [How to contribute](#contribute)
    1. [Code guidelines](#follow_guidelines)
    2. [Code documentation](#code_documentation)
    3. [Code linting](#code_linting)
    4. [Code tests](#code_tests)
    5. [Code versioning](#code_versioning)
4. [Additional notes](#more_notes)

<br>

<a name="get_started"></a>
## How to get started

<a name="install"></a>
## Installation



#### Install rSOILWAT2


Please not that `rSOILWAT2` compiles C source code of [SOILWAT2][]
-- see section on binary version below for alternatives.
Your computer must be set up adequately (see below for requirements).


  * On the command line
    ```{bash}
    git clone -b main --single-branch --recursive \
        https://github.com/DrylandEcology/rSOILWAT2.git rSOILWAT2
    R CMD INSTALL rSOILWAT2
    ```

  * Using R
    ```
    system2(
      command = "git",
      args = paste(
        "clone -b main --single-branch --recursive",
        "https://github.com/DrylandEcology/rSOILWAT2.git",
        "rSOILWAT2"
      )
    )
    tools::Rcmd(args = "INSTALL rSOILWAT2")
    ```

  * Using the `remotes` R package
    ```{r}
    remotes::install_github("DrylandEcology/rSOILWAT2", build_vignettes = TRUE)
    ```

    Please note that "remotes" will download the latest version of [SOILWAT2][]
    irrespective of what the actual submodule status requests
    (see [remotes issue #260](https://github.com/r-lib/remotes/issues/260)).
    If it happens that the latest [SOILWAT2][] version doesn't yet work
    correctly with the latest version of `rSOILWAT2`
    (as has happened in [issue #175](https://github.com/DrylandEcology/rSOILWAT2/issues/175)),
    then please use one of the other options above to
    correctly install `rSOILWAT2` or explore one of the development branches.


### Binary package
If you require a binary version of the 'rSOILWAT2' package
(e.g., to distribute to someone without development tools)
for a platform to which you do not have access,
then you may consider using one of the cloud services (no endorsements), e.g.,
- [rhub](https://builder.r-hub.io) offers different
  `Linux`, `Windows`, and `macOS` flavors as targets



### Minimal requirements
  - on any platform:
    - `gcc` or `clang`/`llvm` toolchains;
      ideally, `gcc >= v4.9` or `clang >= v3.3`
    - POSIX- [`make`](https://pubs.opengroup.org/onlinepubs/9699919799/) or
      GNU-compliant [`make`](https://www.gnu.org/software/make/)
    - `git` to download the code
  - additionally, on Windows OS:
    - [`Rtools`](https://cloud.r-project.org/bin/windows/Rtools/)
      that match your R version
  - on `macOS`:
    - `xcode` command line tools
      (run `xcode-select --install` on the command line)
    - having agreed to the `xcode` license (run `xcodebuild -license`)
    - or, alternatively, the full [`xcode`](https://developer.apple.com/xcode/)
      installation
  - optional:
    - a minimal `latex` installation (see below) and
      `pandoc` (`RStudio` comes bundled with `pandoc`)
      to generate package vignettes


#### Example instructions for a minimal `latex` installation
  * install the R package [`tinytex`](https://yihui.org/tinytex/)
    ```{r}
    install.packages("tinytex")
    tinytex::install_tinytex()
    ```

  * if you don't have write permission for `/usr/local/bin`,
    then appropriate symlinks are not generated;
    thus, locate the path to `tlmgr`, e.g.,
    with help of `tinytex::tinytex_root()`, and
    fix symlinks with escalated privileges
    ```{bash}
    sudo [path/to/]tlmgr path add
    ```

<br>


<a name="get_documentation"></a>
### Documentation

`rSOILWAT2` offers documentation and code examples of exported functions
    `help(package = "rSOILWAT2")`, in particular `?sw_exec`,
and several vignettes
    `vignette(package = "rSOILWAT2")`, in particular
    `vignette("rSOILWAT2_demo", package = "rSOILWAT2")`.




<br>

<a name="rSW2"></a>
## Overview of rSW2

The `DrylandEcology` team hosts a group of related repositories and R packages.

They are organized around two simulation models, i.e.,
[SOILWAT2][], an dryland ecosystem water balance simulation model,
and [STEPWAT2][], an individual-based model for dryland plant communities.
They are both written in compiled languages.


We developed a family of R packages to support
[SOILWAT2][] and [STEPWAT2][] simulation experiments:
* [rSW2utils][] provides miscellaneous utility tools
* [rSW2st][] provides spatiotemporal tools
  including functions to create and interact with `netCDF` files
* [rSW2data][] provides input data preparation
* [rSW2exter][] provides access to external data
* [rSOILWAT2][] is a R package that directly connects to [SOILWAT2][]
  in memory, i.e., without writing/reading input and output files to/from disk
* [rSW2funs][] calculates new response variables from [rSOILWAT2][] output
* [rSFSW2][] manages large [rSOILWAT2][] simulation experiment
* [rSFSTEP2][] manages large [STEPWAT2][] simulation experiment


<br>

<a name="contribute"></a>
## How to contribute
You can help us in different ways:

1. Reporting [issues][]
2. Contributing code and sending a [pull request][]

Please note that this project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this
project you agree to abide by its terms.

<br>


<a name="follow_guidelines"></a>
### Follow our guidelines as detailed [here][guidelines]

<br>


### Tests, documentation, and code

We develop code on development branches and,
after they are reviewed and pass our checks,
merge them into the main branch for release:
  * Create a development branch
      * Set the package version number (`DESCRIPTION`) to a
        development version (ending in `-9000`)
      * Start a new section in `NEWS`
  * Develop, document and test code and create a pull request
  * Finalize code development
  * Once code is reviewed, sufficiently tested, and ready for merging into main
      * Decide on a package version number for the new release
      * Set the package version number in `DESCRIPTION`
      * Finalize `NEWS`
  * Merge pull request into main and
    create new `rSOILWAT2` [release](#code_versioning)



<a name="code_documentation"></a>
#### Code documentation
  * This is based on the section
    ['Documentation' of the book 'R packages' by Wickham][r-pkgs]
  * Use [roxygen2][] to write inline code documentation of functions
  * Use regular R-style comments to additionally document code
  * Update help pages and the `NAMESPACE` file with the command
    `devtools::document()`
  * Add examples to function documentation and check that these examples work
    with the command `devtools::run_examples()`

<br>


<a name="code_linting"></a>
#### Code linting
  * Please run `lintr::lint_package()` to confirm that code conforms to
    our style guide (see file `.lintr`) and update code style where needed
    before pushing a commit or finalizing a pull-request.
  * These checks are also run automatically as a github action
    to confirm that a pull-request meets our requirements for merging.

<br>


<a name="code_tests"></a>
#### Code tests and package checks
  * This is based on the section
    ['Testing' of the book 'R packages' by Wickham][r-pkgs]

  * Unit tests
    * Use [testthat][] to add unit tests to the existing framework
    * Run unit tests with the command `devtools::test()`

  * Package checks
    * Package checks are run with
      `devtools::check(cran = TRUE, env_vars = c(NOT_CRAN = "true"))` or
      `R CMD build . && NOT_CRAN = "true" R CMD check *.tar.gz`
    * Package checks include unit tests, code style, and spelling
    * These checks will be run on the continuous integration frameworks
      via a workflow in `Github Action` for pull requests
    * Development/feature branches can only be merged into main if they pass
      all checks

    * Integration tests: run a default example and examine the
      output (see also `?sw_exec`), e.g.,
      ```{r}
      path_demo <- system.file("extdata", "example1", package = "rSOILWAT2")
      x <- rSOILWAT2::sw_exec(dir = path_demo)
      ```

  * __Run the following steps locally__
    in order to prepare a pull-request or commit that will be reviewed.
    Fix any problem and repeat as necessary.

    1. Make sure that the documentation is up-to-date with:
       ```{r}
       pkgbuild::compile_dll()
       devtools::document()
       ```

    1. Run and check the code from the examples and vignettes:
       ```{r}
       devtools::run_examples()
       ```

    1. Run tests as if not on CRAN in an interactive R session.
       ```{r}
       # Run in R.app, RStudio, or in an R terminal-session:
       Sys.setenv(NOT_CRAN = "true")
       devtools::test()
       ```
       Notes:
        - Make sure that no test is skipped. Investigate if any is skipped.
        - Investigate if any warning is reported.
        - This combines unit tests, documentation and code-style checks;
          the latter take a substantial amount of time to complete.
       The environmental variable `RSOILWAT_ALLTESTS` determines whether or not
       long-running expectations/unit-tests are skipped; the default is "true",
       i.e., run all expectations/unit-tests. You may decide to run tests
       while temporary skipping time-intensive tests, e.g.,
       - `Sys.setenv(RSOILWAT_ALLTESTS = "false"); devtools::test()`
       - `RSOILWAT_ALLTESTS="false" R CMD check *tar.gz`

    1. Run tests as if not on CRAN in an non-interactive session.
       ```{bash}
       # Run via shell in the terminal:
       R CMD INSTALL .
       Rscript -e 'Sys.setenv(NOT_CRAN = "true"); devtools::test()'
       ```

    1. The environmental variable `RSOILWAT_INTEGRATIONTESTS` determines
       whether intensive (and potentially interactive) integration tests are
       executed; the default is "false".
       To set it to true, e.g.,
         * `Sys.setenv(RSOILWAT_INTEGRATIONTESTS = "true"); devtools::test()`
         * `RSOILWAT_INTEGRATIONTESTS="true" R CMD check *tar.gz`

    1. Run R package-level checks as if on CRAN.
       ```{r}
       # Run in R.app, RStudio, or in an R terminal-session:
       Sys.setenv(NOT_CRAN = "false")
       devtools::check(cran = TRUE)
       ```
       Notes:
        - Avoid adding new `R CMD check` warnings and/or notes; see, milestone
          [Clean code](https://github.com/DrylandEcology/rSOILWAT2/milestone/2)

    1. There are additional tests available in `tools/`
       (which may have additional dependencies), e.g.,
         * test concurrent reading and writing to a weather database with
           `test_dbW_concurrency.R`


__Debugging compiled code__
  * Compile C code in `src/` and `src/SOILWAT2/` in 'debugging' mode.
    This will define `SWDEBUG` in `SOILWAT2` source code
    (see the README of [SOILWAT2][]):
    * Install package on the command line:
      ```
      MAKEFLAGS="PKG_DEBUG=-DRSWDEBUG" R CMD INSTALL --preclean --clean .
      ```
    * Using R package `devtools` (e.g., while running R interactively):
      ```{r}
      Sys.setenv(PKG_DEBUG="-DRSWDEBUG")
      devtools::clean_dll() # if you debug in `src/SOILWAT2`
      devtools::load_all(compile = TRUE)
      ```
  * For a more formal approach using `gdb`/`lldb`, please see the
    [section 'Debugging-compiled-code' in the 'R-exts' manual](https://cran.r-project.org/doc/manuals/R-exts.html#Debugging-compiled-code)


__How to update the submodule 'SOILWAT2'__ to the latest commit
```
git submodule update --remote #--remote: uses the latest commit; without --remote: uses the previously defined commit
git commit -am "Pulled down latest commit 'COMMIT-FLAG' for submodule SOILWAT2"
git push
```

Change the branch of submodule `SOILWAT2`
```
open .gitmodules # and change branch = <branch> to the desired <branch>

git submodule init
git submodule update --remote #--remote: uses the latest commit; without --remote: uses the previously defined commit
git commit -am "Changed to branch 'branch' commit 'COMMIT-FLAG' for submodule SOILWAT2"
git push
```

Run the script `./data-raw/prepare_testInput_objects.R` from within `rSOILWAT2/`
if the `SOILWAT2` updated included changes to the input files.

<br>

<a name="code_versioning"></a>
#### Version numbers

We base our versions on the guidelines of [semantic versioning][]
with version numbers of `MAJOR.MINOR.PATCH`.

We create a new release for each update to the main branch;
a new release is identified by the package version (`DESCRIPTION`) and
by a [github release](https://github.com/DrylandEcology/rSOILWAT2/releases)
that also creates a git tag of the same name.
The main branch is updated via pull requests from development branches
after they are reviewed and pass required checks.

If the version numbers changes, then the following files must be updated
* `DESCRIPTION`: adjust lines 'Version'
* `NEWS`: add a new section describing pertinent changes to a package user
  (see section ['NEWS' of the book 'R packages' by Wickham][r-pkgs] and
  [`tidyverse` news style](https://style.tidyverse.org/news.html?q=news#news))


<br>

<a name="more_notes"></a>
## Notes

### Citation
Please cite the package if you publish results based on simulations carried
out with our package, see `citation("rSOILWAT2")`, and we would like to hear
about your publication.

Some other references

* Schlaepfer, D. R., W. K. Lauenroth, and J. B. Bradford. 2012. Ecohydrological
  niche of sagebrush ecosystems. Ecohydrology 5:453-466.
* Bradford, J. B., D. R. Schlaepfer, and W. K. Lauenroth. 2014. Ecohydrology of
  adjacent sagebrush and lodgepole pine ecosystems: The consequences of climate
  change and disturbance. Ecosystems 17:590-605.
* Palmquist, K.A., Schlaepfer, D.R., Bradford, J.B., and Lauenroth, W.K. 2016.
  Mid-latitude shrub steppe plant communities: climate change consequences for
  soil water resources. Ecology 97:2342–2354.


References for the original version of `Soilwat`

* Parton, W.J. (1978). Abiotic section of ELM. In: Grassland simulation model
  (ed. Innis, G.S.). Springer New York, NY, pp. 31-53.
* Sala, O.E., Lauenroth, W.K. & Parton, W.J. (1992). Long-term soil-water
  dynamics in the shortgrass steppe. Ecology, 73, 1175-1181.


### Considerations and Caveats

We haven't really published the code yet nor prepared it for sharing (though
through our use of `github` made it openly accessible), it is actively and
gradually being developed by the Lauenroth lab and affiliates, and there is
no manual either - we cannot give you individual support in setting up and
running the model except if we agreed on a collaboration or similar agreement.

Not every part of the code has been extensively tested or is in a stable state.
Similarly, not every combination of model inputs and options has been evaluated
in depth and there is not guarantee for anything to work. The code comes with
no warranty and no guarantees, expressed or implied, as to suitability,
completeness, accuracy, and whatever other claim you would like to make.

There is no graphical user interface, help pages and available documentation
may be out of date, and you will need to write your own tools to analyze
outputs.



### Funding
Work on this package has been supported by various funds managed by
Dr. John Bradford (USGS), Dr. Bill Lauenroth (Yale University),
Dr. Kyle Palmquist (Marshall University), and Dr. Daniel Schlaepfer.


<br>

### License
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, [version 3 of the License](LICENSE.md).

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.


<br>
