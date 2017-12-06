
| Unix | Windows | Release | License | Coverage | Downloads |
| :---- | :---- | :---- | :---- | :---- | :---- |
[ ![Travis build status][1]][2] | [![Appveyor build status][3]][4] | [ ![github release][5]][6] | [![license][7]][8] | [![coveralls status][9]][10] | [![github downloads][11]][12] |

[1]: https://travis-ci.org/Burke-Lauenroth-Lab/rSOILWAT2.svg?branch=master
[2]: https://travis-ci.org/Burke-Lauenroth-Lab/rSOILWAT2
[3]: https://ci.appveyor.com/api/projects/status/3bgvcsokr27bo1uh/branch/master?svg=true
[4]: https://ci.appveyor.com/project/dschlaep/rSOILWAT2/branch/master
[5]: https://img.shields.io/github/release/Burke-Lauenroth-Lab/rSOILWAT2.svg?label=current+release
[6]: https://github.com/Burke-Lauenroth-Lab/rSOILWAT2/releases
[7]: https://img.shields.io/github/license/Burke-Lauenroth-Lab/rSOILWAT2.svg
[8]: https://www.gnu.org/licenses/gpl.html
[9]: https://coveralls.io/repos/github/Burke-Lauenroth-Lab/rSOILWAT2/badge.svg
[10]: https://coveralls.io/github/Burke-Lauenroth-Lab/rSOILWAT2
[11]: https://img.shields.io/github/downloads/Burke-Lauenroth-Lab/rSOILWAT2/total.svg
[12]: https://github.com/Burke-Lauenroth-Lab/rSOILWAT2


# rSOILWAT2
A R package for SOILWAT2.

## Considerations

We haven’t really published the code yet nor prepared it for sharing (though through our
use of github made it openly accessible), it is actively and gradually being developed by
the Lauenroth lab and affiliates, and there is no manual either - we cannot give you
individual support in setting up and running the model except if we agreed on a
collaboration or similar agreement.

Not every part of the code has been extensively tested or is in a stable state. Similarly,
not every combination of model inputs and options has been evaluated in depth and there is
not guarantee for anything to work. The code comes with no warranty and no guarantees,
expressed or implied, as to suitability, completeness, accuracy, and whatever other claim
you would like to make.

There is no graphical user interface, help pages and available documentation may be out
of date, and you will need to write your own tools to analyse outputs.

Note, the branch 'master' is deployable and is released with version numbers.

If you make use of this model, please cite appropriate references, and we would like to
hear about your particular study (especially a copy of any published paper).


Some recent references

* Bradford, J. B., D. R. Schlaepfer, and W. K. Lauenroth. 2014. Ecohydrology of adjacent
  sagebrush and lodgepole pine ecosystems: The consequences of climate change and
  disturbance. Ecosystems 17:590-605.
* Palmquist, K.A., Schlaepfer, D.R., Bradford, J.B., and Lauenroth, W.K. 2016.
  Mid-latitude shrub steppe plant communities: climate change consequences for soil water
  resources. Ecology 97:2342–2354.
* Schlaepfer, D. R., W. K. Lauenroth, and J. B. Bradford. 2012. Ecohydrological niche of
  sagebrush ecosystems. Ecohydrology 5:453-466.

Original references

* Parton, W.J. (1978). Abiotic section of ELM. In: Grassland simulation model
  (ed. Innis, G.S.). Springer New York, NY, pp. 31-53.
* Sala, O.E., Lauenroth, W.K. & Parton, W.J. (1992). Long-term soil-water dynamics in the
  shortgrass steppe. Ecology, 73, 1175-1181.



### Obtain the source package

There are several options:

- Download the
  [package zip file](https://github.com/Burke-Lauenroth-Lab/rSOILWAT2/archive/master.zip)
  via your web browser.

- Use git to clone and get submodules in one line
  ```
  git clone -b master --single-branch --recursive https://github.com/Burke-Lauenroth-Lab/rSOILWAT2.git rSOILWAT2
  ```

- Use git to clone and get submodules step by step
  ```
  git clone https://github.com/Burke-Lauenroth-Lab/rSOILWAT2.git rSOILWAT2
  cd rSOILWAT2/
  git checkout master
  git submodule init
  git submodule update ## --remote: uses the latest submodule commit; without --remote: uses a previously defined submodule commit
  cd src/SOILWAT2
  git checkout master
  git pull
  cd ../..
  ```


### Installation

'rSOILWAT2' will compile the c code of SOILWAT2. Your computer must be set up adequately.
- If you use a Windows OS, then you need the
  [Rtools](http://cran.us.r-project.org/bin/windows/Rtools/)
  installed that match your R version; please find further information for instance
  [here](https://www.biostat.wisc.edu/~kbroman/Rintro/Rwinpack.html).
- If you use a macOS, then you need [Xcode](https://developer.apple.com/xcode/) and
  its [command-line tools](https://developer.apple.com/library/content/technotes/tn2339/_index.html)
  installed; please find further information for instance
  [here](https://railsapps.github.io/xcode-command-line-tools.html).


After you downloaded the source package, run
```
R CMD INSTALL rSOILWAT2
```

Or do all at once from within R:
```{r}
system2(command = "git", args = "clone -b master --single-branch --recursive https://github.com/Burke-Lauenroth-Lab/rSOILWAT2.git rSOILWAT2")
tools::Rcmd(args = paste("INSTALL rSOILWAT2"))
```

### Binary package version
If you want a binary version of the 'rSOILWAT2' package (e.g., to distribute to someone
without development tools) for a platform to which you do not have access, then you may
consider using one of the cloud services (no endorsements):
- https://builder.r-hub.io offers different Linux, Windows, and mac OS flavors as targets
- http://win-builder.r-project.org/ offers Windows OS as target

Alternatively, you may access the previous binary package version for Windows OS from our
CI appveyor service if the build was successful and an artifact was generated for the
binary package (this would be named 'rSOILWAT2_X.Y.Z.zip' with version number X.Y.Z) at
- https://ci.appveyor.com/project/dschlaep/rSOILWAT2/build/artifacts
If the latest build should have failed, then you may want to check out the 'History' tab
for binaries of older versions.



### For code contributors only

__Follow our guidelines__ as detailed [here](https://github.com/Burke-Lauenroth-Lab/workflow_guidelines)

__Tests, documentation, and code__ form a trinity
- Code documentation
  * Read the [section 'Object documentation' in Wickham's book 'R packages'](http://r-pkgs.had.co.nz/man.html)
  * Use [roxygen2](https://cran.r-project.org/web/packages/roxygen2/vignettes/formatting.html)
    to write inline code documentation
  * Update help pages and NAMESPACE with the command `devtools::document()`
  * Ideally, add examples to function documentation and check these examples with the
    command `devtools::run_examples()`
- Code tests
  * Read the [section 'Testing' in Wickham's book 'R packages'](http://r-pkgs.had.co.nz/tests.html)
  * Use [testthat](https://github.com/hadley/testthat) to add unit tests to the existing
    framework
  * Run unit tests locally with the command `devtools::test()`
  * These unit tests will also be run on the command-line with `R CMD check .`
  * The command-line check will be run on the continuous integration frameworks 'travis'
    and 'appveyor' when commits are pushed
  * Development/feature branches can only be merged into master if they pass all checks


__How to update the submodule 'SOILWAT2'__ to the latest commit
```
git clone https://github.com/Burke-Lauenroth-Lab/rSOILWAT2.git rSOILWAT2
cd rSOILWAT2
git checkout master
# if you want to change to the branch 'XXX' of the submodule 'SOILWAT2'
# git config -f .gitmodules submodule.SOILWAT2.branch XXX

git submodule init
git submodule update --remote #--remote: uses the latest commit; without --remote: uses the previously defined commit
cd src/SOILWAT2

git checkout master # or whatever branch you are working on
git pull

cd ../..
git commit -am "Pulled down latest commit 'COMMIT-FLAG' to submodule SOILWAT2"
git push
```

### Version numbers

We attempt to follow guidelines of [semantic versioning](http://semver.org/) with version
numbers of MAJOR.MINOR.PATCH.
If the version numbers changes, then the following files must be updated
* DESCRIPTION: adjust lines 'Version' and 'Date'
* R/zzz.R: adjust package startup message in function '.onAttach'

### How to contribute
You can help us in different ways:

1. Reporting [issues](https://github.com/Burke-Lauenroth-Lab/rSOILWAT2/issues)
2. Contributing code and sending a [pull request](https://github.com/Burke-Lauenroth-Lab/rSOILWAT2/pulls)

In order to contribute to the code base of this project, you should first contact the
Lauenroth Lab. We retain any decision to accept your suggestions/contributions or not.




# Code of conduct
Please note that this project is released with a
[Contributor Code of Conduct](CONDUCT.md). By participating in this project you agree
to abide by its terms.


# License
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, [version 3 of the License](LICENSE).

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.


# Notes

__Repository renamed from Rsoilwat (Rsoilwat31) to rSOILWAT2 on Feb 23, 2017__

All existing information should [automatically be redirected](https://help.github.com/articles/renaming-a-repository/) to the new name.

Contributors are encouraged, however, to update local clones to [point to the new URL](https://help.github.com/articles/changing-a-remote-s-url/), i.e.,
```
git remote set-url origin https://github.com/Burke-Lauenroth-Lab/rSOILWAT2.git
```
If you have installed a previous version as R package, then you may consider removing the old version with
```{r}
remove.packages("Rsoilwat31")
```

