# Rsoilwat
A R package for SOILWAT.

We haven’t really published the code yet nor prepared it for sharing (though through our use of github made it openly accessible), it is actively and gradually being developed by the Lauenroth lab and affiliates, and there is no manual either - we cannot give you individual support in setting up and running the model except if we agreed on a collaboration or similar agreement.

Not every part of the code has been extensively tested or is in a stable state. Similarly, not every combination of model inputs and options has been evaluated in depth and there is not guarantee for anything to work. The code comes with no warranty and no guarantees, expressed or implied, as to suitability, completeness, accuracy, and whatever other claim you would like to make.

There is no graphical user interface, help pages and available documentation may be out of date, and you will need to write your own tools to analyse outputs.

Note, the branch 'master' is deployable and is released with version numbers.

If you make use of this model, please cite appropriate references, and we would like to hear about your particular study (especially a copy of any published paper).


Some recent references

* Bradford, J. B., D. R. Schlaepfer, and W. K. Lauenroth. 2014. Ecohydrology of adjacent sagebrush and lodgepole pine ecosystems: The consequences of climate change and disturbance. Ecosystems 17:590-605.
* Schlaepfer, D. R., W. K. Lauenroth, and J. B. Bradford. 2012. Ecohydrological niche of sagebrush ecosystems. Ecohydrology 5:453-466.

Original references

* Parton, W.J. (1978). Abiotic section of ELM. In: Grassland simulation model (ed. Innis, G.S.). Springer New York, NY, pp. 31-53.
* Sala, O.E., Lauenroth, W.K. & Parton, W.J. (1992). Long-term soil-water dynamics in the shortgrass steppe. Ecology, 73, 1175-1181.



### Clone and get submodules
```
git clone -b master --single-branch --recursive https://github.com/Burke-Lauenroth-Lab/Rsoilwat.git Rsoilwat_v31
```

### Clone and get submodules
```
git clone https://github.com/Burke-Lauenroth-Lab/Rsoilwat.git Rsoilwat_v31
cd Rsoilwat_v31/
git checkout master
git submodule init
git submodule update ## --remote: uses the latest submodule commit; without --remote: uses a previously defined submodule commit
cd src
git checkout master
git pull
```

### Install
After you downloaded the source code, use one of the bash scripts
```
./TarAndInstall_31.sh
./TarAndInstall_31n.sh
```
alternatively,
```
tar -pczf Rsoilwat_v31.tar.gz Rsoilwat_v31
R CMD INSTALL Rsoilwat_v31.tar.gz
```

Or do all at once from within R:
```{r}
system2(command = "git", args = "clone -b master --single-branch --recursive https://github.com/Burke-Lauenroth-Lab/Rsoilwat.git Rsoilwat")
tools::Rcmd(args = paste("INSTALL Rsoilwat"))
```


### For contributors only
Update the submodule 'SOILWAT' to the latest commit
```
git clone https://github.com/Burke-Lauenroth-Lab/Rsoilwat.git Rsoilwat_v31
cd Rsoilwat_v31
git checkout master
# if you want to change to the branch 'XXX' of the submodule 'SOILWAT'
# git config -f .gitmodules submodule.SOILWAT.branch XXX

git submodule init
git submodule update --remote #--remote: uses the latest commit; without --remote: uses the previously defined commit
cd src

git checkout master
git pull

cd ..
git commit -am "Pulled down latest commit 'COMMIT-FLAG' to submodule SoilWat"
git push
```

### Version numbers

We attempt to follow guidelines of [semantic versioning](http://semver.org/) with version numbers of MAJOR.MINOR.PATCH.
If the version numbers changes, then the following files must be updated
* DESCRIPTION: adjust lines 'Version' and 'Date'
* R/zzz.R: adjust package startup message in function '.onAttach'

### How to contribute
You can help us in different ways:

1. Reporting [issues](https://github.com/Burke-Lauenroth-Lab/Rsoilwat/issues)
2. Contributing code and sending a [pull request](https://github.com/Burke-Lauenroth-Lab/Rsoilwat/pulls)

In order to contribute to the code base of this project, you must first contact the Lauenroth Lab. We retain any decision to accept your suggestions/contributions or not.
