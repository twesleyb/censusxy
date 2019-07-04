## Release summary
This is the initial version of the `censusxy` package.

## Test environments
* local OS X install: R 3.6.0
* Linux xenial distribution (on Travis CI): R-release, R-oldrel, R-devel, R-3.4.4, and R-3.3.3
* macOS (on Travis CI): R-release, R-oldrel, R-3.4.4, and R-3.3.3
* windows x64 (on Appveyor): R-release, R-patched, R-oldrel, R-devel, R-3.4.4, and R-3.3.3
* windows i386 (on Appveyor): R-patched
* winbuilder, R-release, R-oldrel, R-devel

* r-hub not used because it lacks dependencies needed to build `sf` on Debian

## R CMD check results
There were no ERRORs, WARNINGs, or NOTEs with local or CI checks.

There was one NOTE on winbuilder checks:

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Christopher Prener <chris.prener@slu.edu>'

New submission

Possibly mis-spelled words in DESCRIPTION:
  Geocoder (2:38)
  geocoded (15:73)
  geocoding (10:72, 12:58, 14:31)
  vectorized (12:26)

The four words identified are all spelled correctly.

## Reverse dependencies
Not applicable.