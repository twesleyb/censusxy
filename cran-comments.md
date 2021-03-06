## Release summary
Brian Ripley brought an issue to our attention on 8/8/19 about temporary files not being properly removed from the temp directory. This release addresses this concern by fixing a typo in line 15 of `cxy_geocoder.R` to clarify where in the temporary directory `addresses.csv` is saved to. Additionally, `addresses.csv` is now explicitly removed using `unlink()` on line 57 of `cxy_geocoder.R` to ensure that it is removed.

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

There was one NOTE on winbuilder checks (see below). The four words identified are all spelled correctly, and the concersn identified with the temporary directory have been addressed.

* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Christopher Prener <chris.prener@slu.edu>'

New submission

Possibly mis-spelled words in DESCRIPTION:
  Geocoder (2:38)
  geocoded (16:41)
  geocoding (10:72, 12:53, 12:78, 14:73)

CRAN repository db overrides:
  X-CRAN-Comment: Removed on 2019-08-08 for policy violation.

  Leaves file in /tmp

## Reverse dependencies
Not applicable.
