## Test environments
- local R installation, R 4.0.4
- R-hub windows-x86_64-devel (r-devel)
- R-hub ubuntu-gcc-release (r-release)
- R-hub fedora-clang-devel (r-devel)

## R CMD check results
- The following reported mis-spelled words in DESCRIPTION is a false positive
  (see: https://en.wikipedia.org/wiki/Metagenomics)
  
> On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... NOTE
  Maintainer: 'James A. Fellows Yates <jfy133@gmail.com>'
  

  
  Possibly mis-spelled words in DESCRIPTION:
  New submission
    metagenomic (12:57, 15:10)

- The following seems to be stochastic. On a previous run of test-builds, this
  error occured on Fedora and not Windows.

> On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release)
  checking for future file timestamps ... NOTE
  unable to verify current time
  
