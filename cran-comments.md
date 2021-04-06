## Resubmission
From second resubmission comments (from Julia Haider):

- Please add \value to .Rd files regarding exported methods and explain
  the functions results in the documentation. Please write about the
  structure of the output (class) and also what the output means.
  - **Response** I have added `\value` tags to all user-facing functions. 
  I removed one accidentally exported function.

From first resubmission comments:

-  The Description field should not start with the package name,
     'This package' or similar.

- Do you not check the package yourself? I know that you got coinfused when 
  I wrote it is not a library. But I assumed you would see that you should 
  omit the redundancy at all, sorry for causing confusion.
    - **Response** Sorry, indeed I misread the comment. Removed this entirely.

This is a resubmission. From the comments of Uwe Ligges, I have:
    
- Please change http --> https, add trailing slashes, or follow moved
content as appropriate.
  - **Response** Corrected URL to follow move content 
    (status of link in `inst/doc/cuperdec-intro.html` is now 200)
- For your Description field, you write "This library" but this is a
package, not a library.
  - **Response** Updated description to say package. Also one instance 
    in corrected README 
- Is there some reference about the method you can add in the Description
field in the form Authors (year) <doi:.....>?
  - **Response** Unfortunately not at the moment. A paper is under review 
    but this package is being released early due to popular interest.
    A reference will be added in the next release.

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
  
