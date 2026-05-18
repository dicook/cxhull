## Maintainer change

The package was removed from CRAN, and the original author is uncommunicative. 
I'd like to take over as the maintainer of this package, because it is useful 
for various projects. I believe the changes now satisfy the issues that were 
responsible for it's removal. 

## Spelling

"Possibly misspelled words in DESCRIPTION: Qhull, adjacencies" — these
  are correctly spelled. Qhull is the name of the bundled computational
  geometry library, and adjacencies is a standard mathematical term.
  
## gcc-san NOTE

The gcc-san check reports UBSan warnings of the form:
  "index N out of bounds for type 'setelemT [1]'"

These are known false positives arising from Qhull's use of the
"struct hack" (a size-1 array as a flexible array member), a well-known
C idiom predating C99. The memory is correctly allocated and accessed;
UBSan incorrectly flags it because it cannot see the dynamic allocation.
This is a documented limitation of UBSan with this pattern and is not
a real out-of-bounds access.

## Test environments

rhub::rhub_check() 

✔ Check started: linux, m1-san, windows (selfish-norwaylobster).
  See <https://github.com/dicook/cxhull/actions> for output.
  
And also additional linux checks (sheepskin-dairycow).

All green.  

## devtools::check()

── R CMD check results ──────────────────────────────────────────────── cxhull 0.8.1 ────
Duration: 17.5s

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## R CMD check results

Status: OK

## Reverse dependencies

There's no consequence on the reverse dependencies.

