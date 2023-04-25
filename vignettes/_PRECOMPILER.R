# rstudioapi::jobRunScript("~/r-packages/cstidy/vignettes/_PRECOMPILER.R", workingDir="/home/raw996/r-packages/cstidy/vignettes")
devtools::load_all("~/r-packages/cstidy")
setwd("~/r-packages/cstidy/vignettes")

knitr::knit("benchmarks.Rmd.orig", "benchmarks.Rmd")
setwd("~/r-packages/cstidy")

# pkgdown::build_site("~/norsyss")
