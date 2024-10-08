library(devtools)
library(usethis)

# usethis::use_build_ignore(c("setup"))
# usethis::use_build_ignore(c("LICENSE"))

#usethis::build_readme()
#usethis::use_data(mirtdata, overwrite = T)
#usethis::use_tutorial("irt", "Item Response Theory", open = interactive())
#usethis::use_vignette("Package_Functions")

library(roxygen2)
roxygenize()
devtools::check()
tools::buildVignettes(dir = ".", tangle=TRUE)

#dir.create("inst")
dir.create("inst/doc")
file.copy(dir("vignettes", full.names=TRUE), "inst/doc", overwrite=TRUE)

library(pkgdown)
# usethis::use_pkgdown()
pkgdown::build_site()
# usethis::use_pkgdown_github_pages()

devtools::install_github("doomlab/ViSe")

##R CMD build .
##R CMD check --as-cran ViSe_0.1.1.tar.gz

library(ViSe)
ViSe::runExample()
