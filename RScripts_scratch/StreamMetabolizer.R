install.packages('remotes')
library(remotes)

Sys.setenv(GITHUB_PAT="stream")

remotes::install_github('appling/unitted')

remotes::install_github(
  "USGS-R/streamMetabolizer", # soon to be "DOI-USGS/streamMetabolizer"
  build_vignettes = TRUE)

