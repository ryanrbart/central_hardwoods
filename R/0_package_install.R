# Script containing R packages to be installed on cluster for central hardwood project

# In theory, this script should just include packages used by the current
# repository. Any dependent packages should be automatically downloaded.
# However, it often gets more complicated because some packages are only
# available through github. Further, some R packages on cluster may be imported
# by anaconda and not require direct download here.

# Use this function to identify functions within scripts in a folder
# RHESSysWorkflowinR::identify_packages("R")

# Once multiple projects are set up, this script may need to be consolidated
# into a single location (package install repository or lookup table in
# RHESSysWorkflowinR?) to avoid duplication. The main complication from this is
# that there may be a few differences in required libraries between projects,
# though most libraries will be the same.

# ------------------------------------------------------------------------------

# CRAN libraries
install.packages("dplyr", dependencies = TRUE)
install.packages("lubridate", dependencies = TRUE)
install.packages("purrr", dependencies = TRUE)
install.packages("raster", dependencies = TRUE)
install.packages("readr", dependencies = TRUE)
install.packages("remotes", dependencies = TRUE)
install.packages("rlang", dependencies = TRUE)
install.packages("sf", dependencies = TRUE)
install.packages("terra", dependencies = TRUE)
install.packages("tibble", dependencies = TRUE)
install.packages("zoo", dependencies = TRUE)


# Github packages
remotes::install_github("ucanr-igis/caladaptr", dependencies = TRUE)
remotes::install_github("RHESSys/RHESSysIOinR", ref="develop", dependencies = TRUE)
remotes::install_github("RHESSys/RHESSysPreprocessing", dependencies = TRUE)
remotes::install_github("ryanrbart/RHESSysWorkflowinR", ref="main", dependencies = TRUE, auth_token = "")


# Github packages that are required by RHESSysWorkflowinR but may not be
# downloaded directly as dependencies.
remotes::install_github("mikejohnson51/climateR", dependencies = TRUE)
remotes::install_github("ryanrbart/EcoHydroConversions", dependencies = TRUE)


