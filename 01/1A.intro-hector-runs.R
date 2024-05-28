# Description:
# By:
# Date:
# 0. Set Up --------------------------------------------------------------------
# Call packages here & set directories if needed.
library(hector)
library(ggplot2)


# 1. Run SSP119 ----------------------------------------------------------------
# Get results for 1850 to 2100 for global mean air temp, global mean surface temp and [CO2]
yrs <- 1850:2100
hector_vars <- c(GLOBAL_TAS(), GMST(), CONCENTRATIONS_CO2())

# Define the path to the ini file
ini <- system.file(package = "hector", "input/hector_ssp119.ini")
core <- newcore(ini, name = "ssp119")
run(core)
ssp119_out <- fetchvars(core, dates = yrs, vars = hector_vars)


# 2. Run SSP245 ----------------------------------------------------------------
# Get results for 1850 to 2100 for global mean air temperature and [CO2], see
# section 1 and https://jgcri.github.io/hector/articles/hector.html for examples


# 3. Run SSP585 ----------------------------------------------------------------
# Get results for 1850 to 2100 for global mean air temperature and [CO2]


# 4. Plots ---------------------------------------------------------------------
#  R skills used here - concatenating data, sets together, subsetting/filtering data,
# ggplot2 data visualization

# A) Three plots showing one variable at at time changing over time for the three
# different scenarios

# B) A single plot of the results (will want to facet plot)

# C) Plot comparing the two different temperatures with one another













