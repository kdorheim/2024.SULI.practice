# Description: Script to practice running Hector and plotting in R
# By: Peter Scully (questions from Kalyn Dorheim)
# Date: 5/29-5/30/24
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

ini <- system.file(package = "hector", "input/hector_ssp245.ini")
core <- newcore(ini, name = "ssp245")
run(core)
ssp245_out <- fetchvars(core, dates = yrs, vars = hector_vars)


# 3. Run SSP585 ----------------------------------------------------------------
# Get results for 1850 to 2100 for global mean air temperature and [CO2]

ini <- system.file(package = "hector", "input/hector_ssp585.ini")
core <- newcore(ini, name = "ssp585")
run(core)
ssp585_out <- fetchvars(core, dates = yrs, vars = hector_vars)

# 4. Plots ---------------------------------------------------------------------
#  R skills used here - concatenating data, sets together, subsetting/filtering data,
# ggplot2 data visualization

# A) Three plots showing one variable at a time changing over time for the three
# different scenarios

# Combining all of the dataframes
all_ssp_out <- rbind(ssp119_out, ssp245_out, ssp585_out)

# Setting base plot aesthetics
base <- ggplot(data = all_ssp_out, aes(x = year, y = value, color = scenario))

# Creating a plot for each variable
for (curr_var in hector_vars) {
  
  # Determining units
  if (curr_var == CONCENTRATIONS_CO2()) {
    curr_units <- "(ppmv)"
  } else {
    curr_units <- "(\u00B0C)"
  }
  
  # Making a plot using only values for the variable currently being plotted
  # Also updates the y-axis and plot titles accordingly
  curr_plot <- base + 
    geom_line(data = filter(all_ssp_out, variable == curr_var)) +
    labs(y = paste(curr_var, curr_units), 
         title = paste(curr_var, "from 1850-2100"))
  ggsave(paste(curr_var, "time.png", sep = "_"), curr_plot)
}


# B) A single plot of the results (will want to facet plot)

# Making a plot with 3 panels (one for each variable)
plot_B <- base + 
  geom_line() + 
  facet_wrap(~ variable, scales = "free") +
  ggtitle("Overall Results")
ggsave("combined_plot.png")

# C) Plot comparing the two different temperatures with one another

# Making plot with just temperature variables
# Temperatures are distinguished by their line type
# Scenarios are still distinguished by color
plot_C <- base + 
  geom_line(data = filter(all_ssp_out, 
                          variable == GLOBAL_TAS() | variable == GMST()),
            aes(linetype = variable)) +
  labs(y = "Temperature (\u00B0C)", title = "Temperature Comparison")
ggsave("T_comparison.png")












