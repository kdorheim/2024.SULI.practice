# Description: Script to practice modifying Hector's parameters in R
# By: Peter Scully;exercise and initial code by Kalyn Dorheim
# Date: 5/30/24
# 0. Set Up --------------------------------------------------------------------
# Call packages here & set directories if needed.
library(hector)
library(ggplot2)


# 1.Default Hector Run ---------------------------------------------------------
# Run Hector with the ssp245 using the default parameter set
# This section is incomplete, you are going to need to add some code and
# it should be similar to the previous Hector exercise
ini_file <- system.file("input/hector_ssp245.ini", package = "hector")
core <- newcore(ini_file)


# Get the default parameter for climate sensitivity (aka ECS), the same
# function that is used to get the results is used to get the parameter values
# but no year time
initial_ECS <- fetchvars(core = core, dates = NA, vars = ECS())


# Run hector & save a copy of the temperature & co2 results, this will be used
# in comparison plots.

# Storing the variables/dates we care about
hector_vars <- c(GLOBAL_TAS(), GMST(), CONCENTRATIONS_CO2())
yrs <- 1850:2100  # Assuming we want the same date range as previously

# Running Hector and saving the results
run(core)
initial_results <- fetchvars(core, yrs, vars = hector_vars)

# 2. Set new ECSvalue ----------------------------------------------------------
# This code comes from the "Setting parameters" section of  https://jgcri.github.io/hector/articles/hector.html
setvar(core = core, dates = NA, var = ECS(), values = 3.5, unit = getunits(ECS()))
reset(core = core)
run(core = core)

# Fetch the new hector results
new_ecs_results <- fetchvars(core, yrs, vars = hector_vars)


# 3. Comparison Plots ----------------------------------------------------------

# Renaming scenarios in dataframes
initial_results$scenario = paste("ECS =", initial_ECS$value)
new_ecs_results$scenario = "ECS = 3.5"

# Combining all of the dataframes
all_results <- rbind(initial_results, new_ecs_results)

# Making the faceted comparison plot
plot <- ggplot(data = all_results, aes(x = year, y = value, color = scenario)) +
  geom_line() + 
  facet_wrap(~ variable, scales = "free") +
  ggtitle("Comparing Results")
ggsave("ecs_comparison_plot.png")

core <- shutdown(core)




# 4. Practice playing around with setting Hector parameters --------------------
# I want you to play around with the parameters/dynamics you explored in
# https://github.com/kdorheim/2024.SULI.practice/issues/1 & plot the results
#
# Some Hints
# - View(fxntable) may be helpful
# - consider writing a function to avoid writing the same lines of code over
#   and over again, something like run_with_param function of https://jgcri.github.io/hector/articles/hector.html#sensitivity-analysis
#   might be a good starting point

# Creating function
vary_param <- function(param, new_val) {
  core <- newcore(ini_file)
  
  
  # Get the default parameter value
  initial_param <- fetchvars(core = core, dates = NA, vars = param)
  
  # Running Hector and saving the results
  run(core)
  initial_results <- fetchvars(core, yrs, vars = hector_vars)
  
  # Setting new parameter value
  # This code comes from the "Setting parameters" section of  https://jgcri.github.io/hector/articles/hector.html
  setvar(core = core, dates = NA, var = param, values = new_val, unit = getunits(param))
  reset(core = core)
  run(core = core)
  
  # Fetch the new hector results
  new_param_results <- fetchvars(core, yrs, vars = hector_vars)
  
  # Renaming scenarios in dataframes
  initial_results$scenario = paste(param, "=", initial_param$value)
  new_param_results$scenario = paste(param, "=", new_val)
  
  # Combining all of the dataframes
  all_results <- rbind(initial_results, new_param_results)
  
  # Making the faceted comparison plot
  plot <- ggplot(data = all_results, aes(x = year, y = value, color = scenario)) +
    geom_line() + 
    facet_wrap(~ variable, scales = "free") +
    ggtitle("Comparing Results")
  ggsave(paste(param, "_comparison_plot.png", sep = ""))
  core <- shutdown(core)
}

# Making plots for each of the 6 parameters
vary_param(AERO_SCALE(), 0.3)
vary_param(BETA(), 0.72)
vary_param(DIFFUSIVITY(), 4.6)
vary_param(ECS(), 6.0)
vary_param(Q10_RH(), 4.0)
vary_param(VOLCANIC_SCALE(), 0.5)






