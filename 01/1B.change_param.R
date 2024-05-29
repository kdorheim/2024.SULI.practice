# Description:
# By:
# Date:
# 0. Set Up --------------------------------------------------------------------
# Call packages here & set directories if needed.
library(hector)
library(ggplot2)


# 1.Default Hector Run ---------------------------------------------------------
# Run Hector with the ssp245 using the default parameter set
# TODO this section is incomplete, you are going to need to add some code and
# it should be similar to the previous Hector exercise
ini_file <- system.file("input/hector_ssp245.ini", package = "hector")
core <- newcore(ini_file)

# Get the default parameter for climate sensitivity (aka ECS), the same
# function that is used to get the results is used to get the parameter values
# but no year time
fetchvars(core = core, dates = NA, vars = ECS())


# Run hector & save a copy of the temperature & co2 results, this will be used
# in comparison plots.


# 2. Set new ECSvalue ----------------------------------------------------------
# This code comes from the "Setting parameters" section of  https://jgcri.github.io/hector/articles/hector.html
setvar(core = core, dates = NA, var = ECS(), values = 3.5, unit = getunits(ECS()))
reset(core = core)
run(core = core)

# Fetch the new hector results



# 3. Comparison Plots ----------------------------------------------------------




# 4. Practice playing around with setting Hector parameters --------------------
# I want you to play around with the parameters/dynamics you explored in
# https://github.com/kdorheim/2024.SULI.practice/issues/1 & plot the results
#
# Some Hints
# - View(fxntable) may be helpful
# - consider writing a function to avoid writing the same lines of code over
#   and over again, something like run_with_param function of https://jgcri.github.io/hector/articles/hector.html#sensitivity-analysis
#   might be a good starting point








