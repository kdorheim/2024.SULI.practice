# Title: Remaking Manuscript Table 3
# Author: Peter Scully
# Date: 6/5/24

### Imports and Constants ###

library(hector)
library(dplyr)

# Storing output file path
OUTPUT <- file.path(here::here(), "01", "table_making", "table_replica.txt")

# Storing ocean heat content constants
OCEAN_AREA <- 5100656e8 * (1 - 0.29) # The total area of the ocean
W_TO_ZJ <- 3.155693e-14              # Watts to ZJ

# Storing variables for ERF calculations
AEROSOL_RF_VARS <- c(RF_BC(), RF_OC(), RF_NH3(), RF_SO2(), RF_ACI())
NONGHG_RF_VARS <- c(AEROSOL_RF_VARS, RF_VOL(), RF_ALBEDO(), RF_MISC())


#################
### FUNCTIONS ###
#################


# run_hector - function to run Hector using a given ini file
#
# args:
#   ini_file - path to the ini file to use to run Hector
#   yrs      - year range to get Hector data from
#   vars     - Hector variables to get data on
#
# returns: data frame containing the variables' values for the given date range
run_hector <- function(ini_file, yrs, vars) {
  core <- newcore(ini_file)
  run(core)
  data <- fetchvars(core, yrs, vars = vars)
  shutdown(core)
  return(data)
}


# get_var_change - function to find the change in a variable between a start
#                  and end date
#
# args:
#   data  - data frame outputted by fetchvars
#   var   - string containing the Hector variable to find the change in
#   start - start year
#   end   - end year
#
# returns: single value indicating the change in variable from start year to
#          end year
get_var_change <- function(data, var, start, end) {
  initial_val <- filter(data, variable == var & year == start)$value
  final_val   <- filter(data, variable == var & year == end)$value
  return(final_val - initial_val)
}


# get_interval_avg - function to find the average value of a Hector variable
#                    over a given time interval
#
# args:
#   data  - data frame outputted by run_hector (or fetchvars)
#   var   - string containing the Hector variable to average
#   start - start year for finding average
#   end   - end year for finding average
#
# returns: average value of var across the provided interval
get_interval_avg <- function(data, var, start, end) {
  data %>%
    subset(variable == var) %>%
    subset(start <= year & year <= end) %>%
    .$value -> interval_vals
  return(mean(interval_vals))
}


# rel_to_val - function to adjust values of a Hector variable to be relative
#              to a new value
#
# args:
#   data      - data frame outputted by fetchvars
#   var       - string containing the Hector variable to adjust
#   benchmark - value to make variable relative to
#
# returns: data frame containing the new values for var relative to benchmark
rel_to_val <- function(data, var, benchmark) {
  # Updating all of the values for var that are in data
  data %>%
    mutate(value = ifelse(variable == var, value - benchmark, value)) -> data

  return(data)
}

# rel_to_interval - function to normalize values of a Hector variable to a
#                   reference period
#
# args:
#   data  - data frame outputted by fetchvars
#   var   - string containing the Hector variable to adjust
#   start - start year for reference period
#   end   - end year for reference period
#
# returns: data frame containing the normalized values for that variable
rel_to_interval <- function(data, var, start, end) {
  benchmark <- get_interval_avg(data, var, start, end)
  return(rel_to_val(data, var, benchmark))
}


# sum_vars - function to sum the values of several Hector variables at each
#            timestep. adds new rows to the provided data frame containing the
#            summed values at each time step
#
# args:
#   data - data frame outputted by fetchvars
#   vars - vector of Hector variables to sum together
#   name - name to call the new variable containing the sums of the values in
#           vars
#   unit - units to assign to the new variable
#   yrs  - vector of years to sum these variables for
#
# returns: original data frame with additional rows containing the sums of the
#          values for the inputted variables
#
# Note: Currently, all columns other than year, variable, and value will be set
#       equal to the same values as the last row in the original data frame
sum_vars <- function(data, vars, name, unit, yrs) {

  # Setting up iterator to iterate through new rows of data frame
  curr_row <- nrow(data) + 1
  for (yr in yrs) {
    # Adding new row to data frame
    data[curr_row,] <- data[curr_row - 1,]
    data[curr_row,]$year <- yr
    data[curr_row,]$variable <- name
    data[curr_row,]$units <- unit

    # Summing the values across all variables
    data[curr_row,]$value <-
      sum(filter(data, year == yr & variable %in% vars)$value)
    curr_row <- curr_row + 1
  }
  return(data)
}


# write_metric - writes the value for a metric to the output file
#
# args:
#   name - name of metric
#   val  - value of metric
#
# returns: nothing
#
# Note: The value of the metric is rounded to 3 sig figs when outputted
write_metric <- function(name, val) {
  write(paste(name, signif(val, 3)), file = OUTPUT, append = TRUE)
}


###########################
### CALCULATING METRICS ###
###########################

#-------------#
# KEY METRICS #
#-------------#

#----------------------------#
# HISTORICAL WARMING AND ERF #
#----------------------------#

### Initial Hector Run ###

# Setting up variables
hist_yrs <- 1750:2019
hist_vars <- c(GLOBAL_TAS(),               # For GSAT warming
               HEAT_FLUX(),                # For ocean heat content change
               RF_CH4(),                   # For Methane ERF
               NONGHG_RF_VARS, RF_TOTAL()  # For other ERF calcs
               )
hist_file <- system.file("input/hector_ssp245.ini", package = "hector")

# Running Hector
hist_data <- run_hector(ini_file = hist_file, yrs = hist_yrs, vars = hist_vars)


### Finding GSAT Warming ###

# Normalizing temperatures
hist_data <- rel_to_interval(data  = hist_data,
                             var   = GLOBAL_TAS(),
                             start = 1850,
                             end   = 1900)

# Getting 1995-2014 avg
GSAT_warming <- get_interval_avg(data  = hist_data,
                                 var   = GLOBAL_TAS(),
                                 start = 1995,
                                 end   = 2014)


### Finding ocean heat content change ###

# Summing ocean heat (by taking average and multiplying by number of yrs)
avg_flux <- get_interval_avg(data  = hist_data,
                             var   = HEAT_FLUX(),
                             start = 1971,
                             end   = 2018)
total_flux <- avg_flux * length(1971:2018)

# Converting flux to heat content change
ocean_heat_content_change <- total_flux * OCEAN_AREA * W_TO_ZJ


### Finding total aerosol ERF ###

# Getting total aerosol forcing for relevant years
hist_data <- sum_vars(data = hist_data,
                      vars = AEROSOL_RF_VARS,
                      name = "tot_aer_ERF",
                      unit = "w/m^2",
                      yrs  = 2005:2015)

# Getting average forcing from 2005-2015
tot_aer_erf <- get_interval_avg(data  = hist_data,
                                var   = "tot_aer_ERF",
                                start = 2005,
                                end   = 2015)

### Finding WMGHG ERF ###

# Getting total non-GHG ERF
hist_data <- sum_vars(data = hist_data,
                      vars = NONGHG_RF_VARS,
                      name = "non_ghg_ERF",
                      unit = "W/m^2",
                      yrs  = 2019)

# Subtracting non-GHG ERF from total RF
wmghg_erf <- filter(hist_data, variable == RF_TOTAL() & year == 2019)$value -
               filter(hist_data, variable == "non_ghg_ERF" & year == 2019)$value


### Finding methane ERF ###
methane_erf <- filter(hist_data, variable == RF_CH4() & year == 2019)$value


#----------------#
# FUTURE WARMING #
#----------------#

# Setting up variables
future_yrs <- 1995:2100
future_vars <- GLOBAL_TAS()

# Getting the names of each scenario file
scenarios <- c("119", "126", "245", "370", "585")
scenario_names <- paste("input/hector_ssp", scenarios, ".ini", sep = "")
scenario_files <- system.file(scenario_names, package = "hector")

# Setting up results data frame
future_results <- data.frame(matrix(nrow = 0, ncol = 4))
colnames(future_results) <- c("scenario", "start", "end", "GSAT")

for (scen_counter in 1:length(scenario_files)) {
  # Getting data
  scen_data <- run_hector(ini_file = scenario_files[scen_counter],
                          yrs = future_yrs,
                          vars = future_vars)

  # Normalizing temperatures
  scen_data <- rel_to_interval(data = scen_data,
                               var = GLOBAL_TAS(),
                               start = 1995,
                               end = 2014)

  # Finding the averages for each time interval
  start_yrs <- c(2021, 2041, 2081)
  end_yrs <- start_yrs + 19

  # Getting the necessary information for each time interval
  for (i in 1:length(start_yrs)) {

    # Finding average temperatures for the given time interval
    temps <- get_interval_avg(data = scen_data,
                              var = GLOBAL_TAS(),
                              start = start_yrs[i],
                              end = end_yrs[i])

    # Adding the scenario, start and end dates, and avg temperature to the
    # data frame
    future_results[nrow(future_results) + 1, ] <-
        list(paste("ssp", scenarios[scen_counter], sep = ""),
             start_yrs[i],
             end_yrs[i],
             signif(temps, 3))
  }
}

##########################
### OUTPUTTING RESULTS ###
##########################

# Right now, I haven't written the code to find these metrics
write("***Key Metrics***", file = OUTPUT)
write("ECS: ", file = OUTPUT, append = TRUE)
write("TCRE: ", file = OUTPUT, append = TRUE)
write("TCR: ", file = OUTPUT, append = TRUE)
write("", file = OUTPUT, append = TRUE)

write("***Historical Warming and ERF***", file = OUTPUT, append = TRUE)
write_metric("GSAT Warming:             ", GSAT_warming)
write_metric("Ocean Heat Content Change:", ocean_heat_content_change)
write_metric("Total Aerosol ERF:        ", tot_aer_erf)
write_metric("WMGHG ERF:                ", wmghg_erf)
write_metric("Methane ERF:              ", methane_erf)
write("", file = OUTPUT, append = TRUE)

write("***Future Warming***", file = OUTPUT, append = TRUE)
write.table(future_results,
            file = OUTPUT,
            append = TRUE,
            quote = FALSE,
            sep = "\t",
            row.names = FALSE)
