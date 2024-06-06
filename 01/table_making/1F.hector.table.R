# Title: Remaking Manuscript Table 3
# Author: Peter Scully
# Date: 6/5/24

# Import statements
library(hector)
library(dplyr)

# Storing output file path
OUTPUT <- file.path(here::here(), "01", "table_making", "table_replica.txt")

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
# args:
#   data  - data frame outputted by fetchvars
#   var   - string containing the Hector variable to adjust
#   start - start year for reference period
#   end   - end year for reference period
#
# returns - data frame containing the normalized values for that variable
rel_to_interval <- function(data, var, start, end) {
  benchmark <- get_interval_avg(data, var, start, end)
  return(rel_to_val(data, var, benchmark))
}


###########################
### CALCULATING METRICS ###
###########################

### FUTURE WARMING ###

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
             round(temps, 2))
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
write("GSAT Warming: ", file = OUTPUT, append = TRUE)
write("Ocean Heat Content Change: ", file = OUTPUT, append = TRUE)
write("Total Aerosol ERF: ", file = OUTPUT, append = TRUE)
write("WMGHG ERF: ", file = OUTPUT, append = TRUE)
write("Methane ERF: ", file = OUTPUT, append = TRUE)
write("", file = OUTPUT, append = TRUE)

write("***Future Warming***", file = OUTPUT, append = TRUE)
write.table(future_results,
            file = OUTPUT,
            append = TRUE,
            quote = FALSE,
            sep = "\t",
            row.names = FALSE)
