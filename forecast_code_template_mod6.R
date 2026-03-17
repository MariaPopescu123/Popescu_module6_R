## install.packages('remotes')
## install.packages('tidyverse') # collection of R packages for data manipulation, analysis, and visualisation
## install.packages('lubridate') # working with dates and times
## remotes::install_github('eco4cast/neon4cast') # package from NEON4cast challenge organisers to assist with forecast building and submission

#I have updated this script to indicate places where I used AI and provide my explanations showing
#my understanding of what I did for this assignment, but
#but also please see comment in Canvas assignment submission regarding how I used AI! 

# ------ Load packages -----
library(tidyverse)
library(lubridate)
#--------------------------#

# Change this for your model ID
# Include the word "example" in my_model_id for a test submission
# Don't include the word "example" in my_model_id for a forecast that you have registered (see neon4cast.org for the registration form)
my_model_id <- 'maria_example_ID'

# --Model description--- #
# This is a lagged autoregressive model using 31 ensemble members and water
#temperature from the day prior to predict water temperature. 

# Add a brief description of your modeling approach
# This is mosty detailed within my uncertainty representation. But I use historical 
# water temperature and NOAA weather to fit a linear model to temperature. 
# That model is then applied to predict water temperature using future NOAA ensemble members, 
# and water temperature based on the initial conditions I create (with IC uncertainty). Water temperature from the forecast
# is then plugged back in to be used in the next time step. 

# -- Uncertainty representation -- #
# Describe what sources of uncertainty are included in your forecast and how you estimate each source.
#1. parameter uncertainty was added from obtaining the output from the model itself and then making a distribution based on the models own uncertainty.
#   the summary of the model provides the coefficient and se for each parameter and then we draw randomly (for however many ensemble members we have)
#   to provide slightly different numbers based on this spread
#2. IC uncertainty: I added temp yday so that I could also include IC uncertainty (had a lot of trouble with this one so I used AI)
#   but I understand the it is essentially random noise produced at the beginning and now by adding temp yday it is now a lagged autoregressive model
#3. driver uncertainty: this is already done by using 31 ensemble members from NOAA forecast 
#4. process uncertainty: this is done by calculating the difference between the model and actual temp observations. 
#   we then obtain the sigma from this and later add it as random noise within the loop from a random draw with 0 as the starting point and sigma as the width. 


#------- Read data --------
# read in the targets data
targets <- read_csv("https://sdsc.osn.xsede.org/bio230014-bucket01/challenges/targets/project_id=neon4cast/duration=P1D/aquatics-targets.csv.gz")

# read in the sites data
aquatic_sites <- read_csv("https://raw.githubusercontent.com/eco4cast/neon4cast-ci/refs/heads/main/neon4cast_field_site_metadata.csv") |>
  dplyr::filter(aquatics == 1)

focal_sites <- aquatic_sites |> 
  filter(field_site_subtype == 'Lake') |> 
  pull(field_site_id)

# Filter the targets
targets <- targets %>%
  filter(site_id %in% focal_sites,
         variable == 'temperature')
#--------------------------#



# ------ Weather data ------
met_variables <- c("air_temperature")

# Past stacked weather -----
weather_past_s3 <- neon4cast::noaa_stage3()

weather_past <- weather_past_s3  |> 
  dplyr::filter(site_id %in% focal_sites,
                datetime >= ymd('2017-01-01'),
                variable %in% met_variables) |> 
  dplyr::collect()

# aggregate the past to mean values
weather_past_daily <- weather_past |> 
  mutate(datetime = as_date(datetime)) |> 
  group_by(datetime, site_id, variable) |> 
  summarize(prediction = mean(prediction, na.rm = TRUE), .groups = "drop") |> 
  # convert air temperature to Celsius if it is included in the weather data
  mutate(prediction = ifelse(variable == "air_temperature", prediction - 273.15, prediction)) |> 
  pivot_wider(names_from = variable, values_from = prediction)

# Future weather forecast --------
# New forecast only available at 5am UTC the next day
forecast_date <- Sys.Date() 
noaa_date <- forecast_date - days(1)

weather_future_s3 <- neon4cast::noaa_stage2(start_date = as.character(noaa_date))

weather_future <- weather_future_s3 |> 
  dplyr::filter(datetime >= forecast_date,
                site_id %in% focal_sites,
                variable %in% met_variables) |> 
  collect()

weather_future_daily <- weather_future |> 
  mutate(datetime = as_date(datetime)) |> 
  # mean daily forecasts at each site per ensemble
  group_by(datetime, site_id, parameter, variable) |> 
  summarize(prediction = mean(prediction, na.rm = TRUE), .groups = "drop") |> 
  # convert air temperature to Celsius if it is included in the weather data
  mutate(prediction = ifelse(variable == "air_temperature", prediction - 273.15, prediction)) |> 
  pivot_wider(names_from = variable, values_from = prediction) |> 
  select(any_of(c('datetime', 'site_id', met_variables, 'parameter')))

#--------------------------#

forecast_horizon <- 30
forecasted_dates <- seq(from = ymd(forecast_date), to = ymd(forecast_date) + forecast_horizon, by = "day")
n_members <- 31

# ----- Fit model & generate forecast----

#initial condition uncertainty (from water temp done)
#parameter uncertainty (done)
#driver uncertainty (from the 31 NOAA ensemble members)
#process uncertainty (done)


# Generate a dataframe to fit the model to 
targets_lm <- targets |> 
  pivot_wider(names_from = 'variable', values_from = 'observation') |> 
  left_join(weather_past_daily, 
            by = c("datetime","site_id"))

# Loop through each site to fit the model
forecast_df <- NULL

for(i in 1:length(focal_sites)) { 
  
  curr_site <- focal_sites[i] #UNCOMMENT WHEN DONE TESTING
  
  site_target <- targets_lm |>
    filter(site_id == curr_site) |>
    mutate(temp_yday = lag(temperature))
  
  noaa_future_site <- weather_future_daily |> 
    filter(site_id == curr_site)
  
  weather_ensemble_names <- unique(noaa_future_site$parameter)
  
  
  #Fit linear model based on past data: water temperature = b1 + b2 * yesterday's water temp + b3 * air temperature
  #you will need to change the variable on the left side of the ~ if you are forecasting oxygen or chla
  fit <- lm(site_target$temperature ~ site_target$temp_yday + site_target$air_temperature)
  
  #parameter uncertainty
  coeffs <- round(fit$coefficients, 2)
  fit_summary <- summary(fit)
  params_se <- fit_summary$coefficients[,2]
  mod <- predict(fit, site_target) #replace model_data with site_target

  #this part needed for parameter uncertainty
  param_df <- data.frame(beta1 = rnorm(n_members, coeffs[1], params_se[1]),
                         beta2 = rnorm(n_members, coeffs[2], params_se[2]),
                         beta3 = rnorm(n_members, coeffs[3], params_se[3]))

  #this part needed for process uncertainty
  residuals <- mod - site_target$temperature
  sigma <- sd(residuals, na.rm = TRUE) # Process Uncertainty Noise Std Dev.; this is your sigma
  #used ai here^ to help me figure out how to introduce process uncertainty
  #but I understand that process uncertainty is what the model essentially doesn't capture. 
  #it's just the difference between model temperature and what the actual temperature is 

  #this part needed for IC uncertainty
  #need to update forecast_date to max datetime, because forecast date is not in the dataframe?
  curr_wt <- site_target %>% #need to update lake_df to site_target
    filter(datetime == max(datetime)) %>% # wtemp to temperature
    pull(temperature)
  
  ic_sd <- 0.1
  ic_uc <- rnorm(n = n_members, mean = curr_wt, sd = ic_sd) #this adds random noise based around my current wt to produce IC uncertainty
  
  #for each ensemble member there is now a diff IC value saved in this df to start it off
  #this from module 6 rmd
  ic_df <- tibble(forecast_date = rep(as.Date(forecast_date), times = n_members),
                  ensemble_member = c(1:n_members),
                  forecast_variable = "water_temperature",
                  value = ic_uc,
                  uc_type = "initial_conditions")
  
  forecast_ic_unc <- tibble(forecast_date = rep(forecasted_dates, times = n_members),
                            ensemble_member = rep(1:n_members, each = length(forecasted_dates)),
                            forecast_variable = "water_temperature",
                            value = as.double(NA),
                            uc_type = "initial_conditions") %>%
  rows_update(ic_df, by = c("forecast_date","ensemble_member","forecast_variable",
                              "uc_type")) 
  
  # Loop through all forecast dates
  for (t in 1:length(forecasted_dates)) {

    # Loop over each ensemble member
    for(ens in 1:n_members){

      met_ens <- weather_ensemble_names[ens]

      #pull driver ensemble for the relevant date; here we are using all 31 NOAA ensemble members
      temp_driv <- weather_future_daily %>%
        filter(datetime == forecasted_dates[t],
               site_id == curr_site,
               parameter == met_ens)

      # pull lagged water temp: use IC uncertainty for first date, previous forecast for subsequent dates
      #had difficulty in this part with the for loop and integrating ensemble members, had help w AI
      if(t == 1){
        temp_lag <- forecast_ic_unc %>%
          filter(forecast_date == forecasted_dates[t],
                 ensemble_member == ens) %>%
          pull(value)
      } else {
        temp_lag <- forecast_ic_unc %>%
          filter(forecast_date == forecasted_dates[t-1],
                 ensemble_member == ens) %>%
          pull(value)
      }

      #updated for param + IC + process uncertainty: wt = b1 + [b2 * yesterday's wt] + [b3 * air temp] + W
      forecasted_temperature <- param_df$beta1[ens] + param_df$beta2[ens] * temp_lag + param_df$beta3[ens] * temp_driv$air_temperature[1] + rnorm(1, mean = 0, sd = sigma) #<- ai helped me integrate process uncertainty here 
      #param uncertainty coming from the coefficients calculated earlier
      #IC uncertainty is set up before the for loop and integrated in the first time step when I make the dataframe
      #process uncertainty is the last part of this equation calculated from sigma earlier from the difference between the model and the observed temperatures. 
      
      # store forecast back into forecast_ic_unc so it can be used as lag for next timestep
      forecast_ic_unc <- forecast_ic_unc %>%
        mutate(value = ifelse(forecast_date == forecasted_dates[t] & ensemble_member == ens,
                              forecasted_temperature, value))

      # put all the relevant information into a tibble that we can bind together
      curr_site_df <- tibble(datetime = forecasted_dates[t],
                             site_id = curr_site,
                             parameter = met_ens,
                             prediction = forecasted_temperature,
                             variable = "temperature") #Change this if you are forecasting a different variable

      forecast_df <- dplyr::bind_rows(forecast_df, curr_site_df)

    }
  }
  
  message(curr_site, ' forecast run')
  
}

#---- Covert to EFI standard ----

# Make forecast fit the EFI standards
forecast_df_EFI <- forecast_df %>%
  filter(datetime > forecast_date) %>%
  mutate(model_id = my_model_id,
         reference_datetime = forecast_date,
         family = 'ensemble',
         duration = 'P1D',
         parameter = as.character(parameter),
         project_id = 'neon4cast') %>%
  select(datetime, reference_datetime, duration, site_id, family, parameter, variable, prediction, model_id, project_id)
#---------------------------#



# ----- Submit forecast -----
# Write the forecast to file
theme <- 'aquatics'
date <- forecast_df_EFI$reference_datetime[1]
forecast_name <- paste0(forecast_df_EFI$model_id[1], ".csv")
forecast_file <- paste(theme, date, forecast_name, sep = '-')

write_csv(forecast_df_EFI, forecast_file)

neon4cast::forecast_output_validator(forecast_file)


neon4cast::submit(forecast_file =  forecast_file, ask = FALSE) # if ask = T (default), it will produce a pop-up box asking if you want to submit

#--------------------------#

forecast_df_EFI |> 
  ggplot(aes(x=datetime, y=prediction, group = parameter)) +
  geom_line() +
  facet_wrap(~site_id) +
  labs(title = paste0('Forecast generated for ', forecast_df_EFI$variable[1], ' on ', forecast_df_EFI$reference_datetime[1]))

plot_file_name <- paste0("Submit_forecast/", forecast_df_EFI$variable[1], '-', forecast_df_EFI$reference_datetime[1], ".png")
ggsave(plot_file_name)

