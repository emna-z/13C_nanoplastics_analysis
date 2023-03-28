library("tidyverse")

# source("./1_calibration_curves.R")

# calculate ppm HS --------------------------------------------------------

kill_c <- read_delim("../data_used/kill_controls.txt")

kill_c <- kill_c |>
  mutate(CO2_ppm = co2_area * slope_2) |> 
  mutate(CO2_HS_ml = HS_ml/1000000*CO2_ppm) |> 
  mutate(CO2_mmol = ((pressure_bar+delta_pressure)*CO2_HS_ml)/(0.083145*(273.15+T_C))) |> 
  mutate(CO2_umol = CO2_mmol*1000)
  
# write_csv(kill_c, "../data_used/kill_controls_calc1.csv")


# summary_CO2 -------------------------------------------------------------

kills_summary <- kill_c|>
  group_by(Sample, time ) |> 
  summarise(average_CO2_mmol = mean(CO2_mmol), std_CO2_area_ppm = sd(CO2_mmol),
            average_delta_13c = mean(delta_13C), std_delta_13c = sd(delta_13C))

