library(terra)
library(dplyr)
library(lubridate)

nc_files <- list.files(
  "Data/Raw/Wave sites/",
  pattern = "\\.nc$", full.names = TRUE)

file_lookup <- data.frame(
  file = c("cas_wave.nc", "mtb_wave.nc", "kil_wave.nc", "bea_wave.nc", "eas_wave.nc",
           "lee_wave.nc", "men_wave.nc", "ovi_wave.nc", "sho_wave.nc"),
  site = c("CAS", "MTB", "KIL", "BEA", "EAS", "LEE", "MEN", "OVI", "SHO"),
  stringsAsFactors = FALSE)

sampling_events <- data.frame(
  site = c("CAS","CAS","CAS","CAS","CAS","CAS","CAS","CAS","CAS","CAS",
           "CAS","CAS","CAS","CAS","MTB","MTB","MTB","MTB","MTB","MTB",
           "MTB","MTB","MTB","MTB","MTB","MTB","MTB","MTB","KIL","KIL",
           "KIL","LEE","LEE","LEE","OVI","OVI","OVI","OVI","OVI","SHO",
           "SHO","SHO","BEA","BEA","BEA","EAS","EAS","EAS","EAS","MEN",
           "MEN","MEN","MEN","MEN","MEN","MEN"),
  sampling_date = as.Date(c(
    "2024-10-19","2024-11-16","2024-12-14","2025-01-18","2025-02-15","2025-03-15",
    "2025-04-12","2025-05-17","2025-06-14","2025-07-12","2025-08-09","2025-10-18",
    "2025-11-08","2025-12-06","2024-11-16","2024-12-14","2025-01-18","2025-02-15",
    "2025-03-02","2025-03-15","2025-04-12","2025-05-17","2025-06-14","2025-07-12",
    "2025-08-09","2025-10-18","2025-11-08","2025-12-06","2025-10-18","2025-11-22",
    "2025-12-20","2025-10-18","2025-11-01","2025-12-13","2025-05-23","2025-07-19",
    "2025-10-18","2025-11-01","2025-12-13","2025-10-04","2025-11-09","2025-12-14",
    "2025-10-12","2025-11-08","2025-12-06","2025-05-20","2025-10-11","2025-11-08",
    "2025-12-06","2025-02-12","2025-03-12","2025-04-12","2025-05-18","2025-10-18",
    "2025-11-01","2025-12-13")))

results <- list()

for (file in nc_files) {
  
  site_name <- file_lookup$site[file_lookup$file == basename(file)]
  if (length(site_name) == 0) next
  
  wave <- rast(file)
  
  time(wave) <- as.POSIXct(time(wave), origin = "1950-01-01", tz = "UTC")
  
  site_dates <- sampling_events %>% filter(site == site_name)
  if (nrow(site_dates) == 0) next
  
  for (i in 1:nrow(site_dates)) {
    
    sd <- site_dates$sampling_date[i]
    
    start_time <- as.POSIXct(sd - days(6), tz = "UTC")
    end_time   <- as.POSIXct(sd + days(1), tz = "UTC")
    
    time_idx <- which(time(wave) >= start_time & time(wave) < end_time)
    if (length(time_idx) == 0) next
    
    wave_window <- wave[[time_idx]]
    
    hs_ts <- global(wave_window, mean, na.rm = TRUE)[,1]
    
    results[[length(results) + 1]] <- data.frame(
      site = site_name,
      sampling_date = sd,
      p95_wave = quantile(hs_ts, 0.95, na.rm = TRUE),
      mean_energy = mean(hs_ts^2, na.rm = TRUE),
      n_hours = length(time_idx)
    ) }}

wave_exposure <- bind_rows(results) %>%
  mutate(
    expected_hours = 24 * 7,
    coverage = n_hours / expected_hours)

summary(wave_exposure$p95_wave)
summary(wave_exposure$mean_energy)
