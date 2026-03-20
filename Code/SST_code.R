library(terra)
library(dplyr)
library(lubridate)

nc_files <- list.files(
  "Data/Raw/SST sites/",
  pattern = "\\.nc$",
  full.names = TRUE)

file_lookup <- data.frame(
  file = c(
    "cas_sst.nc", "mtb_sst.nc", "kil_sst.nc", "bea_sst.nc", "eas_sst.nc",
    "lee_sst.nc", "men_sst.nc", "ovi_sst.nc", "sho_sst.nc"),
  site = c(
    "CAS", "MTB", "KIL", "BEA", "EAS","LEE", "MEN", "OVI", "SHO"),
  stringsAsFactors = FALSE)

sampling_events <- data.frame(
  site = c("CAS", "CAS", "CAS", "CAS", "CAS", "CAS", "CAS", "CAS", "CAS", "CAS",
           "CAS", "CAS", "CAS", "CAS", "MTB", "MTB", "MTB", "MTB", "MTB", "MTB",
           "MTB", "MTB", "MTB", "MTB", "MTB", "MTB", "MTB", "MTB", "KIL", "KIL",
           "KIL", "LEE", "LEE", "LEE", "OVI", "OVI", "OVI", "OVI", "OVI", "SHO",
           "SHO", "SHO", "BEA", "BEA", "BEA", "EAS", "EAS", "EAS", "EAS", "MEN",
           "MEN", "MEN", "MEN", "MEN", "MEN", "MEN"),
  sampling_date = as.Date(c(
    "2024-10-19", "2024-11-16", "2024-12-14", "2025-01-18", "2025-02-15", "2025-03-15",
    "2025-04-12", "2025-05-17", "2025-06-14", "2025-07-12", "2025-08-09", "2025-10-18",
    "2025-11-08", "2025-12-06", "2024-11-16", "2024-12-14", "2025-01-18", "2025-02-15",
    "2025-03-02", "2025-03-15", "2025-04-12", "2025-05-17", "2025-06-14", "2025-07-12",
    "2025-08-09", "2025-10-18", "2025-11-08", "2025-12-06", "2025-10-18", "2025-11-22",
    "2025-12-20", "2025-10-18", "2025-11-01", "2025-12-13", "2025-05-23", "2025-07-19",
    "2025-10-18", "2025-11-01", "2025-12-13", "2025-10-04", "2025-11-09", "2025-12-14",
    "2025-10-12", "2025-11-08", "2025-12-06", "2025-05-20", "2025-10-11", "2025-11-08",
    "2025-12-06", "2025-02-12", "2025-03-12", "2025-04-12", "2025-05-18", "2025-10-18",
    "2025-11-01", "2025-12-13")))

results <- list()

for (file in nc_files) {
  
  site_name <- file_lookup$site[
    file_lookup$file == basename(file)]
  
  if (length(site_name) == 0) {
    warning(paste("No site match for file:", basename(file)))
    next}
  
  sst <- rast(file)
  
  time(sst) <- as.POSIXct(
    time(sst),
    origin = "1950-01-01",
    tz = "UTC")
  
  site_dates <- sampling_events %>%
    filter(site == site_name)
  
  if (nrow(site_dates) == 0) next
  
  for (i in 1:nrow(site_dates)) {
    
    sd <- site_dates$sampling_date[i]
    
    start_time <- as.POSIXct(sd - days(6), tz = "UTC")
    end_time   <- as.POSIXct(sd + days(1), tz = "UTC")
    
    time_idx <- which(
      time(sst) >= start_time &
        time(sst) < end_time)
    
    if (length(time_idx) == 0) next
    
    sst_window <- sst[[time_idx]]
    
    results[[length(results) + 1]] <- data.frame(
      site = site_name,
      sampling_date = sd,
      mean_sst = global(sst_window, mean, na.rm = TRUE)[1,1],
      n_hours = length(time_idx))}}

sst_7day_means <- bind_rows(results) %>%
  mutate(
    expected_hours = 24 * 7,
    coverage = n_hours / expected_hours)

summary(sst_7day_means$mean_sst)
summary(sst_7day_means$coverage)
