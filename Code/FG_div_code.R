dat <- read.csv(
  "Data/Cleaned/dissertation_data - obs.csv",
  stringsAsFactors = FALSE)

dat$date <- as.Date(dat$date)

dat$site <- trimws(dat$site)

site_lookup <- data.frame(
  site = c(
    "Menai Bridge Beach, Bangor",
    "East Sands Beach, Fife",
    "Beadnell Haven, Northumberland",
    "Ovingdean Beach, Brighton",
    "East Beach, Southend-On-Sea",
    "Lee-on-the-Solent Beach, Hampshire",
    "Kilve Beach, Somerset",
    "Castle Beach, Falmouth",
    "Mount Batten Beach, Plymouth"),
  site_id = c(
    "MEN", "EAS", "BEA", "OVI", "SHO",
    "LEE", "KIL", "CAS", "MTB"),
  stringsAsFactors = FALSE)

site_lookup$site <- trimws(site_lookup$site)

dat$site_id <- site_lookup$site_id[
  match(trimws(dat$site), site_lookup$site)]

table(is.na(dat$site_id))

unique(dat$site[is.na(dat$site_id)])

dat$event_id <- paste(dat$site_id, dat$date, sep = "_")

dat_unique <- dat[!duplicated(dat[, c("event_id", "species")]), ]

#--------FUNCTIONAL GROUP RICHNESS--------#

fg_richness <- aggregate(
  functional_group ~ event_id,
  data = dat_unique,
  FUN = function(x) length(unique(x)))

names(fg_richness)[2] <- "fg_richness"


event_info <- unique(dat[, c("event_id", "site", "site_id", "date")])

fg_richness <- merge(
  fg_richness,
  event_info,
  by = "event_id",
  all.x = TRUE)

#--------FUNCTIONAL REDUNDANCY--------#

species_richness <- aggregate(
  species ~ event_id,
  data = dat_unique,
  FUN = length)

names(species_richness)[2] <- "species_richness"

redundancy_df <- merge(
  species_richness,
  fg_richness,
  by = "event_id")

redundancy_df$functional_redundancy <-
  redundancy_df$species_richness / redundancy_df$fg_richness

event_info <- unique(dat[, c("event_id", "site", "site_id", "date")])

redundancy_df <- merge(
  redundancy_df,
  event_info,
  by = "event_id",
  all.x = TRUE)
