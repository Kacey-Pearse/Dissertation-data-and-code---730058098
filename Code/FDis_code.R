library(dplyr)
library(tidyr)
library(FD)
library(tibble)

traits <- read.csv("Data/Cleaned/dissertation_data - trait_table.csv", stringsAsFactors = FALSE)
obs <- read.csv("Data/Cleaned/dissertation_data - obs.csv", stringsAsFactors = FALSE)

clean_names <- function(x) {
  x |>
    trimws() |>                 # remove leading/trailing spaces
    tolower() |>                # force lowercase
    gsub("\\s+", " ", x = _)    # remove double spaces
}

traits$species <- clean_names(traits$species)
obs$species <- clean_names(obs$species)

trait_matrix <- traits %>%
  select(
    species,
    trophic_group,
    feeding_mode,
    primary_food,
    mobility,
    body_size,
    body_form)

trait_matrix <- distinct(trait_matrix)

trait_matrix <- trait_matrix %>%
  distinct(species, .keep_all = TRUE)

trait_matrix[-1] <- lapply(trait_matrix[-1], as.factor)

trait_matrix <- column_to_rownames(trait_matrix, "species")
nrow(trait_matrix)

obs <- obs %>%
  mutate(site_date = paste(site, date, sep = "_"))

community_matrix <- obs %>%
  group_by(site_date, species) %>%
  summarise(abundance = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = species,
    values_from = abundance,
    values_fill = 0)
community_matrix <- column_to_rownames(community_matrix, "site_date")
ncol(community_matrix)

common_species <- intersect(
  colnames(community_matrix),
  rownames(trait_matrix))

length(common_species)

if(length(common_species) < 10){
  stop("Too few matching species — check species name formatting.")}

community_matrix <- community_matrix[, common_species]
trait_matrix <- trait_matrix[common_species, ]
community_matrix <- community_matrix[rowSums(community_matrix) > 0, ]
nrow(community_matrix)

species_per_comm <- rowSums(community_matrix > 0)
summary(species_per_comm)

fd_results <- dbFD(
  x = trait_matrix,
  a = community_matrix,
  calc.FRic = FALSE,
  calc.FDiv = FALSE,
  stand.x = FALSE)

fdis_values <- fd_results$FDis

fdis_df <- data.frame(
  site_date = rownames(community_matrix),
  FDis = fdis_values)

fdis_df <- fdis_df %>%
  separate(site_date, into = c("site", "date"), sep = "_")

fdis_df$date <- as.Date(fdis_df$date)