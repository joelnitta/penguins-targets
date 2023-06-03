#' Clean raw penguins data
#'
#' @param penguins_data_raw Raw data for palmer penguins; the data contained
#' in palmerpenguins::path_to_file("penguins_raw.csv").
#'
#' @return Data frame; the cleaned data for modeling bill depth and length
#'
clean_penguin_data <- function(penguins_data_raw) {
  penguins_data_raw %>%
    select(
      species = Species,
      bill_length_mm = `Culmen Length (mm)`,
      bill_depth_mm = `Culmen Depth (mm)`
    ) %>%
    remove_missing(na.rm = TRUE) %>%
    separate(species, into = "species", extra = "drop")
}

#' Augment a model with predicted values of a response variable and include
#' the name of the model in the output
#'
#' @param mod_in_list Named list of length 1; the element should be a model
#' (output of lm()).
#'
#' @return Dataframe including the name of the model in column "model"
#'
augment_with_mod_name <- function(mod_in_list) {
  model_name <- names(mod_in_list)
  broom::augment(mod_in_list[[1]]) %>%
    mutate(model = model_name)
}

#' Summarize parameters of a model and include the name of the model in the
#' output
#'
#' @param mod_in_list Named list of length 1; the element should be a model
#' (output of lm()).
#'
#' @return Dataframe including the name of the model in column "model"
#'
glance_with_mod_name <- function(mod_in_list) {
  model_name <- names(mod_in_list)
  broom::glance(mod_in_list[[1]]) %>%
    mutate(model = model_name)
}
