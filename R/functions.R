clean_penguin_data <- function(penguin_data_raw) {
  penguin_data_raw %>%
    janitor::clean_names() %>%
    transmute(
      species = as.factor(species),
      island = as.factor(island),
      bill_length_mm = culmen_length_mm,
      bill_depth_mm = culmen_depth_mm,
      body_mass_g = body_mass_g,
      sex = as.factor(sex),
      year = lubridate::year(date_egg)
    ) %>%
    remove_missing()
}

augment_with_mod_name <- function(mod_in_list) {
  model_name <- names(mod_in_list)
  broom::augment(mod_in_list[[1]]) %>%
    mutate(model = model_name)
}

glance_with_mod_name <- function(mod_in_list) {
  model_name <- names(mod_in_list)
  broom::glance(mod_in_list[[1]]) %>%
    mutate(model = model_name)
}