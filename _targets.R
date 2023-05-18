source("R/packages.R")
source("R/functions.R")

tar_plan(
  tar_file_read(
    penguin_data_raw,
    path_to_file("penguins_raw.csv"),
    read_csv(!!.x)
  ),
  penguin_data = clean_penguin_data(penguin_data_raw),
  penguin_models = list(
    combined = lm(
      bill_depth_mm ~ bill_length_mm, data = penguin_data),
    separate = lm(
      bill_depth_mm ~ bill_length_mm * species, data = penguin_data)
  ),
  tar_target(
    penguin_models_augmented,
    augment_penguins(penguin_models),
    pattern = map(penguin_models)
  ),
  tar_target(
    penguin_models_summary,
    glance_penguins(penguin_models),
    pattern = map(penguin_models)
  ),
  tar_quarto(
    penguin_report,
    path = "penguin_report.qmd",
    quiet = FALSE,
    packages = c("targets", "tidyverse")
  )
)
