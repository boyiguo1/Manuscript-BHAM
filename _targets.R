library(targets)
library(tarchetypes)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.

options(tidyverse.quiet = TRUE)
# Set target-specific options such as packages.
tar_option_set(packages = c("dplyr", "tidyverse", "rticles"))

# End this file with a list of target objects.
list(
  # tar_target(data, data.frame(x = sample.int(100), y = sample.int(100))),
  tar_render(manu, "Manuscript/main.Rmd", output_file = "SS_GAM.pdf")
)
