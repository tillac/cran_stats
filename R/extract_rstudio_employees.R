# ------------------------- #
# Extract RStudio employees #
# ------------------------- #

library(rvest)
library(tidyverse)

# Read page ---------------------------------------------------------------
rstudio_about <- read_html("https://rstudio.com/about/")

# List of employees -------------------------------------------------------
df_rstudio_employees <- tibble(name = rstudio_about %>%
                                 html_nodes(".teammember") %>%
                                 html_nodes(".name") %>%
                                 html_text()) %>%
  mutate(
    name = str_remove_all(name, "\n"),
    first_name = str_extract(name, "([^\\s]+)"),
    last_name = str_extract(name, "\\s(.*)"),
    last_name = str_trim(last_name)
  ) %>%
  filter(name != "Maybe You ?")

# Export ------------------------------------------------------------------
write_rds(df_rstudio_employees, "data/rstudio_employees.rds")