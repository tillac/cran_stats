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
    name = stringi::stri_trans_general(name, "Latin-ASCII"),
    name = case_when(name == "J.J. Allaire" ~ "JJ Allaire",
                     name == "Thomas Pedersen" ~ "Thomas Lin Pedersen",
                     name == "Rich Iannone" ~ "Richard Iannone",
                     TRUE ~ name)
  ) %>%
  filter(name != "Maybe You?")

# Export ------------------------------------------------------------------
write_rds(df_rstudio_employees, "data/rstudio_employees.rds")