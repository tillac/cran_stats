# -------------------- #
# Scrape CRAN data #
# -------------------- #

library(tidyverse)
library(rvest)
library(cranlogs)
library(polite)

# CRAN pkg list -----------------------------------------------------------
# Bow
session <-
  bow("https://CRAN.r-project.org/",
      user_agent = "Thomas Vroylandt",
      delay = 0.5)

# scrape list of packages
df_cran_pkg <-
  tibble(
    pkg_link = session %>%
      nod(path = "web/packages/available_packages_by_name.html") %>%
      scrape(content = "text/html; charset=UTF-8") %>%
      html_nodes("a") %>%
      html_attr("href")
  ) %>%
  filter(!str_detect(pkg_link, "#available-packages-")) %>%
  mutate(
    pkg_link = str_remove(pkg_link, "../../"),
    pkg_name = str_remove_all(pkg_link,
                              "web/packages/|/index.html")
  )

# CRAN pkg description ----------------------------------------------------
# progress bar
total <- length(df_cran_pkg$pkg_link)
pb <-
  progress::progress_bar$new(format = "[:bar] :current/:total (:percent)", total = total)

# scrape pages
df_cran_pkg_desc <- df_cran_pkg %>%
  mutate(description = map(pkg_link, function(x) {
    pb$tick()
    
    session %>%
      nod(path = x) %>%
      scrape(content = "text/html; charset=UTF-8")
  }))

# clean them all
df_cran_pkg_desc_clean <- df_cran_pkg_desc %>%
  mutate(description_clean = map(description, function(x) {
    df_name <- tibble(X1 = "short_desc",
                      X2 = x %>%
                        html_node("h2") %>%
                        html_text())
    
    df_desc <- tibble(X1 = "long_desc",
                      X2 = x %>%
                        html_node("p") %>%
                        html_text())
    
    df_tables <- tibble(x %>%
                          html_nodes("table") %>%
                          html_table() %>%
                          bind_rows())
    
    df_name %>%
      bind_rows(df_desc) %>%
      bind_rows(df_tables)
  })) %>%
  select(pkg_name, description_clean) %>%
  unnest(description_clean) %>%
  mutate(X1 = str_remove(X1, ":"),
         X1 = str_replace_all(X1, "\\s", "_")) %>%
  pivot_wider(names_from = X1, values_from = X2)

# Clean descriptions ------------------------------------------------------
# pkg info
df_cran_pkg_infos <- df_cran_pkg_desc_clean %>%
  select(pkg_name,
         short_desc,
         long_desc,
         Version,
         Published,
         Maintainer,
         License) %>%
  rename_all(str_to_lower) %>%
  separate(maintainer,
           into = c("maintainter", "maintainer_mail"),
           sep = "<") %>%
  mutate(
    maintainer_mail = str_remove(maintainer_mail, ">"),
    maintainer_mail = str_extract(maintainer_mail, "at(.*)"),
    maintainer_mail = str_remove(maintainer_mail, "at ")
  )

# pkg subject
df_cran_pkg_subject <- df_cran_pkg_desc_clean %>%
  select(pkg_name, In_views) %>%
  separate(In_views, into = paste0("In_views", seq(1, 100)), sep = ", ") %>%
  pivot_longer(contains("In_views"), values_to = "subject") %>%
  filter(!is.na(subject) & subject != "") %>%
  select(-name)

# pkg author + role
df_cran_pkg_authors <- df_cran_pkg_desc_clean %>%
  select(pkg_name, Author) %>%
  separate(Author, into = paste0("Author", seq(1, 1000)), sep = "\\]") %>%
  pivot_longer(contains("Author")) %>%
  filter(!is.na(value) & value != "") %>%
  select(-name) %>%
  separate(value, into = c("authors", "role"), sep = "\\[") %>%
  separate(role, into = paste0("role", seq(1, 10)), sep = ",") %>%
  pivot_longer(contains("role"), values_to = "role") %>%
  select(-name) %>%
  filter(!is.na(role) & role != "") %>%
  mutate(
    role = str_trim(role),
    authors = str_remove_all(authors, ",|\n|\\s*\\([^\\)]+\\)|\\)"),
    authors = str_trim(authors)
  )

# function for dependencies
make_dep_long <- function(data, var_long) {
  data %>%
    select(pkg_name, all_of(var_long)) %>%
    separate(var_long, into = paste0(var_long, seq(1, 1000)), sep = ",") %>%
    pivot_longer(cols = contains(var_long),
                 names_to = "type_dep",
                 values_to = "name_dep") %>%
    filter(!is.na(name_dep)) %>%
    mutate(type_dep = var_long) %>%
    separate(name_dep,
             into = c("name_dep", "version_dep"),
             sep = " \\(") %>%
    mutate(
      version_dep = str_remove(version_dep, "\\)|\n"),
      version_dep = str_sub(version_dep, 2, 20),
      version_dep = str_trim(version_dep),
      name_dep = str_trim(name_dep)
    )
}

# dependencies
df_cran_pkg_deps <-
  make_dep_long(df_cran_pkg_desc_clean, "Depends") %>%
  bind_rows(make_dep_long(df_cran_pkg_desc_clean, "Imports")) %>%
  bind_rows(make_dep_long(df_cran_pkg_desc_clean, "Suggests")) %>%
  bind_rows(make_dep_long(df_cran_pkg_desc_clean, "Reverse_depends")) %>%
  bind_rows(make_dep_long(df_cran_pkg_desc_clean, "Reverse_imports")) %>%
  bind_rows(make_dep_long(df_cran_pkg_desc_clean, "Reverse_suggests"))

# df_cran_pkg_deps %>%
#   filter(name_dep != "R") %>%
#   group_by(pkg_name, type_dep) %>%
#   count() %>%
#   arrange(desc(n))

# CRAN logs ---------------------------------------------------------------
# download full logs
df_cran_pkg_logs <-
  cran_downloads(df_cran_pkg$pkg_name, from = "2012-10-01", to = "2020-07-31") %>%
  as_tibble()

# Export ------------------------------------------------------------------
write_rds(df_cran_pkg_infos, "data/cran_pkg_infos.rds")
write_rds(df_cran_pkg_subject, "data/cran_pkg_subject.rds")
write_rds(df_cran_pkg_authors, "data/cran_pkg_authors.rds")
write_rds(df_cran_pkg_deps, "data/cran_pkg_deps.rds")
write_rds(df_cran_pkg_logs, "data/cran_pkg_logs.rds")
