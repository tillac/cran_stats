# -------------------- #
# Scrape CRAN data #
# -------------------- #

library(tidyverse)
library(rvest)
library(polite)
library(cranlogs)

# Task Views Pages --------------------------------------------------------
# Bow
session <-
  bow("https://CRAN.r-project.org/",
      user_agent = "Thomas Vroylandt",
      delay = 5)

# scrape list of views
df_cran_views_link <-
  tibble(
    link = session %>%
      nod(path = "web/views/") %>%
      scrape(content = "text/html; charset=UTF-8") %>%
      html_nodes("a") %>%
      html_attr("href")
  ) %>%
  filter(!str_detect(link, "http")) %>%
  mutate(link = paste0("web/views/", link))

# scrape views content
df_cran_views <- df_cran_views_link %>%
  mutate(description = map(link, function(x) {
    session %>%
      nod(path = x) %>%
      scrape(content = "text/html; charset=UTF-8")
  }))

# clean views
df_cran_pkg_views <- df_cran_views %>%
  mutate(
    views = str_remove_all(link, "web/views/|.html"),
    pkg_name = map(description, function(x) {
      x %>%
        html_nodes("a") %>%
        html_attr("href")
    })
  ) %>%
  select(views, pkg_name) %>%
  unnest(c(pkg_name)) %>%
  filter(str_detect(pkg_name, "../packages/")) %>%
  mutate(pkg_name = str_remove_all(pkg_name, "../packages/|/index.html")) %>%
  distinct(views, pkg_name)

# CRAN pkg description ----------------------------------------------------
df_cran_pkg_desc <- tools::CRAN_package_db() %>%
  as_tibble() %>%
  janitor::clean_names()

# Clean descriptions ------------------------------------------------------
# pkg info
df_cran_pkg_infos <- df_cran_pkg_desc %>%
  select(package, version, license, title, description) %>%
  left_join(df_cran_pkg_views, by = c("package" = "pkg_name"))

# pkg author + role
df_cran_pkg_authors <- df_cran_pkg_desc %>%
  select(package, author) %>%
  separate_rows(author, sep = "\\]") %>%
  filter(author != "") %>%
  separate(author, into = c("authors", "role"), sep = "\\[") %>%
  separate_rows(role, sep = ", ") %>%
  mutate(
    role = str_trim(role),
    authors = str_remove_all(authors, ",|\n|\\s*\\([^\\)]+\\)|\\)"),
    authors = str_trim(authors)
  )

# pkg author email last - to get institution
df_cran_pkg_mails <- df_cran_pkg_desc %>%
  filter(!is.na(authors_r)) %>%
  select(package, authors_r) %>%
  separate_rows(authors_r, sep = "email|person\\(|comment|ORCID|role") %>%
  filter(str_detect(authors_r, "\\@")) %>%
  mutate(
    authors_r = str_remove_all(authors_r, ' |=|"|\\\n|\\\\|\\(|\\)|,'),
    mail_domain = str_extract(authors_r, "(?<=\\@)(.*)")
  ) %>%
  select(package, mail_domain)

# function for dependencies
make_dep_long <- function(data, var_long) {
  data %>%
    select(package, {
      {
        var_long
      }
    }) %>%
    separate_rows({
      {
        var_long
      }
    }, sep = ",") %>%
    separate({
      {
        var_long
      }
    }, into = c("pkg_dep", "version_dep"), sep = "\\(") %>%
    mutate(
      version_dep = str_remove(version_dep, "\\)"),
      version_dep = str_trim(version_dep),
      pkg_dep = str_trim(pkg_dep),
      type_dep = quo_name(var_long)
    ) %>%
    select(package, pkg_dep, type_dep, version_dep)
}

# dependencies
df_cran_pkg_deps <-
  make_dep_long(df_cran_pkg_desc, "depends") %>%
  bind_rows(make_dep_long(df_cran_pkg_desc, "imports")) %>%
  bind_rows(make_dep_long(df_cran_pkg_desc, "suggests")) %>%
  bind_rows(make_dep_long(df_cran_pkg_desc, "linking_to"))

# CRAN logs ---------------------------------------------------------------
# cut the pkg list into chunks of size 100
list_pkg_size100 <-
  split(df_cran_pkg_desc$package, ceiling(seq_along(df_cran_pkg_desc$package) /
                                            100))

# progress bar
total <- length(list_pkg_size100)
pb <-
  progress::progress_bar$new(format = "[:bar] :current/:total (:percent)", total = total)


# download full logs
df_cran_pkg_logs <- map_dfr(list_pkg_size100,
                            function(x) {
                              pb$tick()
                              cran_downloads(x, from = "2013-01-01", to = "2020-07-31")
                            }) %>%
  as_tibble() %>% 
  distinct()

# clean logs
df_cran_pkg_logs_clean <- df_cran_pkg_logs  %>%
  filter(count > 0) %>% 
  mutate(year = lubridate::year(date),
         month = lubridate::month(date)) %>%
  group_by(year, month, package) %>%
  summarise(nb_download = sum(count)) %>%
  ungroup()

# Export ------------------------------------------------------------------
write_rds(df_cran_pkg_infos, "data/cran_pkg_infos.rds")
write_rds(df_cran_pkg_mails, "data/cran_pkg_mails.rds")
write_rds(df_cran_pkg_authors, "data/cran_pkg_authors.rds")
write_rds(df_cran_pkg_deps, "data/cran_pkg_deps.rds")
write_rds(df_cran_pkg_logs_clean, "data/cran_pkg_logs.rds")
