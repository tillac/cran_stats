# -------------------- #
# Scrape CRAN data #
# -------------------- #

library(tidyverse)
library(cranlogs)

# CRAN pkg description ----------------------------------------------------
df_cran_pkg_desc <- tools::CRAN_package_db() %>%
  as_tibble() %>%
  janitor::clean_names()

# Clean descriptions ------------------------------------------------------
# pkg info
df_cran_pkg_infos <- df_cran_pkg_desc %>%
  select(package, version, license, title, description)

# pkg author + role
df_cran_pkg_authors <- df_cran_pkg_desc %>%
  select(package, author) %>%
  separate_rows(author, sep = "\\]") %>%
  filter(author != "") %>%
  separate(author, into = c("authors", "role"), sep = "\\[") %>%
  separate_rows(role, sep = ",") %>%
  separate_rows(authors, sep = ",") %>%
  separate_rows(authors, sep = " and ") %>%
  separate_rows(authors, sep = " & ") %>%
  separate_rows(authors, sep = ";") %>%
  separate_rows(authors, sep = "\\\n") %>%
  mutate(
    role = str_trim(role),
    authors = stringi::stri_trans_general(authors, "Latin-ASCII"),
    authors = str_remove_all(authors, ",|\n|\\s*\\([^\\)]+\\)|\\)|'"),
    authors = str_remove_all(authors, "(?<=<)(.*)(?=>)"),
    authors = str_remove_all(authors, "(?<=with )(.*)"),
    authors = str_remove_all(authors, "(?<=contribution)(.*)"),
    authors = str_remove_all(authors, "'|<|>|with |contribution"),
    authors = str_replace_all(authors, "  ", " "),
    authors = str_trim(authors)
  ) %>%
  filter(authors != "") %>% 
mutate(
  authors_bis = str_replace(authors, " ", "_"),
  authors_bis = stringi::stri_replace_last_fixed(authors_bis, " ", "_"),
  space_count = str_count(authors_bis, " "),
  authors_ok = if_else(space_count == 0, "ok", "ko")
)

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
write_rds(df_cran_pkg_authors, "data/cran_pkg_authors.rds")
write_rds(df_cran_pkg_deps, "data/cran_pkg_deps.rds")
write_rds(df_cran_pkg_logs_clean, "data/cran_pkg_logs.rds")
