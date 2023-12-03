## code to prepare `all_forms.R` dataset goes here

library(tidyverse)
library(reformR)
#library(DataEDU)
library(magrittr)


get_single_template <- function (file.name) {
  path <- str_c("data-raw/data/", file.name)
  read_excel_all(path) %>%
    generate_template()
}

all_forms_fun <- function() {
  templates <- tibble (files = list.files("data-raw/data/")) %>%
    filter(str_detect(files, ".xlsx$")) %>%
    mutate (files.temp = str_remove(files, ".xlsx$")) %>%
    mutate (version = str_extract(files.temp, "\\.v\\d.*") %>%
              str_sub(start = 2),
            form.name = str_remove(files.temp, "\\.v\\d.*")) %>%
    select (-files.temp)

  templates %>%
    bind_cols(
      map(templates$files, get_single_template) %>%
        bind_rows
    )
}

all_forms <- all_forms_fun()

usethis::use_data(all_forms, overwrite = TRUE)
