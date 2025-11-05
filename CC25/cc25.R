library(tidyverse)
library(janitor)

# ---- Locate all CSVs ----
files <- list.files("CC25", pattern = "\\.csv$", full.names = TRUE)

# ---- Read all files once and process ----
all_data <- map_dfr(files, ~{
  df <- read_csv(.x, show_col_types = FALSE) %>% clean_names()
  
  # extract date from file name
  dd_mm <- str_remove(basename(.x), "\\.csv$")
  file_date <- dmy(paste0(dd_mm, "-", year(Sys.Date())))
  
  # Add file_date and webinar name
  df %>%
    mutate(
      file_date = file_date,
      webinar = dd_mm,
      registered_flag = if_else(registration_status == "Registered", 1L, 0L),
      attendance_flag = if_else(role == "Attendee", 1L, 0L)
    )
})

# ---- Summary table: total registered and attended per file ----
summary_tbl <- all_data %>%
  group_by(file_date) %>%
  summarise(
    registered = sum(registered_flag, na.rm = TRUE),
    attendance = sum(attendance_flag, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(file_date)

summary_tbl

# ---- Registration counts per email ----
registration_summary <- all_data %>%
  group_by(registration_email) %>%
  summarise(webinars_registered = sum(registered_flag, na.rm = TRUE), .groups = "drop") %>%
  count(webinars_registered, name = "num_people") %>%
  arrange(desc(webinars_registered))

registration_summary

# ---- Extract domain from registration_email ----
domain_summary <- all_data %>%
  # remove missing emails and keep only unique emails
  filter(!is.na(registration_email) & registration_email != "") %>%
  distinct(registration_email, .keep_all = TRUE) %>%   # ensure no double-count
  # extract everything after @
  mutate(domain = str_extract(registration_email, "(?<=@).*")) %>%
  # count unique registrations per domain
  count(domain, name = "num_people") %>%
  arrange(desc(num_people))

domain_summary


# ---- Get unique emails with domain ----
unique_domains <- all_data %>%
  filter(!is.na(registration_email) & registration_email != "") %>%
  distinct(registration_email, .keep_all = TRUE) %>%   # only one row per email
  mutate(domain = str_extract(registration_email, "(?<=@).*"),
         ac_uk = if_else(str_ends(domain, "ac.uk"), "ac.uk", "non-ac.uk"))

# ---- Count academic vs non-academic ----
ac_summary <- unique_domains %>%
  count(ac_uk, name = "num_people")

ac_summary

write_csv(domain_summary, file.path("domain_counts.csv"))
