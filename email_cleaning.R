library(tidyverse)
library(knitr)

csv_files <- list.files(pattern = "\\.csv$", full.names = TRUE)

summary_all <- tibble()
attendance_all <- tibble()

for (file_path in csv_files) {
  
    summary_data <- read_csv(
    file_path,
    col_names = FALSE,
    n_max = 10,
    col_types = cols_only(
      X1 = col_character(),
      X2 = col_character()
    ),
    show_col_types = FALSE
  )
  
  summary_tbl <- summary_data %>%
    filter(X1 %in% c("Registered participants", "Attended participants")) %>%
    pivot_wider(names_from = X1, values_from = X2) %>%
    rename_with(~ str_replace_all(tolower(.x), " ", "_")) %>%
    mutate(
      across(everything(), as.numeric),
      session = str_remove(basename(file_path), "\\.csv$")
    ) %>%
    select(session, everything())
  
  summary_all <- bind_rows(summary_all, summary_tbl)
  
  full_data <- read_csv(file_path, col_names = FALSE, show_col_types = FALSE)
  
  attendance_header_row <- which(full_data$X1 == "Name")[1]
  activity_header_row <- which(full_data$X1 == "3. In-Meeting Activities")[1]
  
  attendance_block <- full_data[(attendance_header_row):(activity_header_row - 2), ]
  colnames(attendance_block) <- attendance_block[1, ]
  attendance_block <- attendance_block[-1, ]
  
  clean_attendance <- attendance_block %>%
    select(`Email`, `Registration Email`, `Role`) %>%
    mutate(
      Email = coalesce(`Registration Email`, `Email`),  
      session = str_remove(basename(file_path), "\\.csv$")
    ) %>%
    select(session, Email, Role)
  
  attendance_all <- bind_rows(attendance_all, clean_attendance)
}

# Summary statistics ------------------------------------------------------

summary_with_averages <- summary_all %>%
  bind_rows(
    tibble(
      session = "Average",
      registered_participants = round(mean(summary_all$registered_participants, na.rm = TRUE)),
      attended_participants = round(mean(summary_all$attended_participants, na.rm = TRUE))
    )
  )

kable(summary_with_averages, caption = "Summary of Registered and Attended Participants with Averages")

# Calculate unique registration emails
unique_registrations <- n_distinct(attendance_all$Email)

# Calculate unique attendees (those with "Attendee" in the Role column)
unique_attendees <- n_distinct(attendance_all$Email[attendance_all$Role == "Attendee"])

unique_counts <- tibble(
  metric = c("Unique registrations", "Unique attendees"),
  count = c(
    n_distinct(attendance_all$Email),
    n_distinct(attendance_all$Email[attendance_all$Role == "Attendee"])
  )
)

kable(unique_counts, caption = "Unique Registrations and Attendees")

# Function to extract domain from email
extract_institution <- function(email) {
  domain <- sub(".*@", "", email)  # Extract everything after "@"
  return(domain)
}

# Create a new column with the institution domain
attendance_all <- attendance_all %>%
  mutate(institution = extract_institution(Email))

# Get unique institutions for registrations
unique_institutions_registrations <- attendance_all %>%
  filter(!is.na(Email)) %>%
  distinct(institution) %>%
  pull(institution)

# Get unique institutions for attendees (those with "Attendee" in Role)
unique_institutions_attendees <- attendance_all %>%
  filter(Role == "Attendee", !is.na(Email)) %>%
  distinct(institution) %>%
  pull(institution)

attendance_by_institution <- attendance_all %>%
  filter(!is.na(Email)) %>%
  mutate(is_attendee = Role == "Attendee") %>%
  group_by(institution) %>%
  summarise(
    registrations = n_distinct(Email),
    attendances = n_distinct(Email[is_attendee]),
    .groups = "drop"
  ) %>%
  arrange(desc(registrations))

# Print the table
kable(attendance_by_institution, caption = "Registered and Attended Partipants by Email Domain")

# Registrants
registrants_summary <- attendance_all %>%
  filter(!is.na(Email)) %>%
  distinct(Email, institution) %>%
  mutate(is_acuk = str_detect(institution, "\\.ac\\.uk$")) %>%
  count(is_acuk, name = "registrants")

# Attendees
attendees_summary <- attendance_all %>%
  filter(Role == "Attendee", !is.na(Email)) %>%
  distinct(Email, institution) %>%
  mutate(is_acuk = str_detect(institution, "\\.ac\\.uk$")) %>%
  count(is_acuk, name = "attendees")

# Combine
combined_summary <- full_join(registrants_summary, attendees_summary, by = "is_acuk") %>%
  mutate(type = if_else(is_acuk, ".ac.uk domain", "Other domain")) %>%
  select(type, registrants, attendees)

# Print
kable(combined_summary, caption = "Registrants and Attendees from UK Instiutions")

# Get distinct registrants with non-.ac.uk domains
non_acuk_registrants <- attendance_all %>%
  filter(!is.na(Email)) %>%
  distinct(Email, institution) %>%
  filter(!str_detect(institution, "\\.ac\\.uk$"))

non_acuk_list <- non_acuk_registrants %>%
  distinct(institution) %>%
  arrange(institution) %>%
  rename(`Non-UK Institution Domains` = institution)

kable(non_acuk_list, caption = "List of Non-UK Institution Domains")