#bradyn test


# here you can use the mutate to create a new column for the type of thing you want to look at -- maybe like intervention or something?
new_data <- data %>%
  mutate(intervention_type = case_when(
    latitude >= -31.24 & latitude <= 34.2 ~ "levee_above",
    latitude >= -31.24 & latitude <= 34.2 ~ "levee_below",
    TRUE ~ "no intervention"
  ))

# then do the same thing for timing of the interventions, before and after urbanization or whatever
new_data <- new_data %>%
  mutate(control = case_when(
    date >= as.Date("XXXX=XX-XX") & date <= as.Date("XXXX-XX-XX") ~ "pre_urban",
    date >= as.Date("XXXX=XX-XX") & date <= as.Date("XXXX-XX-XX") ~ "post_urban",
    TRUE ~ "no intervention"
  ))