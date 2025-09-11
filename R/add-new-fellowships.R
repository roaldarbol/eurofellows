library(vroom)
library(dplyr)
library(janitor)
library(lubridate)
library(stringr)
library(here)

here::i_am("R/add-new-fellowships.R")

new_data <- vroom::vroom(here::here("data", "european-fellowships-new.csv")) |>
  janitor::clean_names()

if (nrow(new_data) != 0){
  new_data <- new_data |> 
    dplyr::rename(fellowship_funder = funder,
           fellowship_url = link,
           fellowship_duration = duration,
           host_country = location,
           host_institution = institution,
           academic_field = academic_fields,
           connection_country = eligible_country,
           connection_type = connection_type,
           connection_in_or_not = nationalities_in_or_not,
           academic_field_category_major = major_field,
           academic_field_category_minor = minor_field,
           requires_mobility = mobility_required,
           requires_phd = ph_d_certificate_required,
           requires_publication = publication_required,
           minimum_years_post_phd = minimum_years_post_ph_d_2,
           maximum_years_post_phd = maximum_years_post_ph_d_2,
           contributor_name = your_name,
           contributor_url = profile_url,
           date_created = created_at) |> 
    dplyr::mutate(
      career_stage = as.factor(career_stage),
      host_country = dplyr::if_else(
        host_country == "Specific European country" | host_country == "Specific countries",
        eligible_countries,
        host_country
      ),
      connection_in_or_not = dplyr::if_else(
        is.na(connection_in_or_not), 
        NA, 
        connection_in_or_not
      ),
      academic_field_category_major = as.factor(academic_field_category_major),
      academic_field_category_minor = as.factor(academic_field_category_minor),
      application_deadline = lubridate::date(application_deadline),
      application_deadline = if_else(
        is.na(rolling_application_deadline), 
        lubridate::date(application_deadline),
        NA),
      date_created = lubridate::date(date_created),
      requires_mobility = dplyr::if_else(
        is.na(requires_mobility),
        "Not required",
        "Required"),
      requires_phd = dplyr::if_else(
        is.na(requires_phd),
        "Not required",
        "Required"),
      requires_publication = dplyr::if_else(
        is.na(requires_publication),
        "Not required",
        "Required"),
      minimum_years_post_phd = as.numeric(minimum_years_post_phd),
      maximum_years_post_phd = as.numeric(maximum_years_post_phd),
      contributor_name = as.character(contributor_name),
      contributor_url = as.character(contributor_url),
      comments = as.character(comments),
      date_updated = date_created
      ) |> 
    dplyr::select(
      -c(eligible_countries,
         limited_to_specific_institution, 
         only_available_within_specific_fields,
         rolling_application_deadline,
         minimum_years_post_ph_d,
         maximum_years_post_ph_d)) |> 
    dplyr::relocate(connection_country, .after = host_institution) |> 
    dplyr::relocate(fellowship_duration, .after = fellowship_url) |> 
    dplyr::relocate(connection_type, .before = connection_country) |> 
    dplyr::relocate(connection_in_or_not, .before = connection_country)
  
  attributes(new_data)$spec <- NULL
}

# Existing data
existing_data <- vroom::vroom(here::here("data", "european-fellowships.csv"))
  # mutate(eligible_fields = if_else(is.na(eligible_fields), "Any field", eligible_fields),
  #        eligible_nationalities = if_else(eligible_nationalities == "Any country worldwide", "Any country", eligible_nationalities)) |>
  # dplyr::rename(eligible_connection = eligible_nationalities,
  #               eligible_connection_in_or_not = eligible_nationalities_from_or_not) |> 
  # rowwise() |> 
  # mutate(
  #   requires_mobility = dplyr::if_else(
  #     !isTRUE(requires_mobility),
  #     "Not required",
  #     "Required"),
  #   requires_phd = dplyr::if_else(
  #     !isTRUE(requires_phd),
  #     "Not required",
  #     "Required"),
  #   requires_publication = dplyr::if_else(
  #     !isTRUE(requires_publication),
  #     "Not required",
  #     "Required"),
  # )

# Filter out duplicates
new_data <- dplyr::anti_join(new_data, existing_data, by = "id")

# Combine data
combined_data <- new_data |> 
  dplyr::bind_rows(existing_data) |>
  dplyr::arrange(application_deadline)
  # mutate(connection_country = if_else(is.na(connection_country), "Not required", connection_country))
  # rename(host_country = eligible_host_location,
  #        host_institution = eligible_institution,
  #        connection_type = eligible_connection_type,
  #        connection_in_or_not = eligible_connection_in_or_not,
  #        connection_country = eligible_connection,
  #        academic_field = eligible_fields,
  #        academic_field_category_major = field_category_major,
  #        academic_field_category_minor = field_category_minor)
  # mutate(minimum_years_post_phd = if_else(is.na(minimum_years_post_phd), 
  #                                         0,
  #                                         minimum_years_post_phd))
  # mutate(
  #   eligible_connection_in_or_not = case_when(
  #     is.na(eligible_connection_in_or_not) ~ "In",
  #     eligible_connection_in_or_not == "From" ~ "In",
  #     .default = "Not in"
  #   ),
  #   eligible_host_location = eligible_host_location |> 
  #     stringr::str_replace_all("(,(?=\\S)|:)", ", ") |> 
  #     stringr::str_to_title(),
  #   eligible_fields = eligible_fields |> 
  #     stringr::str_replace_all("(,(?=\\S)|:)", ", ") |> 
  #     stringr::str_to_title(),
  #   eligible_connection = eligible_connection |> 
  #     stringr::str_replace_all("(,(?=\\S)|:)", ", ") |> 
  #     stringr::str_to_title(),
  #   eligible_connection = if_else(
  #     eligible_connection == "Any Country",
  #     NA,
  #     eligible_connection
  #   ),
  #   eligible_connection_in_or_not = if_else(
  #     is.na(eligible_connection),
  #     NA,
  #     eligible_connection_in_or_not
  #   ),
  #   eligible_fields = if_else(
  #     eligible_fields == "Any Field",
  #     NA,
  #     eligible_fields
  #   )
  # ) 


# Update the file
vroom::vroom_write(combined_data, here::here("data", "european-fellowships.csv"), delim = ";")
