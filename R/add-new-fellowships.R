library(vroom)
library(dplyr)
library(janitor)
library(lubridate)
library(here)

here::i_am("R/add-new-fellowships.R")

new_data <- vroom::vroom(here::here("data", "european-fellowships-new.csv")) |>
  janitor::clean_names()

if (nrow(new_data) != 0){
  new_data <- new_data |> 
    dplyr::rename(fellowship_funder = funder,
           fellowship_url = link,
           fellowship_duration = duration,
           eligible_host_location = location,
           eligible_institution = institution,
           eligible_fields = academic_fields,
           field_category_major = major_field,
           field_category_minor = minor_field,
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
      eligible_host_location = dplyr::if_else(
        eligible_host_location == "Specific European countries",
        eligible_countries,
        eligible_host_location
      ),
      eligible_nationalities = dplyr::if_else(
        eligible_nationalities_24 == "Specific European countries", 
        eligible_nationalities_25, 
        eligible_nationalities_24),
      field_category_major = as.factor(field_category_major),
      field_category_minor = as.factor(field_category_minor),
      application_deadline = lubridate::date(application_deadline),
      application_deadline = if_else(
        is.na(rolling_application_deadline), 
        lubridate::date(application_deadline),
        NA),
      date_created = lubridate::date(date_created),
      requires_mobility = dplyr::if_else(
        isTRUE(requires_mobility),
        TRUE,
        FALSE),
      requires_phd = dplyr::if_else(
        isTRUE(requires_phd),
        TRUE,
        FALSE),
      requires_publication = dplyr::if_else(
        isTRUE(requires_publication),
        TRUE,
        FALSE
      ),
      minimum_years_post_phd = as.numeric(minimum_years_post_phd),
      maximum_years_post_phd = as.numeric(maximum_years_post_phd),
      contributor_name = as.character(contributor_name),
      contributor_url = as.character(contributor_url),
      comments = as.character(comments),
      date_updated = date_created) |> 
    dplyr::select(
      -c(eligible_countries,
         limited_to_specific_institution, 
         only_available_within_specific_fields,
         rolling_application_deadline,
         minimum_years_post_ph_d,
         maximum_years_post_ph_d,
         eligible_nationalities_24,
         eligible_nationalities_25)) |> 
    dplyr::relocate(eligible_nationalities, .after = eligible_institution) |> 
    dplyr::relocate(fellowship_duration, .after = fellowship_url)
  
  attributes(new_data)$spec <- NULL
}

# Update the file
vroom::vroom_write(new_data, here::here("data", "european-fellowships.csv"), delim = ";", append = TRUE)
