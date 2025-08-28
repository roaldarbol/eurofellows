library(vroom)
library(dplyr)
library(lubridate)

# Write older entries over to a different file
today_date <- lubridate::today()
current_fellowships <- vroom::vroom("data/eu-fellowships.csv")                
past_fellowships <- current_fellowships |> 
  dplyr::filter(application_deadline < today_date)

if (nrow(past_fellowships) != 0){
  future_fellowships <- dplyr::anti_join(current_fellowships, past_fellowships)
  vroom::vroom_write(future_fellowships, "data/eu-fellowships.csv", delim = ";")
  vroom::vroom_write(past_fellowships, "data/eu-fellowships-past.csv", delim = ";", append = TRUE)
}