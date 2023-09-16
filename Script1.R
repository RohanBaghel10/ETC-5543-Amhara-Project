library(tidyverse)

data <- read_csv(here::here("data/famillyplanning_maternal_child_health_products.csv"))

unique_regions <- unique(tsibble_data$region)
unique_items <- unique(tsibble_data$item)

create_autoplot <- function(data, item_name) {
  data_filtered <- data %>% 
    filter(item == item_name)
  
  autoplot(data_filtered) +
    facet_wrap(~ region, scales = "free") +
    theme(legend.position = "none")
}

create_autoplot(tsibble_data, "Amoxicillin - 125mg - Tablet (Dispersible)")

create_autoplot(tsibble_data, "Amoxicillin - 250mg - Tablet (Dispersible)")

create_autoplot(tsibble_data,"Chlorhexidine digluconate - 7.1% ( which contains 4% Chlorhexidine) - Gel" )


