install.packages("fedstatAPIr")
install.packages("devtools")


library(fedstatAPIr)
library(tidyverse)
library(devtools)


??fedstatapir

data <- fedstat_data_load_with_filters(indicator_id = '31074')

data

data_ids <- fedstat_get_data_ids("31074")


d1 <- fedstat_indicators_names_database()


view(fedstat_indicators_names_database)


fedstat_indicators_names_database %>% 
  select(name) %>% 
  grepl('граждан', name)


fedstat_indicators_names_database %>% 
  group_by(department) %>% 
  count()


## Not run:
# Get data filters identificators for week prices
# standardize names for DVFO and extract week numbers
# filter the data_ids to get data for week 21 and 22 of 2021
# for all goods and services for Russian Federation
data_ids_filtered <- fedstat_get_data_ids("37426") %>%
  fedstat_get_data_ids_special_cases_handle(
    filter_value_title_alias_lookup_table = data.frame(
      "filter_value_title" = "Dalnevostochnyj federalnyj okrug ( s 03.11.2018)",
      "filter_value_title_alias" = "Dalnevostochnyj federalnyj okrug"
    )
  ) %>%
  fedstat_data_ids_filter(
    filters = list(
      "Territory" = "Russian Federation",
      "Year" = "2021",
      "Period" = c(21, 22),
      "Types of goods and services" = "*"
    )
  )


# In this example names for Far Eastern Federal District are latinized for CRAN
# Not actual filter field titles and filter values titles because of ASCII requirement for CRAN
## End(Not run)