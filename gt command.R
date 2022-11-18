library(gt)
library(tidyverse)
library(glue)





# Take the `islands` dataset and use some
# dplyr functionality to obtain the ten
# biggest islands in the world
islands_tbl <- 
  tibble(
    name = names(islands),
    size = islands
  ) %>%
  arrange(desc(size)) %>%
  slice(1:10)

islands_tbl


# Display the table
islands_tbl


# Create a display table showing ten of
# the largest islands in the world
gt_tbl <- gt(islands_tbl)

# Show the gt Table
gt_tbl


# Make a display table with the `islands_tbl`
# table; put a heading just above the column labels
gt_tbl <- 
  gt_tbl %>%
  tab_header(
    title = "Large Landmasses of the World",
    subtitle = "The top ten largest are presented"
  )

# Show the gt Table
gt_tbl


# Use markdown for the heading's `title` and `subtitle` to
# add bold and italicized characters
gt(islands_tbl[1:2,]) %>%
  tab_header(
    title = md("**Large Landmasses of the World**"),
    subtitle = md("The *top two* largest are presented")
  )

# Display the `islands_tbl` data with a heading and
# two source notes
gt_tbl <- 
  gt_tbl %>%
  tab_source_note(
    source_note = "Source: The World Almanac and Book of Facts, 1975, page 406."
  ) %>%
  tab_source_note(
    source_note = md("Reference: McNeil, D. R. (1977) *Interactive Data Analysis*. Wiley.")
  )

# Show the gt table
gt_tbl



# Add footnotes (the same text) to two different
# cell; data cells are targeted with `data_cells()`
gt_tbl <- 
  gt_tbl %>%
  tab_footnote(
    footnote = "The Americas.",
    locations = cells_body(columns = name, rows = 3:4)
  )

# Show the gt table
gt_tbl


# Determine the row that contains the
# largest landmass ('Asia')
largest <- 
  islands_tbl %>% 
  arrange(desc(size)) %>%
  slice(1) %>%
  pull(name)

# Create two additional footnotes, using the
# `columns` and `where` arguments of `data_cells()`
gt_tbl <- 
  gt_tbl %>%
  tab_footnote(
    footnote = md("The **largest** by area."),
    locations = cells_body(
      columns = size,
      rows = name == largest
    )
  ) %>%
  tab_footnote(
    footnote = "The lowest by population.",
    locations = cells_body(
      columns = size,
      rows = size == min(size)
    )
  )

# Show the gt table
gt_tbl


# Create a gt table showing ten of the
# largest islands in the world; this
# time with a stub
gt_tbl <- 
  islands_tbl %>%
  gt(rowname_col = "name")

# Show the gt table
gt_tbl


# Generate a simple table with a stub
# and add a stubhead label
gt_tbl <- 
  gt_tbl %>%
  tab_stubhead(label = "landmass")

# Show the gt table
gt_tbl


# Display the `islands_tbl` data with a stub,
# a heading, source notes, and footnotes
gt_tbl <- 
  gt_tbl %>%
  tab_header(
    title = "Large Landmasses of the World",
    subtitle = "The top ten largest are presented"
  ) %>%
  tab_source_note(
    source_note = "Source: The World Almanac and Book of Facts, 1975, page 406."
  ) %>%
  tab_source_note(
    source_note = md("Reference: McNeil, D. R. (1977) *Interactive Data Analysis*. Wiley.")
  ) %>%
  tab_footnote(
    footnote = md("The **largest** by area."),
    locations = cells_body(
      columns = size, rows = largest
    )
  ) %>%
  tab_footnote(
    footnote = "The lowest by population.",
    locations = cells_body(
      columns = size, rows = contains("arc")
    )
  )

# Show the gt table
gt_tbl


# Create three row groups with the
# `tab_row_group()` function
gt_tbl <- 
  gt_tbl %>% 
  tab_row_group(
    label = "continent",
    rows = 1:6
  ) %>%
  tab_row_group(
    label = "country",
    rows = c("Australia", "Greenland")
  ) %>%
  tab_row_group(
    label = "subregion",
    rows = c("New Guinea", "Borneo")
  )

# Show the gt table
gt_tbl




# Modify the `airquality` dataset by adding the year
# of the measurements (1973) and limiting to 10 rows
airquality_m <- 
  airquality %>%
  mutate(Year = 1973L) %>%
  slice(1:10)

# Create a display table using the `airquality`
# dataset; arrange columns into groups
gt_tbl <- 
  gt(airquality_m) %>%
  tab_header(
    title = "New York Air Quality Measurements",
    subtitle = "Daily measurements in New York City (May 1-10, 1973)"
  ) %>%
  tab_spanner(
    label = "Time",
    columns = c(Year, Month, Day)
  ) %>%
  tab_spanner(
    label = "Measurement",
    columns = c(Ozone, Solar.R, Wind, Temp)
  )

# Show the gt table
gt_tbl
