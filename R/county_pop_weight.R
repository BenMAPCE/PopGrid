county_pop_weight <- function(
    df,
    variables,
    year = 2010
){
  # dissolve to user grid to get total population for each variable in each grid cell
  grid_pop <- df %>%
    group_by(Column, Row) %>%
    summarize(across(all_of(variables), ~sum(.x, na.rm = TRUE), .names = "grid_{.col}"))

  # join grid cell totals back to original data frame
  weight_data <- df %>%
    left_join(grid_pop, by = c("Column" = "Column", "Row" = "Row"))

  # divide grid cell/county values by total grid cell population for each variable to get county proportions
  weight_data <- weight_data %>%
    mutate(across(all_of(variables), ~ ifelse(get(paste0("grid_", cur_column())) == 0, 0, . / get(paste0("grid_", cur_column()))), .names = "{.col}"), .keep = "unused") %>%
    mutate(Year = year)

  return(weight_data)
}
