county_aggregation <- function(
    states = NULL,
    year = 2020,
    census_file = "dhc",
    variables,
    var_info,
    var_info_abb,
    final_vars,
    output_path = getwd(),
    crs = NULL,
    output_name,
    overwrite = FALSE
){
  # read in Census data at the county level
  raw_data <- suppressMessages(get_decennial(geography = "county",
                                             variables = variables,
                                             state = states,
                                             sumfile = census_file,
                                             year = year,
                                             cb = FALSE,
                                             geometry = TRUE,
                                             output = "wide"
  )) %>% st_transform(crs)

  raw_data <- raw_data %>% adjust_bins_tract_county()

  #use MARS ratios to allocate the OTHER, MULTI, and HIPI races to BenMAP's default four races: white, black, natamer, asian
  # HIPI is distributed to asian 100%, the rest use MARS ratios at national or state level
  raw_data <- raw_data %>% MARS_adjustments()

  # add COL (state FIPS) and ROW (county FIPS) columns
  out_data <- raw_data %>% mutate(Column = substr(GEOID, 1, 2), Row = substr(GEOID, 3, 5))

  # reshape to long table and drop geometry
  out_csv <- out_data %>%
    pivot_longer(cols = all_of(final_vars), names_to = "variable", values_to = "Population") %>%
    st_drop_geometry() %>%
    as.data.frame() %>%
    left_join(var_info, by = c("variable" = "name")) %>%
    select(-GEOID, -NAME, -variable) %>%
    mutate(Year = year)

  # generate weights
  if (year == 2020){
    out_weights <- out_data %>%
      st_drop_geometry() %>%
      as.data.frame() %>%
      mutate(P12I = rowSums(select(., matches("^P12I")), na.rm = TRUE),
             P12J = rowSums(select(., matches("^P12J")), na.rm = TRUE),
             P12K = rowSums(select(., matches("^P12K")), na.rm = TRUE),
             P12L = rowSums(select(., matches("^P12L")), na.rm = TRUE),
             P12P = rowSums(select(., matches("^P12P")), na.rm = TRUE),
             P12Q = rowSums(select(., matches("^P12Q")), na.rm = TRUE),
             P12R = rowSums(select(., matches("^P12R")), na.rm = TRUE),
             P12S = rowSums(select(., matches("^P12S")), na.rm = TRUE),
             countyID = substr(GEOID, 1, 5)) %>%
      select(-all_of(final_vars), -NAME) %>%
      county_pop_weight(variables = c("P12I", "P12J", "P12K", "P12L", "P12P", "P12Q", "P12R", "P12S"), year = year) %>%
      pivot_longer(cols = all_of(c("P12I", "P12J", "P12K", "P12L", "P12P", "P12Q", "P12R", "P12S")), names_to = "variable", values_to = "Value") %>%
      left_join(var_info_abb, by = c("variable" = "var")) %>%
      rename(TargetCol = Column, TargetRow = Row) %>%
      mutate(SourceCol = substr(countyID, 1, 2), SourceRow = substr(countyID, 3, 5)) %>%
      select(-c(countyID, variable))
  } else {
    out_weights <- out_data %>%
      st_drop_geometry() %>%
      as.data.frame() %>%
      mutate(P012A = rowSums(select(., matches("^P12A\\d{3}")), na.rm = TRUE),
             P012B = rowSums(select(., matches("^P12B\\d{3}")), na.rm = TRUE),
             P012C = rowSums(select(., matches("^P12C\\d{3}")), na.rm = TRUE),
             P012D = rowSums(select(., matches("^P12D\\d{3}")), na.rm = TRUE),
             P012E = rowSums(select(., matches("^P12E\\d{3}")), na.rm = TRUE),
             P012F = rowSums(select(., matches("^P12F\\d{3}")), na.rm = TRUE),
             P012G = rowSums(select(., matches("^P12G\\d{3}")), na.rm = TRUE)) %>%
      select(-all_of(variables), -NAME) %>%
      county_pop_weight(variables = c("P012A", "P012B", "P012C", "P012D", "P012E", "P012F", "P012G"), year = year) %>%
      pivot_longer(cols = all_of(c("P012A", "P012B", "P012C", "P012D", "P012E", "P012F", "P012G")), names_to = "variable", values_to = "Value") %>%
      left_join(var_info_abb, by = c("variable" = "var")) %>%
      rename(TargetCol = Column, TargetRow = Row) %>%
      mutate(SourceCol = substr(countyID, 1, 2), SourceRow = substr(countyID, 3, 5)) %>%
      select(-c(countyID, variable))
  }

  # set up output files
  outfile <- file.path(output_path, paste0(output_name, ".shp"))
  csv_outfile <- file.path(output_path, paste0(output_name, ".csv"))
  weights_outfile <- file.path(output_path, paste0(output_name, "_weights", ".csv"))

  # write output files
  if (file.exists(outfile)|| file.exists(csv_outfile) || file.exists(weights_outfile)){
    if (!overwrite){
      stop("One or more output files already exist and overwrite is set to FALSE. Canceling...")
    } else{
      message("One or more output files already exist and overwrite is set to TRUE. Overwriting...")
      st_write(out_data, outfile, delete_layer = overwrite, quiet = TRUE)
      write.table(out_csv, file = csv_outfile, row.names = FALSE, sep = ",")
      write.table(out_weights, file = weights_outfile, row.names = FALSE, sep = ",")
    }
  } else{
    st_write(out_data, outfile, quiet = TRUE)
    write.table(out_csv, file = csv_outfile, row.names = FALSE, sep = ",")
    write.table(out_weights, file = weights_outfile, row.names = FALSE, sep = ",")
  }
}
