#' Aggregate block-level population data from the decennial Census to a chosen grid definition
#'
#' @description
#' This function allows users to aggregate block-level population data from the decennial U.S. Census to a chosen grid definition. Users may provide a grid in the form of a shapefile or use the Census's county or tract grids. Other inputs allow the user control over allocation method, Census year and variables to use, and included U.S. states.
#'
#' @param mode A character value indicating the type of grid to use for aggregation, one of `c("shapefile", "county", "tract")`. Defaults to using user-specified shapefile (`mode = "shapefile"`). `"county"` and `"tract"` options instead use U.S. Census-defined county and tract shapes, respectively.
#' @param grid_path A character value representing the file path to the shapefile to use. Only used when `mode = "shapefile"`.
#' @param grid_name A character value representing the name of the shapefile to use. Do not include file extension. Only used when `mode = "shapefile"`.
#' @param year A numeric value indicating the year of the desired Census data. Defaults to `2020`, currently the only supported decennial Census year.
#' @param variables A character vector indicating the Census variables to include. Use [tidycensus::load_variables()] to get information on Census variables. Defaults to BenMAP input variables binned by age, sex, race, and ethnicity.
#' @param area_weight A `TRUE/FALSE` value indicating whether to use an area weighting approach (block population data are allocated to grid cells based on area proportion, `area_weight = TRUE`) or a centroid approach (block population data are allocated to grid cells based on block centroids, `area_weight = FALSE`) to allocate block data to the chosen grid definition. Defaults to the area weighting approach (`area_weight = TRUE`). Only used when `mode = "county"` or `"tract"`.
#' @param states A character value or character vector of state postal abbreviations indicating which states to include. If no input is provided, all CONUS states are included.
#' @param output_path A character value representing the file path to the output folder. Defaults to the current working directory (`output_path = getwd()`)
#' @param output_name A character value representing the name to use for output files.
#' @param overwrite A `TRUE/FALSE` value indicating whether to overwrite existing output files. Defaults to `FALSE`.
#'
#' @return [PopGrid_v2::run_aggregation()] does not return a value, but instead saves three output files: a shapefile of the selected grid with population data, a CSV with the number of people in each grid cell for each of the race-ethnicity-gender-age demographic groups included in BenMAP, and a CSV with the fraction of the total population in each of the eight race-ethnic groups that comes from each U.S. county.
#' @export
#'
#' @examples
#' run_aggregation(mode = "county",
#'  year = 2020,
#'  states = c("MA", "RI", "CT", "ME", "NH", "VT"),
#'  output_name = "New England")

run_aggregation <- function(
    mode = "shapefile",
    grid_path = NULL,
    grid_name = NULL,
    year = 2020,
    variables = NULL,
    area_weight = TRUE,
    states = NULL,
    output_path = getwd(),
    output_name,
    overwrite = FALSE,
    shiny = FALSE
){
  t1 <- Sys.time()
  message(paste0("Starting at ", t1))

  if (Sys.getenv("CENSUS_API_KEY") == ""){
    message("Please set a Census API key. If you do not have one already, you can request one from https://api.census.gov/data/key_signup.html and set it in your .Renviron file by running by running census_api_key('my_key_here', install = TRUE, overwrite = TRUE), with the value of your key replacing the text my_key_here. You only need to do this one time. Restart R or reload your environment with readRenviron('~/.Renviron') for this change to take effect.")
    return()
  }

  # message user about which states are being included
  if (is.null(states)){
    states <- state.abb[!state.abb %in% c("AK", "HI")] %>% append("DC")
    message("Using all CONUS states")
  } else {
    message("Using provided list of states")
  }

  # set summary file for the selected year
  if (year == 2020){
    census_file <- "dhc"
  } else {
    census_file = "sf1"
  }

  # set CRS to BenMAP default input
  NA_eq <- st_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

  # load Census variable information and filter to just input variables
  if (is.null(variables)){
    message("Using default BenMAP variables")
    all_variables <- load_variables(year = year, dataset = census_file)
    if (year == 2020){
      if (mode == "shapefile"){
        variables <- all_variables[grepl("P12[I-V]", all_variables$name),] %>% filter(!substr(name, nchar(name) - 2, nchar(name)) %in% c("01N", "02N", "26N"))
        variables <- variables$name %>% unique
        tract_vars <- all_variables[grepl("PCT12[A-G]", all_variables$name),] %>% filter(substr(name, nchar(name) - 3, nchar(name)) %in% c("003N", "004N", "005N", "006N", "007N", "107N", "108N", "109N", "110N", "111N"))
        tract_vars <- tract_vars$name %>% unique
      } else {
          block_vars <- all_variables[grepl("P12[I-V]", all_variables$name),] %>% filter(!substr(name, nchar(name) - 2, nchar(name)) %in% c("01N", "02N", "26N"))
          block_vars <- block_vars$name %>% unique
          other_vars <- all_variables[grepl("PCT12[A-G I-O]", all_variables$name),] %>% filter(substr(name, nchar(name) - 3, nchar(name)) %in% c("003N", "107N"))
          other_vars <- other_vars$name %>% unique
          variables  <- c(block_vars, other_vars)
      }
    } else {
      variables <- all_variables[grepl("P012[A-G]", all_variables$name),] %>% filter(!substr(name, nchar(name) - 1, nchar(name)) %in% c("01", "02", "26"))
      variables <- variables$name %>% unique
    }
  } else {
    message("Using provided list of Census variables")
  }

  drop_patterns <- c("009", "010", "019", "021", "033", "034", "043", "045", "007", "031", "PCT","P12M","P12N","P12O","P12T","P12U","P12V") %>% paste(collapse = "|")
  var_letters <- c("I", "J", "K", "L", "P", "Q", "R", "S")
  new_digits <- c("002", "026")
  combinations <- expand.grid(var_letters, new_digits, stringsAsFactors = FALSE)
  new_patterns <- c(paste0("P12", combinations$Var1, "_", combinations$Var2, "N"))
  final_vars <- c(variables[!grepl(drop_patterns, variables)], new_patterns) # BenMAP defaults 4 races, 2 ethnicity, 2 gender, 19 ages

  var_info <- var_information(vars = final_vars, year = year, dataset = census_file)
  var_info_abb <- var_info %>%
    mutate(var = substr(name, 1, 4)) %>%
    select(var, Race, Ethnicity) %>%
    distinct()

  # if user has selected either county or tract mode, run helper functions specific to those grid definitions
  if (mode == "county"){
    message("Aggregating population data to U.S. Census County level")
    cat(paste0("PopGrid_v2 run started at ", t1),
        "Inputs used:",
        paste0("mode = ", mode),
        paste0("year = ", as.character(year)),
        paste0("states = ", states),
        paste0("output_path = ", output_path),
        paste0("output_name = ", output_name),
        paste0("overwrite = ", overwrite), sep = "\n", file = file.path(output_path, paste0(output_name, "_audit_trail.txt")))
    county_aggregation(states = states, year = year, census_file = census_file, variables = variables, var_info = var_info, var_info_abb = var_info_abb, final_vars = final_vars, output_path = output_path, crs = NA_eq, output_name = output_name, overwrite = overwrite)
    cat(paste0("Run completed at ", Sys.time()), file = file.path(output_path, paste0(output_name, "_audit_trail.txt")), append = TRUE)
    message("Aggregation complete, outputs saved")
    return()
  } else if (mode == "tract"){
    message("Aggregating population data to U.S. Census Tract level")
    cat(paste0("PopGrid_v2 run started at ", t1),
        "Inputs used:",
        paste0("mode = ", mode),
        paste0("year = ", as.character(year)),
        paste0("states = ", states),
        paste0("output_path = ", output_path),
        paste0("output_name = ", output_name),
        paste0("overwrite = ", overwrite), sep = "\n", file = file.path(output_path, paste0(output_name, "_audit_trail.txt")))
    tract_aggregation(states = states, year = year, census_file = census_file, variables = variables, var_info = var_info, var_info_abb = var_info_abb, final_vars = final_vars, output_path = output_path, crs = NA_eq, output_name = output_name, overwrite = overwrite)
    cat(paste0("Run completed at ", Sys.time()), file = file.path(output_path, paste0(output_name, "_audit_trail.txt")), append = TRUE)
    message("Aggregation complete, outputs saved")
    return()
  }

  # if user has selected shapefile mode, continue
  # read in county shapes
  county_state <- counties(state = states,
                           cb = FALSE,
                           year = year,
                           class = "sf"
  ) %>% st_transform(NA_eq)
  state_FIPS <- county_state$STATEFP %>% unique

  # read in user grid and convert to correct CRS
  message("Aggregating population data to user-provided grid definition")
  user_grid <- st_read(dsn = grid_path, layer = grid_name) %>% st_transform(NA_eq)

  # check that user_grid has columns with names "COL" and "ROW"
  if (!("COL" %in% names(user_grid) && "ROW" %in% names(user_grid))){
    stop("Provided grid must have the columns COL and ROW and the value pairs must be unique. Canceling...")
  }
    user_grid <- user_grid %>% rename(Column = COL,Row = ROW)

  # set up output files
  edge_outfile <- file.path(output_path, "edge_merge.shp")
  interior_outfile <- file.path(output_path, paste0(output_name, ".shp"))
  csv_outfile <- file.path(output_path, paste0(output_name, ".csv"))
  edge_weights_outfile <- file.path(output_path, "edge_weights.csv")
  weights_outfile <- file.path(output_path, paste0(output_name, "_weights", ".csv"))
  overwrite_check <- file.path(output_path, "overwrite_check")
  audit_trail_file <- file.path(output_path, paste0(output_name, "_audit_trail.txt"))
  if (file.exists(overwrite_check)){
    file.remove(overwrite_check)
  }

  # iterate across states and counties
  state_FIPS %>% lapply(function(state){
    if (shiny == TRUE){
      incProgress(1/length(state_FIPS), detail = paste0("Working on state ", state, " (FIPS)"))
    }

    state_counties <- county_state %>% filter(STATEFP == state)
    county_names <- state_counties$COUNTYFP %>% unique

    # set up cluster to do parallel processing
    my_cluster <- makeCluster(availableCores()/2, type = "PSOCK")
    clusterEvalQ(my_cluster, {
      library(PopGrid_v2)
    })
    message(paste0("state = ", state))

    county_names %>% parLapply(my_cluster, ., function(county, retries){
      county_parallel_internal <- function(county, retries){
        tryCatch({
          # read in block-level data for the county
          raw_block_data <- suppressMessages(get_decennial(geography = "block",
                                                           variables = variables,
                                                           state = state,
                                                           county = county,
                                                           year = year,
                                                           sumfile = census_file,
                                                           output = "wide",
                                                           cb = FALSE,
                                                           geometry = TRUE
          )) %>% st_transform(NA_eq)

          raw_block_data <- raw_block_data %>% adjust_bins(year = year, state = state, county = county, block_variables = variables, tract_variables = tract_vars, census_file = census_file)

          #use MARS ratios to allocate the OTHER, MULTI, and HIPI races to BenMAP's default four races: white, black, natamer, asian
          # HIPI is distributed to asian 100%, the rest use MARS ratios at national or state level
          raw_block_data <- raw_block_data %>% MARS_adjustments()

          # grab county shape and subset user grid to cells that overlap county
          county_shape <- county_state %>% filter(STATEFP == state & COUNTYFP == county)
          county_grid <- user_grid[county_shape,]

          # run spatial analysis using helper functions based on user's selected allocation method
          if (area_weight){
            dissolved <- allocate_area_weight(county_grid = county_grid, pop_data = raw_block_data, variables = final_vars, year = year)
            weights <- dissolved$weight
            dissolved <- dissolved$dissolved
          } else {
            dissolved <- allocate_centroids(county_grid = county_grid, pop_data = raw_block_data, variables = final_vars, year = year)
            weights <- dissolved$weight
            dissolved <- dissolved$dissolved
          }

          # grab the grid cells that overlap the edge of the county, these will be dissolved again later to remove county boundaries
          edge_buffer <- county_shape %>% st_boundary %>% st_buffer(dist = 0.01)
          diss_edge <- dissolved[edge_buffer,]
          weights_edge <- weights[edge_buffer,] %>% st_drop_geometry() %>% as.data.frame()

          # store gridIDs for edge cells
          edge_codes <- diss_edge$gridID %>% unique
          weights_edge_codes <- weights_edge$gridID %>% unique

          # grab the grid cells that don't border the edge of the county, these are ready to go and can be saved
          diss_interior <- dissolved %>% filter(!(gridID %in% edge_codes))
          interior_csv <- diss_interior %>%
            pivot_longer(cols = all_of(final_vars), names_to = "variable", values_to = "Population") %>%
            st_drop_geometry() %>%
            as.data.frame() %>%
            left_join(var_info, by = c("variable" = "name")) %>%
            select(-gridID, -variable) %>%
            mutate(Year = year)
          if (year == 2020){
            weights_interior <- weights %>%
              filter(!(gridID %in% edge_codes)) %>%
              st_drop_geometry() %>%
              as.data.frame() %>%
              mutate(P12I = rowSums(select(., matches("^P12I")), na.rm = TRUE),
                     P12J = rowSums(select(., matches("^P12J")), na.rm = TRUE),
                     P12K = rowSums(select(., matches("^P12K")), na.rm = TRUE),
                     P12L = rowSums(select(., matches("^P12L")), na.rm = TRUE),
                     P12P = rowSums(select(., matches("^P12P")), na.rm = TRUE),
                     P12Q = rowSums(select(., matches("^P12Q")), na.rm = TRUE),
                     P12R = rowSums(select(., matches("^P12R")), na.rm = TRUE),
                     P12S = rowSums(select(., matches("^P12S")), na.rm = TRUE)) %>%
              select(-all_of(final_vars)) %>%
              county_pop_weight(variables = c("P12I", "P12J", "P12K", "P12L", "P12P", "P12Q", "P12R", "P12S"), year = year) %>%
              pivot_longer(cols = all_of(c("P12I", "P12J", "P12K", "P12L", "P12P", "P12Q", "P12R", "P12S")), names_to = "variable", values_to = "Value") %>%
              left_join(var_info_abb, by = c("variable" = "var")) %>%
              rename(TargetCol = Column, TargetRow = Row) %>%
              mutate(SourceCol = substr(countyID, 1, 2), SourceRow = substr(countyID, 3, 5)) %>%
              select(-c(countyID, gridID, variable))
          } else{
            weights_interior <- weights %>%
              filter(!(gridID %in% edge_codes)) %>%
              st_drop_geometry() %>%
              as.data.frame() %>%
              mutate(P012A = rowSums(select(., matches("^P12A\\d{3}")), na.rm = TRUE),
                     P012B = rowSums(select(., matches("^P12B\\d{3}")), na.rm = TRUE),
                     P012C = rowSums(select(., matches("^P12C\\d{3}")), na.rm = TRUE),
                     P012D = rowSums(select(., matches("^P12D\\d{3}")), na.rm = TRUE),
                     P012E = rowSums(select(., matches("^P12E\\d{3}")), na.rm = TRUE),
                     P012F = rowSums(select(., matches("^P12F\\d{3}")), na.rm = TRUE),
                     P012G = rowSums(select(., matches("^P12G\\d{3}")), na.rm = TRUE)) %>%
              select(-all_of(variables)) %>%
              county_pop_weight(variables = c("P12A", "P12B", "P12C", "P12D", "P12E", "P12F", "P12G"), year = year) %>%
              pivot_longer(cols = all_of(c("P12A", "P12B", "P12C", "P12D", "P12E", "P12F", "P12G")), names_to = "variable", values_to = "Value") %>%
              left_join(var_info_abb, by = c("variable" = "var")) %>%
              rename(TargetCol = Column, TargetRow = Row) %>%
              mutate(SourceCol = substr(countyID, 1, 2), SourceRow = substr(countyID, 3, 5)) %>%
              select(-c(countyID, gridID, variable))
          }

          diss_edge <- diss_edge %>% select(-gridID)
          diss_interior <- diss_interior %>% select(-gridID)

          # write output files
          lock_file <- file.path(output_path, "lock.txt")
          l1 <- lock(lock_file, exclusive = TRUE)
          if (which(state_FIPS == state) == 1 & !file.exists(overwrite_check)){
            if (file.exists(edge_outfile) || file.exists(interior_outfile) || file.exists(csv_outfile) || file.exists(weights_outfile)){
              if (!overwrite){
                stop("One or more output files already exist and overwrite is set to FALSE. Canceling...", call. = FALSE)
              } else{
                message("One or more output files already exist and overwrite is set to TRUE. Overwriting...")
                file.create(overwrite_check)
                st_write(diss_edge, edge_outfile, delete_layer = overwrite, quiet = TRUE)
                st_write(diss_interior, interior_outfile, delete_layer = overwrite, quiet = TRUE)
                write.table(interior_csv, file = csv_outfile, row.names = FALSE, sep = ",")
                write.table(weights_interior, file = weights_outfile, row.names = FALSE, sep = ",")
                write.table(weights_edge, file = edge_weights_outfile, row.names = FALSE, sep = ",")
                cat(paste0("PopGrid_v2 run started at ", t1),
                    "Inputs used:",
                    paste0("mode = ", mode),
                    paste0("grid_path = ", grid_path),
                    paste0("grid_name = ", grid_name),
                    paste0("year = ", as.character(year)),
                    paste0("area_weight = ", area_weight),
                    paste0("states = ", states),
                    paste0("output_path = ", output_path),
                    paste0("output_name = ", output_name),
                    paste0("overwrite = ", overwrite), sep = "\n", file = audit_trail_file)
              }
            } else{
              file.create(overwrite_check)
              st_write(diss_edge, edge_outfile)
              st_write(diss_interior, interior_outfile)
              write.table(interior_csv, file = csv_outfile, row.names = FALSE, sep = ",")
              write.table(weights_interior, file = weights_outfile, row.names = FALSE, sep = ",")
              write.table(weights_edge, file = edge_weights_outfile, row.names = FALSE, sep = ",")
              cat(paste0("PopGrid_v2 run started at ", t1),
                  "Inputs used:",
                  paste0("mode = ", mode),
                  paste0("grid_path = ", grid_path),
                  paste0("grid_name = ", grid_name),
                  paste0("year = ", as.character(year)),
                  paste0("area_weight = ", area_weight),
                  paste0("states = ", states),
                  paste0("output_path = ", output_path),
                  paste0("output_name = ", output_name),
                  paste0("overwrite = ", overwrite), sep = "\n", file = audit_trail_file)
            }
          } else{
            st_write(diss_edge, edge_outfile, append = TRUE, quiet = TRUE)
            st_write(diss_interior, interior_outfile, append = TRUE, quiet = TRUE)
            write.table(interior_csv, file = csv_outfile, append = TRUE, col.names = FALSE, row.names = FALSE, sep = ",")
            write.table(weights_interior, file = weights_outfile, append = TRUE, col.names = FALSE, row.names = FALSE, sep = ",")
            write.table(weights_edge, file = edge_weights_outfile, append = TRUE, col.names = FALSE, row.names = FALSE, sep = ",")
          }
          unlock(l1)
        },
        error = function(err){
          if (retries <= 4){
            county_parallel_internal(county = county, retries = retries + 1)
          }
        }
        )
      }

      county_parallel_internal(county = county, retries = retries)
    }, retries = 0)
    stopCluster(my_cluster)
  })

  # read in edges
  full_edge <- st_read(dsn = output_path, layer = "edge_merge")
  edge_weights <- read_csv(edge_weights_outfile)

  # dissolve edges
  dissolved_edge <- full_edge %>%
    group_by(Column, Row) %>%
    summarize(across(all_of(final_vars), ~sum(., na.rm = TRUE)))
  rm(full_edge)

  edge_csv <- dissolved_edge %>%
    pivot_longer(cols = all_of(final_vars), names_to = "variable", values_to = "Population") %>%
    st_drop_geometry() %>%
    as.data.frame() %>%
    left_join(var_info, by = c("variable" = "name")) %>%
    mutate(Year = year) %>%
    select(-variable)
  if (year == 2020){
    edge_weights <- edge_weights %>%
      mutate(P12I = rowSums(select(., matches("^P12I")), na.rm = TRUE),
             P12J = rowSums(select(., matches("^P12J")), na.rm = TRUE),
             P12K = rowSums(select(., matches("^P12K")), na.rm = TRUE),
             P12L = rowSums(select(., matches("^P12L")), na.rm = TRUE),
             P12P = rowSums(select(., matches("^P12P")), na.rm = TRUE),
             P12Q = rowSums(select(., matches("^P12Q")), na.rm = TRUE),
             P12R = rowSums(select(., matches("^P12R")), na.rm = TRUE),
             P12S = rowSums(select(., matches("^P12S")), na.rm = TRUE),) %>%
      select(-all_of(final_vars)) %>%
      county_pop_weight(variables = c("P12I", "P12J", "P12K", "P12L", "P12P", "P12Q", "P12R", "P12S"), year = year) %>%
      pivot_longer(cols = all_of(c("P12I", "P12J", "P12K", "P12L", "P12P", "P12Q", "P12R", "P12S")), names_to = "variable", values_to = "Value") %>%
      left_join(var_info_abb, by = c("variable" = "var")) %>%
      rename(TargetCol = Column, TargetRow = Row) %>%
      mutate(SourceCol = substr(countyID, 1, 2), SourceRow = substr(countyID, 3, 5)) %>%
      select(-c(countyID, gridID, variable))
  } else{
    edge_weights <- edge_weights %>%
      mutate(P012A = rowSums(select(., matches("^P12A\\d{3}")), na.rm = TRUE),
             P012B = rowSums(select(., matches("^P12B\\d{3}")), na.rm = TRUE),
             P012C = rowSums(select(., matches("^P12C\\d{3}")), na.rm = TRUE),
             P012D = rowSums(select(., matches("^P12D\\d{3}")), na.rm = TRUE),
             P012E = rowSums(select(., matches("^P12E\\d{3}")), na.rm = TRUE),
             P012F = rowSums(select(., matches("^P12F\\d{3}")), na.rm = TRUE),
             P012G = rowSums(select(., matches("^P12G\\d{3}")), na.rm = TRUE)) %>%
      select(-all_of(variables)) %>%
      county_pop_weight(variables = c("P12A", "P12B", "P12C", "P12D", "P12E", "P12F", "P12G"), year = year) %>%
      pivot_longer(cols = all_of(c("P12A", "P12B", "P12C", "P12D", "P12E", "P12F", "P12G")), names_to = "variable", values_to = "Value") %>%
      left_join(var_info_abb, by = c("variable" = "var")) %>%
      rename(TargetCol = Column, TargetRow = Row) %>%
      mutate(SourceCol = substr(countyID, 1, 2), SourceRow = substr(countyID, 3, 5)) %>%
      select(-c(countyID, gridID, variable))
  }

  # write dissolved edges to final output files
  st_write(dissolved_edge, interior_outfile, append = TRUE)
  write.table(edge_csv, file = csv_outfile, append = TRUE, col.names = FALSE, row.names = FALSE, sep = ",")
  write.table(edge_weights, file = weights_outfile, append = TRUE, col.names = FALSE, row.names = FALSE, sep = ",")

  # delete intermediate output files
  edge_delete <- file.path(output_path, paste0("edge_merge"))
  file.remove(paste0(edge_delete, c(".shp", ".shx", ".dbf", ".prj")))
  file.remove(edge_weights_outfile)
  file.remove(file.path(output_path, "lock.txt"))
  file.remove(file.path(output_path, "overwrite_check"))

  t2 <- Sys.time() - t1
  cat(paste0("Run completed at ", Sys.time()), file = audit_trail_file, append = TRUE)
  print(t2)
}

