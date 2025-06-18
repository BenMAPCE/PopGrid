var_information <- function(
  vars = NULL,
  year = NULL,
  dataset = NULL
){
  var_info <- load_variables(year = year, dataset = dataset) %>% filter(name %in% vars)
  var_info <- var_info %>%
    mutate(
      Race = case_when(
        substr(name, 4, 4) %in% c("I", "P") ~ "WHITE",
        substr(name, 4, 4) %in% c("J", "Q") ~ "BLACK",
        substr(name, 4, 4) %in% c("K", "R") ~ "NATAMER",
        substr(name, 4, 4) %in% c("L", "S") ~ "ASIAN",
        .default = NA
      ),
      Ethnicity = case_when(
        substr(name, 4, 4) %in% c("I", "J", "K", "L") ~ "NON-HISPANIC",
        substr(name, 4, 4) %in% c("P", "Q", "R", "S") ~ "HISPANIC",
        .default = NA
      ),
      Gender = case_when(
        as.numeric(substr(name, 7, 8)) <= 25 ~ "MALE",
        as.numeric(substr(name, 7, 8)) >  25 ~ "FEMALE",
        .default = NA
      ),
      AgeRange = case_when(
        substr(name, 7, 8) %in% c("02", "26") ~ "0TO0",
        substr(name, 7, 8) %in% c("03", "27") ~ "1TO4",
        substr(name, 7, 8) %in% c("04", "28") ~ "5TO9",
        substr(name, 7, 8) %in% c("05", "29") ~ "10TO14",
        substr(name, 7, 8) %in% c("06", "30") ~ "15TO19",
        substr(name, 7, 8) %in% c("08", "32") ~ "20TO24",
        substr(name, 7, 8) %in% c("11", "35") ~ "25TO29",
        substr(name, 7, 8) %in% c("12", "36") ~ "30TO34",
        substr(name, 7, 8) %in% c("13", "37") ~ "35TO39",
        substr(name, 7, 8) %in% c("14", "38") ~ "40TO44",
        substr(name, 7, 8) %in% c("15", "39") ~ "45TO49",
        substr(name, 7, 8) %in% c("16", "40") ~ "50TO54",
        substr(name, 7, 8) %in% c("17", "41") ~ "55TO59",
        substr(name, 7, 8) %in% c("18", "42") ~ "60TO64",
        substr(name, 7, 8) %in% c("20", "44") ~ "65TO69",
        substr(name, 7, 8) %in% c("22", "46") ~ "70TO74",
        substr(name, 7, 8) %in% c("23", "47") ~ "75TO79",
        substr(name, 7, 8) %in% c("24", "48") ~ "80TO84",
        substr(name, 7, 8) %in% c("25", "49") ~ "85UP",
        .default = NA
      )
    ) %>%
    select(-c(concept, label))
  return(var_info)
}
