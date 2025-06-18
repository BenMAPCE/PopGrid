#' @export

adjust_bins_tract_county <- function(
    data = NULL
){

  # get 0-1 age bin
  data <- data %>% mutate(
    P12I_002N = PCT12I_003N,
    P12J_002N = PCT12J_003N,
    P12K_002N = PCT12K_003N,
    P12L_002N = PCT12L_003N,
    P12M_002N = PCT12M_003N,
    P12N_002N = PCT12N_003N,
    P12O_002N = PCT12O_003N,
    P12I_026N = PCT12I_107N,
    P12J_026N = PCT12J_107N,
    P12K_026N = PCT12K_107N,
    P12L_026N = PCT12L_107N,
    P12M_026N = PCT12M_107N,
    P12N_026N = PCT12N_107N,
    P12O_026N = PCT12O_107N,
    P12P_002N = PCT12A_003N - PCT12I_003N, #total male white minus total male white NH = total male white hisp
    P12Q_002N = PCT12B_003N - PCT12J_003N,
    P12R_002N = PCT12C_003N - PCT12K_003N,
    P12S_002N = PCT12D_003N - PCT12L_003N,
    P12T_002N = PCT12E_003N - PCT12M_003N,
    P12U_002N = PCT12F_003N - PCT12N_003N,
    P12V_002N = PCT12G_003N - PCT12O_003N,
    P12P_026N = PCT12A_107N - PCT12I_107N,
    P12Q_026N = PCT12B_107N - PCT12J_107N,
    P12R_026N = PCT12C_107N - PCT12K_107N,
    P12S_026N = PCT12D_107N - PCT12L_107N,
    P12T_026N = PCT12E_107N - PCT12M_107N,
    P12U_026N = PCT12F_107N - PCT12N_107N,
    P12V_026N = PCT12G_107N - PCT12O_107N
  )

  # subtract 0-1 bin from <5 bin to get 1-4 bin
  data <- data %>% mutate(
    P12I_003N = P12I_003N - P12I_002N,
    P12J_003N = P12J_003N - P12J_002N,
    P12K_003N = P12K_003N - P12K_002N,
    P12L_003N = P12L_003N - P12L_002N,
    P12M_003N = P12M_003N - P12M_002N,
    P12N_003N = P12N_003N - P12N_002N,
    P12O_003N = P12O_003N - P12O_002N,
    P12P_003N = P12P_003N - P12P_002N,
    P12Q_003N = P12Q_003N - P12Q_002N,
    P12R_003N = P12R_003N - P12R_002N,
    P12S_003N = P12S_003N - P12S_002N,
    P12T_003N = P12T_003N - P12T_002N,
    P12U_003N = P12U_003N - P12U_002N,
    P12V_003N = P12V_003N - P12V_002N,
    P12I_027N = P12I_027N - P12I_026N,
    P12J_027N = P12J_027N - P12J_026N,
    P12K_027N = P12K_027N - P12K_026N,
    P12L_027N = P12L_027N - P12L_026N,
    P12M_027N = P12M_027N - P12M_026N,
    P12N_027N = P12N_027N - P12N_026N,
    P12O_027N = P12O_027N - P12O_026N,
    P12P_027N = P12P_027N - P12P_026N,
    P12Q_027N = P12Q_027N - P12Q_026N,
    P12R_027N = P12R_027N - P12R_026N,
    P12S_027N = P12S_027N - P12S_026N,
    P12T_027N = P12T_027N - P12T_026N,
    P12U_027N = P12U_027N - P12U_026N,
    P12V_027N = P12V_027N - P12V_026N
  )

  # remove extra columns
  data <- data %>% select(-matches("PCT"))

  # collapse age bins that we don't need
  for (letter in c("I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V")) {
    col_name08 <- paste0("P12", letter, "_008N")
    col_name09 <- paste0("P12", letter, "_009N")
    col_name10 <- paste0("P12", letter, "_010N")
    col_match08 <- paste(col_name08, col_name09, col_name10, sep = "|")
    col_name18 <- paste0("P12", letter, "_018N")
    col_name19 <- paste0("P12", letter, "_019N")
    col_match18 <- paste(col_name18, col_name19, sep = "|")
    col_name20 <- paste0("P12", letter, "_020N")
    col_name21 <- paste0("P12", letter, "_021N")
    col_match20 <-paste(col_name20, col_name21, sep = "|")
    col_name32 <- paste0("P12", letter, "_032N")
    col_name33 <- paste0("P12", letter, "_033N")
    col_name34 <- paste0("P12", letter, "_034N")
    col_match32 <- paste(col_name32, col_name33, col_name34, sep = "|")
    col_name42 <- paste0("P12", letter, "_042N")
    col_name43 <- paste0("P12", letter, "_043N")
    col_match42 <- paste(col_name42, col_name43, sep = "|")
    col_name44 <- paste0("P12", letter, "_044N")
    col_name45 <- paste0("P12", letter, "_045N")
    col_match44 <- paste(col_name44, col_name45, sep = "|")
    col_name06 <- paste0("P12", letter, "_006N")
    col_name07 <- paste0("P12", letter, "_007N")
    col_match06 <- paste(col_name06, col_name07, sep = "|")
    col_name30 <- paste0("P12", letter, "_030N")
    col_name31 <- paste0("P12", letter, "_031N")
    col_match30 <- paste(col_name30, col_name31, sep = "|")
    data <- data %>%
      mutate({{col_name08}} := rowSums(select(st_drop_geometry(.), matches(col_match08)), na.rm = TRUE),
             {{col_name18}} := rowSums(select(st_drop_geometry(.), matches(col_match18)), na.rm = TRUE),
             {{col_name20}} := rowSums(select(st_drop_geometry(.), matches(col_match20)), na.rm = TRUE),
             {{col_name32}} := rowSums(select(st_drop_geometry(.), matches(col_match32)), na.rm = TRUE),
             {{col_name42}} := rowSums(select(st_drop_geometry(.), matches(col_match42)), na.rm = TRUE),
             {{col_name44}} := rowSums(select(st_drop_geometry(.), matches(col_match44)), na.rm = TRUE),
             {{col_name06}} := rowSums(select(st_drop_geometry(.), matches(col_match06)), na.rm = TRUE),
             {{col_name30}} := rowSums(select(st_drop_geometry(.), matches(col_match30)), na.rm = TRUE)) %>%
      select(-all_of(c(col_name09, col_name10, col_name19, col_name21, col_name33, col_name34, col_name43, col_name45,col_name07,col_name31)))
  }

  return(data)
}
