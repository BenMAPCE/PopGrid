
MARS_adjustments <- function(
    data = NULL
){

  age_values<-unique(substr(colnames(data)[!grepl(c("GEOID","NAME","geometry") %>% paste(collapse = "|"),colnames(data))],6,9))
  state.allocation.factors<-PopGrid_v2:::State.MARS
    state.allocation.factors$STATE_FIPS<-sprintf("%02d",state.allocation.factors$STATE_FIPS)
    #add state names and join state allocation factors with data
    data$STATE_FIPS<-substr(data$GEOID,1,2)
    data<-left_join(data,state.allocation.factors,by="STATE_FIPS")

  for (age in age_values){

   #STEP 1: redistribute the MULTI population category between other race and MULTI using MARS 2010 national data
    #get column names for Other race, not hispanic (N)
    col_name_N <- paste0("P12N_", age)
    #get column names for Other race, hispanic (U)
    col_name_U <- paste0("P12U_", age)
    #get column names for Other race, not hispanic (O)
    col_name_O <- paste0("P12O_", age)
    #get column names for Other race, hispanic (V)
    col_name_V <- paste0("P12V_", age)

    #Distribute MULTI race population between the OTHER and MULTI population
    # using the ratio of Multi-race:Some Other Race national national MARS ratio for hispanic (0.3725263310922090) and non-hispanic (0.0346537787331533)
    # new OTHER population = 1 + ratio * MULTI pop; new MULTI pop = 1 - ratio * MULTI pop
    data <- data %>%
      mutate(!!col_name_N := get(!!col_name_N) + (get(!!col_name_O) * 0.0346537787331533),
             !!col_name_U := get(!!col_name_U) + (get(!!col_name_V) * 0.3725263310922090),
             !!col_name_O := get(!!col_name_O) * (1 - 0.0346537787331533),
             !!col_name_V := get(!!col_name_V) * (1 - 0.3725263310922090))

  #STEP 2: Allocate OTHER population category to: White, Black, NatAmer, Asian, HIPI, and Multi using MARS 2010 national data
  Other.allocate<-data.frame(Race=c("WHITE","BLACK","NATAMER","ASIAN","HIPI","MULTI"),
                             HISPANIC=c(0.871193706392089,0.0405189908075793,0.0223258817872013,0.00681451799809359,0.00190419259140142,0.0572427104236354),
                             NONHISPANIC=c(0.765906107292845,0.146652678322662,0.00874447458774174,0.0562903394629576,0.00187403001296175,0.0205323703208315))
    names(Other.allocate)[3]<-"NON-HISPANIC"

  for (letter in c("I","J","K","L","M","O","P","Q","R","S","T","V")){
    #identify ethnicity/race and column names
    race<-case_when(letter %in% c("I", "P") ~ "WHITE", letter %in% c("J", "Q") ~ "BLACK",
      letter %in% c("K", "R") ~ "NATAMER", letter %in% c("L", "S") ~ "ASIAN",
      letter %in% c("M", "T") ~ "HIPI", letter %in% c("O", "V") ~ "MULTI",.default = NA)

    ethn<-ifelse(letter %in% c("I", "J", "K", "L", "M", "N", "O"),"NON-HISPANIC","HISPANIC")
    col_name_OTHER <- paste0("P12",ifelse(ethn=="HISPANIC","U_","N_"), age)
    col_name_Target <- paste0("P12",letter,"_", age)

    ratio<-Other.allocate[Other.allocate$Race==race,names(Other.allocate)==ethn]

    #distribute other population to race population
    data <- data %>%
      mutate(!!col_name_Target := get(!!col_name_Target) + (get(!!col_name_OTHER) * ratio))
  }
    #drop other columns
    drop_Other<-paste0(c("P12N_","P12U_"),age) %>% paste(collapse = "|")
    data <- data[,!grepl(drop_Other, names(data))]

  #STEP 3: Combine HIPI and Asian, label as Asian, remove HIPI column
    col_name_L<-paste0("P12L_", age)
    col_name_M<-paste0("P12M_", age)
    col_match_L<-paste(col_name_L, col_name_M, sep = "|")
    col_name_S<-paste0("P12S_", age)
    col_name_T<-paste0("P12T_", age)
    col_match_S<-paste(col_name_S, col_name_T, sep = "|")
    data <- data %>%
      mutate(!!col_name_L := get(!!col_name_L)+get(!!col_name_M),
             !!col_name_S := get(!!col_name_S)+get(!!col_name_T))%>%
             select(-all_of(c(col_name_M,col_name_T)))

  #STEP 4: Allocate MULTI race to the 4 races: white, black, natamer, and asian
  for (letter in c("I","J","K","L","P","Q","R","S")){
    #identify ethnicity/race and column names
    race<-case_when(letter %in% c("I", "P") ~ "WHITE", letter %in% c("J", "Q") ~ "BLACK",
                    letter %in% c("K", "R") ~ "NATAMER", letter %in% c("L", "S") ~ "ASIAN",.default = NA)

    ethn<-ifelse(letter %in% c("I", "J", "K", "L"),"NON-HISPANIC","HISPANIC")
    col_name_MULTI <- paste0("P12",ifelse(ethn=="HISPANIC","V_","O_"), age)
    col_name_Target <- paste0("P12",letter,"_", age)

    #distribute other population to race population
    data <- data %>%
      mutate(!!col_name_Target := get(!!col_name_Target) + (get(!!col_name_MULTI) * get(!!race)))
  }
  }
    #drop all excess races and allocation ratios
    drop_races<-c("P12M","P12N","P12O","P12T","P12U","P12V","STATE_FIPS","State","WHITE","BLACK","ASIAN","NATAMER") %>% paste(collapse = "|")
    data <- data[,!grepl(drop_races, names(data))]

  return(data)
}
