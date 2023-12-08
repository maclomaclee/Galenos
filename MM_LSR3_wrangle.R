data <- reconciled_varied_relevant
data <- subset(data, data$`DiseaseModelLabel(s)` != 'DAT +/-')
data <- subset(data, data$`DiseaseModelLabel(s)` != 'MK-801 + vehicle (MK-801 locomotion) – MALE SUBGROUP')
data <- subset(data, data$`DiseaseModelLabel(s)` != 'MK-801 + vehicle (MK-801 locomotion) – FEMALE SUBGROUP')
data <- subset(data, data$CohortLabel != 'MK-801 + vehicle (MK-801 locomotion) – MALE SUBGROUP')
data <- subset(data, data$CohortLabel != 'MK-801 + vehicle (MK-801 locomotion) – FEMALE SUBGROUP')


groups <- as.data.frame(unique(data$GroupID))
groups$`unique(data$GroupID)` <- as.character(groups$`unique(data$GroupID)`)

###run 1  - moving relevant control and sham to another column###

group <- groups[1,1]
  cohset <- subset(data, data$GroupID == group)
  
  chhrows <- nrow(cohset)
  # Assuming chhrows is the number of rows in cohset
  chhrows <- nrow(cohset)
  

  for (j in 1:chhrows) {
    if (cohset$CohortType[j] == 'Negative control for simple intervention') {
      cohset$Control_label <- cohset[j, 62]
      cohset$Control_n <- cohset[j, 63]
      cohset$Control_m <- cohset[j, 78]
      cohset$Control_v <- cohset[j, 80]
    }
  }
  
  for (j in 1:chhrows) {
    if(cohset$CohortType[j] == 'Sham for simple intervention'){
      cohset$Sham_label <- cohset[j,62]
      cohset$Sham_n <- cohset[j,63]
      cohset$Sham_m <- cohset[j,78]
      cohset$Sham_v <- cohset[j,80]
    }
  }
  for (j in 1:chhrows) {
    if(cohset$CohortType[j] == 'Control for combination intervention'){
      cohset$CombC_label <- cohset[j,62]
      cohset$CombC_n <- cohset[j,63]
      cohset$CombC_m <- cohset[j,78]
      cohset$CombC_v <- cohset[j,80]
    }
  }
  for (j in 1:chhrows) {
    if(cohset$CohortType[j] == 'Sham for TAAR1KO comb.'){
      cohset$ShamTAAR_label <- cohset[j,62]
      cohset$ShamTAAR_n <- cohset[j,63]
      cohset$ShamTAAR_m <- cohset[j,78]
      cohset$ShamTAAR_v <- cohset[j,80]
    }
  }
  for (j in 1:chhrows) {
    if(cohset$CohortType[j] == 'Negative control for TAAR1KO comb.'){
      cohset$CTAAR_label <- cohset[j,62]
      cohset$CTAAR_n <- cohset[j,63]
      cohset$CTAAR_m <- cohset[j,78]
      cohset$CTAAR_v <- cohset[j,80]
    }
  }

  data_org <- cohset

###now run the rest###
  
    for(i in 2:nrow(groups)){
  group <- groups[i,1]
  cohset <- subset(data, data$GroupID == group)
  #chhrows <- nrow(cohset)
  # Assuming chhrows is the number of rows in cohset
  chhrows <- nrow(cohset)
  
for (j in 1:chhrows) {
    if (cohset$CohortType[j] == 'Negative control for simple intervention') {
      cohset$Control_label <- cohset[j, 62]
      cohset$Control_n <- cohset[j, 63]
      cohset$Control_m <- cohset[j, 78]
      cohset$Control_v <- cohset[j, 80]
    }
  }
  
  for (j in 1:chhrows) {
    if(cohset$CohortType[j] == 'Sham for simple intervention'){
    cohset$Sham_label <- cohset[j,62]
    cohset$Sham_n <- cohset[j,63]
    cohset$Sham_m <- cohset[j,78]
    cohset$Sham_v <- cohset[j,80]
  }
  }
  for (j in 1:chhrows) {
    if(cohset$CohortType[j] == 'Control for combination intervention'){
      cohset$CombC_label <- cohset[j,62]
      cohset$CombC_n <- cohset[j,63]
      cohset$CombC_m <- cohset[j,78]
      cohset$CombC_v <- cohset[j,80]
    }
  }
  for (j in 1:chhrows) {
    if(cohset$CohortType[j] == 'Sham for TAAR1KO comb.'){
      cohset$ShamTAAR_label <- cohset[j,62]
      cohset$ShamTAAR_n <- cohset[j,63]
      cohset$ShamTAAR_m <- cohset[j,78]
      cohset$ShamTAAR_v <- cohset[j,80]
    }
  }
  for (j in 1:chhrows) {
    if(cohset$CohortType[j] == 'Negative control for TAAR1KO comb.'){
      cohset$CTAAR_label <- cohset[j,62]
      cohset$CTAAR_n <- cohset[j,63]
      cohset$CTAAR_m <- cohset[j,78]
      cohset$CTAAR_v <- cohset[j,80]
    }
  }

  data_org <- bind_rows(data_org, cohset)  
}

data_SI <- subset(data_org, data_org$CohortType== 'Simple intervention')
data_CI <- subset(data_org, data_org$CohortType== 'Combination intervention')
data_TI <- subset(data_org, data_org$CohortType== 'Intervention and TAAR1KO comb.')

### get standard placement - Single I ###
data_SI$F_C_L <- data_SI$Control_label
data_SI$F_C_n <- data_SI$Control_n
data_SI$F_C_m <- data_SI$Control_m
data_SI$F_C_v <- data_SI$Control_v

data_SI$F_T_L <- data_SI$CohortLabel
data_SI$F_T_n <- data_SI$NumberOfAnimals
data_SI$F_T_m <- data_SI$OutcomeResult
data_SI$F_T_v <- data_SI$OutcomeError

data_SI$F_S_L <- data_SI$Sham_label
data_SI$F_S_n <- data_SI$Sham_n
data_SI$F_S_m <- data_SI$Sham_m
data_SI$F_S_v <- data_SI$Sham_v

### get standard placement - Combined I ###
data_CI$F_C_L <- data_CI$CombC_label
data_CI$F_C_n <- data_CI$CombC_n
data_CI$F_C_m <- data_CI$CombC_m
data_CI$F_C_v <- data_CI$CombC_v

data_CI$F_T_L <- data_CI$CohortLabel
data_CI$F_T_n <- data_CI$NumberOfAnimals
data_CI$F_T_m <- data_CI$OutcomeResult
data_CI$F_T_v <- data_CI$OutcomeError

data_CI$F_S_L <- data_CI$Sham_label
data_CI$F_S_n <- data_CI$Sham_n
data_CI$F_S_m <- data_CI$Sham_m
data_CI$F_S_v <- data_CI$Sham_v

### get standard placement - TAAR I ###
data_TI$F_C_L <- data_TI$CTAAR_label
data_TI$F_C_n <- data_TI$CTAAR_n
data_TI$F_C_m <- data_TI$CTAAR_m
data_TI$F_C_v <- data_TI$CTAAR_v

data_TI$F_T_L <- data_TI$CohortLabel
data_TI$F_T_n <- data_TI$NumberOfAnimals
data_TI$F_T_m <- data_TI$OutcomeResult
data_TI$F_T_v <- data_TI$OutcomeError

data_TI$F_S_L <- data_TI$ShamTAAR_label
data_TI$F_S_n <- data_TI$ShamTAAR_n
data_TI$F_S_m <- data_TI$ShamTAAR_m
data_TI$F_S_v <- data_TI$ShamTAAR_v

flatten_column <- function(col) {   
  if (is.matrix(col) || is.list(col)) {     
    # Convert matrix or list to a character string     
    return(map_chr(col, toString))   
  } else {     # Leave other types of columns as they are     
    return(col)   } } # Apply the function to each column in the dataframe 

data_CI_F <- data_CI[,-c(81:101)]
data_SI_F <- data_SI[,-c(81:101)]
data_TI_F <- data_TI[,-c(81:101)]



data_CI_F[,81] <- unlist(data_CI_F[,81])
data_CI_F[,82] <- unlist(data_CI_F[,82])
data_CI_F[,83] <- unlist(data_CI_F[,83])
data_CI_F[,84] <- unlist(data_CI_F[,84])
data_CI_F[,89] <- unlist(data_CI_F[,89])
data_CI_F[,90] <- unlist(data_CI_F[,90])
data_CI_F[,91] <- unlist(data_CI_F[,91])
data_CI_F[,92] <- unlist(data_CI_F[,92])

data_SI_F[,81] <- unlist(data_SI_F[,81])
data_SI_F[,82] <- unlist(data_SI_F[,82])
data_SI_F[,83] <- unlist(data_SI_F[,83])
data_SI_F[,84] <- unlist(data_SI_F[,84])
data_SI_F[,89] <- unlist(data_SI_F[,89])
data_SI_F[,90] <- unlist(data_SI_F[,90])
data_SI_F[,91] <- unlist(data_SI_F[,91])
data_SI_F[,92] <- unlist(data_SI_F[,92])

data_TI_F[,81] <- unlist(data_TI_F[,81])
data_TI_F[,82] <- unlist(data_TI_F[,82])
data_TI_F[,83] <- unlist(data_TI_F[,83])
data_TI_F[,84] <- unlist(data_TI_F[,84])
data_TI_F[,89] <- unlist(data_TI_F[,89])
data_TI_F[,90] <- unlist(data_TI_F[,90])
data_TI_F[,91] <- unlist(data_TI_F[,91])
data_TI_F[,92] <- unlist(data_TI_F[,92])

data_all_F <- rbind(data_CI_F, data_SI_F, data_TI_F)

data_all_F <- data_all_F%>%
  mutate(drugname1 = case_when(
    grepl("RO5263397", data_all_F$`InterventionLabel[1]`) ~ "RO5263397",
    grepl("olanzepine", data_all_F$`InterventionLabel[1]`) ~ "olanzepine",
    grepl("SEP-363856", data_all_F$`InterventionLabel[1]`) ~ "SEP-363856",
    grepl("SEP-383856", data_all_F$`InterventionLabel[1]`) ~ "SEP-363856",
    grepl("SEP-856", data_all_F$`InterventionLabel[1]`) ~ "SEP-363856",
    grepl("SEP", data_all_F$`InterventionLabel[1]`) ~ "SEP",
    grepl("RO5203648", data_all_F$`InterventionLabel[1]`) ~ "RO5203648",
    grepl("LK000764", data_all_F$`InterventionLabel[1]`) ~ "LK000764",
    grepl("RO5256390", data_all_F$`InterventionLabel[1]`) ~ "RO5256390",
    grepl("SEP-856", data_all_F$`InterventionLabel[1]`) ~ "SEP-856",
    grepl("RO5073012", data_all_F$`InterventionLabel[1]`) ~ "RO5073012",
    grepl("Compound 50B", data_all_F$`InterventionLabel[1]`) ~ "Compound 50B",
    grepl("Compound 50A", data_all_F$`InterventionLabel[1]`) ~ "Compound 50A",
    grepl("RO5166017", data_all_F$`InterventionLabel[1]`) ~ "RO5166017",
    grepl("risperidone", data_all_F$`InterventionLabel[1]`) ~ "risperidone",
    grepl("Olanzapine", data_all_F$`InterventionLabel[1]`) ~ "olanzapine",
    grepl("OLZ", data_all_F$`InterventionLabel[1]`) ~ "olanzepine",
    grepl("TAAR1 KO", data_all_F$`InterventionLabel[1]`) ~ "TAAR1 KO",
    grepl("AP163", data_all_F$`InterventionLabel[1]`) ~ "AP163",
    TRUE ~ "Other"
  ))
data_all_F <- data_all_F%>%
  mutate(drugname2 = case_when(
    grepl("RO5263397", data_all_F$`InterventionLabel[2]`) ~ "RO5263397",
    grepl("olanzepine", data_all_F$`InterventionLabel[2]`) ~ "olanzepine",
    grepl("SEP-363856", data_all_F$`InterventionLabel[2]`) ~ "SEP-363856",
    grepl("SEP-383856", data_all_F$`InterventionLabel[1]`) ~ "SEP-363856",
    grepl("SEP-856", data_all_F$`InterventionLabel[2]`) ~ "SEP-363856",
    grepl("SEP", data_all_F$`InterventionLabel[2]`) ~ "SEP",
    grepl("RO5203648", data_all_F$`InterventionLabel[2]`) ~ "RO5203648",
    grepl("LK000764", data_all_F$`InterventionLabel[2]`) ~ "LK000764",
    grepl("RO5256390", data_all_F$`InterventionLabel[2]`) ~ "RO5256390",
    grepl("SEP-856", data_all_F$`InterventionLabel[2]`) ~ "SEP-856",
    grepl("RO5073012", data_all_F$`InterventionLabel[2]`) ~ "RO5073012",
    grepl("Compound 50B", data_all_F$`InterventionLabel[2]`) ~ "Compound 50B",
    grepl("Compound 50A", data_all_F$`InterventionLabel[2]`) ~ "Compound 50A",
    grepl("RO5166017", data_all_F$`InterventionLabel[2]`) ~ "RO5166017",
    grepl("risperidone", data_all_F$`InterventionLabel[2]`) ~ "risperidone",
    grepl("Olanzapine", data_all_F$`InterventionLabel[2]`) ~ "olanzapine",
    grepl("OLZ", data_all_F$`InterventionLabel[2]`) ~ "olanzepine",
    grepl("TAAR1 KO", data_all_F$`InterventionLabel[2]`) ~ "TAAR1 KO",
    grepl("AP163", data_all_F$`InterventionLabel[2]`) ~ "AP163",
    TRUE ~ "Other"
  ))

data_SI_F <- subset(data_all_F, data_all_F$CohortType== 'Simple intervention')
data_CI_F <- subset(data_all_F, data_all_F$CohortType== 'Combination intervention')
data_TI_F <- subset(data_all_F, data_all_F$CohortType== 'Intervention and TAAR1KO comb.')


write_csv(data_CI_F, 'dataCI_121123_0937.csv')
write_csv(data_SI_F, 'dataSI_121123_0937.csv')
write_csv(data_TI_F, 'dataTI_121123_0937.csv')
write_csv(data_all_F, 'dataall_121123_0937.csv')

interventions1 <- data_all_F[,72]
interventions2 <- data_all_F[,73]
colnames(interventions1) <- colnames(interventions2) <- 'drug'
interventions <- rbind(interventions1, interventions2)
interventions <- unique(interventions)
interventions <- interventions %>%
  mutate(drugname = case_when(
    grepl("RO5263397", interventions$drug) ~ "RO5263397",
    grepl("olanzepine", interventions$drug) ~ "olanzepine",
    grepl("SEP-363856", interventions$drug) ~ "SEP-363856",
    grepl("SEP-383856", interventions$drug) ~ "SEP-363856",
    grepl("SEP-856", interventions$drug) ~ "SEP-363856",
    grepl("SEP", interventions$drug) ~ "SEP",
    grepl("RO5203648", interventions$drug) ~ "RO5203648",
    grepl("LK000764", interventions$drug) ~ "LK000764",
    grepl("RO5256390", interventions$drug) ~ "RO5256390",
    grepl("SEP-856", interventions$drug) ~ "SEP-856",
    grepl("RO5073012", interventions$drug) ~ "RO5073012",
    grepl("Compound 50B", interventions$drug) ~ "Compound 50B",
    grepl("Compound 50A", interventions$drug) ~ "Compound 50A",
    grepl("RO5166017", interventions$drug) ~ "RO5166017",
    grepl("risperidone", interventions$drug) ~ "risperidone",
    grepl("Olanzapine", interventions$drug) ~ "olanzapine",
    grepl("OLZ", interventions$drug) ~ "olanzepine",
    grepl("TAAR1 KO", interventions$drug) ~ "TAAR1 KO",
    grepl("AP163", interventions$drug) ~ "AP163",
    TRUE ~ "Other"
  ))

