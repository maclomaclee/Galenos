data <- reconciled_records_unique
#data <- subset(data, data$`DiseaseModelLabel(s)` != 'DAT +/-')
#data <- subset(data, data$`DiseaseModelLabel(s)` != 'MK-801 + vehicle (MK-801 locomotion) – MALE SUBGROUP')
#data <- subset(data, data$`DiseaseModelLabel(s)` != 'MK-801 + vehicle (MK-801 locomotion) – FEMALE SUBGROUP')



groups <- as.data.frame(unique(data$GroupID))
groups$`unique(data$GroupID)` <- as.character(groups$`unique(data$GroupID)`)

###run 1  - moving relevant control and sham to another column###

group <- groups[1,1]
  cohset <- subset(data, data$GroupID == group)
  
  chhrows <- nrow(cohset)
  for (j in 1:chhrows) {
    if (cohset$CohortType[j] == 'Negative control') {
      cohset$Control_label <- cohset[j, 115]
      cohset$Control_n <- cohset[j, 116]
      cohset$Control_m <- cohset[j, 133]
      cohset$Control_v <- cohset[j, 136]
    }
  }
  
  for (j in 1:chhrows) {
    if(cohset$CohortType[j] == 'Sham'){
      cohset$Sham_label <- cohset[j,115]
      cohset$Sham_n <- cohset[j,116]
      cohset$Sham_m <- cohset[j,133]
      cohset$Sham_v <- cohset[j,136]
    }
  }
  for (j in 1:chhrows) {
    if(cohset$CohortType[j] == 'Negative control for cotreatment'){
      cohset$CombC_label <- cohset[j,115]
      cohset$CombC_n <- cohset[j,116]
      cohset$CombC_m <- cohset[j,133]
      cohset$CombC_v <- cohset[j,136]
    }
  }

  data_org <- cohset

###now run the rest###
  
    for(i in 2:nrow(groups)){
  group <- groups[i,1]
  cohset <- subset(data, data$GroupID == group)
  
  chhrows <- nrow(cohset)
  for (j in 1:chhrows) {
    if (cohset$CohortType[j] == 'Negative control') {
      cohset$Control_label <- cohset[j, 115]
      cohset$Control_n <- cohset[j, 116]
      cohset$Control_m <- cohset[j, 133]
      cohset$Control_v <- cohset[j, 136]
    }
  }
  
  for (j in 1:chhrows) {
    if(cohset$CohortType[j] == 'Sham'){
      cohset$Sham_label <- cohset[j,115]
      cohset$Sham_n <- cohset[j,116]
      cohset$Sham_m <- cohset[j,133]
      cohset$Sham_v <- cohset[j,136]
    }
  }
  for (j in 1:chhrows) {
    if(cohset$CohortType[j] == 'Negative control for cotreatment'){
      cohset$CombC_label <- cohset[j,115]
      cohset$CombC_n <- cohset[j,116]
      cohset$CombC_m <- cohset[j,133]
      cohset$CombC_v <- cohset[j,136]
    }
  }
  data_org <- bind_rows(data_org, cohset)  
}

data_SI <- subset(data_org, data_org$CohortType== 'Exercise Intervention')
data_CI <- subset(data_org, data_org$CohortType== 'Intervention and cotreatment')


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

data_CI_F <- data_CI[,-c(142:153)]
data_SI_F <- data_SI[,-c(142:153)]


data_CI_F[,142] <- unlist(data_CI_F[,142])
data_CI_F[,143] <- unlist(data_CI_F[,143])
data_CI_F[,144] <- unlist(data_CI_F[,144])
data_CI_F[,145] <- unlist(data_CI_F[,145])
data_CI_F[,150] <- unlist(data_CI_F[,150])
data_CI_F[,151] <- unlist(data_CI_F[,151])
data_CI_F[,152] <- unlist(data_CI_F[,152])
data_CI_F[,153] <- unlist(data_CI_F[,153])


data_SI_F[,142] <- unlist(data_SI_F[,142])
data_SI_F[,143] <- unlist(data_SI_F[,143])
data_SI_F[,144] <- unlist(data_SI_F[,144])
data_SI_F[,145] <- unlist(data_SI_F[,145])
data_SI_F[,150] <- unlist(data_SI_F[,150])
data_SI_F[,151] <- unlist(data_SI_F[,151])
data_SI_F[,152] <- unlist(data_SI_F[,152])
data_SI_F[,153] <- unlist(data_SI_F[,153])

data_all_F <- rbind(data_CI_F, data_SI_F)

data_all_F <- data_all_F%>%
  mutate(drugname1 = case_when(
    grepl("fluoxetine", data_all_F$`InterventionLabel[1]`) ~ "fluoxetine",
    grepl("Fluoxetine", data_all_F$`InterventionLabel[1]`) ~ "Fluoxetine",
    TRUE ~ "Other"
  ))
data_all_F <- data_all_F%>%
  mutate(drugname2 = case_when(
    grepl("fluoxetine", data_all_F$`InterventionLabel[2]`) ~ "fluoxetine",
    grepl("Fluoxetine", data_all_F$`InterventionLabel[2]`) ~ "Fluoxetine",
    TRUE ~ "Other"
  ))

data_SI_F <- subset(data_all_F, data_all_F$CohortType== 'Exercise Intervention')
data_CI_F <- subset(data_all_F, data_all_F$CohortType== 'Intervention and cotreatment')



write_csv(data_CI_F, 'data_LSR2_CI.csv')
write_csv(data_SI_F, 'dataSI_LSR2_.csv')
write_csv(data_all_F, 'dataall_LSR2.csv')

####ends here###

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
