library(dplyr)
library(stringr)
library(tibble)
library(tidyverse)
library(tidyr)
library(purrr)

setwd("C:/Users/mmacleo2/OneDrive - University of Edinburgh/Applications/WT Mental Health/LSR2_PTSD")

# Import SyRF outcome data
LSR2_data <- read_csv("Quantitative_data_-_2023_12_08_-_fd256f61-62c2-4868-9ffb-6a64d694c8ed_-_Investigators_Unblinded.csv")


# Filter for reconciled studies (and rename columns for consistency with shiny outcomes/remove SYRF columnns)
LSR2_reconciled <- subset(LSR2_data, LSR2_data$`is this a reconciliation?` == TRUE) %>%  #for PTSD `is this a reconciliation?`
  rename(ModelID = `DiseaseModelId(s)`, 
         ExperimentID = ExperimentId, 
         InterventionLabel = `TreatmentLabel(s)`,
         InterventionID = `TreatmentId(s)`,
         OutcomeResult = OutcomeAverage) 

## 3. Further step to remove observations from an accidental dual-reconciliation: choose the most recent reconciliation (unlikely to be necessary)

# Choose the most recent reconciliation 
recent_reconciledID <- LSR2_reconciled %>%
  arrange(desc(DateTimeDataEntered)) %>%
  group_by(StudyId) %>%
  slice(1) %>%
  ungroup()

# Filter all records that have a StudyIdStr and AnnotatorIdStr that is in the reconciled_study_annotator_pairs
reconciled_records_unique <- LSR2_reconciled %>%
  semi_join(recent_reconciledID, by = "StudyId")

opname <- paste0('reconciled_records_LSR2_',Sys.Date(),'.csv')
write_csv(reconciled_records_unique, opname)


# Rearrange rows to be grouped by studies and then experiment within studies. Reorder columns for readability
reconciled_records <- reconciled_records_unique %>%
  arrange(StudyId, ExperimentID, CohortId, OutcomeId, InterventionID) %>% 
  relocate(ExperimentLabel, .after = ExperimentID) %>% 
  relocate(CohortLabel, .after = CohortId) %>% 
  relocate(OutcomeLabel, .after = OutcomeId) %>% 
  relocate(c(OutcomeLabel, OutcomeId), .after = ExperimentLabel) %>% 
  relocate(c(InterventionLabel), .after = InterventionID)


# Remove columns that do not vary their values within individual studies (-> reconciled_studyconstants df). Join later

#reconciled_constants <- reconciled_records %>%
#  group_by(StudyId) %>%
#  summarise(across(everything(), ~n_distinct(.) == 1)) %>%
#  select_if(~all(.)) %>%
#  names() #contains information that is important to each experiment e.g. Age of animal at intervention, Type #of numerical data (all are means). But for ease, still remove because will left join them back onto smaller df #later.

#reconciled_varied <- reconciled_records %>% 
#  select(-any_of(reconciled_constants))

#write.csv(reconciled_varied, 'reconciled_varied_PTSD_111123.csv')

# Split any column with a ; value 
cols_to_split <- as.data.frame(sapply(reconciled_records_unique, function(column) any(grepl(";", column))))
cols_to_split <- subset(cols_to_split, cols_to_split$`sapply(reconciled_records_unique, function(column) any(grepl(";", column)))` == 'TRUE')
cols_to_split$col <- rownames(cols_to_split)

for(i in 1:nrow(cols_to_split)) {
  col <- cols_to_split[i,2]
  reconciled_records_unique <- reconciled_records_unique %>%
    separate(col, into = c(paste0(col, "[1]"), paste0(col, "[2]"),paste0(col,"[3]")),
             sep = ";", fill = "right", remove = TRUE) 
}

###sorting out the columns created###
reconciled_records_unique$`Voluntary or forced exercise?[1]` <- ifelse(reconciled_records_unique$`Voluntary or forced exercise?[1]` == "", reconciled_records_unique$`Voluntary or forced exercise?[2]`, reconciled_records_unique$`Voluntary or forced exercise?[1]`)

reconciled_records_unique$`Equipment used for exercise intervention[1]` <- ifelse(reconciled_records_unique$`Equipment used for exercise intervention[1]` == "", reconciled_records_unique$`Equipment used for exercise intervention[2]`, reconciled_records_unique$`Equipment used for exercise intervention[1]`)

reconciled_records_unique$`Duration of intervention (total)[1]` <- ifelse(reconciled_records_unique$`Duration of intervention (total)[1]` == "", reconciled_records_unique$`Duration of intervention (total)[2]`, reconciled_records_unique$`Duration of intervention (total)[1]`)

reconciled_records_unique$`Unit of measurement (exercise duration)[1]` <- ifelse(reconciled_records_unique$`Unit of measurement (exercise duration)[1]` == "", reconciled_records_unique$`Unit of measurement (exercise duration)[2]`, reconciled_records_unique$`Unit of measurement (exercise duration)[1]`)

reconciled_records_unique$`Exercise dosing regime:[1]` <- ifelse(reconciled_records_unique$`Exercise dosing regime:[1]` == "", reconciled_records_unique$`Exercise dosing regime:[2]`, reconciled_records_unique$`Exercise dosing regime:[1]`)

reconciled_records_unique$`Number of sessions:[1]` <- ifelse(reconciled_records_unique$`Number of sessions:[1]` == "", reconciled_records_unique$`Number of sessions:[2]`, reconciled_records_unique$`Number of sessions:[1]`)

reconciled_records_unique$`Frequency of sessions:[1]` <- ifelse(reconciled_records_unique$`Frequency of sessions:[1]` == "", reconciled_records_unique$`Frequency of sessions:[2]`, reconciled_records_unique$`Frequency of sessions:[1]`)

reconciled_records_unique$`Unit of measurement (frequency)[1]` <- ifelse(reconciled_records_unique$`Unit of measurement (frequency)[1]` == "", reconciled_records_unique$`Unit of measurement (frequency)[2]`, reconciled_records_unique$`Unit of measurement (frequency)[1]`)

reconciled_records_unique$`Exercise intensity:[1]` <- ifelse(reconciled_records_unique$`Exercise intensity:[1]` == "", reconciled_records_unique$`Exercise intensity:[2]`, reconciled_records_unique$`Exercise intensity:[1]`)

reconciled_records_unique$`Exercise intensity units:[1]` <- ifelse(reconciled_records_unique$`Exercise intensity units:[1]` == "", reconciled_records_unique$`Exercise intensity units:[2]`, reconciled_records_unique$`Exercise intensity units:[1]`)

reconciled_records_unique$`Exercise individual session duration:[1]` <- ifelse(reconciled_records_unique$`Exercise individual session duration:[1]` == "", reconciled_records_unique$`Exercise individual session duration:[2]`, reconciled_records_unique$`Exercise individual session duration:[1]`)

reconciled_records_unique$`Exercise duration units:[1]` <- ifelse(reconciled_records_unique$`Exercise duration units:[1]` == "", reconciled_records_unique$`Exercise duration units:[2]`, reconciled_records_unique$`Exercise duration units:[1]`)

reconciled_records_unique$`What was the timing of the initiation of treatment[1]` <- ifelse(reconciled_records_unique$`What was the timing of the initiation of treatment[1]` == "", reconciled_records_unique$`What was the timing of the initiation of treatment[2]`, reconciled_records_unique$`What was the timing of the initiation of treatment[2]`)

reconciled_records_unique <- reconciled_records_unique[,-c(72,73,75,76,78,79,81,82,84,85,87,88,90,91,93,94,96,97,99,100,102,103,105,106,108,109,111,112)]


# Label types of cohorts
#Aim = Make each experiment one comparison (sham vs. control vs. #intervention)

reconciled_records_unique <- reconciled_records_unique %>%
  mutate(
    Treatment1Type = case_when(
      str_detect(`InterventionLabel[1]`, regex("sed", ignore_case = TRUE)) ~ "Negative control",
      str_detect(`InterventionLabel[1]`, regex("sham exercise", ignore_case = TRUE)) ~ "Negative control",
      str_detect(`InterventionLabel[1]`, regex("fluoxetine", ignore_case = TRUE)) ~ "cotreatment",
      str_detect(`InterventionLabel[1]`, regex("sham ovariectomy", ignore_case = TRUE)) ~ "cotreatment control",
      str_detect(`InterventionLabel[1]`, regex("ovariectomy", ignore_case = TRUE)) ~ "cotreatment",
      is.na(`InterventionLabel[1]`) ~ "Negative control",
      TRUE ~ "Intervention"
    ),
    Treatment2Type = case_when(
      str_detect(`InterventionLabel[2]`, regex("sed", ignore_case = TRUE)) ~ "Negative control",
      str_detect(`InterventionLabel[2]`, regex("sham exercise", ignore_case = TRUE)) ~ "Negative control",
      str_detect(`InterventionLabel[2]`, regex("fluoxetine", ignore_case = TRUE)) ~ "cotreatment",
      str_detect(`InterventionLabel[2]`, regex("sham ovariectomy", ignore_case = TRUE)) ~ "cotreatment control",
      str_detect(`InterventionLabel[2]`, regex("ovariectomy", ignore_case = TRUE)) ~ "cotreatment",
      is.na(`InterventionLabel[2]`) ~ "Negative control",
      TRUE ~ "Intervention"
    )
  )

## Make CohortType column

# define shams ##
reconciled_records_unique <- reconciled_records_unique %>%
  mutate(CohortType = case_when(
    (IsDiseaseModelControl == 'True' | is.na(IsDiseaseModelControl)) & (Treatment1Type == "Negative control" & Treatment2Type == "Negative control") ~ "Sham",
    
    (IsDiseaseModelControl == 'True' | is.na(IsDiseaseModelControl)) & ((Treatment1Type == "cotreatment" & Treatment2Type == "Negative control")  | (Treatment2Type == "cotreatment" & Treatment1Type == "Negative control")) ~ "Sham for cotreatment",
    
    (IsDiseaseModelControl == 'True' | is.na(IsDiseaseModelControl)) & ((Treatment1Type == "cotreatment" & Treatment2Type == "Intervention")  | (Treatment2Type == "cotreatment" & Treatment1Type == "Intervention")) ~ "Sham for combined treatment",     
    
    (IsDiseaseModelControl == 'True' | is.na(IsDiseaseModelControl)) & ((Treatment1Type == "Negative control" & Treatment2Type == "Intervention")  | (Treatment2Type == "Negative control" & Treatment1Type == "Intervention")) ~ "Sham in presence of intervention", 
    
    (IsDiseaseModelControl == 'True' | is.na(IsDiseaseModelControl)) & 
      ((Treatment1Type == "cotreatment control" & Treatment2Type == "Intervention") |
         (Treatment2Type == "cotreatment control" & Treatment1Type == "Intervention")) ~ "Sham for cotreatment in presence of intervention",
    
    (IsDiseaseModelControl == 'True' | is.na(IsDiseaseModelControl)) & 
      ((Treatment1Type == "cotreatment control" & Treatment2Type == "Negative control") |
         (Treatment2Type == "cotreatment control" & Treatment1Type == "Negative control")) ~ "Sham for cotreatment",
    
    ###define sps groups ###
    IsDiseaseModelControl == 'False' & 
      ((Treatment1Type == "cotreatment" & Treatment2Type == "Negative control") |
         (Treatment2Type == "cotreatment" & Treatment1Type == "Negative control")) ~ "Negative control for cotreatment",
    
    IsDiseaseModelControl == 'False' & 
      ((Treatment1Type == "cotreatment" & Treatment2Type == "Intervention") |
         (Treatment2Type == "cotreatment" & Treatment1Type == "Intervention")) ~ "Intervention and cotreatment",
    
    IsDiseaseModelControl == 'False' & 
      (Treatment1Type == "Negative control" & Treatment2Type == "Negative control") ~ "Negative control",
    
    IsDiseaseModelControl == 'False' & 
      ((Treatment1Type == "cotreatment control" & Treatment2Type == "Negative control") |
         (Treatment2Type == "cotreatment control" & Treatment1Type == "Negative control")) ~ "Negative control for cotreatment",
    
    IsDiseaseModelControl == 'False' & 
      ((Treatment1Type == "cotreatment control" & Treatment2Type == "Intervention") |
         (Treatment2Type == "cotreatment control" & Treatment1Type == "Intervention")) ~ "Intervention and cotreatment",
    
    
    IsDiseaseModelControl == 'False' & 
      ((Treatment1Type == "Negative control" & Treatment2Type == "Intervention") |
         (Treatment2Type == "Negative control" & Treatment1Type == "Intervention")) ~ "Exercise Intervention"
  ))

reconciled_records_unique <- reconciled_records_unique %>%    mutate(GroupID = interaction(StudyId, ExperimentID, OutcomeId, TimeInMinute))


### now wrangle to one line per contrast
data <- reconciled_records_unique


groups <- as.data.frame(unique(data$GroupID))
colnames(groups) <- 'GroupId'
groups$GroupId <- as.character(groups$GroupId)

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

#name output files
LSR2_simple <- paste0('data_LSR2_simple_intervention_',Sys.Date(),'.csv')
LSR2_cotreat <- paste0('data_LSR2_intervention_with_cotreatment_',Sys.Date(),'.csv')
LSR2_all <- paste0('data_LSR2_all_interventions_',Sys.Date(),'.csv')

write_csv(data_CI_F, LSR2_simple)
write_csv(data_SI_F, LSR2_cotreat)
write_csv(data_all_F, LSR2_all)

