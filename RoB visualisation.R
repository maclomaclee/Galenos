### risk of bias visualisation function ###

devtools::install_github("mcguinlu/robvis")
install_github("mcguinlu/robvis")
library(dplyr)
library(robvis)
library(stringr)
library(ggplot2)


#read in data
data <- read.csv("Downloads/LSR3 - TAAR agonists for psychosis 101123.csv")
#name
title <- paste0('LSR3_',Sys.Date())

#remove values from appendix records
data <- data %>% filter(data$StudyId != 'dd9dddac-739b-412f-9fac-6f36e0a21494')
data <- data %>% filter(data$StudyId != '283c541f-6f14-462b-9f3a-800b69a3f44c')

#remove non reconciled
reconciled <- subset(data, data$Is.this.a.reconciliation. == TRUE)
#remove should have been excluded
reconciled <- subset(reconciled, reconciled$should.have.been.excluded == FALSE)

#extract RoB items

RoB <- unique(reconciled[,c(1,3,9, 22:55)])
#change studyId to Author, year
RoB$StudyId <- toupper(paste0(str_extract(RoB$Authors,"\\b\\w+\\b"),', ',RoB$Year))

# fix >1 publication per first author in a year

# Assuming your data frame is named RoB and the column is named StudyId
unique_study_ids <- unique(RoB$StudyId)
suffix_list <- character(length = nrow(RoB))

for (study_id in unique_study_ids) {
  indices <- RoB$StudyId == study_id
  if (sum(indices) > 1) {
    suffix_list[indices] <- letters[seq_along(suffix_list[indices])]
  }
}

RoB$suffix <- suffix_list

# Add the suffix to the original column
RoB$StudyId <- paste(RoB$StudyId, RoB$suffix, sep = "")

# Remove the 'suffix' column if you no longer need it
RoB <- select(RoB, -suffix)
RoB <- RoB[order(RoB$StudyId),]


#extract Syrcle RoB scores
SyRCLE <- RoB[,c(1,5:14)]

#Change "yes' to 'low' and 'No' to 'high'
SyRCLE <- mutate_all(SyRCLE, list(~ ifelse(. == 'Yes', 'Low', .)))
SyRCLE <- mutate_all(SyRCLE, list(~ ifelse(. == 'No', 'High', .)))


colnames(SyRCLE) <- c('Study ID','Allocation sequence','Baseline similarity','Concealment of allocation sequence','Random housing','Caregivers blinded','Random selection for outcome assessment','Blinded outcome assessor','Incomplete data reporting addressed','Free from selective outcome reporting','Free of other risks of bias')
RoB_summary <- rob_summary(data <- SyRCLE, tool = "Generic", weighted = FALSE, overall = FALSE)
RoB_TL <- rob_traffic_light(data <- SyRCLE, tool = "Generic", psize = 10, overall = FALSE)

#extract ARRIVE reporting scores
ARRIVE <- RoB[,c(1,15:37)]

#Change "yes' to 'low' and 'No' to 'high'
ARRIVE <- mutate_all(ARRIVE, list(~ ifelse(. == 'Yes', 'Low', .)))
ARRIVE <- mutate_all(ARRIVE, list(~ ifelse(. == 'No', 'High', .)))
#but ethics NAs to justification into ethics low risk
ARRIVE <- mutate_all(ARRIVE, list(~ ifelse(. == 'NA (ethical approval declared)', 'Low', .)))
#combine desc stats and variance with ES and CI
ARRIVE <- ARRIVE %>%
  mutate(Data_reporting = ifelse(ARRIVE$X.ARRIVE..Are.desc.stats.for.each.exp.group.provided.with.measure.of.variability. == 'Low' | ARRIVE$X.ARRIVE..Is.the.effect.size.and.confidence.interval.provided. == 'Low', 'Low', 'High'))
ARRIVE <- ARRIVE[,c(1:17,25,20:24)]


colnames(ARRIVE) <- c('Study ID','Groups clearly defined','Experimental unit defined','Exact number of experimental units','Sample size justification',
                      'Inclusion and exclusion criteria given','Any exclusions reported','Randomisation for any experiments','Blinding to group allocation',
                      'Details of what was measured','Statistical approach for each outcome','Assessment of whether data met statistical assumptions',
                      'All species specified','Animal sex specified','Age, weight or developmental stage specified','Timing and frequency of proceedures described',
                      'Any acclimitisation described','Data with variance, or Effect size and CI','Ethical approval with approval number',
                      'Ethical approval with or without approval number','Conflicts of interest statement','Funding sources','Description of any role of funder')
Rep_summary <- rob_summary(data <- ARRIVE, tool = "Generic", weighted = FALSE, overall = FALSE) + ggtitle("Reporting quality")
Rep_TL <- rob_traffic_light(data <- ARRIVE, tool = "Generic", psize = 10, overall = FALSE) + ggtitle("Reporting quality")

RoB_summary_name <- paste0(title,'RoB_summ.jpg')
RoB_tl_name <- paste0(title,'RoB_TL.jpg')
Rep_summary_name <- paste0(title,'Rep_summ.jpg')
Rep_tl_name <- paste0(title,'Rep_TL.jpg')

ggsave(RoB_summary_name, RoB_summary, width = 10, height = 10, units = 'in', dpi = 600)
ggsave(RoB_tl_name, RoB_TL, width = 10, height = 10, units = 'in', dpi = 600)
ggsave(Rep_summary_name, Rep_summary, width = 10, height = 10, units = 'in', dpi = 600)
ggsave(Rep_tl_name, Rep_TL, width = 10, height = 10, units = 'in', dpi = 600)

