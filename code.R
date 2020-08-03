
#title: "Interview Questions"
#author: "Rushikesh Narhare"

# Setup
library(tidyverse)
library(scales)

patients <- read.csv("./massandopolis/patients.csv")
conditions <- read.csv('./massandopolis/conditions.csv')
encounters <- read.csv('./massandopolis/encounters.csv')
procedures <- read.csv('./massandopolisprocedures.csv')
## Q1: How many Males are in the dataset?
male <- filter(patients, GENDER =='M')
print(nrow(male))
#4404

## Q2: What is the average number of unique condition descriptions per patient?
unique_pat_desc <- conditions %>% distinct(conditions$PATIENT, conditions$DESCRIPTION) 
unique_patient <- conditions %>% distinct(conditions$PATIENT)
avg_desc <- nrow(unique_pat_desc)/nrow(unique_patient)
print(avg_desc)
#10.56139

## Q3:Some encounters have more than one procedure, How many females have an encounter like this? 
join_pat_pro <- merge(patients, procedures, by.x = "Id", by.y = "PATIENT")
female <- filter(join_pat_pro, GENDER =='F')
counter <- aggregate(data.frame(count = female$ENCOUNTER), list(value = female$ENCOUNTER,female$Id), length)
female_greater <- filter(counter, count > 1)
female_count <- female_greater %>% distinct(female_greater$Group.2)
print(nrow(female_count))
#3032


## Q4: For Christina143 Langworth352, use the condition table and encounters table to recreate this image as a pdf (width = 6, height = 6). Encounters start date comes from the encounters table
p_christina <- filter(patients, FIRST =='Christina143')
christina_id <- p_christina$Id
encounter_christina <- filter(encounters, encounters$PATIENT ==christina_id)
merged_conditions_encounter <- merge(conditions, encounter_christina, by.x="ENCOUNTER", by.y = "Id")
final_df <- data.frame(merged_conditions_encounter$START.y, merged_conditions_encounter$DESCRIPTION.x)
final_df$merged_conditions_encounter.START.y <- substr(final_df$merged_conditions_encounter.START.y, 1,4)
ggplot(final_df, aes(x=final_df$merged_conditions_encounter.START.y, y=final_df$merged_conditions_encounter.DESCRIPTION.x, width = 6, height = 6, color=merged_conditions_encounter$DESCRIPTION.x)) + geom_point() + theme(legend.position = "none") + labs(x = "ENCOUNTER_START_DATE", y = "DESCRIPTION") 
ggsave("plot.pdf", width = 6, height = 6)
