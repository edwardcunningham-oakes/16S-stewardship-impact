#Set working directory
setwd("~/Documents/16S manuscript September 2024/Resubmission - eBiomedicine/")

#Load libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(firatheme)
library(patchwork)
library(svglite)
library(gtable)
library(gtExtras)
library(ggridges)
library(viridis)
library(hrbrthemes)
library(pals)

#----SUPPLEMENTARY FILE 1----
#----Load original clinical metadata
Carlisle_et_al_supp_1 <- read.csv2("Carlisle_et_al._Supplementary_File_1.csv", sep = ",")


#----SUPPLEMENTARY FILE 2----
#----Clean whitespace and fix known typos
Carlisle_et_al_supp_2 <- Carlisle_et_al_supp_1 %>%
  mutate(
    sample_type = str_trim(sample_type),
    sample_type = str_replace_all(sample_type, regex("TISSE", ignore_case = TRUE), "TISSUE"),
    sample_type = str_replace_all(sample_type, regex("CAREBRAL", ignore_case = TRUE), "CEREBRAL"),
    sample_type = str_replace_all(sample_type, regex("INTERCRANIAL", ignore_case = TRUE), "INTRACRANIAL"),
    sample_type = str_replace_all(sample_type, regex("PLERUAL", ignore_case = TRUE), "PLEURAL"),
    sample_type = str_replace_all(sample_type, regex("BROCHOALEVOLAR", ignore_case = TRUE), "BRONCHOALVEOLAR")
  )

# Group sample types into broader categories
Carlisle_et_al_supp_2 <- Carlisle_et_al_supp_2 %>%
  mutate(
    sample_group = case_when(
      str_detect(sample_type, regex("CSF", ignore_case = TRUE)) ~ "CSF",
      str_detect(sample_type, regex("JOINT|KNEE|HIP|ANKLE|ASPIRATE|EFFUSION|SYNOVIAL", ignore_case = TRUE)) ~ "Joint Fluid",
      str_detect(sample_type, regex("PUS|ABSCESS|CEREBRAL|INTRACRANIAL|FRONTAL|LIP|LIVER", ignore_case = TRUE)) ~ "Pus/Abscess",
      str_detect(sample_type, regex("^TISSUE|SYNOVIAL TISSUE", ignore_case = TRUE)) ~ "Tissue",
      str_detect(sample_type, regex("FLUID", ignore_case = TRUE)) ~ "Fluid (Other)",
      str_detect(sample_type, regex("SWAB|NASAL WASHOUT|IMPRESS", ignore_case = TRUE)) ~ "Swab",
      str_detect(sample_type, regex("BLOOD CULTURE", ignore_case = TRUE)) ~ "Blood Culture",
      str_detect(sample_type, regex("BRONCHIAL|BRONCHOALVEOLAR|LAVAGE", ignore_case = TRUE)) ~ "Respiratory",
      str_detect(sample_type, regex("HICKMAN|LINE TIP", ignore_case = TRUE)) ~ "Line-related",
      is.na(sample_type) | sample_type == "" ~ "Unknown",
      TRUE ~ "Other"
    )
  )

#Sample genus count
Carlisle_et_al_supp_2 <- Carlisle_et_al_supp_2 %>%
  mutate(
    genus = case_when(
      str_detect(organism..one.per.line., regex("Acinetobacter", ignore_case = TRUE)) ~ "Acinetobacter",
      str_detect(organism..one.per.line., regex("Aggregatibacter", ignore_case = TRUE)) ~ "Aggregatibacter",
      str_detect(organism..one.per.line., regex("Akkermansia", ignore_case = TRUE)) ~ "Akkermansia",
      str_detect(organism..one.per.line., regex("Anaerococcus", ignore_case = TRUE)) ~ "Anaerococcus",
      str_detect(organism..one.per.line., regex("Bacteroides", ignore_case = TRUE)) ~ "Bacteroides",
      str_detect(organism..one.per.line., regex("Brevundimonas", ignore_case = TRUE)) ~ "Brevundimonas",
      str_detect(organism..one.per.line., regex("Campylobacter", ignore_case = TRUE)) ~ "Campylobacter",
      str_detect(organism..one.per.line., regex("Capnocytophaga", ignore_case = TRUE)) ~ "Capnocytophaga",
      str_detect(organism..one.per.line., regex("Corynebacterium", ignore_case = TRUE)) ~ "Corynebacterium",
      str_detect(organism..one.per.line., regex("Cutibacterium", ignore_case = TRUE)) ~ "Cutibacterium",
      str_detect(organism..one.per.line., regex("Enhydrobacter", ignore_case = TRUE)) ~ "Enhydrobacter",
      str_detect(organism..one.per.line., regex("Enterobactericeae", ignore_case = TRUE)) ~ "Enterobactericeae",
      str_detect(organism..one.per.line., regex("Enterococcus", ignore_case = TRUE)) ~ "Enterococcus",
      str_detect(organism..one.per.line., regex("Eubacterium", ignore_case = TRUE)) ~ "Eubacterium",
      str_detect(organism..one.per.line., regex("Fusobacterium", ignore_case = TRUE)) ~ "Fusobacterium",
      str_detect(organism..one.per.line., regex("Haemophilus", ignore_case = TRUE)) ~ "Haemophilus",
      str_detect(organism..one.per.line., regex("Hazenella", ignore_case = TRUE)) ~ "Hazenella",
      str_detect(organism..one.per.line., regex("Jeotgalicoccus", ignore_case = TRUE)) ~ "Jeotgalicoccus",
      str_detect(organism..one.per.line., regex("Johnsonella", ignore_case = TRUE)) ~ "Johnsonella",
      str_detect(organism..one.per.line., regex("Kaistobacter", ignore_case = TRUE)) ~ "Kaistobacter",
      str_detect(organism..one.per.line., regex("Mogibacterium", ignore_case = TRUE)) ~ "Mogibacterium",
      str_detect(organism..one.per.line., regex("Moraxella", ignore_case = TRUE)) ~ "Moraxella",
      str_detect(organism..one.per.line., regex("Mycobacterium", ignore_case = TRUE)) ~ "Mycobacterium",
      str_detect(organism..one.per.line., regex("Mycobacteroides", ignore_case = TRUE)) ~ "Mycobacteroides",
      str_detect(organism..one.per.line., regex("Neisseria", ignore_case = TRUE)) ~ "Neisseria",
      str_detect(organism..one.per.line., regex("Parvimonas", ignore_case = TRUE)) ~ "Parvimonas",
      str_detect(organism..one.per.line., regex("Prevotella", ignore_case = TRUE)) ~ "Prevotella",
      str_detect(organism..one.per.line., regex("Propionibacterium", ignore_case = TRUE)) ~ "Propionibacterium",
      str_detect(organism..one.per.line., regex("Proteus", ignore_case = TRUE)) ~ "Proteus",
      str_detect(organism..one.per.line., regex("Pseudomonas", ignore_case = TRUE)) ~ "Pseudomonas",
      str_detect(organism..one.per.line., regex("Rothia", ignore_case = TRUE)) ~ "Rothia",
      str_detect(organism..one.per.line., regex("Staphylococcus", ignore_case = TRUE)) ~ "Staphylococcus",
      str_detect(organism..one.per.line., regex("Streptococcus", ignore_case = TRUE)) ~ "Streptococcus",
      str_detect(organism..one.per.line., regex("Treoponema", ignore_case = TRUE)) ~ "Treoponema",
      str_detect(organism..one.per.line., regex("Veillonella", ignore_case = TRUE)) ~ "Veillonella"))
      

classify_impact <- function(outcome) {
  if (is.na(outcome) || grepl("no infection|unrelated|unclear", outcome, ignore.case = TRUE)) {
    return("Unclear")
  } else if (grepl("stop Abx|confirms treatment choice|confirms non-infectious cause", outcome, ignore.case = TRUE)) {
    return("Rationalisation")
  } else if (grepl("start Abx therapy|change of Abx therapy", outcome, ignore.case = TRUE)) {
    return("Escalation")
  } else {
    return("Unclear")
  }
}

#Apply classifier
Carlisle_et_al_supp_2$impact_category <- sapply(Carlisle_et_al_supp_2$outcome_post_16S, classify_impact)

#Converting age to numeric and create age groups
Carlisle_et_al_supp_2$patient_age <- as.numeric(Carlisle_et_al_supp_2$patient_age)

Carlisle_et_al_supp_2 <- Carlisle_et_al_supp_2 %>%
  mutate(
    age_group = case_when(
      is.na(patient_age)        ~ NA,
      patient_age >= 0  & patient_age <= 4   ~ "00-04",
      patient_age >= 5  & patient_age <= 10  ~ "05-10",
      patient_age >= 11  & patient_age <= 15  ~ "11-15",
      patient_age >= 16  & patient_age <= 20  ~ "15-20",
      patient_age >= 21  & patient_age <= 25  ~ "21-25",
      patient_age >= 26  & patient_age <= 30  ~ "25-30",
      patient_age >= 31  & patient_age <= 35  ~ "31-35",
      patient_age >= 36  & patient_age <= 40  ~ "36-40",
      patient_age >= 41  & patient_age <= 45  ~ "41-45",
      patient_age >= 46  & patient_age <= 50  ~ "46-50",
      patient_age >= 51  & patient_age <= 55  ~ "51-55",
      patient_age >= 56  & patient_age <= 60  ~ "56-60",
      patient_age >= 61  & patient_age <= 65  ~ "61-65",
      patient_age >= 66  & patient_age <= 70  ~ "66-70",
      patient_age >= 71  & patient_age <= 75  ~ "71-75",
      patient_age >= 76  & patient_age <= 80  ~ "76-90",
      patient_age >= 81  & patient_age <= 85  ~ "81-85",
      patient_age >= 86  & patient_age <= 90  ~ "86-90",
    ),
    actionable = impact_category %in% c("Rationalisation", "Escalation"),
    rationalisation = impact_category == "Rationalisation"
  )



#Generate inputs for downstream tables and analysis----
sex_data <- Carlisle_et_al_supp_2 %>% distinct(unique_id, .keep_all = TRUE) %>% 
  filter(!is.na(age_group)) %>%
  group_by(age_group) %>%
  summarise(
    total = n(),
    m = sum(sex == "male", na.rm = TRUE),
    f = sum(sex == "female", na.rm = TRUE),
  ) %>%
  mutate(
    m_perc = round(m / total * 100, 1),
    f_perc = round(f / total * 100, 1)
  )

Carlisle_et_al_supp_2 <- Carlisle_et_al_supp_2 %>% distinct(unique_id, .keep_all = TRUE) %>% 
  mutate(
    age_group_nhs = case_when(
      is.na(patient_age)        ~ NA,
      patient_age >= 0  & patient_age <= 4 ~ "00-04",
      patient_age >= 5  & patient_age <= 11 ~ "05-11",
      patient_age >= 12  & patient_age <= 17 ~ "12-17",
      patient_age >= 18  & patient_age <= 25 ~ "18-25",
      patient_age >= 26  & patient_age <= 34 ~ "26-34",
      patient_age >= 35  & patient_age <= 49 ~ "35-49",
      patient_age >= 50  & patient_age <= 69 ~ "50-69",
      patient_age >= 70  & patient_age <= 79 ~ "70-79",
      patient_age >= 80  & patient_age <= 89 ~ "80-89",
    ),
    actionable = impact_category %in% c("Rationalisation", "Escalation"),
    rationalisation = impact_category == "Rationalisation"
  )

write_csv(Carlisle_et_al_supp_2, "Carlisle_et_al._Supplementary_File_2.csv")

Histo_data <- Carlisle_et_al_supp_2 %>% distinct(unique_id, .keep_all = TRUE) %>% 
  filter(!is.na(age_group_nhs)) %>%
  group_by(age_group_nhs) %>%
  summarise(
    total = n(),
    m = sum(sex == "male", na.rm = TRUE),
    f = sum(sex == "female", na.rm = TRUE),
  ) %>%
  mutate(
    m_perc = round(m / total * 100, 1),
    f_perc = round(f / total * 100, 1)
  )

genus <- Carlisle_et_al_supp_2 %>%
  filter(!is.na(genus)) %>%           
  count(genus, name = "n") %>% 
  mutate(percentage = round(n / sum(n) * 100, 1)) %>% 
  arrange(desc(percentage))
      

genus_by_location <- Carlisle_et_al_supp_2 %>%
  filter(!is.na(genus), !is.na(Patient.location)) %>%           
  group_by(Patient.location, genus) %>%                         
  summarise(n = n(), .groups = "drop") %>%                  
  group_by(Patient.location) %>%
  mutate(
    total = sum(n),                                             
    percentage = round(n / total * 100, 1)                      
  )

genus_by_sample <- Carlisle_et_al_supp_2 %>%
  filter(!is.na(genus), !is.na(sample_group)) %>%           
  group_by(sample_group, genus) %>%                         
  summarise(n = n(), .groups = "drop") %>%                  
  group_by(sample_group) %>%
  mutate(
    total = sum(n),                                             
    percentage = round(n / total * 100, 1)                      
  )
      
# Group by Immunostatus, age_group, sample_type and summarise
Carlisle_et_al_supp_2_summary <- Carlisle_et_al_supp_2 %>% distinct(unique_id, .keep_all = TRUE) %>% 
  group_by(Immunostatus, patient_age, sample_type) %>%
  summarise(
    n = n(),
    prop_actionable = mean(actionable, na.rm = TRUE),
    prop_rationalisation = mean(rationalisation, na.rm = TRUE),
    .groups = "drop"
  )

Impact <- Carlisle_et_al_supp_2 %>% distinct(unique_id, .keep_all = TRUE) %>% 
  count(outcome_post_16S, name = "n") %>%
  mutate(
    percentage = round(100 * n / sum(n), 1),
    label = paste0(outcome_post_16S, " (", percentage, "%)")
  )

Request <- Carlisle_et_al_supp_2 %>% distinct(unique_id, .keep_all = TRUE) %>% 
  count(reason.for.request, name = "n") %>%
  mutate(
    percentage = round(100 * n / sum(n), 1),
    label = paste0(reason.for.request, " (", percentage, "%)")
  )

#----FIGURES AND TABLES----

#---FIGURE 1----
Figure_1A <- ggplot(sex_data) +
  geom_segment(aes(x = age_group, xend = age_group, y = m, yend = f), color = "grey") +
  geom_point(aes(x = age_group, y = m, color = "Male"), size = 5) +
  geom_point(aes(x = age_group, y = f, color = "Female"), size = 5) +
  coord_flip() +
  theme_fira() +
  theme(legend.position = "left",  axis.text.x = element_text(size = 12, face = "bold"),
                                   axis.text.y = element_text(size = 12, face = "bold"),
        panel.grid.major.y = element_blank(),
        legend.text=element_text(size=12)) +
  scale_color_manual(name = "Sex", values = c(Male = "#b2abd2", Female = "#e08214")) +
  xlab("Age group") +
  ylab("Number of samples")


palette <- c(
  "#0074D9","#39CCCC", "#B10DC9","#F012BE","#85144b","#FF4136","#FF851B","#FFDC00","#3D9970","#2ECC40",""
)

n_categories <- length(unique(Histo_data$age_group_nhs))
palette <- palette[1:n_categories]


  Figure_1B <- ggplot(Histo_data, aes(x=age_group_nhs, y=total, fill=age_group_nhs)) + 
  geom_bar(stat = "identity") + scale_fill_manual(values = palette) +
  theme_fira() +
  coord_flip() +
  labs(
    x = "Number of patients",
    y = "NHS age group") + labs(fill='NHS age group') +
  theme(legend.position = "none", axis.text.x = element_text(size = 12, face = "bold"),
                                   axis.text.y = element_text(size = 12, face = "bold"),
        panel.grid.major.y = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black", linewidth = 1.5),
        panel.border = element_rect(fill = NA, colour = "black", linewidth = 1.5))
  
 location_summary <- Carlisle_et_al_supp_2 %>%
  filter(!is.na(Patient.location)) %>%
  count(Patient.location) %>%
  mutate(percent = round(100 * n / sum(n), 1),
         label = paste0(Patient.location, " (", percent, "%)"))
  
 Figure_1C <- ggplot(location_summary, aes(x = "", y = n, fill = label)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  labs(
    x = NULL, y = NULL,
    fill = "Patient Location (%)",
  ) +
   theme_fira() +
   theme(
    legend.position = "right",
    legend.text=element_text(size=11),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    panel.grid = element_blank()
  )
 
Figure_1 <- (Figure_1A / Figure_1B | Figure_1C) +
  plot_annotation(tag_levels = 'A')
  
# === Export as SVG ===
svglite::svglite("Figure_1.svg", width = 1818 / 72, height = 672 / 72)
plot(Figure_1)
dev.off()

#---FIGURE 2----

# === FIGURE 2A: Reason for 16S request ===

Figure_2A <- ggplot(drop_na(Request), aes(x = reorder(reason.for.request, n), y = percentage, fill = reason.for.request)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(percentage, "%")), hjust = -0.1, size = 6, fontface = "bold") +
  coord_flip() + scale_fill_brewer(palette = "Paired") +
  expand_limits(y = max(Impact$percentage) + 10) +
  theme_fira() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = (element_text(size = 14)),
    axis.text.y = (element_text(size = 14))) +
  labs(
    y = "Percentage of Requests",
    title = "Reason for 16S request"
  )

# === FIGURE 2B: Impact of 16S result ===
Figure_2B <- ggplot(drop_na(Impact), aes(x = reorder(outcome_post_16S, n), y = percentage, fill = outcome_post_16S)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(percentage, "%")), hjust = -0.1, size = 6, fontface = "bold") +
  coord_flip() + scale_fill_brewer(palette = "Spectral") +
  expand_limits(y = max(Impact$percentage) + 10) +
  theme_fira() +
  theme(
    legend.position = "none",
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = (element_text(size = 14)),
    axis.text.y = (element_text(size = 14))) +
  labs(
    y = "Percentage of Cases",
    title = "Impact of 16S result"
  )

# === Combine plots as Figure 2 ===
Figure_2 <- Figure_2A + Figure_2B

# === Export as SVG ===
svglite::svglite("Figure_2.svg", width = 1818 / 72, height = 672 / 72)
print(Figure_2)
dev.off()


#---FIGURE 3----
genus_by_location$genus <- factor(genus_by_location$genus, levels = unique(genus_by_location$genus))

Figure_3A <- ggplot(genus_by_location, aes(x=genus, y=percentage, fill=genus)) + 
  geom_bar(stat = "identity") + scale_fill_manual(values=unname(alphabet2())) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(percentage, "%")), hjust = -0.1, size = 4, fontface = "bold") +
  coord_flip() +
  theme_fira() +
  labs(
    x = "Genus",
    y = "Percentage") + labs(fill='Genus') +
  theme(legend.position = "none", axis.text.x = element_text(size = 12, face = "bold"),
                                   axis.text.y = element_text(size = 12, face = "bold.italic"),
        panel.border = element_rect(fill = NA, colour = "black", linewidth = 1.5)) +
  facet_grid(~Patient.location)
  
Figure_3B <- ggplot(genus_by_sample, aes(x=genus, y=percentage, fill=genus)) + 
  geom_bar(stat = "identity") + scale_fill_manual(values=unname(alphabet2())) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(percentage, "%")), hjust = -0.1, size = 4, fontface = "bold") +
  coord_flip() +
  theme_fira() +
  labs(
    x = "Genus",
    y = "Percentage") + labs(fill='Genus') +
  theme(legend.position = "none", axis.text.x = element_text(size = 12, face = "bold"),
                                   axis.text.y = element_text(size = 12, face = "bold.italic"),
        panel.border = element_rect(fill = NA, colour = "black", linewidth = 1.5)) +
  facet_grid(~sample_group)

Figure_3 <-  Figure_3A / Figure_3B +  plot_annotation(tag_levels = 'A')
svglite::svglite("Figure_3.svg", width = 2444 / 72, height = 912 / 72)
print(Figure_3)
dev.off()

#---TABLES---
#---TABLE 1----
Table_1_raw <- Carlisle_et_al_supp_2 %>%
  count(sample_group, name = "n") %>%
  mutate(
    percentage = round(100 * n / sum(n), 1))

Table_1_raw <- Table_1_raw %>% arrange(desc(n))

Table_1 <- gt(Table_1_raw) %>%
  cols_label(
    sample_group = html("Sample type"),
    n = html("Total"),
  ) %>%
  gt_theme_nytimes() %>%
  tab_header("Table 1: Summary of clinical samples and their site of origin")

#---TABLE 4----
Table_4 <- gt(sex_data) %>%
  cols_label(
    age_group = html("age group"),
    total = html("Total"),
    m = html("Males (n)"),
    m_perc = html("Males (%)"),
    f = html("Females (n)"),
    f_perc = html("Females (%)"),
  ) %>%
  gt_theme_nytimes() %>%
  tab_header("Table 4: Demographics of patients sampled for 16S")


#Make new stewardship category for Table 5
Carlisle_et_al_supp_2$stewardship <- ifelse(Carlisle_et_al_supp_2$impact_category %in% c("Escalation", "Rationalisation"), "Yes", "No")

#----TABLE 5----
immune_table <- table(Carlisle_et_al_supp_2$stewardship, Carlisle_et_al_supp_2$Immunostatus)
immune_stats <- fisher.test(immune_table)

sex_table <- table(drop_na(Carlisle_et_al_supp_2,sex)$stewardship, drop_na(Carlisle_et_al_supp_2,sex)$sex)
sex_stats <- fisher.test(sex_table)

age_table <- table(drop_na(Carlisle_et_al_supp_2,age_group)$stewardship, drop_na(Carlisle_et_al_supp_2,age_group)$age_group)
age_stats <- fisher.test(age_table, simulate.p.value = TRUE, B=10000)


#----SUPPLEMENTARY FILE 3----
write_csv(genus, "Carlisle_et_al._Supplementary_File_3.csv")



