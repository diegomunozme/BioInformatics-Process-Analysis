# Decipher Biosciences Data Analyst questions
library(tidyverse)
library(ggplot2)
library(stringr)
library(lubridate)
library(dplyr)
################################
# Data Cleaning Data Importing##
################################

df <- read.csv("QuestionA.csv")

df1 <- df %>% dplyr::select(order__order_number, tissue_received_date, order__order_start_date, sample__sample_id, order__tnp,
                     sample_status, qc_status, pathology_lab)

##############################################################
##    Creating a Data Pipeline to track errors              ## 
##############################################################

df30 <- df %>%   
  group_by(order__order_number) %>% 
  arrange(tissue_received_date)  %>% mutate(tissue_received_date = as.Date(tissue_received_date, "%m/%d/%y")) %>% 
  mutate(blue_line = ifelse(grepl("R02",sample__sample_id),'1','0'), 
         red_line = ifelse(tissue_received_date != lag(tissue_received_date, default = first(tissue_received_date)), '1','0'), 
         yellow_line = ifelse(sample__sample_id != lag(sample__sample_id, default = first(sample__sample_id)), '1','0'),
         success = ifelse(grepl("PASS", qc_status),'1','0'))



# allows user input to show frequency per success, or failure
# 
select_Step = "red_line_mean"

success_means <- df %>%   
  group_by(order__order_number) %>% 
  arrange(tissue_received_date)  %>% mutate(tissue_received_date = as.Date(tissue_received_date, "%m/%d/%y")) %>% 
  mutate(blue_line = ifelse(grepl("R02",sample__sample_id),'1','0'), 
         red_line = ifelse(tissue_received_date != lag(tissue_received_date, default = first(tissue_received_date)), '1','0'), 
         yellow_line = ifelse(sample__sample_id != lag(sample__sample_id, default = first(sample__sample_id)), '1','0'),
         success = ifelse(grepl("PASS", qc_status),'1','0')) %>% dplyr::select(order__order_number,blue_line, red_line, yellow_line, success) %>% filter(success == 1) %>% ungroup() %>%   
  mutate(red_line1 = as.numeric(red_line),
         blue_line = as.numeric(blue_line), 
         yellow_line = as.numeric(yellow_line)) %>% 
  summarise(blue_line_mean = mean(blue_line), 
            red_line_mean = mean(red_line1), 
            yellow_line_mean = mean(yellow_line)) 
success_means


failed_means <- df %>%   
  group_by(order__order_number) %>% 
  arrange(tissue_received_date)  %>% mutate(tissue_received_date = as.Date(tissue_received_date, "%m/%d/%y")) %>% 
  mutate(blue_line = ifelse(grepl("R02",sample__sample_id),'1','0'), 
         red_line = ifelse(tissue_received_date != lag(tissue_received_date, default = first(tissue_received_date)), '1','0'), 
         yellow_line = ifelse(sample__sample_id != lag(sample__sample_id, default = first(sample__sample_id)), '1','0'),
         success = ifelse(grepl("PASS", qc_status),'1','0')) %>% dplyr::select(order__order_number,blue_line, red_line, yellow_line, success) %>% filter(success == 0) %>% ungroup() %>%   
  mutate(red_line1 = as.numeric(red_line),
         blue_line = as.numeric(blue_line), 
         yellow_line = as.numeric(yellow_line)) %>% 
  summarise(blue_line_mean = mean(blue_line), 
            red_line_mean = mean(red_line1), 
            yellow_line_mean = mean(yellow_line)) %>% dplyr::select(select_Step)
failed_means



######################
# Visualizing errors #
######################
# Venn Diagram 
library(VennDiagram)
library(tidyverse)
library(limma)
library(tm)
library(proustr)
library(dplyr)

vd_df <- df %>%   
  group_by(order__order_number) %>% 
  arrange(tissue_received_date)  %>% mutate(tissue_received_date = as.Date(tissue_received_date, "%m/%d/%y")) %>% 
  mutate(blue_line = ifelse(grepl("R02",sample__sample_id),'1','0'), 
         red_line = ifelse(tissue_received_date != lag(tissue_received_date, default = first(tissue_received_date)), '1','0'), 
         yellow_line = ifelse(sample__sample_id != lag(sample__sample_id, default = first(sample__sample_id)), '1','0'),
         success = ifelse(grepl("PASS", qc_status),'1','0')) %>% arrange(order__order_number) %>% filter(success == 1) %>% ungroup() %>% 
  dplyr::select(red_line, yellow_line, blue_line)


attach(vd_df)
red <- (red_line >= 1)
blue <- (blue_line >= 1)
yellow <- (yellow_line >= 1)
vd_df1 <- cbind(red, blue, yellow)
a_1V <- vennCounts(vd_df1)


#Preparing the Data
vennDiagram(a_1V, 
            circle.col = c("red","blue","yellow4"))




###############
# Question A2 #
###############

top_5_pathology_lab_fails <- df30 %>%  
  filter(success == 0) %>% 
  dplyr::select(pathology_lab, order__order_number) %>% 
  group_by(pathology_lab) %>% 
  count() %>%
  filter(n >= 10) %>% 
  arrange(desc(n))
##################################### 
#  Writing a Machine Learning Script to figure
#  out what variables are most important for 
#  predicting a failed test or not 
#  QUESTION  A.3 
######################################
library(caret)
library(caTools)
library(MASS)
library(VIF)  
library(car)

# Pre-Processing the data for logistic regression
df30$success = as.numeric(df30$success)
df30$blue_line = as.numeric(df30$blue_line)
df30$red_line = as.numeric(df30$red_line)
df30$yellow_line = as.numeric(df30$yellow_line)

set.seed(642)
split = caTools::sample.split(df30$success, SplitRatio = 0.65) 
Test = subset(df30, split==FALSE)
Train = subset(df30, split==TRUE)
 


Model1 = glm(success ~ tissue_received_date + order__primary_gleason + order__secondary_gleason +
               total_accessions + total_accessions_failed + as.factor(sample_type) +
               pathology_primary_gleason + pathology_secondary_gleason + as.numeric(tumor_length) + as.numeric(perc_benign_glands) + 
               rna_260_280 + rna_concentration__ng_ul_ + cdna_concentration__ng_ul_ + cdna_260_280 + 
               percent_present + blue_line + yellow_line + red_line, data = Train, family = binomial("logit")) 
summary(Model1) #Use stepAIC to remove all non significant variables and reduce collinearity

stepwise_model = Model1 %>% stepAIC(direction = "backward", trace = FALSE)
coef(stepwise_model)
car::vif(stepwise_model) #removing yellow_ine to minimize collinearity

stepwise_model2 = glm(success~ tissue_received_date + total_accessions_failed + pathology_primary_gleason + cdna_concentration__ng_ul_
                      + percent_present + blue_line, data = Train, family = binomial("logit"))
summary(stepwise_model2)
coef(stepwise_model2)
car::vif(stepwise_model2) #removing yellow_line to minimize collinearity
anova(stepwise_model2)

predict_accuracy = predict(stepwise_model2, newdata=Test, type="response")
table(Test$success, predict_accuracy > .5)
(50+46)/(50+46+6+7)  
#88% accuracy on the second logistic regression model