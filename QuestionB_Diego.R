# Decipher Biosciences Data Analyst questions
# Question B

library(tidyverse)
library(ggplot2)
library(stringr)
library(lubridate)
library(ICC)
library(psych)

QuestionB_exprs <- read_csv("QuestionB_exprs.csv")
QuestionB_main <- read_csv("QuestionB_main.csv")



##################
# Question B.2   #
##################
# calculate ICC using R package and data preparation
df1 = QuestionB_main %>% filter(pipeline == "New") %>% 
  dplyr::select(material_id, key_sig_score)


ICC(t(df1))

##########
# Part B #
##########

b_2_new1 = (QuestionB_main %>% filter(pipeline == "New") %>%  
             group_by(key_sig_class, material_id, pipeline) %>% summarise(n = n()))

b_2_old1 = (QuestionB_main %>% filter(pipeline == "Original") %>%  
             group_by(key_sig_class, material_id, pipeline) %>% summarise(n = n()))

b_2_outer_join1 = merge(b_2_new1, b_2_old1, by = c("material_id", "key_sig_class"), all = TRUE) #table for total errors
b_2_outer_join2 = merge(b_2_new1, b_2_old1, by = c("material_id", "key_sig_class"), all.x = TRUE) # since n.x = 3 but NA, I can deduce
                                                                                                  # There is a concordance error
                                                                                                  # between pipelines



b_2_answer <- (nrow(b_2_outer_join2) - length(unique(QuestionB_main$material_id)))

b_2_answer1 <- (1 - (as.data.frame(b_2_answer)/length(unique(QuestionB_main$material_id))))
b_2_answer1 #we had a total of 96% concordance in this case study only 17 had an error between old and new pipelines


##########################################################
# Visualizing concordance, repeatability, total_errors   #
##########################################################

b_3 <- b_2_outer_join1 %>% group_by(material_id)  %>% 
  mutate(total_errors = ifelse(key_sig_class != lag(key_sig_class, default = first(key_sig_class)), '1','0'),
         concordance = ifelse(pipeline.x != lag(pipeline.x, default = first(pipeline.x)), '1','0'), 
         repeatability = ifelse(n.x == 2, '1','0'))
b_3$concordance[is.na(b_3$concordance)] <- 1
b_3$repeatability[is.na(b_3$repeatability)] <- 0

#Visualization

b_3_plot <- b_3 %>% dplyr::select(material_id ,total_errors, concordance, repeatability, n.x)

b_3_plot$material_id = as.numeric(b_3_plot$material_id)

#Dynamic visual, made as a 
# first step for the shiny tool, please 
#jump to the shiny tool for full visuals
SelectKPI = c(2)
SelectKPI = switch(SelectKPI, "concordance", "repeatability")


b_3_plot1 <- b_3_plot %>% ggplot(aes(x=!!ensym(SelectKPI), fill = (material_id))) + 
  geom_bar(stat = "count") + 
  theme_classic() + 
  geom_text(aes(label = !!ensym(SelectKPI)), y=-.5) +
  scale_x_discrete(name =(SelectKPI)  , labels = c("Pass", "Fail")) + 
   ggtitle(paste("Count of", SelectKPI, "errors")) + theme(legend.position = "none")
  
b_3_plot1



######################
##                   #
##     Part B4       #
##                   #
##                   #
######################

library(caret)
library(caTools)
set.seed(429)

QuestionB_exprs <- read_csv("QuestionB_exprs.csv")
QuestionB_main <- read_csv("QuestionB_main.csv")
split = sample.split(QuestionB_exprs, SplitRatio = 0.70) 
quesB_visuals = subset(QuestionB_exprs, split==FALSE)


select_Pipeline = 'F5DD40DE223'
                   # F981070B6D0
                   # AC579CCFDC0

line.stuff = QuestionB_main %>% dplyr::select(id, key_sig_score) %>% filter(id == select_Pipeline)
  
trial1 <- QuestionB_exprs %>% select(select_Pipeline) 
trail1 <- as.numeric(unlist(trial1))



# Calculating Z-Score 
z_score12 <- line.stuff  %>% 
  mutate(zscore = (key_sig_score - mean(trail1)/sd(trail1)
  )) %>% select(zscore)
z_score12
line.stuff$key_sig_score
mean(trail1)/sd(trail1)

####################
# Displaying Values#
####################

his_B4 <- ggplot(QuestionB_exprs, aes(x=!!ensym(select_Pipeline))) + 
  geom_density(fill = "blue", 
               alpha = .2) + 
  geom_vline(xintercept = line.stuff$key_sig_score,
             color = 'red', size = 1) + 
  scale_x_continuous(name = "Sig Score Expression", limits = c(-2, 2)) + scale_color_brewer(palette="Accent") + 
  theme_classic() +
  geom_text(aes(x=line.stuff$key_sig_score, y=1),
            label= round(line.stuff$key_sig_score, 4), 
            hjust = 1, 
            size = 7) + 
  theme(legend.position = "none") + 
  guides(y = "none")
his_B4

