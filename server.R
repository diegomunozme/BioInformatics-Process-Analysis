library(shiny)
library(maps)
library(mapproj)
library(quantmod)
library(ggplot2)
library(leaflet)
library(dplyr)
library(tidygeocoder)
library(ggmap)
library(reshape2)
library(treemapify)
library(tidyverse)
library(caTools)
library(car)
library(MASS)
library(limma)


#####################
#Data Pre-Processing#
#####################
QuestionB_exprs <- read_csv("QuestionB_exprs.csv")
QuestionB_main <- read_csv("QuestionB_main.csv")
df <- read.csv("QuestionA.csv")
#######################################
# DATA PRE-PROCESSING FOR CONCORDANCE #
# AND REPEATABILIT TOOL               #
#######################################
# If the pipelines have 100% concordance then
# splitting and rejoining the data should have no 
# effect on the number rows of the dataset

b_2_new1 = (QuestionB_main %>% filter(pipeline == "New") %>%  
              group_by(key_sig_class, material_id, pipeline) %>% summarise(n = n()))

b_2_old1 = (QuestionB_main %>% filter(pipeline == "Original") %>%  
              group_by(key_sig_class, material_id, pipeline) %>% summarise(n = n()))

b_2_outer_join1 = merge(b_2_new1, b_2_old1, by = c("material_id", "key_sig_class"), all = TRUE)
b_2_outer_join2 = merge(b_2_new1, b_2_old1, by = c("material_id", "key_sig_class"), all.x = TRUE) 
# since n.x = 3 but NA, I can deduce
# There is a concordance error
# between pipelines

# 2 extra rows = 2 errors (for b_2_outer_join1), one discrepency between old and new pipeline (concordance)
# another discrepency within a triplicate run (repeatability)
b_2_answer <- (nrow(b_2_outer_join2) - length(unique(QuestionB_main$material_id)))

# Calculate the concordance 1-(total_error)/(unique_Material_ID's)
b_2_answer1 <- (1 - (as.data.frame(b_2_answer)/length(unique(QuestionB_main$material_id))))*100

b_2_answer1 #we had a total of 92% concordance in this case study

b_3 <- b_2_outer_join1 %>% group_by(material_id)  %>% 
  mutate(total_errors = ifelse(key_sig_class != lag(key_sig_class, default = first(key_sig_class)), '1','0'),
         Concordance = ifelse(pipeline.x != lag(pipeline.x, default = first(pipeline.x)), '1','0'), 
         Repeatability = ifelse(n.x == 2, '1','0'))
b_3$Concordance[is.na(b_3$Concordance)] <- 1
b_3$Repeatability[is.na(b_3$Repeatability)] <- 0
b_3_plot <- b_3 %>% dplyr::select(material_id ,total_errors, Concordance, Repeatability, n.x)




shinyServer(function(input, output) {
  ########################################
  ##  RESCUE STEP KPI's + VENN DIAGRAM  ##  
  ##         QUESTION 1A                ##
  ########################################
  # Function to calculate mean success
  succ_mean_func <- function(show_mean_succ) {
    select_Step <- input$error_mean
    
    box1 <- valueBox(df %>%   
                       group_by(order__order_number) %>% 
                       arrange(tissue_received_date)  %>% mutate(tissue_received_date = as.Date(tissue_received_date, "%m/%d/%y")) %>% 
                       mutate(blue_line = ifelse(grepl("R02",sample__sample_id),'1','0'), 
                              red_line = ifelse(tissue_received_date != lag(tissue_received_date, default = first(tissue_received_date)), '1','0'), 
                              yellow_line = ifelse(sample__sample_id != lag(sample__sample_id, default = first(sample__sample_id)), '1','0'),
                              success = ifelse(grepl("PASS", qc_status),'1','0')) %>% dplyr::select(order__order_number,blue_line, red_line, yellow_line, success) %>% filter(success == 1) %>% ungroup() %>%   
                       mutate(red_line1 = as.numeric(red_line),
                              blue_line = as.numeric(blue_line), 
                              yellow_line = as.numeric(yellow_line)) %>% 
                       summarise(blue_line_mean = round(mean(blue_line),3), 
                                 red_line_mean = round(mean(red_line1),3), 
                                 yellow_line_mean = round(mean(yellow_line),3)) %>% dplyr::select(select_Step), "Mean Steps Per Success")
    return(box1)
    
  }
  
  fail_mean_func <- function(show_mean_fail) {
    select_Step <- input$error_mean
    
    box2 <- valueBox(df %>%   
                       group_by(order__order_number) %>% 
                       arrange(tissue_received_date)  %>% mutate(tissue_received_date = as.Date(tissue_received_date, "%m/%d/%y")) %>% 
                       mutate(blue_line = ifelse(grepl("R02",sample__sample_id),'1','0'), 
                              red_line = ifelse(tissue_received_date != lag(tissue_received_date, default = first(tissue_received_date)), '1','0'), 
                              yellow_line = ifelse(sample__sample_id != lag(sample__sample_id, default = first(sample__sample_id)), '1','0'),
                              success = ifelse(grepl("PASS", qc_status),'1','0')) %>% dplyr::select(order__order_number,blue_line, red_line, yellow_line, success) %>% filter(success == 0) %>% ungroup() %>%   
                       mutate(red_line1 = as.numeric(red_line),
                              blue_line = as.numeric(blue_line), 
                              yellow_line = as.numeric(yellow_line)) %>% 
                       summarise(blue_line_mean = round(mean(blue_line),3), 
                                 red_line_mean = round(mean(red_line1),3), 
                                 yellow_line_mean = round(mean(yellow_line),3)) %>% dplyr::select(select_Step), "Mean Steps Per Failure")
    return(box2)
    
  }
  
  venn_PN_func <- function(show_Venn) {
    venn_filter <- input$venn_PN
    
    vd_df <- df %>%   
      group_by(order__order_number) %>% 
      arrange(tissue_received_date)  %>% mutate(tissue_received_date = as.Date(tissue_received_date, "%m/%d/%y")) %>% 
      mutate(blue_line = ifelse(grepl("R02",sample__sample_id),'1','0'), 
             red_line = ifelse(tissue_received_date != lag(tissue_received_date, default = first(tissue_received_date)), '1','0'), 
             yellow_line = ifelse(sample__sample_id != lag(sample__sample_id, default = first(sample__sample_id)), '1','0'),
             success = ifelse(grepl("PASS", qc_status),'1','0')) %>% arrange(order__order_number) %>% filter(success == venn_filter) %>% ungroup() %>% 
      dplyr::select(red_line, yellow_line, blue_line)
    
    
    
    
    attach(vd_df)
    red <- (red_line >= 1)
    blue <- (blue_line >= 1)
    yellow <- (yellow_line >= 1)
    vd_df1 <- cbind(red, blue, yellow)
    a_1V <- vennCounts(vd_df1)
    return(vennDiagram(a_1V, 
                       circle.col = c("red","blue","yellow4")))
  }
  
  #KPI Box to visualize average steps per success
  output$success_means <- renderValueBox({
    succ_mean_func(input$error_mean)
  })
  
  #Visualize average steps per failure
  output$fail_means <- renderValueBox({
    fail_mean_func(input$error_mean)
  })
  
  
  output$venn_D <- renderPlot({
    venn_PN_func(input$venn_PN)
    })
  
  ######################################
  # CONCORDANCE & REPEATABILITY KPI's ##
  #       QUESTION B.2 & B.3          ##
  ######################################
  
  value_conc_visuals <- function(conc_visuals) {
    
    SelectKPI <- input$var
    
    b_3_plot1 <- b_3_plot %>% ggplot(aes(x=!!ensym(SelectKPI), fill = material_id)) + 
      geom_bar(stat = "count") + 
      theme_classic() + 
      geom_text(aes(label = !!ensym(SelectKPI)), y=-.5) +
      scale_x_discrete(name =(SelectKPI)  , labels = c("Pass", "Fail")) + 
      theme(legend.position = "none") + ggtitle(paste("Count of", SelectKPI, "errors"))
    return(b_3_plot1)
    
  }
  
  #Plot
  output$concrep <- renderPlot({
    value_conc_visuals(input$var)
  })
  # ValueBoxes
  output$total_Errors <- renderValueBox({
    valueBox(paste(b_2_answer), "Total Concordance Error(s)", color = "blue")
             })
  output$concordance_Perc <- renderValueBox({
    valueBox(paste(round(b_2_answer1, 1), "%"), "Pipeline Class Accuracy", color = "blue")
  })
  ################# 
  # Z-SCORE TOOL ##
  #################
  
  text_input_func <- function(select_PipeID) {
    
    select_Pipeline <- input$textInput1
    line.stuff = QuestionB_main %>%  dplyr::select(id, key_sig_score) %>% filter(id == select_Pipeline)
    
    trial1 <- QuestionB_exprs %>% dplyr::select(select_Pipeline) 
    trail1 <- as.numeric(unlist(trial1))
    
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
      theme(legend.position = "none")
    return(his_B4)}
  
  
  value_box_func <- function(show_zScore) {
    select_Pipeline <- input$textInput1
    line.stuff <- QuestionB_main %>% dplyr::select(id, key_sig_score) %>% filter(id == select_Pipeline)
    
    trial1 <- QuestionB_exprs %>% dplyr::select(select_Pipeline) 
    trail1 <- as.numeric(unlist(trial1))
    
    g <- valueBox(line.stuff  %>%
                    mutate(zscore = round(key_sig_score - mean(trail1)/sd(trail1), 3)) %>%
                    dplyr::select(zscore), "Z Score")
    return(g)
    
  }
  
  #############
  # Value Box #
  #############
  
  output$z_Value <- renderValueBox({
    value_box_func(input$textInput1)
  })
 
 
 
 output$hist <- renderPlot({
   text_input_func(input$textInput1)
 })
 ##########################
 # End of Server Function #
 ##########################
  })
