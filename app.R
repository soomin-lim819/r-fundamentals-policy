library(shiny)
library(tidyverse)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(plotly)
library(stats)
library(RColorBrewer)


# Load the dataset initially
setwd("/Users/soom/Desktop/final project")
comp2_add <- read_csv("/Users/soom/Desktop/final project/comp2_add.csv")


df <- comp2_add %>%
  mutate(avgincome1 = case_when(
    avgincome %in% c('1', '2', '3')  ~ '1',
    avgincome %in% c('4', '5')       ~ '2',
    avgincome %in% c('6', '7')       ~ '3',
    avgincome %in% c('8', '9')       ~ '4',
    avgincome %in% c('10', '11')     ~ '5',
    TRUE                        ~ NA_character_  # Handle any unexpected levels
  )) %>%
  mutate(avgincome1 = factor(avgincome1))

subset_data <- df[, c("edu", "age", "v_abort", "v_child", "gender", "religion", "v_parents", "v_chastity", "political", "avgincome1", "married")]

subset_data$age_group <- cut(subset_data$age, 
                             breaks = c(19, 24, 34, 49, 69, Inf),  
                             labels = c("19-24", "25-34", "35-49", "50-69", "70-98"))

edu_labels <- c("Middle School Graduates", "High School Graduates", "Undergraduates", "Graduates")

custom_labels <- c(
  "1" = "Strongly Agree",
  "2" = "Agree",
  "3" = "Disagree",
  "4" = "Strongly Disagree"
  # ... add more labels as needed
)

subset_data <- na.omit(subset_data)


calculate_mode_percentage <- function(x) {
  mode_value <- as.integer(names(sort(table(x), decreasing = TRUE)[1]))
  percentage <- sum(x == mode_value) / length(x) * 100
  return(data.frame(mode_value, percentage))
}


# Define UI for application 
ui <- fluidPage(
  titlePanel("Love in Layers: Mapping Marriage Choices in Korea (2006 - 2022)"),
  
  # Add a tabsetPanel for the "Should We Marry?" tab
  tabsetPanel(
    tabPanel("Is Marriage A Must?",
             navlistPanel(
               "Choose a group",
               tabPanel("Age", plotlyOutput("age_plot")),
               tabPanel("Gender", plotlyOutput("gender_plot")),
               tabPanel("Education", plotlyOutput("edu_plot")),
               tabPanel("Income Level", plotlyOutput("income_plot"))
             )
    ), 
    
    tabPanel("Choosing Partners", 
             navlistPanel(
               "Spouce Selection Criteria",
               tabPanel("Gender", plotlyOutput("select_gender")),
               tabPanel("Marital Status", plotlyOutput("select_married")),
               tabPanel("Education", plotlyOutput("select_edu")),
               tabPanel("Income Level", plotlyOutput("select_income"))
             )
    ), 
    
    tabPanel("Marriage Values", 
             navlistPanel(
               "Values about Marriage Life",
               tabPanel("Pre-marital Chasity", plotlyOutput("heatmap1")),
               tabPanel("Abortion", plotlyOutput("heatmap2")),
               tabPanel("Political Reception", plotlyOutput("heatmap3")),
               tabPanel("Child Support", plotlyOutput("heatmap4"))
             )
    ),
    
    tabPanel("Appendix", 
             navlistPanel(
               tabPanel("Analysis", plotlyOutput("additional_analysis"))
             )
    )
  )
)

# Define server logic required to draw plots
server <- function(input, output) {
  # Your existing server code here...
  
  ### Marriage is a must? - Age comparison
  comp2_add <- comp2_add %>%
    mutate(age_group = cut(age, breaks = c(18, 25, 35, 50, 70, 90), 
                           labels = c("18-24", "25-34", "35-49", "50-69", "70-98")))
  
  age_class_1 <- comp2_add %>%
    filter(!is.na(age_group), !is.na(marrper), age_group != 9, marrper != 9) %>% 
    group_by(marrper, age_group) %>%
    summarize(
      total_marrper_age = n(),
      total_marrper_agec = n() / nrow(comp2_add)
    ) %>%
    group_by(age_group) %>%
    mutate(probability = total_marrper_agec / sum(total_marrper_agec))  
  
  plot_marrper_age <- ggplot(age_class_1, 
                             aes(x = probability, y = age_group, fill = as.factor(marrper))) +
    geom_bar(stat = "identity", position = "stack") +
    labs(title = "Do You think Marriage is a Must?",
         y = "Age Group",
         x = "Probability",
         fill = "Marriage Type") +
    theme_bw() +
    scale_fill_brewer(palette = "PiYG", label = c("Yes, it is a must!", "You'd better to", 
                                                  "No, not necessary!"))
  
 output$age_plot <- renderPlotly({
    plotly_age <- ggplotly(plot_marrper_age) %>% 
      layout(
        legend = list(
          x = 0, y = -0.2, orientation = "h"
        )
      )
    ##  Manually set legend labels...
    legend_labels <- c("Yes, it is a must!", "You'd better to", "No, not necessary!")
    for (i in seq_along(plotly_age$x$data)) {
      plotly_age$x$data[[i]]$name <- legend_labels[i]
    }
    print(plotly_age)
  })
  
  # Gender probability
  gender_class_1 <- comp2_add %>%
    filter(!is.na(gender), !is.na(marrper), marrper != 9) %>% 
    mutate(gender = case_when(
      gender == 1 ~ "Male", gender == 2 ~ "Female", TRUE ~ as.character(gender)
    )) %>% 
    group_by(marrper, gender) %>%
    summarize(
      total_marrper_gender = n(),
      total_marrper_genderc = n() / nrow(comp2_add)
    ) %>%
    group_by(gender) %>%
    mutate(probability1 = total_marrper_genderc / sum(total_marrper_genderc))
  
  # Plot by gender
  plot_marrper_gender <- ggplot(gender_class_1, 
                                aes(x = probability1, y = as.factor(gender), fill = as.factor(marrper))) +
    geom_bar(stat = "identity", position = "stack") +
    labs(y = "Gender",
         x = "Probability",
         fill = "Marriage Type") +
    theme_bw() +
    scale_fill_brewer(palette = "PiYG", label = c("Yes, it is a must!", "You'd better to", 
                                                  "No, not necessary!"))
  
  # Plotly - gender
  output$gender_plot <- renderPlotly({
    plotly_gender <- ggplotly(plot_marrper_gender) %>% 
      layout(
        legend = list(
          x = 0, y = -0.2, orientation = "h"
        )
      )
    ##  Manually set legend labels...
    for (i in seq_along(plotly_gender$x$data)) {
      plotly_gender$x$data[[i]]$name <- legend_labels[i]
    }
    print(plotly_gender)
  })
  
  # Education 
  edu_class_1 <- comp2_add %>%
    filter(!is.na(edu), !is.na(marrper), edu != 9, marrper != 9) %>% 
    group_by(marrper, edu) %>%
    mutate(edu = case_when(
      edu == 1 ~ "Middle Scl", edu == 2 ~ "High Schl", 
      edu == 3 ~ "Undergraduate", edu == 4 ~ "Graduate", TRUE ~ as.character(edu)
    )) %>% 
    summarize(
      total_marrper_edu = n(),
      total_marrper_educ = n() / nrow(comp2_add)
    ) %>%
    group_by(edu) %>%
    mutate(probability = total_marrper_educ / sum(total_marrper_educ))  
  
  # Plot by education
  plot_marrper_edu <- ggplot(edu_class_1, 
                             aes(x = probability, y = edu, fill = as.factor(marrper))) +
    geom_bar(stat = "identity", position="stack") +
    labs(title = "Do You think Marriage is a Must?",
         y = "Education",
         x = "Probability",
         fill = "Marriage Type") +
    theme_bw() +
    scale_fill_brewer(palette = "PiYG", label = c("Yes, it is a must!", "You'd better to", 
                                                  "No, not necessary!"))
  
  # Plotly - education
  output$edu_plot <- renderPlotly({
    plotly_edu <- ggplotly(plot_marrper_edu) %>% 
      layout(
        legend = list(
          x = 0, y = -0.2, orientation = "h"
        )
      )
    ##  Manually set legend labels...
    for (i in seq_along(plotly_edu$x$data)) {
      plotly_edu$x$data[[i]]$name <- legend_labels[i]
    }
    print(plotly_edu)
  })
  
  
  # Income group
  avgincome_class_1 <- comp2_add %>%
    filter(!is.na(avgincome), !is.na(marrper), marrper != 9) %>% 
    mutate(avgincome = case_when(
      avgincome %in% c(1, 2, 3) ~ "Under KRW 2Mil/m",  
      avgincome %in% c(4, 5) ~ "KRW 2M~2.99Milm",   
      avgincome %in% c(6, 7) ~ "KRW 3M~4.99Mil/m",
      avgincome %in% c(8, 9) ~ "KRW 5M~6.99Mil/m",
      avgincome %in% c(10, 11) ~ "Above KRW 7Mil/m",
      TRUE ~ as.character(avgincome))) %>% 
    group_by(marrper, avgincome) %>%
    summarize(
      total_marrper_avgincome = n(),
      total_marrper_avgincomec = n() / nrow(comp2_add)
    ) %>%
    group_by(avgincome) %>%
    mutate(probability = total_marrper_avgincomec / sum(total_marrper_avgincomec))
  
  # Plot by income group
  plot_marrper_avgincome <- ggplot(avgincome_class_1, 
                                   aes(x = probability, y = avgincome, fill = as.factor(marrper))) +
    geom_bar(stat = "identity", position="stack") +
    labs(title = "Do You think Marriage is a Must?",
         y = "Income Level",
         x = "Probability",
         fill = "Marriage Type") +
    theme_bw() +
    scale_fill_brewer(palette = "PiYG", label = c("Yes, it is a must!", "You'd better to", 
                                                  "No, not necessary!"))
  
  # Plotly - income
  output$income_plot <- renderPlotly({ 
    plotly_avgincome <- ggplotly(plot_marrper_avgincome) %>% 
      layout(
        legend = list(
          x = 0, y = -0.2, orientation = "h"))
    ##  Manually set legend labels...
    for (i in seq_along(plotly_avgincome$x$data)) {
      plotly_avgincome$x$data[[i]]$name <- legend_labels[i]
    }
    print(plotly_avgincome)  
  })
  
  
  
  ########## 2nd Question
  # We are now deselecting 2nd preference
  
  mode_percentage_data1 <- subset_data %>%
    group_by(edu, religion) %>%
    summarise(mode_data = calculate_mode_percentage(v_chastity)) %>%
    ungroup() %>%
    tidyr::unnest(mode_data)
  
  #This is the heatmap
  output$heatmap1 <- renderPlotly({ 
    heatmap_interactive <- plot_ly(
      data = mode_percentage_data1,
      x = ~edu,
      y = ~religion,
      z = ~percentage,  # Use calculated percentage here
      type = "heatmap",
      colorscale = list(c(0, 1), c("#FFB6C1", "#C71585")),
      zmin = 1, # Set minimum z value
      zmax = 100, # Set maximum z value to 100 for percentage
      colorbar = list(
        title = "Percentage of Mode Response",
        tickvals = seq(0, 100, by = 25),
        ticktext = c("0%", "25%", "50%", "75%", "100%")
      ),
      hoverinfo = "text",
      text = ~paste("<br>Percentage: ", sprintf("%.2f%%", percentage))
    ) %>%
      layout(
        title = 'You should make a vow of pre-marital chastity',
        xaxis = list(
          title = 'Education level', 
          tickvals = 1:4, 
          ticktext = edu_labels
        ),
        yaxis = list(
          title = 'Religion', 
          tickvals = 1:5, 
          ticktext = c("Christian(Pr>otestant)", "Catholic", "Buddhism", "Other religions", "No religion")
        )
      )
    
  })
  
  
  ##################v_abort######################
  
  
  mode_percentage_data2 <- subset_data %>%
    group_by(political, edu) %>%
    summarise(mode_data = calculate_mode_percentage(v_abort)) %>%
    ungroup() %>%
    tidyr::unnest(mode_data)
  
  output$heatmap2 <- renderPlotly({ 
    heatmap_interactive <- plot_ly(
      data = mode_percentage_data2,
      x = ~political,
      y = ~edu,
      z = ~percentage,  # Use calculated percentage here
      type = "heatmap",
      colorscale = list(c(0, 1), c("lightblue", "darkblue")),
      zmin = 1, # Set minimum z value
      zmax = 100, # Set maximum z value to 100 for percentage
      colorbar = list(
        title = "Percentage of Mode Response",
        tickvals = seq(0, 100, by = 25),
        ticktext = c("0%", "25%", "50%", "75%", "100%")
      ),
      hoverinfo = "text",
      text = ~paste("<br>Percentage: ", sprintf("%.2f%%", percentage))
    ) %>%
      layout(
        title = 'You should not do abortion',
        xaxis = list(title = 'Political perception', tickvals = 1:5, ticktext = c("very conservative", "relatively conservative", "moderate", "relatively progressive", "very progressive")),
        yaxis = list(title = 'Education level', tickvals = 1:4, ticktext = edu_labels)
      )
    
  })
  
  mode_percentage_data3 <- subset_data %>%
    group_by(edu, avgincome1) %>%
    summarise(mode_data = calculate_mode_percentage(v_child)) %>%
    ungroup() %>%
    tidyr::unnest(mode_data)
  
  output$heatmap3 <- renderPlotly({ 
    heatmap_interactive <- plot_ly(
      data = mode_percentage_data3,
      x = ~edu,
      y = ~avgincome1,
      z = ~percentage,  # Use calculated percentage here
      type = "heatmap",
      colorscale = list(c(0, 1), c("#FFB6C1", "#C71585")),
      zmin = 1, # Set minimum z value
      zmax = 100, # Set maximum z value to 100 for percentage
      colorbar = list(
        title = "Percentage of Mode Response",
        tickvals = seq(0, 100, by = 25),
        ticktext = c("0%", "25%", "50%", "75%", "100%")
      ),
      hoverinfo = "text",
      text = ~paste("<br>Percentage: ", sprintf("%.2f%%", percentage))
    ) %>%
      layout(
        title = 'Having a Child is Necessary',
        xaxis = list(title = 'Education level', tickvals = 1:4, ticktext = edu_labels),
        yaxis = list(title = 'Average income', tickvals = 1:5, ticktext = c("1000~1990k", "2000~2990k", "3000~4990k", "5000~6990k", "more than 7000k")
        )
      )
    
    
  })  
  
  #################parents##################################
  
  
  mode_percentage_data4 <- subset_data %>%
    group_by(married, avgincome1) %>%
    summarise(mode_data = calculate_mode_percentage(v_parents)) %>%
    ungroup() %>%
    tidyr::unnest(mode_data)
  
  output$heatmap4 <- renderPlotly({ 
    heatmap_interactive <- plot_ly(
      data = mode_percentage_data4,
      x = ~married,
      y = ~avgincome1,
      z = ~percentage,  # Use calculated percentage here
      type = "heatmap",
      colorscale = list(c(0, 1), c("lightblue", "darkblue")),
      zmin = 1, # Set minimum z value
      zmax = 100, # Set maximum z value to 100 for percentage
      colorbar = list(
        title = "Percentage of Mode Response",
        tickvals = seq(0, 100, by = 25),
        ticktext = c("0%", "25%", "50%", "75%", "100%")
      ),
      hoverinfo = "text",
      text = ~paste("<br>Percentage: ", sprintf("%.2f%%", percentage))
    ) %>%
      layout(
        title = 'A child should support their elderly parents',
        xaxis = list(title = 'Marital status', tickvals = 1:3, ticktext = c("Not married", "Married", "Divorced/separated")),
        yaxis = list(title = 'Average income', tickvals = 1:5, ticktext = c("1000~1990k", "2000~2990k", "3000~4990k", "5000~6990k", "more than 7000k")
        )
      )  
    
  }) 
  
  ########## 3rd Question
  # We are now deselecting 2nd preference
  comp2_add <- comp2_add %>%
    mutate(p_personality = replace(p_personality, p_personality == 2, NA))
  comp2_add <- comp2_add %>%
    mutate(p_app = replace(p_app, p_app == 2, NA))
  comp2_add <- comp2_add %>%
    mutate(p_religion = replace(p_religion, p_religion == 2, NA))
  comp2_add <- comp2_add %>%
    mutate(p_fambg = replace(p_fambg, p_fambg == 2, NA))
  comp2_add <- comp2_add %>%
    mutate(p_hobby = replace(p_hobby, p_hobby == 2, NA))
  comp2_add <- comp2_add %>%
    mutate(p_edu = replace(p_edu, p_edu == 2, NA))
  comp2_add <- comp2_add %>%
    mutate(p_wealth = replace(p_wealth, p_wealth == 2, NA))
  comp2_add <- comp2_add %>%
    mutate(p_job = replace(p_job, p_job == 2, NA))
  comp2_add <- comp2_add %>%
    mutate(p_others = replace(p_others, p_others == 2, NA))    
  
  
  ## Summarize for radar chart visualization
  plot_p_gender_pexcl <- comp2_add %>%
    filter(between(age, 18, 45)) %>%
    select(gender, p_personality, p_wealth, p_app, p_job, p_religion, p_fambg, p_edu, p_hobby) %>%
    group_by(gender) %>%
    summarize(
      total_count=n(),
      p_wealth_sum = sum(p_wealth, na.rm = TRUE)*100/total_count,
      p_app_sum = sum(p_app, na.rm = TRUE)*100/total_count,
      p_job_sum = sum(p_job, na.rm = TRUE)*100/total_count,
      p_religion_sum = sum(p_religion, na.rm = TRUE)*100/total_count,
      p_fambg_sum = sum(p_fambg, na.rm = TRUE)*100/total_count,
      p_edu_sum = sum(p_edu, na.rm = TRUE)*100/total_count,
      p_hobby_sum = sum(p_hobby, na.rm = TRUE)*100/total_count)
  
  # Plotting radarchart - gender
  output$select_gender <- renderPlotly({
    gender_persoff <- plot_ly(
      type = 'scatterpolar',
      fill = 'tonext',
      line = list(shape = 'linear')  # Add this line to control line shape
    )
    
    for (gender_value in unique(plot_p_gender_pexcl$gender)) {
      gender_data <- filter(plot_p_gender_pexcl, gender == gender_value)
      
      gender_persoff <- gender_persoff %>%
        add_trace(
          r = c(
            gender_data$p_wealth_sum,
            gender_data$p_job_sum,
            gender_data$p_religion_sum,
            gender_data$p_app_sum,
            gender_data$p_fambg_sum,
            gender_data$p_edu_sum,
            gender_data$p_hobby_sum
          ),
          theta = c(
            'Wealth',
            'Job',
            'Religion',
            'Appearance',
            'Family Background',
            'Education',
            'Hobby'
          ),
          name = ifelse(gender_value == 1, 'Male', 'Female')  # Add gender label to legend
        )
    }
    
    gender_persoff <- gender_persoff %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0, 15)  # Set the maximum value to 20
          )
        )
      )
    print(gender_persoff)
    
  }) 
  
  #Marital status (exc_p)
  plot_p_married_pexcl  <- comp2_add %>% 
    filter(between(age, 18, 45)) %>%
    filter(!is.na(married), married != 9) %>% 
    select(married, p_wealth, p_app, p_job, p_religion, p_fambg, p_edu, p_hobby) %>%
    group_by(married) %>% 
    summarize(
      total_count=n(),
      p_wealth_sum = sum(p_wealth, na.rm = TRUE)*100/total_count,
      p_app_sum = sum(p_app, na.rm = TRUE)*100/total_count,
      p_job_sum = sum(p_job, na.rm = TRUE)*100/total_count,
      p_religion_sum = sum(p_religion, na.rm = TRUE)*100/total_count,
      p_fambg_sum = sum(p_fambg, na.rm = TRUE)*100/total_count,
      p_edu_sum = sum(p_edu, na.rm = TRUE)*100/total_count,
      p_hobby_sum = sum(p_hobby, na.rm = TRUE)*100/total_count)  
  
  output$select_married <- renderPlotly({     
    married_plot_pexcl <- plot_ly(
      type = 'scatterpolar',
      fill = 'tonext',
      line = list(shape = 'linear')  # Add this line to control line shape
    )
    
    for (married_value in unique(plot_p_married_pexcl$married)) {
      married_data <- filter(plot_p_married_pexcl, married == married_value)
      
      married_plot_pexcl <- married_plot_pexcl %>%
        add_trace(
          r = c(
            married_data$p_wealth_sum,
            married_data$p_app_sum,
            married_data$p_job_sum,
            married_data$p_religion_sum,
            married_data$p_fambg_sum,
            married_data$p_edu_sum,
            married_data$p_hobby_sum
          ),
          theta = c(
            'Wealth',
            'Apperance',
            'Job',
            'Religion',
            'Family Background.',
            'Education',
            'Hobby'
          ),
          name = ifelse(married_value == 1, 'Single', ifelse(married_value == 2, 'Married', 'Other'))
        )
    }
    
    married_plot_pexcl <- married_plot_pexcl %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0, 23)  # Set the maximum value to 20
          )
        )
      )
    
    print(married_plot_pexcl)
    
  }) 
  
  ##Education
  
  plot_p_edu_pexcl<- comp2_add %>% 
    filter(between(age, 18, 45)) %>%
    filter(!is.na(edu), edu != 9) %>% 
    select(edu, p_wealth, p_app, p_job, p_religion, p_fambg, p_edu, p_hobby) %>%
    group_by(edu) %>% 
    summarize(
      total_count=n(),
      p_wealth_sum = sum(p_wealth, na.rm = TRUE)*100/total_count,
      p_app_sum = sum(p_app, na.rm = TRUE)*100/total_count,
      p_job_sum = sum(p_job, na.rm = TRUE)*100/total_count,
      p_religion_sum = sum(p_religion, na.rm = TRUE)*100/total_count,
      p_fambg_sum = sum(p_fambg, na.rm = TRUE)*100/total_count,
      p_edu_sum = sum(p_edu, na.rm = TRUE)*100/total_count,
      p_hobby_sum = sum(p_hobby, na.rm = TRUE)*100/total_count)
  
  #radarchart - edu (excluding p)
  output$select_edu <- renderPlotly({         
    edu_plot_pexcl <- plot_ly(
      type = 'scatterpolar',
      fill = 'tonext',
      line = list(shape = 'linear')  # Add this line to control line shape
    )
    
    for (edu_value in unique(plot_p_edu_pexcl$edu)) {
      edu_data <- filter(plot_p_edu_pexcl, edu == edu_value)
      
      edu_plot_pexcl <- edu_plot_pexcl %>%
        add_trace(
          r = c(
            edu_data$p_wealth_sum,
            edu_data$p_app_sum,
            edu_data$p_job_sum,
            edu_data$p_religion_sum,
            edu_data$p_fambg_sum,
            edu_data$p_edu_sum,
            edu_data$p_hobby_sum
          ),
          theta = c(
            'Wealth',
            'Apperance',
            'Job',
            'Religion',
            'Family Background.',
            'Education',
            'Hobby'
          ),
          name = ifelse(edu_value == 1, 'Middle School', 
                        ifelse(edu_value == 2, 'High School', 
                               ifelse(edu_value == 3, 'Undergraduate', 
                                      ifelse(edu_value == 4, 'Graduate', 'Other'))
                        )
          )
        )
    }
    
    edu_plot_pexcl <- edu_plot_pexcl %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0, 20)  # Set the maximum value to 20
          )
        )
      )
    
    print(edu_plot_pexcl)
    
  })
  
  ###4. Income Comparison (including personality)
  
  plot_p_income <- comp2_add %>% 
    filter(between(age, 18, 45)) %>%
    filter(!is.na(avgincome), avgincome != 9) %>% 
    select(avgincome, p_wealth, p_app, p_job, p_religion, p_fambg, p_edu, p_hobby) %>%
    group_by(avgincome) %>% 
    mutate(avgincome = case_when(
      avgincome %in% c(1, 2, 3) ~ "Under KRW 2M",  
      avgincome %in% c(4, 5) ~ "KRW 2M~2.99M",   
      avgincome %in% c(6, 7) ~ "KRW 3M~4.99M",
      avgincome %in% c(8, 9) ~ "KRW 5M~6.99",
      avgincome %in% c(10, 11) ~ "Above KRW 7M",
      TRUE ~ as.character(avgincome))) %>% 
    summarize(
      total_count=n(),
      p_wealth_sum = sum(p_wealth, na.rm = TRUE)*100/total_count,
      p_app_sum = sum(p_app, na.rm = TRUE)*100/total_count,
      p_job_sum = sum(p_job, na.rm = TRUE)*100/total_count,
      p_religion_sum = sum(p_religion, na.rm = TRUE)*100/total_count,
      p_fambg_sum = sum(p_fambg, na.rm = TRUE)*100/total_count,
      p_edu_sum = sum(p_edu, na.rm = TRUE)*100/total_count,
      p_hobby_sum = sum(p_hobby, na.rm = TRUE)*100/total_count)
  
  
  #radar chart - income
  output$select_income <- renderPlotly({     
    income_plot_pexcl<- plot_ly(
      type = 'scatterpolar',
      fill = 'tonext',
      line = list(shape = 'linear')  # Add this line to control line shape
    )
    
    for (income_group in unique(plot_p_income_exc$avgincome)) {
      income_data <- filter(plot_p_income_exc, avgincome == income_group)
      
      income_plot_pexcl <- income_plot_pexcl %>%
        add_trace(
          r = c(
            income_data$p_wealth_sum,
            income_data$p_app_sum,
            income_data$p_job_sum,
            income_data$p_religion_sum,
            income_data$p_fambg_sum,
            income_data$p_edu_sum,
            income_data$p_hobby_sum
          ),
          theta = c(
            'Wealth',
            'Apperance',
            'Job',
            'Religion',
            'Family Background.',
            'Education',
            'Hobby'
          ),
          name = as.character(income_group)
        )
    }
    
    income_plot_pexcl <- income_plot_pexcl %>%
      layout(
        polar = list(
          radialaxis = list(
            visible = TRUE,
            range = c(0, 20)  # Set the maximum value to 20
          )
        )
      )
    
    print(income_plot_pexcl)
    
  })
  
}

shinyApp(ui = ui, server = server)
