
###'######################################################################
###'
###' Shiny Web Application
###' 
###' Data Visualization of GCSE Data Example
###' 
###' 
###' 20190131 JoonHo Lee
###' 
###' 

###'######################################################################
###'
###' Basic settings
###'
###'

### Load libraries
library(shiny)
library(shinythemes)
library(ggthemes)
library(scales)
library(tidyverse)
library(DT)
library(forcats)


# ### Load functions
# list.files("functions", full.names = TRUE) %>% walk(source)



###'######################################################################
###'
###' Data preparation
###'
###'

# ### Set temporary working directory for testing
# setwd("~/SACS/shiny_web_apps/LCFF_Funding_Snapshot")


### Load dataset: Prepared as a long format
load(file = "data/funding_snapshot_13to17.rda")


### Load Basic County/District information
load(file = "data/years_of_operation.rda")


### Filter only school districts
df <- funding_snapshot_13to17 %>%
  filter(LEA_type == "School District")


### Merge years of operation data
df <- df %>%
  select(-County, -LEA, -Charter_Num, -LEA_type) %>%
  left_join(years_of_operation, by = c("Ccode", "Dcode")) %>%
  select(Fiscalyear, Ccode, Cname, Dcode, Dname, Dtype, 
         first, last, opr_years, everything())



###'######################################################################
###'
###' Preset ggplot themes & Define functions  
###'    
###'        

### Theme settings
theme_preset <- 
  theme_bw() + 
  theme(panel.background = element_blank(),
        panel.grid = element_blank(), 
        legend.position = "bottom", 
        legend.direction = "horizontal", 
        legend.title = element_blank())


### Temporary labels
temp_labels <- labs(title = "Enter title here", 
                    subtitle = "Enter subtitle here", 
                    caption = "Enter caption here", 
                    y = "Enter ylabel here",  
                    x = "Enter xlabel here")


### Define manual palettes
color_palette <- c("firebrick1", "dodgerblue1", "forestgreen", "darkorchid1", "darkgoldenrod1", 
                   "blue", "green", "purple", "gold", "red")    
shape_palette <- c(16, 17, 15, 18, 1, 2, 0, 5, 6, 4, 3, 8, 10, 7, 9) 


### Function for automatically setting ylim
auto_ylim <- function(value_vec = NULL, tweak = 5){
  
  ### The optimal y-limits
  bottom <- min(value_vec) - (min(value_vec) - 0)/tweak
  ceiling <- max(value_vec) + (min(value_vec) - 0)/tweak
  
  ### Return objects
  auto_ylim <- c(bottom, ceiling)
  return(auto_ylim)
}



###'######################################################################
###'
###' Define levels of factors
###'
###'

### Define vectors with only necessary factor entries
target_vec <- c("Base Grant Funding", 
                "Supplemental Grant Funding", 
                "Concentration Grant Funding", 
                "Necessary Small Schools Allowance", 
                "Add-On Funding")


transition_vec <- c("LCFF Target Entitlement (If funded at the LCFF Target)", 
                    "LCFF Floor Entitlement (If not funded at the LCFF Target)", 
                    "Current Year Gap Funding", 
                    "Economic Recovery Target", 
                    "Additional LCFF State Aid to Meet the Minimum (Additional SA for MSA)", 
                    "Miscellaneous Adjustments (School Districts Only)")


target_vs_floor_vec <- c("LCFF Floor Entitlement (If not funded at the LCFF Target)", 
                         " Current Year Gap Funding (If not funded at the LCFF Target)", 
                         "Remaining Need")


sources_vec <- c("Local Revenue", 
                 "Education Protection Account State Aid", 
                 "LCFF State Aid Before MSA", 
                 "Additional SA for MSA")


### Combine as a list
factor_list <- list(target_vec, 
                    transition_vec, 
                    target_vs_floor_vec, 
                    sources_vec)


### Vector containing the order of categories
category_vec <- c("LCFF Target Entitlement", 
                  "LCFF Transition Entitlement", 
                  "LCFF Target VS. LCFF Floor", 
                  "LCFF Funding Sources (Actual Funding)")



###'######################################################################
###'
###' User interface
###'
###'

ui <- fluidPage(
  
  ### Shiny theme
  theme = shinytheme("lumen"), 
  
  
  ### Application title
  titlePanel("Local Control Funding Formula - Funding Snapshot", 
             windowTitle = "LCFF Funding Snapshot"),
  
  
  ### Sidebar layout with a input and output definitions
  
  sidebarLayout(
    
    ###'############
    ###' Inputs  ###
    ###'############ 
    
    
    sidebarPanel(
      
      ###'#######################################
      ###' Input section 1) Category and Factor
      ###'
      
      h3("Variable"),  
      
      # Select a category to plot  
      radioButtons(inputId = "plot_category", 
                   label = "Select a Category to Plot:", 
                   choices = c("LCFF Target Entitlement", 
                               "LCFF Transition Entitlement", 
                               "LCFF Target VS. LCFF Floor", 
                               "LCFF Funding Sources (Actual Funding)"), 
                   selected = c("LCFF Target VS. LCFF Floor")
      ),
      
      hr(),
      
      
      ###'#######################################
      ###' Input section 2) Level of Data Summary   
      ###'
      
      h3("Data Summary"),
      
      
      # Level of data summary
      radioButtons(inputId = "summary_level", 
                   label = "The level of data summary:", 
                   choices = c("State-level" = "state", 
                               "District-level" = "district")
      ),
      
      
      # Conditional panel 1) Summary-level: State
      conditionalPanel(
        condition = "input.summary_level == 'state'",
        checkboxGroupInput(inputId = "state_dist_type", 
                           label = "Select District Type(s) to summarize:", 
                           choices = c("ELEMENTARY", "HIGH", "UNIFIED"), 
                           selected = c("ELEMENTARY", "HIGH", "UNIFIED"))
      ), 
      
      
      # Conditional panel 2) Summary-level: District
      conditionalPanel(
        condition = "input.summary_level == 'district'", 
        selectizeInput(inputId = "district", 
                       label = "Search and Select District Name(s):", 
                       choices = unique(df$Dname), 
                       options = list(maxOptions = 50), 
                       selected = "Los Angeles Unified")
      ), 
      
      
      h5("Data table"),
      
      # Show data table
      checkboxInput(inputId = "show_data",
                    label = "Show data table",
                    value = TRUE),
      
      hr(),
      
      ###'#######################################
      ###' Input section 3) Supplementary Data 
      ###'
      
      h3("Demographic Data"),
      
      
      # Show ADA
      checkboxInput(inputId = "show_ADA",
                    label = "Average Daily Attendance (ADA)",
                    value = TRUE),
      
      
      # Show UPP
      checkboxInput(inputId = "show_UPP",
                    label = "Unduplicated Pupil Percentage (UPP)",
                    value = TRUE),
      
      
      
      ###'#######################################
      ###' Input section 4) Miscellaneous Inputs
      ###' 
      
      # Built with Shiny by JoonHo Lee
      br(), br(),
      h5("Built with",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", 
             height = "30px"),
         "by JoonHo Lee (joonho@berkeley.edu)"
      )
      
      , width = 3),  # End of sidbarPanel
    
    
    
    ###'############
    ###' Outputs
    ###'############ 
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  id = "tabsetpanel",
                  
                  tabPanel(title = "Plot", 
                           plotOutput(outputId = "plot_proportions", 
                                      width = "100%")),
                  
                  tabPanel(title = "Table", 
                           br(),
                           DT::dataTableOutput(outputId = "data_table"), 
                           downloadButton(outputId = "download_data", 
                                          label = "Download data")), 
                  
                  tabPanel(title = "ADA", 
                           plotOutput(outputId = "plot_ADA", 
                                      width = "100%")),
                  
                  tabPanel(title = "UPP", 
                           plotOutput(outputId = "plot_UPP", 
                                      width = "100%"))
                  
      ) # End of tabsetPanel
      , width = 9)  # End of mainPanel
    
    )  # End of sidebarLayout
  )  # End of ui() function



###'######################################################################
###'
###' Server logic
###'
###'

server <- function(input, output, session) {
  
  ###'#######################################
  ###' Filter only relevant category
  ###' Remove unnecessary factor entries
  ###' Convert to factor (for ordering)
  ###' 
  
  df_category <- reactive({
    
    idx <- which(category_vec == input$plot_category)
    
    valid_factors <- factor_list[[idx]]
    
    df %>%
      filter(category %in% input$plot_category) %>%
      filter(variable %in% valid_factors) %>%
      mutate(variable = factor(variable, levels = valid_factors))
    
  })
  
  ###'#######################################
  ###' Summarize funding data 
  ###' at state or district level
  ###' 
  
  df_sum <- reactive({
    
    # (1) State-level summary
    if(input$summary_level == "state") {
      
      df_category() %>%
        filter(Dtype %in% input$state_dist_type) %>% 
        group_by(Fiscalyear, Dtype, variable) %>%
        summarise(value = round(weighted.mean(value_PP_16, ADA_Total, na.rm = TRUE), 0)) %>%
        filter(!is.na(Dtype)) %>%
        ungroup() %>%
        group_by(Fiscalyear, Dtype) %>%
        mutate(group_sum = sum(value, na.rm = TRUE), 
               percent = value/group_sum * 100, 
               label_text = paste0(sprintf("%.1f", percent), "%")) %>%
        filter(percent != 0)
      
      # (2) District-level summary
    } else if (input$summary_level == "district") {
      
      df_category() %>%
        filter(Dname == input$district) %>% 
        select(-value, -value_PP) %>%
        rename(value = value_PP_16) %>%
        select(Fiscalyear, Dtype, variable, value) %>%
        mutate_at(.vars = c("value"), .funs = round) %>%
        group_by(Fiscalyear) %>%
        mutate(group_sum = sum(value, na.rm = TRUE), 
               percent = value/group_sum * 100, 
               label_text = paste0(sprintf("%.1f", percent), "%")) %>%
        filter(percent != 0)
      
    }
  })
  
  
  ###' Prepare group total dataframe 
  ###' To plot values at the tip of bar graphs
  
  group_total <- reactive({
    
    df_sum() %>%
      group_by(Fiscalyear, Dtype) %>%
      summarise(group_sum = first(group_sum))
    
  }) 
  
  
  ###'#######################################
  ###' Summarize demographic data (ADA, UPP) 
  ###' at state or district level
  ###' 
  
  ### Grade span factor levels & labels
  grade_span_levels <- c("ADA_K_3", "ADA_4_6", "ADA_7_8", "ADA_9_12")
  grade_span_labels <- c("Grade K-3", "Grade 4-6", "Grade 7-8", "Grade 9-12")
  
  
  ### Generate reactive dataframe
  df_demo <- reactive({
    
    # (1) State-level summary
    if(input$summary_level == "state") {
      
      df_category() %>% 
        filter(Dtype %in% input$state_dist_type) %>% 
        select(Fiscalyear, Ccode, Dcode, Dtype, UPP, starts_with("ADA")) %>%
        distinct() %>%
        gather(key = "grade_span", value = "ADA_grade", ADA_K_3:ADA_9_12) %>%
        select(Ccode, Dcode, Dtype, Fiscalyear, UPP, grade_span, ADA_grade, ADA_Total) %>% 
        arrange(Ccode, Dcode, Fiscalyear, grade_span) %>%
        group_by(Fiscalyear, Dtype, grade_span) %>% 
        summarise(ADA_grade = mean(ADA_grade, na.rm = TRUE), 
                  ADA_Total = mean(ADA_Total, na.rm = TRUE), 
                  UPP = mean(UPP, na.rm = TRUE)) %>%
        mutate(grade_span = factor(grade_span, 
                                   levels = grade_span_levels, 
                                   labels = grade_span_labels)) %>%
        arrange(Dtype, Fiscalyear, grade_span) %>%
        mutate_at(.vars = c("ADA_grade", "ADA_Total"), .funs = round) %>%
        filter(!is.na(Dtype)) %>%
        group_by(Fiscalyear, Dtype) %>%
        mutate(percent = ADA_grade/ADA_Total * 100, 
               label_text = paste0(sprintf("%.1f", percent), "%"), 
               UPP = round(UPP*100, 1)) %>%
        filter(percent != 0)
      
      
      # (2) District-level summary
    } else if (input$summary_level == "district") {
      
      df_category() %>%
        filter(Dname == input$district) %>%
        select(Fiscalyear, Ccode, Dcode, Dtype, UPP, starts_with("ADA")) %>%
        distinct() %>%
        gather(key = "grade_span", value = "ADA_grade", ADA_K_3:ADA_9_12) %>%
        select(Ccode, Dcode, Dtype, Fiscalyear, UPP, grade_span, ADA_grade, ADA_Total) %>% 
        arrange(Ccode, Dcode, Fiscalyear, grade_span) %>%
        group_by(Fiscalyear, Dtype, grade_span) %>% 
        summarise(ADA_grade = mean(ADA_grade, na.rm = TRUE), 
                  ADA_Total = mean(ADA_Total, na.rm = TRUE), 
                  UPP = mean(UPP, na.rm = TRUE)) %>%
        mutate(grade_span = factor(grade_span, 
                                   levels = grade_span_levels, 
                                   labels = grade_span_labels)) %>%
        arrange(Dtype, Fiscalyear, grade_span) %>%
        mutate_at(.vars = c("ADA_grade", "ADA_Total"), .funs = round) %>%
        filter(!is.na(Dtype)) %>%
        group_by(Fiscalyear, Dtype) %>%
        mutate(percent = ADA_grade/ADA_Total * 100, 
               label_text = paste0(sprintf("%.1f", percent), "%"), 
               UPP = round(UPP*100, 1)) %>%
        filter(percent != 0)
      
    }
  })
  
  
  ###' Prepare group total dataframe 
  ###' To plot values at the tip of bar graphs
  
  ADA_total <- reactive({
    
    df_demo() %>%
      group_by(Fiscalyear, Dtype) %>%
      summarise(ADA_Total = first(ADA_Total))
    
  }) 
  
  
  ###' UPP data to plot
  df_UPP <- reactive({
    
    df_demo()  %>% 
      group_by(Fiscalyear, Dtype) %>% 
      summarise(UPP = first(UPP))
    
  })
  
  
  
  
  ###'###################################################
  ###' Prepare labels
  ###'
  
  summary_level_Vec <- c("State-level" = "state", 
                         "District-level" = "district")
  
  lab_subtitle <- reactive({
    
    if (input$summary_level == "state"){
      
      paste0("Proportions Based on ", 
             names(summary_level_Vec)[summary_level_Vec == input$summary_level], 
             " Averages Weighted by ADA")
      
    } else if (input$summary_level == "district") {
      
      paste0("Proportions Based on ", 
             names(summary_level_Vec)[summary_level_Vec == input$summary_level], 
             " Averages: ", 
             input$district)
      
    }
  })
  
  
  lab_subtitle_demo <- reactive({
    
    if (input$summary_level == "state"){
      
      paste0(names(summary_level_Vec)[summary_level_Vec == input$summary_level], 
             " Averages")
      
    } else if (input$summary_level == "district") {
      
      paste0(names(summary_level_Vec)[summary_level_Vec == input$summary_level], 
             " Values: ", 
             input$district)
      
    }
  })
  
  
  
  ###'###################################################
  ###' Create Output: plot_proportions 
  ###' 
  
  output$plot_proportions <- renderPlot({
    
    ggplot(data = df_sum(), 
           aes(x = Fiscalyear, y = value, fill = variable)) + 
      geom_bar(position = position_stack(reverse = TRUE), stat = "identity", width = 0.7) +
      geom_text(aes(label = label_text), 
                position = position_stack(vjust = 0.5, reverse = TRUE), size = 4.5) + 
      geom_text(data = group_total(),  
                aes(x = Fiscalyear, y = group_sum + mean(df_sum()$group_sum)/30, 
                    label = comma(group_sum), fill = NULL), 
                size = 4.5) + 
      facet_wrap(~Dtype) + 
      scale_x_continuous(breaks = seq(min(df_sum()$Fiscalyear), 
                                      max(df_sum()$Fiscalyear), 
                                      by = 1)) + 
      scale_y_continuous(labels = comma) + 
      guides(fill = guide_legend(nrow = 2, byrow = TRUE)) + 
      scale_fill_brewer(palette = "Paired") + 
      theme_preset + 
      theme(text = element_text(size = 16)) + 
      labs(title = input$plot_category,
           subtitle = lab_subtitle(), 
           caption = "Source: LCFF Funding Snapshot Data", 
           y = "Amount in real 2016 dollars (Per-pupil)",   
           x = "Fiscal Years") 
    
  }, height = 650)
  
  
  
  ###'###################################################
  ###' Create Output: plot_ADA 
  ###' 
  
  output$plot_ADA <- renderPlot({
    
    ggplot(data = df_demo(), 
           aes(x = Fiscalyear, y = ADA_grade, fill = grade_span)) + 
      geom_bar(position = position_stack(reverse = TRUE), stat = "identity", width = 0.7) +
      geom_text(aes(label = label_text), 
                position = position_stack(vjust = 0.5, reverse = TRUE), size = 4.5) + 
      geom_text(data = ADA_total(),  
                aes(x = Fiscalyear, y = ADA_Total + mean(df_demo()$ADA_Total)/30, 
                    label = comma(ADA_Total), fill = NULL), 
                size = 4.5) + 
      facet_wrap(~Dtype) + 
      scale_y_continuous(labels = comma) + 
      guides(fill = guide_legend(nrow = 2, byrow = TRUE)) + 
      scale_fill_brewer(palette = "Paired") + 
      theme_preset + 
      theme(text = element_text(size = 16)) + 
      labs(title = "Average Daily Attendance", 
           subtitle = lab_subtitle_demo(), 
           caption = "Source: LCFF Funding Snapshot Data", 
           y = "State-level averages",   
           x = "Fiscal Years")
    
  }, height = 650)
  
  
  ###'###################################################
  ###' Create Output: plot_UPP 
  ###' 
  
  output$plot_UPP <- renderPlot({
    
    ggplot(data = df_UPP(), 
           aes(x = Fiscalyear, y = UPP, group = Dtype)) +
      geom_point(aes(shape = Dtype, color = Dtype), size = 3.0) +
      geom_path(aes(linetype = Dtype, color = Dtype), size = 1.0) + 
      geom_text(aes(label = comma(UPP)), size = 4.5, hjust = 0.5, vjust = 2.0) +  
      scale_y_continuous(labels = comma, 
                         limits = c(min(df_UPP()$UPP) - 5, max(df_UPP()$UPP) + 5)) +
      labs(title = "Unduplicated Pupil Percentages", 
           subtitle = lab_subtitle_demo(), 
           caption = "Source: LCFF Funding Snapshot Data", 
           y = "Percentage (%)",   
           x = "Fiscal Years") + 
      theme_preset + 
      theme(text = element_text(size = 16)) + 
      scale_color_manual(values = color_palette[seq(unique(df_UPP()$Dtype))]) + 
      scale_shape_manual(values = shape_palette[seq(unique(df_UPP()$Dtype))])
    
  }, width = 600, height = 600)
  
  
  ###'###################################################
  ###'  Create Output: Data table
  ###'  
  
  ### Define reactive data table
  table <- reactive({
    
    df_sum() %>% 
      select(Dtype, Fiscalyear, variable, value, label_text) %>%
      rename(percent = label_text) %>%
      arrange(Dtype, variable)
    
  })
  
  
  ### Print data table if checked
  output$data_table <- DT::renderDataTable(
    
    DT::datatable(data = table(), 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  )
  
  
  ### Display supplemental data tabs only if show_data is checked
  observeEvent(input$show_data, {
    
    if(input$show_data){
      
      showTab(inputId = "tabsetpanel", target = "Table", select = FALSE)
      
    } else {
      
      hideTab(inputId = "tabsetpanel", target = "Table")
      
    }
  })
  
  observeEvent(input$show_ADA, {
    
    if(input$show_ADA){
      
      showTab(inputId = "tabsetpanel", target = "ADA", select = FALSE)
      
    } else {
      
      hideTab(inputId = "tabsetpanel", target = "ADA")
      
    }
  })
  
  observeEvent(input$show_UPP, {
    
    if(input$show_UPP){
      
      showTab(inputId = "tabsetpanel", target = "UPP", select = FALSE)
      
    } else {
      
      hideTab(inputId = "tabsetpanel", target = "UPP")
      
    }
  })
  
  
  ### Download file
  output$download_data <- downloadHandler(
    
    filename = function() {
      
      paste0("data_table.csv")
      
    },
    
    content = function(file) { 
      
      write_csv(table(), file) 
      
    }
  )
  
} # End of server() function



###'######################################################################
###' 
###' Run the application
###' 
###'   

shinyApp(ui = ui, server = server)




