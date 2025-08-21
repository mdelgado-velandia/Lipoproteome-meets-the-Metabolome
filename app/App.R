
# R version 4.4.1


# Load packages----
library(shiny) # 1.9.1
library(bslib) # 0.8.0
library(reactable) # 0.4.4
library(rio) # 1.2.3
library(tidyverse) # 2.0.0
library(shinyFeedback) # 0.4.0
library(circlize) # 0.4.16
library(ComplexHeatmap) # 2.21.1
library(TwoSampleMR) # 0.6.8




# Set working directory----





# Load data----
load("Data_250816_str.RData")




# Custom function and options----
# Function to download sorted reactables
registerInputHandler("to_csv", convertToDataFrame, force = TRUE)

# Option to plot the longest heatmaps
options(ragg.max_dim = 100000)









# User interface----
ui <- fluidPage(
  
  # Prevent error messages showing in user interface
  # tags$style(type="text/css",
  #            ".shiny-output-error { visibility: hidden; }",
  #            ".shiny-output-error:before { visibility: hidden; }"
  # ),
  
  
  
  
  # Loading data spinner
  # busy_start_up(
  #   loader = spin_epic("orbit", color = "#FFF"),
  #   text = "Loading data, this could take up to 3 seconds",
  #   timeout = 3000,
  #   color = "#FFF",
  #   background = "#112446"
  # ),
  # background = "#008B8B"
  
  
  
  
  page_navbar(
    
    title = tags$b("The Lipoproteome meets the Metabolome"),
    bg = "#39604e", # #007bc2
    inverse = TRUE,
    theme = bslib::bs_theme(version = 5),
    
    
    
    
    
    
    nav_panel(title = "Welcome",
              p(""),
              p(""),
              tags$span(style = "color:black; font-size:13pt", "Welcome!"),
              p(""),
              tags$span(style = "color:black; font-size:13pt", "This webpage was created to provide you with the results from analyses of how clinically measured lipoproteins are related to metabolites as presented in the paper ",tags$i("When the Lipoproteome Meets the Metabolome - Observational and Mendelian Randomization Analyses.") ),
              
              tags$span(style = "color:black; font-size:13pt", "The results consist of two parts."),
              
              tags$span(style = "color:black; font-size:13pt", "The first part used epidemiological observational data in which cinically measured HDL and LDL, as well as 13 lipoprotein concentrations were related to each of 790 non-xenobiotic metabolites. A discovery/validation approach was used. The discovery part was performed in the EpiHealth study and the validation phase was performed in the POEM and PIVUS studies."),
              tags$span(style = "color:black; font-size:13pt", "In order to see the results, please click the ", tags$i("Clinically measured HDL/LDL"), " or the ", tags$i("Lipoprotein subclasses"), " tab inside the ", tags$i("Tables"), " tab at the top of this page and then enter the name of a protein and a metabolite to see the results. Two degrees of adjustment were used, age and sex-adjustment and also additional adjustment for BMI and kidney function (eGFR)."),
              tags$span(style = "color:black; font-size:13pt", "You can also see heatmap plots for any of these association in the ", tags$i("Clinically measured HDL/LDL"), " or the ", tags$i("Lipoprotein subclasses"),  " tab inside the ", tags$i("Heatmaps"), " tab at the top of this page. Models adjusted by age, sex, BMI and kidney function (eGFR) are displayed."),
              
              tags$span(style = "color:black; font-size:13pt", "The second part used two-sample Mendelian randomization (MR) to evaluate how cinically measured HDL and LDL are related to metabolites. Cis-instruments for were derived from UK Biobank data and were related to our own GWAS data for metabolites derived from the EpiHealth and SCAPIS studies. Only lipoprotein->metabolite relationships were evaluated becouse of the difficulty finding non-pleotrophic instruments for the majority of the metabolites."),
              tags$span(style = "color:black; font-size:13pt", "In order to see the MR results, please click the ", tags$i("Mendelian Randomization"), " tab within the ", tags$i("Clinically measured HDL/LDL") ," tab." ),
              tags$span(style = "color:black; font-size:13pt", "You can also see heatmap plots for any of these associations in the ", tags$i("Mendelian Randomization"), " tab inside the ", tags$i("Clinically measured HDL/LDL"), " tab of the ", tags$i("Heatmaps"), " section."),
              
              tags$span(style = "color:black; font-size:13pt", "The result tables can be downloaded by pressing the ", tags$i("Download"), " button. The heatmap plots can be downloaded by pressing the ", tags$i("Download plot"), " button, and the underlying data can be downloaded by pressing the ", tags$i("Download data"), " button."),
              
              # tags$span(style = "color:black; font-size:13pt", "We have also analyzed how 1,319 proteins were related to each of 790 non-xenobiotic metabolites in the POEM study. Since these relationships were not validated in an external cohort, these results are only available as a table to download in the ", tags$i("Annex"), " tab."),
              p(""),
              p(""),
              p(""),
              p(""),
              p(""),
              p(tags$b("In Collaboration with") ),
              fluidRow(
                column(3, tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/c/cb/Uppsala_universitet_logo.jpg", width = "190px", height = "190px")) ,
                column(3, tags$img(src = "https://upload.wikimedia.org/wikipedia/en/3/3a/Lunds_universitet.svg", width = "180px", height = "180px")) ,
                column(3, tags$img(src = "https://images.ctfassets.net/e8gvzq1fwq00/61AhHssAP6zsqjxPVX5CzD/d1b15d2717f2e35546f51a187ff0826f/HLF_Logotyp_120_RGB_822x222.svg", width = "200px", height = "200px") )
              )
    ),
    
    
    
    
    nav_menu("Tables",
             
             
             nav_panel(title = "Clinically measured HDL/LDL",
                       tabsetPanel(
                         
                         tabPanel(title = "Observational analyses",
                                  
                                  
                                  p(""),
                                  tags$span(style = "color:#c24700; font-size:14pt", "Results for Clinically measured HDL/LDL") ,
                                  p(""),
                                  tags$span(style = "color:black; font-size:13pt", "Please select a metabolite and a lipoprotein.") ,
                                  tags$span(style = "color:black; font-size:13pt", "If you want to remove a metabolite or a lipoprotein, click on the respective box and press backspace.") ,
                                  tags$span(style = "color:black; font-size:13pt", "Models are adjusted for age, sex, BMI, diabetes medication use, statin use and kidney function (eGFR).") ,
                                  p(""),
                                  p(""),
                                  p(""),
                                  
                                  
                                  
                                  
                                  fluidPage(
                                    
                                    
                                    useShinyFeedback(),
                                    
                                    tags$div(  selectizeInput("metabolite_obs_hdl_ldl", "Select or type a metabolite", choices = NULL, multiple = FALSE,
                                                              selected = NULL,
                                                              options = list('plugins' = list('remove_button'),
                                                                             placeholder = '' ) )      ,  style="display:inline-block"),
                                    tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                                    tags$div(  selectizeInput("boolean_obs_hdl_ldl", "AND", choices = c("AND"), multiple = FALSE,
                                                              selected = "AND",
                                                              options = list('plugins' = list('remove_button'),
                                                                             placeholder = '' ) )          ,  style="display:inline-block; width: 100px;"),
                                    tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                                    
                                    tags$div(  selectizeInput("protein_obs_hdl_ldl", "Select or type a lipoprotein", choices = NULL, multiple = FALSE,
                                                              selected = NULL,
                                                              options = list('plugins' = list('remove_button'),
                                                                             placeholder = '' ) )          ,  style="display:inline-block"),
                                    tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                                    
                                    
                                    
                                    
                                    fluidRow(htmlOutput("result_text_obs_hdl_ldl")),
                                    tags$head(tags$style("#result_text_obs_hdl_ldl{font-size: 17px;
                                       }"
                                    )
                                    ),
                                    p(""),
                                    p(""),
                                    
                                    fluidRow(
                                      column(12,reactableOutput("selected_results_hdl_ldl")
                                      )
                                      
                                    ),
                                    fluidRow( column(6, align = "left", uiOutput("download.button.results_hdl_ldl") ) )
                                    
                                    
                                    
                                  ) # end fluidPage
                                  
                         ), # end tabsetPanel
                         
                         
                         tabPanel(title = "Mendelian Randomization",
                                  
                                  p(""),
                                  tags$span(style = "color:#c24700; font-size:14pt", "Results for Clinically measured HDL/LDL") ,
                                  p(""),
                                  tags$span(style = "color:black; font-size:13pt", "Please select a metabolite and a lipoprotein." ) ,
                                  tags$span(style = "color:black; font-size:13pt", "If you want to remove a metabolite or a lipoprotein, click on the respective box and press backspace.") ,
                                  p(""),
                                  p(""),
                                  p(""),
                                  
                                  
                                  
                                  
                                  fluidPage(
                                    
                                    
                                    useShinyFeedback(),
                                    
                                    tags$div(selectizeInput("metabolite_mr_hdl_ldl", "Select or type a metabolite", choices = NULL, multiple = FALSE,
                                                            selected = NULL,
                                                            options = list('plugins' = list('remove_button'),
                                                                           placeholder = '' )   ) ,  style="display:inline-block"),
                                    tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                                    tags$div(selectizeInput("boolean_mr_hdl_ldl", "AND", choices = c("AND"), multiple = FALSE,
                                                            selected = "AND",
                                                            options = list('plugins' = list('remove_button'),
                                                                           placeholder = '' )) ,  style="display:inline-block; width: 100px;"),
                                    tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                                    
                                    tags$div(selectizeInput("protein_mr_hdl_ldl", "Select or type a lipoprotein", choices = NULL, multiple = FALSE,
                                                            selected = NULL,
                                                            options = list('plugins' = list('remove_button'),
                                                                           placeholder = '' )  ) ,  style="display:inline-block"),
                                    tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                                    
                                    
                                    
                                    fluidRow(htmlOutput("table_text_mr_hdl_ldl")),
                                    tags$head(tags$style("#table_text_mr_hdl_ldl{font-size: 17px;
                                       }"
                                    )
                                    ),
                                    p(""),
                                    p(""),
                                    
                                    fluidRow(
                                      column(12,reactableOutput("selected_results_mr_hdl_ldl")
                                      )
                                      
                                    ),
                                    fluidRow( column(6, align = "left", uiOutput("download.button.results_mr_hdl_ldl") ) ),
                                    p(""),
                                    p(""),
                                    p(""),
                                    p(""),
                                    p(""),
                                    p(""),
                                    
                                    
                                    fluidRow(htmlOutput("warning_text_mr_hdl_ldl")),
                                    tags$head(tags$style("#warning_text_mr_hdl_ldl{font-size: 17px; color: #bb2124;
                                       }"
                                    )
                                    ),
                                    
                                    
                                    
                                    fluidRow(htmlOutput("scatter_text_mr_hdl_ldl")),
                                    tags$head(tags$style("#scatter_text_mr_hdl_ldl{font-size: 17px;
                                       }"
                                    )
                                    ),
                                    p(""),
                                    p(""),
                                    
                                    fluidRow(
                                      #   column(12,reactableOutput("selected_protein_details_mr_hdl_ldl")
                                      #   ),
                                      
                                      tags$div( plotOutput("scatter_mr_hdl_ldl", width = "80%", height = "900px") , style = "display:block;" ),
                                      tags$div(  uiOutput("download.button.scatter.plot") ,  style="display:inline-block")
                                      
                                      
                                    ),
                                    p(""),
                                    p(""),
                                    
                                    fluidRow(
                                      column(8,reactableOutput("selected_protein_details_mr_hdl_ldl")
                                      )
                                      
                                    ),
                                    
                                    p(""),
                                    p(""),
                                    p(""),
                                    p(""),
                                    p(""),
                                    p(""),
                                    
                                    fluidRow(htmlOutput("forest_text_mr_hdl_ldl")),
                                    tags$head(tags$style("#forest_text_mr_hdl_ldl{font-size: 17px;
                                               }"
                                    )
                                    ),
                                    p(""),
                                    p(""),
                                    
                                    fluidRow(
                                      
                                      tags$div( plotOutput("forest_mr_hdl_ldl", width = "80%", height = "900px") , style = "display:block;"),
                                      tags$div(  uiOutput("download.button.forest.plot") ,  style="display:inline-block")
                                      
                                    ),
                                    fluidRow( column(6, align = "left", uiOutput("download.button.metabolites_mr_hdl_ldl") ) ),
                                    p(""),
                                    p(""),
                                    p(""),
                                    p(""),
                                    p(""),
                                    p(""),
                                    
                                    fluidRow(htmlOutput("funnel_text_mr_hdl_ldl")),
                                    tags$head(tags$style("#funnel_text_mr_hdl_ldl{font-size: 17px;
                                               }"
                                    )
                                    ),
                                    p(""),
                                    p(""),
                                    
                                    fluidRow(
                                      
                                      
                                      tags$div( plotOutput("funnel_mr_hdl_ldl", width = "80%", height = "900px") , style = "display:block;"),
                                      tags$div(  uiOutput("download.button.funnel.plot") ,  style="display:inline-block")
                                      
                                    ),
                                    p(""),
                                    p(""),
                                    p(""),
                                    p(""),
                                    p(""),
                                    p(""),
                                    
                                    fluidRow(htmlOutput("funnel_position_text_mr_hdl_ldl")),
                                    tags$head(tags$style("#funnel_position_text_mr_hdl_ldl{font-size: 17px;
                                               }"
                                    )
                                    ),
                                    p(""),
                                    p(""),
                                    
                                    fluidRow(
                                      
                                      
                                      
                                      tags$div( plotOutput("funnel_position_mr_hdl_ldl", width = "80%", height = "900px") , style = "display:block;"),
                                      tags$div(  uiOutput("download.button.funnel_position.plot") ,  style="display:inline-block")
                                      
                                    ),
                                    
                                    
                                    
                                    
                                  ) # end fluidPage
                                  
                                  
                         )
                         
                         
                       ) # End tabPanel
                       
             ),
             
             
             nav_panel(title = "Lipoprotein subclasses",
                       tabsetPanel(
                         
                         tabPanel(title = "Observational analyses",
                                  
                                  p(""),
                                  tags$span(style = "color:#c24700; font-size:14pt", "Results for Lipoprotein subclasses") ,
                                  p(""),
                                  tags$span(style = "color:black; font-size:13pt", "Please select a metabolite and a lipoprotein.") ,
                                  tags$span(style = "color:black; font-size:13pt", "If you want to remove a metabolite or a lipoprotein, click on the respective box and press backspace.") ,
                                  tags$span(style = "color:black; font-size:13pt", "Models are adjusted for age, sex, BMI, diabetes medication use, statin use and kidney function (eGFR).") ,
                                  p(""),
                                  p(""),
                                  p(""),
                                  
                                  
                                  
                                  
                                  fluidPage(
                                    
                                    
                                    useShinyFeedback(),
                                    
                                    tags$div(  selectizeInput("metabolite_obs", "Select or type a metabolite", choices = NULL, multiple = FALSE,
                                                              selected = NULL,
                                                              options = list('plugins' = list('remove_button'),
                                                                             placeholder = '' ) )      ,  style="display:inline-block"),
                                    tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                                    tags$div(  selectizeInput("boolean_obs", "AND", choices = c("AND"), multiple = FALSE,
                                                              selected = "AND",
                                                              options = list('plugins' = list('remove_button'),
                                                                             placeholder = '' ) )          ,  style="display:inline-block; width: 100px;"),
                                    tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                                    
                                    tags$div(  selectizeInput("protein_obs", "Select or type a lipoprotein", choices = NULL, multiple = FALSE,
                                                              selected = NULL,
                                                              options = list('plugins' = list('remove_button'),
                                                                             placeholder = '' ) )          ,  style="display:inline-block"),
                                    tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                                    
                                    
                                    
                                    
                                    fluidRow(htmlOutput("result_text_obs")),
                                    tags$head(tags$style("#result_text_obs{font-size: 17px;
                                       }"
                                    )
                                    ),
                                    p(""),
                                    p(""),
                                    
                                    fluidRow(
                                      column(12,reactableOutput("selected_results")
                                      )
                                      
                                    ),
                                    fluidRow( column(6, align = "left", uiOutput("download.button.results") ) )
                                    
                                    
                                    
                                  ) # end fluidPage
                                  
                         ), # end tabsetPanel
                         
                         
                         
                       ) # End tabPanel
                       
             )
             
             
    ), # end nav_panel Tables
    
    
    
    
    nav_menu("Heatmaps",
             
             
             nav_panel(title = "Clinically measured HDL/LDL",
                       tabsetPanel(
                         
                         tabPanel(title = "Observational analyses",
                                  
                                  p(""),
                                  tags$span(style = "color:#c24700; font-size:14pt", "Results for Clinically measured HDL/LDL") ,
                                  p(""),
                                  tags$span(style = "color:black; font-size:13pt", "Please select one super- or sub-pathway, and one or several lipoproteins.", "Nominal p-value <0.01 = two stars; <0.05 = one star; ≥0.05 = no stars. Model adjusted for age, sex, BMI and kidney function (eGFR)") ,
                                  tags$span(style = "color:black; font-size:13pt", "If you want to remove a metabolite or a lipoprotein, click on the respective box and press backspace.") ,
                                  tags$span(style = "color:black; font-size:13pt", "Models are adjusted for age, sex, BMI, diabetes medication use, statin use and kidney function (eGFR).") ,
                                  p(""),
                                  p(""),
                                  p(""),
                                  
                                  
                                  fluidPage(
                                    
                                    
                                    
                                    
                                    tags$div(  selectizeInput("pathway_obs_hdl_ldl_plot", "Select or type a super- or sub-pathway", choices = NULL, multiple = FALSE,
                                                              selected = NULL,
                                                              options = list('plugins' = list('remove_button'),
                                                                             placeholder = '' ) )      ,  style="display:inline-block"),
                                    tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                                    tags$div(  selectizeInput("boolean_obs_hdl_ldl_plot", "AND", choices = c("AND"), multiple = FALSE,
                                                              selected = "AND",
                                                              options = list('plugins' = list('remove_button'),
                                                                             placeholder = '' ) )         ,  style="display:inline-block; width: 100px;"),
                                    tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                                    
                                    tags$div(  selectizeInput("protein_obs_hdl_ldl_plot", "Select or type a lipoprotein", choices = NULL, multiple = TRUE,
                                                              selected = NULL,
                                                              options = list('plugins' = list('remove_button'),
                                                                             placeholder = '' ) )          ,  style="display:inline-block"),
                                    tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                                    
                                    tags$div(  actionButton("button_obs_hdl_ldl_plot", "Plot!", style = 'margin-top:0px') ,  style="display:inline-block"),
                                    
                                    
                                    
                                    
                                    tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                                    tags$div(  uiOutput("download.button.obs.plot_hdl_ldl") ,  style="display:inline-block"),
                                    tags$div(  uiOutput("download.button.obs.plot.data_hdl_ldl") ,  style="display:inline-block"),
                                    
                                    
                                    # br(),
                                    # br(),
                                    
                                    tags$div(  plotOutput("heatmap_obs_hdl_ldl") , style = "display:block;"   )
                                    
                                  ) # end fluidPage
                                  
                         ), # end tabsetPanel
                         
                         
                         tabPanel(title = "Mendelian Randomization",
                                  
                                  p(""),
                                  tags$span(style = "color:#c24700; font-size:14pt", "Results for Clinically measured HDL/LDL") ,
                                  p(""),
                                  tags$span(style = "color:black; font-size:13pt", "Please select one super- or sub-pathway, and one or several lipoproteins.") ,
                                  tags$span(style = "color:black; font-size:13pt", "If you want to remove a super- or sub-pathway, or a lipoprotein, click on the respective box and press backspace.") ,
                                  tags$span(style = "color:black; font-size:13pt", "Heatmap shows Mendelian Randomization estimated effects (beta coefficient) from Inverse Variance Weighting.", "Nominal p-value <0.01 = two stars; <0.05 = one star; ≥0.05 = no stars.") ,
                                  p(""),
                                  p(""),
                                  p(""),
                                  
                                  
                                  
                                  fluidPage(
                                    
                                    
                                    
                                    useShinyFeedback(),
                                    
                                    tags$div(  selectizeInput("pathway_mr_hdl_ldl_plot", "Select or type a super- or sub-pathway", choices = NULL, multiple = FALSE,
                                                              selected = NULL,
                                                              options = list('plugins' = list('remove_button'),
                                                                             placeholder = '' ) )      ,  style="display:inline-block"),
                                    tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                                    tags$div(  selectizeInput("boolean_mr_hdl_ldl_plot", "AND", choices = c("AND"), multiple = FALSE,
                                                              selected = "AND",
                                                              options = list('plugins' = list('remove_button'),
                                                                             placeholder = '' ))         ,  style="display:inline-block; width: 100px;"),
                                    tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                                    
                                    tags$div(  selectizeInput("protein_mr_hdl_ldl_plot", "Select or type a lipoprotein", choices = NULL, multiple = TRUE,
                                                              selected = NULL,
                                                              options = list('plugins' = list('remove_button'),
                                                                             placeholder = '' ) )          ,  style="display:inline-block"),
                                    tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                                    
                                    tags$div(  actionButton("button_mr_hdl_ldl_plot", "Plot!", style = 'margin-top:0px;') ,  style="display:inline-block"),
                                    
                                    
                                    
                                    
                                    
                                    tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                                    tags$div(  uiOutput("download.button.mr.plot_hdl_ldl") ,  style="display:inline-block"),
                                    tags$div(  uiOutput("download.button.mr.plot.data_hdl_ldl") ,  style="display:inline-block"),
                                    
                                    
                                    
                                    br(),
                                    br(),
                                    
                                    tags$div( plotOutput("heatmap_mr_hdl_ldl") , style = "display:block;" )
                                    
                                    
                                    
                                  ) # end fluidPage
                                  
                                  
                         )
                         
                         
                       ) # End tabPanel
                       
             ), # end nav_panel
             
             
             
             
             nav_panel(title = "Lipoprotein subclasses",
                       tabsetPanel(
                         
                         tabPanel(title = "Observational analyses",
                                  
                                  p(""),
                                  tags$span(style = "color:#c24700; font-size:14pt", "Results for Lipoprotein subclasses") ,
                                  p(""),
                                  tags$span(style = "color:black; font-size:13pt", "Please select one super- or sub-pathway, and one or several lipoproteins.", "Nominal p-value <0.01 = two stars; <0.05 = one star; ≥0.05 = no stars. Model adjusted for age, sex, BMI and kidney function (eGFR)") ,
                                  tags$span(style = "color:black; font-size:13pt", "If you want to remove a metabolite or a lipoprotein, click on the respective box and press backspace.") ,
                                  tags$span(style = "color:black; font-size:13pt", "Models are adjusted for age, sex, BMI, diabetes medication use, statin use and kidney function (eGFR).") ,
                                  p(""),
                                  p(""),
                                  p(""),
                                  
                                  
                                  fluidPage(
                                    
                                    
                                    
                                    
                                    tags$div(  selectizeInput("pathway_obs_plot", "Select or type a super- or sub-pathway", choices = NULL, multiple = FALSE,
                                                              selected = NULL,
                                                              options = list('plugins' = list('remove_button'),
                                                                             placeholder = '' ) )      ,  style="display:inline-block"),
                                    tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                                    tags$div(  selectizeInput("boolean_obs_plot", "AND", choices = c("AND"), multiple = FALSE,
                                                              selected = "AND",
                                                              options = list('plugins' = list('remove_button'),
                                                                             placeholder = '' ) )         ,  style="display:inline-block; width: 100px;"),
                                    tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                                    
                                    tags$div(  selectizeInput("protein_obs_plot", "Select or type a lipoprotein", choices = NULL, multiple = TRUE,
                                                              selected = NULL,
                                                              options = list('plugins' = list('remove_button'),
                                                                             placeholder = '' ) )          ,  style="display:inline-block"),
                                    tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                                    
                                    tags$div(  actionButton("button_obs_plot", "Plot!", style = 'margin-top:0px') ,  style="display:inline-block"),
                                    
                                    
                                    
                                    
                                    tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                                    tags$div(  uiOutput("download.button.obs.plot") ,  style="display:inline-block"),
                                    tags$div(  uiOutput("download.button.obs.plot.data") ,  style="display:inline-block"),
                                    
                                    
                                    br(),
                                    br(),
                                    
                                    tags$div(  plotOutput("heatmap_obs") , style = "display:block;"   )
                                    
                                  ) # end fluidPage
                                  
                         ), # end tabsetPanel
                         
                         
                         
                         
                       ) # End tabPanel
                       
             )
             
    ), # end nav_panel
    
    
    
    nav_panel(title = "Supplemental Material",
              p(""),
              p(""),
              tags$span(style = "color:black; font-size:13pt", "Here you can find all supplementary tables and files to our research article."),
              tags$span(style = "color:black; font-size:13pt", "For each table, you can either sort and download it completely, or you can search, filter, sort and download your customized table."),
              # tags$span(style = "color:black; font-size:13pt", "Also, if you cannot see the tables below, please wait, it could take up to 45 seconds for them to show.") ,
              
              p(""),
              p(""),
              
              p( tags$span(style = "font-size:17px", tags$b("Supplementary Table 1."), "Description of the 13 measured lipoproteins used in Observational analyses.") ),
              
              # Supplementary 1; table render and download botton
              fluidRow(
                column(12,
                       reactableOutput("table_supp1"),
                       # tableOutput("test"),
                       
                       shiny::downloadButton(
                         "downloadData_1", "Download",
                         onClick = "Shiny.setInputValue('table_state:to_csv', Reactable.getState('table_supp1').sortedData)"
                       )
                )
              ),
              
              p(""),
              p(""),
              p(""),
              p(""),
              
              p( tags$span(style = "font-size:17px", tags$b("Supplementary Table 2."), "Description of the genetic instruments of lipoproteins' levels used for the Mendelian Randomization analyses.") ),
              
              # Supplementary 2; table render and download botton
              fluidRow(
                column(12,
                       reactableOutput("table_supp2"),
                       # tableOutput("test"),
                       
                       shiny::downloadButton(
                         "downloadData_2", "Download",
                         onClick = "Shiny.setInputValue('table_state:to_csv', Reactable.getState('table_supp2').sortedData)"
                       )
                )
              ),
              
              p(""),
              p(""),
              p(""),
              p(""),
              
              p( tags$span(style = "font-size:17px", tags$b("Supplementary Table 3."), "Description of the 790 non-xenobiotic metabolites analysed in the study.") ),
              
              # Supplementary 3; table render and download botton
              fluidRow(
                column(12,
                       reactableOutput("table_supp3"),
                       # tableOutput("test")
                       shiny::downloadButton(
                         "downloadData_3", "Download",
                         onClick = "Shiny.setInputValue('table_state:to_csv', Reactable.getState('table_supp3').sortedData)"
                       )
                )
              ),
              
              p(""),
              p(""),
              p(""),
              p(""),
              
              p( tags$span(style = "font-size:17px", tags$b("Supplementary Table 4."), "Baseline characteristics of study participants from SCAPIS, EpiHealth, POEM and PIVUS studies. Means and (SD) are given, or proportions in %.") ),
              
              # Supplementary 4; table render and download botton
              fluidRow(
                column(12,
                       reactableOutput("table_supp4"),
                       shiny::downloadButton(
                         "downloadData_4", "Download",
                         onClick = "Shiny.setInputValue('table_state:to_csv', Reactable.getState('table_supp4').sortedData)"
                       )
                )
              ),
              
              p(""),
              p(""),
              p(""),
              p(""),
              
              p( tags$span(style = "font-size:17px", tags$b("Supplemental Files 1-8") ) ),
              # p(""),
              fluidRow(column(6, downloadButton("statFile", "Download" ) ) )
              
              
    ), # end nav_panel Downloads
    
    
    
    
    nav_spacer(),
    
    nav_panel(title = "Contact",
              p(""),
              p(""),
              tags$span(style = "color:black; font-size:13pt", "We would love to hear from you!"),
              p(""),
              tags$span(style = "color:black; font-size:13pt", "Please get in touch with Professor Lars Lind at", tags$span(style = "color: #2D89C8", "lars.lind@medsci.uu.se"  ), "and with Dr. Rui Zheng at", tags$span(style = "color: #2D89C8", "rui.zheng@uu.se"  ) ),
              p(""),
              p(""),
              p(""),
              p(tags$b("In Collaboration with") ),
              fluidRow(
                column(3, tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/c/cb/Uppsala_universitet_logo.jpg", width = "190px", height = "190px")) ,
                column(3, tags$img(src = "https://upload.wikimedia.org/wikipedia/en/3/3a/Lunds_universitet.svg", width = "180px", height = "180px")) ,
                column(3, tags$img(src = "https://images.ctfassets.net/e8gvzq1fwq00/61AhHssAP6zsqjxPVX5CzD/d1b15d2717f2e35546f51a187ff0826f/HLF_Logotyp_120_RGB_822x222.svg", width = "200px", height = "200px") )
              )
    ),
    
    nav_panel(title = "Report a bug",
              p(""),
              p(""),
              tags$span(style = "color:black; font-size:13pt", "Help us improve!"),
              p(""),
              tags$span(style = "color:black; font-size:13pt", "Please report any bug in our website at", tags$span(style = "color: #2D89C8", "mario.delgado.velandia@uu.se"  ) ),
              tags$span(style = "color:black; font-size:13pt", "Thanks in advance."),
              p(""),
              p(""),
              p(""),
              p(tags$b("In Collaboration with") ),
              fluidRow(
                column(3, tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/c/cb/Uppsala_universitet_logo.jpg", width = "190px", height = "190px")) ,
                column(3, tags$img(src = "https://upload.wikimedia.org/wikipedia/en/3/3a/Lunds_universitet.svg", width = "180px", height = "180px")) ,
                column(3, tags$img(src = "https://images.ctfassets.net/e8gvzq1fwq00/61AhHssAP6zsqjxPVX5CzD/d1b15d2717f2e35546f51a187ff0826f/HLF_Logotyp_120_RGB_822x222.svg", width = "200px", height = "200px") )
              ) ),
    
    nav_menu(
      title = "More",
      align = "right",
      nav_item(tags$a("About Us",
                      href = "https://www.uu.se/en/department/medical-sciences/research/research-groups/clinical-epidemiology",
                      target = "_blank" )),
      nav_item(tags$a("EpiHealth",
                      href = "https://www.epihealth.lu.se/en/about-us",
                      target = "_blank" )),
      nav_item(tags$a("SCAPIS",
                      href = "https://www.scapis.org/",
                      target = "_blank" )),
      nav_item(tags$a("PIVUS",
                      href = "https://www.uu.se/en/department/medical-sciences/research/epidemiological-studies/pivus",
                      target = "_blank")),
      nav_item(tags$a("POEM",
                      href = "https://www.maelstrom-research.org/study/poem",
                      target = "_blank" ))
      
    )
    
    
    
  ) # end pageNavbar
) # end fluidPage





# Server side----
server <- function(input, output, session) {
  
  
  
  
  
  # HDL-LDL start----
  
  
  
  # Input alerts for selection boxes----
  
  
  
  observeEvent(input$button_obs_hdl_ldl,{
    
    if ( input$metabolite_obs_hdl_ldl == "" && input$protein_obs_hdl_ldl == "" ) {
      showFeedbackWarning(
        inputId = "metabolite_obs_hdl_ldl",
        text = "Please select a metabolite from the list"
      )
      showFeedbackWarning(
        inputId = "protein_obs_hdl_ldl",
        text = "Please select a protein from the list"
      )
      
    } else if( input$metabolite_obs_hdl_ldl == "" && input$boolean_obs_hdl_ldl == "AND" ){
      showFeedbackWarning(
        inputId = "metabolite_obs_hdl_ldl",
        text = "Please select a metabolite from the list"
      )
      hideFeedback("protein_obs_hdl_ldl")
      
    } else if( input$protein_obs_hdl_ldl == "" && input$boolean_obs_hdl_ldl == "AND" ){
      showFeedbackWarning(
        inputId = "protein_obs_hdl_ldl",
        text = "Please select a protein from the list"
      )
      hideFeedback("metabolite_obs_hdl_ldl")
      
    }  else {
      hideFeedback("metabolite_obs_hdl_ldl")
      hideFeedback("protein_obs_hdl_ldl")
    }
    
  }
  )
  
  
  
  
  
  # Selection boxes----
  updateSelectizeInput(session, 'metabolite_obs_hdl_ldl', choices = metabolite_obs_hdl_ldl_str , server = TRUE, selected = "" )
  updateSelectizeInput(session, 'protein_obs_hdl_ldl', choices = c("HDL cholesterol", "LDL cholesterol"), server = TRUE, selected = "" )
  
  
  
  # Table titles to display----
  line_1 <- eventReactive(isTruthy(input$metabolite_obs_hdl_ldl) && isTruthy(input$protein_obs_hdl_ldl),{
    
    req(isTruthy(input$metabolite_obs_hdl_ldl) && isTruthy(input$protein_obs_hdl_ldl))
    
    line1 <- paste("<br>", "<br>", "<br>", "<b>Table 1. ", "</b>", "Associations between clinically measured lipoproteins and plasma metabolites. Model adjusted for age and sex.")
    
  } )
  
  
  
  
  
  
  
  ## Rendering Table titles----
  output$result_text_obs_hdl_ldl  <- renderText( { line_1() } )
  
  
  
  
  # Results tables to display----
  selected_results_df_hdl_ldl <- eventReactive( isTruthy(input$metabolite_obs_hdl_ldl) && isTruthy(input$protein_obs_hdl_ldl) ,{
    
    req( isTruthy(input$metabolite_obs_hdl_ldl) && isTruthy(input$protein_obs_hdl_ldl) )
    
    df <- obs_df_hdl_ldl[ which( obs_df_hdl_ldl$`Lipoprotein` %in% c( input$protein_obs_hdl_ldl ) &  obs_df_hdl_ldl$Metabolite %in% c(input$metabolite_obs_hdl_ldl) ), ]
    
    return(df)
    
    print(df[])
    
  })
  
  
  
  
  
  ## Rendering Results tables----
  observeEvent( isTruthy(input$metabolite_obs_hdl_ldl) && isTruthy(input$protein_obs_hdl_ldl) , {
    
    req( isTruthy(input$metabolite_obs_hdl_ldl) && isTruthy(input$protein_obs_hdl_ldl) )
    
    
    output$selected_results_hdl_ldl <- renderReactable({
      reactable(selected_results_df_hdl_ldl(),
                sortable = FALSE,
                filterable = FALSE,
                searchable = FALSE,
                bordered = TRUE,
                highlight = TRUE,
                defaultColDef = colDef(
                  align = "center",
                  headerStyle = list(background = "#f7f7f8"),
                  style = JS("function(rowInfo, column, state) {
                           // Highlight sorted columns
                           for (let i = 0; i < state.sorted.length; i++) {
                                  if (state.sorted[i].id === column.id) { 
                                   return { background: 'rgba(0, 0, 0, 0.03)' }
                                                                    }
                                                                  }
                                                                }")
                ),
                columns = list(
                  `Beta` = colDef(minWidth = 50,
                                  filterable = FALSE,
                                  cell = function(value) format(value, digits = 2, scientific = FALSE ) ),
                  `SE` = colDef(minWidth = 50,
                                filterable = FALSE,
                                cell = function(value) format(value, digits = 3, scientific = FALSE ) ),
                  `Nominal p-value` = colDef(minWidth = 50,
                                             filterable = FALSE,
                                             cell = function(value) format(value, digits = 2, scientific = TRUE ) ),
                  `FDR-adjusted p-value` = colDef(minWidth = 50,
                                                  filterable = FALSE,
                                                  cell = function(value) format(value, digits = 3, scientific = TRUE ) )
                ),
                defaultPageSize = 5,
                showPageSizeOptions = FALSE,
                # pageSizeOptions = c(5, 10, 25, 50, 100),
                # paginationType = "jump",
                showPagination = TRUE ) } )
    
  })
  
  
  observeEvent(input$button_obs_hdl_ldl, {
    
    req( isTruthy(input$metabolite_obs_hdl_ldl) || isTruthy(input$protein_obs_hdl_ldl) )
    
    
    output$selected_protein_details <- renderReactable({
      reactable(selected_protein_details_df(),
                filterable = TRUE,
                searchable = TRUE,
                bordered = TRUE,
                highlight = TRUE,
                defaultColDef = colDef(
                  align = "center",
                  headerStyle = list(background = "#f7f7f8"),
                  style = JS("function(rowInfo, column, state) {
                           // Highlight sorted columns
                           for (let i = 0; i < state.sorted.length; i++) {
                                  if (state.sorted[i].id === column.id) { 
                                   return { background: 'rgba(0, 0, 0, 0.03)' }
                                                                    }
                                                                  }
                                                                }")
                ),
                defaultPageSize = 5,
                showPageSizeOptions = TRUE,
                pageSizeOptions = c(5, 10, 25, 50, 100),
                paginationType = "jump",
                showPagination = TRUE ) } )
    
  } )
  
  
  observeEvent(input$button_obs_hdl_ldl, {
    
    req( isTruthy(input$metabolite_obs_hdl_ldl) || isTruthy(input$protein_obs_hdl_ldl) )
    
    
    output$selected_metabolite_details <- renderReactable({
      reactable(selected_metabolite_details_df(),
                filterable = TRUE,
                searchable = TRUE,
                bordered = TRUE,
                highlight = TRUE,
                defaultColDef = colDef(
                  align = "center",
                  headerStyle = list(background = "#f7f7f8"),
                  style = JS("function(rowInfo, column, state) {
                           // Highlight sorted columns
                           for (let i = 0; i < state.sorted.length; i++) {
                                  if (state.sorted[i].id === column.id) { 
                                   return { background: 'rgba(0, 0, 0, 0.03)' }
                                                                    }
                                                                  }
                                                                }")
                ),
                defaultPageSize = 5,
                showPageSizeOptions = TRUE,
                pageSizeOptions = c(5, 10, 25, 50, 100),
                paginationType = "jump",
                showPagination = TRUE ) } )
    
  } )
  
  
  
  # Download buttons-----
  observeEvent( isTruthy(input$metabolite_obs_hdl_ldl) && isTruthy(input$protein_obs_hdl_ldl) ,{
    
    req(isTruthy(input$metabolite_obs_hdl_ldl) && isTruthy(input$protein_obs_hdl_ldl))
    
    output$download.button.results_hdl_ldl <- renderUI({
      
      req( selected_results_df_hdl_ldl() )
      
      shiny::downloadButton(
        "download_results_hdl_ldl", "Download",
        onClick = "Shiny.setInputValue('table_state:to_csv', Reactable.getState('selected_results_hdl_ldl').sortedData)")
    } )
    
    
    
  } )
  
  
  # observeEvent(input$button_obs_hdl_ldl,{
  #   
  #   req(isTruthy(input$metabolite_obs_hdl_ldl) || isTruthy(input$protein_obs_hdl_ldl))
  #   
  #   output$download.button.proteins <- renderUI({
  #     
  #     req( selected_protein_details_df() )
  #     
  #     shiny::downloadButton(
  #       "download_proteins", "Download",
  #       onClick = "Shiny.setInputValue('table_state:to_csv', Reactable.getState('selected_protein_details').sortedData)")
  #   } )
  #   
  # } )
  # 
  # observeEvent(input$button_obs_hdl_ldl,{
  #   
  #   req(isTruthy(input$metabolite_obs_hdl_ldl) || isTruthy(input$protein_obs_hdl_ldl))
  #   
  #   output$download.button.metabolites  <- renderUI({
  #     
  #     req( selected_metabolite_details_df() )
  #     
  #     shiny::downloadButton(
  #       "download_metabolites", "Download",
  #       onClick = "Shiny.setInputValue('table_state:to_csv', Reactable.getState('selected_metabolite_details').sortedData)")
  #   } )
  #   
  # } )
  
  
  ## Rendering Download buttons----
  observeEvent( isTruthy(input$metabolite_obs_hdl_ldl) && isTruthy(input$protein_obs_hdl_ldl) , {
    
    req( isTruthy(input$metabolite_obs_hdl_ldl) && isTruthy(input$protein_obs_hdl_ldl) )
    
    
    output$download_results_hdl_ldl <- downloadHandler(
      filename = function() {
        paste0("Results_obs_", input$protein_obs_hdl_ldl, "_", Sys.time(), ".csv", sep = "")
      },
      content = function(file) {
        data <- input$table_state
        write.csv(data, file, row.names = FALSE)
      }
    )
    
    
  })
  
  
  
  # observeEvent(input$button_obs_hdl_ldl, {
  #   
  #   output$download_proteins <- downloadHandler(
  #     filename = function() {
  #       paste("Proteins_obs_hdl_ldl_", Sys.Date(), ".csv", sep = "")
  #     },
  #     content = function(file) {
  #       data <- input$table_state
  #       write.csv(data, file, row.names = FALSE)
  #     }
  #   )
  #   
  #   
  # })
  # 
  # 
  # 
  # observeEvent(input$button_obs_hdl_ldl, {
  #   
  #   output$download_metabolites <- downloadHandler(
  #     filename = function() {
  #       paste("Metabolites_obs_hdl_ldl_", Sys.Date(), ".csv", sep = "")
  #     },
  #     content = function(file) {
  #       data <- input$table_state
  #       write.csv(data, file, row.names = FALSE)
  #     }
  #   )
  #   
  #   
  # })
  
  # Observational Analyses page end
  
  
  
  
  
  # Mendelian Randomization page start----
  
  
  
  observeEvent(input$button_mr_hdl_ldl,{
    
    if ( input$metabolite_mr_hdl_ldl == "" && input$protein_mr_hdl_ldl == "" ) {
      showFeedbackWarning(
        inputId = "metabolite_mr_hdl_ldl",
        text = "Please select a metabolite from the list"
      )
      showFeedbackWarning(
        inputId = "protein_mr_hdl_ldl",
        text = "Please select a protein from the list"
      )
      
    } else if( input$metabolite_mr_hdl_ldl == "" && input$boolean_mr_hdl_ldl == "AND" ){
      showFeedbackWarning(
        inputId = "metabolite_mr_hdl_ldl",
        text = "Please select a metabolite from the list"
      )
      hideFeedback("protein_mr_hdl_ldl")
      
    } else if( input$protein_mr_hdl_ldl == "" && input$boolean_mr_hdl_ldl == "AND" ){
      showFeedbackWarning(
        inputId = "protein_mr_hdl_ldl",
        text = "Please select a protein from the list"
      )
      hideFeedback("metabolite_mr_hdl_ldl")
      
    } else if ( input$metabolite_mr_hdl_ldl == "" && input$protein_mr_hdl_ldl == "" ) {
      showFeedbackWarning(
        inputId = "metabolite_mr_hdl_ldl",
        text = "Please select a metabolite from the list"
      )
      showFeedbackWarning(
        inputId = "protein_mr_hdl_ldl",
        text = "Please select a protein from the list"
      )
      
    } else {
      hideFeedback("metabolite_mr_hdl_ldl")
      hideFeedback("protein_mr_hdl_ldl")
    }
    
  }
  )
  
  
  
  
  # Selection boxes----
  updateSelectizeInput(session, 'metabolite_mr_hdl_ldl', choices = metabolite_mr_hdl_ldl_str , server = TRUE, selected = "" )
  updateSelectizeInput(session, 'protein_mr_hdl_ldl', choices = c("HDL cholesterol", "LDL cholesterol"), server = TRUE, selected = "" )
  
  
  
  
  
  
  
  
  
  line_1_mr_hdl_ldl <- eventReactive((isTruthy(input$metabolite_mr_hdl_ldl) && isTruthy(input$protein_mr_hdl_ldl)),{
    
    req(isTruthy(input$metabolite_mr_hdl_ldl) && isTruthy(input$protein_mr_hdl_ldl))
    
    line1 <- paste("<br>", "<br>", "<br>", "<b>Table 1. ", "</b>", "Mendelian Randomization estimated effects between genetically predicted lipoprotein levels and plasma metabolites.")
    
  } )
  
  
  line_2_mr_hdl_ldl <- eventReactive((isTruthy(input$metabolite_mr_hdl_ldl) && input$protein_mr_hdl_ldl!= "S-LDL" && input$protein_mr_hdl_ldl!= ""),{
    
    req(isTruthy(input$metabolite_mr_hdl_ldl) && input$protein_mr_hdl_ldl!= "S-LDL" && input$protein_mr_hdl_ldl!= "")
    
    line2 <- paste("<br>", "<br>", "<br>", "<b>Figure 1. ", "</b>", "Scatter plot of Mendelian Randomization estimated effects between genetically predicted lipoprotein levels and plasma metabolites.")
    
  } )
  
  line_3_mr_hdl_ldl <- eventReactive((isTruthy(input$metabolite_mr_hdl_ldl) && input$protein_mr_hdl_ldl!= "S-LDL" && input$protein_mr_hdl_ldl!= ""),{
    
    req(isTruthy(input$metabolite_mr_hdl_ldl) && input$protein_mr_hdl_ldl!= "S-LDL" && input$protein_mr_hdl_ldl!= "")
    
    line3 <- paste("<br>", "<br>", "<br>", "<b>Figure 2. ", "</b>", "Forest plot of Mendelian Randomization estimated effects between genetically predicted lipoprotein levels and plasma metabolites.")
    
  } )
  
  line_4_mr_hdl_ldl <- eventReactive((isTruthy(input$metabolite_mr_hdl_ldl) && input$protein_mr_hdl_ldl!= "S-LDL" && input$protein_mr_hdl_ldl!= ""),{
    
    req(isTruthy(input$metabolite_mr_hdl_ldl) && input$protein_mr_hdl_ldl!= "S-LDL" && input$protein_mr_hdl_ldl!= "")
    
    line4 <- paste("<br>", "<br>", "<br>", "<b>Figure 3. ", "</b>", "Funnel plot of Mendelian Randomization estimated effects between genetically predicted lipoprotein levels and plasma metabolites.")
    
  } )
  
  line_warning_mr_hdl_ldl <- eventReactive((isTruthy(input$metabolite_mr_hdl_ldl) && input$protein_mr_hdl_ldl== "S-LDL" ),{
    
    req(isTruthy(input$metabolite_mr_hdl_ldl) && input$protein_mr_hdl_ldl== "S-LDL" )
    
    line <- paste("<br>", "<br>", "<br>", "<b>", "</b>", "Note: there are only 2 available genetic instruments for S-LDL. Egger-MR and weighted median methods cannot be performed, and figures cannot be plotted.")
    
  } )
  
  
  
  
  
  
  
  
  
  
  
  
  
  ## Rendering Table titles----
  output$table_text_mr_hdl_ldl  <- renderText( { line_1_mr_hdl_ldl() } )
  
  output$scatter_text_mr_hdl_ldl  <- renderText( { line_2_mr_hdl_ldl() } )
  
  output$forest_text_mr_hdl_ldl  <- renderText( { line_3_mr_hdl_ldl() } )
  
  output$funnel_text_mr_hdl_ldl  <- renderText( { line_4_mr_hdl_ldl() } )
  
  output$warning_text_mr_hdl_ldl  <- renderText( { line_warning_mr_hdl_ldl() } )
  
  
  
  
  
  # Results tables to display----
  selected_results_df_mr_hdl_ldl <- eventReactive( (isTruthy(input$metabolite_mr_hdl_ldl) && isTruthy(input$protein_mr_hdl_ldl)),{
    
    req( isTruthy(input$metabolite_mr_hdl_ldl) && isTruthy(input$protein_mr_hdl_ldl))
    
    
    df5 <- harmonised_df_hdl_ldl[ which( ( harmonised_df_hdl_ldl$exposure %in% c( input$protein_mr_hdl_ldl ) ) &  harmonised_df_hdl_ldl$outcome %in% c(input$metabolite_mr_hdl_ldl) ), ]
    
    mr_df <- mr( df5 , method_list = c("mr_ivw", "mr_egger_regression", "mr_weighted_median") )
    
    mr_df <- mr_df %>% left_join(met_anno[c("Metabolite", "Super pathway", "Sub pathway")], join_by("outcome" == "Metabolite"))
    
    mr_df <- mr_df %>% left_join(fdr_df_hdl_ldl[which(fdr_df_hdl_ldl$exposure %in% c(input$protein_mr_hdl_ldl) & fdr_df_hdl_ldl$outcome %in% c(input$metabolite_mr_hdl_ldl) ), c("method", "FDR-adjusted p-value")], join_by("method" == "method"))
    
    mr_df <- mr_df %>% left_join(heterogeneity_hdl_ldl[which(heterogeneity_hdl_ldl$exposure %in% c(input$protein_mr_hdl_ldl) & heterogeneity_hdl_ldl$outcome %in% c(input$metabolite_mr_hdl_ldl) ), c("method", "Q", "Q_pval")], join_by("method" == "method"))
    
    mr_df <- mr_df[c("exposure", "Super pathway", "Sub pathway", "outcome", "method", "nsnp", "b", "se", "pval", "FDR-adjusted p-value", "Q", "Q_pval")]
    
    colnames(mr_df) <- c("Lipoprotein", "Super pathway", "Sub pathway", "Metabolite", "Method", "Number of SNPs", "Beta", "SE", "Nominal p-value", "FDR-adjusted p-value", "Q statistic", "Q statistic - p-value")
    
    
    return(mr_df)
    
  })
  
  
  
  selected_protein_details_df_mr_hdl_ldl <- eventReactive((isTruthy(input$metabolite_mr_hdl_ldl) && input$protein_mr_hdl_ldl!= "S-LDL" && input$protein_mr_hdl_ldl!= ""),{
    
    req( isTruthy(input$metabolite_mr_hdl_ldl) && input$protein_mr_hdl_ldl!= "S-LDL" && input$protein_mr_hdl_ldl!= "" )
    
    df6 <- harmonised_df_hdl_ldl[ which( ( harmonised_df_hdl_ldl$exposure %in% c( input$protein_mr_hdl_ldl ) ) &  harmonised_df_hdl_ldl$outcome %in% c(input$metabolite_mr_hdl_ldl) ), ]
    
    pleiotropy_df <- mr_pleiotropy_test(df6)
    
    pleiotropy_df <- pleiotropy_df[c("egger_intercept", "se", "pval")]
    
    colnames(pleiotropy_df) <- c("Egger intercept", "SE", "p-value")
    
    return(pleiotropy_df)
    
  })
  
  
  
  
  
  
  ## Rendering Results tables----
  
  
  observeEvent((isTruthy(input$metabolite_mr_hdl_ldl) && input$protein_mr_hdl_ldl!= "S-LDL" && input$protein_mr_hdl_ldl!= ""), { # for forest plot
    
    
    req( isTruthy(input$pathway_mr_hdl_ldl) && isTruthy(input$protein_mr_hdl_ldl)  )
    
    
    final_height_forest <- NULL
    makeReactiveBinding("final_height")
    
    
    
  }
  
  )
  
  
  
  
  observeEvent( (isTruthy(input$metabolite_mr_hdl_ldl) && isTruthy(input$protein_mr_hdl_ldl)), {
    
    req( isTruthy(input$metabolite_mr_hdl_ldl) && isTruthy(input$protein_mr_hdl_ldl) )
    
    
    output$selected_results_mr_hdl_ldl <- renderReactable({
      reactable(selected_results_df_mr_hdl_ldl(),
                sortable = FALSE,
                filterable = FALSE,
                searchable = FALSE,
                bordered = TRUE,
                highlight = TRUE,
                defaultColDef = colDef(
                  align = "center",
                  headerStyle = list(background = "#f7f7f8"),
                  style = JS("function(rowInfo, column, state) {
                           // Highlight sorted columns
                           for (let i = 0; i < state.sorted.length; i++) {
                                  if (state.sorted[i].id === column.id) { 
                                   return { background: 'rgba(0, 0, 0, 0.03)' }
                                                                    }
                                                                  }
                                                                }")
                ),
                columns = list(
                  Lipoprotein = colDef( minWidth = 50 ),
                  `Number of SNPs` = colDef( minWidth = 50 ),
                  Beta = colDef( minWidth = 50,
                                 cell = function(value) format(value, digits = 2, scientific = FALSE ) ),
                  SE = colDef( minWidth = 50,
                               cell = function(value) format(value, digits = 2, scientific = FALSE ) ),
                  `Nominal p-value` = colDef(minWidth = 50,
                                             cell = function(value) format(value, digits = 3, scientific = TRUE ) ),
                  `FDR-adjusted p-value` = colDef(minWidth = 50,
                                                  cell = function(value) format(value, digits = 3, scientific = TRUE ) ),
                  `Q statistic` = colDef(minWidth = 50,
                                         cell = function(value) format(value, digits = 3, scientific = TRUE ) ),
                  `Q statistic - p-value` = colDef(minWidth = 50,
                                                   cell = function(value) format(value, digits = 3, scientific = TRUE ) )
                ),
                defaultPageSize = 5,
                showPageSizeOptions = FALSE,
                showPagination = TRUE ) } )
    
  } )
  
  
  
  observeEvent( (isTruthy(input$metabolite_mr_hdl_ldl) && input$protein_mr_hdl_ldl!= "S-LDL" && input$protein_mr_hdl_ldl!= "" ), {
    
    req( isTruthy(input$metabolite_mr_hdl_ldl) && input$protein_mr_hdl_ldl!= "S-LDL" && input$protein_mr_hdl_ldl!= "" )
    
    
    output$selected_protein_details_mr_hdl_ldl <- renderReactable({
      reactable(selected_protein_details_df_mr_hdl_ldl(),
                sortable = FALSE,
                filterable = FALSE,
                searchable = FALSE,
                bordered = TRUE,
                highlight = TRUE,
                defaultColDef = colDef(
                  align = "center",
                  headerStyle = list(background = "#f7f7f8"),
                  style = JS("function(rowInfo, column, state) {
                           // Highlight sorted columns
                           for (let i = 0; i < state.sorted.length; i++) {
                                  if (state.sorted[i].id === column.id) { 
                                   return { background: 'rgba(0, 0, 0, 0.03)' }
                                                                    }
                                                                  }
                                                                }")
                ),
                columns = list(
                  `Egger intercept` = colDef(minWidth = 50,
                                             cell = function(value) format(value, digits = 2, scientific = FALSE ) ),
                  `SE` = colDef(minWidth = 50,
                                cell = function(value) format(value, digits = 2, scientific = FALSE ) ),
                  `p-value` = colDef(minWidth = 50,
                                     cell = function(value) format(value, digits = 5, scientific = FALSE ) )
                ),
                defaultPageSize = 5,
                showPageSizeOptions = FALSE,
                showPagination = FALSE ) } )
    
  } )
  
  
  
  
  
  observeEvent( (isTruthy(input$metabolite_mr_hdl_ldl) && input$protein_mr_hdl_ldl!= "S-LDL" && input$protein_mr_hdl_ldl!= "" ), {
    
    req( isTruthy(input$metabolite_mr_hdl_ldl) && input$protein_mr_hdl_ldl!= "S-LDL" && input$protein_mr_hdl_ldl!= "" )
    
    
    output$scatter_mr_hdl_ldl <- shiny::renderPlot({
      
      req( isTruthy(input$metabolite_mr_hdl_ldl) && input$protein_mr_hdl_ldl!= "S-LDL" && input$protein_mr_hdl_ldl!= "" )
      
      
      df7 <- harmonised_df_hdl_ldl[ which( ( harmonised_df_hdl_ldl$exposure %in% c( input$protein_mr_hdl_ldl ) ) &  harmonised_df_hdl_ldl$outcome %in% c(input$metabolite_mr_hdl_ldl) ), ]
      
      mr_scatter_plot_custom( mr( df7 , method_list = c("mr_egger_regression", "mr_ivw", "mr_weighted_median") ) , 
                              df7 )
      
      
    }
    
    
    )
    
  }
  
  )
  
  
  
  
  
  observeEvent( (isTruthy(input$metabolite_mr_hdl_ldl) && input$protein_mr_hdl_ldl!= "S-LDL" && input$protein_mr_hdl_ldl!= "" ) , {
    
    req( isTruthy(input$metabolite_mr_hdl_ldl) && input$protein_mr_hdl_ldl!= "S-LDL" && input$protein_mr_hdl_ldl!= "" )
    
    
    output$forest_mr_hdl_ldl <- shiny::renderPlot({
      
      req( isTruthy(input$metabolite_mr_hdl_ldl) && input$protein_mr_hdl_ldl!= "S-LDL" && input$protein_mr_hdl_ldl!= "" )
      
      
      df8 <- harmonised_df_hdl_ldl[ which( ( harmonised_df_hdl_ldl$exposure %in% c( input$protein_mr_hdl_ldl ) ) &  harmonised_df_hdl_ldl$outcome %in% c(input$metabolite_mr_hdl_ldl) ), ]
      
      mr_forest_plot_custom(mr_singlesnp( df8 , all_method = c("mr_weighted_median", "mr_egger_regression", "mr_ivw") ) )
      
      
    }
    
    
    )
    
  }
  
  )
  
  
  
  observeEvent( (isTruthy(input$metabolite_mr_hdl_ldl) && input$protein_mr_hdl_ldl!= "S-LDL" && input$protein_mr_hdl_ldl!= "" ) , {
    
    req( isTruthy(input$metabolite_mr_hdl_ldl) && input$protein_mr_hdl_ldl!= "S-LDL" && input$protein_mr_hdl_ldl!= "" )
    
    
    output$funnel_mr_hdl_ldl <- shiny::renderPlot({
      
      req( isTruthy(input$metabolite_mr_hdl_ldl) && input$protein_mr_hdl_ldl!= "S-LDL" && input$protein_mr_hdl_ldl!= "" )
      
      
      df9 <- harmonised_df_hdl_ldl[ which( ( harmonised_df_hdl_ldl$exposure %in% c( input$protein_mr_hdl_ldl ) ) &  harmonised_df_hdl_ldl$outcome %in% c(input$metabolite_mr_hdl_ldl) ), ]
      
      mr_funnel_plot_custom(mr_singlesnp( df9 , all_method = c("mr_weighted_median", "mr_ivw", "mr_egger_regression") ) )
      
      
      
    }
    
    
    )
    
  }
  
  )
  
  
  
  
  
  
  # Download buttons-----
  observeEvent( isTruthy(input$metabolite_mr_hdl_ldl) && isTruthy(input$protein_mr_hdl_ldl) ,{
    
    req( isTruthy(input$metabolite_mr_hdl_ldl) && isTruthy(input$protein_mr_hdl_ldl) )
    
    output$download.button.results_mr_hdl_ldl <- renderUI({
      
      req( selected_results_df_mr_hdl_ldl() )
      
      
      shiny::downloadButton(
        "download_results_mr_hdl_ldl", "Download",
        onClick = "Shiny.setInputValue('table_state:to_csv', Reactable.getState('selected_results_mr_hdl_ldl').sortedData)")
    })
    
  } )
  
  
  
  
  
  ## Rendering Download buttons----
  observeEvent( isTruthy(input$metabolite_mr_hdl_ldl) && isTruthy(input$protein_mr_hdl_ldl) , {
    
    req( isTruthy(input$metabolite_mr_hdl_ldl) && isTruthy(input$protein_mr_hdl_ldl) )
    
    
    output$download_results_mr_hdl_ldl <- downloadHandler(
      filename = function() {
        paste("Results_mr_", input$protein_mr_hdl_ldl, "_", Sys.time(), ".csv", sep = "")
      },
      content = function(file) {
        data <- input$table_state
        write.csv(data, file, row.names = FALSE)
      }
    )
    
    
  })
  
  
  
  # observeEvent(input$button_mr_hdl_ldl, {
  #   
  #   req( isTruthy(input$metabolite_mr_hdl_ldl) || isTruthy(input$protein_mr_hdl_ldl) )
  #   
  #   
  #   output$download_proteins_mr_hdl_ldl <- downloadHandler(
  #     filename = function() {
  #       paste("Proteins_mr_hdl_ldl_", Sys.Date(), ".csv", sep = "")
  #     },
  #     content = function(file) {
  #       data <- input$table_state
  #       write.csv(data, file, row.names = FALSE)
  #     }
  #   )
  #   
  #   
  # })
  # 
  # 
  # 
  # observeEvent(input$button_mr_hdl_ldl, {
  #   
  #   req( isTruthy(input$metabolite_mr_hdl_ldl) || isTruthy(input$protein_mr_hdl_ldl) )
  #   
  #   
  #   output$download_metabolites_mr_hdl_ldl <- downloadHandler(
  #     filename = function() {
  #       paste("Metabolites_mr_hdl_ldl_", Sys.Date(), ".csv", sep = "")
  #     },
  #     content = function(file) {
  #       data <- input$table_state
  #       write.csv(data, file, row.names = FALSE)
  #     }
  #   )
  #   
  #   
  # })
  
  
  # Mendelian Randomization page end
  
  
  
  
  
  
  
  # Heatmap page starts
  
  
  # Selection boxes Obs plot----
  
  observeEvent(input$button_obs_hdl_ldl_plot,{
    
    if ( input$pathway_obs_hdl_ldl_plot == "" && (is.null(input$protein_obs_hdl_ldl_plot )) ) {
      showFeedbackWarning(
        inputId = "pathway_obs_hdl_ldl_plot",
        text = "Please select a pathway from the list"
      )
      showFeedbackWarning(
        inputId = "protein_obs_hdl_ldl_plot",
        text = "Please select a protein from the list"
      )
      
    } else if( input$pathway_obs_hdl_ldl_plot == "" && input$boolean_obs_hdl_ldl_plot == "AND" ){
      showFeedbackWarning(
        inputId = "pathway_obs_hdl_ldl_plot",
        text = "Please select a pathway from the list"
      )
      hideFeedback("protein_obs_hdl_ldl_plot")
      
    } else if( (is.null(input$protein_obs_hdl_ldl_plot )) && input$boolean_obs_hdl_ldl_plot == "AND" ){
      showFeedbackWarning(
        inputId = "protein_obs_hdl_ldl_plot",
        text = "Please select a protein from the list"
      )
      hideFeedback("pathway_obs_hdl_ldl_plot")
      
    } else {
      hideFeedback("pathway_obs_hdl_ldl_plot")
      hideFeedback("protein_obs_hdl_ldl_plot")
    }
    
  }
  )
  
  
  
  updateSelectizeInput(session, 'pathway_obs_hdl_ldl_plot', choices = pathway_obs_hdl_ldl_plot_str, server = TRUE, selected = "" )
  updateSelectizeInput(session, 'protein_obs_hdl_ldl_plot', choices = c("HDL cholesterol", "LDL cholesterol"), server = TRUE, selected = "" )
  
  
  
    observeEvent(input$button_obs_hdl_ldl_plot,{
      # observeEvent( isTruthy(input$pathway_obs_hdl_ldl_plot) && isTruthy(input$protein_obs_hdl_ldl_plot), {# To take plot and its dimensions outside of the reactive, to save it
    
    req( isTruthy(input$pathway_obs_hdl_ldl_plot) && isTruthy(input$protein_obs_hdl_ldl_plot)  )
    
    HT_obs_hdl_ldl_plot <- NULL
    makeReactiveBinding("HT_obs_hdl_ldl_plot")
    
    final_width <- NULL
    makeReactiveBinding("final_width")
    
    final_height <- NULL
    makeReactiveBinding("final_height")
    
    df_to_download_obs_hdl_ldl <- NULL
    makeReactiveBinding("df_to_download_obs_hdl_ldl")
    
    rows_order <- NULL
    makeReactiveBinding("rows_order")
    
    files_names <- NULL
    makeReactiveBinding("files_names")
    
  }
  
  )
  
  
    observeEvent(input$button_obs_hdl_ldl_plot,{
      
    # observeEvent( isTruthy(input$pathway_obs_hdl_ldl_plot) && isTruthy(input$protein_obs_hdl_ldl_plot), {# To take plot and its dimensions outside of the reactive, to save it
    
    req( isTruthy(input$pathway_obs_hdl_ldl_plot) && isTruthy(input$protein_obs_hdl_ldl_plot)  )
    
    files_names <<- ifelse(length(input$protein_obs_hdl_ldl_plot) > 1, 
                           paste0(input$protein_obs_hdl_ldl_plot[1], "_", input$protein_obs_hdl_ldl_plot[2]),
                           input$protein_obs_hdl_ldl_plot)
    
  }
  )
  
  
  
    ht_obs_hdl_ldl <-   observeEvent(input$button_obs_hdl_ldl_plot,{
    # ht_obs_hdl_ldl <- observeEvent( isTruthy(input$pathway_obs_hdl_ldl_plot) && isTruthy(input$protein_obs_hdl_ldl_plot) ,{
    
    req( isTruthy( input$pathway_obs_hdl_ldl_plot != "" & !is.null(input$protein_obs_hdl_ldl_plot) & input$boolean_obs_hdl_ldl_plot == "AND" ) ||
           isTruthy( (input$pathway_obs_hdl_ldl_plot != "" & is.null(input$protein_obs_hdl_ldl_plot)) & input$boolean_obs_hdl_ldl_plot == "ONLY" )  ||
           isTruthy( (input$pathway_obs_hdl_ldl_plot == "" & !is.null(input$protein_obs_hdl_ldl_plot)) & input$boolean_obs_hdl_ldl_plot == "ONLY" ))
    
    
    {
      
      
      
      if(input$boolean_obs_hdl_ldl_plot == "AND"){
        
        
        req( isTruthy(input$pathway_obs_hdl_ldl_plot) && isTruthy(input$protein_obs_hdl_ldl_plot)  )
        
        
        df_filtered <- obs_df_hdl_ldl[ which( ( (obs_df_hdl_ldl$`Super pathway` %in% c( input$pathway_obs_hdl_ldl_plot ) | obs_df_hdl_ldl$`Sub pathway` %in% c( input$pathway_obs_hdl_ldl_plot ) ) & obs_df_hdl_ldl$Lipoprotein %in% c( input$protein_obs_hdl_ldl_plot ) ) ), c("Lipoprotein", "Metabolite", "Beta", "Nominal p-value") ]        
        
        showNotification("Processing your request.", type ="message",  duration = 1.5 )
        
        
      } 
      
      
    }
    
    df_to_download_obs_hdl_ldl <<- df_filtered[order(df_filtered$Lipoprotein),]
    
    
    
    
    df_filtered$Metabolite <- first_letter_uppercase(df_filtered$Metabolite)
    
    
    
    df_betas <- df_filtered %>%
      select(Lipoprotein, Metabolite, `Beta`) %>%
      pivot_wider(names_from = Metabolite, values_from = `Beta` )
    
    
    
    
    betas <- as.matrix(df_betas[!colnames(df_betas) %in% c("Lipoprotein") ])
    
    row.names(betas) <- df_betas$Lipoprotein
    
    
    
    df_pvalues <- df_filtered %>%
      select(Lipoprotein, Metabolite, `Nominal p-value`) %>%
      pivot_wider(names_from = Metabolite, values_from = `Nominal p-value` )
    
    
    
    
    pvalues <- as.matrix(df_pvalues[!colnames(df_pvalues) %in% c("Lipoprotein")])
    
    row.names(pvalues) <- df_pvalues$Lipoprotein
    
    pvalues[is.na(pvalues)] <- 1.5 # To draw absent lines
    
    
    
    # Starts function but no lines for absent metabolites
    stars_pvalues <- function(j, i, x, y, w, h, fill) {
      
      # Measure star to render in the center
      gb = textGrob("*")
      gb_w = convertWidth(grobWidth(gb), "mm")
      gb_h = convertHeight(grobHeight(gb), "mm")
      
      
      # Vector of cell positions for each group of stars
      v = pindex(pvalues, i, j)
      double_star <-  v < 0.01
      single_star <-  v >= 0.01 & v < 0.05
      no_star <-  v >= 0.05 & v <= 1
      
      
      
      grid.text("**", x[ double_star ] , y[ double_star ] - gb_h*0.35  , gp = gpar(fontsize = 16))
      grid.text("*", x[ single_star ], y[ single_star ] - gb_h*0.35   , gp = gpar(fontsize = 16))
      grid.text(" ", x[ no_star ], y[ no_star ] - gb_h*0.35   , gp = gpar(fontsize = 16))
      
    }
    
    
    
    
    # Colors for the heatmap body
    col_fun = colorRamp2(c(-1, 0, 1), c("blue", "white", "red"))
    
    
    
    
    
    
    
    stars_pvalues_cell <- function(j, i, x, y, w, h, fill) {
      
      gb = textGrob("*")
      gb_w = convertWidth(grobWidth(gb), "mm")
      gb_h = convertHeight(grobHeight(gb), "mm")
      
      
      
      if(pvalues[i, j] < 0.01 ) {
        grid.text("**", x, y - gb_h*0.35, gp = gpar(fontsize = 16))
      } else if( (pvalues[i, j] >= 0.01 & pvalues[i, j] < 0.05 )) {
        grid.text("*", x, y - gb_h*0.35, gp = gpar(fontsize = 16))
      } else if( (pvalues[i, j] >= 0.05 & (pvalues[i, j] <= 1 )) ) {
        grid.text(" ", x, y - gb_h*0.35, gp = gpar(fontsize = 16))
      }
      
    }
    
    
    # Creating vector to list lipoproteins according to their size
    real_order_obs_hdl_ldl <- c( "LDL cholesterol", "HDL cholesterol" )
    names(real_order_obs_hdl_ldl) <- c("LDL cholesterol", "HDL cholesterol")
    rows_order_obs_hdl_ldl <<- real_order_obs_hdl_ldl[real_order_obs_hdl_ldl %in% rownames(betas)]
    
    
    
    if( nrow( df_filtered ) > 400 ){
      
      ht <- Heatmap(  betas, name = "Beta", col = col_fun,
                      cluster_columns = FALSE,
                      show_row_dend = FALSE,
                      show_column_dend= FALSE,
                      row_order = rows_order_obs_hdl_ldl,
                      show_row_names = TRUE,
                      row_names_gp = gpar(fontsize = 12),
                      row_names_side = "left",
                      column_labels = colnames(betas),
                      column_order = colnames(betas)[order(colnames(betas))],
                      column_names_max_height = convertHeight( grobWidth(textGrob(colnames(betas), gpar(fontsize = 11, fontface = 1))) , "mm"),
                      column_names_side = "top",
                      column_title_side = c("top"),
                      column_names_rot = 45,
                      column_names_gp = gpar(fontsize = 11),
                      width = unit(dim(betas)[2]*0.7, "cm"), height = unit(dim(betas)[1]*0.6, "cm"),
                      layer_fun = stars_pvalues,
                      heatmap_legend_param = list(
                        title = "Beta",
                        title_gp = gpar(fontsize = 12, fontface = "bold"),
                        title_position = "leftcenter",
                        col = col_fun,
                        at = (c(-1,-0.5,0,0.5, 1)),
                        labels = c("-1", "-0.5", "0", "0.5", "1"),
                        legend_width = unit(16, "cm"),
                        labels_gp = gpar(fontsize = 12), labels_rot = 0,
                        title_gp = gpar(fontsize = 12.5, fontface = "bold"),
                        legend_direction = c("horizontal") ),
                      border = TRUE
      )
      
      ht = ComplexHeatmap::draw(ht,
                                annotation_legend_side = "bottom",
                                legend_grouping = "original",
                                heatmap_legend_side = c("top")
      )
      
      
      print("---> Running layer_fun")
      
    } else {
      
      
      ht <- Heatmap(  betas, name = "Beta", col = col_fun,
                      cluster_columns = FALSE,
                      show_row_dend = FALSE,
                      show_column_dend= FALSE,
                      row_order = rows_order_obs_hdl_ldl,
                      show_row_names = TRUE,
                      row_names_gp = gpar(fontsize = 12),
                      row_names_side = "left",
                      column_labels = colnames(betas),
                      column_order = colnames(betas)[order(colnames(betas))],
                      column_names_max_height = convertHeight( grobWidth(textGrob(colnames(betas), gpar(fontsize = 11, fontface = 1))) , "mm"),
                      column_names_side = "top",
                      column_title_side = c("top"),
                      column_names_rot = 45,
                      column_names_gp = gpar(fontsize = 11),
                      width = unit(dim(betas)[2]*0.7, "cm"), height = unit(dim(betas)[1]*0.6, "cm"),
                      cell_fun = stars_pvalues_cell,
                      heatmap_legend_param = list(
                        title = "Beta",
                        title_gp = gpar(fontsize = 12, fontface = "bold"),
                        title_position = "leftcenter",
                        col = col_fun,
                        at = (c(-1,-0.5,0,0.5, 1)),
                        labels = c("-1", "-0.5", "0", "0.5", "1"),
                        legend_width = unit(16, "cm"),
                        labels_gp = gpar(fontsize = 12), labels_rot = 0,
                        title_gp = gpar(fontsize = 12.5, fontface = "bold"),
                        legend_direction = c("horizontal") ),
                      border = TRUE
      )
      
      ht = ComplexHeatmap::draw(ht,
                                annotation_legend_side = "bottom",
                                legend_grouping = "original",
                                heatmap_legend_side = c("top")
      )
      
      
      print("---> Running cell_fun")
      
      
    }
    
    
    
    
    HT_obs_hdl_ldl_plot <<- ht
    
    
    final_width <<- (( (convertX( grobWidth(textGrob(rownames(betas), gpar(fontsize = 11, fontface = 1))) , "inch", valueOnly = TRUE)/4) + convertX(  ComplexHeatmap:::width( draw( ht)) , "inch", valueOnly = TRUE )  ) )
    
    final_height <<- ( convertX(  ComplexHeatmap:::height(draw(ht)) , "inch", valueOnly = TRUE ) )
    
    
    
    
    output$heatmap_obs_hdl_ldl <- shiny::renderPlot({
      
      draw(ht)
      
    },
    
    width = ( final_width * 72 ) ,
    
    height = ( final_height * 72 )
    
    )
    
    
    
    
  }
  
  )
  
  
  
  
  
    observeEvent(input$button_obs_hdl_ldl_plot,{
    # observeEvent( isTruthy(input$pathway_obs_hdl_ldl_plot) && isTruthy(input$protein_obs_hdl_ldl_plot) ,{
    
    req( isTruthy(input$pathway_obs_hdl_ldl_plot) && isTruthy(input$protein_obs_hdl_ldl_plot)  && input$boolean_obs_hdl_ldl_plot == "AND"  )
    
    
    output$download.button.obs.plot_hdl_ldl <- renderUI({
      
      
      shiny::downloadButton(
        "download_obs_hdl_ldl_plot", "Download Plot" )
      
      
    } )
    
    
    
  } )
  
  
  
  
  
  
  output$download_obs_hdl_ldl_plot <- downloadHandler(
    filename = function() {
      
      if( !is.null( input$pathway_obs_hdl_ldl_plot ) & is.null( input$pathway_obs_hdl_ldl_plot ) ){
        
        paste0("Heatmap_" , files_names, "_obs.svg")
        
      }else if( is.null( input$pathway_obs_hdl_ldl_plot ) & !is.null( input$pathway_obs_hdl_ldl_plot ) ){
        
        paste0("Heatmap_" , files_names, "_obs.svg")
        
      } else if( !is.null( input$pathway_obs_hdl_ldl_plot ) & !is.null( input$pathway_obs_hdl_ldl_plot ) ){
        
        paste0("Heatmap_" , files_names, "_obs.svg")
        
        
      }
      
    },
    
    content = function(file) {
      
      svg(file, width = final_width, height = final_height  )
      
      draw(HT_obs_hdl_ldl_plot)
      
      dev.off()
      
    }
    
  )
  
  
  
  
  
  observeEvent(input$button_obs_hdl_ldl_plot,{
  # observeEvent( isTruthy(input$pathway_obs_hdl_ldl_plot) && isTruthy(input$protein_obs_hdl_ldl_plot) ,{
    
    req( isTruthy(input$pathway_obs_hdl_ldl_plot) && isTruthy(input$protein_obs_hdl_ldl_plot)  && input$boolean_obs_hdl_ldl_plot == "AND"  )
    
    
    output$download.button.obs.plot.data_hdl_ldl <- renderUI({
      
      
      shiny::downloadButton(
        "download_obs_hdl_ldl_plot_data", "Download Data" )
      
      
    } )
    
    
    
  } )
  
  
  
  
  
  
  output$download_obs_hdl_ldl_plot_data <- downloadHandler(
    filename = function() {
      
      if( !is.null( input$pathway_obs_hdl_ldl_plot ) & is.null( input$pathway_obs_hdl_ldl_plot ) ){
        
        paste0("Data_" , files_names, "_obs_hdl_ldl.csv")
        
      }else if( is.null( input$pathway_obs_hdl_ldl_plot ) & !is.null( input$pathway_obs_hdl_ldl_plot ) ){
        
        paste0("Data_" , files_names, "_obs_hdl_ldl.csv")
        
      } else if( !is.null( input$pathway_obs_hdl_ldl_plot ) & !is.null( input$pathway_obs_hdl_ldl_plot ) ){
        
        paste0("Data_" , files_names, "_obs_hdl_ldl.csv")
        
        
      }
      
    },
    
    content = function(file) {
      
      write.csv(df_to_download_obs_hdl_ldl, file, row.names = FALSE)
      
    }
    
  )
  
  
  
  
  
  
  
  
  # MR heatmaps
  
  
  # Selection boxes MR plot----
  
  
  
  observeEvent(input$button_mr_hdl_ldl_plot,{
    
    if ( input$pathway_mr_hdl_ldl_plot == "" && (is.null(input$protein_mr_hdl_ldl_plot ) ) ) {
      showFeedbackWarning(
        inputId = "pathway_mr_hdl_ldl_plot",
        text = "Please select a pathway from the list"
      )
      showFeedbackWarning(
        inputId = "protein_mr_hdl_ldl_plot",
        text = "Please select a protein from the list"
      )
      
    } else if( input$pathway_mr_hdl_ldl_plot == "" && input$boolean_mr_hdl_ldl_plot == "AND" ){
      showFeedbackWarning(
        inputId = "pathway_mr_hdl_ldl_plot",
        text = "Please select a pathway from the list"
      )
      hideFeedback("protein_mr_hdl_ldl_plot")
      
    } else if( (is.null(input$protein_mr_hdl_ldl_plot ) ) && input$boolean_mr_hdl_ldl_plot == "AND" ){
      showFeedbackWarning(
        inputId = "protein_mr_hdl_ldl_plot",
        text = "Please select a protein from the list"
      )
      hideFeedback("pathway_mr_hdl_ldl_plot")
      
    } else {
      hideFeedback("pathway_mr_hdl_ldl_plot")
      hideFeedback("protein_mr_hdl_ldl_plot")
    }
    
  }
  )
  
  
  
  
  updateSelectizeInput(session, 'pathway_mr_hdl_ldl_plot', choices = pathway_obs_hdl_ldl_plot_str, server = TRUE, selected = "" )
  updateSelectizeInput(session, 'protein_mr_hdl_ldl_plot', choices = c("HDL cholesterol", "LDL cholesterol"), server = TRUE, selected = "" )
  
  
  
  observeEvent(input$button_mr_hdl_ldl_plot,{
  # observeEvent( isTruthy(input$pathway_mr_hdl_ldl_plot) && isTruthy(input$protein_mr_hdl_ldl_plot) , {
    
    
    req( isTruthy(input$pathway_mr_hdl_ldl_plot) && isTruthy(input$protein_mr_hdl_ldl_plot)  )
    
    
    
    # To take plot and its dimenstions outside of the reactive, to save it
    HT_mr_hdl_ldl_plot <- NULL
    makeReactiveBinding("HT_mr_hdl_ldl_plot")
    
    final_width_mr_hdl_ldl <- NULL
    makeReactiveBinding("final_width")
    
    final_height_mr_hdl_ldl <- NULL
    makeReactiveBinding("final_height")
    
    df_to_download <- NULL
    makeReactiveBinding("df_to_download")
    
    rows_order_mr_hdl_ldl <- NULL
    makeReactiveBinding("rows_order")
    
    files_names_obs_mr <- NULL
    makeReactiveBinding("files_names_obs_mr")
    
  }
  
  )
  
  
  observeEvent(input$button_mr_hdl_ldl_plot,{
  # observeEvent( isTruthy(input$pathway_mr_hdl_ldl_plot) && isTruthy(input$protein_mr_hdl_ldl_plot) , {
    
    req( isTruthy(input$pathway_mr_hdl_ldl_plot) && isTruthy(input$protein_mr_hdl_ldl_plot)  )
    
    files_names_obs_mr <<- ifelse(length(input$protein_mr_hdl_ldl_plot) > 1, 
                                  paste0(input$protein_mr_hdl_ldl_plot[1], "_", input$protein_mr_hdl_ldl_plot[2]),
                                  input$protein_mr_hdl_ldl_plot)
    
  }
  )
  
  
  
  
  
  
  # ht_mr_hdl_ldl <- observeEvent( input$button_mr_hdl_ldl_plot ,{
  ht_mr_hdl_ldl <- observeEvent(input$button_mr_hdl_ldl_plot,{
  # ht_mr_hdl_ldl <- observeEvent( isTruthy(input$pathway_mr_hdl_ldl_plot) && isTruthy(input$protein_mr_hdl_ldl_plot) ,{
    
    req( isTruthy( input$pathway_mr_hdl_ldl_plot != "" & !is.null(input$protein_mr_hdl_ldl_plot) & input$boolean_mr_hdl_ldl_plot == "AND" ) ||
           isTruthy( (input$pathway_mr_hdl_ldl_plot != "" & is.null(input$protein_mr_hdl_ldl_plot)) & input$boolean_mr_hdl_ldl_plot == "ONLY" )  ||
           isTruthy( (input$pathway_mr_hdl_ldl_plot == "" & !is.null(input$protein_mr_hdl_ldl_plot)) & input$boolean_mr_hdl_ldl_plot == "ONLY" ) )
    
    
    
    
    
    {
      
      
      if(input$boolean_mr_hdl_ldl_plot == "AND"){
        
        
        req( isTruthy(input$pathway_mr_hdl_ldl_plot) && isTruthy(input$protein_mr_hdl_ldl_plot)  )
        
        
        df_filtered <- htmap_hdl_ldl[ which( ( ( htmap_hdl_ldl$`Super pathway` %in% c( input$pathway_mr_hdl_ldl_plot ) | htmap_hdl_ldl$`Sub pathway` %in% c( input$pathway_mr_hdl_ldl_plot ) ) & htmap_hdl_ldl$Lipoprotein %in% c( input$protein_mr_hdl_ldl_plot ) ) ), c("Lipoprotein", "Metabolite", "Beta", "Nominal p-value") ]
        
        
        showNotification("Processing your request.", type ="message",  duration = 1.5 )
        
        
      } 
      
      
    }
    
    
    
    
    
    df_to_download <<- df_filtered[order(df_filtered$Lipoprotein),]
    
    
    
    
    df_filtered$Metabolite <- first_letter_uppercase(df_filtered$Metabolite)
    
    
    
    df_betas <- df_filtered %>%
      select(Lipoprotein, Metabolite, `Beta`) %>%
      pivot_wider(names_from = Metabolite, values_from = `Beta` )
    
    
    
    
    betas <- as.matrix(df_betas[!colnames(df_betas) %in% c("Lipoprotein") ])
    
    row.names(betas) <- df_betas$Lipoprotein
    
    
    
    df_pvalues <- df_filtered %>%
      select(Lipoprotein, Metabolite, `Nominal p-value`) %>%
      pivot_wider(names_from = Metabolite, values_from = `Nominal p-value` )
    
    
    
    
    pvalues <- as.matrix(df_pvalues[!colnames(df_pvalues) %in% c("Lipoprotein")])
    
    row.names(pvalues) <- df_pvalues$Lipoprotein
    
    pvalues[is.na(pvalues)] <- 1.5 # To draw absent lines
    
    
    
    
    
    
    stars_pvalues <- function(j, i, x, y, w, h, fill) {
      
      # Measure star to render in the center
      gb = textGrob("*")
      gb_w = convertWidth(grobWidth(gb), "mm")
      gb_h = convertHeight(grobHeight(gb), "mm")
      
      
      # Vector of cell positions for each group of stars
      v = pindex(pvalues, i, j)
      double_star <-  v < 0.01
      single_star <-  v >= 0.01 & v < 0.05
      no_star <-  v >= 0.05 & v <= 1
      
      
      grid.text("**", x[ double_star ] , y[ double_star ] - gb_h*0.35  , gp = gpar(fontsize = 16))
      grid.text("*", x[ single_star ], y[ single_star ] - gb_h*0.35   , gp = gpar(fontsize = 16))
      grid.text(" ", x[ no_star ], y[ no_star ] - gb_h*0.35   , gp = gpar(fontsize = 16))
      
      
    }
    
    
    
    stars_pvalues_cell <- function(j, i, x, y, w, h, fill) {
      
      gb = textGrob("*")
      gb_w = convertWidth(grobWidth(gb), "mm")
      gb_h = convertHeight(grobHeight(gb), "mm")
      
      
      
      if(pvalues[i, j] < 0.01 ) {
        grid.text("**", x, y - gb_h*0.35, gp = gpar(fontsize = 16))
      } else if( (pvalues[i, j] >= 0.01 & pvalues[i, j] < 0.05 )) {
        grid.text("*", x, y - gb_h*0.35, gp = gpar(fontsize = 16))
      } else if( (pvalues[i, j] >= 0.05 & (pvalues[i, j] <= 1 )) ) {
        grid.text(" ", x, y - gb_h*0.35, gp = gpar(fontsize = 16))
      }
      
    }
    
    
    
    
    # Colors for the heatmap body
    col_fun = colorRamp2(c(-1, 0, 1), c("blue", "white", "red"))
    
    
    
    
    
    
    # Custom row order
    real_order_mr_hdl_ldl <- c("LDL cholesterol", "HDL cholesterol")
    names(real_order_mr_hdl_ldl) <- c("LDL cholesterol", "HDL cholesterol")
    rows_order_mr_hdl_ldl <<- real_order_mr_hdl_ldl[real_order_mr_hdl_ldl %in% rownames(betas)]
    
    
    
    
    
    
    if( nrow( df_filtered ) > 400 ){
      
      ht <- Heatmap(  betas, name = "IVW - Beta", col = col_fun,
                      cluster_columns = FALSE,
                      show_row_dend = FALSE,
                      show_column_dend= FALSE,
                      row_order = rows_order_mr_hdl_ldl,
                      show_row_names = TRUE,
                      row_names_gp = gpar(fontsize = 12),
                      row_names_side = "left",
                      column_labels = colnames(betas),
                      column_order = colnames(betas)[order(colnames(betas))],
                      column_names_max_height = convertHeight( grobWidth(textGrob(colnames(betas), gpar(fontsize = 11, fontface = 1))) , "mm"),
                      column_names_side = "top",
                      column_title_side = c("top"),
                      column_names_rot = 45,
                      column_names_gp = gpar(fontsize = 11),
                      width = unit(dim(betas)[2]*0.7, "cm"), height = unit(dim(betas)[1]*0.6, "cm"),
                      layer_fun = stars_pvalues,
                      heatmap_legend_param = list(
                        title = "IVW - Beta",
                        title_gp = gpar(fontsize = 12, fontface = "bold"),
                        title_position = "leftcenter",
                        col = col_fun,
                        at = (c(-1,-0.5,0,0.5, 1)),
                        labels = c("-1", "-0.5", "0", "0.5", "1"),
                        legend_width = unit(16, "cm"),
                        labels_gp = gpar(fontsize = 12), labels_rot = 0,
                        title_gp = gpar(fontsize = 12.5, fontface = "bold"),
                        legend_direction = c("horizontal") ),
                      border = TRUE
      )
      
      ht = ComplexHeatmap::draw(ht,
                                annotation_legend_side = "bottom",
                                legend_grouping = "original",
                                heatmap_legend_side = c("top")
      )
      
      print("---> Running layer_fun")
      
      
    } else{
      
      ht <- Heatmap(  betas, name = "IVW - Beta", col = col_fun,
                      cluster_columns = FALSE,
                      show_row_dend = FALSE,
                      show_column_dend= FALSE,
                      row_order = rows_order_mr_hdl_ldl,
                      show_row_names = TRUE,
                      row_names_gp = gpar(fontsize = 12),
                      row_names_side = "left",
                      column_labels = colnames(betas),
                      column_order = colnames(betas)[order(colnames(betas))],
                      column_names_max_height = convertHeight( grobWidth(textGrob(colnames(betas), gpar(fontsize = 11, fontface = 1))) , "mm"),
                      column_names_side = "top",
                      column_title_side = c("top"),
                      column_names_rot = 45,
                      column_names_gp = gpar(fontsize = 11),
                      width = unit(dim(betas)[2]*0.7, "cm"), height = unit(dim(betas)[1]*0.6, "cm"),
                      cell_fun = stars_pvalues_cell,
                      heatmap_legend_param = list(
                        title = "IVW - Beta",
                        title_gp = gpar(fontsize = 12, fontface = "bold"),
                        title_position = "leftcenter",
                        col = col_fun,
                        at = (c(-1,-0.5,0,0.5, 1)),
                        labels = c("-1", "-0.5", "0", "0.5", "1"),
                        legend_width = unit(16, "cm"),
                        labels_gp = gpar(fontsize = 12), labels_rot = 0,
                        title_gp = gpar(fontsize = 12.5, fontface = "bold"),
                        legend_direction = c("horizontal") ),
                      border = TRUE
      )
      
      ht = ComplexHeatmap::draw(ht,
                                annotation_legend_side = "bottom",
                                legend_grouping = "original",
                                heatmap_legend_side = c("top")
      )
      
      print("---> Running cell_fun")
      
      
    }
    
    
    
    HT_mr_hdl_ldl_plot <<- ht
    
    
    final_width_mr_hdl_ldl <<- (( (convertX( grobWidth(textGrob(rownames(betas), gpar(fontsize = 11, fontface = 1))) , "inch", valueOnly = TRUE)/4 ) + convertX(  ComplexHeatmap:::width(ht) , "inch", valueOnly = TRUE )  ) )
    
    final_height_mr_hdl_ldl <<- ( convertX(  ComplexHeatmap:::height(ht) , "inch", valueOnly = TRUE ) )
    
    print(final_width_mr_hdl_ldl)
    print(final_height_mr_hdl_ldl)
    
    output$heatmap_mr_hdl_ldl <- shiny::renderPlot({
      
      
      draw(ht)
      
    },
    
    width = ( final_width_mr_hdl_ldl * 72 ) ,
    
    height = ( final_height_mr_hdl_ldl * 72 )
    
    )
    
    
    
    
  }
  
  )
  
  
  
  
  observeEvent(input$button_mr_hdl_ldl_plot,{
  # observeEvent( isTruthy(input$pathway_mr_hdl_ldl_plot) && isTruthy(input$protein_mr_hdl_ldl_plot) ,{
    
    req( isTruthy(input$pathway_mr_hdl_ldl_plot) && isTruthy(input$protein_mr_hdl_ldl_plot)  && input$boolean_mr_hdl_ldl_plot == "AND"  )
    
    
    output$download.button.mr.plot_hdl_ldl <- renderUI({
      
      
      shiny::downloadButton(
        "download_mr_hdl_ldl_plot", "Download Plot" )
    } )
    
    
    
  } )
  
  
  
  
  output$download_mr_hdl_ldl_plot <- downloadHandler(
    
    
    filename = function() {
      
      if( !is.null( input$pathway_mr_hdl_ldl_plot ) & is.null( input$pathway_mr_hdl_ldl_plot ) ){
        
        paste0("Heatmap_" , files_names_obs_mr, "_mr_hdl_ldl.svg")
        
      }else if( is.null( input$pathway_mr_hdl_ldl_plot ) & !is.null( input$pathway_mr_hdl_ldl_plot ) ){
        
        paste0("Heatmap_" , files_names_obs_mr, "_mr_hdl_ldl.svg")
        
      } else if( !is.null( input$pathway_mr_hdl_ldl_plot ) & !is.null( input$pathway_mr_hdl_ldl_plot ) ){
        
        paste0("Heatmap_" , files_names_obs_mr, "_mr_hdl_ldl.svg")
        
        
      }
      
    },
    
    content = function(file) {
      
      svg(file, width = final_width_mr_hdl_ldl, height = final_height_mr_hdl_ldl  )
      
      draw(HT_mr_hdl_ldl_plot)
      
      dev.off()
      
    }
    
  )
  
  
  
  
  
  
  observeEvent( input$button_mr_hdl_ldl_plot ,{
  # observeEvent( isTruthy(input$pathway_mr_hdl_ldl_plot) && isTruthy(input$protein_mr_hdl_ldl_plot) ,{
    
    req( isTruthy(input$pathway_mr_hdl_ldl_plot) && isTruthy(input$protein_mr_hdl_ldl_plot)  && input$boolean_mr_hdl_ldl_plot == "AND"  )
    
    
    output$download.button.mr.plot.data_hdl_ldl <- renderUI({
      
      
      shiny::downloadButton(
        "download_mr_hdl_ldl_plot_data", "Download Data" )
    } )
    
    
    
  } )
  
  
  
  
  
  
  output$download_mr_hdl_ldl_plot_data <- downloadHandler(
    filename = function() {
      
      if( !is.null( input$pathway_mr_hdl_ldl_plot ) & is.null( input$pathway_mr_hdl_ldl_plot ) ){
        
        paste0("Data_" , files_names_obs_mr, "_mr_hdl_ldl.csv")
        
      }else if( is.null( input$pathway_mr_hdl_ldl_plot ) & !is.null( input$pathway_mr_hdl_ldl_plot ) ){
        
        paste0("Data_" , files_names_obs_mr, "_mr_hdl_ldl.csv")
        
      } else if( !is.null( input$pathway_mr_hdl_ldl_plot ) & !is.null( input$pathway_mr_hdl_ldl_plot ) ){
        
        paste0("Data_" , files_names_obs_mr, "_mr_hdl_ldl.csv")
        
        
      }
      
    },
    
    content = function(file) {
      
      write.csv(df_to_download, file, row.names = FALSE)
      
    }
    
  )
  
  
  
  
  
  
  
  
  
  output$upset_mr_hdl_ldl <- shiny::renderPlot({
    
    
    plot
    
    
  }  )
  
  
  
  
  
  # End Heatmap
  
  
  
  
  # End HDL-LDL page
  
  
  
  
  
  
  
  # Lipoproteins page start----
  
  
  # Input alerts for selection boxes----
  
  
  
  observeEvent(input$button_obs,{
    
    if ( input$metabolite_obs == "" && input$protein_obs == "" ) {
      showFeedbackWarning(
        inputId = "metabolite_obs",
        text = "Please select a metabolite from the list"
      )
      showFeedbackWarning(
        inputId = "protein_obs",
        text = "Please select a protein from the list"
      )
      
    } else if( input$metabolite_obs == "" && input$boolean_obs == "AND" ){
      showFeedbackWarning(
        inputId = "metabolite_obs",
        text = "Please select a metabolite from the list"
      )
      hideFeedback("protein_obs")
      
    } else if( input$protein_obs == "" && input$boolean_obs == "AND" ){
      showFeedbackWarning(
        inputId = "protein_obs",
        text = "Please select a protein from the list"
      )
      hideFeedback("metabolite_obs")
      
    }  else {
      hideFeedback("metabolite_obs")
      hideFeedback("protein_obs")
    }
    
  }
  )
  
  
  
  
  
  # Selection boxes----
  updateSelectizeInput(session, 'metabolite_obs', choices = metabolite_obs_hdl_ldl_str , server = TRUE, selected = "" )
  updateSelectizeInput(session, 'protein_obs', choices = c("IDL", "L-HDL", "L-LDL", "L-VLDL", "M-HDL", "M-LDL", "M-VLDL", "S-HDL", "S-LDL", "S-VLDL", "XL-HDL", "XL-VLDL", "XS-VLDL"), server = TRUE, selected = "" )
  
  
  
  # Table titles to display----
  line_1_1 <- eventReactive(isTruthy(input$metabolite_obs) && isTruthy(input$protein_obs),{
    
    req(isTruthy(input$metabolite_obs) && isTruthy(input$protein_obs))
    
    line1 <- paste("<br>", "<br>", "<br>", "<b>Table 1. ", "</b>", "Associations between measured lipoprotein levels and plasma metabolites. Model adjusted for age and sex.")
    
  } )
  
  
  
  
  
  
  
  ## Rendering Table titles----
  output$result_text_obs  <- renderText( { line_1_1() } )
  
  
  
  
  # Results tables to display----
  selected_results_df <- eventReactive( isTruthy(input$metabolite_obs) && isTruthy(input$protein_obs) ,{
    
    req( isTruthy(input$metabolite_obs) && isTruthy(input$protein_obs) )
    
    df <- obs_df[ which( obs_df$`Lipoprotein` %in% c( input$protein_obs ) &  obs_df$Metabolite %in% c(input$metabolite_obs) ), ]
    
    return(df)
    
  })
  
  
  
  
  
  ## Rendering Results tables----
  observeEvent( isTruthy(input$metabolite_obs) && isTruthy(input$protein_obs) , {
    
    req( isTruthy(input$metabolite_obs) && isTruthy(input$protein_obs) )
    
    
    output$selected_results <- renderReactable({
      reactable(selected_results_df(),
                sortable = FALSE,
                filterable = FALSE,
                searchable = FALSE,
                bordered = TRUE,
                highlight = TRUE,
                defaultColDef = colDef(
                  align = "center",
                  headerStyle = list(background = "#f7f7f8"),
                  style = JS("function(rowInfo, column, state) {
                           // Highlight sorted columns
                           for (let i = 0; i < state.sorted.length; i++) {
                                  if (state.sorted[i].id === column.id) { 
                                   return { background: 'rgba(0, 0, 0, 0.03)' }
                                                                    }
                                                                  }
                                                                }")
                ),
                columns = list(
                  `Beta` = colDef(minWidth = 50,
                                  filterable = FALSE,
                                  cell = function(value) format(value, digits = 2, scientific = FALSE ) ),
                  `SE` = colDef(minWidth = 50,
                                filterable = FALSE,
                                cell = function(value) format(value, digits = 3, scientific = FALSE ) ),
                  `Nominal p-value` = colDef(minWidth = 50,
                                             filterable = FALSE,
                                             cell = function(value) format(value, digits = 2, scientific = TRUE ) ),
                  `FDR-adjusted p-value` = colDef(minWidth = 50,
                                                  filterable = FALSE,
                                                  cell = function(value) format(value, digits = 3, scientific = TRUE ) )
                ),
                defaultPageSize = 5,
                showPageSizeOptions = FALSE,
                # pageSizeOptions = c(5, 10, 25, 50, 100),
                # paginationType = "jump",
                showPagination = TRUE ) } )
    
  })
  
  
  observeEvent(input$button_obs, {
    
    req( isTruthy(input$metabolite_obs) || isTruthy(input$protein_obs) )
    
    
    output$selected_protein_details <- renderReactable({
      reactable(selected_protein_details_df(),
                filterable = TRUE,
                searchable = TRUE,
                bordered = TRUE,
                highlight = TRUE,
                defaultColDef = colDef(
                  align = "center",
                  headerStyle = list(background = "#f7f7f8"),
                  style = JS("function(rowInfo, column, state) {
                           // Highlight sorted columns
                           for (let i = 0; i < state.sorted.length; i++) {
                                  if (state.sorted[i].id === column.id) { 
                                   return { background: 'rgba(0, 0, 0, 0.03)' }
                                                                    }
                                                                  }
                                                                }")
                ),
                defaultPageSize = 5,
                showPageSizeOptions = TRUE,
                pageSizeOptions = c(5, 10, 25, 50, 100),
                paginationType = "jump",
                showPagination = TRUE ) } )
    
  } )
  
  
  observeEvent(input$button_obs, {
    
    req( isTruthy(input$metabolite_obs) || isTruthy(input$protein_obs) )
    
    
    output$selected_metabolite_details <- renderReactable({
      reactable(selected_metabolite_details_df(),
                filterable = TRUE,
                searchable = TRUE,
                bordered = TRUE,
                highlight = TRUE,
                defaultColDef = colDef(
                  align = "center",
                  headerStyle = list(background = "#f7f7f8"),
                  style = JS("function(rowInfo, column, state) {
                           // Highlight sorted columns
                           for (let i = 0; i < state.sorted.length; i++) {
                                  if (state.sorted[i].id === column.id) { 
                                   return { background: 'rgba(0, 0, 0, 0.03)' }
                                                                    }
                                                                  }
                                                                }")
                ),
                defaultPageSize = 5,
                showPageSizeOptions = TRUE,
                pageSizeOptions = c(5, 10, 25, 50, 100),
                paginationType = "jump",
                showPagination = TRUE ) } )
    
  } )
  
  
  
  # Download buttons-----
  observeEvent( isTruthy(input$metabolite_obs) && isTruthy(input$protein_obs) ,{
    
    req(isTruthy(input$metabolite_obs) && isTruthy(input$protein_obs))
    
    output$download.button.results <- renderUI({
      
      req( selected_results_df() )
      
      shiny::downloadButton(
        "download_results", "Download",
        onClick = "Shiny.setInputValue('table_state:to_csv', Reactable.getState('selected_results').sortedData)")
    } )
    
    
    
  } )
  
  
  observeEvent(input$button_obs,{
    
    req(isTruthy(input$metabolite_obs) || isTruthy(input$protein_obs))
    
    output$download.button.proteins <- renderUI({
      
      req( selected_protein_details_df() )
      
      shiny::downloadButton(
        "download_proteins", "Download",
        onClick = "Shiny.setInputValue('table_state:to_csv', Reactable.getState('selected_protein_details').sortedData)")
    } )
    
  } )
  
  observeEvent(input$button_obs,{
    
    req(isTruthy(input$metabolite_obs) || isTruthy(input$protein_obs))
    
    output$download.button.metabolites  <- renderUI({
      
      req( selected_metabolite_details_df() )
      
      shiny::downloadButton(
        "download_metabolites", "Download",
        onClick = "Shiny.setInputValue('table_state:to_csv', Reactable.getState('selected_metabolite_details').sortedData)")
    } )
    
  } )
  
  
  ## Rendering Download buttons----
  observeEvent( isTruthy(input$metabolite_obs) && isTruthy(input$protein_obs) , {
    
    req( isTruthy(input$metabolite_obs) && isTruthy(input$protein_obs) )
    
    
    output$download_results <- downloadHandler(
      filename = function() {
        paste("Results_Obs_", input$protein_obs, "_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        data <- input$table_state
        write.csv(data, file, row.names = FALSE)
      }
    )
    
    
  })
  
  
  
  observeEvent(input$button_obs, {
    
    output$download_proteins <- downloadHandler(
      filename = function() {
        paste("Proteins_Obs_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        data <- input$table_state
        write.csv(data, file, row.names = FALSE)
      }
    )
    
    
  })
  
  
  
  observeEvent(input$button_obs, {
    
    output$download_metabolites <- downloadHandler(
      filename = function() {
        paste("Metabolites_Obs_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        data <- input$table_state
        write.csv(data, file, row.names = FALSE)
      }
    )
    
    
  })
  
  # Observational Analyses page end
  
  
  
  
  # Heatmap page starts
  
  
  # Selection boxes Obs plot----
  
  observeEvent(input$button_obs_plot,{
    
    if ( input$pathway_obs_plot == "" && (is.null(input$protein_obs_plot )) ) {
      showFeedbackWarning(
        inputId = "pathway_obs_plot",
        text = "Please select a pathway from the list"
      )
      showFeedbackWarning(
        inputId = "protein_obs_plot",
        text = "Please select a protein from the list"
      )
      
    } else if( input$pathway_obs_plot == "" && input$boolean_obs_plot == "AND" ){
      showFeedbackWarning(
        inputId = "pathway_obs_plot",
        text = "Please select a pathway from the list"
      )
      hideFeedback("protein_obs_plot")
      
    } else if( (is.null(input$protein_obs_plot )) && input$boolean_obs_plot == "AND" ){
      showFeedbackWarning(
        inputId = "protein_obs_plot",
        text = "Please select a protein from the list"
      )
      hideFeedback("pathway_obs_plot")
      
    } else {
      hideFeedback("pathway_obs_plot")
      hideFeedback("protein_obs_plot")
    }
    
  }
  )
  
  
  
  updateSelectizeInput(session, 'pathway_obs_plot', choices = pathway_obs_hdl_ldl_plot_str, server = TRUE, selected = "" )
  updateSelectizeInput(session, 'protein_obs_plot', choices = c("IDL", "L-HDL", "L-LDL", "L-VLDL", "M-HDL", "M-LDL", "M-VLDL", "S-HDL", "S-LDL", "S-VLDL", "XL-HDL", "XL-VLDL", "XS-VLDL"), server = TRUE, selected = "" )
  
  
  
  observeEvent(input$button_obs_plot,{
  # observeEvent( isTruthy(input$pathway_obs_plot) && isTruthy(input$protein_obs_plot), {# To take plot and its dimensions outside of the reactive, to save it
    
    req( isTruthy(input$pathway_obs_plot) && isTruthy(input$protein_obs_plot)  )
    
    HT_obs_plot <- NULL
    makeReactiveBinding("HT_obs_plot")
    
    final_width <- NULL
    makeReactiveBinding("final_width")
    
    final_height <- NULL
    makeReactiveBinding("final_height")
    
    df_to_download_obs_mr <- NULL
    makeReactiveBinding("df_to_download_obs_mr")
    
    rows_order <- NULL
    makeReactiveBinding("rows_order")
    
    files_names_obs_lipoprot <- NULL
    makeReactiveBinding("files_names_obs_lipoprot")
    
  }
  
  )
  
  
  observeEvent(input$button_obs_plot,{
  # observeEvent( isTruthy(input$pathway_obs_plot) && isTruthy(input$protein_obs_plot), {# To take plot and its dimensions outside of the reactive, to save it
    
    req( isTruthy(input$pathway_obs_plot) && isTruthy(input$protein_obs_plot)  )
    
    files_names_obs_lipoprot <<- ifelse(length(input$protein_obs_plot) > 1, 
                                        paste0(input$protein_obs_plot[1], "_", input$protein_obs_plot[2]),
                                        input$protein_obs_plot)
    
  }
  )
  
  
  
  
  
  
  ht_obs <- observeEvent(input$button_obs_plot,{
  # ht_obs <- observeEvent( isTruthy(input$pathway_obs_plot) && isTruthy(input$protein_obs_plot) ,{
    
    req( isTruthy( input$pathway_obs_plot != "" & !is.null(input$protein_obs_plot) & input$boolean_obs_plot == "AND" ) ||
           isTruthy( (input$pathway_obs_plot != "" & is.null(input$protein_obs_plot)) & input$boolean_obs_plot == "ONLY" )  ||
           isTruthy( (input$pathway_obs_plot == "" & !is.null(input$protein_obs_plot)) & input$boolean_obs_plot == "ONLY" ))
    
    
    {
      
      
      
      if(input$boolean_obs_plot == "AND"){
        
        
        req( isTruthy(input$pathway_obs_plot) && isTruthy(input$protein_obs_plot)  )
        
        
        df_filtered <- obs_df[ which( ( (obs_df$`Super pathway` %in% c( input$pathway_obs_plot ) | obs_df$`Sub pathway` %in% c( input$pathway_obs_plot ) ) & obs_df$Lipoprotein %in% c( input$protein_obs_plot ) ) ), c("Lipoprotein", "Metabolite", "Beta", "Nominal p-value") ]        
        
        showNotification("Processing your request.", type ="message",  duration = 1.5 )
        
        
      } 
      
      
    }
    
    df_to_download_obs_mr <<- df_filtered[order(df_filtered$Lipoprotein),]
    
    
    
    
    df_filtered$Metabolite <- first_letter_uppercase(df_filtered$Metabolite)
    
    
    
    df_betas <- df_filtered %>%
      select(Lipoprotein, Metabolite, `Beta`) %>%
      pivot_wider(names_from = Metabolite, values_from = `Beta` )
    
    
    
    
    betas <- as.matrix(df_betas[!colnames(df_betas) %in% c("Lipoprotein") ])
    
    row.names(betas) <- df_betas$Lipoprotein
    
    
    
    df_pvalues <- df_filtered %>%
      select(Lipoprotein, Metabolite, `Nominal p-value`) %>%
      pivot_wider(names_from = Metabolite, values_from = `Nominal p-value` )
    
    
    
    
    pvalues <- as.matrix(df_pvalues[!colnames(df_pvalues) %in% c("Lipoprotein")])
    
    row.names(pvalues) <- df_pvalues$Lipoprotein
    
    pvalues[is.na(pvalues)] <- 1.5 # To draw absent lines
    
    
    
    # Starts function but no lines for absent metabolites
    stars_pvalues <- function(j, i, x, y, w, h, fill) {
      
      # Measure star to render in the center
      gb = textGrob("*")
      gb_w = convertWidth(grobWidth(gb), "mm")
      gb_h = convertHeight(grobHeight(gb), "mm")
      
      
      # Vector of cell positions for each group of stars
      v = pindex(pvalues, i, j)
      double_star <-  v < 0.01
      single_star <-  v >= 0.01 & v < 0.05
      no_star <-  v >= 0.05 & v <= 1
      
      
      
      grid.text("**", x[ double_star ] , y[ double_star ] - gb_h*0.35  , gp = gpar(fontsize = 16))
      grid.text("*", x[ single_star ], y[ single_star ] - gb_h*0.35   , gp = gpar(fontsize = 16))
      grid.text(" ", x[ no_star ], y[ no_star ] - gb_h*0.35   , gp = gpar(fontsize = 16))
      
    }
    
    
    
    
    # Colors for the heatmap body
    col_fun = colorRamp2(c(-1, 0, 1), c("blue", "white", "red"))
    
    
    
    
    
    
    
    stars_pvalues_cell <- function(j, i, x, y, w, h, fill) {
      
      gb = textGrob("*")
      gb_w = convertWidth(grobWidth(gb), "mm")
      gb_h = convertHeight(grobHeight(gb), "mm")
      
      
      
      if(pvalues[i, j] < 0.01 ) {
        grid.text("**", x, y - gb_h*0.35, gp = gpar(fontsize = 16))
      } else if( (pvalues[i, j] >= 0.01 & pvalues[i, j] < 0.05 )) {
        grid.text("*", x, y - gb_h*0.35, gp = gpar(fontsize = 16))
      } else if( (pvalues[i, j] >= 0.05 & (pvalues[i, j] <= 1 )) ) {
        grid.text(" ", x, y - gb_h*0.35, gp = gpar(fontsize = 16))
      }
      
    }
    
    
    # Creating vector to list lipoproteins according to their size
    real_order <- c("XL-VLDL", "L-VLDL", "M-VLDL", "S-VLDL", "XS-VLDL", "IDL", "L-LDL", "M-LDL", "S-LDL", "XL-HDL", "L-HDL", "M-HDL", "S-HDL")
    names(real_order) <- c("XL-VLDL", "L-VLDL", "M-VLDL", "S-VLDL", "XS-VLDL", "IDL", "L-LDL", "M-LDL", "S-LDL", "XL-HDL", "L-HDL", "M-HDL", "S-HDL")
    rows_order <<- real_order[real_order %in% rownames(betas)]
    
    
    
    if( nrow( df_filtered ) > 400 ){
      
      ht <- Heatmap(  betas, name = "Beta", col = col_fun,
                      cluster_columns = FALSE,
                      show_row_dend = FALSE,
                      show_column_dend= FALSE,
                      row_order = rows_order,
                      show_row_names = TRUE,
                      row_names_gp = gpar(fontsize = 12),
                      row_names_side = "left",
                      column_labels = colnames(betas),
                      column_order = colnames(betas)[order(colnames(betas))],
                      column_names_max_height = convertHeight( grobWidth(textGrob(colnames(betas), gpar(fontsize = 11, fontface = 1))) , "mm"),
                      column_names_side = "top",
                      column_title_side = c("top"),
                      column_names_rot = 45,
                      column_names_gp = gpar(fontsize = 11),
                      width = unit(dim(betas)[2]*0.7, "cm"), height = unit(dim(betas)[1]*0.6, "cm"),
                      layer_fun = stars_pvalues,
                      heatmap_legend_param = list(
                        title = "Beta",
                        title_gp = gpar(fontsize = 12, fontface = "bold"),
                        title_position = "leftcenter",
                        col = col_fun,
                        at = (c(-1,-0.5,0,0.5, 1)),
                        labels = c("-1", "-0.5", "0", "0.5", "1"),
                        legend_width = unit(16, "cm"),
                        labels_gp = gpar(fontsize = 12), labels_rot = 0,
                        title_gp = gpar(fontsize = 12.5, fontface = "bold"),
                        legend_direction = c("horizontal") ),
                      border = TRUE
      )
      
      ht = ComplexHeatmap::draw(ht,
                                annotation_legend_side = "bottom",
                                legend_grouping = "original",
                                heatmap_legend_side = c("top")
      )
      
      
      print("---> Running layer_fun")
      
    } else {
      
      
      ht <- Heatmap(  betas, name = "Beta", col = col_fun,
                      cluster_columns = FALSE,
                      show_row_dend = FALSE,
                      show_column_dend= FALSE,
                      row_order = rows_order,
                      show_row_names = TRUE,
                      row_names_gp = gpar(fontsize = 12),
                      row_names_side = "left",
                      column_labels = colnames(betas),
                      column_order = colnames(betas)[order(colnames(betas))],
                      column_names_max_height = convertHeight( grobWidth(textGrob(colnames(betas), gpar(fontsize = 11, fontface = 1))) , "mm"),
                      column_names_side = "top",
                      column_title_side = c("top"),
                      column_names_rot = 45,
                      column_names_gp = gpar(fontsize = 11),
                      width = unit(dim(betas)[2]*0.7, "cm"), height = unit(dim(betas)[1]*0.6, "cm"),
                      cell_fun = stars_pvalues_cell,
                      heatmap_legend_param = list(
                        title = "Beta",
                        title_gp = gpar(fontsize = 12, fontface = "bold"),
                        title_position = "leftcenter",
                        col = col_fun,
                        at = (c(-1,-0.5,0,0.5, 1)),
                        labels = c("-1", "-0.5", "0", "0.5", "1"),
                        legend_width = unit(16, "cm"),
                        labels_gp = gpar(fontsize = 12), labels_rot = 0,
                        title_gp = gpar(fontsize = 12.5, fontface = "bold"),
                        legend_direction = c("horizontal") ),
                      border = TRUE
      )
      
      ht = ComplexHeatmap::draw(ht,
                                annotation_legend_side = "bottom",
                                legend_grouping = "original",
                                heatmap_legend_side = c("top")
      )
      
      
      print("---> Running cell_fun")
      
      
    }
    
    
    
    
    HT_obs_plot <<- ht
    
    
    final_width <<- (( (convertX( grobWidth(textGrob(rownames(betas), gpar(fontsize = 11, fontface = 1))) , "inch", valueOnly = TRUE)/4) + convertX(  ComplexHeatmap:::width( draw( ht)) , "inch", valueOnly = TRUE )  ) )
    
    final_height <<- ( convertX(  ComplexHeatmap:::height(draw(ht)) , "inch", valueOnly = TRUE ) )
    
    
    
    
    output$heatmap_obs <- shiny::renderPlot({
      
      draw(ht)
      
    },
    
    width = ( final_width * 72 ) ,
    
    height = ( final_height * 72 )
    
    )
    
    
    
    
  }
  
  )
  
  
  
  
  
  
  observeEvent(input$button_obs_plot,{
  # observeEvent( isTruthy(input$pathway_obs_plot) && isTruthy(input$protein_obs_plot) ,{
    
    req( isTruthy(input$pathway_obs_plot) && isTruthy(input$protein_obs_plot)  && input$boolean_obs_plot == "AND"  )
    
    
    output$download.button.obs.plot <- renderUI({
      
      
      shiny::downloadButton(
        "download_obs_plot", "Download Plot" )
      
      
    } )
    
    
    
  } )
  
  
  
  
  
  
  output$download_obs_plot <- downloadHandler(
    filename = function() {
      
      if( !is.null( input$pathway_obs_plot ) & is.null( input$pathway_obs_plot ) ){
        
        paste0("Heatmap_" , files_names_obs_lipoprot, "_obs.svg")
        
      }else if( is.null( input$pathway_obs_plot ) & !is.null( input$pathway_obs_plot ) ){
        
        paste0("Heatmap_" , files_names_obs_lipoprot, "_obs.svg")
        
      } else if( !is.null( input$pathway_obs_plot ) & !is.null( input$pathway_obs_plot ) ){
        
        paste0("Heatmap_" , files_names_obs_lipoprot, "_obs.svg")
        
        
      }
      
    },
    
    content = function(file) {
      
      svg(file, width = final_width, height = final_height  )
      
      draw(HT_obs_plot)
      
      dev.off()
      
    }
    
  )
  
  
  
  
  
  observeEvent(input$button_obs_plot,{
  # observeEvent( isTruthy(input$pathway_obs_plot) && isTruthy(input$protein_obs_plot) ,{
    
    req( isTruthy(input$pathway_obs_plot) && isTruthy(input$protein_obs_plot)  && input$boolean_obs_plot == "AND"  )
    
    
    output$download.button.obs.plot.data <- renderUI({
      
      
      shiny::downloadButton(
        "download_obs_plot_data", "Download Data" )
      
      
    } )
    
    
    
  } )
  
  
  
  
  
  
  output$download_obs_plot_data <- downloadHandler(
    filename = function() {
      
      if( !is.null( input$pathway_obs_plot ) & is.null( input$pathway_obs_plot ) ){
        
        paste0("Data_" , files_names_obs_lipoprot, "_obs.csv")
        
      }else if( is.null( input$pathway_obs_plot ) & !is.null( input$pathway_obs_plot ) ){
        
        paste0("Data_" , files_names_obs_lipoprot, "_obs.csv")
        
      } else if( !is.null( input$pathway_obs_plot ) & !is.null( input$pathway_obs_plot ) ){
        
        paste0("Data_" , files_names_obs_lipoprot, "_obs.csv")
        
        
      }
      
    },
    
    content = function(file) {
      
      write.csv(df_to_download_obs_mr, file, row.names = FALSE)
      
    }
    
  )
  
  
  
  
  
  
  # Download page start
  
  # Supplementary table 1
  output$table_supp1 <- renderReactable({
    reactable(measured_prot_anno,
              sortable = FALSE,
              filterable = FALSE,
              searchable = FALSE,
              bordered = TRUE,
              highlight = TRUE,
              defaultColDef = colDef(
                # cell = function(value) format(value, nsmall = 1),
                align = "center",
                headerStyle = list(background = "#f7f7f8"),
                style = JS("function(rowInfo, column, state) {
                           // Highlight sorted columns
                           for (let i = 0; i < state.sorted.length; i++) {
                                  if (state.sorted[i].id === column.id) { 
                                   return { background: 'rgba(0, 0, 0, 0.03)' }
                                                                    }
                                                                  }
                                                                }")
              ),
              defaultPageSize = 13,
              showPageSizeOptions = FALSE,
              # pageSizeOptions = c(5, 10, 25, 50, 100),
              paginationType = "jump",
              showPagination = TRUE )
  })
  
  output$downloadData_1 <- downloadHandler(
    filename = function() {
      paste("SuppTable1_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- input$table_state
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # Supplementary table 2
  output$table_supp2 <- renderReactable({
    reactable(prot_anno,
              filterable = TRUE,
              searchable = TRUE,
              bordered = TRUE,
              highlight = TRUE,
              defaultColDef = colDef(
                # cell = function(value) format(value, nsmall = 1),
                align = "center",
                headerStyle = list(background = "#f7f7f8"),
                style = JS("function(rowInfo, column, state) {
                           // Highlight sorted columns
                           for (let i = 0; i < state.sorted.length; i++) {
                                  if (state.sorted[i].id === column.id) { 
                                   return { background: 'rgba(0, 0, 0, 0.03)' }
                                                                    }
                                                                  }
                                                                }")
              ),
              columns = list(
                `Effect allele frequency` = colDef(cell = function(value) format(value, digits = 2, scientific = FALSE ) ),
                `Beta` = colDef(minWidth = 50,
                                cell = function(value) format(value, digits = 2, scientific = FALSE ),
                                filterable = FALSE,
                                searchable = TRUE),
                `SE` = colDef(minWidth = 50,
                              cell = function(value) format(value, digits = 2, scientific = FALSE ),
                              filterable = FALSE,
                              searchable = TRUE),
                `p-value` = colDef(minWidth = 50,
                                   cell = function(value) format(value, digits = 2, scientific = TRUE ),
                                   filterable = FALSE,
                                   searchable = TRUE ),
                `F-statistic` = colDef(minWidth = 80,
                                       cell = function(value) format(value, digits = 2, scientific = TRUE ),
                                       filterable = FALSE,
                                       searchable = TRUE)
              ),
              defaultPageSize = 5,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(5, 10, 25, 50, 100),
              paginationType = "jump",
              showPagination = TRUE )
  })
  
  output$downloadData_2 <- downloadHandler(
    filename = function() {
      paste("SuppTable2_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- input$table_state
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # Supplementary table 3
  output$table_supp3 <- renderReactable({
    reactable(met_anno,
              filterable = TRUE,
              searchable = TRUE,
              bordered = TRUE,
              highlight = TRUE,
              defaultColDef = colDef(
                # cell = function(value) format(value, nsmall = 1),
                align = "center",
                headerStyle = list(background = "#f7f7f8"),
                style = JS("function(rowInfo, column, state) {
                           // Highlight sorted columns
                           for (let i = 0; i < state.sorted.length; i++) {
                                  if (state.sorted[i].id === column.id) { 
                                   return { background: 'rgba(0, 0, 0, 0.03)' }
                                                                    }
                                                                  }
                                                                }")
              ),
              defaultPageSize = 5,
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(5, 10, 25, 50, 100),
              paginationType = "jump",
              showPagination = TRUE )
  })
  
  output$downloadData_3 <- downloadHandler(
    filename = function() {
      paste("SuppTable3_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- input$table_state
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # Supplementary table 4
  output$table_supp4 <- renderReactable({
    reactable(studios_description,
              filterable = FALSE,
              searchable = FALSE,
              sortable = FALSE,
              bordered = TRUE,
              highlight = TRUE,
              defaultColDef = colDef(
                # cell = function(value) format(value, nsmall = 1),
                align = "center",
                headerStyle = list(background = "#f7f7f8"),
                style = JS("function(rowInfo, column, state) {
                           // Highlight sorted columns
                           for (let i = 0; i < state.sorted.length; i++) {
                                  if (state.sorted[i].id === column.id) { 
                                   return { background: 'rgba(0, 0, 0, 0.03)' }
                                                                    }
                                                                  }
                                                                }")
              ),
              defaultPageSize = 19,
              showPageSizeOptions = FALSE,
              # pageSizeOptions = c(5, 10, 25, 50, 100),
              paginationType = "jump",
              showPagination = TRUE )
  })
  
  output$downloadData_4 <- downloadHandler(
    filename = function() {
      paste("SuppTable4_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- input$table_state
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  
  # Download page finish
  
  
  
  # Download Annex
  output$statFile <- downloadHandler(
    filename = function() {
      "Supplemental Files 1-8.7z"
    },
    content=function(file) {
      file.copy("Supplemental FIles 1-8.7z", file)
    }
  )
  
  
  
} # end server



shinyApp(ui, server)

