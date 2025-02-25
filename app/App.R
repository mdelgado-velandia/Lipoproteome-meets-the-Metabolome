
# R version 4.4.1


# Load packages----
library(shiny) # 1.9.1
library(bslib) #
library(reactable) # 0.4.4
library(rio) # 1.2.3
library(tidyverse) # 2.0.0
library(shinyFeedback) # 0.4.0
library(circlize) # 0.4.16
library(ComplexHeatmap) # 2.21.1
library(shinybusy) # 0.3.3
library(fst) # 0.9.18
library(TwoSampleMR) # 0.6.8




# Set working directory----





# Load data----
# load("D:/All OneDrive/Doctorado/Postdoc/Lipoproteins - metabolomics/Shiny app/App/Data.RData")
load("Data.RData")



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
    bg = "#39604e",
    inverse = TRUE,
    theme = bslib::bs_theme(version = 5),
    
    
    
    
    
    
    nav_panel(title = "Welcome",
              p(""),
              p(""),
              tags$span(style = "color:black; font-size:13pt", "Welcome!"),
              p(""),
              tags$span(style = "color:black; font-size:13pt", "This webpage was created to provide you with the results from analyses of how lipoprotein concentrations are related to metabolites as presented in the paper ",tags$i("When the Lipoproteome Meets the Metabolome - Observational and Mendelian Randomization Analyses.") ),
              
              tags$span(style = "color:black; font-size:13pt", "The results consist of two parts."),
              
              tags$span(style = "color:black; font-size:13pt", "The first part used epidemiological observational data in which each of 13 lipoprotein concentrations were related to each of 790 non-xenobiotic metabolites. A discovery/validation approach was used. The discovery part was performed in the EpiHealth study and the validation phase was performed in the POEM and PIVUS studies."),
              tags$span(style = "color:black; font-size:13pt", "In order to see the results, please click the ", tags$i("Observational analyses"),  " tab inside the ", tags$i("Tables"), " tab at the top of this page and then enter the name of a protein and a metabolite to see the results. Two degrees of adjustment were used, age and sex-adjustment and also additional adjustment for BMI and kidney function (eGFR)."),
              tags$span(style = "color:black; font-size:13pt", "You can also see heatmap plots for any of these association in the ", tags$i("Observational analyses"),  " tab inside the ", tags$i("Heatmaps"), " tab at the top of this page. Models adjusted by age, sex, BMI and kidney function (eGFR) are displayed."),
              
              tags$span(style = "color:black; font-size:13pt", "The second part used two-sample Mendelian randomization (MR) to evaluate how lipoprotein concentrations are related to metabolites. Cis-instruments for the lipoproteins were derived from UK Biobank data and were related to our own GWAS data for metabolites derived from the EpiHealth and SCAPIS studies. Only the lipoprotein->metabolite relationships were evaluated since it is hard to find non-pleotrophic instruments for the majority of the metabolites."),
              tags$span(style = "color:black; font-size:13pt", "In order to see the MR results, please click the ", tags$i("Mendelian Randomization"), " tab in the ", tags$i("Tables"), " tab at the top of this page and then enter the name of a lipoprotein and a metabolite to see the results."),
              tags$span(style = "color:black; font-size:13pt", "You can also see heatmap plots for any of these associations in the ", tags$i("Mendelian Randomization"), " tab inside the ", tags$i("Tables"), " tab at the top of this page."),
              
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
    
    nav_panel(title = "Tables",
              tabsetPanel(
                
                tabPanel(title = "Observational analyses",
                         
                         p(""),
                         p(""),
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
                           
                           tags$div(  selectizeInput("protein_obs", "Select or type a protein", choices = NULL, multiple = FALSE,
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
                
                
                tabPanel(title = "Mendelian Randomization",
                         
                         p(""),
                         p(""),
                         p(""),
                         tags$span(style = "color:black; font-size:13pt", "Please select a metabolite and a lipoprotein." ) ,
                         tags$span(style = "color:black; font-size:13pt", "If you want to remove a metabolite or a lipoprotein, click on the respective box and press backspace.") ,
                         p(""),
                         p(""),
                         p(""),
                         
                         
                         
                         
                         fluidPage(
                           
                           
                           useShinyFeedback(),
                           
                           tags$div(selectizeInput("metabolite_mr", "Select or type a metabolite", choices = NULL, multiple = FALSE,
                                                   selected = NULL,
                                                   options = list('plugins' = list('remove_button'),
                                                                  placeholder = '' )   ) ,  style="display:inline-block"),
                           tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                           tags$div(selectizeInput("boolean_mr", "AND", choices = c("AND"), multiple = FALSE,
                                                   selected = "AND",
                                                   options = list('plugins' = list('remove_button'),
                                                                  placeholder = '' )) ,  style="display:inline-block; width: 100px;"),
                           tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                           
                           tags$div(selectizeInput("protein_mr", "Select or type a protein", choices = NULL, multiple = FALSE,
                                                   selected = NULL,
                                                   options = list('plugins' = list('remove_button'),
                                                                  placeholder = '' )  ) ,  style="display:inline-block"),
                           tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                           

                           
                           fluidRow(htmlOutput("table_text_mr")),
                           tags$head(tags$style("#table_text_mr{font-size: 17px;
                                       }"
                           )
                           ),
                           p(""),
                           p(""),
                           
                           fluidRow(
                             column(12,reactableOutput("selected_results_mr")
                             )
                             
                           ),
                           fluidRow( column(6, align = "left", uiOutput("download.button.results_mr") ) ),
                           p(""),
                           p(""),
                           p(""),
                           p(""),
                           p(""),
                           p(""),
                           
                           
                           fluidRow(htmlOutput("warning_text_mr")),
                           tags$head(tags$style("#warning_text_mr{font-size: 17px; color: #bb2124;
                                       }"
                           )
                           ),
                           
                           
                           
                           fluidRow(htmlOutput("scatter_text_mr")),
                           tags$head(tags$style("#scatter_text_mr{font-size: 17px;
                                       }"
                           )
                           ),
                           p(""),
                           p(""),
                           
                           fluidRow(
                             #   column(12,reactableOutput("selected_protein_details_mr")
                             #   ),
                             
                             tags$div( plotOutput("scatter_mr", width = "80%", height = "900px") , style = "display:block;" ),
                             tags$div(  uiOutput("download.button.scatter.plot") ,  style="display:inline-block")
                             
                             
                           ),
                           p(""),
                           p(""),
                           
                           fluidRow(
                             column(8,reactableOutput("selected_protein_details_mr")
                             )
                             
                           ),
                           
                           p(""),
                           p(""),
                           p(""),
                           p(""),
                           p(""),
                           p(""),
                           
                           fluidRow(htmlOutput("forest_text_mr")),
                           tags$head(tags$style("#forest_text_mr{font-size: 17px;
                                               }"
                           )
                           ),
                           p(""),
                           p(""),
                           
                           fluidRow(
           
                             tags$div( plotOutput("forest_mr", width = "80%", height = "900px") , style = "display:block;"),
                             tags$div(  uiOutput("download.button.forest.plot") ,  style="display:inline-block")
                             
                           ),
                           fluidRow( column(6, align = "left", uiOutput("download.button.metabolites_mr") ) ),
                           p(""),
                           p(""),
                           p(""),
                           p(""),
                           p(""),
                           p(""),
                           
                           fluidRow(htmlOutput("funnel_text_mr")),
                           tags$head(tags$style("#funnel_text_mr{font-size: 17px;
                                               }"
                           )
                           ),
                           p(""),
                           p(""),
                           
                           fluidRow(
  
                             
                             tags$div( plotOutput("funnel_mr", width = "80%", height = "900px") , style = "display:block;"),
                             tags$div(  uiOutput("download.button.funnel.plot") ,  style="display:inline-block")
                             
                           ),
                           p(""),
                           p(""),
                           p(""),
                           p(""),
                           p(""),
                           p(""),
                           
                           fluidRow(htmlOutput("funnel_position_text_mr")),
                           tags$head(tags$style("#funnel_position_text_mr{font-size: 17px;
                                               }"
                           )
                           ),
                           p(""),
                           p(""),
                           
                           fluidRow(
 
                             
                             
                             tags$div( plotOutput("funnel_position_mr", width = "80%", height = "900px") , style = "display:block;"),
                             tags$div(  uiOutput("download.button.funnel_position.plot") ,  style="display:inline-block")
                             
                           ),

                           
                           
                           
                         ) # end fluidPage
                         
                         
                )
                
                
              ) # End tabPanel
              
    ), # end nav_panel Observational Analyses
    
    
    nav_panel(title = "Heatmaps",
              tabsetPanel(
                
                tabPanel(title = "Observational analyses",
                         
                         p(""),
                         p(""),
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
                           
                           tags$div(  selectizeInput("protein_obs_plot", "Select or type a protein", choices = NULL, multiple = TRUE,
                                                     selected = NULL,
                                                     options = list('plugins' = list('remove_button'),
                                                                    placeholder = '' ) )          ,  style="display:inline-block"),
                           tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),

                           
                           
                           tags$div(  uiOutput("download.button.obs.plot") ,  style="display:inline-block"),
                           tags$div(  uiOutput("download.button.obs.plot.data") ,  style="display:inline-block"),
                           
                           
                           
                           tags$div(  plotOutput("heatmap_obs") , style = "display:block;"   )
                           
                         ) # end fluidPage
                         
                ), # end tabsetPanel
                
                
                tabPanel(title = "Mendelian Randomization",
                         
                         p(""),
                         p(""),
                         p(""),
                         tags$span(style = "color:black; font-size:13pt", "Please select one super- or sub-pathway, and one or several lipoproteins.") ,
                         tags$span(style = "color:black; font-size:13pt", "If you want to remove a super- or sub-pathway, or a lipoprotein, click on the respective box and press backspace.") ,
                         tags$span(style = "color:black; font-size:13pt", "Heatmap shows Mendelian Randomization estimated effects (beta coefficient) from Inverse Variance Weighting.", "Nominal p-value <0.01 = two stars; <0.05 = one star; ≥0.05 = no stars.") ,
                         p(""),
                         p(""),
                         p(""),
                         
                         
                         
                         fluidPage(
                           
                           
                           
                           useShinyFeedback(),
                           
                           tags$div(  selectizeInput("pathway_mr_plot", "Select or type a super- or sub-pathway", choices = NULL, multiple = FALSE,
                                                     selected = NULL,
                                                     options = list('plugins' = list('remove_button'),
                                                                    placeholder = '' ) )      ,  style="display:inline-block"),
                           tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                           tags$div(  selectizeInput("boolean_mr_plot", "AND", choices = c("AND"), multiple = FALSE,
                                                     selected = "AND",
                                                     options = list('plugins' = list('remove_button'),
                                                                    placeholder = '' ))         ,  style="display:inline-block; width: 100px;"),
                           tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                           
                           tags$div(  selectizeInput("protein_mr_plot", "Select or type a protein", choices = NULL, multiple = TRUE,
                                                     selected = NULL,
                                                     options = list('plugins' = list('remove_button'),
                                                                    placeholder = '' ) )          ,  style="display:inline-block"),
                           tags$div( tags$div("") , style="display:inline-block; width: 70px;"  ),
                           

                           
                           
                           tags$div(  uiOutput("download.button.mr.plot") ,  style="display:inline-block"),
                           tags$div(  uiOutput("download.button.mr.plot.data") ,  style="display:inline-block"),
                           
                           

                           
                           
                           tags$div( plotOutput("heatmap_mr") , style = "display:block;" )
                           
                           
                           
                         ) # end fluidPage
                         
                         
                )
                
                
              ) # End tabPanel
              
    ), # end nav_panel
    
    
    nav_panel(title = "Downloads",
              p(""),
              p(""),
              tags$span(style = "color:black; font-size:13pt", "Here you can find all supplementary tables to our research article."),
              tags$span(style = "color:black; font-size:13pt", "For each table, you can either sort and download it completely, or you can search, filter, sort and download your customized table."),
              tags$span(style = "color:black; font-size:13pt", "Also, if you cannot see the tables below, please wait, it could take up to 45 seconds for them to show.") ,
              
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
              )
              
              
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
  
  
  
  # Observational analyses page start----
  
  
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
  updateSelectizeInput(session, 'metabolite_obs', choices = unique(obs_df$Metabolite[order(obs_df$Metabolite)]) , server = TRUE, selected = "" )
  updateSelectizeInput(session, 'protein_obs', choices = unique(c( obs_df$`Lipoprotein` )[order(c( obs_df$`Lipoprotein` )) ]), server = TRUE, selected = "" )
  
  
  
  # Table titles to display----
  line_1 <- eventReactive(isTruthy(input$metabolite_obs) && isTruthy(input$protein_obs),{
    
    req(isTruthy(input$metabolite_obs) && isTruthy(input$protein_obs))
    
    line1 <- paste("<br>", "<br>", "<br>", "<b>Table 1. ", "</b>", "Associations between measured lipoprotein levels and plasma metabolites. Model adjusted for age and sex.")
    
  } )
  
  

  
  
  
  
  ## Rendering Table titles----
  output$result_text_obs  <- renderText( { line_1() } )
  

  
  
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
                  headerStyle = list(background = "#f7f7f8")
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
                                                  cell = function(value) format(value, digits = 3, scientific = FALSE ) )
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
                  headerStyle = list(background = "#f7f7f8")
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
                  headerStyle = list(background = "#f7f7f8")
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
        paste("Results_Obs_", Sys.Date(), ".csv", sep = "")
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
  
  
  
  
  
  # Mendelian Randomization page start----
  
  
  
  observeEvent(input$button_mr,{
    
    if ( input$metabolite_mr == "" && input$protein_mr == "" ) {
      showFeedbackWarning(
        inputId = "metabolite_mr",
        text = "Please select a metabolite from the list"
      )
      showFeedbackWarning(
        inputId = "protein_mr",
        text = "Please select a protein from the list"
      )
      
    } else if( input$metabolite_mr == "" && input$boolean_mr == "AND" ){
      showFeedbackWarning(
        inputId = "metabolite_mr",
        text = "Please select a metabolite from the list"
      )
      hideFeedback("protein_mr")
      
    } else if( input$protein_mr == "" && input$boolean_mr == "AND" ){
      showFeedbackWarning(
        inputId = "protein_mr",
        text = "Please select a protein from the list"
      )
      hideFeedback("metabolite_mr")
      
    } else if ( input$metabolite_mr == "" && input$protein_mr == "" ) {
      showFeedbackWarning(
        inputId = "metabolite_mr",
        text = "Please select a metabolite from the list"
      )
      showFeedbackWarning(
        inputId = "protein_mr",
        text = "Please select a protein from the list"
      )
      
    } else {
      hideFeedback("metabolite_mr")
      hideFeedback("protein_mr")
    }
    
  }
  )
  
  
  
  
  # Selection boxes----
  updateSelectizeInput(session, 'metabolite_mr', choices = unique(harmonised_df$outcome[order(harmonised_df$outcome)]) , server = TRUE, selected = "" )
  updateSelectizeInput(session, 'protein_mr', choices = unique(c(harmonised_df$exposure)[order(harmonised_df$exposure) ]), server = TRUE, selected = "" )
  
  
  
  
  
  
  
  
  
  line_1_mr <- eventReactive((isTruthy(input$metabolite_mr) && isTruthy(input$protein_mr)),{
    
    req(isTruthy(input$metabolite_mr) && isTruthy(input$protein_mr))
    
    line1 <- paste("<br>", "<br>", "<br>", "<b>Table 1. ", "</b>", "Mendelian Randomization estimated effects between genetically predicted lipoprotein levels and plasma metabolites.")
    
  } )
  
  
  line_2_mr <- eventReactive((isTruthy(input$metabolite_mr) && input$protein_mr!= "S-LDL" && input$protein_mr!= ""),{
    
    req(isTruthy(input$metabolite_mr) && input$protein_mr!= "S-LDL" && input$protein_mr!= "")
    
    line2 <- paste("<br>", "<br>", "<br>", "<b>Figure 1. ", "</b>", "Scatter plot of Mendelian Randomization estimated effects between genetically predicted lipoprotein levels and plasma metabolites.")
    
  } )
  
  line_3_mr <- eventReactive((isTruthy(input$metabolite_mr) && input$protein_mr!= "S-LDL" && input$protein_mr!= ""),{
    
    req(isTruthy(input$metabolite_mr) && input$protein_mr!= "S-LDL" && input$protein_mr!= "")
    
    line3 <- paste("<br>", "<br>", "<br>", "<b>Figure 2. ", "</b>", "Forest plot of Mendelian Randomization estimated effects between genetically predicted lipoprotein levels and plasma metabolites.")
    
  } )
  
  line_4_mr <- eventReactive((isTruthy(input$metabolite_mr) && input$protein_mr!= "S-LDL" && input$protein_mr!= ""),{
    
    req(isTruthy(input$metabolite_mr) && input$protein_mr!= "S-LDL" && input$protein_mr!= "")
    
    line4 <- paste("<br>", "<br>", "<br>", "<b>Figure 3. ", "</b>", "Funnel plot of Mendelian Randomization estimated effects between genetically predicted lipoprotein levels and plasma metabolites.")
    
  } )
  
  line_warning_mr <- eventReactive((isTruthy(input$metabolite_mr) && input$protein_mr== "S-LDL" ),{
    
    req(isTruthy(input$metabolite_mr) && input$protein_mr== "S-LDL" )
    
    line <- paste("<br>", "<br>", "<br>", "<b>", "</b>", "Note: there are only 2 available genetic instruments for S-LDL. Egger-MR and weighted median methods cannot be performed, and figures cannot be plotted.")
    
  } )
  
  

  
  
  
  
  
  
  
  
  
  
  ## Rendering Table titles----
  output$table_text_mr  <- renderText( { line_1_mr() } )
  
  output$scatter_text_mr  <- renderText( { line_2_mr() } )
  
  output$forest_text_mr  <- renderText( { line_3_mr() } )
  
  output$funnel_text_mr  <- renderText( { line_4_mr() } )
  
  output$warning_text_mr  <- renderText( { line_warning_mr() } )
  

    
  
  
  # Results tables to display----
  selected_results_df_mr <- eventReactive( (isTruthy(input$metabolite_mr) && isTruthy(input$protein_mr)),{
    
    req( isTruthy(input$metabolite_mr) && isTruthy(input$protein_mr))
    
    
    df5 <- harmonised_df[ which( ( harmonised_df$exposure %in% c( input$protein_mr ) ) &  harmonised_df$outcome %in% c(input$metabolite_mr) ), ]
    
    mr_df <- mr( df5 , method_list = c("mr_ivw", "mr_egger_regression", "mr_weighted_median") )
    
    mr_df <- mr_df %>% left_join(met_anno[c("Metabolite", "Super pathway", "Sub pathway")], join_by("outcome" == "Metabolite"))
    
    mr_df <- mr_df %>% left_join(fdr_df[which(fdr_df$exposure %in% c(input$protein_mr) & fdr_df$outcome %in% c(input$metabolite_mr) ), c("method", "FDR-adjusted p-value")], join_by("method" == "method"))
    
    mr_df <- mr_df[c("exposure", "Super pathway", "Sub pathway", "outcome", "method", "nsnp", "b", "se", "pval", "FDR-adjusted p-value")]
    
    colnames(mr_df) <- c("Lipoprotein", "Super pathway", "Sub pathway", "Metabolite", "Method", "Number of SNPs", "Beta", "SE", "Nominal p-value", "FDR-adjusted p-value")
    

    return(mr_df)
    
  })
  
  
  
  selected_protein_details_df_mr <- eventReactive((isTruthy(input$metabolite_mr) && input$protein_mr!= "S-LDL" && input$protein_mr!= ""),{
    
    req( isTruthy(input$metabolite_mr) && input$protein_mr!= "S-LDL" && input$protein_mr!= "" )
    
    df6 <- harmonised_df[ which( ( harmonised_df$exposure %in% c( input$protein_mr ) ) &  harmonised_df$outcome %in% c(input$metabolite_mr) ), ]
    
    pleiotropy_df <- mr_pleiotropy_test(df6)
    
    pleiotropy_df <- pleiotropy_df[c("egger_intercept", "se", "pval")]
    
    colnames(pleiotropy_df) <- c("Egger intercept", "SE", "p-value")
    
    return(pleiotropy_df)
    
  })
  
  
  
  
  
  
  ## Rendering Results tables----
  
  
  observeEvent((isTruthy(input$metabolite_mr) && input$protein_mr!= "S-LDL" && input$protein_mr!= ""), { # for forest plot
    
    
    req( isTruthy(input$pathway_mr) && isTruthy(input$protein_mr)  )
    
    
    final_height_forest <- NULL
    makeReactiveBinding("final_height")
    
    
    
  }
  
  )
  
  
  
  
  observeEvent( (isTruthy(input$metabolite_mr) && isTruthy(input$protein_mr)), {
    
    req( isTruthy(input$metabolite_mr) && isTruthy(input$protein_mr) )
    
    
    output$selected_results_mr <- renderReactable({
      reactable(selected_results_df_mr(),
                sortable = FALSE,
                filterable = FALSE,
                searchable = FALSE,
                bordered = TRUE,
                highlight = TRUE,
                defaultColDef = colDef(
                  align = "center",
                  headerStyle = list(background = "#f7f7f8")
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
                                                  cell = function(value) format(value, digits = 3, scientific = TRUE ) )
                ),
                defaultPageSize = 5,
                showPageSizeOptions = FALSE,
                showPagination = TRUE ) } )
    
  } )
  
  
  
  observeEvent( (isTruthy(input$metabolite_mr) && input$protein_mr!= "S-LDL" && input$protein_mr!= "" ), {
    
    req( isTruthy(input$metabolite_mr) && input$protein_mr!= "S-LDL" && input$protein_mr!= "" )
    
    
    output$selected_protein_details_mr <- renderReactable({
      reactable(selected_protein_details_df_mr(),
                sortable = FALSE,
                filterable = FALSE,
                searchable = FALSE,
                bordered = TRUE,
                highlight = TRUE,
                defaultColDef = colDef(
                  align = "center",
                  headerStyle = list(background = "#f7f7f8")
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
  
  
  
  
  
  observeEvent( (isTruthy(input$metabolite_mr) && input$protein_mr!= "S-LDL" && input$protein_mr!= "" ), {
    
    req( isTruthy(input$metabolite_mr) && input$protein_mr!= "S-LDL" && input$protein_mr!= "" )
    
    
    output$scatter_mr <- shiny::renderPlot({
      
      req( isTruthy(input$metabolite_mr) && input$protein_mr!= "S-LDL" && input$protein_mr!= "" )
      
      
      df7 <- harmonised_df[ which( ( harmonised_df$exposure %in% c( input$protein_mr ) ) &  harmonised_df$outcome %in% c(input$metabolite_mr) ), ]
      
      mr_scatter_plot_custom( mr( df7 , method_list = c("mr_egger_regression", "mr_ivw", "mr_weighted_median") ) , 
                              df7 )
      
      
    }
    

    )
    
  }
  
  )
  
  
  
  
  
  observeEvent( (isTruthy(input$metabolite_mr) && input$protein_mr!= "S-LDL" && input$protein_mr!= "" ) , {
    
    req( isTruthy(input$metabolite_mr) && input$protein_mr!= "S-LDL" && input$protein_mr!= "" )
    
    
    output$forest_mr <- shiny::renderPlot({
      
      req( isTruthy(input$metabolite_mr) && input$protein_mr!= "S-LDL" && input$protein_mr!= "" )
      
      
      df8 <- harmonised_df[ which( ( harmonised_df$exposure %in% c( input$protein_mr ) ) &  harmonised_df$outcome %in% c(input$metabolite_mr) ), ]
      
      mr_forest_plot_custom(mr_singlesnp( df8 , all_method = c("mr_weighted_median", "mr_egger_regression", "mr_ivw") ) )
      
      
    }

    
    )
    
  }
  
  )
  
  
  
  observeEvent( (isTruthy(input$metabolite_mr) && input$protein_mr!= "S-LDL" && input$protein_mr!= "" ) , {
    
    req( isTruthy(input$metabolite_mr) && input$protein_mr!= "S-LDL" && input$protein_mr!= "" )
    
    
    output$funnel_mr <- shiny::renderPlot({
      
      req( isTruthy(input$metabolite_mr) && input$protein_mr!= "S-LDL" && input$protein_mr!= "" )
      
      
      df9 <- harmonised_df[ which( ( harmonised_df$exposure %in% c( input$protein_mr ) ) &  harmonised_df$outcome %in% c(input$metabolite_mr) ), ]
      
      mr_funnel_plot_custom(mr_singlesnp( df9 , all_method = c("mr_weighted_median", "mr_ivw", "mr_egger_regression") ) )
      
      
      
    }

    
    )
    
  }
  
  )
  
  
  
 
  
  
  # Download buttons-----
  observeEvent( isTruthy(input$metabolite_mr) && isTruthy(input$protein_mr) ,{
    
    req( isTruthy(input$metabolite_mr) && isTruthy(input$protein_mr) )
    
    output$download.button.results_mr <- renderUI({
      
      req( selected_results_df_mr() )
      
      
      shiny::downloadButton(
        "download_results_mr", "Download",
        onClick = "Shiny.setInputValue('table_state:to_csv', Reactable.getState('selected_results_mr').sortedData)")
    })
    
  } )
  
  
  
  
  
  ## Rendering Download buttons----
  observeEvent( isTruthy(input$metabolite_mr) && isTruthy(input$protein_mr) , {
    
    req( isTruthy(input$metabolite_mr) && isTruthy(input$protein_mr) )
    
    
    output$download_results_mr <- downloadHandler(
      filename = function() {
        paste("Results_MR_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        data <- input$table_state
        write.csv(data, file, row.names = FALSE)
      }
    )
    
    
  })
  
  
  
  observeEvent(input$button_mr, {
    
    req( isTruthy(input$metabolite_mr) || isTruthy(input$protein_mr) )
    
    
    output$download_proteins_mr <- downloadHandler(
      filename = function() {
        paste("Proteins_MR_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        data <- input$table_state
        write.csv(data, file, row.names = FALSE)
      }
    )
    
    
  })
  
  
  
  observeEvent(input$button_mr, {
    
    req( isTruthy(input$metabolite_mr) || isTruthy(input$protein_mr) )
    
    
    output$download_metabolites_mr <- downloadHandler(
      filename = function() {
        paste("Metabolites_MR_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        data <- input$table_state
        write.csv(data, file, row.names = FALSE)
      }
    )
    
    
  })
  
  
  # Mendelian Randomization page end
  
  
  
  
  
  
  
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
  
  
  
  updateSelectizeInput(session, 'pathway_obs_plot', choices = c( unique(obs_df$`Super pathway`[order(obs_df$`Super pathway`)]), unique(obs_df$`Sub pathway`[order(obs_df$`Sub pathway`)]) ), server = TRUE, selected = "" )
  updateSelectizeInput(session, 'protein_obs_plot', choices = unique(c(obs_df$`Lipoprotein`, obs_df$`Lipoprotein`)[order(c(obs_df$`Lipoprotein`)) ]), server = TRUE, selected = "" )
  
  
  
  
  # observeEvent( input$button_obs_plot, {# To take plot and its dimensions outside of the reactive, to save it
  observeEvent( isTruthy(input$pathway_obs_plot) && isTruthy(input$protein_obs_plot), {# To take plot and its dimensions outside of the reactive, to save it
    
    req( isTruthy(input$pathway_obs_plot) && isTruthy(input$protein_obs_plot)  )
    
    HT_obs_plot <- NULL
    makeReactiveBinding("HT_obs_plot")
    
    final_width <- NULL
    makeReactiveBinding("final_width")
    
    final_height <- NULL
    makeReactiveBinding("final_height")
    
    df_to_download <- NULL
    makeReactiveBinding("df_to_download")
    
    rows_order <- NULL
    makeReactiveBinding("rows_order")
    
  }
  
  )
  
  
  
  
  
  
  # ht_obs <- observeEvent(input$button_obs_plot,{
  ht_obs <- observeEvent( isTruthy(input$pathway_obs_plot) && isTruthy(input$protein_obs_plot) ,{
    
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
  
  
  
  
  
  
  # observeEvent(input$button_obs_plot,{
  observeEvent( isTruthy(input$pathway_obs_plot) && isTruthy(input$protein_obs_plot) ,{
    
    req( isTruthy(input$pathway_obs_plot) && isTruthy(input$protein_obs_plot)  && input$boolean_obs_plot == "AND"  )
    
    
    output$download.button.obs.plot <- renderUI({
      
      
      shiny::downloadButton(
        "download_obs_plot", "Download Plot" )
      
      
    } )
    
    
    
  } )
  
  
  
  
  
  
  output$download_obs_plot <- downloadHandler(
    filename = function() {
      
      if( !is.null( input$pathway_obs_plot ) & is.null( input$pathway_obs_plot ) ){
        
        paste0("Heatmap_" , input$pathway_obs_plot, "_obs.svg")
        
      }else if( is.null( input$pathway_obs_plot ) & !is.null( input$pathway_obs_plot ) ){
        
        paste0("Heatmap_" , input$protein_obs_plot, "_obs.svg")
        
      } else if( !is.null( input$pathway_obs_plot ) & !is.null( input$pathway_obs_plot ) ){
        
        paste0("Heatmap_" , input$pathway_obs_plot, "_",input$protein_obs_plot, "_obs.svg")
        
        
      }
      
    },
    
    content = function(file) {
      
      svg(file, width = final_width, height = final_height  )
      
      draw(HT_obs_plot)
      
      dev.off()
      
    }
    
  )
  
  
  
  
  
  
  observeEvent( isTruthy(input$pathway_obs_plot) && isTruthy(input$protein_obs_plot) ,{
    
    req( isTruthy(input$pathway_obs_plot) && isTruthy(input$protein_obs_plot)  && input$boolean_obs_plot == "AND"  )
    
    
    output$download.button.obs.plot.data <- renderUI({
      
      
      shiny::downloadButton(
        "download_obs_plot_data", "Download Data" )
      
      
    } )
    
    
    
  } )
  
  
  
  
  
  
  output$download_obs_plot_data <- downloadHandler(
    filename = function() {
      
      if( !is.null( input$pathway_obs_plot ) & is.null( input$pathway_obs_plot ) ){
        
        paste0("Data_" , input$pathway_obs_plot, "_obs.csv")
        
      }else if( is.null( input$pathway_obs_plot ) & !is.null( input$pathway_obs_plot ) ){
        
        paste0("Data_" , input$protein_obs_plot, "_obs.csv")
        
      } else if( !is.null( input$pathway_obs_plot ) & !is.null( input$pathway_obs_plot ) ){
        
        paste0("Data_" , input$pathway_obs_plot, "_",input$protein_obs_plot, "_obs.csv")
        
        
      }
      
    },
    
    content = function(file) {
      
      write.csv(df_to_download, file, row.names = FALSE)
      
    }
    
  )
  
  
  
  
  
  
  
  
  # MR heatmaps
  
  
  # Selection boxes MR plot----
  
  
  
  observeEvent(input$button_mr_plot,{
    
    if ( input$pathway_mr_plot == "" && (is.null(input$protein_mr_plot ) ) ) {
      showFeedbackWarning(
        inputId = "pathway_mr_plot",
        text = "Please select a pathway from the list"
      )
      showFeedbackWarning(
        inputId = "protein_mr_plot",
        text = "Please select a protein from the list"
      )
      
    } else if( input$pathway_mr_plot == "" && input$boolean_mr_plot == "AND" ){
      showFeedbackWarning(
        inputId = "pathway_mr_plot",
        text = "Please select a pathway from the list"
      )
      hideFeedback("protein_mr_plot")
      
    } else if( (is.null(input$protein_mr_plot ) ) && input$boolean_mr_plot == "AND" ){
      showFeedbackWarning(
        inputId = "protein_mr_plot",
        text = "Please select a protein from the list"
      )
      hideFeedback("pathway_mr_plot")
      
    } else {
      hideFeedback("pathway_mr_plot")
      hideFeedback("protein_mr_plot")
    }
    
  }
  )
  
  
  
  
  updateSelectizeInput(session, 'pathway_mr_plot', choices = c( unique(htmap$`Super pathway`[order(htmap$`Super pathway`)]), unique(htmap$`Sub pathway`[order(htmap$`Sub pathway`)]) ), server = TRUE, selected = "" )
  updateSelectizeInput(session, 'protein_mr_plot', choices = unique(c(htmap$`Lipoprotein`)[order(c(htmap$Lipoprotein)) ]), server = TRUE, selected = "" )
  
  
  
  
  observeEvent( isTruthy(input$pathway_mr_plot) && isTruthy(input$protein_mr_plot) , {
    
    
    req( isTruthy(input$pathway_mr_plot) && isTruthy(input$protein_mr_plot)  )
    
    
    
    # To take plot and its dimenstions outside of the reactive, to save it
    HT_mr_plot <- NULL
    makeReactiveBinding("HT_mr_plot")
    
    final_width_mr <- NULL
    makeReactiveBinding("final_width")
    
    final_height_mr <- NULL
    makeReactiveBinding("final_height")
    
    df_to_download <- NULL
    makeReactiveBinding("df_to_download")
    
    rows_order_mr <- NULL
    makeReactiveBinding("rows_order")
    
  }
  
  )
  
  
  
  
  
  
  # ht_mr <- observeEvent( input$button_mr_plot ,{
  ht_mr <- observeEvent( isTruthy(input$pathway_mr_plot) && isTruthy(input$protein_mr_plot) ,{
    
    req( isTruthy( input$pathway_mr_plot != "" & !is.null(input$protein_mr_plot) & input$boolean_mr_plot == "AND" ) ||
           isTruthy( (input$pathway_mr_plot != "" & is.null(input$protein_mr_plot)) & input$boolean_mr_plot == "ONLY" )  ||
           isTruthy( (input$pathway_mr_plot == "" & !is.null(input$protein_mr_plot)) & input$boolean_mr_plot == "ONLY" ) )
    
    
    
    
    
    {
      
      
      if(input$boolean_mr_plot == "AND"){
        
        
        req( isTruthy(input$pathway_mr_plot) && isTruthy(input$protein_mr_plot)  )
        
        
        df_filtered <- htmap[ which( ( ( htmap$`Super pathway` %in% c( input$pathway_mr_plot ) | htmap$`Sub pathway` %in% c( input$pathway_mr_plot ) ) & htmap$Lipoprotein %in% c( input$protein_mr_plot ) ) ), c("Lipoprotein", "Metabolite", "Beta", "Nominal p-value") ]
        

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
    real_order <- c("XL-VLDL", "L-VLDL", "M-VLDL", "S-VLDL", "XS-VLDL", "IDL", "L-LDL", "M-LDL", "S-LDL", "XL-HDL", "L-HDL", "M-HDL", "S-HDL")
    names(real_order) <- c("XL-VLDL", "L-VLDL", "M-VLDL", "S-VLDL", "XS-VLDL", "IDL", "L-LDL", "M-LDL", "S-LDL", "XL-HDL", "L-HDL", "M-HDL", "S-HDL")
    rows_order_mr <<- real_order[real_order %in% rownames(betas)]
    
    
    
    
    
    
    if( nrow( df_filtered ) > 400 ){
      
      ht <- Heatmap(  betas, name = "IVW - Beta", col = col_fun,
                      cluster_columns = FALSE,
                      show_row_dend = FALSE,
                      show_column_dend= FALSE,
                      row_order = rows_order_mr,
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
                      row_order = rows_order_mr,
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
    
    
    
    HT_mr_plot <<- ht
    
    
    final_width_mr <<- (( (convertX( grobWidth(textGrob(rownames(betas), gpar(fontsize = 11, fontface = 1))) , "inch", valueOnly = TRUE)/4 ) + convertX(  ComplexHeatmap:::width(ht) , "inch", valueOnly = TRUE )  ) )
    
    final_height_mr <<- ( convertX(  ComplexHeatmap:::height(ht) , "inch", valueOnly = TRUE ) )
    
    print(final_width_mr)
    print(final_height_mr)
    
    output$heatmap_mr <- shiny::renderPlot({
      
      
      draw(ht)
      
    },
    
    width = ( final_width_mr * 72 ) ,
    
    height = ( final_height_mr * 72 )
    
    )
    
    
    
    
  }
  
  )
  
  
  
  
  # observeEvent(input$button_mr_plot,{
  observeEvent( isTruthy(input$pathway_mr_plot) && isTruthy(input$protein_mr_plot) ,{
    
    req( isTruthy(input$pathway_mr_plot) && isTruthy(input$protein_mr_plot)  && input$boolean_mr_plot == "AND"  )
    
    
    output$download.button.mr.plot <- renderUI({
      
      
      shiny::downloadButton(
        "download_mr_plot", "Download Plot" )
    } )
    
    
    
  } )
  
  
  
  
  output$download_mr_plot <- downloadHandler(
    
    
    filename = function() {
      
      if( !is.null( input$pathway_mr_plot ) & is.null( input$pathway_mr_plot ) ){
        
        paste0("Heatmap_" , input$pathway_mr_plot, "_mr.svg")
        
      }else if( is.null( input$pathway_mr_plot ) & !is.null( input$pathway_mr_plot ) ){
        
        paste0("Heatmap_" , input$protein_mr_plot, "_mr.svg")
        
      } else if( !is.null( input$pathway_mr_plot ) & !is.null( input$pathway_mr_plot ) ){
        
        paste0("Heatmap_" , input$pathway_mr_plot, "_",input$protein_mr_plot, "_mr.svg")
        
        
      }
      
    },
    
    content = function(file) {
      
      svg(file, width = final_width_mr, height = final_height_mr  )
      
      draw(HT_mr_plot)
      
      dev.off()
      
    }
    
  )
  
  
  
  
  
  
  # observeEvent( input$button_mr_plot ,{
  observeEvent( isTruthy(input$pathway_mr_plot) && isTruthy(input$protein_mr_plot) ,{
    
    req( isTruthy(input$pathway_mr_plot) && isTruthy(input$protein_mr_plot)  && input$boolean_mr_plot == "AND"  )
    
    
    output$download.button.mr.plot.data <- renderUI({
      
      
      shiny::downloadButton(
        "download_mr_plot_data", "Download Data" )
    } )
    
    
    
  } )
  
  
  
  
  
  
  output$download_mr_plot_data <- downloadHandler(
    filename = function() {
      
      if( !is.null( input$pathway_mr_plot ) & is.null( input$pathway_mr_plot ) ){
        
        paste0("Data_" , input$pathway_mr_plot, "_mr.csv")
        
      }else if( is.null( input$pathway_mr_plot ) & !is.null( input$pathway_mr_plot ) ){
        
        paste0("Data_" , input$protein_mr_plot, "_mr.csv")
        
      } else if( !is.null( input$pathway_mr_plot ) & !is.null( input$pathway_mr_plot ) ){
        
        paste0("Data_" , input$pathway_mr_plot, "_",input$protein_mr_plot, "_mr.csv")
        
        
      }
      
    },
    
    content = function(file) {
      
      write.csv(df_to_download, file, row.names = FALSE)
      
    }
    
  )
  
  
  
  
  
  
  
  
  
  output$upset_mr <- shiny::renderPlot({
    
    
    plot
    
    
  }  )
  
  

  
  
  # End Heatmap
  
  
  
  
  
  
  
  
  
  
  
  
  
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
                headerStyle = list(background = "#f7f7f8")
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
                headerStyle = list(background = "#f7f7f8")
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
                `p-value` = colDef(filterable = FALSE,
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
                headerStyle = list(background = "#f7f7f8")
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
                headerStyle = list(background = "#f7f7f8")
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
      paste("Annex1_", Sys.Date(), ".csv", sep = "")
    },
    content=function(file) {
      file.copy("www/Proteomics_and_metabolomics_analyses_in_POEM.csv", file)
    }
  )
  
  
  
} # end server


                                        
shinyApp(ui, server)
