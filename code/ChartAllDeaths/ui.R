library(shiny)
library(shinythemes)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(plotly)
library(shinycustomloader)

ui <- function(req){
  navbarPage(
  theme = shinytheme("yeti"),
  "Deaths induced by:", id="Plot",
  navbarMenu(

    # Opioids tab -------------------------------------------------------------
    "Opioids",

    tabPanel(value="PlotOA", 
      # Opioids by intent, opioid and age -----------------------------------------
      "By opioid, intent and age",
      h1("Opioid induced deaths"),
      h3("By opioid, intent and age"),
      
      tabsetPanel(
        type = "tabs",
        tabPanel("Plot",
          mainPanel(
            withLoader(plotlyOutput("opioidPlotA"), type = "html", loader = "loader4"),
            fluidRow(includeMarkdown("notesOpioids.md"))
          ),

          sidebarPanel(
            sliderInput("yearsOA", "Period",
              min = 1997,
              max = 2016, value = c(2007, 2016), sep = ""
            ),
            selectInput(
              "plotOA", "Plot:",
              c(
                "Number of deaths" = "deaths",
                "Deaths per 100,000 people" = "deathrateht",
                "Deaths per 100,000 people (95% CI)" = "deathratehtci",
                "Deaths per 1,000,000 people" = "deathratem",
                "Deaths per 1,000,000 people (95% CI)" = "deathratemci"
              ),
              selected="deathrateht"
            ),

            selectInput("drugOA", "Opioid:",
              choices = c(
                "All opioids",
                "Heroin",
                "Methadone",
                "Natural and semi-synthetic opioids",
                "Synthetic opioids",
                "Other and unspecified opioids"
              ),
              selected = c("All opioids")
            ),

            checkboxGroupInput(
              "codOA", "Intent:",
              c("All", "Accidental", "Intentional", "Undetermined"),
              selected = "All"
            ),

            checkboxGroupInput("ageOA", "Age group:",
              choices = c(
                "15 to 24" = "15-24",
                "25 to 34" = "25-34",
                "35 to 44" = "35-44",
                "45 to 54" = "45-54",
                "55 to 64" = "55-64",
                "65 to 74" = "65-74",
                "75 to 84" = "75-84",
                "15 to 54" = "15-54",
                "15 to 64" = "15-64",
                "All ages" = "All ages"
              ),
              selected = c("All ages")
            )
          )
        ),
        tabPanel("Notes", includeMarkdown("notesOpioidsByIntentTypeAge.md"))
      )
    ),
    tabPanel(value="PlotOB", 
      # Opioids by intent, opioid and sex -----------------------------------------
      "By intent, opioid and sex",
      h1("Opioid induced deaths"),
      h3("By intent, opioid and sex"),

      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Plot",


          mainPanel(
            withLoader(plotlyOutput("opioidPlotB"), type = "html", loader = "loader4"),
            fluidRow(includeMarkdown("notesOpioids.md"))
          ),


          sidebarPanel(
            sliderInput("yearsOB", "Period",
              min = 1997,
              max = 2016, value = c(2007, 2016), sep = ""
            ),
            selectInput(
              "plotOB", "Plot:",
              c(
                "Number of deaths" = "deaths",
                "Deaths per 100,000 people" = "deathrateht",
                "Deaths per 100,000 people (95% CI)" = "deathratehtci",
                "Deaths per 1,000,000 people" = "deathratem",
                "Deaths per 1,000,000 people (95% CI)" = "deathratemci"
              ),
              selected="deathrateht"
            ),

            selectInput(
              "ageOB", "Age range:",
              c("All ages", "15 to 64" = "15-64")
            ),

            selectInput(
              "codOB", "Intent:",
              c("All", "Accidental", "Intentional", "Undetermined")
            ),

            checkboxGroupInput("drugOB", "Opioid:",
              choices = c(
                "All opioids",
                "Heroin",
                "Methadone",
                "Natural and semi-synthetic opioids",
                "Synthetic opioids",
                "Other and unspecified opioids"
              ),
              selected = c("All opioids")
            ),

            checkboxGroupInput("sexOB", "Sex:",
              choices = c("Male", "Female", "All"),
              selected = c("Male", "Female", "All")
            )
          )
        ),
        tabPanel("Notes", includeMarkdown("notesOpioidsByIntentTypeSex.md"))
      )
    ),

    tabPanel(value="PlotOC", 
      # Opioids by sex, intent and opioid -----------------------------------------
      "By sex, intent and opioid",
      h1("Opioid induced deaths"),
      h3("By sex, intent and opioid"),

      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Plot",

          mainPanel(
            withLoader(plotlyOutput("opioidPlotC"), type = "html", loader = "loader4"),
            fluidRow(includeMarkdown("notesOpioids.md"))
          ),

          sidebarPanel(
            sliderInput("yearsOC", "Period",
              min = 1997,
              max = 2016, value = c(2007, 2016), sep = ""
            ),
            selectInput(
              "plotOC", "Plot:",
              c(
                "Number of deaths" = "deaths",
                "Deaths per 100,000 people" = "deathrateht",
                "Deaths per 100,000 people (95% CI)" = "deathratehtci",
                "Deaths per 1,000,000 people" = "deathratem",
                "Deaths per 1,000,000 people (95% CI)" = "deathratemci"
              ),
              selected="deathrateht"
            ),

            selectInput(
              "ageOC", "Age range:",
              c("All ages", "15 to 64" = "15-64")
            ),

            selectInput("sexOC", "Sex:",
              choices = c("Male", "Female", "All"),
              selected = c("All")
            ),


            checkboxGroupInput(
              "codOC", "Intent:",
              c("All", "Accidental", "Intentional", "Undetermined"),
              selected = c("All")
            ),

            checkboxGroupInput("drugOC", "Opioid:",
              choices = c(
                "All opioids",
                "Heroin",
                "Methadone",
                "Natural and semi-synthetic opioids",
                "Synthetic opioids",
                "Other and unspecified opioids"
              ),
              selected = c("All opioids")
            )
          )
        ),
        tabPanel("Notes", includeMarkdown("notesOpioidsBySexIntentType.md"))
      )
    ),


    tabPanel(value="PlotOD", 

      # Opioids by intent, jurisdiction and sex ---------------------------------
      "By intent, jurisdiction and sex",

      h1("Opioid induced deaths"),
      h3("By intent, jurisdiction and sex"),

      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Plot",
          mainPanel(
            withLoader(plotlyOutput("opioidPlotD"), type = "html", loader = "loader4"),
            fluidRow(includeMarkdown("notesOpioids.md"))
          ),

          sidebarPanel(
            sliderInput("yearsOD", "Period",
              min = 1997,
              max = 2016, value = c(2007, 2016), sep = ""
            ),
            selectInput(
              "plotOD", "Plot:",
              c(
                "Number of deaths" = "deaths",
                "Deaths per 100,000 people" = "deathrateht",
                "Deaths per 100,000 people (95% CI)" = "deathratehtci",
                "Deaths per 1,000,000 people" = "deathratem",
                "Deaths per 1,000,000 people (95% CI)" = "deathratemci"
              ),
              selected="deathrateht"
            ),

            selectInput(
              "ageOD", "Age range:",
              c("All ages", "15 to 64" = "15-64")
            ),
            
            selectInput(
              "codOD", "Intent:",
              c("All", "Accidental")
            ),

            checkboxGroupInput(
              "stateOD", "Jurisdiction:",
              c(
                "Australia" = "AUS",
                "New South Wales" = "NSW",
                "Victoria" = "VIC",
                "Queensland" = "QLD",
                "South Australia" = "SA",
                "Western Australia" = "WA",
                "Tasmania" = "TAS",
                "Northern Territory" = "NT",
                "Australian Capital Territory" = "ACT"
              ),
              selected = "AUS"
            ),

            checkboxGroupInput("sexOD", "Sex:",
              choices = c("Male", "Female", "All"),
              selected = c("Male", "Female", "All")
            )
          )
        ),
        tabPanel("Notes", includeMarkdown("notesOpioidsByIntentJurisdictionSex.md"))
      )
    ),
    tabPanel(value="PlotOE", 
             # Opioids with other drugs by age -----------------------------------------
             "Opioids with other drugs, by age",
             h1("Opioid induced deaths"),
             h3("Opioids with other drugs"),

             tabsetPanel(
               type = "tabs",
               tabPanel(
                 "Plot",
                 mainPanel(
                   withLoader(plotlyOutput("PlotOE"), type = "html", loader = "loader4")
                 ),
                 sidebarPanel(
                   sliderInput("yearsOE", "Period",
                               min = 1997,
                               max = 2016, value = c(2007, 2016), sep = ""
                   ),
                   selectInput(
                     "plotOE", "Plot:",
                     c(
                       "Number of deaths" = "deaths",
                       "Deaths per 100,000 people" = "deathrateht",
                       "Deaths per 100,000 people (95% CI)" = "deathratehtci",
                       "Deaths per 1,000,000 people" = "deathratem",
                       "Deaths per 1,000,000 people (95% CI)" = "deathratemci"
                     ),
                     selected="deathrateht"
                   ),
                   
                   selectInput(
                     "intentOE", "Intent:",
                     c("All", "Accidental", "Intentional", "Undetermined"),
                     selected = "All"
                   ),
                   
                   
                   selectInput("sexOE", "Sex:",
                               choices = c(
                                 "Male",
                                 "Female",
                                 "All"
                               ),
                               selected = c("All")
                   ),
                   
                   
                   checkboxGroupInput("ageOE", "Age group:",
                                      choices = c(
                                        "15 to 24" = "15-24",
                                        "25 to 34" = "25-34",
                                        "35 to 44" = "35-44",
                                        "45 to 54" = "45-54",
                                        "55 to 64" = "55-64",
                                        "65 to 74" = "65-74",
                                        "75 to 84" = "75-84",
                                        "15 to 64" = "15-64",
                                        "All ages" = "All ages"
                                      ),
                                      selected = c("All ages")
                   ),
                   
                   checkboxGroupInput("drugOE", "All opioids with:",
                                      choices = c(
                                        "Alcohol" = "All opioids with alcohol",
                                        "Antidepressants" = "All opioids with antidepressants",
                                        "Antipsychotics" = "All opioids with antipsychotics",
                                        "Benzodiazepines" = "All opioids with benzodiazepines",
                                        "Paracetamol" = "All opioids with paracetamol"
                                      ),
                                      selected = c("All opioids with alcohol")
                   )
                 )
               ),
               tabPanel("Notes", "Notes go here")
             )
  ),

  tabPanel(value="PlotOF", 
           # Opioids with other drugs by sex -----------------------------------------
           "Opioids with other drugs, by sex",
           h1("Opioid induced deaths"),
           h3("Opioids with other drugs"),

           # Opioids and other drugs by sex ------------------------------------------
           tabPanel(
             "Opioids and other drugs by sex",
             tabsetPanel(
               type = "tabs",
               tabPanel(
                 "Plot",
                 mainPanel(
                   withLoader(plotlyOutput("PlotOF"), type = "html", loader = "loader4")
                 ),
                 sidebarPanel(
                   sliderInput("yearsOF", "Period",
                               min = 1997,
                               max = 2016, value = c(2007, 2016), sep = ""
                   ),
                   selectInput(
                     "plotOF", "Plot:",
                     c(
                       "Number of deaths" = "deaths",
                       "Deaths per 100,000 people" = "deathrateht",
                       "Deaths per 100,000 people (95% CI)" = "deathratehtci",
                       "Deaths per 1,000,000 people" = "deathratem",
                       "Deaths per 1,000,000 people (95% CI)" = "deathratemci"
                     ),
                     selected="deathrateht"
                   ),
                   
                   selectInput(
                     "intentOF", "Intent:",
                     c("All", "Accidental", "Intentional", "Undetermined"),
                     selected = "All"
                   ),
                   
                   selectInput(
                     "ageOF", "Age:",
                     c("15-64",
                       "All ages" = "All ages"
                     ),
                     selected = "All ages"
                   ),
                   
                   checkboxGroupInput("sexOF", "Sex:",
                                      choices = c(
                                        "Male",
                                        "Female",
                                        "All"
                                      ),
                                      selected = c("All")
                   ),
                   checkboxGroupInput("drugOF", "All opioids with:",
                                      choices = c(
                                        "Alcohol" = "All opioids with alcohol",
                                        "Antidepressants" = "All opioids with antidepressants",
                                        "Antipsychotics" = "All opioids with antipsychotics",
                                        "Benzodiazepines" = "All opioids with benzodiazepines",
                                        "Paracetamol" = "All opioids with paracetamol"
                                      ),
                                      selected = c("All opioids with alcohol")
                   )
                 )
               ),
               tabPanel("Notes", "Notes go here")
             )
           )
  ),
  
  tabPanel(value="PlotOG", 
           # Exclusive opioids -----------------------------------------
           "Exclusive opioids",
           h1("Opioid induced deaths"),
           h3("Exclusive opioids"),
           
           tabsetPanel(
             type = "tabs",
             tabPanel("Plot",
                      mainPanel(
                        withLoader(plotlyOutput("PlotOG"), type = "html", loader = "loader4")
                      ),
                      
                      sidebarPanel(
                        sliderInput("yearsOG", "Period",
                                    min = 2007,
                                    max = 2016, value = c(2007, 2016), sep = ""
                        ),
                        selectInput(
                          "plotOG", "Plot:",
                          c(
                            "Number of deaths" = "deaths",
                            "Deaths per 100,000 people" = "deathrateht",
                            "Deaths per 100,000 people (95% CI)" = "deathratehtci",
                            "Deaths per 1,000,000 people" = "deathratem",
                            "Deaths per 1,000,000 people (95% CI)" = "deathratemci"
                          ),
                          selected="deathrateht"
                        ),
                        
                        selectInput(
                          "intentOG", "Intent:",
                          c("All", "Accidental", "Intentional", "Undetermined"),
                          selected = "All"
                        ),
                        
                        selectInput(
                          "ageOG", "Age:",
                          c("15-64",
                            "All ages" = "All ages"
                          ),
                          selected = "All ages"
                        ),
                        
                        checkboxGroupInput("sexOG", "Sex:",
                                           choices = c(
                                             "Male",
                                             "Female",
                                             "All"
                                           ),
                                           selected = c("All")
                        ),
                        
                        checkboxGroupInput("drugOG", "Drug:",
                                           choices = c(
                                             "Exclusive illicit opioids",
                                             "Exclusive pharmaceutical opioids",
                                             "Heroin/Opium with pharmaceutical opioids",
                                             "Unspecified opioids"
                                           ),
                                           selected = c("Exclusive illicit opioids",
                                                        "Exclusive pharmaceutical opioids"))
                      )
                      
                      ),
             tabPanel("Notes")
           )
  )),

  
    # Amphetamines tab --------------------------------------------------------
  tabPanel(value="PlotA", 
    "Amphetamines",
    h1("Drug induced deaths involving amphetamines"),

    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Plot",
        mainPanel(
      withLoader(plotlyOutput("amphetaminePlot"), type = "html", loader = "loader4"),
      fluidRow(includeMarkdown("notesNoSexSummary.md"))
    ),

    sidebarPanel(
      sliderInput("yearsA", "Period",
        min = 1997,
        max = 2016, value = c(2007, 2016), sep = ""
      ),
      selectInput(
        "plotA", "Plot:",
        c(
          "Number of deaths" = "deaths",
          "Deaths per 100,000 people" = "deathrateht",
          "Deaths per 100,000 people (95% CI)" = "deathratehtci",
          "Deaths per 1,000,000 people" = "deathratem",
          "Deaths per 1,000,000 people (95% CI)" = "deathratemci"
        ),
        selected="deathrateht"
      ),

      radioButtons(
        "codA", "Intent:",
        c("All", "Accidental"),
        selected="All"
      ),

      conditionalPanel(
        condition = "input.codA == 'Accidental'",
        radioButtons(
          "natureA", "Nature of death:",
          c("Any mention", "Underlying"),
          selected="Any mention"
        )
      ),

      conditionalPanel(
        condition = "input.codA == 'All'",
        radioButtons(
          "natureA", "Nature of death:",
          c("Any mention"),
          selected="Any mention"
        )
      ),

      checkboxGroupInput("ageA", "Age group:",
        choices = c(
          "15 to 24" = "15-24",
          "25 to 34" = "25-34",
          "35 to 44" = "35-44",
          "45 to 54" = "45-54",
          "55 to 64" = "55-64",
          "65 to 74" = "65-74",
          "75 to 84" = "75-84",
          "15 to 54" = "15-54",
          "15 to 64" = "15-64",
          "All ages" = "All ages"
        ),
        selected = c("All ages")
      )
    )
    
  ),
  tabPanel("Notes", includeMarkdown("notesAmphetamines.md"))
  )
  ),

  # Cocaine tab -------------------------------------------------------------
  tabPanel(value="PlotC", 
    "Cocaine",
    h1("Drug induced deaths involving cocaine"),

    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Plot",
        mainPanel(
      withLoader(plotlyOutput("cocainePlot"), type = "html", loader = "loader4"),
      fluidRow(includeMarkdown("notesNoSexSummary.md"))
    ),

    sidebarPanel(
      sliderInput("yearsC", "Period",
        min = 1997,
        max = 2016, value = c(2007, 2016), sep = ""
      ),
      selectInput(
        "plotC", "Plot:",
        c(
          "Number of deaths" = "deaths",
          "Deaths per 100,000 people" = "deathrateht",
          "Deaths per 100,000 people (95% CI)" = "deathratehtci",
          "Deaths per 1,000,000 people" = "deathratem",
          "Deaths per 1,000,000 people (95% CI)" = "deathratemci"
        ),
        selected="deathrateht"
      ),

      radioButtons(
        "codC", "Intent:",
        c(
          "All" = "All",
          "Accidental" = "Accidental"
        ),
        selected="All"
      ),

      conditionalPanel(
        condition = "input.codC == 'Accidental'",
        radioButtons(
          "natureC", "Nature of death:",
          c("Any mention", "Underlying"),
          selected="Any mention"
        )
      ),

      conditionalPanel(
        condition = "input.codC == 'All'",
        radioButtons(
          "natureC", "Nature of death:",
          c("Any mention"),
          selected = "Any mention"
        )
      ),
      
      selectInput("ageC", "Age group:",
        choices = c(
          "All ages" = "All ages",
          "15 to 54" = "15-54",
          "15 to 64" = "15-64"
        )
      )
    )
    
      ),
    tabPanel("Notes", includeMarkdown("notesCocaine.md"))
    )
  ),
  

  # All drugs tab ---------------------------------------------------------------
  tabPanel(value="PlotD", 
    "All drugs",
    h1("Drug induced deaths"),

    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Plot",
        mainPanel(
      withLoader(plotlyOutput("drugPlot"), type = "html", loader = "loader4"),
      fluidRow(includeMarkdown("notesAllDrugsPlotPage.md"))
    ),

    sidebarPanel(
      sliderInput("yearsD", "Period",
        min = 1997,
        max = 2016, value = c(2007, 2016), sep = ""
      ),
      selectInput(
        "plotD", "Plot:",
        c(
          "Number of deaths" = "deaths",
          "Deaths per 100,000 people" = "deathrateht",
          "Deaths per 100,000 people (95% CI)" = "deathratehtci",
          "Deaths per 1,000,000 people" = "deathratem",
          "Deaths per 1,000,000 people (95% CI)" = "deathratemci"
        ),
        selected="deathrateht"
      ),

      selectInput("ageD", "Age group:",
        choices = c(
          "All ages" = "All ages",
          "15 to 54" = "15-54",
          "15 to 64" = "15-64"
        )
      ),

      checkboxGroupInput("drugD", "Drug:",
        choices = c(
          "Amphetamines",
          "Cocaine",
          "All opioids",
          "Heroin",
          "Methadone",
          "Natural and semi-synthetic opioids",
          "Synthetic opioids",
          "Other and unspecified opioids"
        ),
        selected = c("Amphetamines", "Cocaine", "All opioids")
      )
    )
    
  ),
  tabPanel("Notes", includeMarkdown("notesAllDrugs.md"))
)
),


# Notes tab ---------------------------------------------------------------
  tabPanel(
    "Summary Notes",
    fluidRow(
      column(width = 8, includeMarkdown("notesOverall.md")),
      column(width = 4, includeHTML("DNetLogo.html"))
    )
  )
)
}