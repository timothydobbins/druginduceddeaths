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
  "Deaths related to:", id="Plot",
  navbarMenu(

    # Opioids tab -------------------------------------------------------------
    "Opioids",

    tabPanel(value="PlotOA", 
      # Opioids by intent, type and age -----------------------------------------
      "By opioid type, intent and age",
      h1("Opioid related deaths over time"),
      h3("By opioid type, intent and age"),
      
      tabsetPanel(
        type = "tabs",
        tabPanel("Plot",
          mainPanel(
            withLoader(plotlyOutput("opioidPlotA"), type = "html", loader = "loader4"),
            fluidRow(includeMarkdown("notes.md"))
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
                "Deaths per 100,000" = "deathrateht",
                "Deaths per 100,000 with CI" = "deathratehtci",
                "Deaths per 1,000,000" = "deathratem",
                "Deaths per 1,000,000 with CI" = "deathratemci"
              )
            ),

            selectInput("drugOA", "Opioid type:",
              choices = c(
                "All opioids",
                "Heroin",
                "Methadone",
                "Other opioids",
                "Other and unspecified narcotics",
                "Other synthetic narcotics"
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
      # Opioids by intent, type and sex -----------------------------------------
      "By intent, opioid type and sex",
      h1("Opioid related deaths over time"),
      h3("By intent, opioid type and sex"),

      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Plot",


          mainPanel(
            withLoader(plotlyOutput("opioidPlotB"), type = "html", loader = "loader4"),
            fluidRow(includeMarkdown("notes.md"))
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
                "Deaths per 100,000" = "deathrateht",
                "Deaths per 100,000 with CI" = "deathratehtci",
                "Deaths per 1,000,000" = "deathratem",
                "Deaths per 1,000,000 with CI" = "deathratemci"
              )
            ),

            selectInput(
              "ageOB", "Age range:",
              c("All ages", "15 to 64" = "15-64")
            ),

            selectInput(
              "codOB", "Intent:",
              c("All", "Accidental", "Intentional", "Undetermined")
            ),

            checkboxGroupInput("drugOB", "Opioid type:",
              choices = c(
                "All opioids",
                "Heroin",
                "Methadone",
                "Other opioids",
                "Other and unspecified narcotics",
                "Other synthetic narcotics"
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
      # Opioids by sex, intent and type -----------------------------------------
      "By sex, intent and opioid type",
      h1("Opioid related deaths over time"),
      h3("By sex, intent and opioid type"),

      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Plot",

          mainPanel(
            withLoader(plotlyOutput("opioidPlotC"), type = "html", loader = "loader4"),
            fluidRow(includeMarkdown("notes.md"))
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
                "Deaths per 100,000" = "deathrateht",
                "Deaths per 100,000 with CI" = "deathratehtci",
                "Deaths per 1,000,000" = "deathratem",
                "Deaths per 1,000,000 with CI" = "deathratemci"
              )
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

            checkboxGroupInput("drugOC", "Opioid type:",
              choices = c(
                "All opioids",
                "Heroin",
                "Methadone",
                "Other opioids",
                "Other and unspecified narcotics",
                "Other synthetic narcotics"
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

      h1("Opioid related deaths over time"),
      h3("By intent, jurisdiction and sex"),

      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Plot",
          mainPanel(
            withLoader(plotlyOutput("opioidPlotD"), type = "html", loader = "loader4")
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
                "Deaths per 100,000" = "deathrateht",
                "Deaths per 100,000 with CI" = "deathratehtci",
                "Deaths per 1,000,000" = "deathratem",
                "Deaths per 1,000,000 with CI" = "deathratemci"
              )
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
    )
  ),

  # Amphetamines tab --------------------------------------------------------
  tabPanel(value="PlotA", 
    "Amphetamines",
    h1("Amphetamine related deaths over time"),

    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Plot",
        mainPanel(
      withLoader(plotlyOutput("amphetaminePlot"), type = "html", loader = "loader4")
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
          "Deaths per 100,000" = "deathrateht",
          "Deaths per 100,000 with CI" = "deathratehtci",
          "Deaths per 1,000,000" = "deathratem",
          "Deaths per 1,000,000 with CI" = "deathratemci"
        )
      ),

      selectInput(
        "codA", "Intent:",
        c("All", "Accidental")
      ),

      conditionalPanel(
        condition = "input.codA == 'Accidental'",
        selectInput(
          "natureA", "Nature of death:",
          c("Any mention", "Underlying")
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
    h1("Cocaine related deaths over time"),

    tabsetPanel(
      type = "tabs",
      tabPanel(
        "Plot",
        mainPanel(
      withLoader(plotlyOutput("cocainePlot"), type = "html", loader = "loader4")
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
          "Deaths per 100,000" = "deathrateht",
          "Deaths per 100,000 with CI" = "deathratehtci",
          "Deaths per 1,000,000" = "deathratem",
          "Deaths per 1,000,000 with CI" = "deathratemci"
        )
      ),

      selectInput(
        "codC", "Intent:",
        c(
          "All" = "All",
          "Accidental" = "Accidental"
        )
      ),

      conditionalPanel(
        condition = "input.codC == 'Accidental'",
        selectInput(
          "natureC", "Nature of death:",
          c("Any mention", "Underlying")
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
    h1("Drug related deaths over time"),

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
          "Deaths per 100,000" = "deathrateht",
          "Deaths per 100,000 with CI" = "deathratehtci",
          "Deaths per 1,000,000" = "deathratem",
          "Deaths per 1,000,000 with CI" = "deathratemci"
        )
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
          "Other opioids",
          "Other and unspecified narcotics",
          "Other synthetic narcotics"
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