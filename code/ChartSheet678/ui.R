library(tidyverse)
library(plotly)
library(janitor)
library(summarytools)
library(shiny)
library(shinythemes)
library(shinycustomloader)

ui <- function(req) {
  navbarPage(
    theme = shinytheme("yeti"),
    "Deaths related to:", id = "Plot",

    # Opioids and other drugs by age -------------------------------------------------------------
    tabPanel(
      "Opioids and other drugs by age",
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Plot",
            mainPanel(
              withLoader(plotlyOutput("PlotA"), type = "html", loader = "loader4")
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
              "intentA", "Intent:",
              c("All", "Accidental", "Intentional", "Undetermined"),
              selected = "All"
            ),


            selectInput("sexA", "Sex:",
              choices = c(
                "Male",
                "Female",
                "All"
              ),
              selected = c("All")
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
                "15 to 64" = "15-64",
                "All ages" = "Allages"
              ),
              selected = c("Allages")
            ),

            checkboxGroupInput("drugA", "All opioids with:",
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

    # Opioids and other drugs by sex ------------------------------------------
    tabPanel(
      "Opioids and other drugs by sex",
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Plot",
          mainPanel(
            withLoader(plotlyOutput("PlotB"), type = "html", loader = "loader4")
          ),
          sidebarPanel(
            sliderInput("yearsB", "Period",
              min = 1997,
              max = 2016, value = c(2007, 2016), sep = ""
            ),
            selectInput(
              "plotB", "Plot:",
              c(
                "Number of deaths" = "deaths",
                "Deaths per 100,000" = "deathrateht",
                "Deaths per 100,000 with CI" = "deathratehtci",
                "Deaths per 1,000,000" = "deathratem",
                "Deaths per 1,000,000 with CI" = "deathratemci"
              )
            ),

            selectInput(
              "intentB", "Intent:",
              c("All", "Accidental", "Intentional", "Undetermined"),
              selected = "All"
            ),

            selectInput(
              "ageB", "Age:",
              c("15-64",
                "All ages" = "Allages"
              ),
              selected = "Allages"
            ),

            checkboxGroupInput("sexB", "Sex:",
              choices = c(
                "Male",
                "Female",
                "All"
              ),
              selected = c("All")
            ),
            checkboxGroupInput("drugB", "All opioids with:",
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

    # Exclusive opioids ------------------------------------------
    tabPanel(
      "Exclusive opioids",
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Plot",
          mainPanel(
            withLoader(plotlyOutput("PlotC"), type = "html", loader = "loader4")
          ),
          sidebarPanel(
            sliderInput("yearsC", "Period",
                        min = 2007,
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
              "intentC", "Intent:",
              c("All", "Accidental", "Intentional", "Undetermined"),
              selected = "All"
            ),
            
            selectInput(
              "ageC", "Age:",
              c("15-64",
                "All ages" = "Allages"
              ),
              selected = "Allages"
            ),
            
            checkboxGroupInput("sexC", "Sex:",
                               choices = c(
                                 "Male",
                                 "Female",
                                 "All"
                               ),
                               selected = c("All")
            ),
            
            checkboxGroupInput("drugC", "Drug:",
                               choices = c(
                                 "Exclusive illiicit opioids",
                                 "Exclusive pharmaceutical opioids",
                                 "Heroin/Opium with pharmaceutical opioids",
                                 "Unspecified opioids"
                               ),
                               selected = c("Exclusive illiicit opioids",
                                            "Exclusive pharmaceutical opioids"))
      )
        ),
      tabPanel("Notes", "Notes go here")
  )))
}

