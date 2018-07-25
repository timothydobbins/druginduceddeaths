library(tidyverse)
library(plotly)
library(janitor)
library(summarytools)
library(shiny)
library(shinythemes)
library(shinycustomloader)

df <- read_csv("2018-07-11_Deaths_Pop_CI_Sheets678.csv")

# Extract data from sheet 6
# PLot sheet 6 with intent and sex as drop-downs
dfplot6 <- filter(df, sex == "All", word(drug, start = 1, end = 3) == "All opioids with")
qplot(x=n, data=dfplot6)
# g6 <- ggplot(filter(dfplot6, intent=="Accidental")) + aes(x = year, y = n, colour = age_group, linetype=drug) +
#   geom_line()
# g6

# Extract data from sheet 7
# Plot sheet 7 as intent and age group as drop-downs
dfplot7 <- filter(df, word(drug, start = 1, end = 3) == "All opioids with")
# g7 <- ggplot(filter(dfplot7, intent=="Accidental", age_group=="15-64")) + aes(x = year, y = n, colour = sex, linetype=drug) +
#   geom_line()
# g7

# Extract data from sheet 8
# Plot sheet 8 with intent and age group as drop-downs
dfplot8 <- filter(df, drug %in% c(
  "Exclusive illiicit opioids",
  "Exclusive pharmaceutical opioids",
  "Heroin/Opium with pharmaceutical opioids",
  "Unspecified opioids"
))

# g8 <- ggplot(filter(dfplot8, intent=="Accidental", age_group=="15-64")) + aes(x = year, y = n, colour = sex, linetype=drug) +
#   geom_line()
# g8

library(shiny)

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
          mainPanel("Plot goes here"),
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
          mainPanel("Plot goes here"),
          sidebarPanel(
            "Options go here",
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
          mainPanel("Plot goes here"),
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
            
            checkboxGroupInput("drugD", "Drug:",
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

server <- function(input, output, session) {

}

shinyApp(ui, server)

