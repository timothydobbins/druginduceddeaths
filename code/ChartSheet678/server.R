library(shiny)
library(shinythemes)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(plotly)
library(shinycustomloader)

df <- read_csv("2018-07-11_Deaths_Pop_CI_Sheets678.csv") %>%
  distinct() %>%
  mutate(
    age_group = factor(age_group, levels = c("15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", "15-54", "15-64", "Allages")),
    jurisdiction = factor(jurisdiction, levels = c("AUS", "NSW", "VIC", "QLD", "WA", "SA", "TAS", "ACT", "NT")),
    intent = factor(intent, levels = c("All", "Accidental", "Intentional", "Undetermined"))
  )

agecols <- c(
  "15-24" = "#c09840",
  "25-34" = "#657d39",
  "35-44" = "#76b74b",
  "45-54" = "#4db598",
  "55-64" = "#6b8bcd",
  "65-74" = "#8d62ca",
  "75-84" = "#c75fa1",

  "15-54" = "#fdcc8a",
  "15-64" = "#fc8d59",
  "Allages" = "#e34a33"
)

statecols <- c(
  "NSW" = "#72CDF4",
  "VIC" = "#002D62",
  "QLD" = "#7C0040",
  "WA" = "#FFD200",
  "SA" = "#E31837",
  "TAS" = "#00583D",
  "ACT" = "#0079C1",
  "NT" = "#F58426",
  "AUS" = "#666666"
)

drugcols <- c(
  "Opioids" = "#332288",
  "Heroin" = "#88CCEE",
  "Methadone" = "#44AA99",
  "Other Synthetic Narcotics" = "#117733",
  "Opium" = "#999933",
  "Other Opioids" = "#882255",
  "Other Opioids NEC" = "#CC6677",
  "Cocaine" = "#DDCC77",
  "Amphetamines" = "#AA4499"
)

drugltype <- c(
  "All opioids with alcohol"=1,
  "All opioids with antidepressants"=2,
  "All opioids with antipsychotics"=3,
  "All opioids with benzodiazepines"=4,
  "All opioids with paracetamol"=5,
  
  "Exclusive illicit opioids"=1,
  "Exclusive pharmaceutical opioids"=2,
  "Heroin/Opium with pharmaceutical opioids"=3,
  "Unspecified opioids"=4
)

codtype <- c(
  "All" = 1,
  "Accidental" = 2,
  "Intentional" = 3,
  "Undetermined" = 4
)

sextype <- c(
  "All" = 1,
  "Male" = 2,
  "Female" = 3
)

sexcols <- c(
  "All" = "#808080",
  "Male" = "#5B7EBB",
  "Female" = "#B3564D"
)

# Allow for site's state to be bookmarked via the url
# See https://shiny.rstudio.com/articles/bookmarking-state.html for details
enableBookmarking("url")

server <- function(input, output, session) {

  # Allow direct linking to specific tabs (with defaul configs)
  observe({
    query <- parseQueryString(session$clientData$url_search)
    query1 <- paste(names(query), query, sep = "=", collapse = ", ")
    print(query1)
    if (query1 == "tab=Plot6") {
      updateTabsetPanel(session, inputId = "Plot", selected = "Plot6")
    }
    if (query1 == "tab=Plot7") {
      updateTabsetPanel(session, inputId = "Plot", selected = "Plot7")
    }
    if (query1 == "tab=Plot8") {
      updateTabsetPanel(session, inputId = "Plot", selected = "Plot8")
    }
    # Trigger this observer every time an input changes
    reactiveValuesToList(input)
    session$doBookmark()
  })

  onBookmarked(function(url) {
    updateQueryString(url)
  })

  # Plot 6 -----------------------------------------------------------------
  output$PlotA <- renderPlotly({
    sub <- filter(df, word(drug, start = 1, end = 3) == "All opioids with" & jurisdiction == "AUS" &
                    drug %in% input$drugA & intent %in% input$intentA & age_group %in% input$ageA & 
                    (year >= input$yearsA[[1]] & year <= input$yearsA[[2]]) & sex==input$sexA)
    
    
    if (input$plotA == "deaths") {
  p <- ggplot(sub) + aes(x = year, y = n, colour = age_group, linetype = drug, group = 1,
                         text = paste0(
                           "Year: ", year,
                           "<br>Deaths: ", n,
                           "<br>Drug: ", str_to_title(drug),
                           "<br>Intent: ", str_to_title(intent),
                           "<br>Age group: ", age_group
                         )) + geom_line() +
    scale_y_continuous(limits = c(0, NA)) +
    labs(x = "Year", y = "Number of deaths", color = "Age") +
    theme_light() + scale_colour_manual(values = agecols) + scale_linetype_manual(values = drugltype) +
    theme(legend.title = element_blank())
    }
  
    if (input$plotA == "deathrateht") {
      p <- ggplot(sub) + aes(x = year, y = rate_ht, colour = age_group, linetype = drug, group = 1,
                             text = paste0(
                               "Year: ", year,
                               "<br>Deaths: ", n,
                               "<br>Rate: ", round(rate_ht, 2),
                               "<br>Drug: ", str_to_title(drug),
                               "<br>Intent: ", str_to_title(intent),
                               "<br>Age group: ", age_group
                             )) + geom_line() +
        scale_y_continuous(limits = c(0, NA)) +
        labs(x = "Year", y = "Deaths per 100,000", color = "Age") +
        theme_light() + scale_colour_manual(values = agecols) +scale_linetype_manual(values = drugltype) +
        theme(legend.title = element_blank())
    }

    if (input$plotA == "deathratehtci") {
      p <- ggplot(sub) + aes(x = year, y = rate_ht, colour = age_group, linetype = drug, group = 1,
                             text = paste0(
                               "Year: ", year,
                               "<br>Deaths: ", n,
                               "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
                               "<br>Drug: ", str_to_title(drug),
                               "<br>Intent: ", str_to_title(intent),
                               "<br>Age group: ", age_group
                             )) + geom_line() +
        geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0) +
        scale_y_continuous(limits = c(0, NA)) +
        labs(x = "Year", y = "Deaths per 100,000", color = "Age") +
        theme_light() + scale_colour_manual(values = agecols) +scale_linetype_manual(values = drugltype) +
        theme(legend.title = element_blank())
    }
    
    if (input$plotA == "deathratem") {
      p <- ggplot(sub) + aes(x = year, y = rate_m, colour = age_group, linetype = drug, group = 1,
                             text = paste0(
                               "Year: ", year,
                               "<br>Deaths: ", n,
                               "<br>Rate: ", round(rate_m, 2),
                               "<br>Drug: ", str_to_title(drug),
                               "<br>Intent: ", str_to_title(intent),
                               "<br>Age group: ", age_group
                             )) + geom_line() +
        scale_y_continuous(limits = c(0, NA)) +
        labs(x = "Year", y = "Deaths per 1,000,000", color = "Age") +
        theme_light() + scale_colour_manual(values = agecols) +scale_linetype_manual(values = drugltype) +
        theme(legend.title = element_blank())
    }
    
    if (input$plotA == "deathratemci") {
      p <- ggplot(sub) + aes(x = year, y = rate_m, colour = age_group, linetype = drug, group = 1,
                             text = paste0(
                               "Year: ", year,
                               "<br>Deaths: ", n,
                               "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
                               "<br>Drug: ", str_to_title(drug),
                               "<br>Intent: ", str_to_title(intent),
                               "<br>Age group: ", age_group
                             )) + geom_line() +
        geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0) +
        scale_y_continuous(limits = c(0, NA)) +
        labs(x = "Year", y = "Deaths per 1,000,000", color = "Age") +
        theme_light() + scale_colour_manual(values = agecols) +scale_linetype_manual(values = drugltype) +
        theme(legend.title = element_blank())
    }
    
    
    ggplotly(p, tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/program/drug-trends">DrugTrends</a>, NDARC',
        xref = "paper", yref = "paper",
        x = 0.01, xanchor = "left",
        y = 0.995, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "grey")
      ) %>%
      add_annotations(
        text = "Age and drug", xref = "paper", yref = "paper",
        x = 1.02, xanchor = "left",
        y = 0.95, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.95, yanchor = "top"), margin = list(b = 100, l = 100)) %>%
      config(displaylogo=F, collaborate = FALSE, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                                               "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                                               "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
    
  })

  # Plot 7 -----------------------------------------------------------------
  output$PlotB <- renderPlotly({
    sub <- filter(df, word(drug, start = 1, end = 3) == "All opioids with" & jurisdiction == "AUS" &
                    drug %in% input$drugB & intent == input$intentB & age_group == input$ageB & 
                    (year >= input$yearsB[[1]] & year <= input$yearsB[[2]]) & sex %in% input$sexB)
    
    
    if (input$plotB == "deaths") {
      p <- ggplot(sub) + aes(x = year, y = n, colour = sex, linetype = drug, group = 1,
                             text = paste0(
                               "Year: ", year,
                               "<br>Deaths: ", n,
                               "<br>Drug: ", str_to_title(drug),
                               "<br>Intent: ", str_to_title(intent),
                               "<br>Sex: ", sex
                             )) + geom_line() +
        scale_y_continuous(limits = c(0, NA)) +
        labs(x = "Year", y = "Number of deaths", color = "Sex") +
        theme_light() + scale_colour_manual(values = sexcols) +scale_linetype_manual(values = drugltype) +
        theme(legend.title = element_blank())
    }
    
    if (input$plotB == "deathrateht") {
      p <- ggplot(sub) + aes(x = year, y = rate_ht, colour = sex, linetype = drug, group = 1,
                             text = paste0(
                               "Year: ", year,
                               "<br>Deaths: ", n,
                               "<br>Rate: ", round(rate_ht, 2),
                               "<br>Drug: ", str_to_title(drug),
                               "<br>Intent: ", str_to_title(intent),
                               "<br>Sex: ", sex
                             )) + geom_line() +
        scale_y_continuous(limits = c(0, NA)) +
        labs(x = "Year", y = "Deaths per 100,000", color = "Sex") +
        theme_light() + scale_colour_manual(values = sexcols) +scale_linetype_manual(values = drugltype) +
        theme(legend.title = element_blank())
    }
    
    if (input$plotB == "deathratehtci") {
      p <- ggplot(sub) + aes(x = year, y = rate_ht, colour = sex, linetype = drug, group = 1,
                             text = paste0(
                               "Year: ", year,
                               "<br>Deaths: ", n,
                               "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
                               "<br>Drug: ", str_to_title(drug),
                               "<br>Intent: ", str_to_title(intent),
                               "<br>Sex: ", sex
                             )) + geom_line() +
        geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0) +
        scale_y_continuous(limits = c(0, NA)) +
        labs(x = "Year", y = "Deaths per 100,000", color = "Sex") +
        theme_light() + scale_colour_manual(values = sexcols) +scale_linetype_manual(values = drugltype) +
        theme(legend.title = element_blank())
    }
    
    if (input$plotB == "deathratem") {
      p <- ggplot(sub) + aes(x = year, y = rate_m, colour = sex, linetype = drug, group = 1,
                             text = paste0(
                               "Year: ", year,
                               "<br>Deaths: ", n,
                               "<br>Rate: ", round(rate_m, 2),
                               "<br>Drug: ", str_to_title(drug),
                               "<br>Intent: ", str_to_title(intent),
                               "<br>Sex: ", sex
                             )) + geom_line() +
        scale_y_continuous(limits = c(0, NA)) +
        labs(x = "Year", y = "Deaths per 1,000,000", color = "Sex") +
        theme_light() + scale_colour_manual(values = sexcols) +scale_linetype_manual(values = drugltype) +
        theme(legend.title = element_blank())
    }
    
    if (input$plotB == "deathratemci") {
      p <- ggplot(sub) + aes(x = year, y = rate_m, colour = sex, linetype = drug, group = 1,
                             text = paste0(
                               "Year: ", year,
                               "<br>Deaths: ", n,
                               "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
                               "<br>Drug: ", str_to_title(drug),
                               "<br>Intent: ", str_to_title(intent),
                               "<br>Sex: ", sex
                             )) + geom_line() +
        geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0) +
        scale_y_continuous(limits = c(0, NA)) +
        labs(x = "Year", y = "Deaths per 1,000,000", color = "Sex") +
        theme_light() + scale_colour_manual(values = sexcols) +scale_linetype_manual(values = drugltype) +
        theme(legend.title = element_blank())
    }
    
    
    ggplotly(p, tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/program/drug-trends">DrugTrends</a>, NDARC',
        xref = "paper", yref = "paper",
        x = 0.01, xanchor = "left",
        y = 0.995, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "grey")
      ) %>%
      add_annotations(
        text = "Sex and drug", xref = "paper", yref = "paper",
        x = 1.02, xanchor = "left",
        y = 0.95, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.95, yanchor = "top"), margin = list(b = 100, l = 100)) %>%
      config(displaylogo=F, collaborate = FALSE, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                                               "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                                               "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
    
  })

  # Plot 8 -----------------------------------------------------------------
  output$PlotC <- renderPlotly({
    sub <- filter(df, jurisdiction == "AUS" &
                    drug %in% input$drugC & intent == input$intentC & age_group == input$ageC & 
                    (year >= input$yearsC[[1]] & year <= input$yearsC[[2]]) & sex %in% input$sexC)
    
    
    if (input$plotC == "deaths") {
      p <- ggplot(sub) + aes(x = year, y = n, colour = sex, linetype = drug, group = 1,
                             text = paste0(
                               "Year: ", year,
                               "<br>Deaths: ", n,
                               "<br>Drug: ", str_to_title(drug),
                               "<br>Intent: ", str_to_title(intent),
                               "<br>Sex: ", sex
                             )) + geom_line() +
        scale_y_continuous(limits = c(0, NA)) +
        labs(x = "Year", y = "Number of deaths", color = "Sex") +
        theme_light() + scale_colour_manual(values = sexcols) +scale_linetype_manual(values = drugltype) +
        theme(legend.title = element_blank())
    }
    
    if (input$plotC == "deathrateht") {
      p <- ggplot(sub) + aes(x = year, y = rate_ht, colour = sex, linetype = drug, group = 1,
                             text = paste0(
                               "Year: ", year,
                               "<br>Deaths: ", n,
                               "<br>Rate: ", round(rate_ht, 2),
                               "<br>Drug: ", str_to_title(drug),
                               "<br>Intent: ", str_to_title(intent),
                               "<br>Sex: ", sex
                             )) + geom_line() +
        scale_y_continuous(limits = c(0, NA)) +
        labs(x = "Year", y = "Deaths per 100,000", color = "Sex") +
        theme_light() + scale_colour_manual(values = sexcols) +scale_linetype_manual(values = drugltype) +
        theme(legend.title = element_blank())
    }
    
    if (input$plotC == "deathratehtci") {
      p <- ggplot(sub) + aes(x = year, y = rate_ht, colour = sex, linetype = drug, group = 1,
                             text = paste0(
                               "Year: ", year,
                               "<br>Deaths: ", n,
                               "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
                               "<br>Drug: ", str_to_title(drug),
                               "<br>Intent: ", str_to_title(intent),
                               "<br>Sex: ", sex
                             )) + geom_line() +
        geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0) +
        scale_y_continuous(limits = c(0, NA)) +
        labs(x = "Year", y = "Deaths per 100,000", color = "Sex") +
        theme_light() + scale_colour_manual(values = sexcols) +scale_linetype_manual(values = drugltype) +
        theme(legend.title = element_blank())
    }
    
    if (input$plotC == "deathratem") {
      p <- ggplot(sub) + aes(x = year, y = rate_m, colour = sex, linetype = drug, group = 1,
                             text = paste0(
                               "Year: ", year,
                               "<br>Deaths: ", n,
                               "<br>Rate: ", round(rate_m, 2),
                               "<br>Drug: ", str_to_title(drug),
                               "<br>Intent: ", str_to_title(intent),
                               "<br>Sex: ", sex
                             )) + geom_line() +
        scale_y_continuous(limits = c(0, NA)) +
        labs(x = "Year", y = "Deaths per 1,000,000", color = "Sex") +
        theme_light() + scale_colour_manual(values = sexcols) +scale_linetype_manual(values = drugltype) +
        theme(legend.title = element_blank())
    }
    
    if (input$plotC == "deathratemci") {
      p <- ggplot(sub) + aes(x = year, y = rate_m, colour = sex, linetype = drug, group = 1,
                             text = paste0(
                               "Year: ", year,
                               "<br>Deaths: ", n,
                               "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
                               "<br>Drug: ", str_to_title(drug),
                               "<br>Intent: ", str_to_title(intent),
                               "<br>Sex: ", sex
                             )) + geom_line() +
        geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0) +
        scale_y_continuous(limits = c(0, NA)) +
        labs(x = "Year", y = "Deaths per 1,000,000", color = "Sex") +
        theme_light() + scale_colour_manual(values = sexcols) +scale_linetype_manual(values = drugltype) +
        theme(legend.title = element_blank())
    }
    
    
    ggplotly(p, tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/program/drug-trends">DrugTrends</a>, NDARC',
        xref = "paper", yref = "paper",
        x = 0.01, xanchor = "left",
        y = 0.995, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "grey")
      ) %>%
      add_annotations(
        text = "Sex and drug", xref = "paper", yref = "paper",
        x = 1.02, xanchor = "left",
        y = 0.95, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.95, yanchor = "top"), margin = list(b = 100, l = 100)) %>%
      config(displaylogo=F, collaborate = FALSE, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                                               "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                                               "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
    
  })
    
  }
