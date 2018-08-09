library(shiny)
library(shinythemes)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(plotly)
library(shinycustomloader)

df <- read_csv("Deaths_Pop_CI.csv") %>%
  distinct() %>%
  mutate(
    age_group = ifelse(age_group == "Allages", "All ages", age_group),
    age_group = factor(age_group, levels = c("15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75-84", "15-54", "15-64", "All ages")),
    jurisdiction = factor(jurisdiction, levels = c("AUS", "NSW", "VIC", "QLD", "WA", "SA", "TAS", "ACT", "NT")),
    intent = factor(intent, levels = c("All", "Accidental", "Intentional", "Undetermined")),

    # Rename clunky opioids
    drug = case_when(
      drug == "Other opioids" ~ "Natural and semi-synthetic opioids",
      drug == "Other and unspecified narcotics" ~ "Other and unspecified opioids",
      drug == "Other synthetic narcotics" ~ "Synthetic opioids",
      drug == "Heroin/Opium with pharmaceutical opioids" ~ "Illicit and pharmaceutical opioids",
      TRUE ~ drug
    )
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
  "All ages" = "#e34a33"
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

drugcols <- c("All opioids" = "#332288",
              "Heroin" = "#88CCEE",
              "Methadone" = "#44AA99",
              "Synthetic opioids" = "#117733",
              "Opium" = "#999933",
              "Natural and semi-synthetic opioids" = "#882255",
              "Other and unspecified opioids" = "#CC6677",
              "Cocaine" = "#DDCC77",
              "Amphetamines" = "#AA4499")

drugltype <- c(
  "All opioids with alcohol"=1,
  "All opioids with antidepressants"=2,
  "All opioids with antipsychotics"=3,
  "All opioids with benzodiazepines"=4,
  "All opioids with paracetamol"=5,
  
  "Exclusive illicit opioids"=1,
  "Exclusive pharmaceutical opioids"=2,
  "Illicit and pharmaceutical opioids"=3,
  "Unspecified opioids"=4)

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
    query1 <- paste(names(query), query, sep = "=", collapse=", ")
    print(query1)
    if(query1 == "tab=PlotOA"){
      updateTabsetPanel(session, inputId = "Plot", selected = "PlotOA")
    }
    if(query1 == "tab=PlotOB"){
      updateTabsetPanel(session, inputId = "Plot", selected = "PlotOB")
    }
    if(query1 == "tab=PlotOC"){
      updateTabsetPanel(session, inputId = "Plot", selected = "PlotOC")
    }
    if(query1 == "tab=PlotOD"){
      updateTabsetPanel(session, inputId = "Plot", selected = "PlotOD")
    }
    if(query1 == "tab=PlotOE"){
      updateTabsetPanel(session, inputId = "Plot", selected = "PlotOE")
    }
    if(query1 == "tab=PlotOF"){
      updateTabsetPanel(session, inputId = "Plot", selected = "PlotOF")
    }
    if(query1 == "tab=PlotOG"){
      updateTabsetPanel(session, inputId = "Plot", selected = "PlotOG")
    }
    if(query1 == "tab=PlotOH"){
      updateTabsetPanel(session, inputId = "Plot", selected = "PlotOH")
    }
    if(query1 == "tab=PlotA"){
      updateTabsetPanel(session, inputId = "Plot", selected = "PlotA")
    }
    if(query1 == "tab=PlotC"){
      updateTabsetPanel(session, inputId = "Plot", selected = "PlotC")
    }
    if(query1 == "tab=PlotD"){
      updateTabsetPanel(session, inputId = "Plot", selected = "PlotD")
    }
    # Trigger this observer every time an input changes
    reactiveValuesToList(input)
    session$doBookmark()
  })

  onBookmarked(function(url) {
    updateQueryString(url)
  })
  
  # Plot OA -----------------------------------------------------------------
  output$opioidPlotA <- renderPlotly({
    sub <- subset(df, subset = (sex == "All" & jurisdiction == "AUS" & drug == input$drugOA &
      intent %in% input$codOA & age_group %in% input$ageOA &
      (year >= input$yearsOA[[1]] & year <= input$yearsOA[[2]])))

    if (input$plotOA == "deaths") {
      p <- ggplot(sub) + aes(
        x = year, y = n, colour = age_group, linetype = intent, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Opioid: ", str_to_title(drug),
          "<br>Intent: ", str_to_title(intent),
          "<br>Age group: ", age_group
        )
      ) +
        geom_line() + scale_y_continuous(limits = c(0, NA)) +
        labs(x = "Year", y = "Number of deaths", color = "Age") +
        theme_light() + scale_colour_manual(values = agecols) +
        scale_linetype_manual(values = codtype) + theme(legend.title = element_blank())
    }

    else if (input$plotOA == "deathrateht") {
      p <- ggplot(sub) + aes(
        x = year, y = rate_ht, colour = age_group, linetype = intent, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
          "<br>Opioid: ", str_to_title(drug),
          "<br>Intent: ", str_to_title(intent),
          "<br>Age group: ", age_group
        )
      ) +
        geom_line() + scale_y_continuous(limits = c(0, NA)) +
        labs(x = "Year", y = "Deaths per 100,000", colour = "Age") +
        theme_light() + scale_colour_manual(values = agecols) +
        scale_linetype_manual(values = codtype) + theme(legend.title = element_blank())
    }

    else if (input$plotOA == "deathratehtci") {
      p <- ggplot(sub) + aes(
        x = year, y = rate_ht, colour = age_group, linetype = intent, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
          "<br>Opioid: ", str_to_title(drug),
          "<br>Intent: ", str_to_title(intent),
          "<br>Age group: ", age_group
        )
      ) +
        geom_line() + scale_y_continuous(limits = c(0, NA)) +
        geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0) +
        labs(x = "Year", y = "Deaths per 100,000", colour = "Age") +
        theme_light() + scale_colour_manual(values = agecols) +
        scale_linetype_manual(values = codtype) + theme(legend.title = element_blank())
    }

    else if (input$plotOA == "deathratem") {
      p <- ggplot(sub) + aes(
        x = year, y = rate_m, colour = age_group, linetype = intent, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
          "<br>Opioid: ", str_to_title(drug),
          "<br>Intent: ", str_to_title(intent),
          "<br>Age group: ", age_group
        )
      ) +
        geom_line() + scale_y_continuous(limits = c(0, NA)) +
        labs(x = "Year", y = "Deaths per 1,000,000", colour = "Age") +
        theme_light() + scale_colour_manual(values = agecols) +
        scale_linetype_manual(values = codtype) + theme(legend.title = element_blank())
    }

    else if (input$plotOA == "deathratemci") {
      p <- ggplot(sub) + aes(
        x = year, y = rate_m, colour = age_group, linetype = intent, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
          "<br>Opioid: ", str_to_title(drug),
          "<br>Intent: ", str_to_title(intent),
          "<br>Age group: ", age_group
        )
      ) +
        geom_line() + scale_y_continuous(limits = c(0, NA)) +
        geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0) +
        labs(x = "Year", y = "Deaths per 1,000,000", colour = "Age") +
        theme_light() + scale_colour_manual(values = agecols) +
        scale_linetype_manual(values = codtype) + theme(legend.title = element_blank())
    }

    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Remove vertical gridlines
    p <- p + theme(panel.grid.minor.x = element_blank(),
                   panel.grid.major.x = element_blank())
    
    ggplotly(p, tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/opioid-amphetamine-and-cocaine-induced-deaths-australia-august-2018">DrugTrends</a>, NDARC',
        xref = "paper", yref = "paper",
        x = 0.01, xanchor = "left",
        y = 0.995, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "grey")
      ) %>%
      add_annotations(
        text = "Age by intent", xref = "paper", yref = "paper",
        x = 1.02, xanchor = "left",
        y = 0.95, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.95, yanchor = "top"), margin = list(b = 100, l = 100)) %>% 
      config(displaylogo=F, collaborate = FALSE, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                          "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                          "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
  })

  # Plot OB -----------------------------------------------------------------
  output$opioidPlotB <- renderPlotly({
    sub <- subset(df, subset = (age_group == input$ageOB & jurisdiction == "AUS" & drug %in% input$drugOB &
      intent == input$codOB & sex %in% input$sexOB  &
        (year >= input$yearsOB[[1]] & year <= input$yearsOB[[2]])))

    if (input$plotOB == "deaths") {
      p <- ggplot(sub) + aes(
        x = year, y = n, colour = drug, linetype=sex, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Intent: ", str_to_title(intent),
          "<br>Opioid: ", drug,
          "<br>Sex: ", sex
        )
      ) +
        geom_line() +
        scale_y_continuous(limits = c(0, NA)) +
        labs(x = "Year", y = "Number of deaths") +
        theme_light() + scale_colour_manual(values = drugcols) +
        scale_linetype_manual(values = sextype) + theme(legend.title = element_blank())
    }

    else if (input$plotOB == "deathrateht") {
      p <- ggplot(sub) + aes(
        x = year, y = rate_ht, colour = drug, linetype=sex, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
          "<br>Intent: ", str_to_title(intent),
          "<br>Opioid: ", drug,
          "<br>Sex: ", sex
        )
      ) +
        geom_line() +
        scale_y_continuous(limits = c(0, NA)) +
        labs(x = "Year", y = "Deaths per 100,000") +
        theme_light() + scale_colour_manual(values = drugcols) +
        scale_linetype_manual(values = sextype) + theme(legend.title = element_blank())
    }

    else if (input$plotOB == "deathratehtci") {
      p <- ggplot(sub) + aes(
        x = year, y = rate_ht, colour = drug, linetype=sex, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
          "<br>Intent: ", str_to_title(intent),
          "<br>Opioid: ", drug,
          "<br>Sex: ", sex
        )
      ) +
        geom_line() +
        geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0) +
        labs(x = "Year", y = "Deaths per 100,000") +
        theme_light() + scale_colour_manual(values = drugcols) +
        scale_linetype_manual(values = sextype) + theme(legend.title = element_blank())
    }

    else if (input$plotOB == "deathratem") {
      p <- ggplot(sub) + aes(
        x = year, y = rate_m, colour = drug, linetype=sex, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
          "<br>Intent: ", str_to_title(intent),
          "<br>Opioid: ", drug,
          "<br>Sex: ", sex
        )
      ) +
        geom_line() +
        scale_y_continuous(limits = c(0, NA)) +
        labs(x = "Year", y = "Deaths per 1,000,000") +
        theme_light() + scale_colour_manual(values = drugcols) +
        scale_linetype_manual(values = sextype) + theme(legend.title = element_blank())
    }

    else if (input$plotOB == "deathratemci") {
      p <- ggplot(sub) + aes(
        x = year, y = rate_m, colour = drug, linetype=sex, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
          "<br>Intent: ", str_to_title(intent),
          "<br>Opioid: ", drug,
          "<br>Sex: ", sex
        )
      ) +
        geom_line() +
        geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0) +
        labs(x = "Year", y = "Deaths per 1,000,000") +
        theme_light() + scale_colour_manual(values = drugcols) +
        scale_linetype_manual(values = sextype) + theme(legend.title = element_blank())
    }

    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Remove vertical gridlines
    p <- p + theme(panel.grid.minor.x = element_blank(),
                   panel.grid.major.x = element_blank())

    ggplotly(p, tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/opioid-amphetamine-and-cocaine-induced-deaths-australia-august-2018">DrugTrends</a>, NDARC',
        xref = "paper", yref = "paper",
        x = 0.01, xanchor = "left",
        y = 0.995, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "grey")
      ) %>%
      add_annotations(
        text = "Opioid by sex", xref = "paper", yref = "paper",
        x = 1.02, xanchor = "left",
        y = 0.95, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.95, yanchor = "top"), margin = list(b = 100, l = 100)) %>% 
      config(displaylogo=F, collaborate = FALSE, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                                               "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                                               "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
  })
  

# Plot OC -----------------------------------------------------------------
  output$opioidPlotC <- renderPlotly({
    sub <- subset(df, subset = (age_group == input$ageOC & jurisdiction == "AUS" & drug %in% input$drugOC &
                                  intent %in% input$codOC & sex == input$sexOC  &
                                  (year >= input$yearsOC[[1]] & year <= input$yearsOC[[2]])))
    
    if (input$plotOC == "deaths") {
      p <- ggplot(sub) + aes(
        x = year, y = n, colour = drug, linetype=intent, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Sex: ", sex,
          "<br>Intent: ", str_to_title(intent),
          "<br>Opioid: ", drug
        )
      ) +
        geom_line() +
        scale_y_continuous(limits = c(0, NA)) +
        labs(x = "Year", y = "Number of deaths") +
        theme_light() + scale_colour_manual(values = drugcols) +
        scale_linetype_manual(values = codtype) + theme(legend.title = element_blank())
    }
    
    else if (input$plotOC == "deathrateht") {
      p <- ggplot(sub) + aes(
        x = year, y = rate_ht, colour = drug, linetype=intent, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
          "<br>Sex: ", sex,
          "<br>Intent: ", str_to_title(intent),
          "<br>Opioid: ", drug
        )
      ) +
        geom_line() +
        scale_y_continuous(limits = c(0, NA)) +
        labs(x = "Year", y = "Deaths per 100,000") +
        theme_light() + scale_colour_manual(values = drugcols) +
        scale_linetype_manual(values = codtype) + theme(legend.title = element_blank())
    }
    
    else if (input$plotOC == "deathratehtci") {
      p <- ggplot(sub) + aes(
        x = year, y = rate_ht, colour = drug, linetype=intent, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
          "<br>Sex: ", sex,
          "<br>Intent: ", str_to_title(intent),
          "<br>Opioid: ", drug
        )
      ) +
        geom_line() +
        geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0) +
        labs(x = "Year", y = "Deaths per 100,000") +
        theme_light() + scale_colour_manual(values = drugcols) +
        scale_linetype_manual(values = codtype) + theme(legend.title = element_blank())
    }
    
    else if (input$plotOC == "deathratem") {
      p <- ggplot(sub) + aes(
        x = year, y = rate_m, colour = drug, linetype=intent, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
          "<br>Sex: ", sex,
          "<br>Intent: ", str_to_title(intent),
          "<br>Opioid: ", drug
        )
      ) +
        geom_line() +
        scale_y_continuous(limits = c(0, NA)) +
        labs(x = "Year", y = "Deaths per 1,000,000") +
        theme_light() + scale_colour_manual(values = drugcols) +
        scale_linetype_manual(values = codtype) + theme(legend.title = element_blank())
    }
    
    else if (input$plotOC == "deathratemci") {
      p <- ggplot(sub) + aes(
        x = year, y = rate_m, colour = drug, linetype=intent, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
          "<br>Sex: ", sex,
          "<br>Intent: ", str_to_title(intent),
          "<br>Opioid: ", drug
        )
      ) +
        geom_line() +
        geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0) +
        labs(x = "Year", y = "Deaths per 1,000,000") +
        theme_light() + scale_colour_manual(values = drugcols) +
        scale_linetype_manual(values = codtype) + theme(legend.title = element_blank())
    }
    
    validate(need(nrow(sub) > 0, "No data selected"))

    # Remove vertical gridlines
    p <- p + theme(panel.grid.minor.x = element_blank(),
                   panel.grid.major.x = element_blank())
    
    ggplotly(p, tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/opioid-amphetamine-and-cocaine-induced-deaths-australia-august-2018">DrugTrends</a>, NDARC',
        xref = "paper", yref = "paper",
        x = 0.01, xanchor = "left",
        y = 0.995, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "grey")
      ) %>%
      add_annotations(
        text = "Opioid <br>by intent", xref = "paper", yref = "paper",
        x = 1.02, xanchor = "left",
        y = 0.95, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.95, yanchor = "top"), margin = list(b = 100, l = 100)) %>% 
      config(displaylogo=F, collaborate = FALSE, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                                               "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                                               "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
})

  # Plot OD -----------------------------------------------------------------
  output$opioidPlotD <- renderPlotly({
    sub <- subset(df, subset = (age_group == input$ageOD & drug == "All opioids" & jurisdiction %in% input$stateOD &
                                  intent == input$codOD & sex %in% input$sexOD &
                                  (year >= input$yearsOD[[1]] & year <= input$yearsOD[[2]])))
    
    if (input$plotOD == "deaths") {
      p <- ggplot(sub) + aes(
        x = year, y = n, colour = jurisdiction, linetype = sex, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Intent: ", str_to_title(intent), 
          "<br>Jurisdiction: ", jurisdiction,
          "<br>Sex: ", sex
        )
      ) +
        geom_line() +
        scale_y_continuous(limits = c(0, NA)) +
        labs(x = "Year", y = "Number of deaths") +
        theme_light() + scale_colour_manual(values = statecols) +
        scale_linetype_manual(values = sextype) + theme(legend.title = element_blank())
    }
    
    else if (input$plotOD == "deathrateht") {
      p <- ggplot(sub) + aes(
        x = year, y = rate_ht, colour = jurisdiction, linetype = sex, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
          "<br>Intent: ", str_to_title(intent), 
          "<br>Jurisdiction: ", jurisdiction,
          "<br>Sex: ", sex
        )
      ) +
        geom_line() +
        scale_y_continuous(limits = c(0, NA)) +
        labs(x = "Year", y = "Deaths per 100,000") +
        theme_light() + scale_colour_manual(values = statecols) +
        scale_linetype_manual(values = sextype) + theme(legend.title = element_blank())
    }
    
    else if (input$plotOD == "deathratehtci") {
      p <- ggplot(sub) + aes(
        x = year, y = rate_ht, colour = jurisdiction, linetype = sex, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
          "<br>Intent: ", str_to_title(intent), 
          "<br>Jurisdiction: ", jurisdiction,
          "<br>Sex: ", sex
        )
      ) +
        geom_line() +
        geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0) +
        labs(x = "Year", y = "Deaths per 100,000") +
        theme_light() + scale_colour_manual(values = statecols) +
        scale_linetype_manual(values = sextype) + theme(legend.title = element_blank())
    }
    
    else if (input$plotOD == "deathratem") {
      p <- ggplot(sub) + aes(
        x = year, y = rate_m, colour = jurisdiction, linetype = sex, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
          "<br>Intent: ", str_to_title(intent), 
          "<br>Jurisdiction: ", jurisdiction,
          "<br>Sex: ", sex
        )
      ) +
        geom_line() +
        scale_y_continuous(limits = c(0, NA)) +
        labs(x = "Year", y = "Deaths per 1,000,000") +
        theme_light() + scale_colour_manual(values = statecols) +
        scale_linetype_manual(values = sextype) + theme(legend.title = element_blank())
    }
    
    else if (input$plotOD == "deathratemci") {
      p <- ggplot(sub) + aes(
        x = year, y = rate_m, colour = jurisdiction, linetype = sex, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
          "<br>Intent: ", str_to_title(intent), 
          "<br>Jurisdiction: ", jurisdiction,
          "<br>Sex: ", sex
        )
      ) +
        geom_line() +
        geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0) +
        labs(x = "Year", y = "Deaths per 1,000,000") +
        theme_light() + scale_colour_manual(values = statecols) +
        scale_linetype_manual(values = sextype) + theme(legend.title = element_blank())
    }
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Remove vertical gridlines
    p <- p + theme(panel.grid.minor.x = element_blank(),
                   panel.grid.major.x = element_blank())
    
    ggplotly(p, tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/opioid-amphetamine-and-cocaine-induced-deaths-australia-august-2018">DrugTrends</a>, NDARC',
        xref = "paper", yref = "paper",
        x = 0.01, xanchor = "left",
        y = 0.995, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "grey")
      ) %>%
      add_annotations(
        text = "Jurisdiction by sex", xref = "paper", yref = "paper",
        x = 1.02, xanchor = "left",
        y = 0.95, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.95, yanchor = "top"), margin = list(b = 100, l = 100)) %>% 
      config(displaylogo=F, collaborate = FALSE, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                                               "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                                               "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
  })
  
  
  
  # Amphetamine plot --------------------------------------------------------
  output$amphetaminePlot <- renderPlotly({
    sub <- subset(df, subset = (drug == "Amphetamines" & intent == input$codA & nature == input$natureA &
      age_group %in% input$ageA & sex == "All" & jurisdiction == "AUS" &
        (year >= input$yearsA[[1]] & year <= input$yearsA[[2]])))

    if (input$plotA == "deaths") {
      p <- ggplot(sub) + aes(
        x = year, y = n, colour = age_group, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Intent: ", str_to_title(intent),
          "<br>Nature: ", str_to_title(nature),
          "<br>Age group: ", age_group
        )
      ) +
        geom_line() +
        scale_y_continuous(limits = c(0, NA)) +
        labs(x = "Year", y = "Number of deaths") +
        theme_light() + scale_colour_manual(values = agecols) +
        theme(legend.title = element_blank())
    }

    else if (input$plotA == "deathrateht") {
      p <- ggplot(sub) + aes(
        x = year, y = rate_ht, colour = age_group, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
          "<br>Intent: ", str_to_title(intent),
          "<br>Nature: ", str_to_title(nature),
          "<br>Age group: ", age_group
        )
      ) +
        geom_line() +
        scale_y_continuous(limits = c(0, 5)) +
        labs(x = "Year", y = "Deaths per 100,000") +
        theme_light() + scale_colour_manual(values = agecols) +
        theme(legend.title = element_blank())
    }

    else if (input$plotA == "deathratehtci") {
      p <- ggplot(sub) + aes(
        x = year, y = rate_ht, colour = age_group, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
          "<br>Intent: ", str_to_title(intent),
          "<br>Nature: ", str_to_title(nature),
          "<br>Age group: ", age_group
        )
      ) +
        geom_line() +
        scale_y_continuous(limits = c(0, 5)) +
        geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0) +
        labs(x = "Year", y = "Deaths per 100,000") +
        theme_light() + scale_colour_manual(values = agecols) +
        theme(legend.title = element_blank())
    }

    else if (input$plotA == "deathratem") {
      p <- ggplot(sub) + aes(
        x = year, y = rate_m, colour = age_group, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
          "<br>Intent: ", str_to_title(intent),
          "<br>Nature: ", str_to_title(nature),
          "<br>Age group: ", age_group
        )
      ) +
        geom_line() +
        scale_y_continuous(limits = c(0, 50)) +
        labs(x = "Year", y = "Deaths per 1,000,000") +
        theme_light() + scale_colour_manual(values = agecols) +
        theme(legend.title = element_blank())
    }

    else if (input$plotA == "deathratemci") {
      p <- ggplot(sub) + aes(
        x = year, y = rate_m, colour = age_group, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
          "<br>Intent: ", str_to_title(intent),
          "<br>Nature: ", str_to_title(nature),
          "<br>Age group: ", age_group
        )
      ) +
        geom_line() +
        scale_y_continuous(limits = c(0, 50)) +
        geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0) +
        labs(x = "Year", y = "Deaths per 1,000,000") +
        theme_light() + scale_colour_manual(values = agecols) +
        theme(legend.title = element_blank())
    }

    validate(need(nrow(sub) > 0, "No data selected"))

    # Remove vertical gridlines
    p <- p + theme(panel.grid.minor.x = element_blank(),
                   panel.grid.major.x = element_blank())
    
    ggplotly(p, tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/opioid-amphetamine-and-cocaine-induced-deaths-australia-august-2018">DrugTrends</a>, NDARC',
        xref = "paper", yref = "paper",
        x = 0.01, xanchor = "left",
        y = 0.995, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "grey")
      ) %>%
      add_annotations(
        text = "Age", xref = "paper", yref = "paper",
        x = 1.02, xanchor = "left",
        y = 0.95, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.95, yanchor = "top"), margin = list(b = 100, l = 100)) %>% 
      config(displaylogo=F, collaborate = FALSE, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                                               "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                                               "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))  })


# Cocaine plot ------------------------------------------------------------
  output$cocainePlot <- renderPlotly({
    sub <- subset(df, subset = (drug == "Cocaine" & intent == input$codC & nature == input$natureC &
      age_group == input$ageC & sex == "All" & jurisdiction == "AUS" &
        (year >= input$yearsC[[1]] & year <= input$yearsC[[2]])))

    if (input$plotC == "deaths") {
      p <- ggplot(sub) + aes(
        x = year, y = n, colour=age_group, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Intent: ", str_to_title(intent),
          "<br>Nature: ", str_to_title(nature),
          "<br>Age group: ", age_group
        )
      ) +
        geom_line() +
        scale_y_continuous(limits = c(0, 100)) +
        labs(x = "Year", y = "Number of deaths") +
        theme_light() + scale_colour_manual(values = agecols) +
        theme(legend.title = element_blank())
    }

    else if (input$plotC == "deathrateht") {
      p <- ggplot(sub) + aes(
        x = year, y = rate_ht, colour=age_group, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
          "<br>Intent: ", str_to_title(intent),
          "<br>Nature: ", str_to_title(nature),
          "<br>Age group: ", age_group
        )
      ) +
        geom_line() +
        scale_y_continuous(limits = c(0, 5)) +
        labs(x = "Year", y = "Deaths per 100,000") +
        theme_light() + scale_colour_manual(values = agecols) +
        theme(legend.title = element_blank())
    }

    else if (input$plotC == "deathratehtci") {
      p <- ggplot(sub) + aes(
        x = year, y = rate_ht, colour=age_group, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
          "<br>Intent: ", str_to_title(intent),
          "<br>Nature: ", str_to_title(nature),
          "<br>Age group: ", age_group
        )
      ) +
        geom_line() +
        scale_y_continuous(limits = c(0, 5)) +
        geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0) +
        labs(x = "Year", y = "Deaths per 100,000") +
        theme_light() + scale_colour_manual(values = agecols) +
        theme(legend.title = element_blank())
    }

    else if (input$plotC == "deathratem") {
      p <- ggplot(sub) + aes(
        x = year, y = rate_m, colour=age_group, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
          "<br>Intent: ", str_to_title(intent),
          "<br>Nature: ", str_to_title(nature),
          "<br>Age group: ", age_group
        )
      ) +
        geom_line() +
        scale_y_continuous(limits = c(0, 50)) +
        labs(x = "Year", y = "Deaths per 1,000,000") +
        theme_light() + scale_colour_manual(values = agecols) +
        theme(legend.title = element_blank())
    }

    else if (input$plotC == "deathratemci") {
      p <- ggplot(sub) + aes(
        x = year, y = rate_m, colour=age_group, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
          "<br>Intent: ", str_to_title(intent),
          "<br>Nature: ", str_to_title(nature),
          "<br>Age group: ", age_group
        )
      ) +
        geom_line() +
        scale_y_continuous(limits = c(0, 50)) +
        geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0) +
        labs(x = "Year", y = "Deaths per 1,000,000") +
        theme_light() + scale_colour_manual(values = agecols) +
        theme(legend.title = element_blank())
    }

    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Remove vertical gridlines
    p <- p + theme(panel.grid.minor.x = element_blank(),
                   panel.grid.major.x = element_blank())
    
    ggplotly(p, tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/opioid-amphetamine-and-cocaine-induced-deaths-australia-august-2018">DrugTrends</a>, NDARC',
        xref = "paper", yref = "paper",
        x = 0.01, xanchor = "left",
        y = 0.995, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "grey")
      ) %>% 
      layout(margin = list(b = 100, l = 100), showlegend=FALSE) %>% 
      config(displaylogo=F, collaborate = FALSE, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                                               "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                                               "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
  })


# All drugs plot ----------------------------------------------------------
  output$drugPlot <- renderPlotly({
    sub <- subset(df, subset = (intent=="All" & nature=="Underlying" & age_group == input$ageD & sex == "All" & jurisdiction == "AUS" &
        (year >= input$yearsD[[1]] & year <= input$yearsD[[2]]))) %>%
      filter(drug %in% input$drugD)

    if (input$plotD == "deaths") {
      p <- ggplot(sub) + aes(
        x = year, y = n, colour = drug, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Nature: ", str_to_title(nature),
          "<br>Age group: ", age_group,
          "<br>Drug: ", drug
        )
      ) +
        geom_line() +
        labs(x = "Year", y = "Number of deaths") +
        theme_light() + scale_colour_manual(values = drugcols) +
        theme(legend.title = element_blank())
    }

    else if (input$plotD == "deathrateht") {
      p <- ggplot(sub) + aes(
        x = year, y = rate_ht, colour = drug, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
          "<br>Nature: ", str_to_title(nature),
          "<br>Age group: ", age_group,
          "<br>Drug: ", drug
        )
      ) +
        geom_line() +
        scale_y_continuous(limits = c(0, NA)) +
        labs(x = "Year", y = "Deaths per 100,000") +
        theme_light() + scale_colour_manual(values = drugcols) +
        theme(legend.title = element_blank())
    }

    else if (input$plotD == "deathratehtci") {
      p <- ggplot(sub) + aes(
        x = year, y = rate_ht, colour = drug, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_ht, 2), " (", round(rate_ht_lcl, 2), ", ", round(rate_ht_ucl, 2), ")",
          "<br>Nature: ", str_to_title(nature),
          "<br>Age group: ", age_group,
          "<br>Drug: ", drug
        )
      ) +
        geom_line() +
        geom_ribbon(aes(ymin = rate_ht_lcl, ymax = rate_ht_ucl), alpha = 0.1, size = 0) +
        labs(x = "Year", y = "Deaths per 100,000") +
        theme_light() + scale_colour_manual(values = drugcols) +
        theme(legend.title = element_blank())
    }

    else if (input$plotD == "deathratem") {
      p <- ggplot(sub) + aes(
        x = year, y = rate_m, colour = drug, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
          "<br>Nature: ", str_to_title(nature),
          "<br>Age group: ", age_group,
          "<br>Drug: ", drug
        )
      ) +
        geom_line() +
        scale_y_continuous(limits = c(0, NA)) +
        labs(x = "Year", y = "Deaths per 1,000,000") +
        theme_light() + scale_colour_manual(values = drugcols) +
        theme(legend.title = element_blank())
    }

    else if (input$plotD == "deathratemci") {
      p <- ggplot(sub) + aes(
        x = year, y = rate_m, colour = drug, group = 1,
        text = paste0(
          "Year: ", year,
          "<br>Deaths: ", n,
          "<br>Rate: ", round(rate_m, 2), " (", round(rate_m_lcl, 2), ", ", round(rate_m_ucl, 2), ")",
          "<br>Nature: ", str_to_title(nature),
          "<br>Age group: ", age_group,
          "<br>Drug: ", drug
        )
      ) +
        geom_line() +
        geom_ribbon(aes(ymin = rate_m_lcl, ymax = rate_m_ucl), alpha = 0.1, size = 0) +
        labs(x = "Year", y = "Deaths per 1,000,000") +
        theme_light() + scale_colour_manual(values = drugcols) +
        theme(legend.title = element_blank())
    }

    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Remove vertical gridlines
    p <- p + theme(panel.grid.minor.x = element_blank(),
                   panel.grid.major.x = element_blank())

    ggplotly(p, tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/opioid-amphetamine-and-cocaine-induced-deaths-australia-august-2018">DrugTrends</a>, NDARC',
        xref = "paper", yref = "paper",
        x = 0.01, xanchor = "left",
        y = 0.995, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "grey")
      ) %>% 
      add_annotations(
        text = "Drug", xref = "paper", yref = "paper",
        x = 1.02, xanchor = "left",
        y = 0.95, yanchor = "bottom", # Same y as legend below
        legendtitle = TRUE, showarrow = FALSE
      ) %>%
      layout(legend = list(y = 0.95, yanchor = "top"), margin = list(b = 100, l = 100)) %>% 
      config(displaylogo=F, collaborate = FALSE, modeBarButtonsToRemove = list("sendDataToCloud","zoom2d","pan2d","select2d","lasso2d",
                                                                               "zoomIn2d","zoomOut2d","autoScale2d","hoverClosestCartesian",
                                                                               "hoverCompareCartesian", "resetScale2d", "toggleSpikelines"))
  })
  
  # Opioids with other drugs  -----------------------------------------------------------------
  output$PlotOE <- renderPlotly({
    sub <- filter(df, word(drug, start = 1, end = 3) == "All opioids with" & jurisdiction == "AUS" &
                    drug %in% input$drugOE & intent %in% input$intentOE & age_group %in% input$ageOE & 
                    (year >= input$yearsOE[[1]] & year <= input$yearsOE[[2]]) & sex==input$sexOE)
    
    
    if (input$plotOE == "deaths") {
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
    
    if (input$plotOE == "deathrateht") {
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
    
    if (input$plotOE == "deathratehtci") {
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
    
    if (input$plotOE == "deathratem") {
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
    
    if (input$plotOE == "deathratemci") {
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
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Remove vertical gridlines
    p <- p + theme(panel.grid.minor.x = element_blank(),
                   panel.grid.major.x = element_blank())
    
    ggplotly(p, tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/opioid-amphetamine-and-cocaine-induced-deaths-australia-august-2018">DrugTrends</a>, NDARC',
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
  
  # Opioids and other drugs by sex ------------------------------------------
  output$PlotOF <- renderPlotly({
    sub <- filter(df, word(drug, start = 1, end = 3) == "All opioids with" & jurisdiction == "AUS" &
                    drug %in% input$drugOF & intent == input$intentOF & age_group == input$ageOF & 
                    (year >= input$yearsOF[[1]] & year <= input$yearsOF[[2]]) & sex %in% input$sexOF)
    
    
    if (input$plotOF == "deaths") {
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
    
    if (input$plotOF == "deathrateht") {
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
    
    if (input$plotOF == "deathratehtci") {
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
    
    if (input$plotOF == "deathratem") {
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
    
    if (input$plotOF == "deathratemci") {
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
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Remove vertical gridlines
    p <- p + theme(panel.grid.minor.x = element_blank(),
                   panel.grid.major.x = element_blank())
    
    ggplotly(p, tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/opioid-amphetamine-and-cocaine-induced-deaths-australia-august-2018">DrugTrends</a>, NDARC',
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
  
  # Exclusive opioids ------------------------------------------
  output$PlotOG <- renderPlotly({
    sub <- filter(df, jurisdiction == "AUS" &
                    drug %in% input$drugOG & intent == input$intentOG & age_group == input$ageOG & 
                    (year >= input$yearsOG[[1]] & year <= input$yearsOG[[2]]) & sex %in% input$sexOG)
    
    
    if (input$plotOG == "deaths") {
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
    
    if (input$plotOG == "deathrateht") {
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
    
    if (input$plotOG == "deathratehtci") {
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
    
    if (input$plotOG == "deathratem") {
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
    
    if (input$plotOG == "deathratemci") {
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
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Remove vertical gridlines
    p <- p + theme(panel.grid.minor.x = element_blank(),
                   panel.grid.major.x = element_blank())

    ggplotly(p, tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/opioid-amphetamine-and-cocaine-induced-deaths-australia-august-2018">DrugTrends</a>, NDARC',
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
  
  # Exclusive opioids as percents ------------------------------------------
  output$PlotOH <- renderPlotly({
    sub <- filter(df, drug %in% c( "Exclusive illicit opioids",
                                   "Exclusive pharmaceutical opioids",
                                   "Illicit and pharmaceutical opioids",
                                   "Unspecified opioids") &
                    intent == input$intentOH & 
                    age_group == input$ageOH & 
                    (year >= input$yearsOH[[1]] & year <= input$yearsOH[[2]]) & 
                    sex == input$sexOH) %>% 
      group_by(year, intent, nature, sex, jurisdiction, age_group) %>% 
      mutate(alldeaths = sum(n),
             percent = round(n/sum(n)*100, 2),
             drug = factor(drug, levels = c( "Unspecified opioids",
                                             "Illicit and pharmaceutical opioids",
                                             "Exclusive pharmaceutical opioids",
                                             "Exclusive illicit opioids"
                                             )))

    p <- ggplot(sub, aes(x=year, y=percent, fill=drug, group=1, text = paste0(
      "Year: ", year,
      "<br>Deaths: ", n,
      "<br>Percent: ", percent, "%",
      "<br>Drug: ", str_to_title(drug),
      "<br>Intent: ", str_to_title(intent),
      "<br>Sex: ", sex
    ))) +
      geom_area() +
      labs(x = "Year", y = "Percent of opioid induced deaths") +
      theme_light() + 
      theme(legend.title = element_blank()) + 
      scale_fill_brewer(palette = "PuBu")
    
    validate(need(nrow(sub) > 0, "No data selected"))
    
    # Remove vertical gridlines
    p <- p + theme(panel.grid.minor.x = element_blank(),
                   panel.grid.major.x = element_blank())

    ggplotly(p,  tooltip = "text") %>%
      add_annotations(
        text = 'Source: <a href="https://ndarc.med.unsw.edu.au/resource/opioid-amphetamine-and-cocaine-induced-deaths-australia-august-2018">DrugTrends</a>, NDARC',
        xref = "paper", yref = "paper",
        x = 0.01, xanchor = "left",
        y = 0.995, yanchor = "top",
        showarrow = F, font = list(size = 10, color = "grey")
      ) %>%
      add_annotations(
        text = "Drug", xref = "paper", yref = "paper",
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
