# Load libraries
library(shiny)
library(shinythemes)
library(tidyverse)
library(ggthemes)
library(janitor)
library(DT)

angler_data <- read_csv("angler_data.csv") %>% 
  clean_names() %>%
  mutate(body = case_when(
    str_detect(body_shape_i, "short") ~ "short/deep",
    str_detect(body_shape_i, "elongated") ~ "elongated",
    str_detect(body_shape_i, "fusiform") ~ "fusiform/normal",
    str_detect(body_shape_i, "other") ~ "other"
  )) %>% 
  select(!c(body_shape_i)) %>% 
  mutate_at(c("subfamily", "aquarium", "dangerous", "body", "genus"), as.factor) %>% 
  drop_na(c(depth_range_shallow, depth_range_deep)) %>% 
  rowwise() %>%
  mutate(avg_depth = mean(c(depth_range_shallow, depth_range_deep)))

# User interface
ui <- fluidPage(
  theme = shinytheme("superhero"),
  titlePanel(title = "Splish splash, it's a fish dash(board)!"),
  
  sidebarLayout(
    sidebarPanel(
      tags$div(
        HTML(paste0(
          "The ",
          tags$a(href="https://www.fishbase.de/summary/OrdersSummary.php?order=Lophiiformes", "Lophiiformes order"),
          " includes (but is not limited to) ",
          tags$a(href="https://en.wikipedia.org/wiki/Goosefish", "goosefish"),
          ", ",
          tags$a(href="https://en.wikipedia.org/wiki/Frogfish", "frogfish"),
          ", ",
          tags$a(href="https://en.wikipedia.org/wiki/Ceratiidae", "sea devils"),
          ", and other ",
          tags$a(href="https://en.wikipedia.org/wiki/Anglerfish#Classification", "deep-sea anglers"),
          "."
        )),
        "Use this dashboard to explore information about their scientific classifications, living conditions, anatomical information, and vulnerability. 
        Note that this list is not comprehensive, as only species with both a reported shallow and deep depth range were included."
      ),
      tags$hr(),
      tags$p("You can select a depth range with the slider below. 
             This will filter the data based on the shallow depth range (minimum value) and deep depth range (maximum value) for each species."),
      tags$p("You can also select the variable of interest to group the plot using the radio buttons below."),
      tags$p("The data generally have greater distribution among the categories at shallower depths; keep this in mind when setting depth range."),
      sliderInput("depth_range",
                  label = h3("Depth range (meters)"),
                  post = " m",
                  step = 1,
                  min = min((angler_data %>% drop_na(depth_range_shallow))$depth_range_shallow),
                  max = max((angler_data %>% drop_na(depth_range_deep))$depth_range_deep),
                  value = c(0, 2000)),
      htmlOutput("depth_info"),
      radioButtons("var_of_interest",
                   label = h3("Variable of interest:"),
                   choices = list(
                     "Body shape" = "body",
                     "Subfamily" = "subfamily",
                     "Threat to humans" = "dangerous",
                     "Kept in aquariums" = "aquarium"
                   ),
                   selected = "body"),
      tags$hr(),
      tags$div("Here are some prompts to consider exploring!",
               tags$ul(
                 tags$li("Does there appear to be relationships between vulnerability and a variable of interest? Do these relationships become more apparent at certain depths?"),
                 tags$li("At which depth range do the species dangerous to humans reside?"),
                 tags$li("Which species have the largest reported deep depth range?"),
                 tags$li("How many species in this dataset have the word 'devil' in their common name, and are they all part of the same genus?")
               )),
      tags$hr(),
      tags$div(
        "This dashboard was created by",
        tags$a(href="https://github.com/gmcginnis", "Gillian McGinnis"),
        "in April 2021.",
        "Data was taken via the",
        tags$a(href="http://fishbase.org/", "FishBase"),
        "API in March 2021, using the",
        tags$a(href = "https://github.com/ropensci/rfishbase", tags$code("rfishbase")),
        "package."
      )
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel(
                    "Plot",
                    plotOutput("graph"),
                    tags$p(
                      "'NA's on the graph indicate that there is no data recorded for that variable in FishBase. They are still included in the graph to get an idea for depth distribution.",
                      "Consider looking at the table (accessible via the tabs above) to see details about each species (data point) plotted."
                    )
                  ),
                  tabPanel(
                    "Table",
                    tags$p("Table is filtered to the selected depth range. You can further subset the depth data (and other variables) using the available column filters."),
                    dataTableOutput("table")
                  )
      )
    )
  )
)

# Server function
server <- function(input, output){
  
  angler_wrangled <- reactive({
    angler_data %>%
      filter(depth_range_shallow >= input$depth_range[1],
             depth_range_deep <= input$depth_range[2])
  })
  
  output$graph <- renderPlot({
    ggplot(angler_wrangled(), aes(x = .data[[input$var_of_interest]],
                                  y = avg_depth,
                                  color = vulnerability,
                                  ymin = depth_range_shallow,
                                  ymax = depth_range_deep)) +
      geom_pointrange(
        position = position_jitter(),
        linetype = "dotted",
        alpha = 0.75,
        shape = 1
      ) +
      theme_hc(style = "darkunica") +
      scale_color_distiller(palette = "Spectral", limits = c(0, 100)) +
      scale_y_reverse() +
      scale_x_discrete(position = "top") +
      theme(
        legend.position = "top",
        legend.justification = "left",
        legend.box = "vertical",
        axis.text.x = element_text(color = "gray", size = 12),
        plot.background = element_rect(fill = "#002246"),
        legend.background = element_rect(fill = NA)
      ) +
      labs(
        title = "Depth ranges (with circled averages) and vulnerability for select Lophiiformes, grouped by a selected variable of interest.",
        subtitle = paste("Grouping variable:", input$var_of_interest),
        x = "",
        y = "Depth (m)",
        caption = "Data from FishBase\nPlot code by Gillian McGinnis",
        color = "Vulnerability: "
      )
  })
  
  output$table <- renderDataTable({
    datatable(
      style = "bootstrap",
      filter = list(position = "top", plain = TRUE),
      angler_wrangled() %>%
        select(
          species, f_bname, genus, subfamily,
          body, length,
          depth_range_shallow, depth_range_deep, avg_depth,
          vulnerability,
          everything()
        ) %>% 
        clean_names(case = "title") %>% 
        rename(
          "Common Name" = "F Bname",
          "Deep depth range (m)" = "Depth Range Deep",
          "Shallow depth range (m)" = "Depth Range Shallow",
          "Calculated mean depth (m)" = "Avg Depth",
          "Length (cm)" = "Length",
          "Threat to humans" = "Dangerous"
        )
    )
  })
  
  output$depth_info <- renderText({
    paste(
      "Based on the selected depth range, you are viewing information for <b>",
      nrow(angler_wrangled()),
      "species</b>, out of",
      nrow(angler_data),
      "total."
    )
  })
  
}

# Creates app
shinyApp(ui = ui, server = server)