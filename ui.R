library("shiny")
library("shinyjs")
library("shinycssloaders")
library("DT")
library("colourpicker")
options(scipen=999)
URL <- 'https://covid.ourworldindata.org/data/ecdc/full_data.csv'
all <- unique(as.character(tryCatch(read.csv(URL), error = function(e) NULL)$location))
countries <- all[!all %in% c("World","International")]

shinyUI(fluidPage(
  
  # Application title
  titlePanel("COVID19: Trends in Countries"),
  
  sidebarLayout(position='left',
                 sidebarPanel(width=2,
                              useShinyjs(),
                              div(id="submissiontemplate",
                                  selectizeInput("countries", "Select countries you wish to plot", 
                                                 choices = countries,
                                                 selected = "India", multiple = TRUE,
                                                 options = list(plugins= list('remove_button'),
                                                                maxItems=25)),
                                  checkboxInput("smoothen", "Smoothen the plot", TRUE),
                                  checkboxInput("adjust", "Adjust for population", FALSE),
                                  checkboxInput("log", "Plot in log scale", FALSE),
                                  checkboxInput("daysince", "Plot from 'day since N' confirmed cases", FALSE),
                                  conditionalPanel(
                                    condition = "input.daysince == true",
                                    sliderInput("nCasesStart", "Choose N (number of COVID-19 cases) to plot from the day when N confirmed cases got detected", 
                                                value = 100, min = 0, max = 1000, step=20)
                                  ),
                                  checkboxInput("plotstyle", "Change Plot Styles", FALSE),
                                  conditionalPanel(
                                    condition = "input.plotstyle == true",
                                    colourInput("panelbg", "Panel background fill color", "white")
                                  ),
                                  conditionalPanel(
                                    condition = "input.plotstyle == true",
                                    colourInput("plotbg",  "Plot background fill color", "lightcyan2")
                                  ),
                                  
                                  conditionalPanel(
                                    condition = "input.plotstyle == true",
                                    colourInput("panelborder", "Panel background border color", "black")
                                  ),
                                  conditionalPanel(
                                    condition = "input.plotstyle == true",
                                    sliderInput("panellinesize", "Panel background line size (width)", 
                                                value = 0.1, min = 0, max = 2, step=0.1)
                                  ),
                                  
                                  
                                  conditionalPanel(
                                    condition = "input.plotstyle == true",
                                    colourInput("plotborder", "Plot background border color", "black")
                                  ),
                                  
                                  conditionalPanel(
                                    condition = "input.plotstyle == true",
                                    colourInput("majorgridcolor", "Major gridline color", "gray50")
                                  ),
                                  conditionalPanel(
                                    condition = "input.plotstyle == true",
                                    colourInput("minorgridcolor", "Minor gridline color", "gray50")
                                  ),
                                 
                                  conditionalPanel(
                                    condition = "input.plotstyle == true",
                                    selectizeInput("panellinetype", "Panel line type", 
                                                   choices = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
                                                   multiple = FALSE, selected = "solid")
                                  ),
                                  conditionalPanel(
                                    condition = "input.plotstyle == true",
                                    sliderInput("plotborderlinesize", "Panel background line size (width)", 
                                                value = 0.1, min = 0, max = 2, step=0.1)
                                  ),
                                  conditionalPanel(
                                    condition = "input.plotstyle == true",
                                    selectizeInput("plotborderlinetype", "Panel line type", 
                                                   choices = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
                                                   multiple = FALSE, selected = "solid")
                                  ),
                                  
                                  conditionalPanel(
                                    condition = "input.plotstyle == true",
                                    selectizeInput("majorgridlinetype", "Line type for major grids", 
                                                   choices = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
                                                   multiple = FALSE, selected = "dotted")
                                  ),
                                  conditionalPanel(
                                    condition = "input.plotstyle == true",
                                    selectizeInput("minorgridlinetype", "Line type for minor grids", 
                                                   choices = c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
                                                   multiple = FALSE, selected = "dotted")
                                  ),
                                  
                                  
                                  
                                  
                                  
                                  actionButton("reset_input", "Reset to default"))
                              
                              
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                   
                   tabsetPanel(type = "tabs",
                               tabPanel("Cumulative Trends", 
                                        plotOutput("countries", height=350) %>% withSpinner(color="#0dc5c1"),
                                        plotOutput("deaths", height = 350) %>% withSpinner(color="#0dc5c1")
                               ),
                               tabPanel("Daily Trends",
                                        plotOutput("countries2", height = 350) %>% withSpinner(color="#0dc5c1"),
                                        plotOutput("deaths2", height = 350) %>% withSpinner(color="#0dc5c1")
                               ),
                               tabPanel("Table", 
                                        splitLayout(DT::dataTableOutput("table1", width = 300) %>% withSpinner(color="#0dc5c1"),
                                                    DT::dataTableOutput("table2", width = 300) %>% withSpinner(color="#0dc5c1"))
                                        
                               )),
                   #downloadButton('download1', 'Download the graphs'),
                   br(),
                   h5(textOutput("Data Source")),
                   uiOutput("url"),
                   h6(textOutput("description"))
                 )
                 
  )
)
)
