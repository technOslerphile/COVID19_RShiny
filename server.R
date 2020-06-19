library("shiny")
library("shinyjs")
library("ggplot2")
library("dplyr")
library("reshape2")
options(scipen = 999)


URL1 <- 'https://covid.ourworldindata.org/data/ecdc/total_cases_per_million.csv'
URL2 <- 'https://covid.ourworldindata.org/data/ecdc/total_deaths_per_million.csv'
URL3 <- 'https://covid.ourworldindata.org/data/ecdc/new_cases_per_million.csv'
URL4 <- 'https://covid.ourworldindata.org/data/ecdc/new_deaths_per_million.csv'
URL5 <- 'https://covid.ourworldindata.org/data/ecdc/full_data.csv'

fullData <- read.csv(URL5)
fullData$date <- as.Date(fullData$date)
colnames(fullData)[2] <- "Country"
unadjusted <- fullData %>% arrange(Country, date)

daCases <- read.csv(URL3)
taCases <- read.csv(URL1)
daDeaths <- read.csv(URL4)
taDeaths <- read.csv(URL2)

d1 <- melt(daCases, id.vars = c("date"), variable.name = "Country", value.name = "new_cases_per_million")
d2 <- melt(daDeaths, id.vars = c("date"), variable.name = "Country", value.name = "new_deaths_per_million")
d3 <- melt(taCases, id.vars = c("date"), variable.name = "Country", value.name = "total_cases_per_million")
d4 <- melt(taDeaths, id.vars = c("date"), variable.name = "Country", value.name = "total_deaths_per_million")

d <- d1 %>% inner_join(d2) %>% inner_join(d3) %>% inner_join(d4)
d$Country <- gsub(".", " ", d$Country, fixed=TRUE)
adjusted <- d %>% filter(Country %in% unadjusted$Country)
adjusted$date <- as.Date(adjusted$date)
unadjusted <- unadjusted %>% filter(Country %in% adjusted$Country)
rm(d1,d2,d3,d4, daCases, taCases, daDeaths, taDeaths, fullData, d)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  observeEvent(input$reset_input, {
    shinyjs::reset("submissiontemplate")
  })
  
  
  return_theme <- reactive({
    if (input$plotstyle){
    mytheme <- theme(axis.text.y = element_text(face="bold", color="#000000",
                                                              size=10, angle=0),
                                   axis.text.x = element_text(face="bold", color="#000000",
                                                    size=10, angle=0),
                                   axis.title.x = element_text(size = 11, face="bold"),
                                   axis.title.y = element_text(size = 11, face="bold"),
                                   plot.title = element_text(face="bold", size = 15, hjust = 0.5),
                                   plot.subtitle = element_text(size = 12),
                                   plot.background = element_rect(fill = input$plotbg, colour = input$plotborder, 
                                                                  size = input$plotborderlinesize,
                                                                  linetype = input$plotborderlinetype),
                                   panel.background = element_rect(fill = input$panelbg, colour = input$panelborder, 
                                                                   size = input$panellinesize, 
                                                                   linetype = input$panellinetype),
                                   panel.grid.major = element_line(size = 0.1, linetype = input$majorgridlinetype, 
                                                                   colour = input$majorgridcolor), 
                                   panel.grid.minor = element_line(size = 0.1, linetype = input$minorgridlinetype, 
                                                                   colour = input$minorgridcolor))
    } else{
      print("no plot style selected...")
      mytheme <- theme(axis.text.y = element_text(face="bold", color="#000000",
                                                  size=10, angle=0),
                       axis.text.x = element_text(face="bold", color="#000000",
                                                  size=10, angle=0),
                       axis.title.x = element_text(size = 11, face="bold"),
                       axis.title.y = element_text(size = 11, face="bold"),
                       plot.title = element_text(face="bold", size = 15, hjust = 0.5),
                       plot.subtitle = element_text(size = 12),
                       plot.background = element_rect(fill = "lightcyan2", colour = "black"),
                       panel.background = element_rect(fill = "white", colour = "black", size = 0.1, linetype = "solid"),
                       panel.grid.major = element_line(size = 0.1, linetype = 'dotted', colour = "gray50"), 
                       panel.grid.minor = element_line(size = 0.1, linetype = 'dotted', colour = "gray50"))
      
      
    }
    
    return(mytheme)
    })
  
  
  
  filter_data <- reactive({
    selectedCountries <- as.character(input$countries)
    if (input$daysince){
      indexDates <- (unadjusted %>% filter(Country %in% selectedCountries) %>% group_by(Country) %>%
                       filter(total_cases >= input$nCasesStart) %>%
                       mutate(index_date = min(date)) %>%
                       select(Country, index_date) %>% distinct())
      t1 <- (unadjusted %>% inner_join(indexDates) %>% filter(date >= index_date) %>% 
               group_by(Country) %>% mutate(date = 1:length(index_date)) %>% ungroup())#unadjusted data filtered for day since N cum cases
      t2 <- adjusted %>% inner_join(indexDates) %>% filter(date >= index_date) #adjusted data data filtered day since N cum cases
      #xlabtitle <- paste0("Day since ", input$nCasesStart, " cases") 
      
      }else if (!input$daysince){
        t1 <- unadjusted %>% filter(Country %in% selectedCountries) #unadjusted data 
        t2 <- adjusted %>% filter(Country %in% selectedCountries)#adjusted data
        #xlabtitle <- "Date"
      }
    
    if (input$adjust){
      plot_data <- (t2 %>% select(Country, date, total_cases_per_million, total_deaths_per_million,
                                     new_cases_per_million, new_deaths_per_million) %>%
                      rename(total_cases = total_cases_per_million,
                             total_deaths = total_deaths_per_million,
                             new_cases = new_cases_per_million,
                             new_deaths = new_deaths_per_million))
    }else if (!input$adjust){
      plot_data <- t1 %>% select(Country, date, total_cases, total_deaths, new_cases, new_deaths)
    }
    return(plot_data) #returns either the adjusted or the unadjusted data filtered for dates (day since N) as per user selection
    })
  

  
  output$countries <- renderPlot({
    
    data_to_plot <- filter_data()
    if (input$adjust) plotTitle <- "COVID-19 Confirmed Cases: Adjusted for population" else plotTitle <- "COVID-19 Confirmed Cases"
    if (input$adjust) ylabTitle <- "Cumulative cases per million" else ylabTitle <- "Cumulative cases"
    if (input$daysince) xlabTitle <- paste0("Day since ", input$nCasesStart, " cases") else xlabTitle <- "Date"
    p <- (ggplot(data=data_to_plot, aes(x= date, y=total_cases, group=Country)))
    if (input$smoothen) p<- p + geom_smooth(aes(color=Country),method="loess", se=F, lwd=1) else p <- p  + geom_line(aes(color=Country), lwd=1)
    p <- (p + ggtitle(plotTitle) +
            ylab(ylabTitle) + xlab(xlabTitle) + return_theme())
    if (input$log) p + scale_y_log10() else p

  })
  
  output$deaths <- renderPlot({
    data_to_plot <- filter_data()
    if (input$adjust) plotTitle <- "COVID-19 Deaths: Adjusted for population" else plotTitle <- "COVID-19 Deaths" 
    if (input$adjust) ylabTitle <- "Cumulative deaths per million" else ylabTitle <- "Cumulative deaths" 
    if (input$daysince) xlabTitle <- paste0("Day since ", input$nCasesStart, " cases") else xlabTitle <- "Date" 
    p <- (ggplot(data=data_to_plot, aes(x= date, y=total_deaths, group=Country))) 
    if (input$smoothen) p<- p + geom_smooth(aes(color=Country),method="loess", se=F, lwd=1) else p <- p  + geom_line(aes(color=Country), lwd=1)
    p <- (p + ggtitle(plotTitle) + 
            ylab(ylabTitle) + xlab(xlabTitle) + return_theme())
    if (input$log) p + scale_y_log10() else p
    })
  
  
  output$countries2 <- renderPlot({
    data_to_plot <- filter_data()
    if (input$adjust) plotTitle <- "COVID-19 Confirmed Cases: Adjusted for population" else plotTitle <- "COVID-19 Confirmed Cases" 
    if (input$adjust) ylabTitle <- "Daily cases per million" else ylabTitle <- "Daily cases" 
    if (input$daysince) xlabTitle <- paste0("Day since ", input$nCasesStart, " cases") else xlabTitle <- "Date" 
    p <- (ggplot(data=data_to_plot, aes(x= date, y=new_cases, group=Country))) 
    if (input$smoothen) p<- p + geom_smooth(aes(color=Country),method="loess", se=F, lwd=1) else p <- p  + geom_line(aes(color=Country), lwd=1)
    p <- (p + ggtitle(plotTitle) + 
            ylab(ylabTitle) + xlab(xlabTitle) + return_theme())
    if (input$log) p + scale_y_log10() else p
  })
  
  output$deaths2 <- renderPlot({
    data_to_plot <- filter_data()
    if (input$adjust) plotTitle <- "COVID-19 Deaths: Adjusted for population" else plotTitle <- "COVID-19 Deaths" 
    if (input$adjust) ylabTitle <- "Daily deaths per million" else ylabTitle <- "Daily deaths" 
    if (input$daysince) xlabTitle <- paste0("Day since ", input$nCasesStart, " cases") else xlabTitle <- "Date" 
    p <- (ggplot(data=data_to_plot, aes(x= date, y=new_deaths, group=Country))) 
    if (input$smoothen) p<- p + geom_smooth(aes(color=Country),method="loess", se=F, lwd=1) else p <- p  + geom_line(aes(color=Country), lwd=1)
    p <- (p + ggtitle(plotTitle) + 
            ylab(ylabTitle) + xlab(xlabTitle) + return_theme())
    if (input$log) p + scale_y_log10() else p
  })
  

  output$table1 <- DT::renderDataTable({
    if (input$adjust) df <- adjusted else df <- unadjusted
    if (input$adjust){
      df <- (df %>% rename(total_cases = total_cases_per_million,
                          total_deaths = total_deaths_per_million,
                          new_cases = new_cases_per_million,
                          new_deaths = new_deaths_per_million))
    }
  
    if (input$adjust) tableTitle <- 'Table: Countries ordered by burden of total COVID-19 cases/million' else tableTitle <- 'Table: Countries ordered by their burden of total COVID-19 cases'
    tableTitle <- paste0(tableTitle, "\n", "(as of ", format.Date(max(df$date, na.rm = TRUE), "%d-%b"), ")")
    
    top10 <- (df %>% group_by(Country) %>% mutate(MAXN = round(max(total_cases, na.rm = TRUE),0)) %>%
                arrange(desc(date)) %>%
                mutate(latest_daily_N = round(new_cases[1])) %>%
                select(Country, MAXN, latest_daily_N) %>% distinct() %>% arrange(desc(MAXN)) %>% filter(Country != 'World'))
   
    top10$Rank <- as.integer(row.names(top10))
    colnames(top10) <- c("Country", "Cases", "Latest Daily Cases", "Rank")
    top10 <- top10 %>% select(Country, Cases, 'Latest Daily Cases')
    datatable(top10, class="compact cell-border", rownames = FALSE, options = list(autoWidth=FALSE, scrollX = FALSE, 
                                    columnDefs = list(list(width = '10px', targets = "_all"    ))),
              caption = tableTitle)
  })
  
  output$table2 <- DT::renderDataTable({
    if (input$adjust) df <- adjusted else df <- unadjusted
    if (input$adjust){
      df <- (df %>% rename(total_cases = total_cases_per_million,
                           total_deaths = total_deaths_per_million,
                           new_cases = new_cases_per_million,
                           new_deaths = new_deaths_per_million))
    }
    if (input$adjust) tableTitle <- 'Table: Countries ordered by burden of total COVID-19 deaths/million' else tableTitle <- 'Table: Countries ordered by their burden of total COVID-19 deaths'
    tableTitle <- paste0(tableTitle, "\n", "(as of ", format.Date(max(df$date, na.rm = TRUE), "%d-%b"), ")")
    
    top10 <- (df %>% group_by(Country) %>% mutate(MAXN = round(max(total_deaths, na.rm = TRUE),0)) %>% 
                arrange(desc(date)) %>%
                mutate(latest_daily_N = round(new_deaths[1])) %>%
                select(Country, MAXN, latest_daily_N) %>% distinct() %>% arrange(desc(MAXN)) %>% filter(Country != 'World'))
    
    top10$Rank <- as.integer(row.names(top10))
    colnames(top10) <- c("Country", "Deaths", "Latest Daily Deaths", "Rank")
    top10 <- top10 %>% select(Country, Deaths, 'Latest Daily Deaths')
    
    
    datatable(top10, class="compact cell-border", rownames = FALSE, 
              options = list(autoWidth=FALSE, scrollX = FALSE, 
                             columnDefs = list(list(width = '10px', targets = "_all"    ))),
              caption = tableTitle)
  })
  
  
    output$`Data Source` <- renderText({
      print("Data source")
    })
    
    output$url <- renderUI(a(href=paste0('https://covid.ourworldindata.org/'),"Our World in Data for COVID-19",target="_blank"))
  
    
    output$description <- renderText({
      print("The data is sourced directly from the OurWorldInData servers in real time and hence the data for this dashboard gets updated automatically")
    })
    
    
  })