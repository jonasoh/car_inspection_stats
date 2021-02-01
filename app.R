library(shiny)
library(shinydashboard)
library(DT)

library(data.table)
library(janitor)
library(stringr)
#library(ggplot2)
library(dplyr)
library(tidyr)

# load and clean data
stats <- fread('grouped_stats_per_car.csv')
stats <- stats[`Number of inspections` > 0
               ][, `Number of inspections` := as.integer(`Number of inspections`)
               ][, `Demand for repairs (number of faults)` := as.integer(`Demand for repairs (number of faults)`)
               ][, `Rejections (number of faults)` := as.integer(`Rejections (number of faults)`)
               ][, `Driving bans (number of faults)` := as.integer(`Driving bans (number of faults)`)
               ][, `Average mileage` := as.integer(`Average mileage`)
               ][, `Median mileage` := as.integer(`Median mileage`)
               ][, `Year of inspection` := as.integer(`Year of inspection`)
               ][, `Registration year` := as.integer(`Registration year`)
               ][, `Vehicle age` := as.integer(`Year of inspection`) - `Registration year`
               ][, fault_pct := (`Demand for repairs (number of faults)` + `Rejections (number of faults)` +
                     `Driving bans (number of faults)`) / `Number of inspections`
               ][, brand := str_match(`Brand and model series`, '^(.*) - ')[,2]
               ][, make := str_match(`Brand and model series`, ' - (.*)$')[,2]
               ][, `Brand and model series` := str_replace(`Brand and model series`, fixed(' - '), ' ')]

stats <- clean_names(stats)
names(stats)[4] <- 'main_fault_object'
names(stats)[8:10] <- c('demand_for_repairs', 'rejections', 'driving_bans')

fault_categories <- sort(unique(stats$main_fault_object))
model_related <- c("Axles, wheels and suspension (all objects)", 
                   "Chassis and body (all objects)",
                   "Brake systems (all objects)",
                   "Steering equipment (all objects)",
                   "Environmental hazards (all objects)")
years <- sort(unique(stats$registration_year))
ages <- sort(unique(stats$vehicle_age))
cars <- sort(unique(stats$brand_and_model_series))

stats_model_year <- stats[main_fault_object %in% model_related, 
                          .(fault_pct=sum(c(demand_for_repairs, rejections, driving_bans))/number_of_inspections[1],
                            average_mileage=average_mileage[1],
                            number_of_inspections=number_of_inspections[1]), 
                          by=.(year_of_inspection, brand_and_model_series, registration_year)
                          ][, .(fault_pct=round(weighted.mean(fault_pct, number_of_inspections), 3),
                                average_mileage=as.integer(weighted.mean(average_mileage, number_of_inspections)),
                                number_of_inspections=sum(number_of_inspections)), 
                                by=.(brand_and_model_series, registration_year)]

stats_by_fault <- stats[, .(fault_pct=sum(c(demand_for_repairs, rejections, driving_bans))/number_of_inspections[1],
                            number_of_inspections=number_of_inspections[1]),
                          by=.(year_of_inspection, brand_and_model_series, registration_year, main_fault_object)]

avg_stats_by_fault <- stats_by_fault[, .(fault_pct=weighted.mean(fault_pct, number_of_inspections),
                                         number_of_inspections=sum(number_of_inspections)),
                                     by=.(year_of_inspection, registration_year, main_fault_object)]


stats_age <- stats[main_fault_object %in% model_related, 
                   .(fault_pct=sum(c(demand_for_repairs, rejections, driving_bans))/number_of_inspections[1],
                     average_mileage=average_mileage[1],
                     number_of_inspections=number_of_inspections[1]), 
                   by=.(year_of_inspection, brand_and_model_series, vehicle_age)
                    ][, .(fault_pct=round(weighted.mean(fault_pct, number_of_inspections), 3),
                          average_mileage=as.integer(weighted.mean(average_mileage, number_of_inspections)),
                          number_of_inspections=sum(number_of_inspections)), 
                      by=.(brand_and_model_series, vehicle_age)]

# dashboard ui
header <- dashboardHeader(title="Car inspection statistics")

sidebar <- dashboardSidebar(
    sidebarMenu(id="sidebar", 
        menuItem("By model and year", tabName="model_year", icon=icon("car")),
        menuItem("By age", tabName="by_age", icon=icon("car")),
        menuItem("Car model overview", tabName="model_overview", icon=icon("car"))
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName='model_year',fluidRow(
            box('Fault stats by model and registration year',
                sliderInput('reg_year', 'Registration year:',
                            min=min(years), max=max(years), value=min(years),
                            step=1, round=T, sep='', ticks=F))),
            fluidRow(DT::dataTableOutput('reg_year_table'))
        ),
        tabItem(tabName='by_age', fluidRow(
            box('Fault stats by vehicle age',
                sliderInput('vehicle_age', 'Vehicle age:',
                            min=min(ages), max=max(ages), value=max(ages),
                            step=1, round=T, sep='', ticks=F))),
            fluidRow(DT::dataTableOutput('age_table'))
        ),
        tabItem(tabName='model_overview', fluidRow(
            box(selectInput('car_model', 'Car model overview',
                            cars, selected='Honda CIVIC'),
                sliderInput('model_reg_year', 'Registration year:',
                            min=min(years), max=max(years), value=min(years),
                            step=1, round=T, sep='', ticks=F))),
            fluidRow(tableOutput('model_table'))
        )
    )
)

ui <- dashboardPage(header, sidebar, body)

# server logic
server <- function(input, output) {
    output$reg_year_table <- DT::renderDataTable({
        dt <- stats_model_year[registration_year==input$reg_year]
        dt$fault_pct <- dt$fault_pct * 100
                         
        names(dt) <- c('Model', 'Year', 'Fault%', 'Avg. mileage (km)', 'n')
        dt[,c(1,3:5)]
    }, 
    options=list(pageLength=200, order=list(list(1, 'asc'))), 
    server=F, rownames=F)

    output$age_table <- DT::renderDataTable({
        dt <- stats_age[vehicle_age==input$vehicle_age]
        dt$fault_pct <- dt$fault_pct * 100
        
        names(dt) <- c('Model', 'Age', 'Fault%', 'Avg. mileage (km)', 'n')
        dt[,c(1,3:5)]
    }, 
    options=list(pageLength=200, order=list(list(1, 'asc'))), 
    server=F, rownames=F)
    
    output$model_table <- renderTable({
        model_dt <- stats_by_fault[brand_and_model_series==input$car_model & registration_year==input$model_reg_year]
        avg_dt <- avg_stats_by_fault[registration_year==input$model_reg_year]
        model_dt_long <- model_dt %>% pivot_longer(fault_pct) %>%
            select(c(main_fault_object, value, number_of_inspections)) %>%
            mutate(value=value*100) %>%
            group_by(main_fault_object) %>%
            summarize(model_value=weighted.mean(value, number_of_inspections))
        avg_dt_long <- avg_dt %>% pivot_longer(fault_pct)%>%
            select(c(main_fault_object, value, number_of_inspections)) %>%
            mutate(value=value*100) %>%
            group_by(main_fault_object) %>%
            summarize(avg_value=weighted.mean(value, number_of_inspections))
        model_table <- model_dt_long %>% left_join(avg_dt_long, by="main_fault_object")
        model_table <- model_table %>% mutate(diff=if_else(model_value < avg_value, 
                                                           str_c(round(1-(model_value / avg_value), 2)*100, '% better'), 
                                                           str_c(round(1-(avg_value / model_value), 2)*100, '% worse')))
        names(model_table) <- c('Fault type', 'This model (%)', 'Average (%)', 'This model compared to average')
        model_table
    })
}

# run the application 
shinyApp(ui = ui, server = server)
