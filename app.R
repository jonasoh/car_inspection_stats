library(shiny)
library(shinydashboard)
library(DT)

library(data.table)
#library(ggplot2)

# load precomputed data (see preprocess.R)
d <- readRDS('app_data.rds')
stats_model_year <- d$stats_model_year
stats_age <- d$stats_age
model_stats_by_fault <- d$model_stats_by_fault
avg_model_stats_by_fault <- d$avg_model_stats_by_fault
brand_stats_by_age <- d$brand_stats_by_age
fault_categories <- d$fault_categories
years <- d$years
ages <- d$ages
cars <- d$cars

# not every model was registered across the full 2002-2021 span (e.g. the
# Mitsubishi Colt only has 2005-2010 data), so keep each model's own
# registration-year range to adjust the slider on the model overview tab
model_year_range <- model_stats_by_fault[, .(min_yr=min(registration_year), max_yr=max(registration_year)),
                                          by=brand_and_model_series]
setkey(model_year_range, brand_and_model_series)

# dashboard ui
header <- dashboardHeader(title="Car inspection statistics")

sidebar <- dashboardSidebar(
    sidebarMenu(id="sidebar", 
        menuItem("Page info", tabName="info", icon=icon("car")),
        menuItem("By model and year", tabName="model_year", icon=icon("car")),
        menuItem("By age", tabName="by_age", icon=icon("car")),
        menuItem("Car model overview", tabName="model_overview", icon=icon("car")),
        menuItem("Brand leaderboard", tabName="brand_leaderboard", icon=icon("car"))
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(tabName='info', fluidRow(
                h1('Car inspection stats'),
                p('This website presents Finnish car inspection stats from the', 
                  a('Traficom Statistics Database', href='https://trafi2.stat.fi/PXWeb/pxweb/en/TraFi/TraFi__Katsastuksen_vikatilastot/?tablelist=true'),
                  'which details the results of periodic inspections for all car models with over 100 inspected cars per year.'),
                p('For the ranking by registration year and age, as well as for the brand leaderboard, only model-related errors which cause a demand for repair is accounted for,',
                  'i.e., broken parking lights or slightly rusted brake discs do not affect the ratings.'),
                p('As statistics are aggregated by model and fault category, some models will have over 100% fault rating. This is a necessary consequence of how the data are delivered by Traficom, and arguably the better way to present the data.'),
                p('Data covers periodic inspections carried out in 2017-2025. Traficom changed its model naming and grouping in 2023 (e.g. splitting or merging some model variants); ',
                  'names have been harmonized so each model forms one continuous series across all years.'),
                p('Use the menu', icon('bars'), 'to choose which statistics to view.')
            )
        ),
        tabItem(tabName='model_year', fluidRow(
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
        ),
        tabItem(tabName='brand_leaderboard', fluidRow(
            box(sliderInput('brand_model_reg_year', 'Registration year:',
                            min=min(years), max=max(years), value=min(years),
                            step=1, round=T, sep='', ticks=F))),
            fluidRow(DT::dataTableOutput('brand_leaderboard_table'))
        )
    )
)

ui <- dashboardPage(header, sidebar, body)

# server logic
server <- function(input, output, session) {
    # a model's registration years can be a strict subset of the global
    # range, so keep the slider matched to the selected model instead of
    # silently showing an empty table
    observeEvent(input$car_model, {
        rng <- model_year_range[.(input$car_model)]
        value <- if (isTRUE(input$model_reg_year >= rng$min_yr && input$model_reg_year <= rng$max_yr)) input$model_reg_year else rng$min_yr
        updateSliderInput(session, 'model_reg_year', min=rng$min_yr, max=rng$max_yr, value=value)
    })
    output$reg_year_table <- DT::renderDataTable({
        dt <- stats_model_year[registration_year==input$reg_year]
        dt$fault_pct <- round(dt$fault_pct * 100, 1)

        names(dt) <- c('Model', 'Year', 'Fault%', 'Avg. mileage (km)', 'n', 'Brand')
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
        model_dt <- model_stats_by_fault[brand_and_model_series==input$car_model & registration_year==input$model_reg_year,
                                          .(main_fault_object, model_value)]
        validate(need(nrow(model_dt) > 0,
                       sprintf('No inspections recorded for %s registered in %d.', input$car_model, input$model_reg_year)))
        avg_dt <- avg_model_stats_by_fault[registration_year==input$model_reg_year, .(main_fault_object, avg_value)]
        model_table <- model_dt[avg_dt, on="main_fault_object"]
        model_table[, diff := fcase(model_value < avg_value, paste0(round(1-(model_value / avg_value), 2)*100, '% better'),
                                    model_value >= avg_value, paste0(round(1-(avg_value / model_value), 2)*100, '% worse'))]
        names(model_table) <- c('Fault type', 'This model (%)', 'Average (%)', 'This model compared to average')
        model_table
    })
    
    output$brand_leaderboard_table <- DT::renderDataTable({
        dt <- brand_stats_by_age[registration_year==input$brand_model_reg_year]
        dt[,-1]
    }, 
    options=list(pageLength=50, order=list(list(0, 'asc'))), 
    server=F, rownames=F)
}

# run the application 
shinyApp(ui = ui, server = server)
