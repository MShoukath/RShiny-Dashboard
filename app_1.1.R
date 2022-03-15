source('main_1.1.R')

library(shiny)
library(shinythemes)
library(tippy)

# #working directory
# setwd("/srv/shiny-server/nnatool")
# system("setfacl -R -m u:test2:rwx /srv/shiny-server/nnatool")
# system("setfacl -R -m u:gokul:rwx /srv/shiny-server/nnatool")
# system("setfacl -R -m u:shiny:rwx /srv/shiny-server/nnatool")
# system("setfacl -R -m u:test2:rwx /var/log/shiny-server/")
# system("setfacl -R -m u:gokul:rwx /var/log/shiny-server /")

pages <- c(unlist(unique(df$grouped_page_name)))
countries <- c(unlist(unique(df$country_name)))
device <- c(unlist(unique(df$Device)))
cookies <- c(unlist(unique(df$cookies)))

#for start and end dates 
start <- min(as.Date(df$event_date, format = "%d-%m-%Y"))
end <- max(as.Date(df$event_date, format = "%d-%m-%Y"))

# Define UI
ui <- fluidPage(theme = shinytheme("yeti"),
                sidebarPanel(
                  
                  tags$h3("Filters:"),
                  sliderInput("mde", "MDE:", min = 0.00, max = 0.15, value = 0.01, step = 0.01, post = " in %"),
                  
                  sliderInput("lift", 'Lift Value:', min = 0, max = 10, value=1, post = " %"),
                  
                  fluidRow(
                    
                    tags$div(
                      column(4, dateInput("startdate", label = h5("Start Date"), format = "yyyy-mm-dd", start), align='left')),
                    
                    tags$div(
                      column(4, dateInput("enddate", label = h5("End Date"), format = "yyyy-mm-dd", end), align='center')),
                    
                    tags$div(
                      column(4, dateInput("rpdate", label = h5("Ramp Date"), format = "dd/mm/yyyy", value = Sys.Date()), align='right')  
                    )),
                  
                  selectizeInput('page_name', 'Page Name',
                                 choices = pages),
                  
                  selectizeInput('country_name', 'Country',
                                 choices = countries),
                  
                  selectizeInput('devc_type', 'Device Type',
                                 choices = device),
                  
                  selectizeInput('cookies', 'Cookies',
                                 choices = cookies)
                ), # sidebarPanel
                
                mainPanel(
                  h1("MSS/Activations", align="center"),
                  hr(),
                  
                  tabsetPanel(
                    tabPanel('Overall Funnel',
                             
                             column(width = 12,  
                                    highchartOutput("hc",height="200px")
                             ),
                             tags$div(
                               h3('Conversion Table'),
                               tableOutput('oconversion'),
                               align='center'),
                             
                             # tags$div(
                             #   h3('Funnel Table'),
                             #   tableOutput('ofunnel'), align='center'),
                             
                             
                             hr(),
                             
                             fluidRow(
                               
                               tags$div(
                                 column(6, h4('Sample Size'), tableOutput('osizing')), align='center'),
                               
                               tags$div(
                                 column(6, h4('NNA Calculations '), tableOutput('onna')), align='center')
                             )),
                    
                    tabPanel('Merchant Funnel',
                             
                             tags$div(
                               h3('Conversion Table'),
                               tableOutput('mconversion'),
                               align='center'),
                             hr(),
                             
                             tags$div(
                               h3('Funnel Table'),
                               tableOutput('mfunnel'), align='center'),
                             
                             
                             hr(),
                             
                             fluidRow(
                               
                               tags$div(
                                 column(6, h4('MSS'), tableOutput('msizing')), align='center'),
                               
                               tags$div(
                                 column(6, h4('NNA Calculations '), tableOutput('mnna')), align='center')
                             )),
                    
                    tabPanel('Consumer Funnel',
                             tags$div(
                               h3('Conversion Table'),
                               tableOutput('cconversion'),
                               align='center'),
                             hr(),
                             
                             tags$div(
                               h3('Funnel Table'),
                               tableOutput('cfunnel'), align='center'),
                             
                             
                             hr(),
                             
                             fluidRow(
                               
                               tags$div(
                                 column(6, h4('Sample Size'), tableOutput('csizing')), align='center'),
                               tags$div(
                                 column(6, h4('NNA Calculations '), tableOutput('cnna')), align='center')
                             ))
                  )#tabsetPanel
                  
                ) # mainPanel
                
) # fluidPage





# #working directory
# setwd("/srv/shiny-server/nnatool")
# system("setfacl -R -m u:test2:rwx /srv/shiny-server/nnatool")
# system("setfacl -R -m u:gokul:rwx /srv/shiny-server/nnatool")
# system("setfacl -R -m u:shiny:rwx /srv/shiny-server/nnatool")
# system("setfacl -R -m u:test2:rwx /var/log/shiny-server/")
# system("setfacl -R -m u:gokul:rwx /var/log/shiny-server /")

# Define server function  
server <- function(input, output) {
  
  #Filter Inputs
  # filters1 <- reactive({
  #   data.frame(
  #     Filter = c("MDE",
  #                "Start Date",
  #                "Country",
  #                "Page"),
  #     
  #     values = c(as.double(input$mde),
  #                
  #                as.character.Date(input$startdate, format="%Y/%m/%d"),
  #                as.character(input$country_name), 
  #                as.character.Date(input$page_name)), stringsAsFactors = FALSE)
  # })
  # 
  # filters2 <- reactive({
  #   data.frame(
  #     Filter = c("Device Type",
  #                "End Date",
  #                "Lift Percentage",
  #                "Ramp Date"),
  #     
  #     values = c(as.character(input$devc_type),
  #                as.character.Date(input$enddate, format="%Y/%m/%d"),
  #                as.integer(input$lift),
  #                as.character.Date(input$rpdate, format="%Y/%m/%d")), stringsAsFactors = FALSE)
  # })
  
  output$hc <- renderHighchart({
    hc
  })
  
  #Overall Reactive calls
  overallcon <- reactive(get_overall_funnel(df, as.character.Date(input$startdate, format="%Y-%m-%d"), as.character.Date(input$enddate,  format="%Y-%m-%d"), input$mde, input$lift, as.character.Date(input$rpdate, format="%d/%m/%y"), input$country_name, input$page_name, input$devc_type, input$cookies))
  overall_conversion <- reactive({
    conversion <- data.frame(overallcon()[1],check.names = FALSE)
  })
  overall_funnel <- reactive({
    funnel <- data.frame(overallcon()[2],check.names = FALSE)
  })
  
  overall_sizing <- reactive({
    sizing <- data.frame(overallcon()[3],check.names = FALSE)
  })
  
  overall_nna <- reactive({
    nna <- data.frame(overallcon()[4],check.names = FALSE)
  })
  
  #Merchant Reactive Calls
  merchantcon <- reactive(get_merchant_funnel(df, as.character.Date(input$startdate, format="%Y-%m-%d"), as.character.Date(input$enddate,  format="%Y-%m-%d"), input$mde, input$lift, as.character.Date(input$rpdate, format="%d/%m/%y"), input$country_name, input$page_name, input$devc_type, input$cookies))
  merchant_conversion <- reactive({
    conversion <- data.frame(merchantcon()[1], check.names = FALSE)
  })
  merchant_funnel <- reactive({
    funnel <- data.frame(merchantcon()[2], check.names = FALSE)
  })
  
  merchant_sizing <- reactive({
    sizing <- data.frame(merchantcon()[3], check.names = FALSE)
  })
  
  merchant_nna <- reactive({
    nna <- data.frame(merchantcon()[4], check.names = FALSE)
  })
  
  #Consumer Reactive Calls
  consumercon <- reactive(get_consumer_funnel(df, as.character.Date(input$startdate, format="%Y-%m-%d"), as.character.Date(input$enddate,  format="%Y-%m-%d"), input$mde, input$lift, as.character.Date(input$rpdate, format="%d/%m/%y"), input$country_name, input$page_name, input$devc_type, input$cookies))
  consumer_conversion <- reactive({
    conversion <- data.frame(consumercon()[1], check.names = FALSE)
  })
  consumer_funnel <- reactive({
    funnel <- data.frame(consumercon()[2], check.names = FALSE)
  })
  
  consumer_sizing <- reactive({
    sizing <- data.frame(consumercon()[3], check.names = FALSE)
  })
  
  consumer_nna <- reactive({
    nna <- data.frame(consumercon()[4], check.names = FALSE)
  })
  
  
  #Filters O/P table
  # output$filter1 <- renderTable({filters1()})
  # output$filter2 <- renderTable({filters2()})
  
  #Overall funnel O/P's
  output$oconversion <- renderTable(format({overall_conversion()}, big.mark=",", trim=TRUE), striped = TRUE, hover = TRUE, bordered=TRUE)
  output$ofunnel <- renderTable(format({overall_funnel()}, big.mark=",", trim=TRUE), striped = TRUE, hover = TRUE, bordered=TRUE)
  output$osizing <- renderTable(format({overall_sizing()}, big.mark=",", trim=TRUE), striped = TRUE, hover = TRUE, bordered=TRUE)
  output$onna <- renderTable(format({overall_nna()}, big.mark=",", trim=TRUE), striped = TRUE, hover = TRUE, bordered=TRUE)
  
  #Merchant funnel O/P's
  output$mconversion <- renderTable(format({merchant_conversion()}, big.mark=",", trim=TRUE), striped = TRUE, hover = TRUE, bordered=TRUE)
  output$mfunnel <- renderTable(format({merchant_funnel()}, big.mark=",", trim=TRUE), striped = TRUE, hover = TRUE, bordered=TRUE)
  output$msizing <- renderTable(format({merchant_sizing()}, big.mark=",", trim=TRUE), striped = TRUE, hover = TRUE, bordered=TRUE)
  output$mnna <- renderTable(format({merchant_nna()}, big.mark=",", trim=TRUE), striped = TRUE, hover = TRUE, bordered=TRUE)
  
  #Consumer funnel O/P's
  output$cconversion <- renderTable(format({consumer_conversion()}, big.mark=",", trim=TRUE), striped = TRUE, hover = TRUE, bordered=TRUE)
  output$cfunnel <- renderTable(format({consumer_funnel()}, big.mark=",", trim=TRUE), striped = TRUE, hover = TRUE, bordered=TRUE)
  output$csizing <- renderTable(format({consumer_sizing()}, big.mark=",", trim=TRUE), striped = TRUE, hover = TRUE, bordered=TRUE)
  output$cnna <- renderTable(format({consumer_nna()}, big.mark=",", trim=TRUE), striped = TRUE, hover = TRUE, bordered=TRUE)
  
}

# Create Shiny object
shinyApp(ui = ui, server = server)


