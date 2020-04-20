ui <- dashboardPage(
  
  # Application title
  dashboardHeader(title = "US Higher Education"),
  dashboardSidebar(
    menuItem("View by States", 
             tabName = "overview", 
             icon = icon("dashboard")),
    menuItem("Overview", 
             tabName = "stateview", 
             icon = icon("th"))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                column(2,
                       pickerInput(
                         inputId = "statePicker",
                         label = "Choose a state",
                         choices = c(state.name)
                       )),
                column(6,
                       plotOutput("distPlot")),
                column(4,
                       valueBoxOutput('popBox',
                                      width = 12),
                       valueBoxOutput('incomeBox',
                                      width = 12),
                       valueBoxOutput('unempBox',
                                      width = 12)
                )
              ),
              tags$br(),
              fluidRow(
                column(2, 
                       offset = 0, 
                       style='padding:0px;'),
                column(5,
                       plotOutput('racePlot')),
                column(5,
                       plotOutput('genderPlot'))
              ),
              tags$br(),
              fluidRow(
                column(2, 
                       offset = 0, 
                       style='padding:0px;'),
                column(10,
                       plotOutput('schoolPlot'))
              ),
              tags$br(),
              fluidRow(
                column(2, 
                       offset = 0, 
                       style='padding:0px;'),
                column(10,
                       plotlyOutput('aidScatterPlot'))
              )
      ),
      tabItem(tabName = 'stateview',
              fluidRow(
                column(6,
                       tags$h4('Income distribution by state'),
                       plotOutput('incomeByStatePlot')),
                column(6,
                       tags$h4('Unemployment distribution by state'),
                       plotOutput('unempByStatePlot'))
              ),
              tags$br(),
              tags$h4('Education Information'),
              fluidRow(
                plotOutput('avgAceptancePlot')
              )
      )
    )
  )
)