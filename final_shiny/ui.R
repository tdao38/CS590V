# ui <- dashboardPage(
#   skin = 'red',
#   # Application title
#   dashboardHeader(title = "US Higher Education"),
#   dashboardSidebar(
#     menuItem("View by States", 
#              tabName = "overview", 
#              icon = icon("dashboard")),
#     menuItem("Overview", 
#              tabName = "stateview", 
#              icon = icon("th"))
#   ),
#   dashboardBody(
#     tabItems(
#       tabItem(tabName = "overview",
#               fluidRow(
#                 column(2,
#                        pickerInput(
#                          inputId = "statePicker",
#                          label = "Choose a state",
#                          choices = c(state.name)
#                        )),
#                 column(6,
#                        plotOutput("distPlot")),
#                 column(4,
#                        valueBoxOutput('popBox',
#                                       width = 12),
#                        valueBoxOutput('incomeBox',
#                                       width = 12),
#                        valueBoxOutput('unempBox',
#                                       width = 12)
#                 )
#               ),
#               tags$br(),
#               fluidRow(
#                 column(2, 
#                        offset = 0, 
#                        style='padding:0px;'),
#                 column(5,
#                        plotOutput('racePlot')),
#                 column(5,
#                        plotOutput('genderPlot'))
#               ),
#               tags$br(),
#               fluidRow(
#                 column(2, 
#                        offset = 0, 
#                        style='padding:0px;'),
#                 column(10,
#                        plotOutput('schoolPlot'))
#               ),
#               tags$br(),
#               fluidRow(
#                 column(2, 
#                        offset = 0, 
#                        style='padding:0px;'),
#                 column(10,
#                        plotlyOutput('aidScatterPlot'))
#               )
#       ),
#       tabItem(tabName = 'stateview',
#               fluidRow(
#                 column(6,
#                        tags$h4('Income distribution by state'),
#                        plotOutput('incomeByStatePlot')),
#                 column(6,
#                        tags$h4('Unemployment distribution by state'),
#                        plotOutput('unempByStatePlot'))
#               ),
#               tags$br(),
#               tags$h4('Education Information'),
#               fluidRow(
#                 plotOutput('avgAceptancePlot')
#               )
#       )
#     )
#   )
# )
ui <- dashboardPage(
  skin = 'red',
  # Application title
  dashboardHeader(
    title = "US Higher Education",
    titleWidth =300
  ),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      div(h4('Filter colleges by:')),
      # menuItem("States", 
      #          tabName = "overview", 
      #          icon = icon("dashboard")),
      # menuItem("Overview", 
      #          tabName = "stateview", 
      #          icon = icon("th")),
      menuItem("States",
               #startExpanded = TRUE,
               selectizeInput(
                 inputId = 'state_input',
                 label = 'Choose states',
                 choices = c(state.name),
                 selected = 'Massachusetts', 
                 multiple = TRUE)),
      menuItem("Tuition & Fees", 
               #startExpanded = TRUE,
               sliderInput(
                 inputId = 'tuition_input',
                 label = 'Select range of tuition',
                 min = 5000,
                 max = 60000,
                 value = c(5, 60000),
                 step = 1000)),
      menuItem("Standardized Scores",
               #startExpanded = TRUE,
               sliderInput(
                 inputId = 'sat_input',
                 label = 'Average SAT score range',
                 min = 800,
                 max = 1600,
                 value = c(800, 1600)),
               sliderInput(
                 inputId = 'act_input',
                 label = 'Average ACT score range',
                 min = 15,
                 max = 34,
                 value = c(15, 34))),
      menuItem('Enrollment Size'),
      menuItem('Ranking',
               numericInput(
                 inputId = 'ranking_input',
                 label = 'See colleges of rank higher than',
                 value = 100)),
      menuItem('Institution Type',
               checkboxGroupInput(
                 inputId = 'inst_type_input',
                 label = 'Select institution type',
                 choices = list('Public' = 'public',
                                'Private' = 'private'),
                 selected = c('public', 'private')))
    )
  ),
  dashboardBody(
    fluidRow(
      column(
        width = 7,
        tags$h3('Academics'),
        tags$div(
          class = 'panel panel-danger',
          tags$div(
            class = 'panel-body',
            dataTableOutput('schools_tb_output')
          )
        )
      ),
      column(
        width = 5,
        tags$h3('Financials'),
        tags$div(
          class = 'panel panel-danger',
          tags$div(
            class = 'panel-body',
            #h4('Aid scatterplot'),
            prettyRadioButtons(
              inputId = 'cost_type',
              label = 'Display bubble size by',
              choices = (c('Tuition' = 'tuition',
                           'Cost after aid' = 'cost_after_aid')),
              shape = 'round',
              status = 'danger',
              selected = 'tuition',
              inline = TRUE,
              animation = 'smooth',
              bigger = TRUE
            ),
            plotlyOutput('aidScatterPlot'),
            tags$br()
          )
        )
      )
    )
    # tabItems(
    #   tabItem(tabName = "overview",
    #           fluidRow(
    #             column(2,
    #                    pickerInput(
    #                      inputId = "statePicker",
    #                      label = "Choose a state",
    #                      choices = c(state.name)
    #                    )),
    #             column(6,
    #                    plotOutput("distPlot")),
    #             column(4,
    #                    valueBoxOutput('popBox',
    #                                   width = 12),
    #                    valueBoxOutput('incomeBox',
    #                                   width = 12),
    #                    valueBoxOutput('unempBox',
    #                                   width = 12)
    #             )
    #           ),
    #           tags$br(),
    #           fluidRow(
    #             column(2, 
    #                    offset = 0, 
    #                    style='padding:0px;'),
    #             column(5,
    #                    plotOutput('racePlot')),
    #             column(5,
    #                    plotOutput('genderPlot'))
    #           ),
    #           tags$br(),
    #           fluidRow(
    #             column(2, 
    #                    offset = 0, 
    #                    style='padding:0px;'),
    #             column(10,
    #                    plotOutput('schoolPlot'))
    #           ),
    #           tags$br(),
    #           fluidRow(
    #             column(2, 
    #                    offset = 0, 
    #                    style='padding:0px;'),
    #             column(10,
    #                    plotlyOutput('aidScatterPlot'))
    #           )
    #   ),
    #   tabItem(tabName = 'stateview',
    #           fluidRow(
    #             column(6,
    #                    tags$h4('Income distribution by state'),
    #                    plotOutput('incomeByStatePlot')),
    #             column(6,
    #                    tags$h4('Unemployment distribution by state'),
    #                    plotOutput('unempByStatePlot'))
    #           ),
    #           tags$br(),
    #           tags$h4('Education Information'),
    #           fluidRow(
    #             plotOutput('avgAceptancePlot')
    #           )
    #   )
    # )
  )
)