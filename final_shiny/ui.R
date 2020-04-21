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
               icon = icon('map-marked-alt'),
               #startExpanded = TRUE,
               selectizeInput(
                 inputId = 'state_input',
                 label = 'Choose states',
                 choices = c(state.name),
                 selected = 'Massachusetts', 
                 multiple = TRUE)),
      menuItem("Tuition & Fees", 
               icon = icon('money-bill'),
               #startExpanded = TRUE,
               sliderInput(
                 inputId = 'tuition_input',
                 label = 'Select range of tuition',
                 min = 5000,
                 max = 60000,
                 value = c(5, 60000),
                 step = 1000)),
      menuItem("Standardized Scores",
               icon = icon('book-open'),
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
      menuItem('Enrollment Size',
               icon = icon('users'),
               sliderInput(
                 inputId = 'enrollment_input',
                 label = 'Undergraduate enrollment size',
                 min = 1000,
                 max = 55000,
                 value = c(1000, 55000),
                 step = 1000)),
      menuItem('Ranking',
               icon = icon('arrow-up'),
               numericInput(
                 inputId = 'ranking_input',
                 label = 'See colleges of rank higher than',
                 value = 100)),
      menuItem('Institution Type',
               icon = icon('university'),
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
        tags$h6('Click on a row to see more information about a school'),
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
        tags$h6('Hover to a bubble to see more information'),
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
  )
)