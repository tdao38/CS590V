ui <- dashboardPage(
  #skin = 'red',
  # Application title
  dashboardHeader(
    tags$li(class = "dropdown",
            tags$style(".main-header {max-height: 53px}"),
            tags$style(".main-header .logo {height: 53px}")
    ),
    title = "US Higher Education",
    titleWidth = 300,
    tags$li(a(href = "https://www.cics.umass.edu//",
              img(src = 'mm.png',
                  title = "CICS", height = "39px"),
              style = "padding-top:7px; padding-bottom:1px; padding-right:20px;"),
            class = "dropdown")
  ),
  dashboardSidebar(
    tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #c4888a;
                    border-top: 1px solid #c4888a ;
                    border-bottom: 1px solid #c4888a ;}}")),
    tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #c4888a;
                    border-top: 1px solid #c4888a ;
                    border-bottom: 1px solid #c4888a ;}}")),
    tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar {background: #c4888a;
                    border-top: 1px solid #c4888a ;
                    border-bottom: 1px solid #c4888a ;}}}")),
    tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar {background: #c4888a;
                    border-top: 1px solid #c4888a ;
                    border-bottom: 1px solid #c4888a ;}}}")),
    tags$head(tags$style(HTML('.irs-from, .irs-to, .irs-single { background: #881c1c}'
    ))
    ),
    width = 300,
    sidebarMenu(
      tags$h4('Filter colleges by:'),
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
    tags$style(".small-box.bg-red {background-color: #F2B349 !important; color: #ffffff !important; }"),
    tags$style(".small-box.bg-purple {background-color: #42AD96 !important; color: #ffffff !important; }"),
    tags$style(".small-box.bg-blue {background-color: #B5584E !important; color: #ffffff !important; }"),
    tags$head(tags$style(HTML('
                            .skin-blue .main-header .logo {
                              background-color: #881c1c;
                              }
                              .skin-blue .main-header .logo:hover {
                              background-color: #881c1c;
                              }
                              .skin-blue .main-header .navbar {
                              background-color: #881c1c;
                              } 
                              .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #c4888a;
                              }
                              .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #881c1c;
                              }.
                              .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #c4888a;
                              }
                              .skin-blue .sidebar-menu > li.active > a,
                              .skin-blue .sidebar-menu > li:hover > a {
                              border-left-color: #881c1c;
                              }
                              '))),
    fluidRow(
      column(
        width = 7,
        tags$h3('Academics'),
        tags$h6('Click on a school name to see more information about a school'),
        tags$div(
          class = 'panel panel-danger',
          tags$div(
            tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: #f2cece !important;}')),
            class = 'panel-body',
            dataTableOutput('schools_tb_output') %>% withSpinner(color = '#881c1c', type = 6)
          )
        )
      ),
      column(
        width = 5,
        tags$h3('School type'),
        tags$h6('Public and private school distribution in the chosen states'),
        tags$div(
          class = 'panel panel-danger',
          tags$div(
            class = 'panel-body',
            #h4('Aid scatterplot'),
            tags$br(),
            plotlyOutput('school_type_plot', height = '450px') %>% withSpinner(color = '#881c1c', type = 6),
            tags$br()
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 7,
        tags$h3('Salary'),
        tags$h6('Average mid career salaries after graduating from these schools'),
        tags$div(
          class = 'panel panel-danger',
          tags$div(
            class = 'panel-body',
            tags$h4('Average Mid Career Salaries (10th, 25th, Median, 75th, 90th percentiles) broken down by school'),
            plotlyOutput('incomeScatterplot'),
            tags$br()
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
            plotlyOutput('aidScatterPlot') %>% withSpinner(color = '#881c1c', type = 6),
            tags$br()
          )
        )
      )
    ),
    fluidRow(
      column(
        width = 7,
        tags$h3('Additional Information about Living and Working'),
        tags$div(
          class = 'panel panel-danger',
          tags$div(
            class = 'panel-body',
            leafletOutput(outputId = 'map') %>% withSpinner(color = '#881c1c', type = 6)
          )
        )
      ),
      column(
        width = 5,
        tags$head(tags$style(HTML('.small-box {height: 85px; width: 165px;} 
                                  .small-box h3 {font-size: 23px;}
                                  .small-box p {font-size: 15px;}
                                  .small_icon_test { font-size: 35px;}'))),
        tags$h3('State Information'),
        tags$div(
          class = 'panel panel-danger',
          tags$div(
            class = 'panel-body',
            uiOutput('radioState'),
            fluidRow(
              valueBoxOutput('popBox', width = 4),
              valueBoxOutput('incomeBox', width = 4),
              valueBoxOutput('unempBox', width = 4)
            ),
            fluidRow(
              column(
                width = 6,
                plotlyOutput('race_plot', width = "245px", height="245px") %>% withSpinner(color = '#881c1c', type = 6)
              ),
              column(
                width = 6,
                plotlyOutput('job_type_plot', width = "245px", height="245px") %>% withSpinner(color = '#881c1c', type = 6)
              )
            )
            # valueBoxOutput('popBox',width = 2),
            # valueBoxOutput('USPop',width = 2),
            # valueBoxOutput('incomeBox', width =2),
            # valueBoxOutput('USincome', width = 6),
            # valueBoxOutput('unempBox', width = 6),
            # valueBoxOutput('USunem', width = 6)
            #uiOutput("tabs")
          )
        )
      )
    )
  )
)