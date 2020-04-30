# library(shinydashboard)
# library(tidyverse)
# library(usmap)
# library(ggplot2)
# library(ggmap)
# library(ggrepel)
# library(GGally)
# library(plotly)
# library(shiny)
# library(scales)
# library(shinyWidgets)
# library(shinythemes)
# library(shinyjs)
# library(janitor)
# library(DT)

#source('~/UMass/CS590V/final_shiny/utils.R', echo = TRUE)
source('utils.R', echo = TRUE)

packages(c("shinydashboard", "tidyverse", "usmap", "ggplot2", "ggmap", "ggrepel", 
           "GGally", "plotly", "shiny", "scales", "shinyWidgets", "shinythemes",
           "shinyjs", "janitor", "DT","leaflet","sp","rgdal", "sf", "htmltools",
           "RColorBrewer", "shinycssloaders"))

# Load data
#school <- read_csv('data/school_raw.csv')
school <- read_csv('data/school_new.csv')
census <- read_csv('data/census_new.csv')
income <- read_csv('data/income.csv')
race <- read_csv('data/race.csv')
job_type <- read_csv('data/job_type.csv')
school_type <- read_csv('data/school_type.csv')
school_income <-read_csv('data/school_income.csv')
states <- readOGR(dsn = "data/cb_2016_us_state_500k.shp", encoding = "UTF-8", verbose = FALSE)

school_income$display_name <- gsub("--", " ", school_income$display_name)
school_income$variable <- factor(school_income$variable, levels = c("Mid Career 10th Percentile Salary", "Mid Career 25th Percentile Salary", "Mid Career Median Salary", "Mid Career 75th Percentile Salary", "Mid Career 90th Percentile Salary"))
school_income$value <-as.numeric(school_income$value)
# school_loc <- read.csv("data/hd2018.csv") %>% 
#   select(INSTNM, LONGITUD, LATITUDE) %>% 
#   rename(display_name = INSTNM,
#          long = LONGITUD,
#          lat = LATITUDE)
# 
# CLEANING ===========================================================
# school <- school %>% clean_names()
# school$state_name <- state.name[match(school$state, state.abb)]
# school <- school %>% filter(overall_rank >= 1)
# 
# school_loc$display_name <- gsub("-", " ", school_loc$display_name)
# school$display_name <- gsub("--", " ", school$display_name)
# 
# school <- merge(x = school, y = school_loc, by = "display_name", all.x = TRUE)
# school$state_name <- as.factor(school$state_name)
# write.csv(school, file = 'school_new.csv')

# census <- census %>% clean_names()
# census <- census %>% mutate(state_name = state)
# census$state <- state.abb[match(census$state_name, state.name)]
#write.csv(census, file = 'census_new.csv')

# school_aid <- school %>% select(display_name, state, state_name, overall_rank, acceptance_rate, percent_receiving_aid, cost_after_aid)
# school_aid_complete <- school_aid[complete.cases(school_aid),]

# school_score <- school %>% select(overall_rank, display_name, state_name, act_avg, sat_avg)
# school_score <- school_score[complete.cases(school_score),]

# write.csv(school_aid, file = 'school_aid.csv')
# write.csv(school_aid_complete, file = 'school_aid_complete.csv')
# write.csv(school_score, file = 'school_score.csv')

# Income process
# income_df <- census %>%
#     group_by(state_name, state) %>%
#     summarize(avg_income = mean(income),
#               avg_unemployment = mean(unemployment),
#               pop = sum(total_pop))
# income_df$avg_income <- round(income_df$avg_income)
# income_df$avg_unemployment <- round(income_df$avg_unemployment)

#write.csv(income_df, file = 'income.csv')

# prof
# job_type <- census %>%
#   group_by(state_name) %>%
#   summarize(professional = mean(professional),
#             service = mean(service),
#             office = mean(office),
#             construction = mean(construction),
#             production = mean(production)) %>%
#   gather(key = 'job_type', value = 'pct', c(professional, service, office, construction, production)) %>%
#   arrange(state_name)
# write.csv(job_type, file = 'job_type.csv')

#
job_plot <- job_type %>%
  filter(state_name == 'Alabama') %>%
  plot_ly(labels = ~job_type,
          values = ~pct,
          hovertemplate = "%{label}: %{percent}",
          name = '',
          marker = list(colors = brewer.pal(5, 'Set2'))) %>%
  add_pie(hole = 0.6) %>%
  hide_legend()
  # layout(legend = list(x = 0.25,
  #                      y = 0.25))
#
# # Race
# race <- census %>%
#     group_by(state_name) %>%
#     summarize(hispanic = mean(hispanic),
#               white = mean(white),
#               black = mean(black),
#               native = mean(native),
#               asian = mean(asian),
#               pacific = mean(pacific)) %>%
#     gather(key = 'race', value = 'pct', c(hispanic, white, black, native, asian, pacific))
# race$pct <- round(race$pct)
# write.csv(race, file = 'race.csv')
# race_plot <- race %>%
#   filter(state_name == 'California') %>%
#   plot_ly(labels = ~race,
#           values = ~pct,
#           hovertemplate = "%{label}: %{percent}",
#           name = '',
#           marker = list(colors = brewer.pal(5, 'Set2'))) %>%
#   add_pie(hole = 0.6) %>%
#   hide_legend()
#
# race <- race %>%
#     group_by(State) %>%
#     mutate(fraction = pct/100)
#
# # stack
# school_type <- school %>%
#   select(state_name, display_name, institutional_control) %>%
#   group_by(state_name, institutional_control) %>%
#   summarize(count = n()) %>%
#   spread(institutional_control, count) %>%
#   replace_na(list(private = 0, public = 0)) %>%
#   gather(institutional_control, count, -state_name) %>%
#   arrange(state_name)
#school_type <- school_type[which(complete.cases(school_type)),]
#write.csv(school_type, file = 'school_type.csv')
#
# school_type_plot <- school_type %>%
#   filter(state_name %in% c('California', 'Massachusetts', 'New York')) %>%
#   group_by(state_name) %>%
#   mutate(countT = sum(count)) %>%
#   group_by(institutional_control, add = TRUE) %>%
#   mutate(pct=round(100*count/countT)) %>%
#   ggplot(aes(x = state_name, y = pct, fill = institutional_control)) +
#   geom_col(position = 'stack', width = 0.4) +
#   scale_fill_brewer(palette = 'Pastel1') +
#   #scale_fill_manual(values = c('cornflowerblue', 'salmon')) +
#   theme_bw()
# 
# ggplotly(school_type_plot)
# 
# # Gender
# gender <- census %>%
#     group_by(State) %>%
#     summarize(Male = mean(Men),
#               Female = mean(Women),
#               male = Male/(Male+Female),
#               female = Female/(Male+Female)) %>%
#     select(State, male, female) %>%
#     gather(key = 'gender', value = 'fraction', c(male, female))
# 
# gender$fraction <- round(gender$fraction,2)
# 
# # Aid
# aid <- school %>%
#     select(state_name, display_name, acceptance_rate, percent_receiving_aid, cost_after_aid)
# 
# for (i in seq(3,5)){
#     aid[,i] <- as.integer(aid[[i]])
# }

# Define UI for application that draws a histogram
# ui <- dashboardPage(
# 
#     # Application title
#     dashboardHeader(title = "US Higher Education"),
#     dashboardSidebar(
#         menuItem("View by States", 
#                  tabName = "overview", 
#                  icon = icon("dashboard")),
#         menuItem("Overview", 
#                  tabName = "stateview", 
#                  icon = icon("th"))
#     ),
#     dashboardBody(
#         tabItems(
#             tabItem(tabName = "overview",
#                     fluidRow(
#                         column(2,
#                                pickerInput(
#                                    inputId = "statePicker",
#                                    label = "Choose a state",
#                                    choices = c(state.name)
#                                )),
#                         column(6,
#                                plotOutput("distPlot")),
#                         column(4,
#                                valueBoxOutput('popBox',
#                                               width = 12),
#                                valueBoxOutput('incomeBox',
#                                               width = 12),
#                                valueBoxOutput('unempBox',
#                                               width = 12)
#                         )
#                     ),
#                     tags$br(),
#                     fluidRow(
#                         column(2, 
#                                offset = 0, 
#                                style='padding:0px;'),
#                         column(5,
#                                plotOutput('racePlot')),
#                         column(5,
#                                plotOutput('genderPlot'))
#                     ),
#                     tags$br(),
#                     fluidRow(
#                         column(2, 
#                                offset = 0, 
#                                style='padding:0px;'),
#                         column(10,
#                                plotOutput('schoolPlot'))
#                     ),
#                     tags$br(),
#                     fluidRow(
#                         column(2, 
#                                offset = 0, 
#                                style='padding:0px;'),
#                         column(10,
#                                plotlyOutput('aidScatterPlot'))
#                     )
#             ),
#             tabItem(tabName = 'stateview',
#                     fluidRow(
#                         column(6,
#                                tags$h4('Income distribution by state'),
#                                plotOutput('incomeByStatePlot')),
#                         column(6,
#                                tags$h4('Unemployment distribution by state'),
#                                plotOutput('unempByStatePlot'))
#                     ),
#                     tags$br(),
#                     tags$h4('Education Information'),
#                     fluidRow(
#                         plotOutput('avgAceptancePlot')
#                     )
#             )
#         )
#     )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
# 
#     output$distPlot <- renderPlot({
#         # generate bins based on input$bins from ui.R
#         all_states <- map_data("state")
#         states_chosen <- tolower(input$statePicker)
#         states_positive  <- c()
#         states_positive  <- c(states_positive, states_chosen)
#         ggplot(all_states, aes(x=long, y=lat, group = group)) +
#             geom_polygon(fill="grey", colour = "white") +
#             geom_polygon(fill="cornflowerblue", data = filter(all_states, region %in% states_positive)) + 
#             theme_nothing()
#     })
#     
#     output$popBox <- renderValueBox({
#         income_df %>% 
#             filter(state == tolower(input$statePicker)) %>%
#             #select(Pop) %>%
#             .$Pop %>%
#             format(big.mark=",",scientific=FALSE) %>%
#             valueBox(
#                 subtitle = "Total Population",
#                 color = "blue",
#                 icon = icon("users")
#             )
#     })
# 
#     output$incomeBox <- renderValueBox({
#         income_df %>% 
#             filter(state == tolower(input$statePicker)) %>%
#             #select(AvgIncome) %>%
#             .$AvgIncome %>%
#             scales::dollar() %>%
#             valueBox(
#                 subtitle = "Average Income",
#                 color = "green",
#                 icon = icon("dollar-sign")
#         )
#     })
#     
#     output$unempBox <- renderValueBox({
#         income_df %>% 
#             filter(state == tolower(input$statePicker)) %>%
#             mutate(AvgUnemployment = AvgUnemployment/100) %>%
#             #select(AvgUnemployment) %>%
#             .$AvgUnemployment %>%
#             percent() %>%
#             valueBox(
#                 subtitle = "Average Unemployment",
#                 color = "orange",
#                 icon = icon("stats",lib='glyphicon')
#             )
#     })
#     
#     output$schoolPlot <- renderPlot({
#         school %>%
#             filter(state_name == input$statePicker) %>%
#             select(overall_rank, display_name, act_avg, sat_avg) %>%
#             gather(key = 'measure', value = 'value', c(act_avg, sat_avg)) %>%
#             ggplot(aes(x=reorder(display_name, -overall_rank), y = value, fill=measure)) +
#             geom_bar(stat = 'identity', width = 0.5) + 
#             xlab('Schools (Ordered by Rank)') +
#             ylab('Score') + 
#             facet_wrap(~measure, 
#                        nrow = 1,
#                        scales = 'free_x') +
#             coord_flip() +
#             theme_minimal() + 
#             #scale_fill_manual(values = c("#D95F02","#7570B3"))
#             scale_fill_brewer(type = 'qual',
#                               palette = 'Dark2')
#     })
#     
#     output$racePlot <- renderPlot({
#         race %>%
#             filter(State == input$statePicker) %>%
#             arrange(fraction) %>%
#             mutate(ymax = cumsum(fraction),
#                    ymin = c(0, head(ymax, n=-1))) %>%
#             ggplot(aes(fill = race, 
#                        ymax = ymax,
#                        ymin = ymin,
#                        xmax = 4,
#                        xmin = 3)) +
#             geom_rect() + 
#             coord_polar(theta='y') +
#             xlim(c(0,4)) +
#             scale_fill_brewer(type = 'qual',
#                               palette = 'Dark2') +
#             theme_minimal() +
#             theme(panel.grid=element_blank()) +
#             theme(axis.text=element_blank()) +
#             theme(axis.ticks=element_blank())
#     })
#     
#     output$genderPlot <- renderPlot({
#         gender %>%
#             filter(State == input$statePicker) %>%
#             arrange(fraction) %>%
#             mutate(ymax = cumsum(fraction),
#                    ymin = c(0, head(ymax, n=-1))) %>%
#             ggplot(aes(fill = gender, 
#                        ymax = ymax,
#                        ymin = ymin,
#                        xmax = 4,
#                        xmin = 3)) +
#             geom_rect() + 
#             coord_polar(theta='y') +
#             xlim(c(0,4)) +
#             scale_fill_brewer(type = 'qual',
#                               palette = 'Dark2') +
#             theme_minimal() +
#             theme(panel.grid=element_blank()) +
#             theme(axis.text=element_blank()) +
#             theme(axis.ticks=element_blank())
#     })
#     
#     output$aidPlot <- renderPlotly({
#         p <- aid %>%
#             filter(state_name == input$statePicker) %>%
#             ggparcoord(columns = 3:5,
#                        groupColumn = 2,
#                        showPoints = TRUE) +
#             theme_minimal() + 
#             scale_color_brewer(type = 'qual',
#                               palette = 'Dark2')
#         ggplotly(p)
#     })
#     
#     output$aidScatterPlot <- renderPlotly({
#         p <- aid %>%
#             filter(state_name == input$statePicker) %>%
#             arrange(cost_after_aid) %>%
#             ggplot(aes(x = percent_receiving_aid, 
#                        y = acceptance_rate,
#                        text = paste('School: ', display_name,
#                                     '<br>Acceptance rate: ', acceptance_rate, '%',
#                                     '<br>Percent receiving aid: ', percent_receiving_aid, '%',
#                                     '<br>Cost after aid: ', format(cost_after_aid, big.mark = ',', scientific = FALSE), '$'))) + 
#             geom_point(aes(size = cost_after_aid, fill = display_name, alpha = 0.5)) + 
#             ylab('Acceptance Rate') + 
#             xlab('Percent Receiving Aid') +
#             theme(legend.title = element_text('Schools')) + 
#             theme_bw()
#         ggplotly(p, tooltip = 'text') 
#     })
#     
#     output$incomeByStatePlot <- renderPlot({
#         plot_usmap(data = income, values = "AvgIncome", color = "white") + 
#             scale_fill_continuous(name = "Average Income",
#                                   label = scales::comma,
#                                   low = 'white',
#                                   high = 'seagreen') + 
#             theme(legend.position = "right")
#     })
#     
#     output$unempByStatePlot <- renderPlot({
#         plot_usmap(data = income, values = "AvgUnemployment", color = "white") + 
#                      scale_fill_continuous(name = "Average Income",
#                                            label = scales::comma,
#                                            low = 'white',
#                                            high = 'salmon') + 
#                      theme(legend.position = "right")
#     })
#     
#     output$avgAceptancePlot <- renderPlot({
#         rangeplot_dat <- school %>% filter(overall_rank < 150) %>%
#             mutate(group = ifelse(overall_rank < 50, "top1_50", ifelse(overall_rank > 100, "top100_150", "top51_100"))) %>%  
#             group_by(group) %>% 
#             summarize(ave_aid = mean(percent_receiving_aid, na.rm = TRUE),
#                       ave_acceptance = mean(acceptance_rate,na.rm = TRUE))
#         rangeplot_dat$group <- factor(rangeplot_dat$group, levels = c("top1_50", "top51_100", "top100_150"))
#         data_long <- gather(rangeplot_dat, criteria, value, ave_aid:ave_acceptance, factor_key=TRUE)
#         ggplot(data_long, aes(fill=criteria, y=value, x=group)) + 
#             geom_bar(position="dodge", stat="identity") + 
#             labs(title= "Percent acceptance rate and percent receiving aid \nof the top 150 public universities in the US",
#                  y="percent (%)", x = "group") +
#             theme_linedraw(base_size = 14) + 
#             scale_fill_brewer(type = 'qual',
#                               palette = 'Dark2') + 
#             theme(plot.title = element_text(hjust = 0.5, size = 15)) 
#     })
#     
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)
