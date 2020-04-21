library(shinydashboard)
library(tidyverse)
library(usmap)
library(ggplot2)
library(ggmap)
library(ggrepel)
library(GGally)
library(plotly)
library(shiny)
library(scales)
library(shinyWidgets)
library(shinythemes)
library(shinyjs)
library(janitor)
library(DT)


# Load data
school <- read_csv('data/school.csv')
census <- read_csv('data/census17.csv')
#census_p <- read_csv('data/ss15pusa.csv')

# Clean
school <- school %>% clean_names()
school$state_name <- state.name[match(school$state, state.abb)]
school <- school %>% 
    filter(overall_rank >= 1)

# school_aid <- school %>% select(display_name, state, state_name, overall_rank, acceptance_rate, percent_receiving_aid, cost_after_aid)
# school_aid_complete <- school_aid[complete.cases(school_aid),]

# school_score <- school %>% select(overall_rank, display_name, state_name, act_avg, sat_avg)
# school_score <- school_score[complete.cases(school_score),]

# write.csv(school_aid, file = 'school_aid.csv')
# write.csv(school_aid_complete, file = 'school_aid_complete.csv')
# write.csv(school_score, file = 'school_score.csv')

# Income process
income_df <- income <- census %>% 
    group_by(State) %>%
    summarize(AvgIncome = mean(Income),
              AvgUnemployment = mean(Unemployment),
              Pop = sum(TotalPop)) %>%
    rename(state = 'State')
income_df$state <- tolower(income_df$state)
income_df$AvgIncome <- round(income_df$AvgIncome)
income_df$AvgUnemployment <- round(income_df$AvgUnemployment)

# Race
race <- census %>%
    group_by(State) %>%
    summarize(Hispanic = mean(Hispanic),
              White = mean(White),
              Black = mean(Black),
              Native = mean(Native),
              Asian = mean(Asian),
              Pacific = mean(Pacific)) %>%
    gather(key = 'race', value = 'pct', c(Hispanic, White, Black, Native, Asian, Pacific))
race$pct <- round(race$pct)

race <- race %>%
    group_by(State) %>%
    mutate(fraction = pct/100)

# Gender
gender <- census %>%
    group_by(State) %>%
    summarize(Male = mean(Men),
              Female = mean(Women),
              male = Male/(Male+Female),
              female = Female/(Male+Female)) %>%
    select(State, male, female) %>%
    gather(key = 'gender', value = 'fraction', c(male, female))

gender$fraction <- round(gender$fraction,2)

# Aid
aid <- school %>%
    select(state_name, display_name, acceptance_rate, percent_receiving_aid, cost_after_aid)

for (i in seq(3,5)){
    aid[,i] <- as.integer(aid[[i]])
}

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
