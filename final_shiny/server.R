server <- function(input, output, session) {
  #browser()
  sendSweetAlert(
    session = session,
    title = 'US Higher Education Dashboard',
    text = tags$span(
      icon('school', "fa-3x"),
      tags$hr(),
      tags$h4('This visualization dashboard provides an overview about higher education in the United States.'), 
      tags$h4('It can be an useful tool for people interested in exploring the overall academics scenario in the US,
              especially for high school students or international students, who are in the process of
              choosing potential colleges or estimating the scope of the college application competition.'),
      tags$hr(),
      tags$h4('We use data from US News university data and US Census data for this visualization.'),
      tags$hr(),
      tags$h4('Thank you for choosing our tool. We hope you have a great experience with the dashboard!'),
      tags$hr(),
      tags$h5(tags$b('Created by: '), 'Emily Dzwil, Thu Dao, Trang Tran'),
      tags$h5('Final project for CS590V - Data Visualization'),
      tags$h5('April, 2020')
    ),
    width = '50%',
    btn_colors = 'salmon',
    btn_labels = "Let's explore!",
    html = TRUE
  )
  
  selectedData <- reactive({
    req(input$state_input,
        input$tuition_input,
        input$sat_input,
        input$act_input,
        input$ranking_input,
        input$inst_type_input)
    
    filtered_df <- school %>%
      filter((state_name %in% input$state_input) &
             (tuition >= input$tuition_input[1] | is.na(tuition)) &
             (tuition <= input$tuition_input[2] | is.na(tuition)) &
             (sat_avg >= input$sat_input[1] | is.na(sat_avg)) &
             (sat_avg <= input$sat_input[2] | is.na(sat_avg)) &
             (act_avg >= input$act_input[1] | is.na(act_avg)) &
             (act_avg <= input$act_input[2] | is.na(act_avg)) &
             (enrollment >= input$enrollment_input[1] | is.na(enrollment)) &
             (enrollment <= input$enrollment_input[2] | is.na(enrollment)) &
             (overall_rank <= input$ranking_input | is.na(overall_rank)) &
             (institutional_control %in% input$inst_type_input | is.na(institutional_control)))
    return(filtered_df)
  })

  #shared_df <- SharedData$new(selectedData)

  output$schools_tb_output <- renderDataTable({
    #browser()
    table_df <- selectedData() %>%
      select(overall_rank, display_name, state, sat_avg, act_avg) %>%
      arrange(overall_rank)
    DT::datatable(table_df,
                  rownames = FALSE,
                  colnames = c('Rank', 'School Name', 'State', 'Average SAT', 'Average ACT'),
                  selection = 'single') %>%
      formatStyle(
        'sat_avg',
        background = styleColorBar(selectedData()$sat_avg, 'lightsteelblue'),
        backgroundSize = '95% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'act_avg',
        background = styleColorBar(selectedData()$act_avg, 'lightpink'),
        backgroundSize = '95% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'display_name',
        cursor = 'pointer'
      )
  })
  
  observeEvent(input$schools_tb_output_cell_clicked,{
    #browser()
    #if(is.null(input$schools_tb_output_cell_clicked)){
    if(length(input$schools_tb_output_cell_clicked) == 0){
      return()
    } else {
      #row_index <- input$schools_tb_output_rows_selected
      #subset_df <- selectedData()[row_index,]
      school_chosen <- input$schools_tb_output_cell_clicked$value
      subset_df <- selectedData() %>% filter(display_name == school_chosen)
      
      sendSweetAlert(
        session = session,
        title = paste0(subset_df$display_name),
        text = tags$span(
          tags$img(src = subset_df$primary_photo),
          # tags$h3("With HTML tags",
          #         style = "color: steelblue;"),
          tags$br(),
          tags$b('Location: '), paste0(subset_df$city, ', ', subset_df$state_name),
          tags$br(),
          tags$b('Undergraduate enrollment: '), paste0(format(subset_df$enrollment, big.mark = ',', scientific = FALSE)),
          tags$br(),
          tags$b('Acceptance rate: '), paste0(subset_df$acceptance_rate, '%'),
          tags$br(),
          tags$b('Tuition: '), paste0(format(subset_df$tuition, big.mark = ',', scientific = FALSE), '$'),
          tags$br(),
          tags$b('Percent Receiving Aid: '), paste0(subset_df$percent_receiving_aid, '%'),
          tags$br(),
          tags$b('Cost after aid: '), paste0(format(subset_df$cost_after_aid, big.mark = ',', scientific = FALSE), '$')
        ),
        width = '40%',
        html = TRUE
      )
    }
  })  
  
  output$aidScatterPlot <- renderPlotly({
    #browser()
    selected_cost_type <- input$cost_type
    p <- selectedData() %>%
      ggplot(aes(x = percent_receiving_aid,
                 y = acceptance_rate,
                 text = paste('School: ', display_name,
                              '<br>Acceptance rate: ', acceptance_rate, '%',
                              '<br>Percent receiving aid: ', percent_receiving_aid, '%',
                              '<br>Tuition: ', format(tuition, big.mark = ',', scientific = FALSE), '$',
                              '<br>Cost after aid: ', format(cost_after_aid, big.mark = ',', scientific = FALSE), '$'))) +
      geom_point(aes(size = selectedData()[[selected_cost_type]], fill = institutional_control, alpha = 0.5)) +
      ylab('Acceptance Rate') +
      xlab('Percent Receiving Aid') +
      #scale_size_continuous(range = c(4, 7)) +
      # scale_y_continuous(expand=c(0,0)) +
      # coord_cartesian(clip = 'off') +
      #scale_color_discrete(name = 'Institution Type') +
      #theme(legend.title = element_text('Schools')) +
      #theme(legend.position = 'none') + 
      theme_bw()
    ggplotly(p, tooltip = 'text') %>%
      #hide_legend()
      layout(legend = list(orientation = "h", 
                           x = 0.35,  
                           y = -0.25)) 
  })
  
  
  output$map <- renderLeaflet({
    spatial_df <- merge(states, selectedData(), by.x = "NAME", by.y = "state_name", duplicateGeoms = T)
    spatial_df@data <- spatial_df@data %>%
      filter(NAME %in% input$state_input) %>% 
      distinct(NAME)
    leaflet(spatial_df) %>% 
      addTiles() %>% 
      addProviderTiles(providers$Stamen.TonerLite) %>% 
      setView(-98.483330, 38.712046, zoom = 4) %>%
      addPolygons(fillColor = "#881c1c",
                  fillOpacity = 0.2,
                  color="#881c1c",
                  weight = 0.3,
                  smoothFactor = 0.15) %>%
      addMarkers(~selectedData()$long, ~selectedData()$lat, label = selectedData()$display_name)
  })
  
  
  output$popBox <- renderValueBox({
    income_df %>%
      filter(state == tolower(input$state_input)) %>%
      #select(Pop) %>%
      .$Pop %>%
      format(big.mark=",",scientific=FALSE) %>%
      valueBox(
        subtitle = "State Population",
        color = "blue",
        icon = icon("users")
      )
  })
  
  output$USPop <- renderValueBox({
    valueBox(
      subtitle = "U.S. Population (2017)",
      "325.1 mil",
      color = "blue"
    )
  })
  

  output$incomeBox <- renderValueBox({
    income_df %>%
      filter(state == tolower(input$state_input)) %>%
      #select(AvgIncome) %>%
      .$AvgIncome %>%
      scales::dollar() %>%
      valueBox(
        subtitle = "Average Income",
        color = "green",
        icon = icon("dollar-sign")
      )
  })
  
  output$USincome <- renderValueBox({
    valueBox(
      subtitle = "U.S. Median Income (2017)",
      "$61,372",
      color = "green"
    )
  })

  output$unempBox <- renderValueBox({
    income_df %>%
      filter(state == tolower(input$state_input)) %>%
      mutate(AvgUnemployment = AvgUnemployment/100) %>%
      #select(AvgUnemployment) %>%
      .$AvgUnemployment %>%
      percent() %>%
      valueBox(
        subtitle = "Average Unemployment",
        color = "orange",
        icon = icon("stats",lib='glyphicon')
      )
  })
  
  output$USunem <- renderValueBox({
    valueBox(
      subtitle = "U.S. Unemployment (2017)",
      "4.1%",
      color = "orange"
    )
  })
  
  
  
  
  
  
  # observeEvent(input$map_shape_click, {
  #   spatial_df <- merge(states, selectedData(), by.x = "NAME", by.y = "state_name", duplicateGeoms = T)
  #   spatial_df@data <- spatial_df@data %>%
  #     filter(NAME %in% input$state_input) %>% 
  #     filter(NAME %in% input$map_shape_click) %>% 
  #     distinct(NAME)
  #   print(spatial_df@data)
  # }) 

  
  

  

  # output$distPlot <- renderPlot({
  #   # generate bins based on input$bins from ui.R
  #   all_states <- map_data("state")
  #   states_chosen <- tolower(input$statePicker)
  #   states_positive  <- c()
  #   states_positive  <- c(states_positive, states_chosen)
  #   ggplot(all_states, aes(x=long, y=lat, group = group)) +
  #     geom_polygon(fill="grey", colour = "white") +
  #     geom_polygon(fill="cornflowerblue", data = filter(all_states, region %in% states_positive)) +
  #     theme_nothing()
  # })
  # 
  # output$popBox <- renderValueBox({
  #   income_df %>%
  #     filter(state == tolower(input$statePicker)) %>%
  #     #select(Pop) %>%
  #     .$Pop %>%
  #     format(big.mark=",",scientific=FALSE) %>%
  #     valueBox(
  #       subtitle = "Total Population",
  #       color = "blue",
  #       icon = icon("users")
  #     )
  # })
  # 
  # output$incomeBox <- renderValueBox({
  #   income_df %>%
  #     filter(state == tolower(input$statePicker)) %>%
  #     #select(AvgIncome) %>%
  #     .$AvgIncome %>%
  #     scales::dollar() %>%
  #     valueBox(
  #       subtitle = "Average Income",
  #       color = "green",
  #       icon = icon("dollar-sign")
  #     )
  # })
  # 
  # output$unempBox <- renderValueBox({
  #   income_df %>%
  #     filter(state == tolower(input$statePicker)) %>%
  #     mutate(AvgUnemployment = AvgUnemployment/100) %>%
  #     #select(AvgUnemployment) %>%
  #     .$AvgUnemployment %>%
  #     percent() %>%
  #     valueBox(
  #       subtitle = "Average Unemployment",
  #       color = "orange",
  #       icon = icon("stats",lib='glyphicon')
  #     )
  # })
  # 
  # output$schoolPlot <- renderPlot({
  #   school %>%
  #     filter(state_name == input$statePicker) %>%
  #     select(overall_rank, display_name, act_avg, sat_avg) %>%
  #     gather(key = 'measure', value = 'value', c(act_avg, sat_avg)) %>%
  #     ggplot(aes(x=reorder(display_name, -overall_rank), y = value, fill=measure)) +
  #     geom_bar(stat = 'identity', width = 0.5) +
  #     xlab('Schools (Ordered by Rank)') +
  #     ylab('Score') +
  #     facet_wrap(~measure,
  #                nrow = 1,
  #                scales = 'free_x') +
  #     coord_flip() +
  #     theme_minimal() +
  #     #scale_fill_manual(values = c("#D95F02","#7570B3"))
  #     scale_fill_brewer(type = 'qual',
  #                       palette = 'Dark2')
  # })
  # 
  # output$racePlot <- renderPlot({
  #   race %>%
  #     filter(State == input$statePicker) %>%
  #     arrange(fraction) %>%
  #     mutate(ymax = cumsum(fraction),
  #            ymin = c(0, head(ymax, n=-1))) %>%
  #     ggplot(aes(fill = race,
  #                ymax = ymax,
  #                ymin = ymin,
  #                xmax = 4,
  #                xmin = 3)) +
  #     geom_rect() +
  #     coord_polar(theta='y') +
  #     xlim(c(0,4)) +
  #     scale_fill_brewer(type = 'qual',
  #                       palette = 'Dark2') +
  #     theme_minimal() +
  #     theme(panel.grid=element_blank()) +
  #     theme(axis.text=element_blank()) +
  #     theme(axis.ticks=element_blank())
  # })
  # 
  # output$genderPlot <- renderPlot({
  #   gender %>%
  #     filter(State == input$statePicker) %>%
  #     arrange(fraction) %>%
  #     mutate(ymax = cumsum(fraction),
  #            ymin = c(0, head(ymax, n=-1))) %>%
  #     ggplot(aes(fill = gender,
  #                ymax = ymax,
  #                ymin = ymin,
  #                xmax = 4,
  #                xmin = 3)) +
  #     geom_rect() +
  #     coord_polar(theta='y') +
  #     xlim(c(0,4)) +
  #     scale_fill_brewer(type = 'qual',
  #                       palette = 'Dark2') +
  #     theme_minimal() +
  #     theme(panel.grid=element_blank()) +
  #     theme(axis.text=element_blank()) +
  #     theme(axis.ticks=element_blank())
  # })
  # 
  # output$aidPlot <- renderPlotly({
  #   p <- aid %>%
  #     filter(state_name == input$statePicker) %>%
  #     ggparcoord(columns = 3:5,
  #                groupColumn = 2,
  #                showPoints = TRUE) +
  #     theme_minimal() +
  #     scale_color_brewer(type = 'qual',
  #                        palette = 'Dark2')
  #   ggplotly(p)
  # })
  # 
  # output$aidScatterPlot <- renderPlotly({
  #   p <- aid %>%
  #     filter(state_name == input$statePicker) %>%
  #     arrange(cost_after_aid) %>%
  #     ggplot(aes(x = percent_receiving_aid,
  #                y = acceptance_rate,
  #                text = paste('School: ', display_name,
  #                             '<br>Acceptance rate: ', acceptance_rate, '%',
  #                             '<br>Percent receiving aid: ', percent_receiving_aid, '%',
  #                             '<br>Cost after aid: ', format(cost_after_aid, big.mark = ',', scientific = FALSE), '$'))) +
  #     geom_point(aes(size = cost_after_aid, fill = display_name, alpha = 0.5)) +
  #     ylab('Acceptance Rate') +
  #     xlab('Percent Receiving Aid') +
  #     theme(legend.title = element_text('Schools')) +
  #     theme_bw()
  #   ggplotly(p, tooltip = 'text')
  # })
  # 
  # output$incomeByStatePlot <- renderPlot({
  #   plot_usmap(data = income, values = "AvgIncome", color = "white") +
  #     scale_fill_continuous(name = "Average Income",
  #                           label = scales::comma,
  #                           low = 'white',
  #                           high = 'seagreen') +
  #     theme(legend.position = "right")
  # })
  # 
  # output$unempByStatePlot <- renderPlot({
  #   plot_usmap(data = income, values = "AvgUnemployment", color = "white") +
  #     scale_fill_continuous(name = "Average Income",
  #                           label = scales::comma,
  #                           low = 'white',
  #                           high = 'salmon') +
  #     theme(legend.position = "right")
  # })
  # 
  # output$avgAceptancePlot <- renderPlot({
  #   rangeplot_dat <- school %>% filter(overall_rank < 150) %>%
  #     mutate(group = ifelse(overall_rank < 50, "top1_50", ifelse(overall_rank > 100, "top100_150", "top51_100"))) %>%
  #     group_by(group) %>%
  #     summarize(ave_aid = mean(percent_receiving_aid, na.rm = TRUE),
  #               ave_acceptance = mean(acceptance_rate,na.rm = TRUE))
  #   rangeplot_dat$group <- factor(rangeplot_dat$group, levels = c("top1_50", "top51_100", "top100_150"))
  #   data_long <- gather(rangeplot_dat, criteria, value, ave_aid:ave_acceptance, factor_key=TRUE)
  #   ggplot(data_long, aes(fill=criteria, y=value, x=group)) +
  #     geom_bar(position="dodge", stat="identity") +
  #     labs(title= "Percent acceptance rate and percent receiving aid \nof the top 150 public universities in the US",
  #          y="percent (%)", x = "group") +
  #     theme_linedraw(base_size = 14) +
  #     scale_fill_brewer(type = 'qual',
  #                       palette = 'Dark2') +
  #     theme(plot.title = element_text(hjust = 0.5, size = 15))
  # })

}