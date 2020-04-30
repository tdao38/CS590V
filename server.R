server <- function(input, output, session) {
  #browser()
  sendSweetAlert(
    session = session,
    title = 'US Higher Education Dashboard',
    text = tags$span(
      icon('school', "fa-3x"),
      tags$hr(),
      tags$h4('This dashboard provides an overview about higher education in the United States.'), 
      tags$h4('It can be an useful tool for people interested in exploring the overall academics scenario in the US,
              especially for high school students or international students, who are in the process of
              choosing potential colleges or estimating the scope of the college application competition.'),
      tags$hr(),
      tags$h4('We use data from US News university data and US Census data for this visualization.'),
      tags$hr(),
      tags$h4('Thank you for choosing our tool. We hope you have a great experience with the dashboard!'),
      tags$hr(),
      tags$h5(tags$b('Authors: '), 'Emily Dzwil, Thu Dao, Trang Tran'),
      tags$h5('Final Project for CS590V - Data Visualization'),
      tags$h5('College of Information & Computer Sciences'),
      tags$h5('April, 2020')
    ),
    width = '50%',
    btn_colors = '#881c1c',
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
        background = styleColorBar(selectedData()$sat_avg, '#F2B349'),
        backgroundSize = '95% 80%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      ) %>%
      formatStyle(
        'act_avg',
        background = styleColorBar(selectedData()$act_avg, '#B998B6'),
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
          tags$b('Tuition: '), paste0('$', format(subset_df$tuition, big.mark = ',', scientific = FALSE)),
          tags$br(),
          tags$b('Percent Receiving Aid: '), paste0(subset_df$percent_receiving_aid, '%'),
          tags$br(),
          tags$b('Cost after aid: '), paste0('$',format(subset_df$cost_after_aid, big.mark = ',', scientific = FALSE))
        ),
        width = '40%',
        btn_colors = '#881c1c',
        btn_labels = "Close",
        html = TRUE
      )
    }
  })  
  
  output$school_type_plot <- renderPlotly({
    school_type_plot <- school_type %>%
      filter(state_name %in% input$state_input) %>%
      group_by(state_name) %>%
      mutate(countT = sum(count)) %>%
      group_by(institutional_control, add = TRUE) %>%
      mutate(pct=round(100*count/countT)) %>%
      ggplot(aes(x = state, 
                 y = pct, 
                 fill = institutional_control,
                 width = 0.3,
                 text = paste('State: ', state.name[match(state, state.abb)],
                              '<br>', pct, '% ', institutional_control, ' colleges'))) +
      geom_col(position = 'stack', width = 0.4) +
      ylab('Percentage') +
      xlab('State(s)') +
      scale_fill_manual(values = c('#cc7d7d', '#8DBFBB')) +
      theme_bw()

    ggplotly(school_type_plot, tooltip = 'text') %>%
      layout(legend = list(orientation = "h", 
                           x = 0.30,  
                           y = 10)) 
  })
  
  output$aidScatterPlot <- renderPlotly({
    #browser()
    selected_cost_type <- input$cost_type
    p <- selectedData() %>%
      ggplot(aes(x = percent_receiving_aid,
                 y = acceptance_rate,
                 text = paste('School: ', display_name,
                              '<br>Acceptance rate: ', paste0(acceptance_rate, '%'),
                              '<br>Percent receiving aid: ', paste0(percent_receiving_aid, '%'),
                              '<br>Tuition: ', paste0('$', format(tuition, big.mark = ',', scientific = FALSE)),
                              '<br>Cost after aid: ', paste0('$', format(cost_after_aid, big.mark = ',', scientific = FALSE))))) +
      geom_point(aes(size = selectedData()[[selected_cost_type]], fill = institutional_control, alpha = 1.2)) +
      ylab('Acceptance Rate') +
      xlab('Percent Receiving Aid') +
      scale_fill_manual(values = c('#cc7d7d', '#8DBFBB'))+
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
      addPolygons(fillColor = '#BE5959',
                  fillOpacity = 0.2,
                  color='#881c1c',
                  weight = 0.3,
                  smoothFactor = 0.15) %>%
      addMarkers(~selectedData()$long, ~selectedData()$lat, label = selectedData()$display_name)
  })
  
  output$radioState <- renderUI({
    options <- c(state.abb[match(input$state_input, state.name)])
    # The options are dynamically generated on the server
    prettyRadioButtons(
      inputId = 'state_radio',
      label = 'More facts about',
      choices = options,
      shape = 'round',
      status = 'danger',
      inline = TRUE,
      animation = 'smooth'
    )
  })
  
  output$incomeBox <- renderValueBox({
    #browser()
    income %>%
      filter(state_name == state.name[match(input$state_radio, state.abb)]) %>%
      #select(AvgIncome) %>%
      .$avg_income %>%
      scales::dollar() %>%
      valueBox(
        #subtitle = "Average Income",
        subtitle = renderUI({
          HTML(paste("State Average Income", "$61,372 (U.S)", sep="<br/>"))
        }),
        color = "red",
        icon = icon("dollar-sign", class = "small_icon_test")
      )
  })
  
  output$unempBox <- renderValueBox({
    #browser()
    income %>%
      filter(state_name == state.name[match(input$state_radio, state.abb)]) %>%
      mutate(avg_unemployment = avg_unemployment/100) %>%
      #select(AvgUnemployment) %>%
      .$avg_unemployment %>%
      percent() %>%
      valueBox(
        #subtitle = "Unemployment 4.1% (U.S)",
        subtitle = renderUI({
          HTML(paste("State Unemployment", "4.1% (U.S)", sep="<br/>"))
        }),
        color = "blue",
        icon = icon("stats",lib='glyphicon', class = "small_icon_test")
      )
  })

  output$popBox <- renderValueBox({
    income %>%
      filter(state_name == state.name[match(input$state_radio, state.abb)]) %>%
      #select(Pop) %>%
      .$pop %>%
      format(big.mark=",",scientific=FALSE) %>%
      valueBox(
        #subtitle = "State Population",
        subtitle = renderUI({
          HTML(paste("State Population", "325,100,000 (U.S)", sep="<br/>"))
        }),
        color = "purple",
        icon = icon("users", class = "small_icon_test")
      )
  })
  
  output$race_plot <- renderPlotly({
    race %>%
      filter(state_name == state.name[match(input$state_radio, state.abb)]) %>%
      plot_ly(labels = ~race,
              values = ~pct,
              hovertemplate = "%{label}: %{percent}",
              name = '',
              marker = list(colors = c('#b998b6','#87B3D3','#8DBFBB','#F2B349','#cc7d7d'))) %>%
      add_pie(hole = 0.6) %>%
      hide_legend() %>%
      layout(annotations = list (text = 'Race <br> breakdown',
                                 font = list(size = 13),
                                 showarrow = F,
                                 xref = 'paper', x = 0.5,
                                 yref = 'paper', y = 0.5),
             margin = list(l = 20, r = 20, b = 0, t = 0, pad = 0))
  })
   
  output$job_type_plot <- renderPlotly({
    job_type %>%
      filter(state_name == state.name[match(input$state_radio, state.abb)]) %>%
      plot_ly(labels = ~job_type,
              values = ~pct,
              hovertemplate = "%{label}: %{percent}</b><extra></extra>",
              name = '',
              textposition = 'inside',
              marker = list(colors = c('#8DBFBB','#F2B349','#cc7d7d','#b998b6', '#87B3D3'))) %>%
      add_pie(hole = 0.6) %>%
      hide_legend() %>%
      layout(annotations = list (text = 'Job type <br> breakdown',
                                 font = list(size = 13),
                                 showarrow = F,
                                 xref = 'paper', x = 0.5,
                                 yref = 'paper', y = 0.5),
             margin = list(l = 20, r = 20, b = 0, t = 0, pad = 0))
      # layout(legend = list(x = 0.25,
      #                      y = 0.25))
  })
  # output$USincome <- renderValueBox({
  #   valueBox(
  #     subtitle = "U.S. Median Income (2017)",
  #     "$61,372",
  #     color = "green"
  #   )
  # })
  # 
  # 
  # output$USPop <- renderValueBox({
  #   valueBox(
  #     subtitle = "U.S. Population (2017)",
  #     "325.1 mil",
  #     color = "blue"
  #   )
  # })
  # 
  # 
  # output$USunem <- renderValueBox({
  #   valueBox(
  #     subtitle = "U.S. Unemployment (2017)",
  #     "4.1%",
  #     color = "orange"
  #   )
  # })
  # tabs_content <- reactive({
  #   #browser()
  #   req(input$state_input)
  #   chosen_state_abb <- state.abb[match(input$state_input, state.name)]
  #   #return()
  #   tabs_content <- list()
  #   # tabs_content$Title <- list()
  #   # tabs_content$Content <- list()
  #   for (i in seq(length(chosen_state_abb))){
  #     tabs_content[[i]] <- list(Title = chosen_state_abb[i], Content = 'Hellooo')
  #     #tabs_content <- c(tabs_content, to_append)
  #   }
  #   return(tabs_content)
  #   # tabs.content <- list(list(Title = "Tab1", Content = "Tab1 content"),
  #   #                      list(Title = "Tab2", Content = "Tab2 content"),
  #   #                      list(Title = "Tab3", Content = "Tab3 content"))
  # })
  
  # output$school_type_plot <- renderPlot({
  #   school_type %>%
  #     filter(state_name == state.name[match(tabs_content()[[1]]$Title, state.abb)]) %>%
  #     group_by(state_name) %>%
  #     mutate(countT = sum(count)) %>%
  #     group_by(institutional_control, add = TRUE) %>%
  #     mutate(pct=round(100*count/countT)) %>%
  #     ggplot(aes(x = state_name, y = pct, fill = institutional_control)) +
  #     geom_col(position = 'stack', width = 0.4) +
  #     scale_fill_brewer(palette = 'Pastel1') +
  #     theme_bw()
  # })
  
  
  # tabs.content <- list(list(Title = "Tab1", Content = "Tab1 content"),
  #                      list(Title = "Tab2", Content = "Tab2 content"),
  #                      list(Title = "Tab3", Content = "Tab3 content"))
  # 
  # output$tabs <- renderUI({
  #   browser()
  #   
  #     tabs <- lapply(1:length(tabs_content()), function(i) tabPanel(tabs_content()[[i]]$Title, 
  #                                                                   #tabs_content()[[i]]$Content,
  #                                                                   plotOutput('school_type_plot')))
  #     do.call(tabBox, tabs)
  # })
  
  
  
  
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
  # 
  # output$incomeBox <- renderValueBox({
  #   income %>%
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
  #   income %>%
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