
values <- reactiveValues(rfm_dt = plot_rfm_AB,
                         cust_segment = ABcust_segment,
                         sale_dt = ab_data,
                         dt_gender = ab_data,
                         dt_race = ab_data,
                         dt_income = ab_data,
                         dt_age = ab_data,
                         dt_map = ab_data)

output$AB_nonAB <- renderUI({
  radioButtons("ab",label = NULL, choices = c("AB Brand", "Non AB Brand"), inline = T, selected = "AB Brand")
})


observeEvent(input$ab,{
  if(input$ab == "AB Brand"){
    dt <- plot_rfm_AB
    dt1 <- ABcust_segment
  }else{
    dt <- plot_rfm_nonAB
    dt1 <- NonABcust_segment
  }
  values$rfm_dt <- dt
  values$cust_segment <- dt1
})

output$cus_seg <- plotly::renderPlotly({
  
  req(nrow(values$rfm_dt)>= 1)
  dt <- data.frame(values$rfm_dt)
  p <- bar_plot(plot_data = dt,
           y_var = "count",
           x_var = "seg_name",
           color_var = "seg_name",
           text = "count",
           legend = "bottom",
           type_var="bar",
           color_fill_out = c("#e3af32","#636466"),
           source = "summary_out",
           y_var_label = "",
           x_var_label = "",
           title = "",
           hovertext = "hovertext"
  )
  values$plot_seg <- p
})



output$tab1 <- renderUI({
  div(
    style = 'padding:0 10% 0 10%;',
    fluidRow(
      column(
        width = 6,
        wellPanel(
          style = 'background-color:rgba(250,250,250,1);text-align:left;color:black;padding-top:0px;padding-bottom:0px;',
          h5("Customer Segment"),
          uiOutput("AB_nonAB"),
          fluidRow(
            column(
              width = 12,plotly::plotlyOutput("cus_seg", height = "350px", width = "100%"))
          )
        )
      )
    )
  )
})

#=======================================
plotly_event_out <- reactiveVal(NULL)

observeEvent(event_data("plotly_click", source = "summary_out"),{

  plotly_event_out(event_data("plotly_click", source = "summary_out")$x)
  if(!is.null(plotly_event_out())){

    dt <- values$rfm_dt
    dt <- dt[dt$seg_name == plotly_event_out(), ]
    values$seg_score <- dt

    dt_out <- values$cust_segment
    dt_out <- dt_out[dt_out$seg_name == plotly_event_out(), ]

    sale_dt <- ab_data[ab_data$cust_id %in% dt_out$cust_id, ]
    values$sale_dt <- sale_dt
    
    dt_gender <- get_plot_dt(sale_dt, "gender")
    values$dt_gender <- dt_gender
    
    dt_race <- get_plot_dt(sale_dt, "race")
    values$dt_race <- dt_race
    
    dt_income <- get_plot_dt(sale_dt, "income_text")
    values$dt_income <- dt_income
    
    dt_age <- get_plot_dt(sale_dt, "age_group")
    values$dt_age <- dt_age
    
    dt_map <- get_map_dt(sale_dt)
    values$dt_map <- dt_map


  }

})


observeEvent(plotly_event_out(),{
  if(nrow(values$sale_dt) >= 1 && !is.null(values$seg_score)){

    output$segname_text <- renderText({
      HTML(paste0(input$week, "    ", paste0(plotly_event_out(), " Customer Segment ")))
    })
    
    output$seg_text <- renderText({
      HTML(paste0(plotly_event_out(), " (", sprintf("%.0f", values$seg_score$per), "%)"))
    })
    
   
    
    output$map <- renderLeaflet({
      
      print(values$dt_map )
      dt <- values$dt_map 
      
      factpal <- colorFactor(heat.colors(36), dt$prod_name)
      dt$radius <- dt$unit_price * dt$qty*1000
      
      fig_map  <-  leaflet::leaflet(dt) %>%
        setView(lng = -93.85, lat = 37.45, zoom = 5) %>% 
        addProviderTiles( "CartoDB.Voyager") %>% # Esri.WorldStreetMap
        addCircles(lng = ~lng,
                   lat = ~lat,
                   layerId =  ~ city,
                   weight = 1,
                   radius = ~ radius,
                   fillOpacity = 0.7,
                   popup = ~labels,
                   color = ~factpal(prod_name),
                   #label = ~labels,
                   #fillColor = "red",
                   stroke = FALSE,
                   group = ~city
        ) %>% addResetMapButton() %>%
        addLayersControl(overlayGroups = c("city")) %>%
        leaflet.extras::addSearchFeatures(
          targetGroups = "city",
          options = searchFeaturesOptions(
            zoom=7, 
            openPopup = TRUE, 
            firstTipSubmit = TRUE,
            autoCollapse = TRUE, 
            hideMarkerOnCollapse = TRUE,
          )) %>%  leaflet.extras::addStyleEditor(position = "bottomright")
      values$plot_region <- fig_map
    })
    
    output$race_out <- plotly::renderPlotly({
      p <- bar_plot(plot_data = values$dt_race,
               y_var = "count",
               x_var = "race",
               color_var = "race",
               text = "count",
               legend = "bottom",
               type_var="bar",
               color_fill_out = c("#e3af32","#636466"),
               source = "summary_out",
               y_var_label = "",
               x_var_label = "",
               title = "",
               hovertext = "hovertext"
      )
      values$race_out <- p
    })
    
    output$gender_out <- plotly::renderPlotly({
      p <- pie_plot(dt = values$dt_gender, label = "gender", value = "count", hovertext = "hovertext",colors = c("#636466","#e3af32"))
      values$gender_out <- p

    })
    
    output$age_out <- plotly::renderPlotly({
      p <- bar_plot(plot_data = values$dt_age,
                    y_var = "count",
                    x_var = "age_group",
                    color_var = "age_group",
                    text = "count",
                    legend = "bottom",
                    type_var="bar",
                    color_fill_out = c("#e3af32","#636466"),
                    source = "summary_age_group",
                    y_var_label = "",
                    x_var_label = "",
                    title = "",
                    hovertext = "hovertext"
      )
      values$age_out <- p
    })
    
    output$income_out <- plotly::renderPlotly({
      p <- bar_plot(plot_data = values$dt_income,
                    y_var = "count",
                    x_var = "income_text",
                    color_var = "income_text",
                    text = "count",
                    legend = "bottom",
                    type_var="bar",
                    color_fill_out = c("#e3af32","#636466"),
                    source = "summary_income_text",
                    y_var_label = "",
                    x_var_label = "",
                    title = "",
                    hovertext = "hovertext"
      )
      values$income_out <- p
    })
  }
})





output$tab2 <- renderUI({
  req(nrow(values$selected_schedule)==1) 
  home_team_id <- toupper(values$selected_schedule$home_team_id)
  away_team_id <- toupper(values$selected_schedule$away_team_id)
  div(
    style = 'padding:0 10% 0 10%;',
      fluidRow(column(width = 12,
                      wellPanel(
                        style = 'background-color:rgba(250,250,250,0.4);text-align:center;color:black;padding-top:0px;padding-bottom:0px;',
                        fluidRow(
                          column(width = 8,h2(htmlOutput("segname_text"))),
                          column(width = 2, tags$button(
                            id = values$selected_schedule$home_team_id,
                            class = 'btn action-button',
                            style = 'background-color:rgba(0,0,0,0);',
                            img(
                              src = values$selected_schedule$home_team_logo,
                              width = '80%',height = "70px",
                              style = ''
                            ),
                            onclick ="window.open('https://www.ab-inbev.com/','_blank','resizable,height=260,width=370')"
                          )),
                          column(width = 2,
                                 tags$button(
                                   id = values$selected_schedule$away_team_id,
                                   class = 'btn action-button',
                                   style = 'background-color:rgba(0,0,0,0);',
                                   img(
                                     src = values$selected_schedule$away_team_logo,
                                     width = '80%',height = "70px",
                                     style = ''
                                   ),
                                   onclick ="window.open('https://www.ab-inbev.com/','_blank','resizable,height=260,width=370')"
                                 ))
                        )
                      )
      )
     ),
        wellPanel(
          style = 'background-color:rgba(250,250,250,1);text-align:center;color:black;padding-top:0px;padding-bottom:0px;',
          fluidRow(
            column(
              width = 12,
              h1(htmlOutput("seg_text")))
          ),
          fluidRow(
            column(
              width = 6,leaflet::leafletOutput("map",width = "100%", height = "600px")
              ),
            column(
              width = 6,
              fluidRow(
                column(6,plotly::plotlyOutput("race_out", width = "100%", height = "300px"),
                       plotly::plotlyOutput("age_out", width = "100%", height = "300px")),
                column(6,plotly::plotlyOutput("gender_out", width = "100%", height = "300px"),
                        plotly::plotlyOutput("income_out", width = "100%", height = "300px"))
              )
            )
          )
        )
      )
})



