source("global.R")


server <- function(input, output, session) {
  
  output$nfl_schedule <- renderUI(
    {
      div(
        style = 'padding:0 10% 0 10%;',
        fluidRow(
          column(
            width = 12,
            wellPanel(
              style = 'background-color:rgba(250,250,250,0.4);text-align:left;color:white;padding-top:0px;padding-bottom:0px;',
              h5("NFL SCHEDULE"),
              h2(htmlOutput("week_text")),
              fluidRow(
                column(
                  width = 2,uiOutput("week")),
                column(
                  width = 4,uiOutput("sch_byweek"))
              )
            ),
            wellPanel(
              style = 'background-color:rgba(250,250,250,0.7);text-align:left;color:white;',
              uiOutput("all_sche")
            )
          )
        )
      )
    })
  
  output$week <- renderUI({
    selectInput("week", label = NULL, choices = paste0("WEEK ", 1:18), selected = "WEEK 13", width = "200px")
  })
  
  output$week_text <- renderText({
    req(input$week)
    HTML(paste0("2022 - ", input$week))
  })
  
  output$sch_byweek <- renderUI({
    req(input$week)
    schedule <- nfl_schedule[nfl_schedule$game_week == input$week,]
    schedule_week <- unique(schedule$date_sche)
    selectInput("week_date", label = NULL, choices = schedule_week, selected = schedule_week[1], width = "400px")
  })
  
  output$all_sche <- renderUI({
    req(input$week, input$week_date)
    schedule <- nfl_schedule[nfl_schedule$date_sche == input$week_date,]
    values$schedule <- schedule
    lapply(1:nrow(schedule), function(i){
      actionBttn(
        inputId = paste0("schedule_", schedule$id[[i]]),
        label = HTML(schedule$label[[i]]),
        style = "stretch",
        color = 'primary',
        block = TRUE,
        size = 'md'
      )
    })
  })
  
  
  values <- reactiveValues(lastBtn = character(),
                           schedule = NULL)
  
  lapply(
    X = 1:nrow(nfl_schedule),
    FUN = function(i){
      observeEvent(input[[paste0("schedule_", nfl_schedule$id[[i]])]], {
        if (input[[paste0("schedule_", nfl_schedule$id[[i]])]] > 0) {
          values$lastBtn = paste0("schedule_", nfl_schedule$id[[i]])   
          print(paste("Last button clicked: ",  values$lastBtn))
          schedule <- values$schedule[values$schedule$id == sub("schedule_","", values$lastBtn),]
          values$selected_schedule <- schedule
        }
      })
    }
  )

  output$analytics_out <- renderUI({
  req(nrow(values$selected_schedule)==1) 
  home_team_id <- toupper(values$selected_schedule$home_team_id)
  away_team_id <- toupper(values$selected_schedule$away_team_id)
  div(
    style = 'padding:0 10% 0 10%;',
    fluidRow(
      column(
        width = 8,""),
      column(
        width = 2,
        wellPanel(
          style = 'background-color:rgba(250,250,250,0.0);border-color:rgba(250,250,250,0.0);text-align:center;color:black;padding-top:0px;padding-bottom:0px;',
          h2( home_team_id ),
          tags$button(
            id = values$selected_schedule$home_team_id,
            class = 'btn action-button',
            style = 'background-color:rgba(0,0,0,0);',
            img(
              src = values$selected_schedule$home_team_logo,
              width = '100%',
              style = ''
            ),
            onclick ="window.open('https://www.ab-inbev.com/','_blank','resizable,height=260,width=370')"
          )
        )
      ),
      column(
        width = 2,
        wellPanel(
          style = 'background-color:transparent;border-color:transparent;text-align:center;color:black;padding-top:0px;padding-bottom:0px;',
          h2( away_team_id ),
          tags$button(
            id = values$selected_schedule$away_team_id,
            class = 'btn action-button',
            style = 'background-color:rgba(0,0,0,0);',
            img(
              src = values$selected_schedule$away_team_logo,
              width = '100%',
              style = ''
            ),
            onclick ="window.open('https://www.ab-inbev.com/','_blank','resizable,height=260,width=370')"
          )
        )
      )
    )
  )
  })
    

  
  
  # output$lastButtonCliked <- renderText({
  #   print(paste("Last button clicked: ", rv$lastBtn))
  # })
  

}