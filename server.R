server <- function(input, output, session) {
 observeEvent(input$btnhome,{
   print(input$btnhome)
  #tags$a("https://www.ab-inbev.com/", target = "_blank")
 })
}