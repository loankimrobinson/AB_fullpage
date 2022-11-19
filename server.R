source("global.R")


server <- function(input, output, session) {
  
  source(file.path("server", "schedule_server.R"),  local = TRUE)$value
  source(file.path("server", "analytic.R"),  local = TRUE)$value

}