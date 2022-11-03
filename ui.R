ui <- fullPage(
  
  opts = list(
    controlArrows = FALSE,
    fadingEffect = TRUE,
    fitToSection = TRUE,
    loopBottom = FALSE,
    loopHorizontal = TRUE,
    navigation = FALSE,
    scrollBar = FALSE,
    scrollOverflow = TRUE,
    scrollOverflowReset = TRUE,
    slidesNavigation = TRUE,
    verticalCentered = TRUE
  ),
  
  # menu tabs----
  menu = c(
    'Home' = 'home',
    'Game Schedule' = 'schedule',
    'Analytics' = 'analytic',
    'Recommendations' = 'recommendation',
    'Road to the Super Bowl' = 'road',
    'About Us' = 'about'
  ),
  # home section----
  fullSection(
    menu = 'home',
    center = TRUE,
      fullSlideImage(
          img = 'webhero-home.jpeg',
          div(
              style = 'padding:10vh 25% 0 25%;color:#e3af32;text-align:center;',
              tags$button(
              id = "btnhome",
              class = 'btn action-button',
              style = 'background-color:rgba(0,0,0,0);',
              img(
                  src = paste0('logo_1.svg'),
                  width = '60%',
                  style = ''
              ),
              onclick ="window.open('https://www.ab-inbev.com/','_blank','resizable,height=260,width=370')"
            ),
              hr(),
              h3('#FUTUREWITHMORECHEERS')
      )
    )
  ),
  fullSection(
    menu = 'schedule',
    center = TRUE,
    fullSlideImage(
      img = 'nfl.jpeg'
    )
  ),
  fullSection(
    menu = 'analytic',
    center = TRUE,
    fullSlideImage(
      img = 'Cheers.jpeg'
    )
  ),
  fullSection(
    menu = 'recommendation',
    center = TRUE,
    fullSlideImage(
      img = 'AB-InBev-logo.jpeg'
    )
  ),
  fullSection(
    menu = 'road',
    center = TRUE,
    fullSlideImage(
      img = 'q3earnings_news_banner.png'
    )
  ),
  fullSection(
    menu = 'about',
    center = TRUE,
    fullSlideImage(
      img = '',
      div(style = "padding-left:30px;padding-top:30px;",
          div(
            style = 'padding:0 17% 0 17%;',
            br(),
            h3(style = 'color:black;', 'Meet Our Team'),
            hr()
          ),
          fluidRow(
            column(width = 3,
                   HTML('<div style="padding-left:10px;padding-top:10px;">
                            <div class="card">
                            <img src="loan.jpg" style="width:150px;height:250px;">
                            <h4 style="font-weight: bold;" ><i>Loan Robinson</i></h4>
                            <h4 style="font-weight: bold" ><i>Managing Director</i></h4>
                            </div>
                            </div>
                             ')),
            column(width = 3,
                   HTML('<div style="padding-left:10px;padding-top:10px;">
                            <div class="card">
                            <img src="loan.jpg" style="width:150px;height:250px;">
                             <h4 style="font-weight: bold" ><i>Loan Robinson</i></h4>
                             <h4 style="font-weight: bold" ><i>Managing Director</i></h4>
                             </div>
                             </div>
                             ')),
            column(width = 3,
                   HTML('<div style="padding-left:10px;padding-top:10px;">
                            <div class="card">
                            <img src="loan.jpg" style="width:150px;height:250px;">
                             <h4 style="font-weight: bold" ><i>Loan Robinson</i></h4>
                             <h4 style="font-weight: bold" ><i>Managing Director</i></h4>
                             </div>
                             </div>
                             ')),
            column(width = 3,
                   HTML('<div style="padding-left:10px;padding-top:10px;">
                            <div class="card">
                            <img src="loan.jpg" style="width:150px;height:250px;">
                             <h4 style="font-weight: bold" ><i>Loan Robinson</i></h4>
                             <h4 style="font-weight: bold" ><i>Managing Director</i></h4>
                             </div>
                             </div>
                             '))
          ),
          fluidRow(
            column(width = 3,
                   HTML('<div style="padding-left:10px;padding-top:10px;">
                            <div class="card">
                            <img src="loan.jpg" style="width:150px;height:250px;">
                            <h4 style="font-weight: bold;" ><i>Loan Robinson</i></h4>
                            <h4 style="font-weight: bold" ><i>Managing Director</i></h4>
                            </div>
                            </div>
                             ')),
            column(width = 3,
                   HTML('<div style="padding-left:10px;padding-top:10px;">
                            <div class="card">
                            <img src="loan.jpg" style="width:150px;height:250px;">
                             <h4 style="font-weight: bold" ><i>Loan Robinson</i></h4>
                             <h4 style="font-weight: bold" ><i>Managing Director</i></h4>
                             </div>
                             </div>
                             ')),
            column(width = 3,
                   HTML('<div style="padding-left:10px;padding-top:10px;">
                            <div class="card">
                            <img src="loan.jpg" style="width:150px;height:250px;">
                             <h4 style="font-weight: bold" ><i>Loan Robinson</i></h4>
                             <h4 style="font-weight: bold" ><i>Managing Director</i></h4>
                             </div>
                             </div>
                             ')),
            column(width = 3,
                   HTML('<div style="padding-left:10px;padding-top:10px;">
                            <div class="card">
                            <img src="loan.jpg" style="width:150px;height:250px;">
                             <h4 style="font-weight: bold" ><i>Loan Robinson</i></h4>
                             <h4 style="font-weight: bold" ><i>Managing Director</i></h4>
                             </div>
                             </div>
                             '))
          ),
          div(
            style = 'padding:0 17% 0 17%;',
            br(),br(),
            h3(style = 'color:black;', ''),
            hr()
          ),
      )
    )
  )
)