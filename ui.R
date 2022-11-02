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
    'Location' = 'location',
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
    menu = 'location',
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
      img = 'webhero-home.jpeg'
    )
  )
)