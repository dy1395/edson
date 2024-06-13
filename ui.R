ui = fluidPage(
  
  title = 'Edson',
  theme = shinytheme('darkly'),
  
  # background image----
  setBackgroundImage(src = 'pele.png'),
  
  # activate shiny animate----
  withAnim(),
  
  div(
    style = 'padding:20px 10px 0 10px;',
    # header----
    fluidRow(
      # _qatar logo----
      column(
        width = 1,
        align = 'right',
        style = 'padding:15px 0 0 0;',
        img(
          src = 'qatar_logo.png',
          height = '40px'
        )
      ),
      # _title----
      column(
        width = 10,
        h2(
          'FIFA World Cup - Qatar 2022'
        )
      ),
      # _info button----
      column(
        width = 1,
        align = 'right',
        style = 'padding:20px 70px 0 0;',
        dropdown(
          div(
            align = 'center',
            # __title----
            h3(
              style = 'color:rgb(50,50,50);',
              img(
                src = 'tiger_ball.png',
                width = '90px'
              ),
              tags$span(
                style = 'padding-left:10px;',
                'Project Edson'
              )
            ),
            hr(),
            # __team----
            h5(style = 'color:rgb(50,50,50);', 'Our Team'),
            lapply(
              1:7,
              function(i) {
                tags$a(
                  href = c(
                    'https://www.princeton.edu/',
                    'https://www.princeton.edu/',
                    'https://www.princeton.edu/',
                    'https://www.princeton.edu/',
                    'https://www.princeton.edu/',
                    'https://www.princeton.edu/',
                    'https://www.princeton.edu/'
                  )[i],
                  target = '_blank',
                  actionBttn(
                    inputId = paste0('member', i),
                    label = c(
                      'Riley O\'Hare',
                      'Lauren Pak',
                      'Vince Etherton',
                      'Luis Guaman',
                      'Sena Cetin',
                      'Jorge Hernandez',
                      'Day Yi'
                    )[i],
                    style = 'minimal',
                    color = 'warning',
                    size = 'sm'
                  )
                )
              }
            )
          ),
          hr(),
          # __edson----
          div(
            align = 'justify',
            p(
              style = 'color:grey;',
              paste0(
                'Edson Arantes do Nascimento (aka Pelé) is widely ',
                'regarded as the greatest footballer ',
                'of all time. As a tribute to the legacy of ',
                'Pelé, Project Edson was named in his honor by the ',
                'aforementioned Princeton student and faculty team. ',
                'Our mission is a deep dive into selected ',
                'data and analytic components of the '
              ),
              tags$span(
                tags$a(
                  href = 'https://www.fifa.com/en/tournaments/mens/worldcup/canadamexicousa2026',
                  target = '_blank',
                  'FIFA World Cup 2026.'
                )
              ),
              paste0(
                ' We hope you enjoy this interactive exploration of the ',
                'prior World Cup as we are excited to innovate ',
                'for the next.'
              )
            )
          ),
          hr(),
          # __credits----
          div(
            align = 'center',
            h5(style = 'color:rgb(50,50,50);', 'Credits'),
            tags$a(
              href = 'https://www.fifa.com/en',
              target = '_blank',
              actionBttn(
                inputId = 'credit1',
                label = 'FIFA',
                style = 'minimal',
                color = 'primary',
                size = 'sm'
              )
            ),
            tags$a(
              href = 'https://fbref.com/en/',
              target = '_blank',
              actionBttn(
                inputId = 'credit2',
                label = 'FBref',
                style = 'minimal',
                color = 'primary',
                size = 'sm'
              )
            ),
            tags$a(
              href = 'https://www.youtube.com/@Foxsoccer',
              target = '_blank',
              actionBttn(
                inputId = 'credit3',
                label = 'FOX Soccer',
                style = 'minimal',
                color = 'primary',
                size = 'sm'
              )
            ),
            tags$a(
              href = 'https://www.linkedin.com/in/czheng05/',
              target = '_blank',
              actionBttn(
                inputId = 'credit4',
                label = 'Calvin Zheng',
                style = 'minimal',
                color = 'primary',
                size = 'sm'
              )
            ),
            tags$a(
              href = 'https://stock.adobe.com/',
              target = '_blank',
              actionBttn(
                inputId = 'credit5',
                label = 'Adobe Stock',
                style = 'minimal',
                color = 'primary',
                size = 'sm'
              )
            ),
            tags$a(
              href = 'https://csml.princeton.edu/',
              target = '_blank',
              actionBttn(
                inputId = 'credit6',
                label = 'Princeton CSML',
                style = 'minimal',
                color = 'primary',
                size = 'sm'
              )
            )
          ),
          style = 'jelly', 
          icon = icon('info'),
          status = 'warning', 
          width = '400px',
          size = 'sm',
          circle = FALSE,
          right = TRUE,
          animate = animateOptions(
            enter = animations$fading_entrances$fadeInUp,
            exit = animations$fading_exits$fadeOutDown
          )
        )
      )
    ),
    # filters----
    uiOutput('uiFilters'),
    # group summary----
    uiOutput('uiGroups'),
    # table----
    uiOutput('uiTable')
  ),
  
  # tootips----
  # _tooltip groups----
  lapply(
    1:8,
    function(i) {
      bsTooltip(
        id = paste0('group', i), 
        title = paste0('Group ', LETTERS[i]), 
        placement = 'top', 
        trigger = 'hover'
      )
    }
  ),
  
  # _tooltip countries----
  lapply(
    1:32,
    function(i) {
      bsTooltip(
        id = paste0('cntry', i), 
        title = teams$team[i], 
        placement = 'top', 
        trigger = 'hover'
      )
    }
  )
  
)
