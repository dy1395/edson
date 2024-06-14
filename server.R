server = function(input, output, session) {
  
  # >>>>>>>>>>>>>>>>>>>>>----
  # 0. reactive values----
  ed = reactiveValues(
    tbl = 1, # show table (0,1)
    so = 2, # sort order (1=asc, 2=desc),
    gr = 0, # selected group (0=all, 1 to 8),
    tm = 0, # selected team (0=all, 1 to 32)
    gm = 0, # selected game (1 to 64)
    go = 1, # game opt (1=video, 2=metrics)
    gt = 0, # selected goal team (goals$t)
    gh = 0, # selected goal # (goals$h)
    gv = matrix(
      c(rep(0, 16)), ncol = 8
    ), # selected game goal video available (0,1)
  )
  
  # 0.1. filter icon----
  output$uiFilterIcon = renderUI(
    if (ed$gr > 0 | ed$tm > 0) {
      tags$button(
        id = 'filterx',
        class = 'btn action-button',
        style = 'background-color:rgba(0,0,0,0); padding:2px; margin-right:20px;',
        img(
          src = 'filterx.png',
          height = '60px'
        )
      )
    }
  )
  
  # 0.2. filters----
  output$uiFilters = renderUI(
    {
      div(
        align = 'center',
        fluidRow(
          # _filter all----
          column(
            width = 2,
            align = 'right',
            style = 'padding:30px 0 0 0;',
            uiOutput('uiFilterIcon')
          ),
          # _filter groups----
          lapply(
            0:7,
            function(i) {
              column(
                width = 1,
                style = 'padding:3px;',
                div(
                  style = paste0(
                    'border:solid ',
                    ifelse(ed$gr == i + 1, 'magenta', 'white'), ' 1px; ',
                    'border-radius:0px; ',
                    'padding:5px 0 5px 0; ',
                    'background-color: rgba(150, 150, 150, 0.6);'
                  ),
                  # _filter group----
                  div(
                    align = 'left',
                    actionBttn(
                      inputId = paste0('group', i + 1),
                      label = LETTERS[i + 1],
                      style = 'stretch',
                      color = 'default',
                      size = 'xs'
                    )
                  ),
                  # _filter country row 1----
                  div(
                    lapply(
                      1:2,
                      function(j) {
                        tags$button(
                          id = paste0('cntry', 4 * i + j),
                          class = 'btn action-button',
                          style = 'background-color:rgba(0,0,0,0); padding:2px;',
                          img(
                            src = paste0('flags/', teams$code[4 * i + j], '.png'),
                            width = '25px'
                          )
                        )
                      }
                    )
                  ),
                  # _filter country row 2----
                  div(
                    lapply(
                      3:4,
                      function(j) {
                        tags$button(
                          id = paste0('cntry', 4 * i + j),
                          class = 'btn action-button',
                          style = 'background-color:rgba(0,0,0,0); padding:2px;',
                          img(
                            src = paste0('flags/', teams$code[4 * i + j], '.png'),
                            width = '25px'
                          )
                        )
                      }
                    )
                  )
                )
              )
            }
          )
        )
      )
    }
  )
  
  # >>>>>>>>>>>>>>>>>>>>>----
  # 1. ui table----
  output$uiTable = renderUI(
    if (ed$tbl == 1) {
      div(
        id = 'table',
        style = 'padding:10px 50px 0 50px;',
        # _1.1. table header----
        uiOutput('tblHeader'),
        # _1.2. table body----
        div(
          id = 'tableBody',
          style = paste0(
            'max-height:', ifelse(ed$gr > 0, '320px; ', '544px; '),
            'overflow-y:auto; ',
            'overflow-x:hidden; ',
            'padding:0 5px 0 5px;'
          ),
          uiOutput('tblBody')
        )
      )
    }
  )
  
  # >>>>>>>>>>>>>>>>>>>>>----
  # 2. ui tbl header----
  output$tblHeader = renderUI(
    {
      div(
        id = 'tableHeader',
        style = 'padding:0 5px 0 5px;',
        align = 'center',
        div(
          style = 'background-color:yellow;',
          fluidRow(
            lapply(
              1:5, 
              function(c) {
                column(
                  width = wdths[c],
                  align = c('center', 'right', 'center', 'left', 'center')[c],
                  style = c(
                    'padding:6px 0 0 0;',
                    'padding:2px 80px 2px 2px;',
                    'padding:2px;',
                    'padding:2px 2px 2px 70px;',
                    'padding:2px;'
                  )[c],
                  # the date column
                  if (c == 1) {
                    actionBttn(
                      inputId = 'dateBtn',
                      label = hdrs[1],
                      style = 'stretch',
                      color = 'success',
                      size = 'sm',
                      icon = icon(switch(ed$so, 'arrow-up', 'arrow-down'))
                    )
                  }
                  # all other columns
                  else {
                    h5(
                      style = 'color:#5bc86c;',
                      hdrs[c]
                    )
                  }
                )
              }
            )
          )
        )
      )
    }
  )
  
  # >>>>>>>>>>>>>>>>>>>>>----
  # 3. ui tbl body----
  output$tblBody = renderUI(
    {
      sty = 'padding:0; margin:8px 0 0 0; '
      gr = ifelse(ed$gr > 0, teams$group[4*(ed$gr-1)+1], 'z')
      tc = ifelse(ed$tm > 0, teams$code[ed$tm], 'zz')
      # endpoints of rows based on sort order
      a = switch(ed$so, 1, 64)
      b = switch(ed$so, 64, 1)
      # create the (up to) 64 rows of the table
      lapply(
        a:b, 
        function(r) {
          if (
            (ed$gr == 0 & ed$tm == 0) | 
            gr %in% c(games$group1[r], games$group2[r]) |
            tc %in% c(games$code1[r], games$code2[r])
          ) {
            div(
              # _3.0. banded colors----
              style = ifelse(
                r %% 2 == 1 | ed$gr > 0 | ed$tm > 0,
                case_when(
                  r <= 48 ~ 'background-color:rgba(100,100,100,0.5);',
                  r <= 56 ~ 'background-color:rgba(200,100,100,0.5);',
                  r <= 60 ~ 'background-color:rgba(100,200,100,0.5);',
                  r <= 62 ~ 'background-color:rgba(100,100,180,0.5);',
                  .default = 'background-color:rgba(0,215,255,0.5);'
                ),
                case_when(
                  r <= 48 ~ 'background-color:rgba(150,150,150,0.5);',
                  r <= 56 ~ 'background-color:rgba(250,100,100,0.5);',
                  r <= 60 ~ 'background-color:rgba(100,250,100,0.5);',
                  r <= 62 ~ 'background-color:rgba(100,100,255,0.5);',
                  .default = 'background-color:rgba(255,215,0,0.5);'
                )
              ),
              align = 'center',
              fluidRow(
                style = 'padding:5px 0 5px 0;',
                # _3.1. column date----
                column(
                  width = 2,
                  style = 'padding:2px;',
                  h5(
                    style = sty,
                    games$date[r]
                  )
                ),
                # _3.2. column team 1----
                column(
                  width = 3,
                  fluidRow(
                    style = 'padding:0 15px 0 5px;',
                    # __3.2.1. team name 1----
                    column(
                      width = 10,
                      align = 'right',
                      h5(
                        style = paste0(
                          sty,
                          'color:',
                          case_when(
                            games$winner[r] == 1 ~ 'lime;',
                            games$winner[r] == 2 ~ 'lightgrey;',
                            .default = 'yellow;',
                          )
                        ),
                        games$team1[r]
                      )
                    ),
                    # __3.2.2. flag 1----
                    column(
                      width = 2,
                      align = 'left',
                      style = 'padding-top:5px;',
                      img(
                        src = paste0('flags/', games$code1[r], '.png'),
                        width = '24px',
                        # ___3.2.2.1. flag border 1----
                        style = paste0(
                          'border:',
                          case_when(
                            games$winner[r] == 1 ~ 'solid ',
                            games$winner[r] == 2 ~ 'none ',
                            .default = 'dashed ',
                          ),
                          case_when(
                            games$winner[r] == 1 ~ 'lime ',
                            games$winner[r] == 2 ~ 'white ',
                            .default = 'yellow ',
                          ),
                          '2px; border-radius:12px;'
                        )
                      )
                    )
                  )
                ),
                # _3.3. column score----
                column(
                  width = 2,
                  div(
                    style = 'padding:2px 0 2px 0;',
                    # __3.3.1. shootout score 1----
                    if (games$shootout[r] == 1) {
                      actionBttn(
                        inputId = paste0('ss1_', r),
                        label = paste0(games$sscore1[r]),
                        style = 'stretch',
                        color = 'default',
                        block = FALSE,
                        size = 'sm'
                      )
                    },
                    # __3.3.2. regular score----
                    actionBttn(
                      inputId = paste0('game', r),
                      label = paste0(games$score1[r], ' - ', games$score2[r]),
                      style = 'jelly',
                      color = ifelse(r <= 48, 'warning', 'royal'),
                      block = FALSE,
                      size = 'sm',
                      icon = icon(ifelse(ed$gm == r, 'caret-up', 'caret-down'))
                    ),
                    # __3.3.3. shootout score 2----
                    if (games$shootout[r] == 1) {
                      actionBttn(
                        inputId = paste0('ss2_', r),
                        label = paste0(games$sscore2[r]),
                        style = 'stretch',
                        color = 'default',
                        block = FALSE,
                        size = 'sm'
                      )
                    }
                  )
                ),
                # _3.4. column team 2----
                column(
                  width = 3,
                  fluidRow(
                    style = 'padding:0 15px 0 5px;',
                    # __3.4.1. flag 2----
                    column(
                      width = 2,
                      align = 'right',
                      style = 'padding-top:5px;',
                      img(
                        src = paste0('flags/', games$code2[r], '.png'),
                        width = '24px',
                        # ___3.4.1.2. flag border 2----
                        style = paste0(
                          'border:',
                          case_when(
                            games$winner[r] == 1 ~ 'none ',
                            games$winner[r] == 2 ~ 'solid ',
                            .default = 'dashed ',
                          ),
                          case_when(
                            games$winner[r] == 1 ~ 'white ',
                            games$winner[r] == 2 ~ 'lime ',
                            .default = 'yellow ',
                          ),
                          '2px; border-radius:12px;'
                        )
                      )
                    ),
                    # __3.4.2. team name 2----
                    column(
                      width = 10,
                      align = 'left',
                      h5(
                        style = paste0(
                          sty,
                          'color:',
                          case_when(
                            games$winner[r] == 1 ~ 'lightgrey;',
                            games$winner[r] == 2 ~ 'lime;',
                            .default = 'yellow;',
                          )
                        ),
                        games$team2[r]
                      )
                    )
                  )
                ),
                # _3.5. column stage----
                column(
                  width = 2,
                  h5(
                    style = sty,
                    games$stage[r]
                  )
                )
              ),
              # _3.6. game detail----
              if (ed$gm == r) {
                uiOutput('gameDetail')
              }
            )
          }
        }
      )
    }
  )
  
  # >>>>>>>>>>>>>>>>>>>>>----
  # 4. ui game detail----
  output$gameDetail = renderUI(
    {
      div(
        style = 'padding:10px 3% 10px 3%;',
        align = 'center',
        id = paste0('tblRow', ed$gm),
        div(
          style = paste0(
            'padding:10px; ',
            'background-color:',
            ifelse(
              ed$gm %% 2 == 1,
              'rgba(100,100,100,0.8);',
              'rgba(150,150,150,0.8);'
            )
          ),
          fluidRow(
            style = 'padding:5px 0 0 0;',
            column(
              width = 3,
              style = 'padding:0 0 0 15px;',
              uiOutput('gameDetailL')
            ),
            column(
              width = 6,
              # _4.1. game option buttons----
              fluidRow(
                column(
                  width = 6,
                  style = 'padding:0 2px 5px 15px;',
                  # __4.1.1. video button----
                  actionBttn(
                    inputId = 'gmOpt1',
                    label = NULL,
                    style = switch(ed$go, 'simple', 'stretch'),
                    size = 'sm',
                    color = 'default',
                    block = TRUE,
                    icon = icon('film')
                  )
                ),
                column(
                  width = 6,
                  # __4.1.2. chart button----
                  style = 'padding:0 15px 5px 2px;',
                  actionBttn(
                    inputId = 'gmOpt2',
                    label = NULL,
                    style = switch(ed$go, 'stretch', 'simple'),
                    size = 'sm',
                    color = 'default',
                    block = TRUE,
                    icon = icon('chart-simple')
                  )
                )
              ),
              uiOutput('gameDetailC'),
              uiOutput('shtoutDetail')
            ),
            column(
              width = 3,
              style = 'padding:0 15px 0 0;',
              uiOutput('gameDetailR')
            )
          )
        )
      )
    }
  )
  
  # _4.2. game detail center----
  output$gameDetailC = renderUI(
    if (ed$go == 1) {
      # __4.2.1. video embed----
      # if video option selected
      if (ed$go == 1) {
        # if a team is selected
        if (ed$gt != 0) {
          # if the video is available
          if (ed$gv[ed$gt, ed$gh] == 1) {
            gg = goals |> 
              filter(g == ed$gm & t == ed$gt & h == ed$gh)
            ss = paste0(
              'https://www.dropbox.com/scl/fi/', gg$vid,
              '/goal', ed$gm, '_', ed$gt, '_', ed$gh, '.mp4', 
              '?rlkey=', gg$key,
              '&st=', gg$st,
              '&raw=1'
            )
            tags$video(
              src = ss,
              type = 'video/mp4',
              width = '100%',
              height = '300px',
              controls = TRUE,
              poster = 'poster.png',
              autoplay = TRUE
            )
          } 
          # __4.2.2. image embed----
          else {
            img(
              src = 'poster.png',
              style = 'max-width:100%; max-height:300px;'
            )
          }
        } 
        # __4.2.2. image embed----
        else {
          img(
            src = 'poster.png',
            style = 'max-width:100%; max-height:300px;'
          )
        }
      }
    } 
    # __4.2.3. chart----
    else {
      div(
        style = paste0(
          'border:solid white 1px; ',
          'background-color:rgba(255,255,255,0.4);'
        ),
        h5(
          style = 'padding:0; color:blue;',
          games$team1[ed$gm]
        ),
        highchartOutput(
          outputId = 'gmMetrics',
          height = '216px'
        ),
        h5(
          style = 'padding:0; color:red;',
          games$team2[ed$gm]
        )
      )
    }
  )
  
  # _4.3. game detail chart----
  output$gmMetrics = renderHighchart(
    {
      # __4.3.0. preprocessing----
      # ___4.3.0.1. game values----
      gg = games |> 
        filter(game == ed$gm) |> 
        select(score1, score2, xg1, xg2)
      tmax = ifelse(
        ed$gm <= 48, 
        95, 
        ifelse(
          games$shootout[ed$gm] == 1,
          120,
          95
        )
      )
      dur = 3000 # animation duration in msec
      
      # ___4.3.0.2. team 1 goals----
      g1a = goals |>
        filter(g == ed$gm & t == 1) |>
        select(time) |> 
        mutate(score = ifelse(row_number() == 1, 0.1, row_number() - 1))
      g1b = goals |>
        filter(g == ed$gm & t == 1) |>
        select(time) |> 
        mutate(score = ifelse(row_number() == 0, 0.1, row_number()))
      g1 = g1a |>
        bind_rows(g1b) |>
        bind_rows(
          tibble(
            time = c(0, tmax),
            score = c(0.1, ifelse(gg$score1 == 0, 0.1, gg$score1))
          )
        ) |>
        arrange(time, score)
      # ___4.3.0.3. team 2 goals----
      g2a = goals |>
        filter(g == ed$gm & t == 2) |>
        select(time) |> 
        mutate(score = ifelse(row_number() == 1, 0.1, row_number() - 1))
      g2b = goals |>
        filter(g == ed$gm & t == 2) |>
        select(time) |> 
        mutate(score = ifelse(row_number() == 0, 0.1, row_number()))
      g2 = g2a |>
        bind_rows(g2b) |>
        bind_rows(
          tibble(
            time = c(0, tmax), 
            score = c(0.1, ifelse(gg$score2 == 0, 0.1, gg$score2))
          )
        ) |>
        arrange(time, score)
      # ___4.3.0.4. team 1 xg----
      x1 = tibble(
        time = c(0, tmax),
        xG = c(games$xg1[ed$gm], games$xg1[ed$gm])
      )
      # ___4.3.0.5. team 2 xg----
      x2 = tibble(
        time = c(0, tmax),
        xG = c(games$xg2[ed$gm], games$xg2[ed$gm])
      )
      # __4.3.0. zero line----
      hchart(
        object = tibble(x = c(0,tmax), y = c(0,0)),
        zIndex = 9,
        name = 'zero',
        mapping = hcaes(x = x, y = y),
        type = 'line',
        dashStyle = 'solid',
        animation = list(
          duration = dur,
          easing = 'easeOutBounce'
        )
      ) |>
      # __4.3.1. goals team 1----
      hc_add_series(
        data = g1,
        zIndex = 1,
        name = games$team1[ed$gm],
        mapping = hcaes(x = time, y = score),
        type = 'area',
        animation = list(
          duration = dur,
          easing = 'easeOutBounce'
        )
      ) |> 
      # __4.3.2. goals team 2----
      hc_add_series(
        data = g2,
        zIndex = 2,
        name = games$team2[ed$gm],
        mapping = hcaes(x = time, y = -score),
        type = 'area',
        animation = list(
          duration = dur,
          easing = 'easeOutBounce'
        )
      ) |> 
      # __4.3.3. xg team 1----
      hc_add_series(
        data = x1,
        zIndex = 3,
        name = games$team1[ed$gm],
        mapping = hcaes(x = time, y = xG),
        type = 'line',
        dashStyle = 'Dash',
        animation = FALSE
      ) |>
      # __4.3.4. xg team 2----
      hc_add_series(
        data = x2,
        zIndex = 4,
        name = games$team2[ed$gm],
        mapping = hcaes(x = time, y = -xG),
        type = 'line',
        dashStyle = 'Dash',
        animation = FALSE
      ) |>
        # __4.3.9. chart options----
        hc_tooltip(
          enabled = FALSE
        ) |> 
        # ___4.3.9.1. colors----
        hc_colors(c('white', 'blue', 'red', 'lime', 'lime')) |>
        # ___4.3.9.2. x axis----
        hc_xAxis(
          labels = list(
            enabled = FALSE
          ),
          title = list(
            text = ''
          ),
          visible = FALSE
        ) |> 
        # ___4.3.9.3. y axis----
        hc_yAxis(
          labels = list(
            enabled = FALSE
          ),
          title = list(
            text = ''
          ),
          visible = FALSE
        ) |> 
        # ___4.3.9.4. plot options----
        hc_plotOptions(
          area = list(
            marker = list(enabled = FALSE)
          ),
          line = list(
            marker = list(enabled = FALSE)
          )
        )
    }
  )
  
  # _4.4. game detail left----
  output$gameDetailL = renderUI(
    {
      gg = goals |> 
        filter(g == ed$gm & t == 1) |> 
        arrange(h)
      xx = games$xg1[ed$gm]
      div(
        # __4.4.0. g & xg----
        actionBttn(
          inputId = 'g1',
          label = paste0('G: ', nrow(gg)),
          style = 'jelly',
          color = ifelse(ed$gm > 48, 'royal', 'warning'),
          size = 'sm'
        ),
        actionBttn(
          inputId = 'xg1',
          label = paste0('xG: ', xx),
          style = 'jelly',
          color = 'success',
          size = 'sm'
        ),
        hr(style = 'border-color:white; margin:5px 0 10px 0;'),
        # __4.4.1. goal buttons left----
        if (nrow(gg) > 0) {
          lapply(
            1:nrow(gg), 
            function(i) {
              if (gg$vid[i] != '0') {ed$gv[1, i] = 1}
              div(
                style = 'padding:2px;',
                align = 'right',
                actionBttn(
                  inputId = paste0('goal1_', i),
                  label = div(
                    icon(ifelse(gg$vid[i] != '0', 'film', 'circle-xmark')),
                    paste0(
                      gg$plyr_name[i], ' ', 
                      ifelse(gg$goal_type[i] == 'penalty', '(P) ', ''),
                      ifelse(gg$goal_type[i] == 'own-goal', '(OG) ', ''),
                      gg$time[i], 
                      ifelse(
                        !is.na(gg$atime[i]), 
                        paste0('+', gg$atime[i]),
                        ''
                      ),
                      '\' '
                    ), 
                    icon('futbol')
                  ),
                  style = ifelse(gg$vid[i] != '0', 'simple', 'bordered'),
                  size = 'xs',
                  color = ifelse(gg$vid[i] != '0', 'primary', 'default'),
                  block = FALSE
                )
              )
            }
          )
        }
      )
    }
  )
  
  # _4.5. game detail right----
  output$gameDetailR = renderUI(
    {
      gg = goals |> 
        filter(g == ed$gm & t == 2) |> 
        arrange(h)
      xx = games$xg2[ed$gm]
      div(
        # __4.5.0. xg & g----
        actionBttn(
          inputId = 'xg2',
          label = paste0('xG: ', xx),
          style = 'jelly',
          color = 'success',
          size = 'sm'
        ),
        actionBttn(
          inputId = 'g2',
          label = paste0('G: ', nrow(gg)),
          style = 'jelly',
          color = ifelse(ed$gm > 48, 'royal', 'warning'),
          size = 'sm'
        ),
        hr(style = 'border-color:white; margin:5px 0 10px 0;'),
        # __4.5.1. goal buttons right----
        if (nrow(gg) > 0) {
          lapply(
            1:nrow(gg),
            function(i) {
              if (gg$vid[i] != '0') {ed$gv[2, i] = 1}
              div(
                style = 'padding:2px;',
                align = 'left',
                actionBttn(
                  inputId = paste0('goal2_', i),
                  label = div(
                    icon('futbol'),
                    paste0(
                      ' ',
                      gg$time[i],
                      ifelse(
                        !is.na(gg$atime[i]), 
                        paste0('+', gg$atime[i]),
                        ''
                      ),
                      '\' ',
                      ifelse(gg$goal_type[i] == 'penalty', ' (P) ', ''),
                      ifelse(gg$goal_type[i] == 'own-goal', '(OG) ', ''),
                      gg$plyr_name[i], 
                      ' '
                    ),
                    icon(ifelse(gg$vid[i] != '0', 'film', 'circle-xmark'))
                  ),
                  style = ifelse(gg$vid[i] != '0', 'simple', 'bordered'),
                  size = 'xs',
                  color = ifelse(gg$vid[i] != '0', 'danger', 'default'),
                  block = FALSE
                )
              )
            }
          )
        }
      )
    }
  )
  
  # _4.6. shootout detail----
  output$shtoutDetail = renderUI(
    if (games$shootout[ed$gm] == 1) {
      ss = shoot |> 
        filter(game == ed$gm)
      div(
        style = 'padding:0 30px 0 30px;',
        fluidRow(
          style = 'padding-top:5px;',
          lapply(
            1:2, 
            function(i) {
              column(
                width = 6,
                fluidRow(
                  lapply(
                    1:5, 
                    function(j) {
                      k = 5 * (i - 1) + j
                      # border color
                      bcol = switch(ss$team[k], '#4287f7', '#ed6569')
                      # background-color
                      acol = switch(ss$goal[k] + 1, 'rgba(0,0,0,0)', 'white', bcol)
                      # font-color
                      fcol = switch(ss$goal[k] + 1, 'rgba(0,0,0,0)', bcol, 'white')
                      column(
                        width = 2,
                        style = 'padding:1px;',
                        offset = (j == 1) * 1,
                        div(
                          style = paste0(
                            'border:solid ', bcol, ' 1px; ',
                            'border-radius:5px; ',
                            'background-color:', acol, '; '
                          ),
                          p(
                            style = paste0(
                              'margin:2px; font-size:12px;' ,
                              'color:', fcol, ';'
                            ),
                            ifelse(
                              ss$goal[k] > 0,
                              paste0(
                                ss$i[k], ': ',
                                switch(ss$goal[k], '✘', '✓')
                              ),
                              ss$i[k]
                            )
                          )
                        )
                      )
                    }
                  )
                )
              )
            }
          )
        )
      )
    }
  )
  
  # >>>>>>>>>>>>>>>>>>>>>----
  # 5. group summary----
  output$uiGroups = renderUI(
    if (ed$gr > 0) {
      # _5.0. preprocessing----
      gg = games |> 
        filter(game <= 48) |> 
        filter(group1 == LETTERS[ed$gr] | group2 == LETTERS[ed$gr])
      t1 = teams |> 
        inner_join(gg, by = c('team' = 'team1')) |> 
        select(game, team, gf = score1, ga = score2, xf = xg1, xa = xg2) |> 
        mutate(
          w = ifelse(gf > ga, 1, 0),
          d = ifelse(gf == ga, 1, 0),
          l = ifelse(gf < ga, 1, 0)
        )
      t2 = teams |> 
        inner_join(gg, by = c('team' = 'team2')) |> 
        select(game, team, gf = score2, ga = score1, xf = xg2, xa = xg1) |> 
        mutate(
          w = ifelse(gf > ga, 1, 0),
          d = ifelse(gf == ga, 1, 0),
          l = ifelse(gf < ga, 1, 0)
        )
      t12 = bind_rows(t1, t2)
      tt = t12 |> 
        group_by(team) |> 
        summarize(
          ww = sum(w), 
          dd = sum(d), 
          ll = sum(l),
          gf = sum(gf),
          ga = sum(ga),
          xf = sum(xf),
          xa = sum(xa)
        ) |> 
        mutate(pts = 3*ww + dd) |> 
        mutate(gd = gf - ga) |> 
        arrange(-pts, -gd, -gf) |> 
        inner_join(teams, by = join_by(team))
      # _5.1. layout----
      div(
        style = 'padding:10px 55px 10px 55px;',
        align = 'left',
        div(
          style = 'border:solid magenta 1px; padding:0 15px 15px 15px;',
          h4(paste0('Group Stage Summary - ', LETTERS[ed$gr])),
          fluidRow(
            lapply(
              1:4, 
              function(i) {
                t12a = filter(t12, team == tt$team[i]) |> 
                  arrange(game)
                column(
                  width = 3,
                  style = paste0('padding:0 ', (i == 4) * 15, 'px 0 15px;'),
                  div(
                    style = paste0(
                      'border:solid ',
                      ifelse(i <= 2, 'lime', 'white'), ' 0px; ',
                      'padding:10px 10px 10px 20px; ',
                      'background-color: rgba(150, 150, 150, 0.6);'
                    ),
                    align = 'left',
                    # _5.1.1. flag + team----
                    h5(
                      img(
                        src = paste0('flags/', tt$code[i], '.png'),
                        width = '24px',
                        style = 'margin-left:10px;'
                      ),
                      tags$span(
                        style = 'padding-left:10px;',
                        tt$team[i]
                      )
                    ),
                    # __5.1.1.1. line----
                    hr(
                      style = paste0(
                        'margin:2px 0 5px 0; ',
                        'border-width:2px; ',
                        'border-color:',
                        ifelse(i <= 2, 'lime;', 'white;')
                      )
                    ),
                    # _5.1.2. stats----
                    # __5.1.2.1. pts----
                    fluidRow(
                      column(
                        width = 2,
                        align = 'right',
                        style = 'padding:3px 0 3px 0;',
                        div(
                          style = paste0(
                            'padding:1px 5px 1px 1px; ',
                            'border:solid white 1px; ',
                            'border-radius:5px; ',
                            'background-color:black; ',
                            'width:100%; '
                          ),
                          h5(
                            style = 'padding:0; margin:0 0 3px 0;', 
                            paste0(tt$pts[i], ' :')
                          )
                        )
                      ),
                      column(
                        width = 10,
                        fluidRow(
                          style = 'padding:0 15px 0 2px;',
                          lapply(
                            1:3, 
                            function(j) {
                              column(
                                width = 4,
                                style = 'padding:3px 1px 3px 1px;',
                                div(
                                  align = 'center',
                                  style = paste0(
                                    'padding:1px 1px 1px 1px; ',
                                    'border:solid white 1px; ',
                                    'border-radius:5px; ',
                                    'width:100%; ',
                                    'background-color:',
                                    case_when(
                                      t12a$w[j] == 1 ~ 'lime;',
                                      t12a$d[j] == 1 ~ 'yellow;',
                                      t12a$l[j] == 1 ~ 'lightgrey;'
                                    )
                                  ),
                                  h5(
                                    style = 'padding:0; margin:0 0 3px 0; color:grey;', 
                                    case_when(
                                      t12a$w[j] == 1 ~ 'W',
                                      t12a$d[j] == 1 ~ 'D',
                                      t12a$l[j] == 1 ~ 'L'
                                    )
                                  )
                                )
                              )
                            }
                          )
                        )
                      )
                    ),
                    # __5.1.2.2. G----
                    fluidRow(
                      column(
                        width = 2,
                        align = 'right',
                        style = 'padding:3px;',
                        h5(style = 'padding:0; margin:0 0 3px 0;', 'G :')
                      ),
                      # ___5.1.2.2.1. GF----
                      column(
                        width = 5,
                        align = 'right',
                        style = 'padding:3px 0 3px 3px;',
                        div(
                          id = paste0('gf', i),
                          align = 'left',
                          style = paste0(
                            'padding:1px 1px 1px 5px; ',
                            'border-radius:5px 0 0 5px; ',
                            'width:', 15 + floor(0.85 * tt$gf[i] / 0.12), '%; ',
                            'background-color:darkred;'
                          ),
                          h5(
                            style = 'padding:0; margin:0 0 3px 0; color:white;', 
                            format(tt$gf[i], nsmall = 0)
                          )
                        )
                      ),
                      # ___5.1.2.2.2. GA----
                      column(
                        width = 5,
                        align = 'left',
                        style = 'padding:3px 3px 3px 0;',
                        div(
                          id = paste0('ga', i),
                          align = 'right',
                          style = paste0(
                            'padding:1px 5px 1px 1px; ',
                            'border-radius:0 5px 5px 0; ',
                            'width:', 15 + floor(0.85 * tt$ga[i] / 0.12), '%; ',
                            'background-color:lightpink;'
                          ),
                          h5(
                            style = 'padding:0; margin:0 0 3px 0; color:grey;', 
                            format(tt$ga[i], nsmall = 0)
                          )
                        )
                      )
                    ),
                    # __5.1.2.3. xG----
                    fluidRow(
                      column(
                        width = 2,
                        align = 'right',
                        style = 'padding:3px;',
                        h5(style = 'padding:0; margin:0 0 3px 0;', 'xG :')
                      ),
                      # ___5.1.2.3.1. xGF----
                      column(
                        width = 5,
                        align = 'right',
                        style = 'padding:3px 0 3px 3px;',
                        div(
                          id = paste0('xgf', i),
                          align = 'left',
                          style = paste0(
                            'padding:1px 1px 1px 5px; ',
                            'border-radius:5px 0 0 5px; ',
                            'width:', 15 + floor(0.85 * tt$xf[i] / 0.12), '%; ',
                            'background-color:darkblue;'
                          ),
                          h5(
                            style = 'padding:0; margin:0 0 3px 0; color:white;', 
                            format(tt$xf[i], nsmall = 1)
                          )
                        )
                      ),
                      # ___5.1.2.3.2. xGA----
                      column(
                        width = 5,
                        align = 'left',
                        style = 'padding:3px 3px 3px 0;',
                        div(
                          id = paste0('xga', i),
                          align = 'right',
                          style = paste0(
                            'padding:1px 5px 1px 1px; ',
                            'border-radius:0 5px 5px 0; ',
                            'width:', 15 + floor(0.85 * tt$xa[i] / 0.12), '%; ',
                            'background-color:lightblue;'
                          ),
                          h5(
                            style = 'padding:0; margin:0 0 3px 0; color:grey;', 
                            format(tt$xa[i], nsmall = 1)
                          )
                        )
                      )
                    )
                  )
                )
              }
            )
          )
        )
      )
    }
  )
  
  # >>>>>>>>>>>>>>>>>>>>>----
  # 9. events----
  # 9.1. event home button----
  # observeEvent(input$homeBtn, {ed$tbl = switch(ed$tbl + 1, 1, 0)})
  
  # _9.1.0. event filter button----
  observeEvent(input$filterx, {ed$gr = 0; ed$tm = 0; ed$gm = 0;})
  
  # _9.1.1. events group buttons----
  observeEvent(input$group1, {ed$gr = 1; ed$tm = 0})
  observeEvent(input$group2, {ed$gr = 2; ed$tm = 0})
  observeEvent(input$group3, {ed$gr = 3; ed$tm = 0})
  observeEvent(input$group4, {ed$gr = 4; ed$tm = 0})
  observeEvent(input$group5, {ed$gr = 5; ed$tm = 0})
  observeEvent(input$group6, {ed$gr = 6; ed$tm = 0})
  observeEvent(input$group7, {ed$gr = 7; ed$tm = 0})
  observeEvent(input$group8, {ed$gr = 8; ed$tm = 0})
  
  # _9.1.2. events country buttons----
  observeEvent(input$cntry1, {ed$tm = 1; ed$gr = 0})
  observeEvent(input$cntry2, {ed$tm = 2; ed$gr = 0})
  observeEvent(input$cntry3, {ed$tm = 3; ed$gr = 0})
  observeEvent(input$cntry4, {ed$tm = 4; ed$gr = 0})
  observeEvent(input$cntry5, {ed$tm = 5; ed$gr = 0})
  observeEvent(input$cntry6, {ed$tm = 6; ed$gr = 0})
  observeEvent(input$cntry7, {ed$tm = 7; ed$gr = 0})
  observeEvent(input$cntry8, {ed$tm = 8; ed$gr = 0})
  observeEvent(input$cntry9, {ed$tm = 9; ed$gr = 0})
  observeEvent(input$cntry10, {ed$tm = 10; ed$gr = 0})
  observeEvent(input$cntry11, {ed$tm = 11; ed$gr = 0})
  observeEvent(input$cntry12, {ed$tm = 12; ed$gr = 0})
  observeEvent(input$cntry13, {ed$tm = 13; ed$gr = 0})
  observeEvent(input$cntry14, {ed$tm = 14; ed$gr = 0})
  observeEvent(input$cntry15, {ed$tm = 15; ed$gr = 0})
  observeEvent(input$cntry16, {ed$tm = 16; ed$gr = 0})
  observeEvent(input$cntry17, {ed$tm = 17; ed$gr = 0})
  observeEvent(input$cntry18, {ed$tm = 18; ed$gr = 0})
  observeEvent(input$cntry19, {ed$tm = 19; ed$gr = 0})
  observeEvent(input$cntry20, {ed$tm = 20; ed$gr = 0})
  observeEvent(input$cntry21, {ed$tm = 21; ed$gr = 0})
  observeEvent(input$cntry22, {ed$tm = 22; ed$gr = 0})
  observeEvent(input$cntry23, {ed$tm = 23; ed$gr = 0})
  observeEvent(input$cntry24, {ed$tm = 24; ed$gr = 0})
  observeEvent(input$cntry25, {ed$tm = 25; ed$gr = 0})
  observeEvent(input$cntry26, {ed$tm = 26; ed$gr = 0})
  observeEvent(input$cntry27, {ed$tm = 27; ed$gr = 0})
  observeEvent(input$cntry28, {ed$tm = 28; ed$gr = 0})
  observeEvent(input$cntry29, {ed$tm = 29; ed$gr = 0})
  observeEvent(input$cntry30, {ed$tm = 30; ed$gr = 0})
  observeEvent(input$cntry31, {ed$tm = 31; ed$gr = 0})
  observeEvent(input$cntry32, {ed$tm = 32; ed$gr = 0})
  
  # _9.1.3. event date button----
  observeEvent(input$dateBtn, {ed$so = switch(ed$so, 2, 1)})
  
  # 9.2. events game opts----
  observeEvent(input$gmOpt1, {ed$go = 1; ed$gt = 0})
  observeEvent(input$gmOpt2, {ed$go = 2; ed$gt = 0})
  
  # function to show/hide game details
  gmDet = function(g) {
    if (ed$gm != g) {
      ed$gm = g
    } else {
      ed$gm = 0
      startAnim(session, paste0('tblRow', g), 'backOutUp')
    }
    ed$gt = 0
    ed$gh = 0
    ed$go = 1
    ed$gv = matrix(c(rep(0, 16)), ncol = 8)
  }
  
  # 9.3. events goal buttons----
  observeEvent(input$goal1_1, {ed$gt = 1; ed$gh = 1})
  observeEvent(input$goal1_2, {ed$gt = 1; ed$gh = 2})
  observeEvent(input$goal1_3, {ed$gt = 1; ed$gh = 3})
  observeEvent(input$goal1_4, {ed$gt = 1; ed$gh = 4})
  observeEvent(input$goal1_5, {ed$gt = 1; ed$gh = 5})
  observeEvent(input$goal1_6, {ed$gt = 1; ed$gh = 6})
  observeEvent(input$goal1_7, {ed$gt = 1; ed$gh = 7})
  observeEvent(input$goal1_8, {ed$gt = 1; ed$gh = 8})
  observeEvent(input$goal2_1, {ed$gt = 2; ed$gh = 1})
  observeEvent(input$goal2_2, {ed$gt = 2; ed$gh = 2})
  observeEvent(input$goal2_3, {ed$gt = 2; ed$gh = 3})
  observeEvent(input$goal2_4, {ed$gt = 2; ed$gh = 4})
  observeEvent(input$goal2_5, {ed$gt = 2; ed$gh = 5})
  observeEvent(input$goal2_6, {ed$gt = 2; ed$gh = 6})
  observeEvent(input$goal2_7, {ed$gt = 2; ed$gh = 7})
  observeEvent(input$goal2_8, {ed$gt = 2; ed$gh = 8})
  
  # 9.4. events game buttons----
  observeEvent(input$game1, {gmDet(1)})
  observeEvent(input$game2, {gmDet(2)})
  observeEvent(input$game3, {gmDet(3)})
  observeEvent(input$game4, {gmDet(4)})
  observeEvent(input$game5, {gmDet(5)})
  observeEvent(input$game6, {gmDet(6)})
  observeEvent(input$game7, {gmDet(7)})
  observeEvent(input$game8, {gmDet(8)})
  observeEvent(input$game9, {gmDet(9)})
  observeEvent(input$game10, {gmDet(10)})
  observeEvent(input$game11, {gmDet(11)})
  observeEvent(input$game12, {gmDet(12)})
  observeEvent(input$game13, {gmDet(13)})
  observeEvent(input$game14, {gmDet(14)})
  observeEvent(input$game15, {gmDet(15)})
  observeEvent(input$game16, {gmDet(16)})
  observeEvent(input$game17, {gmDet(17)})
  observeEvent(input$game18, {gmDet(18)})
  observeEvent(input$game19, {gmDet(19)})
  observeEvent(input$game20, {gmDet(20)})
  observeEvent(input$game21, {gmDet(21)})
  observeEvent(input$game22, {gmDet(22)})
  observeEvent(input$game23, {gmDet(23)})
  observeEvent(input$game24, {gmDet(24)})
  observeEvent(input$game25, {gmDet(25)})
  observeEvent(input$game26, {gmDet(26)})
  observeEvent(input$game27, {gmDet(27)})
  observeEvent(input$game28, {gmDet(28)})
  observeEvent(input$game29, {gmDet(29)})
  observeEvent(input$game30, {gmDet(30)})
  observeEvent(input$game31, {gmDet(31)})
  observeEvent(input$game32, {gmDet(32)})
  observeEvent(input$game33, {gmDet(33)})
  observeEvent(input$game34, {gmDet(34)})
  observeEvent(input$game35, {gmDet(35)})
  observeEvent(input$game36, {gmDet(36)})
  observeEvent(input$game37, {gmDet(37)})
  observeEvent(input$game38, {gmDet(38)})
  observeEvent(input$game39, {gmDet(39)})
  observeEvent(input$game40, {gmDet(40)})
  observeEvent(input$game41, {gmDet(41)})
  observeEvent(input$game42, {gmDet(42)})
  observeEvent(input$game43, {gmDet(43)})
  observeEvent(input$game44, {gmDet(44)})
  observeEvent(input$game45, {gmDet(45)})
  observeEvent(input$game46, {gmDet(46)})
  observeEvent(input$game47, {gmDet(47)})
  observeEvent(input$game48, {gmDet(48)})
  observeEvent(input$game49, {gmDet(49)})
  observeEvent(input$game50, {gmDet(50)})
  observeEvent(input$game51, {gmDet(51)})
  observeEvent(input$game52, {gmDet(52)})
  observeEvent(input$game53, {gmDet(53)})
  observeEvent(input$game54, {gmDet(54)})
  observeEvent(input$game55, {gmDet(55)})
  observeEvent(input$game56, {gmDet(56)})
  observeEvent(input$game57, {gmDet(57)})
  observeEvent(input$game58, {gmDet(58)})
  observeEvent(input$game59, {gmDet(59)})
  observeEvent(input$game60, {gmDet(60)})
  observeEvent(input$game61, {gmDet(61)})
  observeEvent(input$game62, {gmDet(62)})
  observeEvent(input$game63, {gmDet(63)})
  observeEvent(input$game64, {gmDet(64)})
  
}