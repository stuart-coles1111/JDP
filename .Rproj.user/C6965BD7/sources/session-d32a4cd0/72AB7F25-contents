library("shiny")
library("shinyMatrix")
library("shinythemes")
library("magrittr")
library("DT")
library("ggplot2")

nTeam <- 6
nmatch <- 8
nPlayer <- 16
init_Bank <- 20
Bank0 <- rep(init_Bank, nTeam)

tournament_data <- dummy_data

Player_names <- tournament_data$Player_names
Player_names <- paste0("P",1:16,": ",Player_names)

m1 <- matrix("", nTeam, nPlayer )
colnames(m1) = paste0("P",1:nPlayer)
rownames(m1)= paste0("Team ",1:nTeam)

m2 <- matrix("", nTeam, nPlayer/2 )
colnames(m2) = paste0("P",1:(nPlayer/2))
rownames(m2)= paste0("Team ",1:nTeam)

m3 <- matrix("", nTeam, nPlayer/4 )
colnames(m3) = paste0("P",1:(nPlayer/4))
rownames(m3)= paste0("Team ",1:nTeam)

m4 <- matrix("", nTeam, nPlayer/8 )
colnames(m4) = paste0("P",1:(nPlayer/8))
rownames(m4)= paste0("Team ",1:nTeam)

m5 <- matrix("", nTeam, nPlayer/8 )
colnames(m5) = paste0("P",1:(nPlayer/8))
rownames(m5)= paste0("Team ",1:nTeam)

r1 <- matrix(0, nmatch, 4)
rownames(r1) = 1:nmatch
colnames(r1)= c('Player 1 id','Player 2 id', 'Player 1 score','Player 2 score')

r2 <- matrix(0, nmatch/2, 4)
rownames(r2) = 1:(nmatch/2)
colnames(r2)= c('Player 1 id','Player 2 id', 'Player 1 score','Player 2 score')

r3 <- matrix(0, nmatch/4, 4)
rownames(r3) = 1:(nmatch/4)
colnames(r3)= c('Player 1 id','Player 2 id', 'Player 1 score','Player 2 score')

r4 <- matrix(0, nmatch/8, 4)
rownames(r4) = 1:(nmatch/8)
colnames(r4)= c('Player 1 id','Player 2 id', 'Player 1 score','Player 2 score')

r5 <- matrix(0, nmatch/8, 4)
rownames(r5) = 1:(nmatch/8)
colnames(r5)= c('Player 1 id','Player 2 id', 'Player 1 score','Player 2 score')

ui <- fluidPage(theme = shinytheme("cerulean"),
                titlePanel("Jogo do Pau: Smartodds Lisbon Tournament"),

                shiny::tabsetPanel(id = "inTabset",
                                   shiny::tabPanel("Round 1", value="panel1",
                                                   fluidPage(
                                                     shiny::h3("Matches"),
                                                     fluidRow(
                                                       column(width =5, DT::DTOutput("table1")),
                                                       column(width = 5, plotOutput("Bank1"), offset = 2),
                                                     ),
                                                     shiny::br(),
                                                     shiny::br(),
                                                     shiny::h3("Bets"),
                                                     fluidRow(
                                                       column(width = 5,
                                                              matrixInput(
                                                                "m1",
                                                                value = m1,
                                                                rows = list(extend = FALSE, names = TRUE),
                                                                cols = list(extend = FALSE, names = TRUE)
                                                              )
                                                       ),

                                                       column(offset = 2, width = 5, plotOutput("bets1"))
                                                     ),

                                                     fluidRow(
                                                       column(width = 5, actionButton("confirm_bets1", "Confirm Bets")
                                                       ),
                                                       column(offset = 2, width = 5, actionButton("play_game", "Match results")
                                                       )
                                                     )

                                                   )
                                   ),

                                   shiny::tabPanel("Round 2", value="panel2",
                                                   fluidPage(
                                                     shiny::h3("Matches"),
                                                     fluidRow(
                                                       column(width =5, DT::DTOutput("table2")),
                                                       column(width = 5,plotOutput("Bank2"), offset = 2),

                                                     ),
                                                     shiny::br(),
                                                     shiny::br(),
                                                     shiny::h3("Bets"),
                                                     fluidRow(
                                                       column(width = 5,
                                                              matrixInput(
                                                                "m2",
                                                                value = m2,
                                                                rows = list(extend = FALSE, names = TRUE),
                                                                cols = list(extend = FALSE, names = TRUE)
                                                              )
                                                       ),

                                                       column(offset = 2, width = 5, plotOutput("bets2"))
                                                     ),

                                                     fluidRow(
                                                       column(width = 5, actionButton("confirm_bets2", "Confirm Bets")
                                                       ),
                                                       column(offset = 2, width = 5, actionButton("play_game2", "Match results")
                                                       )
                                                     )

                                                   )
                                   ),

                                   shiny::tabPanel("Semi finals", value="panel2",

                                                   fluidPage(
                                                     shiny::h3("Matches"),
                                                     fluidRow(
                                                       column(width =5, DT::DTOutput("table3")),
                                                       column(width = 5,plotOutput("Bank3"), offset = 2),

                                                     ),
                                                     shiny::br(),
                                                     shiny::br(),
                                                     shiny::h3("Bets"),
                                                     fluidRow(
                                                       column(width = 5,
                                                              matrixInput(
                                                                "m3",
                                                                value = m3,
                                                                rows = list(extend = FALSE, names = TRUE),
                                                                cols = list(extend = FALSE, names = TRUE)
                                                              )
                                                       ),

                                                       column(offset = 2, width = 5, plotOutput("bets3"))
                                                     ),

                                                     fluidRow(
                                                       column(width = 5, actionButton("confirm_bets3", "Confirm Bets")
                                                       ),
                                                       column(offset = 2, width = 5, actionButton("play_game3", "Match results")
                                                       )
                                                     )

                                                   )
                                   ),

                                   shiny::tabPanel("3rd/4th Place", value="panel3",

                                                   fluidPage(
                                                     shiny::h3("Matches"),
                                                     fluidRow(
                                                       column(width =5, DT::DTOutput("table4")),
                                                       column(width = 5,plotOutput("Bank4"), offset = 2),

                                                     ),
                                                     shiny::br(),
                                                     shiny::br(),
                                                     shiny::h3("Bets"),
                                                     fluidRow(
                                                       column(width = 5,
                                                              matrixInput(
                                                                "m4",
                                                                value = m4,
                                                                rows = list(extend = FALSE, names = TRUE),
                                                                cols = list(extend = FALSE, names = TRUE)
                                                              )
                                                       ),

                                                       column(offset = 2, width = 5, plotOutput("bets4"))
                                                     ),

                                                     fluidRow(
                                                       column(width = 5, actionButton("confirm_bets4", "Confirm Bets")
                                                       ),
                                                       column(offset = 2, width = 5, actionButton("play_game4", "Match results")
                                                       )
                                                     )

                                                   )
                                   ),

                                   shiny::tabPanel("Final", value="panel2",

                                                   fluidPage(
                                                     shiny::h3("Matches"),
                                                     fluidRow(
                                                       column(width =5, DT::DTOutput("table5")),
                                                       column(width = 5,plotOutput("Bank5"), offset = 2),

                                                     ),
                                                     shiny::br(),
                                                     shiny::br(),
                                                     shiny::h3("Bets"),
                                                     fluidRow(
                                                       column(width = 5,
                                                              matrixInput(
                                                                "m5",
                                                                value = m5,
                                                                rows = list(extend = FALSE, names = TRUE),
                                                                cols = list(extend = FALSE, names = TRUE)
                                                              )
                                                       ),

                                                       column(offset = 2, width = 5, plotOutput("bets5"))
                                                     ),

                                                     fluidRow(
                                                       column(width = 5, actionButton("confirm_bets5", "Confirm Bets")
                                                       ),
                                                       column(offset = 2, width = 5, actionButton("play_game5", "Match results")
                                                       )
                                                     )

                                                   )
                                   )
                )

)

server <- function(input, output, session) {

  values <- shiny::reactiveValues(
    round = 1,
    m1= m1,
    m2= m2,
    m3 = m3,
    m4 = m4,
    m5 = m5,
    Bank = Bank0,
    res1 = matrix(0, nrow = nTeam, ncol = 2),
    res2 = matrix(0, nrow = nTeam, ncol = 2),
    res3 = matrix(0, nrow = nTeam, ncol = 2),
    res4 = matrix(0, nrow = nTeam, ncol = 2),
    res5 = matrix(0, nrow = nTeam, ncol = 2),
    winners_r1 = rep(0, nPlayer/2),
    winners_r2 = rep(0, nPlayer/4),
    winners_r3 = rep(0, nPlayer/8),
    winners_r4 = rep(0, nPlayer/16),
    winners_r5 = rep(0, nPlayer/16),
    losers_r3 = rep(0, nPlayer/8),
    left = 1:16,
    out = 1:2
  )

  df <- data.frame(
    Player1 = Player_names[c(1,3,5,7,9,11,13,15)],
    Player2 = Player_names[c(2,4,6,8,10,12,14,16)],
    "Player1 score" = rep("", 8),
    "Player2 score" = rep("", 8)
  )

  output$table1 <- DT::renderDT({
    DT::datatable(df,
                  rownames = paste0("Match: ", 1:8),
                  colnames = c("Player 1", "Player 2", "Player 1 Score", "Player 2 Score"),
                  options = list(
                    initComplete = JS("
                        function(settings, json) {
                          $(this.api().table().header()).css({
                          'font-size': '18px',
                          });
                        }
                    "), dom="t")
    ) %>%
      formatStyle(columns = colnames(.$x$data), `font-size` = "18px")
  })



  df2 <- data.frame(Team = (1:nTeam) %>% as.character, Bank = init_Bank)
  output$Bank1 <- renderPlot({
    ggplot(df2) + geom_col(aes(Team, Bank), width = 0.25, fill="lightblue") +theme(axis.text=element_text(size=16),
                                                                                   axis.title=element_text(size=18,face="bold")) +
      geom_hline(yintercept = init_Bank, colour="red")
  })





  shiny::observeEvent(input$confirm_bets1, {
    temp_m <- input$m1%>% as.numeric
    temp_m[is.na(temp_m)] <- 0
    temp_m <- matrix(temp_m,  nTeam, nPlayer)
    values$m1 <- temp_m %>% as.numeric %>% matrix(nrow = nrow(input$m1), ncol=ncol(input$m1))
    bet_sums <- apply(values$m1, 1, sum)
    values$Bank <- values$Bank - bet_sums
    df <- data.frame(Team = (1:nTeam) %>% as.character, Bank = values$Bank, Winnings = bet_sums)
    output$Bank1 <- renderPlot({
      ggplot(df) + geom_col(aes(Team, Bank), width = 0.25, fill="lightblue") +theme(axis.text=element_text(size=16),
                                                                                    axis.title=element_text(size=18,face="bold"))+
        geom_hline(yintercept = init_Bank, colour="red")
    })
    output$bets1 <- renderPlot({
      ggplot(df) + geom_col(aes(Team, Winnings), width = 0.25, fill="lightblue") +theme(axis.text=element_text(size=16),
                                                                                        axis.title=element_text(size=18,face="bold"))+
        geom_hline(yintercept = 0, colour="green") + ylab("Amount Staked")
    })
  }
  )

  shiny::observeEvent(input$confirm_bets2, {
    temp_m <- input$m2 %>% as.numeric
    temp_m[is.na(temp_m)] <- 0
    temp_m <- matrix(temp_m,  nTeam, nPlayer/2)
    values$m2 <- temp_m %>% as.numeric %>% matrix(nrow = nrow(input$m2), ncol=ncol(input$m2))
    bet_sums <- apply(values$m2, 1, sum)
    values$Bank <- values$Bank - bet_sums
    df <- data.frame(Team = (1:nTeam) %>% as.character, Bank = values$Bank, Winnings = bet_sums)
    output$Bank2 <- renderPlot({
      ggplot(df) + geom_col(aes(Team, Bank), width = 0.25, fill="lightblue")+theme(axis.text=element_text(size=16),
                                                                                   axis.title=element_text(size=18,face="bold"))+
        geom_hline(yintercept = init_Bank, colour="red")
    })
    output$bets2 <- renderPlot({
      ggplot(df) + geom_col(aes(Team, Winnings), width = 0.25, fill="lightblue") +theme(axis.text=element_text(size=16),
                                                                                        axis.title=element_text(size=18,face="bold"))+
        geom_hline(yintercept = 0, colour="green")+ ylab("Amount Staked")
    })
  }
  )

  shiny::observeEvent(input$confirm_bets3, {
    temp_m <- input$m3 %>% as.numeric
    temp_m[is.na(temp_m)] <- 0
    temp_m <- matrix(temp_m,  nTeam, nPlayer/4)
    values$m3 <- temp_m %>% as.numeric %>% matrix(nrow = nrow(input$m3), ncol=ncol(input$m3))
    bet_sums <- apply(values$m3, 1, sum)
    values$Bank <- values$Bank - bet_sums
    df <- data.frame(Team = (1:nTeam) %>% as.character, Bank = values$Bank, Winnings = bet_sums)
    output$Bank3 <- renderPlot({
      ggplot(df) + geom_col(aes(Team, Bank), width = 0.25, fill="lightblue")+theme(axis.text=element_text(size=16),
                                                                                   axis.title=element_text(size=18,face="bold"))+
        geom_hline(yintercept = init_Bank, colour="red")
    })
    output$bets3 <- renderPlot({
      ggplot(df) + geom_col(aes(Team, Winnings), width = 0.25, fill="lightblue") +theme(axis.text=element_text(size=16),
                                                                                        axis.title=element_text(size=18,face="bold"))+
        geom_hline(yintercept = 0, colour="green")+ ylab("Amount Staked")
    })
  }
  )

  shiny::observeEvent(input$confirm_bets4, {
    temp_m <- input$m4 %>% as.numeric
    temp_m[is.na(temp_m)] <- 0
    temp_m <- matrix(temp_m,  nTeam, nPlayer/8)
    values$m4 <- temp_m %>% as.numeric %>% matrix(nrow = nrow(input$m4), ncol=ncol(input$m4))
    bet_sums <- apply(values$m4, 1, sum)
    values$Bank <- values$Bank - bet_sums
    df <- data.frame(Team = (1:nTeam) %>% as.character, Bank = values$Bank, Winnings = bet_sums)
    output$Bank4 <- renderPlot({
      ggplot(df) + geom_col(aes(Team, Bank), width = 0.25, fill="lightblue")+theme(axis.text=element_text(size=16),
                                                                                   axis.title=element_text(size=18,face="bold"))+
        geom_hline(yintercept = init_Bank, colour="red")
    })
    output$bets4 <- renderPlot({
      ggplot(df) + geom_col(aes(Team, Winnings), width = 0.25, fill="lightblue") +theme(axis.text=element_text(size=16),
                                                                                        axis.title=element_text(size=18,face="bold"))+
        geom_hline(yintercept = 0, colour="green")+ ylab("Amount Staked")
    })
  }
  )

  shiny::observeEvent(input$confirm_bets5, {
    temp_m <- input$m5 %>% as.numeric
    temp_m[is.na(temp_m)] <- 0
    temp_m <- matrix(temp_m,  nTeam, nPlayer/8)
    values$m5 <- temp_m %>% as.numeric %>% matrix(nrow = nrow(input$m5), ncol=ncol(input$m5))
    bet_sums <- apply(values$m5, 1, sum)
    values$Bank <- values$Bank - bet_sums
    df <- data.frame(Team = (1:nTeam) %>% as.character, Bank = values$Bank, Winnings = bet_sums)
    output$Bank5 <- renderPlot({
      ggplot(df) + geom_col(aes(Team, Bank), width = 0.25, fill="lightblue")+theme(axis.text=element_text(size=16),
                                                                                   axis.title=element_text(size=18,face="bold"))+
        geom_hline(yintercept = init_Bank, colour="red")
    })
    output$bets5 <- renderPlot({
      ggplot(df) + geom_col(aes(Team, Winnings), width = 0.25, fill="lightblue") +theme(axis.text=element_text(size=16),
                                                                                        axis.title=element_text(size=18,face="bold"))+
        geom_hline(yintercept = 0, colour="green")+ ylab("Amount Staked")
    })
  }
  )

  shiny::observeEvent(input$play_game, {

    id1 <- seq(1, 15, by = 2)
    id2 <- seq(2, 16, by = 2)
    tsim <- mgame_play_l(8, tournament_data[id1, -1], tournament_data[id2, -1])


    r1[, 1] <- seq(1, nPlayer - 1, by = 2)
    r1[, 2] <- seq(2, nPlayer, by = 2)

    r1[, 3] <- tsim$w_1
    r1[, 4] <- tsim$w_2

    values$winners_r1 <- ifelse(r1[,3] > r1[,4], r1[, 1], r1[, 2])
    values$names_r2 <- Player_names[values$winners_r1]
    values$res1 <- r1
    values$left <- ifelse(r1[,3] > r1[,4], id1, id2)

    Winnings <- rep(0, nTeam)
    for(i in 1:nTeam){
      stakes <- values$m1[i,]
      Winnings[i] <- -sum(stakes)
      for(j in values$winners_r1){
        values$Bank[i] <- values$Bank[i] + 2 * stakes[j]
        Winnings[i] <- Winnings[i] + 2 * stakes[j]
      }
    }

    df <- values$res1 %>% as.data.frame
    df$Player1 <- Player_names[values$res1[, "Player 1 id"]]
    df$Player2 <- Player_names[values$res1[, "Player 2 id"]]
    df <- df[, c("Player1","Player2","Player 1 score","Player 2 score")]
    output$table1 <- DT::renderDT({
      DT::datatable(df,
                    rownames = paste0("Match: ", 1:8),
                    colnames = c("Player 1", "Player 2", "Player 1 Score", "Player 2 Score"),
                    options = list(
                      initComplete = JS("
                        function(settings, json) {
                          $(this.api().table().header()).css({
                          'font-size': '18px',
                          });
                        }
                    "), dom="t")
      ) %>%
        formatStyle(columns = colnames(.$x$data), `font-size` = "18px")
    })

    df2 <- data.frame(Team = (1:nTeam) %>% as.character, Bank = values$Bank, Winnings = Winnings)
    df2$win <- ifelse(df2$Winnings > 0, 1, 0) %>% as.factor
    output$Bank1 <- renderPlot({
      ggplot(df2) + geom_col(aes(Team, Bank), width = 0.25, fill="lightblue") +theme(axis.text=element_text(size=16),
                                                                                     axis.title=element_text(size=18,face="bold"))+
        geom_hline(yintercept = init_Bank, colour="red")
    })

    output$bets1 <- renderPlot({
      ggplot(df2) + geom_col(aes(Team, Winnings, fill = win), width = 0.25,) +theme(axis.text=element_text(size=16),
                                                                                    axis.title=element_text(size=18,face="bold"))+
        geom_hline(yintercept = 0, colour="green") + theme(legend.position="none")
    })

    output$Bank2 <-  renderPlot({
      ggplot(df2) + geom_col(aes(Team, Bank), width = 0.25, fill="lightblue") +theme(axis.text=element_text(size=16),
                                                                                     axis.title=element_text(size=18,face="bold"))+
        geom_hline(yintercept = init_Bank, colour="red")
    })

    dfn2 <- data.frame(
      Player1 = Player_names[c(1,3,5,7)],
      Player2 = Player_names[c(2,4,6,8)],
      "Player1 score" = rep("", 4),
      "Player2 score" = rep("", 4)
    )

    dfn2$Player1 <- values$names_r2[c(1,3,5,7)]
    dfn2$Player2 <- values$names_r2[c(2,4,6,8)]
    dfn2$Player1 <- paste0("P", c(1,3,5,7), ":",do.call(rbind,strsplit(dfn2$Player1, ":") )[,2])
    dfn2$Player2 <- paste0("P", c(2,4,6,8), ":",do.call(rbind,strsplit(dfn2$Player2, ":") )[,2])
    output$table2 <- DT::renderDT({
      DT::datatable(dfn2,
                    rownames = paste0("Match: ", 1:4),
                    colnames = c("Player 1", "Player 2", "Player 1 Score", "Player 2 Score"),
                    options = list(
                      initComplete = JS("
                        function(settings, json) {
                          $(this.api().table().header()).css({
                          'font-size': '18px',
                          });
                        }
                    "), dom="t")
      ) %>%
        formatStyle(columns = colnames(.$x$data), `font-size` = "18px")
    })

  }
  )

  shiny::observeEvent(input$play_game2, {

    id1 <- values$left[seq(1, 7, by = 2)]
    id2 <- values$left[seq(2, 8, by = 2)]

    tsim <- mgame_play_l(4, tournament_data[id1, -1], tournament_data[id2, -1])


    r2[, 1] <- seq(1, nPlayer/2 - 1, by = 2)
    r2[, 2] <- seq(2, nPlayer/2, by = 2)

    r2[, 3] <- tsim$w_1
    r2[, 4] <- tsim$w_2

    values$winners_r2 <- ifelse(r2[,3] > r2[,4], r2[, 1], r2[, 2])
    values$names_r3 <- values$names_r2[values$winners_r2]
    values$res2 <- r2
    values$left <- ifelse(r2[,3] > r2[,4], id1, id2)


    Winnings <- rep(0, nTeam)
    for(i in 1:nTeam){
      stakes <- values$m2[i,]
      Winnings[i] <- -sum(stakes)
      for(j in values$winners_r2){
        values$Bank[i] <- values$Bank[i] + 2 * stakes[j]
        Winnings[i] <- Winnings[i] + 2 * stakes[j]
      }
    }

    dfn2 <- values$res2 %>% as.data.frame
    dfn2$Player1 <- values$names_r2[c(1,3,5,7)]
    dfn2$Player2 <- values$names_r2[c(2,4,6,8)]
    dfn2$Player1 <- paste0("P", c(1,3,5,7), ":",do.call(rbind,strsplit(dfn2$Player1, ":") )[,2])
    dfn2$Player2 <- paste0("P", c(2,4,6,8), ":",do.call(rbind,strsplit(dfn2$Player2, ":") )[,2])
    dfn2 <- dfn2[, c("Player1","Player2","Player 1 score","Player 2 score")]
    output$table2 <- DT::renderDT({
      DT::datatable(dfn2,
                    rownames = paste0("Match: ", 1:4),
                    colnames = c("Player 1", "Player 2", "Player 1 Score", "Player 2 Score"),
                    options = list(
                      initComplete = JS("
                        function(settings, json) {
                          $(this.api().table().header()).css({
                          'font-size': '18px',
                          });
                        }
                    "), dom="t")
      ) %>%
        formatStyle(columns = colnames(.$x$data), `font-size` = "18px")
    })



    dfn3 <- data.frame(
      Player1 = Player_names[c(1,3)],
      Player2 = Player_names[c(2,4)],
      "Player1 score" = rep("", 2),
      "Player2 score" = rep("", 2)
    )

    dfn3$Player1 <- values$names_r3[c(1,3)]
    dfn3$Player2 <- values$names_r3[c(2,4)]
    dfn3$Player1 <- paste0("P", c(1,3), ":",do.call(rbind,strsplit(dfn3$Player1, ":") )[,2])
    dfn3$Player2 <- paste0("P", c(2,4), ":",do.call(rbind,strsplit(dfn3$Player2, ":") )[,2])
    output$table3 <- DT::renderDT({
      DT::datatable(dfn3,
                    rownames = paste0("Match ", 1:2),
                    colnames = c("Player 1", "Player 2", "Player 1 Score", "Player 2 Score"),
                    options = list(
                      initComplete = JS("
                        function(settings, json) {
                          $(this.api().table().header()).css({
                          'font-size': '18px',
                          });
                        }
                    "), dom="t")
      ) %>%
        formatStyle(columns = colnames(.$x$data), `font-size` = "18px")
    })


    df <- data.frame(Team = (1:nTeam) %>% as.character, Bank = values$Bank, Winnings = Winnings)
    df$win <- ifelse(df$Winnings > 0, 1, 0) %>% as.factor
    output$Bank2 <- renderPlot({
      ggplot(df) + geom_col(aes(Team, Bank), width = 0.25, fill="lightblue") +theme(axis.text=element_text(size=16),
                                                                                    axis.title=element_text(size=18,face="bold"))+
        geom_hline(yintercept = init_Bank, colour="red")
    })

    output$bets2 <- renderPlot({
      ggplot(df) + geom_col(aes(Team, Winnings, fill = win), width = 0.25,) +theme(axis.text=element_text(size=16),
                                                                                   axis.title=element_text(size=18,face="bold"))+
        geom_hline(yintercept = 0, colour="green") + theme(legend.position="none")
    })

    output$Bank3<- renderPlot({
      ggplot(df) + geom_col(aes(Team, Bank), width = 0.25, fill="lightblue") +theme(axis.text=element_text(size=16),
                                                                                    axis.title=element_text(size=18,face="bold"))+
        geom_hline(yintercept = init_Bank, colour="red")
    })
  }
  )

  shiny::observeEvent(input$play_game3, {

    id1 <- values$left[seq(1, 3, by = 2)]
    id2 <- values$left[seq(2, 4, by = 2)]

    tsim <- mgame_play_l(2, tournament_data[id1, -1], tournament_data[id2, -1])


    r3[, 1] <- seq(1, nPlayer/4 - 1, by = 2)
    r3[, 2] <- seq(2, nPlayer/4, by = 2)

    r3[, 3] <- tsim$w_1
    r3[, 4] <- tsim$w_2

    values$winners_r3 <- ifelse(r3[,3] > r3[,4], r3[, 1], r3[, 2])
    values$losers_r3 <- ifelse(r3[,3] > r3[,4], r3[, 2], r3[, 1])
    values$names_r4 <- values$names_r3[-values$winners_r3]
    values$names_r5 <- values$names_r3[values$winners_r3]
    values$res3 <- r3
    values$left <- ifelse(r3[,3] > r3[,4], id1, id2)
    values$out <- ifelse(r3[,3] > r3[,4], id2, id1)

    Winnings <- rep(0, nTeam)
    for(i in 1:nTeam){
      stakes <- values$m3[i,]
      Winnings[i] <- -sum(stakes)
      for(j in values$winners_r3){
        values$Bank[i] <- values$Bank[i] + 2 * stakes[j]
        Winnings[i] <- Winnings[i] + 2 * stakes[j]
      }
    }


    dfn3 <- values$res3 %>% as.data.frame
    dfn3$Player1 <- values$names_r3[c(1,3)]
    dfn3$Player2 <- values$names_r3[c(2,4)]
    dfn3$Player1 <- paste0("P", c(1,3), ":",do.call(rbind,strsplit(dfn3$Player1, ":") )[,2])
    dfn3$Player2 <- paste0("P", c(2,4), ":",do.call(rbind,strsplit(dfn3$Player2, ":") )[,2])
    dfn3 <- dfn3[, c("Player1","Player2","Player 1 score","Player 2 score")]
    output$table3 <- DT::renderDT({
      DT::datatable(dfn3,
                    rownames = paste0("Match ", 1:2),
                    colnames = c("Player 1", "Player 2", "Player 1 Score", "Player 2 Score"),
                    options = list(
                      initComplete = JS("
                        function(settings, json) {
                          $(this.api().table().header()).css({
                          'font-size': '18px',
                          });
                        }
                    "), dom="t")
      ) %>%
        formatStyle(columns = colnames(.$x$data), `font-size` = "18px")
    })



    dfn4 <- data.frame(
      Player1 = Player_names[c(1)],
      Player2 = Player_names[c(1)],
      "Player1 score" = rep("", 1),
      "Player2 score" = rep("", 1)
    )

    dfn4$Player1 <- values$names_r4[c(1)]
    dfn4$Player2 <- values$names_r4[c(2)]
    dfn4$Player1 <- paste0("P", c(1), ":",do.call(rbind,strsplit(dfn4$Player1, ":") )[,2])
    dfn4$Player2 <- paste0("P", c(2), ":",do.call(rbind,strsplit(dfn4$Player2, ":") )[,2])
    output$table4 <- DT::renderDT({
      DT::datatable(dfn4,
                    rownames = paste0("Match: ", 1),
                    colnames = c("Player 1", "Player 2", "Player 1 Score", "Player 2 Score"),
                    options = list(
                      initComplete = JS("
                        function(settings, json) {
                          $(this.api().table().header()).css({
                          'font-size': '18px',
                          });
                        }
                    "), dom="t")
      ) %>%
        formatStyle(columns = colnames(.$x$data), `font-size` = "18px")
    })

    dfn5 <- data.frame(
      Player1 = Player_names[c(1)],
      Player2 = Player_names[c(1)],
      "Player1 score" = rep("", 1),
      "Player2 score" = rep("", 1)
    )

    dfn5$Player1 <- values$names_r5[c(1)]
    dfn5$Player2 <- values$names_r5[c(2)]
    dfn5$Player1 <- paste0("P", c(1), ":",do.call(rbind,strsplit(dfn5$Player1, ":") )[,2])
    dfn5$Player2 <- paste0("P", c(2), ":",do.call(rbind,strsplit(dfn5$Player2, ":") )[,2])
    output$table5 <- DT::renderDT({
      DT::datatable(dfn5,
                    rownames = paste0("Match: ", 1),
                    colnames = c("Player 1", "Player 2", "Player 1 Score", "Player 2 Score"),
                    options = list(
                      initComplete = JS("
                        function(settings, json) {
                          $(this.api().table().header()).css({
                          'font-size': '18px',
                          });
                        }
                    "), dom="t")
      ) %>%
        formatStyle(columns = colnames(.$x$data), `font-size` = "18px")
    })

    df <- data.frame(Team = (1:nTeam) %>% as.character, Bank = values$Bank, Winnings = Winnings)
    df$win <- ifelse(df$Winnings > 0, 1, 0) %>% as.factor
    output$Bank3 <- renderPlot({
      ggplot(df) + geom_col(aes(Team, Bank), width = 0.25, fill="lightblue") +theme(axis.text=element_text(size=16),
                                                                                    axis.title=element_text(size=18,face="bold"))+
        geom_hline(yintercept = init_Bank, colour="red")
    })

    output$bets3 <- renderPlot({
      ggplot(df) + geom_col(aes(Team, Winnings, fill = win), width = 0.25,) +theme(axis.text=element_text(size=16),
                                                                                   axis.title=element_text(size=18,face="bold"))+
        geom_hline(yintercept = 0, colour="green") + theme(legend.position="none")
    })

    output$Bank4 <- renderPlot({
      ggplot(df) + geom_col(aes(Team, Bank), width = 0.25, fill="lightblue") +theme(axis.text=element_text(size=16),
                                                                                    axis.title=element_text(size=18,face="bold"))+
        geom_hline(yintercept = init_Bank, colour="red")


    })

  }
  )

  shiny::observeEvent(input$play_game4, {

    id1 <- values$out[1]
    id2 <- values$out[2]

    tsim <- mgame_play_l(1, tournament_data[id1, -1], tournament_data[id2, -1])

    r4[, 1] <- seq(1, nPlayer/8 - 1, by = 2)
    r4[, 2] <- seq(2, nPlayer/8, by = 2)

    r4[, 3] <- tsim$w_1
    r4[, 4] <- tsim$w_2

    values$winners_r4 <- ifelse(r4[,3] > r4[,4], r4[, 1], r4[, 2])
    values$res4 <- r4


    Winnings <- rep(0, nTeam)
    for(i in 1:nTeam){
      stakes <- values$m4[i,]
      Winnings[i] <- -sum(stakes)
      for(j in values$winners_r4){
        values$Bank[i] <- values$Bank[i] + 2 * stakes[j]
        Winnings[i] <- Winnings[i] + 2 * stakes[j]
      }
    }


    dfn4 <- values$res4 %>% as.data.frame
    dfn4$Player1 <- values$names_r4[c(1)]
    dfn4$Player2 <- values$names_r4[c(2)]
    dfn4$Player1 <- paste0("P", c(1), ":",do.call(rbind,strsplit(dfn4$Player1, ":") )[,2])
    dfn4$Player2 <- paste0("P", c(2), ":",do.call(rbind,strsplit(dfn4$Player2, ":") )[,2])
    dfn4 <- dfn4[, c("Player1","Player2","Player 1 score","Player 2 score")]
    output$table4 <- DT::renderDT({
      DT::datatable(dfn4,
                    rownames = paste0("Match ", 1),
                    colnames = c("Player 1", "Player 2", "Player 1 Score", "Player 2 Score"),
                    options = list(
                      initComplete = JS("
                        function(settings, json) {
                          $(this.api().table().header()).css({
                          'font-size': '18px',
                          });
                        }
                    "), dom="t")
      ) %>%
        formatStyle(columns = colnames(.$x$data), `font-size` = "18px")
    })



    df <- data.frame(Team = (1:nTeam) %>% as.character, Bank = values$Bank, Winnings = Winnings)
    df$win <- ifelse(df$Winnings > 0, 1, 0) %>% as.factor
    output$Bank4 <- renderPlot({
      ggplot(df) + geom_col(aes(Team, Bank), width = 0.25, fill="lightblue") +theme(axis.text=element_text(size=16),
                                                                                    axis.title=element_text(size=18,face="bold"))+
        geom_hline(yintercept = init_Bank, colour="red")
    })
    output$bets4 <- renderPlot({
      ggplot(df) + geom_col(aes(Team, Winnings, fill = win), width = 0.25,) +theme(axis.text=element_text(size=16),
                                                                                   axis.title=element_text(size=18,face="bold"))+
        geom_hline(yintercept = 0, colour="green") + theme(legend.position="none")
    })
    output$Bank5 <- renderPlot({
      ggplot(df) + geom_col(aes(Team, Bank), width = 0.25, fill="lightblue") +theme(axis.text=element_text(size=16),
                                                                                    axis.title=element_text(size=18,face="bold"))+
        geom_hline(yintercept = init_Bank, colour="red")
    })

  }
  )

  shiny::observeEvent(input$play_game5, {

    id1 <- values$left[1]
    id2 <- values$left[2]

    tsim <- mgame_play_l(1, tournament_data[id1, -1], tournament_data[id2, -1])


    r5[, 1] <- seq(1, nPlayer/8 - 1, by = 2)
    r5[, 2] <- seq(2, nPlayer/8, by = 2)

    r5[, 3] <- tsim$w_1
    r5[, 4] <- tsim$w_2

    values$winners_r5 <- ifelse(r5[,3] > r5[,4], r5[, 1], r5[, 2])
    values$res5 <- r5

    Winnings <- rep(0, nTeam)
    for(i in 1:nTeam){
      stakes <- values$m5[i,]
      Winnings[i] <- -sum(stakes)
      for(j in values$winners_r5){
        values$Bank[i] <- values$Bank[i] + 2 * stakes[j]
        Winnings[i] <- Winnings[i] + 2 * stakes[j]
      }
    }


    dfn5 <- values$res4 %>% as.data.frame
    dfn5$Player1 <- values$names_r5[c(1)]
    dfn5$Player2 <- values$names_r5[c(2)]
    dfn5$Player1 <- paste0("P", c(1), ":",do.call(rbind,strsplit(dfn5$Player1, ":") )[,2])
    dfn5$Player2 <- paste0("P", c(2), ":",do.call(rbind,strsplit(dfn5$Player2, ":") )[,2])
    dfn5 <- dfn5[, c("Player1","Player2","Player 1 score","Player 2 score")]
    output$table5 <- DT::renderDT({
      DT::datatable(dfn5,
                    rownames = paste0("Match: ", 1),
                    colnames = c("Player 1", "Player 2", "Player 1 Score", "Player 2 Score"),
                    options = list(
                      initComplete = JS("
                        function(settings, json) {
                          $(this.api().table().header()).css({
                          'font-size': '18px',
                          });
                        }
                    "), dom="t")
      ) %>%
        formatStyle(columns = colnames(.$x$data), `font-size` = "18px")
    })



    df <- data.frame(Team = (1:nTeam) %>% as.character, Bank = values$Bank, Winnings = Winnings)
    df$win <- ifelse(df$Winnings > 0, 1, 0) %>% as.factor
    output$Bank5 <- renderPlot({
      ggplot(df) + geom_col(aes(Team, Bank), width = 0.25, fill="lightblue") +theme(axis.text=element_text(size=16),
                                                                                    axis.title=element_text(size=18,face="bold"))+
        geom_hline(yintercept = init_Bank, colour="red")
    })
    output$bets5 <- renderPlot({
      ggplot(df) + geom_col(aes(Team, Winnings, fill = win), width = 0.25,) +theme(axis.text=element_text(size=16),
                                                                                   axis.title=element_text(size=18,face="bold"))+
        geom_hline(yintercept = 0, colour="green") + theme(legend.position="none")
    })
  }
  )
}

shinyApp(ui, server)
