##### LIBRARIES ###################################################################################################

library(readr)
library(shiny)
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(dplyr)
library(shiny)
library(shinythemes)
library(shinydashboard)
library(DT)
library("formattable")

##### DATA ###################################################################################################

df <- read_csv("~/Documents/Shiny/passingHistory1/data/passing_history.csv")
df_over20att <- df %>%
  filter(Att > 20) %>%
  arrange(Name)

#long_df <- gather(df, category, number, 7:31)
#View(long_df)



# code to color plots for team color
team_colors <- c("ARI" = "#97233F", "ATL" = "#A71930",
                 "BAL" = "#241773", "BUF" = "#00338D",
                 "CAR" = "#0085CA", "CHI" = "#0B162A", "CIN" = "#FB4F14", "CLV" = "#311D00", "CLE" = "#311D00",
                 "DAL" = "#041E42", "DEN" = "#FC4C02", "DET" = "#005A8B",
                 "GB" = "#183028", "GNB" = "#183028",
                 "HST" = "#03202F", "HOU" = "#03202F",
                 "IND" = "#003A70",
                 "JAX" = "#003A70",
                 "KAN" = "#C8102E", "KC" = "#C8102E",
                 "LAC" = "#002244", "LAR" = "#041E42",
                 "MIA" = "#008E97", "MIN" = "#4F2683",
                 "NE" = "#002244", "NWE" = "#002244", "NO" = "#D3BC8D", "NOR" = "#D3BC8D", "NYG" = "#0B2265", "NYJ" = "#203731",
                 "OAK" = "#000000",
                 "PHI" = "#004953", "PIT" = "#FFB81C", "PHO" = "#97233F",
                 "SDG" = "#002244", "SF" = "#A6192E", "SFO" = "#A6192E", "SEA" = "#002244", "STL" = "#041E42",
                 "RAI" = "#000000", "RAM" = "#041E42",
                 "TB" = "#C8102E", "TAM" = "#C8102E", "TEN" = "#4B92DB", "WAS" = "#773141"
)

options(DT.options = list(pageLength = 20))

##### USER INTERFACE ###################################################################################################
header <- dashboardHeader(title = "Sports Data Graphs!")
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Quarterbacks", tabName = "Quarterbacks", icon = icon("th"), badgeLabel = "new", badgeColor = "green"),
    menuItem("Running Backs", tabName = "RunningBacks", icon = icon("th"), badgeLabel = "coming soon", badgeColor = "red"),
    menuItem("Wide Receiver", tabName = "WideReceiver", icon = icon("th"), badgeLabel = "coming soon", badgeColor = "red")))

# PAGE 1:
p1 <- tabItem(tabName = "Quarterbacks",
              fluidPage(
                # Application title
                titlePanel("NFL QB Historical Player Comparison"),
                
                # Sidebar with a two selector/filters
                sidebarLayout(
                  sidebarPanel(
                    #uiOutput("firstSelection"),
                    uiOutput("secondSelection"),
                    uiOutput("thirdSelection")),
                  
                  # Show two plots and passing data
                  mainPanel("",
                            fluidRow(
                              box(solidHeader=TRUE, plotOutput("PassingTD")), 
                              box(solidHeader=TRUE, plotOutput("PassingTD2")),
                              box(title = "Career Stats", status = "primary", solidHeader = TRUE, collapsible = TRUE,
                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('PassingData'))), 
                              box(title = "Career Stats", status = "primary", solidHeader = TRUE, collapsible = TRUE, 
                                  div(style = 'overflow-x: scroll',DT::dataTableOutput('PassingData2'))))))))

# PAGE 2:
p2 <- tabItem(tabName = "RunningBacks",
              fluidPage(
                mainPanel("",
                          fluidRow(
                            box(solidHeader=TRUE, plotOutput("PassingINT"), width = 6),
                            box(solidHeader=TRUE, plotOutput("PassingYDS"), width = 6)
                          ))
              ))

# PAGE 3:
p3 <- tabItem(tabName = "WideReceiver",
              h2("Wide Receiver Page"))

css <- tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"))
  
body <- dashboardBody(
  tabItems(p1, p2, p3),
  css)

ui <- dashboardPage(header, sidebar, body)

##### SERVER ###################################################################################################

server <- function(input, output) {

  output$firstSelection <- renderUI({
    selectInput("statSelect",
                "Select a Stat:",
                choices = df_over20att$Name,
                selected = "Peyton Manning")
  })  
  
    output$secondSelection <- renderUI({
    selectInput("playerSelect",
                "Select a Player:",
                choices = df_over20att$Name,
                selected = "Peyton Manning")
  })
  
  output$thirdSelection <- renderUI({
    selectInput("playerSelect2",
                "Select a Player:",
                choices = df_over20att$Name,
                selected = "Peyton Manning")
  })
  
  
  # First player plot
  output$PassingTD <- renderPlot({
    
    player <- df_over20att %>%
      filter(Name == input$playerSelect)
    
    
    ggplot(player, aes(as.character(Year), TD, fill = Tm)) + 
      geom_bar(stat = "identity", position = "dodge") + 
      scale_fill_manual(values = team_colors) +
      geom_text(aes(label = TD), hjust = 2, color = "white", size = 3.5) +
      theme_fivethirtyeight() +
      theme(legend.position = "top",
            legend.title = element_text(face = "bold"),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            axis.text.x = element_blank(),
            #plot.background = element_rect(fill = "#EFF2F4"),
            plot.title = element_text(hjust = 0.5)) +
      guides(fill = guide_legend(title = player$Name, reverse = "TRUE")) +
      #scale_x_continuous(breaks = pretty(player$Year)) +
      labs(caption = "@tdemmer18") +
      coord_flip()
  }) 
  # Second player plot
  output$PassingTD2 <- renderPlot({
    
    player <- df_over20att %>%
      filter(Name == input$playerSelect2)
    
    ggplot(player, aes(as.character(Year), TD, fill = Tm)) + 
      geom_bar(stat = "identity", position = "dodge") + 
      scale_fill_manual(values = team_colors) +
      geom_text(aes(label = TD), hjust = 2, color = "white", size = 3.5) +
      theme_fivethirtyeight() +
      theme(legend.position = "top",
            legend.title = element_text(face = "bold"),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            axis.text.x = element_blank(),
            #plot.background = element_rect(fill = "#EFF2F4"),
            plot.title = element_text(hjust = 0.5)) +
      guides(fill = guide_legend(title = player$Name, reverse = "TRUE")) +
      labs(caption = "@tdemmer18") +
      coord_flip()
  })
  
  
  output$PassingINT <- renderPlot({
    
    player <- df_over20att %>%
      filter(Name == input$playerSelect2)
    
    ggplot(player, aes(as.character(Year), Int, fill = Tm)) + 
      geom_bar(stat = "identity", position = "dodge") + 
      scale_fill_manual(values = team_colors) +
      geom_text(aes(label = Int), hjust = 2, color = "white", size = 3.5) +
      theme_fivethirtyeight() +
      theme(legend.position = "top",
            legend.title = element_text(face = "bold"),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            axis.text.x = element_blank(),
            #plot.background = element_rect(fill = "#EFF2F4"),
            plot.title = element_text(hjust = 0.5)) +
      guides(fill = guide_legend(title = player$Name, reverse = "TRUE")) +
      labs(caption = "@tdemmer18") +
      coord_flip()
  })
  
  output$PassingYDS <- renderPlot({
    
    player <- df_over20att %>%
      filter(Name == input$playerSelect2)
    
    ggplot(player, aes(as.character(Year), Yds, fill = Tm)) + 
      geom_bar(stat = "identity", position = "dodge") + 
      scale_fill_manual(values = team_colors) +
      geom_text(aes(label = Yds), hjust = 2, color = "white", size = 3.5) +
      theme_fivethirtyeight() +
      theme(legend.position = "top",
            legend.title = element_text(face = "bold"),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            axis.text.x = element_blank(),
            #plot.background = element_rect(fill = "#EFF2F4"),
            plot.title = element_text(hjust = 0.5)) +
      guides(fill = guide_legend(title = player$Name, reverse = "TRUE")) +
      labs(caption = "@tdemmer18") +
      coord_flip()
  })
  # Display data in table format for first QB selected
  output$PassingData <- DT::renderDataTable({
    as.datatable(formattable(
    playerfilter <- subset(df %>% 
                             select(Year, Tm, Cmp, Att, "Cmp%", Yds, TD, Int), 
                           df$Name == input$playerSelect),
    list(
      Yds = color_tile("#EBEDEF", "#229954"),
      TD = color_tile("#EBEDEF", "#229954"),
      Int = color_tile("#EBEDEF", "#CB4335")
    )
  ))})
  output$PassingData2 <- DT::renderDataTable({
    playerfilter <- subset(df %>%
                             select(Year, Tm, Cmp, Att, "Cmp%", Yds, TD, Int), 
                           df$Name == input$playerSelect2)
  })
}
##### RUN THE APP ###################################################################################################
shinyApp(ui = ui, server = server)

