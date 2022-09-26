#########################
### ASSET MANAGER 2.0 ###
#########################

###################
### DATA IMPORT ###
###################

##### LOADING LIBRARIES #####

library(tidyverse)
library(quantmod)
library(treemapify)
library(highcharter) 
library(plotly)
library(scales)
library(markdown)
library(shinycssloaders)
library(lubridate)
library(shinydashboard)

##### DATA ENTRY FOR GLIDEPATH #####

birthday <- "1993-04-15"

##### COLOR PALETTE #####

colorPal <- c("#031D44", "#04395E", "#70A288", "#A5AD87", 
              "#DAB785", "#D5896F", "#AE5132", "#3A6E73", "#D8A07A")

##### LOADING DATA #####

setwd("~/R Projects/asset_manager/")
rawData <- read_csv("financialActivity.csv") %>%
  mutate(AMOUNT = ifelse(TRANSACTION == "SELL", AMOUNT*-1, AMOUNT),
         COST = round(AMOUNT*PRICE, digits = 4),
         DATE = as.Date(DATE, format = "%m/%d/%Y"),
         LOT_DATE = as.Date(LOT_DATE, format = "%m/%d/%Y"))

sellLots <- rawData %>%
  filter(TRANSACTION == "SELL") %>%
  select(ACCOUNT, FUND, DATE, LOT_DATE, AMOUNT) %>%
  rename(SELL_DATE = DATE, 
         DATE = LOT_DATE,
         SELL_AMOUNT = AMOUNT) %>%
  left_join(rawData) %>%
  mutate(SELL_COST = round(SELL_AMOUNT*PRICE, digits = 4)) %>%
  select(ACCOUNT, FUND, SELL_DATE, SELL_COST) %>%
  rename(DATE = SELL_DATE)

data <- rawData %>%
  left_join(sellLots) %>%
  mutate(COST = ifelse(TRANSACTION == "SELL", SELL_COST, COST)) %>%
  select(-SELL_COST, -LOT_DATE)

# ADJUSTED TARGETS BASED ON AGE
age <- round(time_length(difftime(Sys.Date(), 
                                  as.Date(birthday)), 
                         unit = "years"), digits = 0)
years <- ifelse(age-(55-25) < 0, 0, age-(55-25))
daysInvested <- data.frame(DATE = seq(min(data$DATE), Sys.Date(), by="days"))

glidePath <- read_csv("accountGlidepath.csv") %>%
  mutate(DIFF = ENDING - STARTING,
         ADJ = ((DIFF/25)*years),
         TARGET = STARTING+ADJ)

targetsAdj <- glidePath %>%
  select(ACCOUNT, FUND, TYPE, ASSET, TARGET) %>%
  filter(TARGET > 0)

accountWeight <- data %>%
  mutate(TOTAL = sum(COST)) %>%
  group_by(ACCOUNT) %>%
  summarise(ACCOUNT_WEIGHT = sum(COST)/TOTAL) %>%
  distinct()

accountTargets <- targetsAdj

portfolioTargets <- accountTargets %>%
  left_join(accountWeight) %>%
  mutate(WEIGHT = TARGET*ACCOUNT_WEIGHT) %>%
  group_by(FUND, TYPE, ASSET) %>%
  summarise(TARGET = sum(WEIGHT))

# SAVING CURRENT ALLOCATIONS FOR RECORDS
write.csv(accountTargets, 
          paste0(getwd(),"/Targets by Age/accountAssetTargets_adjusted_", age, ".csv"), 
          row.names = F)

# CREATING ASSETS REFERENCE TABLE
accountChoices <- c("Portfolio", unique(read_csv(paste0(getwd(),"/Targets by Age/accountAssetTargets_adjusted_", age, ".csv"))$ACCOUNT))
typeChoices <- unique(read_csv(paste0(getwd(),"/Targets by Age/accountAssetTargets_adjusted_", age, ".csv"))$TYPE)
assetChoices <- unique(read_csv(paste0(getwd(),"/Targets by Age/accountAssetTargets_adjusted_", age, ".csv"))$ASSET)

assetType <- glidePath %>%
  select(TYPE, ASSET, FUND) %>%
  distinct() %>%
  mutate(TYPE = factor(TYPE, levels = c("US Equities",
                                        "Intl Equities",
                                        "Emerging Markets",
                                        "REITs")),
         ASSET = factor(ASSET, levels = c("US Large Blend",
                                          "US Small Value",
                                          "Intl Large Blend",
                                          "Intl Small Value",
                                          "Emerging Markets",
                                          "REITs")),
         FUND = factor(FUND, levels = c("AVUS",
                                        "AVUV",
                                        "DFAX",
                                        "AVDV",
                                        "AVES",
                                        "DFAR"))) %>%
  arrange(TYPE, ASSET, FUND)

assetType$COLOR <- colorPal[1:nrow(assetType)]
assetType$COLOR <- factor(assetType$COLOR, levels = colorPal[1:nrow(assetType)])

# GET FUND PRICES & TIME SERIES
funds <- unique(data$FUND)
fundPrices_list <- list()
fundSeries_list <- list()
for(i in 1:length(funds)){
  symbol <- funds[i]
  quote <- getQuote(symbol)
  price <- quote$Last
  
  fundPrice <- cbind.data.frame(FUND = symbol, 
                                CURRENT_PRICE = round(price, digits = 2))
  fundPrices_list[[i]] <- fundPrice
  
  ts <- daysInvested %>%
    left_join(getSymbols(symbol, auto.assign = F) %>%
                as_tibble(rownames = "DATE") %>%
                select(DATE, contains(".Close")) %>%
                setNames(., c("DATE", "CLOSE")) %>%
                mutate(DATE = as.Date(DATE)))
  
  ts[1,2] <- ifelse(is.na(first(ts$CLOSE)) == T, 
                    first(ts$CLOSE[is.na(ts$CLOSE) == F]),
                    first(ts$CLOSE))
  
  ts <- ts %>%
    mutate(CLOSE = na.locf(CLOSE),
           FUND = symbol)
  
  fundSeries_list[[i]] <- ts
}

fundPrices <- bind_rows(fundPrices_list) %>%
  mutate(DAY = "Today")
fundSeries <- bind_rows(fundSeries_list)

dayPrices <- fundSeries %>%
  filter(DATE == Sys.Date() %m-% days(1)) %>%
  rename(LAST_PRICE = CLOSE) %>%
  select(-DATE) %>%
  left_join(fundPrices) %>%
  mutate(TODAY_CHANGE = (CURRENT_PRICE - LAST_PRICE)/LAST_PRICE) %>%
  left_join(assetType) %>%
  arrange(TYPE, ASSET)

dayMessage_list <- list()
for(i in 1:nrow(dayPrices)){
  row <- dayPrices[i,]
  q <- paste0(row$FUND, ": ", "$", row$CURRENT_PRICE, " (", 
              percent(row$TODAY_CHANGE, accuracy = .01), ")")
  
  dayMessage_list[[i]] <- q
}


dayMessage <- paste(dayMessage_list, collapse = " | ")

#################
### DATA PREP ###
#################

##### CONTRIBUTION CALCULATOR, REBALANCE, TREEMAP, DEVIATION DATA #####

accountSummary <- data %>%
  group_by(ACCOUNT, FUND) %>%
  summarise(AMOUNT = sum(AMOUNT),
            COST_BASIS = sum(COST)) %>%
  left_join(fundPrices) %>%
  left_join(accountTargets) %>%
  group_by(ACCOUNT) %>%
  mutate(MARKET_VAL = AMOUNT*CURRENT_PRICE,
         ACTUAL = round(MARKET_VAL/sum(MARKET_VAL), digits = 4),
         TARGET_VAL = TARGET*sum(MARKET_VAL),
         VAL_DEV = MARKET_VAL-TARGET_VAL,
         REL_DEV = VAL_DEV/TARGET_VAL,
         REBALANCE = -1*floor(VAL_DEV/CURRENT_PRICE))

portfolioSummary <- data %>%
  group_by(FUND) %>%
  summarise(AMOUNT = sum(AMOUNT),
            COST_BASIS = sum(COST)) %>%
  left_join(fundPrices) %>%
  left_join(portfolioTargets) %>%
  mutate(MARKET_VAL = AMOUNT*CURRENT_PRICE,
         ACTUAL = round(MARKET_VAL/sum(MARKET_VAL), digits = 4),
         TARGET_VAL = TARGET*sum(MARKET_VAL),
         VAL_DEV = MARKET_VAL-TARGET_VAL,
         REL_DEV = VAL_DEV/TARGET_VAL,
         REBALANCE = -1*floor(VAL_DEV/CURRENT_PRICE),
         ACCOUNT = "Portfolio") 

fullSummary <- bind_rows(accountSummary, portfolioSummary) %>%
  mutate(LABEL = paste0(ASSET, "\n(", FUND,"; ", percent(ACTUAL,
                                                         accuracy = .1), ")"),
         TYPE = factor(TYPE, levels = levels(assetType$TYPE)),
         ASSET = factor(ASSET, levels = levels(assetType$ASSET)))

##### PORTFOLIO GROWTH DATA #####

accountGrowth_list <- list()
for(i in 1:length(unique(data$ACCOUNT))){
  accountSeries <- data %>%
    filter(ACCOUNT == unique(data$ACCOUNT)[i])
  
  fundGrowth_list <- list()
  for(k in 1:length(unique(accountSeries$FUND))){
    myShares <- accountSeries %>%
      filter(FUND == unique(accountSeries$FUND)[k])
    
    mySeries <- fundSeries %>%
      filter(FUND == unique(accountSeries$FUND)[k]) %>%
      left_join(myShares) %>%
      filter(DATE >= min(myShares$DATE)) %>%
      arrange(DATE) %>%
      select(DATE, ACCOUNT, FUND, ASSET, AMOUNT, COST, CLOSE) %>%
      mutate(ACCOUNT = na.locf(ACCOUNT),
             FUND = na.locf(FUND),
             ASSET = na.locf(ASSET),
             AMOUNT = ifelse(is.na(AMOUNT) == T, 0, AMOUNT),
             COST = ifelse(is.na(COST) == T, 0, COST)) %>%
      group_by(DATE) %>%
      mutate(AMOUNT = sum(AMOUNT),
             COST = sum(COST)) %>%
      ungroup() %>%
      distinct() %>%
      mutate(SHARES = cumsum(AMOUNT),
             COST_BASIS = cumsum(COST),
             MARKET_VALUE = SHARES*CLOSE) %>%
      select(DATE, ACCOUNT, FUND, ASSET, COST_BASIS, MARKET_VALUE)
    
    fundGrowth_list[[k]] <- mySeries
  }
  accountGrowth_list[[i]] <- bind_rows(fundGrowth_list)
}

growthData <- bind_rows(accountGrowth_list) %>%
  distinct() %>%
  mutate(wday = weekdays(DATE)) %>%
  filter(wday %in% c('Monday','Tuesday', 'Wednesday', 'Thursday', 'Friday')) %>%
  select(-wday)

accountGrowth <- growthData %>%
  group_by(DATE, ACCOUNT) %>%
  summarise(COST_BASIS = sum(COST_BASIS),
            MARKET_VALUE = sum(MARKET_VALUE))

portfolioGrowth <- growthData %>%
  select(-ACCOUNT) %>%
  group_by(DATE, FUND) %>%
  summarise(COST_BASIS = sum(COST_BASIS),
            MARKET_VALUE = sum(MARKET_VALUE)) %>%
  mutate(ACCOUNT = "Portfolio")

fullGrowth <- bind_rows(accountGrowth,
                        portfolioGrowth)

fundGrowth <- growthData %>%
  group_by(DATE, FUND) %>%
  summarise(COST_BASIS = sum(COST_BASIS),
            MARKET_VALUE = sum(MARKET_VALUE))

###################
### GENERATE UI ###
###################

ui <- fluidPage(
  
  titlePanel(span(tagList(icon("bolt"), "Asset Manager"))),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Visualize account assets,
               track growth, and direct new
               contributions."),
      hr(),
      
      h4(span(tagList(icon("dollar-sign"), "Today's Prices"))),
      HTML(paste0('<marquee behavior="scroll" direction="left">', dayMessage,'</marquee>')),
      hr(),
      
      h4(span(tagList(icon("folder"), "Account Selection"))),
      selectInput("account",
                  label = "Choose an account to view:",
                  choices = accountChoices,
                  selected = "Portfolio"),
      hr(),
      
      h4(span(tagList(icon("money-bill"), "Contribution Calculator"))),
      numericInput("contr",
                   label = "Enter new contribution:",
                   value = 0),
      actionButton("calcContr",
                   label = "Calculate"),
      htmlOutput("contrMessage1"),
      htmlOutput("contrMessage2"),
      hr(),
      
      h4(span(tagList(icon("wrench"), "Rebalance Calculator"))),
      actionButton("rebalance",
                   label = "Calculate"),
      htmlOutput("rebalMessage1"),
      htmlOutput("rebalMessage2"),
      htmlOutput("rebalMessage3")
    ),
    
    mainPanel(
      h3("Account Summary", align = "center"),
      h4(span(tagList(icon("chart-pie"), "Asset Allocation"))),
      hr(),
      withSpinner(plotlyOutput(outputId = "treemap")),
      h4(span(tagList(icon("chart-bar"), "Asset Drift"))),
      hr(),
      withSpinner(plotlyOutput(outputId = "driftChart")),
      h4(span(tagList(icon("chart-line"), "Recent Performance & Asset Growth"))),
      hr(),
      withSpinner(plotlyOutput(outputId = "growthChart"))
    )
  ),
  tags$head(tags$style(HTML('* {font-family: "Verdana"};')))
)

withSpinner(plotOutput("my_plot"))
plotOutput("my_plot") %>% withSpinner()

#######################
### GENERATE SERVER ###
#######################

server <- function(input, output, session){
  
  output$treemap <- renderHighchart({
    
    visDat <- fullSummary %>%
      subset(ACCOUNT == input$account) %>%
      ungroup() %>%
      select(ACCOUNT, TYPE, ASSET, LABEL, MARKET_VAL) %>%
      left_join(assetType)
    
    vis <- plot_ly(
      type="treemap",
      labels=visDat$LABEL,
      parents = visDat$ACCOUNT,
      values = visDat$MARKET_VAL,
      marker=list(colors=c(visDat$COLOR))
    )
    
    vis
  })
  
  output$driftChart <- renderPlotly({
    
    visDat <- fullSummary %>%
      subset(ACCOUNT == input$account) %>%
      left_join(assetType) %>%
      arrange(TYPE, ASSET)
    
    visDat$TITLE <- "Asset % Drift from Targets"
    
    ub <- ifelse(max(visDat$REL_DEV) >= .08, max(visDat$REL_DEV)+.02, .1)
    lb <- ifelse(min(visDat$REL_DEV) <= -.08, min(visDat$REL_DEV)-.02, -.1)
    
    vis <- ggplotly(visDat %>%
                      ggplot(aes(x = ASSET, y = REL_DEV, fill = ASSET)) +
                      geom_bar(stat = "identity", position = "dodge", width = .75) +
                      geom_text(aes(label = paste0(FUND, "\n",
                                                   "(", percent(REL_DEV, accuracy = .01), ")"),
                                    y = REL_DEV+.008*sign(REL_DEV)),
                                size = 3) +
                      theme_minimal() +
                      scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                                         limits = c(lb,ub)) +
                      scale_fill_manual(values = as.character(visDat$COLOR)) +
                      ylab(NULL) +
                      xlab(NULL) +
                      theme(legend.position = "none",
                            panel.grid.major.x = element_blank(),
                            panel.grid.major.y = element_blank(),
                            panel.border = element_rect(colour = "black", fill=NA, size=1)) +
                      geom_hline(yintercept = 0) +
                      geom_hline(yintercept = .05, linetype = 'dotted', alpha = .25) +
                      geom_hline(yintercept = -.05, linetype = 'dotted', alpha = .25) +
                      facet_wrap(~TITLE))
    
    vis
    
  })
  
  output$growthChart <- renderPlotly({
    
    acctGrowth <- fullGrowth %>%
      subset(ACCOUNT == input$account) %>%
      select(DATE, ACCOUNT, COST_BASIS, MARKET_VALUE) %>%
      rename(`Cost Basis` = COST_BASIS,
             `Market Value` = MARKET_VALUE) %>%
      pivot_longer(c(-DATE, -ACCOUNT), names_to = "LINE", values_to = "VALUE") %>%
      group_by(DATE, LINE) %>%
      summarise(VALUE = sum(VALUE))
    
    end_mv <- acctGrowth$VALUE[acctGrowth$LINE == "Market Value" &
                                 acctGrowth$DATE == max(acctGrowth$DATE)]
    end_cb <- acctGrowth$VALUE[acctGrowth$LINE == "Cost Basis" &
                                 acctGrowth$DATE == max(acctGrowth$DATE)]
    performance <- percent((end_mv-end_cb)/end_cb, accuracy = .1)
    
    acctGrowth$TITLE <- paste0("Cost Basis vs. Market Value (", performance, " Overall)")
    
    if(input$account == "Portfolio"){
      fundSelect <- growthData %>%
        ungroup() %>%
        select(FUND) %>% distinct()
    } else {
      fundSelect <- growthData %>%
        subset(ACCOUNT == input$account) %>%
        ungroup() %>%
        select(FUND) %>% distinct()
    }
    
    fundGrowth_1m <- fundSeries %>%
      filter(FUND %in% fundSelect$FUND & DATE >= (Sys.Date() %m-% months(1))) %>%
      group_by(FUND) %>%
      left_join(assetType) %>%
      mutate(START = CLOSE[DATE == min(DATE)],
             CHANGE = round((CLOSE-START)/START, digits = 4)) %>%
      mutate(LABEL = paste0(FUND, "\n(", percent(CHANGE[DATE == max(DATE)], 
                                                 accuracy = .1), ")")) %>%
      arrange(DATE, TYPE, ASSET) %>%
      mutate(FUND = factor(FUND, levels = levels(assetType$FUND))) %>%
      mutate(wday = weekdays(DATE)) %>%
      filter(wday %in% c('Monday','Tuesday', 'Wednesday', 'Thursday', 'Friday'))
    
    fundGrowth_1m$LABEL <- factor(fundGrowth_1m$LABEL, 
                                  levels = unique(fundGrowth_1m$LABEL))
    
    vis1 <- ggplotly(fundGrowth_1m %>%
                       ggplot(aes(x = DATE, y = CHANGE, group = FUND)) +
                       geom_line(aes(colour = FUND), size = 1.25) +
                       scale_color_manual(values = as.character(unique(fundGrowth_1m$COLOR))) +
                       ylab(NULL) +
                       xlab(NULL) +
                       theme_minimal() +
                       theme(legend.position = "none",
                             panel.grid.major = element_blank(), 
                             panel.grid.minor = element_blank(),
                             panel.background = element_blank(),
                             axis.text.x = element_blank(),
                             axis.ticks = element_blank(),
                             panel.border = element_rect(colour = "black", fill=NA, size=1)) +
                       scale_y_continuous(labels = scales::percent_format(accuracy = .01)) +
                       geom_hline(yintercept = 0, linetype = "dotted", size = .75, alpha = .25) +
                       facet_wrap(~LABEL, nrow = 1)) %>%
      layout(showlegend = FALSE)
    
    vis2 <- ggplotly(acctGrowth %>%
                       ggplot(aes(x = DATE, y = VALUE, group = LINE)) +
                       geom_line(aes(colour = LINE), size = 1) +
                       scale_color_manual(values=c("black", "grey51"),
                                          guide = guide_legend(title = NULL)) +
                       scale_y_continuous(labels = scales::dollar_format(scale = .001, suffix = "K")) +
                       scale_x_date(date_labels="%b-%y") +
                       theme_minimal() +
                       xlab(NULL) +
                       ylab(NULL) +
                       theme(panel.grid.major = element_blank(), 
                             panel.grid.minor = element_blank(),
                             panel.background = element_blank(),
                             panel.border = element_rect(colour = "black", fill=NA, size=1)) +
                       facet_wrap(~TITLE)) %>%
      layout(showlegend = FALSE)
    
    plotly::subplot(vis1, vis2, nrows=2,
                    margin = 0.05, heights = c(0.3, 0.7))
    
  })
  
  acct <- reactive(input$account)
  
  displayText1 <- eventReactive(input$calcContr, {
    output$contrMessage1 <- renderUI({input$calcContr;
      account <- acct()
      amount <- input$contr
      
      HTML(paste0("<b>Calculating allocations for a $",
                  prettyNum(amount, big.mark = ","),
                  " contribution to your ", account, "...</b>"))
    })
    
    output$contrMessage2 <- renderUI({input$calcContr;
      account <- acct()
      amount <- input$contr
      
      calcData <- fullSummary %>%
        filter(ACCOUNT == input$account)
      
      targetTotal <- amount+sum(calcData$MARKET_VAL)
      
      calcOutput <- calcData %>%
        mutate(NEW_VAL = TARGET*targetTotal,
               DIFFERENCE = case_when(NEW_VAL-MARKET_VAL < 0 ~ 0,
                                      TRUE ~ NEW_VAL-MARKET_VAL),
               ADJ_DIFFERENCE = (DIFFERENCE/sum(DIFFERENCE))*amount,
               BUY_SHARES = floor(ADJ_DIFFERENCE/CURRENT_PRICE)) %>%
        select(FUND, BUY_SHARES) %>%
        filter(BUY_SHARES > 0) %>%
        left_join(assetType) %>%
        arrange(TYPE, ASSET)
      
      text_list <- list()
      for(i in 1:length(unique(calcOutput$FUND))){
        fund <- calcOutput %>%
          filter(FUND == unique(calcOutput$FUND)[i])
        
        action <- paste0("Buy ", fund$BUY_SHARES, " shares of ",
                         fund$FUND, ".")
        
        text_list[[i]] <- action
      }
      
      HTML(renderMarkdown(text = paste("- ", text_list, collapse = "\n")))
      
    })
  })
  
  observe(displayText1())
  
  displayText2 <- eventReactive(input$rebalance, {
    output$rebalMessage1 <- renderUI({input$rebalance;
      account <- acct()
      
      HTML(paste0("<b>Calculating a rebalance for ", account, "...</b>"))
    })
    
    output$rebalMessage2 <- renderUI({input$rebalance;
      account <- acct()
      
      calcData <- fullSummary %>%
        filter(ACCOUNT == input$account)
      
      sellData <- calcData %>%
        filter(REBALANCE < 0) %>%
        mutate(REBALANCE = -1*REBALANCE) %>%
        select(FUND, REBALANCE)
      
      buyData <- calcData %>%
        filter(REBALANCE > 0) %>%
        select(FUND, REBALANCE)
      
      sellText_list <- list()
      if (nrow(sellData) > 0){
        for(i in 1:length(unique(sellData$FUND))){
          fund <- sellData %>%
            filter(FUND == unique(sellData$FUND)[i])
          
          action <- paste0("Sell ", fund$REBALANCE, " shares of ",
                           fund$FUND, ".")
          
          sellText_list[[i]] <- action
        }
      } else {
        sellText_list <- list()
      }
      
      buyText_list <- list()
      if (nrow(buyData) > 0){
        for(i in 1:length(unique(buyData$FUND))){
          fund <- buyData %>%
            filter(FUND == unique(buyData$FUND)[i])
          
          action <- paste0("Buy ", fund$REBALANCE, " shares of ",
                           fund$FUND, ".")
          
          buyText_list[[i]] <- action
        } 
      } else {
        buyText_list <- list()
      }
      
      fullText_list <- c(sellText_list, buyText_list)
      
      HTML(renderMarkdown(text = paste("- ", fullText_list, collapse = "\n")))
      
    })
    
    output$rebalMessage3 <- renderUI({input$rebalance;
      account <- acct()
      
      calcData <- fullSummary %>%
        filter(ACCOUNT == input$account)
      
      maxOver <- calcData$MARKET_VAL[calcData$REL_DEV == max(calcData$REL_DEV)]
      targetOver <- calcData$TARGET[calcData$REL_DEV == max(calcData$REL_DEV)]
      acctValNeeded <- maxOver/targetOver
      
      contrNeeded <- calcData %>%
        mutate(NEEDED_VAL = acctValNeeded*TARGET,
               CONTR_DEV = floor((MARKET_VAL-NEEDED_VAL)/CURRENT_PRICE)*CURRENT_PRICE)
      
      valNeeded <- paste0("$",
                          prettyNum(round(-1*sum(contrNeeded$CONTR_DEV), 
                                          digits = 0), big.mark = ","))
      
      HTML(paste0("A ", valNeeded, " contribution is required to rebalance ",
                  account, " without selling shares."))
      
    })
  })
  
  observe(displayText2())
}

shinyApp(ui = ui, server = server)
