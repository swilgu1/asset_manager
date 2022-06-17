
##### LOADING LIBRARIES #####

library(shiny)
library(tidyverse)
library(quantmod)
library(knitr)
library(plotly)
library(scales)
library(paletteer)
library(markdown)
library(shinycssloaders)

data <- read_csv("FinancialActivity.csv") %>%
  mutate(COST = round(AMOUNT*PRICE, digits = 2)) %>%
  mutate(DATE = as.Date(DATE, format = "%m/%d/%Y"))

accountChoices <- c("Portfolio", unique(read_csv("accountAssetTargets.csv")$ACCOUNT))
assetChoices <- unique(read_csv("accountAssetTargets.csv")$ASSET)

##### UI #####

ui <- fluidPage(
  
  
  titlePanel("Asset Manager"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Visualize account assets,
               track growth, and direct new
               contributions."),
      
      selectInput("account", 
                  label = "Choose an account to view:",
                  choices = accountChoices,
                  selected = "Portfolio"),
      
      h4("Contribution Calculator"),
      
      numericInput("contr",
                   label = "Enter new contribution:",
                   value = 0),
      actionButton("calcContr",
                   label = "Calculate"),
      
      htmlOutput("contrMessage1"),
      htmlOutput("contrMessage2"),
      
      br(),
      br(),
      
      h4("Add Shares"),
      
      selectInput("enterAsset",
                  label = "Choose asset to purchase:",
                  choices = assetChoices),
      numericInput("enterShares",
                   label = "Enter number of shares:",
                   value = 0),
      numericInput("enterPrice",
                   label = "Enter share price:",
                   value = 0),
      actionButton("addShare",
                   label = "Add shares"),
      htmlOutput("saveData")
    ),
    mainPanel(
      h4("Account Summary"),
      htmlOutput("acctSumm"),
      htmlOutput("acctRebalance"),
      h4("Asset Allocation"),
      withSpinner(plotlyOutput(outputId = "assetAllo")),
      h4("Growth of Assets"),
      withSpinner(plotlyOutput(outputId = "growthChart"))
    )
  )
)

withSpinner(plotOutput("my_plot"))
plotOutput("my_plot") %>% withSpinner()
server <- function(input, output){
  
  ##### LOADING DATA #####
  
  targets <- read_csv("accountAssetTargets.csv")
  assetTypes <- targets %>%
    select(ASSET, CLASS) %>%
    distinct()
  
  daysInvested <- data.frame(DATE = seq(min(data$DATE), Sys.Date(), by="days"))
  
  colorPal <- c(as.vector(paletteer_dynamic("cartography::blue.pal",1)),
                as.vector(paletteer_dynamic("cartography::red.pal",1)),
                as.vector(paletteer_dynamic("cartography::green.pal",1)),
                as.vector(paletteer_dynamic("cartography::sand.pal",1)),
                as.vector(paletteer_dynamic("cartography::purple.pal",1)),
                as.vector(paletteer_dynamic("cartography::orange.pal",1)),
                as.vector(paletteer_dynamic("cartography::brown.pal",1)))
  
  acc.port.Cols <- paletteer_dynamic("cartography::blue.pal",length(unique(targets$ASSET)))
  
  palVector <- c("cartography::red.pal", "cartography::green.pal", "cartography::sand.pal", "cartography::purple.pal")
  
  acctBlends <- list(acc.port.Cols)
  for(i in 1:length(unique(data$ACCOUNT))){
    acctBlends[[i+1]] <- paletteer_dynamic(palVector[i], 
                                         length(unique(targets$ASSET[targets$ACCOUNT == unique(data$ACCOUNT)[i]])))
  }
  
  names(acctBlends) <- c("Portfolio", unique(targets$ACCOUNT))
  
  ##### PREPPING DATA #####
  
  accountAssetDatList <- list()
  accountAssetTimeSeries <- list()
  
  for(i in 1:length(unique(targets$ACCOUNT))){
    
    account <- data %>%
      subset(ACCOUNT == unique(targets$ACCOUNT)[i])
    accountAssets <- targets %>%
      subset(ACCOUNT == unique(targets$ACCOUNT)[i])
    
    assetDatList <- list()
    assetTimeSeries <- list()
    
    for(k in 1:length(unique(accountAssets$ASSET))){
      symbol <- unique(accountAssets$ASSET)[k]
      
      # GETTING QUOTE
      quote <- getQuote(symbol)
      
      price <- quote$Last
      
      assetDat <- cbind.data.frame(ASSET = symbol, 
                                   CURRENT_PRICE = round(price, digits = 2))
      
      assetDatList[[k]] <- assetDat
      
      # GETTING PAST CLOSING PRICES
      
      ts <- daysInvested %>%
        left_join(getSymbols(symbol, auto.assign = F) %>%
                    as_tibble(rownames = "DATE") %>%
                    select(DATE, contains(".Close")) %>%
                    setNames(., c("DATE", "CLOSE")) %>%
                    mutate(DATE = as.Date(DATE))) %>%
        mutate(CLOSE = na.locf(CLOSE))
      
      myShares <- account %>%
        select(-ACCOUNT) %>%
        filter(ASSET == symbol) %>%
        arrange(desc(DATE))
      
      myData <- ts %>%
        left_join(myShares) %>%
        filter(DATE >= min(myShares$DATE)) %>%
        arrange(DATE) %>%
        mutate(ASSET = na.locf(ASSET),
               AMOUNT = ifelse(is.na(AMOUNT) == T, 0, AMOUNT),
               COST = ifelse(is.na(COST) == T, 0, COST)) %>%
        group_by(DATE) %>%
        mutate(AMOUNT = sum(AMOUNT),
               COST = sum(COST)) %>%
        ungroup() %>%
        select(-PRICE) %>%
        distinct() %>%
        mutate(CUMULATIVE_AMOUNT = cumsum(AMOUNT),
               CUMULATIVE_COST = cumsum(COST),
               CUMULATIVE_VALUE = CUMULATIVE_AMOUNT*CLOSE,
               ACCOUNT = unique(targets$ACCOUNT)[i]) %>%
        select(DATE, ACCOUNT, ASSET, CUMULATIVE_COST, CUMULATIVE_VALUE)
      
      assetTimeSeries[[k]] <- myData
    }
    
    accountAssetDatList[[i]] <- bind_rows(assetDatList)
    accountAssetTimeSeries[[i]] <- bind_rows(assetTimeSeries)
  }
  
  assetPrices <- bind_rows(accountAssetDatList) %>%
    distinct()
  
  accountAssetGrowth <- bind_rows(accountAssetTimeSeries) %>%
    setNames(., c("DATE", "ACCOUNT", "ASSET", "COST", "VALUE"))
  
  accountGrowth <- daysInvested %>%
    left_join(accountAssetGrowth) %>%
    group_by(ACCOUNT, DATE) %>%
    summarise(COST = sum(COST),
              VALUE = sum(VALUE)) %>%
    mutate(wday = weekdays(DATE)) %>%
    filter(wday %in% c('Monday','Tuesday', 'Wednesday', 'Thursday', 'Friday'))
  
  portfolioGrowth <- daysInvested %>%
    left_join(accountGrowth) %>%
    group_by(DATE) %>%
    summarise(COST = sum(COST),
              VALUE = sum(VALUE)) %>%
    mutate(ACCOUNT = "Portfolio") %>%
    mutate(wday = weekdays(DATE)) %>%
    filter(wday %in% c('Monday','Tuesday', 'Wednesday', 'Thursday', 'Friday'))
  
  growthData <- accountGrowth %>%
    bind_rows(portfolioGrowth)
  
  ##### SUMMARIZING CURRENT VALUE #####
  
  portfolioSummary <- data %>%
    group_by(ACCOUNT, ASSET) %>%
    summarise(AMOUNT = sum(AMOUNT),
              COST_BASIS = round(sum(COST), digits = 2)) %>%
    full_join(targets) %>%
    mutate_if(is.numeric , replace_na, replace = 0) %>%
    left_join(assetPrices) %>%
    mutate(ACTUAL_VALUE = round(AMOUNT*CURRENT_PRICE, digits = 2)) %>%
    group_by(ACCOUNT) %>%
    mutate(TARGET_VALUE = round(TARGET*sum(ACTUAL_VALUE), digits = 4),
           ACTUAL = round(ACTUAL_VALUE/sum(ACTUAL_VALUE), digits = 4)) %>%
    mutate_if(is.numeric , replace_na, replace = 0) %>%
    group_by(ACCOUNT) %>%
    arrange(desc(ACTUAL), .by_group = T) %>%
    mutate(ASSET = fct_inorder(ASSET)) %>%
    select(ACCOUNT, ASSET, AMOUNT, COST_BASIS, TARGET, TARGET_VALUE,
           ACTUAL, ACTUAL_VALUE) %>%
    mutate(DEVIATION = round((ACTUAL_VALUE - TARGET_VALUE)/TARGET_VALUE, digits = 4))
  
  fullSummary <- portfolioSummary %>%
    select(-ACCOUNT) %>%
    group_by(ASSET) %>%
    summarise(AMOUNT = sum(AMOUNT),
              COST_BASIS = round(sum(COST_BASIS), digits = 2),
              TARGET_VALUE = sum(TARGET_VALUE),
              ACTUAL_VALUE = round(sum(ACTUAL_VALUE), digits = 2)) %>%
    mutate(TARGET = round(TARGET_VALUE/sum(TARGET_VALUE), digits = 2),
           ACTUAL = ACTUAL_VALUE/sum(ACTUAL_VALUE)) %>%
    mutate_if(is.numeric , replace_na, replace = 0) %>%
    arrange(desc(ACTUAL), .by_group = T) %>%
    mutate(ASSET = fct_inorder(ASSET),
           ACCOUNT = "Portfolio") %>%
    select(ACCOUNT, ASSET, AMOUNT, COST_BASIS, TARGET, TARGET_VALUE,
           ACTUAL, ACTUAL_VALUE) %>%
    mutate(DEVIATION = round((ACTUAL_VALUE - TARGET_VALUE)/TARGET_VALUE, digits = 4))
  
  output$growthChart <- renderPlotly({
    
    col <- acctBlends[[input$account]]
    
    ggplotly(growthData %>%
               subset(ACCOUNT == input$account) %>%
               select(DATE, COST) %>%
               mutate(LINE = "Cost Basis") %>%
               rename(VALUE = COST) %>%
               bind_rows(growthData %>%
                           subset(ACCOUNT == input$account) %>%
                           select(DATE, VALUE) %>%
                           mutate(LINE = "Market Value")) %>%
               mutate(LINE = factor(LINE, levels = c("Market Value",
                                                     "Cost Basis"))) %>%
               ggplot(aes(x = DATE, y = VALUE, group = LINE)) +
               geom_line(aes(colour = LINE, linetype = LINE), size = 1) +
               scale_color_manual(values=c(rev(col)[1], "grey51")) +
               scale_linetype_manual(values=c("Market Value" = "solid", "Cost Basis" =  "solid"),
                                     guide = guide_legend(override.aes = list(size = 2))) +
               scale_y_continuous(labels = scales::dollar_format(scale = .001, suffix = "K")) +
               scale_x_date(date_labels="%b-%y",date_breaks  ="1 month") +
               theme_minimal() +
               theme(legend.position = "bottom",
                     axis.title.x = element_blank(),
                     axis.title.y = element_blank(),
                     legend.title = element_blank())) %>% layout(legend = list(orientation = 'h'))
    
  })
  
  output$assetAllo <- renderPlotly({
    
    if (input$account == "Portfolio") {
      bd <- portfolioSummary %>%
        select(-ACCOUNT) %>%
        group_by(ASSET) %>%
        summarise(AMOUNT = sum(AMOUNT),
                  TARGET_VALUE = sum(TARGET_VALUE),
                  ACTUAL_VALUE = sum(ACTUAL_VALUE)) %>%
        mutate(TARGET = TARGET_VALUE/sum(TARGET_VALUE),
               ACTUAL = ACTUAL_VALUE/sum(ACTUAL_VALUE)) %>%
        mutate_if(is.numeric , replace_na, replace = 0) %>%
        arrange(desc(ACTUAL), .by_group = T) %>%
        mutate(ASSET = fct_inorder(ASSET),
               ACCOUNT = "Portfolio") %>%
        select(ACCOUNT, ASSET, AMOUNT, TARGET, TARGET_VALUE,
               ACTUAL, ACTUAL_VALUE) %>%
        mutate(DEVIATION = round((ACTUAL_VALUE - TARGET_VALUE)/TARGET_VALUE, digits = 4)) %>%
        left_join(assetTypes) %>%
        mutate(CLASS = fct_inorder(CLASS))
      
      ggplotly(ggplot(bd, aes(x = CLASS, y = ACTUAL_VALUE, fill = interaction(ASSET, CLASS))) +
                 geom_bar(position = "stack", stat = "identity", width = .75) +
                 geom_text(data = subset(bd, ACTUAL > .05), aes(label = paste0(ASSET, "\n",
                                                                               percent(ACTUAL, accuracy = .01))), 
                           position = position_stack(vjust = .5)) +
                 theme_minimal() +
                 theme(legend.position = "none",
                       axis.title.x = element_blank(),
                       axis.title.y = element_blank()) +
                 scale_y_continuous(labels=dollar_format(prefix="$")) +
                 scale_fill_manual(values=rev(acc.port.Cols)))
    } else {
      bd <- portfolioSummary %>%
        filter(ACCOUNT == input$account) %>%
        left_join(assetTypes) %>%
        mutate(CLASS = fct_inorder(CLASS)) %>%
        arrange(desc(ACTUAL)) %>%
        mutate(ASSET = fct_inorder(ASSET))
      
      ggplotly(ggplot(bd, aes(x = CLASS, y = ACTUAL_VALUE, fill = ASSET)) +
                 geom_bar(position = "stack", stat = "identity", width = .75) +
                 geom_text(data = subset(bd, ACTUAL > .05), aes(label = paste0(ASSET, "\n",
                                                                               percent(ACTUAL, accuracy = .01))), 
                           position = position_stack(vjust = .5)) +
                 theme_minimal() +
                 theme(legend.position = "none",
                       axis.title.x = element_blank(),
                       axis.title.y = element_blank()) +
                 scale_y_continuous(labels=dollar_format(prefix="$")) +
                 scale_fill_manual(values=rev(acctBlends[[input$account]])))
    }
  })
  
  acct <- reactive(input$account)
  
  displayText <- eventReactive(input$calcContr, {
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
      
      if (input$account == "Portfolio") {
        accountDat <- portfolioSummary
        
        targetTotal <- amount+sum(accountDat$ACTUAL_VALUE)
        
        contrAllo <- suppressMessages(accountDat %>%
                                        select(-ACCOUNT) %>%
                                        group_by(ASSET) %>%
                                        summarise(AMOUNT = sum(AMOUNT),
                                                  TARGET_VALUE = sum(TARGET_VALUE),
                                                  ACTUAL_VALUE = sum(ACTUAL_VALUE)) %>%
                                        mutate(TARGET = TARGET_VALUE/sum(TARGET_VALUE),
                                               ACTUAL = ACTUAL_VALUE/sum(ACTUAL_VALUE)) %>%
                                        mutate_if(is.numeric , replace_na, replace = 0) %>%
                                        left_join(assetPrices) %>%
                                        mutate(NEW_VALUE = targetTotal*TARGET,
                                               DIFFERENCE = case_when(NEW_VALUE-ACTUAL_VALUE < 0 ~ 0,
                                                                      TRUE ~ NEW_VALUE-ACTUAL_VALUE),
                                               ADJ_DIFFERENCE = (DIFFERENCE/sum(DIFFERENCE))*amount,
                                               BUY_SHARES = floor(ADJ_DIFFERENCE/CURRENT_PRICE)) %>%
                                        select(ASSET, BUY_SHARES))
        
        textList <- list()
        for(i in 1:length(unique(contrAllo$ASSET))){
          asset <- contrAllo %>%
            subset(ASSET == unique(contrAllo$ASSET)[i])
          
          action <- ifelse(asset$BUY_SHARES > 0, 
                           paste0("Buy ", asset$BUY_SHARES, " shares of ",
                                  asset$ASSET, "."),
                           paste0("No contributions into ", asset$ASSET,
                                  " at this time."))
          
          textList[[i]] <- action
        }
        
        HTML(renderMarkdown(text = paste("- ", textList, collapse = "\n")))
        
      } else {
        accountDat <- portfolioSummary %>%
          subset(ACCOUNT == account)
        
        targetTotal <- amount+sum(accountDat$ACTUAL_VALUE)
        
        contrAllo <- suppressMessages(accountDat %>%
                                        left_join(assetPrices) %>%
                                        mutate(NEW_VALUE = targetTotal*TARGET,
                                               DIFFERENCE = case_when(NEW_VALUE-ACTUAL_VALUE < 0 ~ 0,
                                                                      TRUE ~ NEW_VALUE-ACTUAL_VALUE),
                                               ADJ_DIFFERENCE = (DIFFERENCE/sum(DIFFERENCE))*amount,
                                               BUY_SHARES = floor(ADJ_DIFFERENCE/CURRENT_PRICE)) %>%
                                        select(ASSET, BUY_SHARES))
        
        textList <- list()
        for(i in 1:length(unique(contrAllo$ASSET))){
          asset <- contrAllo %>%
            subset(ASSET == unique(contrAllo$ASSET)[i])
          
          action <- ifelse(asset$BUY_SHARES > 0, 
                           paste0("Buy ", asset$BUY_SHARES, " shares of ",
                                  asset$ASSET, "."),
                           paste0("No contributions into ", asset$ASSET,
                                  " at this time."))
          
          textList[[i]] <- action
        }
        
        HTML(renderMarkdown(text = paste("- ", textList, collapse = "\n")))
      }
      
    })
  })
  
  observe(displayText())
  
  asset <- reactive(input$enterAsset)
  shares <- reactive(input$enterShares)
  price <- reactive(input$enterPrice)
  
  addingShares <- eventReactive(input$addShare,{
    
    data <- read_csv("FinancialActivity.csv") %>%
      mutate(COST = round(AMOUNT*PRICE, digits = 2)) %>%
      mutate(DATE = as.Date(DATE, format = "%m/%d/%Y"))
    
    newRow <- cbind.data.frame(DATE = Sys.Date(),
                               ACCOUNT = acct(),
                               ASSET = asset(),
                               AMOUNT = shares(),
                               PRICE = price(),
                               COST = shares()*price())
    
    data <- data %>%
      bind_rows(newRow)
    
    output$saveData <- renderUI({input$addShare;
      HTML(paste0("Purchased ", shares(), " shares of ", asset(), " for $", price(),"."))
    })
    
    write.csv(data, "FinancialActivity.csv", row.names = F)
  })
  
  observe(addingShares())
  
  output$acctSumm <- renderUI({
    accountDat <- bind_rows(portfolioSummary, fullSummary) %>%
      subset(ACCOUNT == input$account)
    
    currentVal <- sum(accountDat$ACTUAL_VALUE)
    
    costBasis <- sum(accountDat$COST_BASIS)
    
    currentReturn <- round((currentVal-costBasis)/costBasis*100, digits = 2)
    
    evalReturn <- ifelse(currentReturn > 0, "up", "down")
    
    HTML(paste0("Current ", 
                input$account, 
                " market value is $", 
                prettyNum(currentVal, big.mark = ","),
                ", ", evalReturn, " ", 
                currentReturn, "%",
                " from your cost basis of $",
                prettyNum(costBasis, big.mark = ","),"."))
  })
  
  output$acctRebalance <- renderUI({
    accountDat <- bind_rows(portfolioSummary, fullSummary) %>%
      subset(ACCOUNT == input$account)
    
    currentVal <- sum(accountDat$ACTUAL_VALUE)
    
    costBasis <- sum(accountDat$COST_BASIS)
    
    currentReturn <- round((currentVal-costBasis)/costBasis*100, digits = 2)
    
    evalReturn <- ifelse(currentReturn > 0, "up", "down")
    
    textList <- list()
    for(i in 1:length(unique(accountDat$ASSET))){
      asset <- suppressMessages(accountDat %>%
                                  subset(ASSET == unique(accountDat$ASSET)[i]) %>%
                                  mutate(REBALANCE = case_when(DEVIATION > .05 ~ (ACTUAL_VALUE - (ACTUAL_VALUE-(ACTUAL_VALUE*.025))),
                                                               TRUE ~ 0)) %>%
                                  left_join(assetPrices))
      
      evalDev <- ifelse(asset$DEVIATION > 0, "up", "down")
      
      evalBalance <- ifelse(asset$DEVIATION > .05,
                            paste0("Recommend selling of ",
                                   floor(asset$REBALANCE/asset$CURRENT_PRICE), " ",
                                   "shares."),
                            "No rebalance necessary at this time.")
      
      textList[[i]] <-  paste0(asset$ASSET, " is currently ",
                               round(asset$ACTUAL*100, digits = 2), "% of the account, ",
                               evalDev, " ", round(asset$DEVIATION*100, digits = 2), "%",
                               " from the target of ",
                               round(asset$TARGET*100, digits = 2), "%. ",
                               evalBalance)
    }
    
    HTML(renderMarkdown(text = paste("- ", textList, collapse = "\n")))
    
  })
  
}

shinyApp(ui = ui, server = server)
