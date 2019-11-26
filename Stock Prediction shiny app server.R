########### LOAD PACKAGES ###################################
packages <- c('readr',
              'xts',
              'DMwR2',
              'quantmod',
              'TTR',
              'performanceEstimation',
              'nnet',
              'e1071',
              'earth',
              'randomForest',
              'kernlab',
              'RQuantLib',
              'ggplot2',
              #'data.table',
              #'rvest',
              #'tidyverse',
              #'stringr',
              #'forecast',
              #'lubridate',
              'plotly'
              #'dplyr',
              #'PerformanceAnalytics'
)

# Checking for package installations on the system and installing if not found
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    install.packages(setdiff(packages, rownames(installed.packages())))  
}

# Including the packages for use
for(package in packages){
    library(package, character.only = TRUE)
}


########### CREATE TECHNICAL INDICATORS ###################################

myATR         <- function(x) ATR(HLC(x))[,'atr'] # Average True Range, measures volatility of series  
mySMI         <- function(x) SMI(HLC(x))[, "SMI"] #  Stochastic Momentum Index 
myADX         <- function(x) ADX(HLC(x))[,'ADX'] # Welles Wilder's Directional Movement Index 
myAroon       <- function(x) aroon(cbind(Hi(x),Lo(x)))$oscillator # Identify starting trends
myBB          <- function(x) BBands(HLC(x))[, "pctB"] # Bollinger Bands
myChaikinVol  <- function(x) Delt(chaikinVolatility(cbind(Hi(x),Lo(x))))[, 1] # Chaikin Volatility
myCLV         <- function(x) EMA(CLV(HLC(x)))[, 1] # Close Location Value 
myEMV         <- function(x) EMV(cbind(Hi(x),Lo(x)),Vo(x))[,2] # Arms' Ease of Movement Value 
myMACD        <- function(x) MACD(Cl(x))[,2] # Moving Average Convergence Divergence
myMFI         <- function(x) MFI(HLC(x), Vo(x)) # Money Flow Index
mySAR         <- function(x) SAR(cbind(Hi(x),Cl(x))) [,1] # Parabolic Stop-and-Reverse
myVolat       <- function(x) volatility(OHLC(x),calc="garman")[,1] # volatility


################## BUILD SHINY APP ##################  

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  ################## DEFINE INPUT VARIABLES ##################      
        datasetInput <- reactive({
            switch(input$symb,
                   "FB"= "FB",
                   "MSFT"= "MSFT",
                   "AAPL"="AAPL",
                   "GOOGL"="GOOGL",
                   "GS"="GS"
            )
        })
        
        selectedmodelInput <- reactive({
            switch(input$selectmodel,
                   "SVM"= "svm",
                   "MARS"= "mars",
                   "NNET"="nnet"
            )
        })
    
################## GENERATE CLOSING PRICE PLOT  ##################       
        
        output$distPlot <- renderPlot({
   
        ticker<-  datasetInput()
        my.model<- selectedmodelInput()
        
        dates <- seq(from=as.Date(Sys.Date()+1), to=as.Date(Sys.Date()+10), by=1)
        bizdates <- isBusinessDay("UnitedStates/NYSE", dates) 
        bizdates.df <- data.frame(dates,bizdates)
        pred.dates.df<-subset(bizdates.df, bizdates.df$bizdates=="TRUE")
        dates.pred <- pred.dates.df[1:5,1]
        
        stock.data <-as.data.frame(assign(ticker,getSymbols(ticker,from = "2017-01-01", env = NULL,auto.assign = F)[,4] ))
        for (i in 1:5) {
            #my.model<- "svm"      
            data.model<-stock.data
            #data.model$myATR <- myATR(data.model)
            #data.model$myBB <- myBB(data.model)
            data.model$EMA.Delt <- EMA(Delt(Cl(data.model)))
            data.model$RSI <- RSI(Cl(data.model))
            #data.model$mySAR <- mySAR(data.model)
            data.model$runMean <- runMean(Cl(data.model))
            
            data.model$Close <- data.model[,1]
            data.model <- na.omit(data.model)
            data.model <- data.model[,c(2:ncol(data.model))]
            
            
            Tdata.train <- data.model[1:(nrow(data.model)-100),]
            Tdata.eval <- data.model[(nrow(data.model)-99):nrow(data.model),]
            Tform <- as.formula('Close ~ .')
            
            days.pred <- 1
            
            best.model<-if (my.model == "nnet") { 
                nnet(Tform,
                     data=Tdata.train, 
                     size = 5, 
                     decay = 0.01, 
                     maxit = 1000, 
                     linout = TRUE, 
                     trace = FALSE)
            } else if (my.model == "svm") {
                svm(Tform, 
                    Tdata.train, 
                    cost=5, 
                    gamma=0.05)
            } else {
                earth(Tform, 
                      Tdata.train)
            }
            
            model.pred <- predict(best.model,newdata=data.frame(
                EMA.Delt = days.pred,
                RSI = days.pred,
                runMean = days.pred,
                Close = days.pred))
            
            
            stock.data<-rbind(stock.data,c(model.pred[1]))
            rownames(stock.data)[nrow(stock.data)]<- as.character(pred.dates.df[i,1])
            
        }
        
         
        myxts<-as.xts(stock.data)
                         
      
        ggplot(data=myxts, aes(x = Index,y = myxts[,1])) + 
            geom_line(col="#0072B2")+
            ggtitle("Closing Price: Next 5 Days") +
            theme(plot.title = element_text(size=14, face="bold"))+
            xlab("Date") + 
            ylab("Closing Stock Price")+
            theme_classic()
        
        })
  
        
        
################## GENERATE PREDICTED RESULTS DATA FRAME   ##################
        
        output$predDF <- DT::renderDataTable({  
          ticker<-  datasetInput()
          df<-as.data.frame(assign(ticker,getSymbols(ticker,from = "2017-01-01", env = NULL,auto.assign = F)[,4] ))
          #df<- df[(nrow(df)-4):nrow(df),]
          
          ticker<-  datasetInput()
          my.model<- selectedmodelInput()
          
          dates <- seq(from=as.Date(Sys.Date()+1), to=as.Date(Sys.Date()+10), by=1)
          bizdates <- isBusinessDay("UnitedStates/NYSE", dates) 
          bizdates.df <- data.frame(dates,bizdates)
          pred.dates.df<-subset(bizdates.df, bizdates.df$bizdates=="TRUE")
          dates.pred <- pred.dates.df[1:5,1]
          
          stock.data <-as.data.frame(assign(ticker,getSymbols(ticker,from = "2017-01-01", env = NULL,auto.assign = F)[,4] ))
          for (i in 1:5) {
            #my.model<- "svm"      
            data.model<-stock.data
            #data.model$myATR <- myATR(data.model)
            #data.model$myBB <- myBB(data.model)
            data.model$EMA.Delt <- EMA(Delt(Cl(data.model)))
            data.model$RSI <- RSI(Cl(data.model))
            #data.model$mySAR <- mySAR(data.model)
            data.model$runMean <- runMean(Cl(data.model))
            
            data.model$Close <- data.model[,1]
            data.model <- na.omit(data.model)
            data.model <- data.model[,c(2:ncol(data.model))]
            
            
            Tdata.train <- data.model[1:(nrow(data.model)-100),]
            Tdata.eval <- data.model[(nrow(data.model)-99):nrow(data.model),]
            Tform <- as.formula('Close ~ .')
            
            days.pred <- 1
            
            best.model<-if (my.model == "nnet") { 
              nnet(Tform,
                   data=Tdata.train, 
                   size = 5, 
                   decay = 0.01, 
                   maxit = 1000, 
                   linout = TRUE, 
                   trace = FALSE)
            } else if (my.model == "svm") {
              svm(Tform, 
                  Tdata.train, 
                  cost=5, 
                  gamma=0.05)
            } else {
              earth(Tform, 
                    Tdata.train)
            }
            
            model.pred <- predict(best.model,newdata=data.frame(
              EMA.Delt = days.pred,
              RSI = days.pred,
              runMean = days.pred,
              Close = days.pred))
            
            
            stock.data<-rbind(stock.data,c(model.pred[1]))
            stock.datdf<-stock.data
            rownames(stock.data)[nrow(stock.data)]<- as.character(pred.dates.df[i,1])
            
          }
          
          df<-(stock.data)
          names(df)<-c("Closing_Price")
          is.num <- sapply(df, is.numeric)
          df[is.num] <- lapply(df[is.num], round, 2)
          DT::datatable(tail(df,5), caption = "The predicted closing price over the next 5 days.")
        })
        
        
})      

