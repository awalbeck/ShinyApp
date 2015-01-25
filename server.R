library(shiny)

library(randomForest)
library(doParallel)
library(reshape2)
library(foreach)
library(ellipse)
library(rattle)
library(caret)
library(party)
library(ROCR)


shinyServer(function(input, output) {
     dataset <- reactive({
    
          # input$file1 will be NULL initially. After the user selects
          # and uploads a file, it will be a data frame with 'name',
          # 'size', 'type', and 'datapath' columns. The 'datapath'
          # column will contain the local filenames where the data can
          # be found.

          inFile <- input$file1

          if (is.null(inFile))
          return(NULL)
    
          read.csv(
               inFile$datapath, 
               header=input$header, 
               sep=input$sep, 
     	     quote=input$quote)

     })

     
     output$predict <- renderDataTable({

          if (input$goButton == 0)
          return()
          
          df <- dataset()

          # PREDICTION PREPARATION
          # Now we need to execute the same transformations and subsetting that we did when we TRAINED

          # acquireDate              <- format(Sys.Date(), "%m/%d/%Y") # Used for PREDICTIONS (Today's date) Instead of the next set of variables
          acquireDate              <- "12/17/2014"     # Date we acquired this dataset
                    
          columnsToModify <- names(df)[sapply(df, is.factor)]
          
          for(i in columnsToModify) {
               featureValues <- unique(df[,i])
               featureValues <- featureValues[!is.na(featureValues) & !featureValues==""]
               isDate <- all(grepl('[0-9]{1,4}/|-[0-9]{1,2}/|-[0-9]{2,4}.*', featureValues))
               if(isDate){
                    cat(i, "is a date column!\n")
                    if(sum(is.na(as.POSIXct(featureValues, format="%m/%d/%Y"))) > 0){
                         cat(i, "failed to parse all values\n")
                    }
                    df[,i] <- as.integer(as.POSIXct(df[,i], format="%m/%d/%Y"))
               }
          }
          df$Business.Start.Date <- as.integer(as.POSIXct(df$Business.Start.Date, format="%m/%d/%Y"))
          
          columnsToModify <- names(df)[sapply(df, is.factor)]
          
          for(i in columnsToModify) {
               featureValues <- unique(df[,i])
               featureValues <- featureValues[!is.na(featureValues) & !featureValues==""]
               isDollars <- all(grepl('^\\$', featureValues))
               if(isDollars){
                    cat(i, "is a dollar column!\n")
                    if(sum(is.na(as.numeric(gsub(pattern="\\$|,| ", "", featureValues, perl=T)))) > 0){
                         cat(i, "failed to parse all values\n")
                    }
                    df[,i] <- as.numeric(gsub(pattern="\\$|,| ", "", df[,i], perl=T))
               }
          }
          
          columnsToModify <- names(df)[sapply(df, is.factor)]
          
          for(i in columnsToModify) {
               featureValues <- unique(df[,i])
               featureValues <- featureValues[!is.na(featureValues) & !featureValues==""]
               if(sum(grepl('^\\$', featureValues)) > 0.1 * length(featureValues)) {
                    cat(i, "is a dollar column!\n")
                    if(sum(is.na(as.numeric(gsub(pattern="\\$|,| ", "", featureValues, perl=T)))) > 0){
                         cat(i, "failed to parse all values\n")
                    }
                    df[,i] <- gsub(pattern="\\(", "-", df[,i], perl=T)
                    df[,i] <- gsub(pattern="\\)", "", df[,i], perl=T)
                    df[,i] <- as.numeric(gsub(pattern="\\$|,| ", "", df[,i], perl=T))
               }
          }
          
          columnsToModify <- names(df)[sapply(df, is.factor)]
          
          for(i in columnsToModify) {
               featureValues <- unique(df[,i])
               featureValues <- featureValues[!is.na(featureValues) & !featureValues==""]
               if(sum(grepl('%$', featureValues)) > 0.1 * length(featureValues)) {
                    cat(i, "has percent values!\n")
                    if(sum(is.na(as.numeric(gsub(pattern="%", "", featureValues, perl=T)))) > 0){
                         cat(i, "failed to parse all values\n")
                    }
                    df[,i] <- as.numeric(gsub(pattern="%", "", df[,i], perl=T))
               }
          }
          
          df$Length.of.Ownership <- gsub(" ", "", df$Length.of.Ownership, ignore.case=TRUE)           
          df$Length.of.Ownership <- gsub("+", "", df$Length.of.Ownership, ignore.case=TRUE)
          df$Length.of.Ownership <- gsub(",", "", df$Length.of.Ownership, ignore.case=TRUE)
          df$Length.of.Ownership <- gsub("n/a", "", df$Length.of.Ownership, ignore.case=TRUE)
          df$Length.of.Ownership <- gsub("and", "", df$Length.of.Ownership, ignore.case=TRUE)
          
          df$Length.of.Ownership <- gsub("months", "/12", df$Length.of.Ownership, ignore.case=TRUE)
          df$Length.of.Ownership <- gsub("month", "/12", df$Length.of.Ownership, ignore.case=TRUE)
          df$Length.of.Ownership <- gsub("moths", "/12", df$Length.of.Ownership, ignore.case=TRUE)
          df$Length.of.Ownership <- gsub("mths", "/12", df$Length.of.Ownership, ignore.case=TRUE)
          df$Length.of.Ownership <- gsub("mth", "/12", df$Length.of.Ownership, ignore.case=TRUE)
          df$Length.of.Ownership <- gsub("mos", "/12", df$Length.of.Ownership, ignore.case=TRUE)
          
          df$Length.of.Ownership <- gsub("yearrs", "+", df$Length.of.Ownership, ignore.case=TRUE)
          df$Length.of.Ownership <- gsub("yaers", "+", df$Length.of.Ownership, ignore.case=TRUE)
          df$Length.of.Ownership <- gsub("years", "+", df$Length.of.Ownership, ignore.case=TRUE)
          df$Length.of.Ownership <- gsub("year", "+", df$Length.of.Ownership, ignore.case=TRUE)
          df$Length.of.Ownership <- gsub("yrs", "+", df$Length.of.Ownership, ignore.case=TRUE)
          df$Length.of.Ownership <- gsub("yr", "+", df$Length.of.Ownership, ignore.case=TRUE)
          
          df$Length.of.Ownership <- gsub("[[:alpha:]]", "", df$Length.of.Ownership)
          df$Length.of.Ownership <- gsub("\\(", "", df$Length.of.Ownership, ignore.case=TRUE)
          df$Length.of.Ownership <- gsub("\\)", "", df$Length.of.Ownership, ignore.case=TRUE)
          df$Length.of.Ownership <- gsub("\\.$", "", df$Length.of.Ownership, ignore.case=TRUE)
          df$Length.of.Ownership <- gsub("\\+$", "", df$Length.of.Ownership, ignore.case=TRUE)
          df$Length.of.Ownership <- gsub("\\+$", "", df$Length.of.Ownership, ignore.case=TRUE)
          df$Length.of.Ownership <- gsub("\\-$", "", df$Length.of.Ownership, ignore.case=TRUE)
          
          for(i in seq_along(df$Length.of.Ownership)){
               if(is.na(df$Length.of.Ownership[i]) | (df$Length.of.Ownership[i]=="")){     
                    if(!is.na(df$Business.Start.Date[i])){                 # If length is missing, calculate it by using Business.Start.Date
                         df$Length.of.Ownership[i] <- as.character(round(as.numeric(as.integer(as.POSIXct(acquireDate, format = "%m/%d/%Y")-(df$Business.Start.Date[i])))/(60*60*24*365), 1))
                    }else{
                         df$Length.of.Ownership[i] <- as.character(round(as.numeric(as.integer(as.POSIXct(acquireDate, format = "%m/%d/%Y")-(mean(df$Business.Start.Date, na.rm=TRUE))))/(60*60*24*365), 1)) # Absolute worst case, replace with "imputed" mean
                    }
               }else if(df$Length.of.Ownership[i]>0){
                    df$Length.of.Ownership[i] <- eval(parse(text=df$Length.of.Ownership[i]))
               }
          }
          
          df$Length.of.Ownership <- as.numeric(df$Length.of.Ownership)
          
          df$Business.Zip <- gsub(" ", "", df$Business.Zip,ignore.case=TRUE)
          df$Business.Zip <- gsub("-.*", "", df$Business.Zip)
          df$Business.Zip <- as.numeric(substr(df$Business.Zip, 1, nchar(df$Business.Zip)-2))
          
          df$Billing.Zip <- gsub(" ", "", df$Billing.Zip,ignore.case=TRUE)
          df$Billing.Zip <- gsub("-.*", "", df$Billing.Zip)
          df$Billing.Zip <- as.numeric(substr(df$Billing.Zip, 1, nchar(df$Billing.Zip)-2))
          
          df$Principal.1.Zip <- gsub(" ", "", df$Principal.1.Zip,ignore.case=TRUE)
          df$Principal.1.Zip <- gsub("-.*", "", df$Principal.1.Zip)
          df$Principal.1.Zip <- as.numeric(substr(df$Principal.1.Zip, 1, nchar(df$Principal.1.Zip)-2))
          
          df$Principal.2.Zip <- gsub(" ", "", df$Principal.2.Zip,ignore.case=TRUE)
          df$Principal.2.Zip <- gsub("-.*", "", df$Principal.2.Zip)
          df$Principal.2.Zip <- as.numeric(substr(df$Principal.2.Zip, 1, nchar(df$Principal.2.Zip)-2))
          
          df$Business.ST <- as.factor(ifelse(!is.na(state.abb[match(df$Business.ST, state.name)]), as.character(state.abb[match(df$Business.ST, state.name)]), as.character(df$Business.ST)))
          df$Business.ST <- as.factor(toupper(df$Business.ST))        # Convert to upper case
          
          df$Billing.ST <- as.factor(ifelse(!is.na(state.abb[match(df$Billing.ST, state.name)]), as.character(state.abb[match(df$Billing.ST, state.name)]), as.character(df$Billing.ST)))
          df$Billing.ST <- as.factor(toupper(df$Billing.ST))          # Convert to upper case
          
          df$Principal.1.ST <- as.factor(ifelse(!is.na(state.abb[match(df$Principal.1.ST, state.name)]), as.character(state.abb[match(df$Principal.1.ST, state.name)]), as.character(df$Principal.1.ST)))
          df$Principal.1.ST <- as.factor(toupper(df$Principal.1.ST))          # Convert to upper case
          
          df$Business.Type <- as.factor(substr(as.character(df$Business.Type), 1, 4))
          df$Business.Type <- as.factor(toupper(gsub(" ", "", df$Business.Type, ignore.case=TRUE)))
          
          df$Principal.1...Ownership <- ifelse(is.na(df$Principal.1...Ownership), 100, df$Principal.1...Ownership)
          df$Principal.2...Ownership <- ifelse(is.na(df$Principal.2...Ownership), 100-df$Principal.1...Ownership, df$Principal.2...Ownership)
          
          meanFICO1 <- mean(df$Principal.1.FICO, na.rm=TRUE)      # 554.5386
          meanFICO2 <- mean(df$Principal.2.FICO, na.rm=TRUE)      # 597.4627
          df$Principal.1.FICO <- ifelse(is.na(df$Principal.1.FICO), ifelse(is.na(df$Principal.2.FICO), meanFICO1, df$Principal.2.FICO), df$Principal.1.FICO)
          df$Principal.2.FICO <- ifelse(is.na(df$Principal.2.FICO), meanFICO2, df$Principal.2.FICO)

          i <- sapply(df, is.factor)                                  # Get boolean list of "factor" variables
          
          df[i] <- lapply(df[i], as.character)                        # Turn those "factor" variables into "character" variables
           
          high_level_factors <- (1:(ncol(df)))[sapply(df, is.character) & (sapply(df, function(x) length(unique(x))) >31)]
          
          for(i in high_level_factors) {
          #     df[,i] <- toupper(df[,i])                              # First convert to upper case
               df[,i][is.na(df[,i])] <- "MISSING"                     # Replace the NA's in this variable with "MISSING"
               df[,i] <- as.numeric(as.factor(df[,i]))                # Change "strings" to numbers (factor levels)
          }
          
          factor_cols <- (1:(ncol(df)))[sapply(df, is.numeric) & sapply(df, function(x) any(is.na(x))) & (sapply(df, function(x) length(unique(x))) <= 15)]
          for(i in factor_cols) {
               df[,i] <- as.character(df[,i])                         # Change numeric variable to "character" variable
               df[,i][is.na(df[,i])] <- "MISSING"                     # If numeric, replace NA's in this variable with "MISSING"
               df[,i] <- as.factor(df[,i])                            #
          }
          
          cut_cols <- (1:(ncol(df)))[sapply(df, is.numeric) & sapply(df, function(x) any(is.na(x)))]
          #names(df)[cut_cols]
          for(i in cut_cols) {
                  df[,i][is.na(df[,i])] <- 10^99
          }
          
          i <- sapply(df, is.factor)
          df[i] <- lapply(df[i], as.character)
          df[is.na(df)] <- "MISSING"
          i <- sapply(df, is.character)
          df[i] <- lapply(df[i], as.factor)
          
          df$CCTotal <- df$Visa.Monthly.Average + df$MasterCard.Monthly.Average + df$Discover.Monthly.Average + df$Amex.Monthly.Average
          df$CCTotal <- replace(df$CCTotal, (df$CCTotal==0) & (df$Processing.Transactions.Average>0), 1)
          df$Processing.Transactions.Average <- replace(df$Processing.Transactions.Average, (df$CCTotal>0) & (df$Processing.Transactions.Average==0), 1)
          df$CCTotal <- df$CCTotal/df$Processing.Transactions.Average
          df$CCTotal <- replace(df$CCTotal, df$CCTotal=="NaN", 0)

          DealID <- df$Deal.ID
          df <- subset(df, select = -c(Deal.ID, Status, Sub.Status, Closing.Date, Total.Collected, Funded.Date,
                             Last.Batch, Billing.City, Business.City, Principal.1.City, Principal.2.Zip,
                             Submission.Date, Principal.3...Ownership, Principal.3.Address, Principal.3.City,
                             Principal.3.ST, Principal.3.Zip, Principal.3.FICO, X, X1, X2, X3))
          
#           df <- df[, lapply(df, function(x) sum(is.na(x))) < (0.995*nrow(df))]       # Mostly NULL
#           df <- df[sapply(df, function(x) length(unique(x))) !=1]                    # ALL the same value
# 
#           i <- sapply(df, is.factor)
#           df[i] <- lapply(df[i], as.character)
#           df[i] <- lapply(df[i], as.numeric)
# 
#           df$Type.of.Entity <- as.factor(df$Type.of.Entity)
#           df$Principal.1...Ownership <- as.numeric(df$Principal.1...Ownership)

          load("PSCmodel2.RData")
          
          new_prob <- data.frame(predict(rf_fit, newdata=df, type="prob"))
          df$ProbGoodLoan <- new_prob$X1           

          df$PREDICTION <- as.character(df$ProbGoodLoan > input$decimal)
          if(sum(df$PREDICTION==TRUE) > 0){
               df[df$PREDICTION == "TRUE",]$PREDICTION <- "YES"
          }
          if(sum(df$PREDICTION==FALSE) > 0){
               df[df$PREDICTION == "FALSE",]$PREDICTION <- "NO"
          }         

#          write.csv(file="TestOut.csv", x=df)

          df$Deal.ID <- DealID
          myvars <- c("Deal.ID", "ProbGoodLoan", "PREDICTION")
          df <- df[myvars]

          df

     })
     
#      dataset <- reactive({
#                validate(
#                need(input$data != "", "Please select a data set")
#           )
#      })
     
     
     output$summary <- renderPrint({
#           dataset <- output$contents()
#            print(dataset)
           summary("This is a test to see if I can put something here that will render correctly.")
  })
     
})
