library(shiny)

shinyUI(fluidPage(
     titlePanel("Uploading Files"),
          sidebarLayout(
               sidebarPanel(
                    fileInput('file1', 
                         'Choose CSV File',
                         accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                    tags$hr(),
                    checkboxInput('header', 'Header MUST Be Included', TRUE),
                    radioButtons('sep', 
                         'Separator or Delimiter',
                         c(Comma=',', Semicolon=';', Tab='\t'), 
                         ','),
                    radioButtons('quote', 
                         'Quote',
                         c(None='', 'Double Quote'='"', 'Single Quote'="'"),
                         '"'),
                    sliderInput("decimal", "Set Probability Cut-Off Value", 
                         min = 0, max = 1, value = 0.5, step= 0.01),
                    
                    actionButton("goButton", "Predict")
                    

               ),
          mainPanel(
#                tableOutput('contents'),
#                uiOutput('contents')

               h3('Bad Loan Advisor - Documentation'),
               p('Overview: This website is designed to predict whether a new loan applicant will be a good or bad loan risk. We accomplish this by training on historical data (performed previously) and then predicting in real time by uploading a file with a header and individual loan records.'),
               p('Step 1: Upload a file with loan applicant records - Text.csv is provided for you.'),
               p('Step 2: Select the Probability Cut-Off...the value at which you want the model to determine whether the loan was Good or Bad.'),
               p('Step 3: Press Predict. This will run all of the records within the file through the prediction algorithm. An table with the DealID, Probability percentage, and Answer of YES or NO will be output.'),
               p('Step 4: The Probability Cut-Off can then be moved around to the optimal location. Loan applicant answers will change as this occurs.'),
               p('NOTE: There is not a lot of error handling built in here yet...that will come soon, so unfortunately, this app is not all that robust right now.'),

               fluidRow(
                    dataTableOutput(outputId="predict")
                    )
#                textOutput('predict')
#               verbatimTextOutput("summary") 
               )
     )
))

# # Define UI for slider demo application
# shinyUI(fluidPage(
#   
#   #  Application title
#   titlePanel("Sliders"),
#   
#   # Sidebar with sliders that demonstrate various available
#   # options
#   sidebarLayout(
#     sidebarPanel(
#       # Decimal interval with step value
#       sliderInput("decimal", "Decimal:", 
#                   min = 0, max = 1, value = 0.5, step= 0.01),
#     ),
#     
#     # Show a table summarizing the values entered
#     mainPanel(
#       tableOutput("values")
#     )
#   )
# ))
# 
# 
# 
# 
# library(shiny)
# 
# # Define UI for dataset viewer application
# shinyUI(fluidPage(
#   
#   # Application title
#   titlePanel("Reactivity"),
#   
#   # Sidebar with controls to provide a caption, select a dataset,
#   # and specify the number of observations to view. Note that
#   # changes made to the caption in the textInput control are
#   # updated in the output area immediately as you type
#   sidebarLayout(
#     sidebarPanel(
#       textInput("caption", "Caption:", "Data Summary"),
#       
#       selectInput("dataset", "Choose a dataset:", 
#                   choices = c("rock", "pressure", "cars")),
#       
#       numericInput("obs", "Number of observations to view:", 10)
#     ),
#     
#     
#     # Show the caption, a summary of the dataset and an HTML 
#       # table with the requested number of observations
#     mainPanel(
#       h3(textOutput("caption", container = span)),
#       
#       verbatimTextOutput("summary"), 
#       
#       tableOutput("view")
#     )
#   )
# ))