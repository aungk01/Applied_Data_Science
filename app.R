#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("global.R", local = TRUE)
library('plotly')
library("dplyr")
library(shinythemes)
library(caret)
# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("superhero"),
  
    # Application title
  titlePanel(h4("Generalized Linear  Model")),
  
  sidebarLayout(
    # Sidebar with a slider input for number of bins 
    sidebarPanel(
      selectInput("plotType", "Choose Task",
                  c(Home = "home", Visualize_Correlation = "correlation", Dementia_vs_Gender_Plot = "box",Visualize_Selected_Variables = "bi_variate",Predict = "predict")
    ),
    conditionalPanel(
      condition = "input.plotType == 'home'",
      # helpText("Home Panel ")
    ),
   
    
    conditionalPanel(
      condition = "input.plotType == 'box'",
      selectInput(inputId = "select_column:",
                  label = "Select Column",
                  choices = c('All','M', 'F'),
                  selected = 'All'),
                 
                  selectInput("Group",'Group',choices=c("All",'Demented','Nonedemented'),
                              selected="All")
                              
                  
        
      ),
    conditionalPanel(
      condition = "input.plotType == 'correlation'",
      selectInput("Cor",'Cor',choices = "Corrolation_Plot")
      ),
    conditionalPanel(
      condition = "input.plotType == 'bi_variate'",
      
      selectInput("sympton","Select Column", choices = c('CDR','EDUC','ASF'), selected= 'CDR'),
      
      selectInput("Group_1","Select Group",choices=c("All",'Demented','Nonedemented'),
                  selected="All")
    ),
    conditionalPanel(
      condition = "input.plotType == 'predict'",
      sliderInput("integer", "Age:",min = 60, max = 100,value = 70),
      #numericInput('age', 'Enter Your Age',60,min = 60 , max = 98),
      sliderInput("edu", "Education Level:",min = 6, max = 23,value = 8),
      #numericInput('edu', 'Enter Education Level', 6,min = 1, max = 23),
      sliderInput("cdr", "CDR Level:",min = 0.0, max = 20.0,value = 5.0),
      #numericInput('cdr', 'Enter CDR Level', 1,min = 0, max = 20),
      sliderInput("asf", "ASF Level:",min = 0.0, max = 20.0,value = 9.0),
      #numericInput('asf', 'Enter ASF  Level', 1,min = 0, max = 20),
      #numericInput('gender', 'Enter Gender', 0,min = 0, max = 1),
      #selectInput('gender',"Enter Gender",choices = c('1', '0'),selected = '1'),
      actionButton("Predict","Predict!"),
      
      ),
    
   
    
),


        # Show a plot of the generated distribution
        mainPanel(
            
            
           
            conditionalPanel(
               h5("Viloin Plot"),
               condition = "input.plotType == 'box'",
               plotOutput("plot_graph")
            
            ),
           
            conditionalPanel(
              h5("Correlation Plot"),
               condition = "input.plotType == 'correlation'",
               plotOutput("plot_cor"),
               
            
          ),
          
               conditionalPanel(
              h3("Predictd Result from the Model"),
              
               condition = "input.plotType == 'predict'",
               htmlOutput("plot_table"),
               #textOutput("plot_table")
              #tableOutput("plot_table")
              h5("Predicted vale: 0.00 means demented "),
              h5("Predicted vale: 1.00 means Nondemented"),
              h5("Having higher CDR and ASF will result in probability of having
                 dementia. In order to see the clear effect of input variable, 
                 enter one input at a time and press Predict!"),
              h5("GLM Model has 89 percent accuracy!")
            
          ),
          conditionalPanel(
            
            h3("Comapring Datset Attributes"),
            condition = "input.plotType == 'bi_variate'",
            #htmlOutput("predict")
            plotOutput("plot_variate")
            #tableOutput("plot_table")
            
          ),
        
           conditionalPanel(
               condition = "input.plotType == 'home'",
               h2("Welcome to Dementia Prediction App"),
               h5("Dementia is a syndrome. It is a group of conditions characterized 
                  by impairment of at least two brain functions, such as memory loss and judgment.
                  Dementia is the term applied to a group of symptoms that negatively impact memory.
                  The exact cause is unknown and the cure is unknown as of today. There are 47.5 million people 
                  around the world are living with dementia according to World Health Organization"),
               h5("The purpose of this App is to predict the chances of having dementia based on the following attributes of the dataset:"),
              
               tags$ul(
                 tags$li(tags$b("ASF"), " - Computed scaling factor that transforms native-space brain and skull to the atlas target
                        "),
                 tags$li(tags$b("CDR"), " - Clinical Dementia Rating (0 = no dementia, 0.5 = very mild AD, 1 = mild AD, 2 = moderate to Advance)"),
                 tags$li(tags$b("Age"), " - Age of the subject"),
                 tags$li(tags$b("Education"), " - Education of the subject"),
                 #
              h5("Below is the dataset link:"),
              tags$a(href='https://www.kaggle.com/datasets/shashwatwork/dementia-prediction-dataset', "Click Here for dataset Information!")
                
               )
           )
        
       )
        
      
   )
  
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  #df <- read.csv('dementia_dataset.csv ')
   
  model <- readRDS("glm_1.rda") 
  
    output$plot_graph <- renderPlot({
      
       plot_graph(df, input$select_column, input$Group)
        
    })
    output$plot_cor <- renderPlot({
      
      plot_cor(df,input$Cor)
      
    })
    
    output$plot_variate<- renderPlot({
      
    plot_variate(df,input$sympton, input$Group_1)
   
       })
    output$plot_table<-renderUI({
      
        f_1()
     
      })
      
       f_1 <-eventReactive(input$Predict,plot_table(input$integer,input$edu,input$cdr,input$asf))
    
    
      
      
    
  
      
    #predict(input$age,input$nWBV,input$CDR,input$SES, input$Predict)
     #(input$age* 0.9650) +(input$edu*0.9205)+(input$cdr*-17.3772 )+ 8.2301 
      #(foo$Age* 0.9650) +(foo$EDUC*0.9205)+(foo$CDR*-17.3772 )+ 8.2301 
      
  
}

# Run the application 
shinyApp(ui = ui, server = server)

