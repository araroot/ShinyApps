library(shiny)

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Hello Shiny!"),

  # Sidebar with a slider input for number of observations
  sidebarPanel(
    textInput("symbol", 
                "Ticker Symbol:", 
                "RCOM.NS"),
    numericInput("n", "Last k and first k days", 2),
    
    submitButton("Update View")
  ),
  
  

  # Show a plot of the generated distribution
  mainPanel(
		plotOutput('plot', width='60%',height='300px'),
        tableOutput("summary")
  )
))

