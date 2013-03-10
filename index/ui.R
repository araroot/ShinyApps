library(shiny)

idx_choices = c('cnxnifty', 'cnx500', 'cnxmidcap', 'cnxsmallcap')
idx_choices = c(idx_choices, 'cnxauto', 'cnxmedia', 'cnxconsuption','cnxbank')
idx_choices = c(idx_choices, 'cnxdivopp', 'cnxenergy', 'cnxfinance','cnxpsubank')
idx_choices = c(idx_choices, 'cnxalpha', 'cnxlowvol', 'cnxhighbeta', 'cnxmetal')
idx_choices = c(idx_choices, 'cnxmnc', 'cnxpharma', 'cnxfmcg', 'cnxit')

# Define UI for application that plots random distributions 
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("SMA and L2F2"),

  # Sidebar with a slider input for number of observations
  sidebarPanel(
    #textInput("symbol", "Ticker Symbol:", "^NSEI"),
    selectInput("index", "Choose an index:",
                choices = idx_choices),
    br(),
    sliderInput("n", 
                "Period for SMA:", 
                 value = 50,
                 min = 5, 
                 max = 200),
    submitButton("Update View")
  ),
  
  

  # Show a plot of the generated distribution
  mainPanel(
		plotOutput('plot_l2f2', width='60%',height='300px'),
        plotOutput('plot_sma', width='60%',height='300px'),
        plotOutput('plot_any', width='60%',height='300px'),
        plotOutput('plot_both', width='60%',height='300px'),
        tableOutput("summary_l2f2"),
        tableOutput("summary_sma")
  )
))

