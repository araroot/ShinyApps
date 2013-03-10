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

  sidebarPanel(
    selectInput("index", "Choose an index:",
                choices = idx_choices),
    br(),
    sliderInput("ayears", "Analysis Years:",
                min = 2000, max = 2012, value = c(2005,2012)),
    br(),            
    selectInput("ma.period", "Choose MA period:",
                choices = c('20','50','100','200')),
                
    submitButton("Update View")
  ),
  
  

  # Show a plot of the generated distribution
  mainPanel(
		plotOutput('plot_l2f2', width='60%',height='300px'),
        plotOutput('plot_sma', width='60%',height='300px'),
        plotOutput('plot_equity', width='80%',height='300px'),
        tableOutput("summary_l2f2")
  )
))

