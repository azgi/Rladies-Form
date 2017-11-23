#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# source for mandatory fields: https://deanattali.com/2015/06/14/mimicking-google-form-shiny/
library(shiny)

# global vars

fieldsMandatory <- c("ladysName", "ladysEmail")

labelMandatory <- function(label) {
        tagList(
                label,
                span("*", class = "mandatory_star")
        )
}

appCSS <-
        ".mandatory_star { color: red; }"




# Define UI for application that draws a histogram
ui <- fluidPage(
        shinyjs::useShinyjs(),
        shinyjs::inlineCSS(appCSS),
   # Application title
   titlePanel("RLadies MCR"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
              textInput("ladysName", labelMandatory("R-Lady's Fullname")),
              textInput("ladysEmail", labelMandatory("Email")),
              # How long have they used R
              radioButtons("rTime", "How long have you used R", 
                           choices = c("new learner", "6 months or less", "6 months to 1 year", "more than 1 year")),
              # Tools used
              checkboxGroupInput("rTools", "Which of the following tools have you used before?", 
                          choices = c("R Markdown" = "rmd", "R Shiny" = "rsh", "R pubs" = "rpb")),
              # Consent for being added to other communication channels of the group
              radioButtons("slack", 
                           labelMandatory("Would you like to be added to other communication channels of the group?"),
                           choices = c("Yes", "No"))
              ,actionButton("submit", "Submit")
              
              # Shiny app
              # Favorite package
              # How far they live?
              # Best day of the month / Week
         # sliderInput("bins",
         #             "Number of bins:",
         #             min = 1,
         #             max = 50,
         #             value = 30)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
              # To include the plots & infographics of the submitted form
              # How many R Ladies have joined
              
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'darkgray', border = 'white')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

