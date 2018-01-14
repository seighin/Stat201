#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Central Limit Theorem Demonstration"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
          selectInput(inputId = "pop.dist", label = "Population distribution:",
                      choices = c("Normal" = "norm",
                                  "Poisson" = "pois",
                                  "Uniform" = "unif")),
          
          numericInput(inputId = "pop.mean", label = "Mean/rate/min:", value=0),
          
          numericInput(inputId = "pop.sd", label = "Standard deviation/max:", value=1, min=0.001)
          
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("pop.plot")
      )
   ),
   
   sidebarLayout(
       sidebarPanel(
           sliderInput("n",
                       "Sample size:",
                       min = 1,
                       max = 50,
                       value = 10),
           
           sliderInput("num.samples",
                       "Number of samples:",
                       min = 1,
                       max = 1000,
                       value = 100
                       )
       ),
       
       
       # Show a plot of the generated distribution
       mainPanel(
           plotOutput("samp.plot")
       )
   )
   
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    pop.info <- reactive({
        # Generate a random population
        pop.size <- 10000
        pop.bins <- 20
        pop.mean <- input$pop.mean
        pop.sd <- input$pop.sd
        pop <- c()
        
        if (input$pop.dist == "norm"){
            pop <- rnorm(pop.size, mean=pop.mean, sd=pop.sd)
        } else if (input$pop.dist == "pois") {
            pop.mean <- max(input$pop.mean,1)
            pop.sd <- sqrt(pop.mean)
            pop <- rpois(pop.size, lambda=pop.mean)
            pop.bins <- min(pop.mean* 1.5, 20)
        }else if (input$pop.dist == "unif"){
            pop <- runif(pop.size, input$pop.mean, input$pop.sd)
            pop.mean <- (input$pop.mean+input$pop.sd)/2
            pop.sd <- (input$pop.sd - input$pop.mean)/sqrt(12)
        }
        
        list(pop=pop, pop.bins=pop.bins, pop.mean=pop.mean, 
             pop.sd=pop.sd, pop.size=pop.size)
    })

    output$pop.plot <- renderPlot({
        pop.info <- pop.info()
        pop <- pop.info$pop
        pop.bins <- pop.info$pop.bins
        
        # Generate a random population
        g <- ggplot(data.frame(pop=pop), aes(x=pop, y=..density..))
        g <- g + geom_histogram(bins=pop.bins, fill="cadetblue", color="black")
        g <- g + theme_bw() + xlab("Population")
        g
        #hist(pop, breaks = 30)
    })
    
    output$samp.plot <- renderPlot({
        pop.info <- pop.info()
        pop <- pop.info$pop
        pop.size <- pop.info$pop.size
        pop.bins <- pop.info$pop.bins
        pop.mean <- pop.info$pop.mean
        pop.sd <- pop.info$pop.sd
        
        set.seed(42)
        
        s.xbar <- data.frame(m<-1:(input$num.samples))
        for (i in 1:input$num.samples){
            s <- sample(pop, input$n)
            s.xbar$m[i] <- mean(s)
        }
        
        clt.sd <- pop.sd / sqrt(input$n)
        
        clt.x <- seq(from=min(pop), to=max(pop), length=1000)
        clt.y <- dnorm(clt.x, mean=pop.mean, sd=clt.sd)
        clt <- data.frame(x=clt.x, y=clt.y)
        
        g <- ggplot(s.xbar, aes(x=m))
        g <- g + geom_histogram(aes(y=..density..), fill="cadetblue", color="black", bins=50)
        g <- g + geom_line(data=clt, mapping=aes(x=x, y=y), size=1.5, color="red", alpha=.6)
        g <- g + theme_bw() + xlab("Sample means")
        g

    })
}

# Run the application 
shinyApp(ui = ui, server = server)

