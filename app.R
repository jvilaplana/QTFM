library(shiny)
library(queueing)

# Define UI for application
ui <- fluidPage(
    
    theme = "app.css",

    # Application title
    titlePanel("Queueing Theory Model for Fog Computing"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            #img(src="fog-model.png", ),
            numericInput(inputId = "c", label = "# Customers", value = 100, min = 0, max = 500, step = 10),
            numericInput(inputId = "mu", label = "Service rate", value = 1/0.2, min = 0.1, max = 10, step = 0.1),
            numericInput(inputId = "m_ps", label = "# Processing Servers", value = 10, min = 1, max = 500, step = 1),
            numericInput(inputId = "m_fs", label = "# Fog Servers", value = 10, min = 1, max = 500, step = 1),
            sliderInput(inputId = "d", label = "Database access probability", min = 0, max = 1, step = 0.1, value = 0.5),
            sliderInput(inputId = "t", label = "Output server probability", value = 0.5, min = 0, max = 1, step = 0.1),
            sliderInput(inputId = "k", label = "Fog server exit probability", value = 0.5, min = 0, max = 1, step = 0.1),
            actionButton("calc", "Calculate"),
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Diagram", img(src="fog-model.png")),
                        tabPanel("Throughput Plot", plotOutput("throughputPlot")),
                        tabPanel("Response Time Plot", plotOutput("responseTimePlot")),
                        tabPanel("Mean Customers Plot", plotOutput("meanCustomersPlot")),
                        tabPanel("Summary", verbatimTextOutput("sum"))
           #plotOutput("throughputPlot"),
           #verbatimTextOutput("sum")
            )
        )
    )
)

# Define server logic required
server <- function(input, output) {
    
    model <- reactive( {
        # Model parameters
        es_service_rate <- input$mu
        ps_service_rate <- input$mu
        ds_service_rate <- input$mu
        os_service_rate <- input$mu
        cs_service_rate <- input$mu
        fs_service_rate <- input$mu
        
        # Database access probability
        d <- input$d
        t <- input$t
        k <- input$k
        
        # Number of Customers
        n <- input$c
        
        node_es <- NewInput.MM1(lambda=0, mu=es_service_rate, n=0)
        node_ps <- NewInput.MMC(lambda=0, mu=ps_service_rate, c=input$m_ps, n=0)
        node_ds <- NewInput.MM1(lambda=0, mu=ds_service_rate, n=0)
        node_os <- NewInput.MM1(lambda=0, mu=os_service_rate, n=0)
        node_cs <- NewInput.MMC(lambda=0, mu=cs_service_rate, c=input$c, n=0)
        node_fs <- NewInput.MMC(lambda=0, mu=fs_service_rate, c=input$m_fs, n=0)
        
        # Think time = 0
        z <- 0
        
        # Operational value
        operational <- FALSE
        
        # Definition of the transition probabilities matrix
        
        #            ES           PS    DS    OS     CS    FS
        prob_es <- c( 0,           1,    0,    0,     0,    0)
        prob_ps <- c( 0, (1-d)*(1-t),    d,  d*t,     0,    0)
        prob_ds <- c( 0,     d*(1-t),    0,    t,     0,    0)
        prob_os <- c( 0,           0,    0,    0,     1,    0)
        prob_cs <- c( 0,           0,    0,    0,     0,    1)
        prob_fs <- c( k,           0,    0,    0, (1-k),    0)
        
        prob <- matrix(data=c(prob_es, prob_ps, prob_ds, prob_os, prob_cs, prob_fs), nrow=6, ncol=6, byrow=TRUE)
        
        result_throughput <- 0
        result_mean_customers <- 0
        # Mean time spend in a queueing model (or network)
        result_mean_time <- 0
        
        for(n in 1:input$c) {
            # Define a new input for the Closed Jackson Network
            cjn1 <- NewInput.CJN(prob, n, z, operational, 0, 0.001, node_es, node_ps, node_ds, node_os, node_cs, node_fs)
            
            # Check the inputs and build the model
            m_cjn1 <- QueueingModel(cjn1)
            Inputs(m_cjn1)
            
            result_throughput[n] <- m_cjn1$Throughput
            result_mean_customers[n] <- m_cjn1$L
            result_mean_time[n] <- m_cjn1$W
        }
        
        sum <- summary(m_cjn1)
        
        # return all object as a list
        list(througput = result_throughput, mean_customers = result_mean_customers, mean_time = result_mean_time, sum = sum)
    })

    output$throughputPlot <- renderPlot({
        mod_list = model()
        plot(1:input$c, mod_list$througput, main = "Throughput evolution", ylab = "Throughput", xlab = "# Customers", type = "l", col = "blue")
        # hist(x, breaks = bins, col = 'darkgray', border = 'white')
        
    })
    
    output$responseTimePlot <- renderPlot({
        plot(1:input$c, model()$mean_time, main = "Mean time evolution", ylab = "Mean Time", xlab = "# Customers", type = "l", col = "blue")
    })
    
    output$responseTimePlot <- renderPlot({
        plot(1:input$c, model()$mean_customers, main = "Mean customers evolution", ylab = "Mean Customers", xlab = "# Customers", type = "l", col = "blue")
    })
    
    output$sum <- renderPrint({
        model()$sum
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
