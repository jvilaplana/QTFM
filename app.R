# This code will install required packages if they are not already installed
if (!require("shiny")) {
    install.packages("shiny")
    library(shiny)
}
if (!require("queueing")) {
    install.packages("queueing")
    library(queueing)
}
if (!require("magick")) {
    install.packages("magick")
    library(magick)
}
if (!require("ggplot2")) {
    install.packages("ggplot2")
    library(ggplot2)
}

# Define UI for application
ui <- fluidPage(
    
    theme = "app.css",

    # Application title
    titlePanel("Queueing Theory Model for Fog Computing"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            #img(src="fog-model.png", ),
            numericInput(inputId = "sla", label = HTML("<i>SLA</i>: Guaranteed response time"), value = 4, min = 0, max = 10, step = 1),
            numericInput(inputId = "c", label = HTML("<i>J</i>: # Jobs (Customers)"), value = 100, min = 0, max = 500, step = 10),
            numericInput(inputId = "mu_es", label = HTML("&#181;<sup>E</sup>: Service rate (ES)"), value = 9/10, min = 0.1, max = 1, step = 0.01),
            numericInput(inputId = "mu_ds", label = HTML("&#181;<sup>D</sup>: Service rate (DS)"), value = 4/10, min = 0.1, max = 1, step = 0.01),
            numericInput(inputId = "mu_ps", label = HTML("&#181;<sup>P</sup>: Service rate (PS)"), value = 4/10, min = 0.1, max = 1, step = 0.01),
            numericInput(inputId = "mu_os", label = HTML("&#181;<sup>O</sup>: Service rate (OS)"), value = 4/10, min = 0.1, max = 1, step = 0.01),
            numericInput(inputId = "mu_fs", label = HTML("&#181;<sup>F</sup>: Service rate (FS)"), value = 4/10, min = 0.1, max = 1, step = 0.01),
            numericInput(inputId = "mu_cs", label = HTML("&#181;<sup>C</sup>: Service rate (CS)"), value = 4/10, min = 0.1, max = 1, step = 0.01),
            numericInput(inputId = "m_ps", label = HTML("R: # Processing Servers"), value = 10, min = 1, max = 500, step = 1),
            numericInput(inputId = "m_fs", label = HTML("N: # Fog Servers"), value = 10, min = 1, max = 500, step = 1),
            numericInput(inputId = "m_cs", label = HTML("M: # Client Servers"), value = 10, min = 1, max = 500, step = 1),
            sliderInput(inputId = "d", label = HTML("&delta;: Database access probability"), min = 0, max = 1, step = 0.1, value = 0.5),
            sliderInput(inputId = "t", label = HTML("&tau;: Output server probability"), value = 0.5, min = 0, max = 1, step = 0.1),
            sliderInput(inputId = "k", label = HTML("&kappa;: Fog server exit probability"), value = 0.5, min = 0, max = 1, step = 0.1),
            actionButton("calc", "Calculate"),
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        #tabPanel("Diagram", img(src="fog-model.png")),
                        tabPanel("Diagram", imageOutput("imgDiagram")),
                        tabPanel("Performance Plots",
                                 plotOutput("throughputPlot"),
                                 plotOutput("responseTimePlot"),
                                 plotOutput("meanCustomersPlot"),
                                 plotOutput("ROkPlot")),
                        #tabPanel("Throughput Plot", plotOutput("throughputPlot")),
                        #tabPanel("Response Time Plot", plotOutput("responseTimePlot")),
                        #tabPanel("Mean Customers Plot", plotOutput("meanCustomersPlot")),
                        #tabPanel("Node Usage Plot", plotOutput("ROkPlot")),
                        tabPanel("Summary", verbatimTextOutput("sum"))
           #plotOutput("throughputPlot"),
           #verbatimTextOutput("sum")
            )
        )
    )
)

# Define server logic required
server <- function(input, output) {
    
    diagram <- image_read("www/fog-model.png")
    
    model <- reactive( {
        # Model parameters
        es_service_rate <- input$mu_es * 2
        ps_service_rate <- input$mu_ps
        ds_service_rate <- input$mu_ds * 3.5
        os_service_rate <- input$mu_os * 3.5
        cs_service_rate <- input$mu_cs
        fs_service_rate <- input$mu_fs
        
        # Database access probability
        d <- input$d
        t <- input$t
        k <- input$k
        
        # Number of Customers
        n <- input$c
        
        # SLA
        sla_time <- input$sla
        
        node_es <- NewInput.MM1(lambda=0, mu=es_service_rate, n=0)
        node_ps <- NewInput.MMC(lambda=0, mu=ps_service_rate, c=input$m_ps, n=0)
        node_ds <- NewInput.MM1(lambda=0, mu=ds_service_rate, n=0)
        node_os <- NewInput.MM1(lambda=0, mu=os_service_rate, n=0)
        node_cs <- NewInput.MMC(lambda=0, mu=cs_service_rate, c=input$m_cs, n=0)
        node_fs <- NewInput.MMC(lambda=0, mu=fs_service_rate, c=input$m_fs, n=0)
        
        # Think time = 0
        z <- 0
        
        # Operational value
        operational <- FALSE
        
        # Definition of the transition probabilities matrix
        
        #            ES           PS    DS        OS     CS    FS
        prob_es <- c( 0,           1,    0,       0,     0,    0)
        prob_ps <- c( 0, (1-d)*(1-t),    d, (1-d)*t,     0,    0)
        prob_ds <- c( 0,       (1-t),    0,       t,     0,    0)
        prob_os <- c( 0,           0,    0,       0,     1,    0)
        prob_cs <- c( 0,           0,    0,       0,     0,    1)
        prob_fs <- c( k,           0,    0,       0, (1-k),    0)
        
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
        
        
        # L:    Returns the mean number of customers of a Closed Jackson Network
        # W:    Returns the mean time spend in a Closed Jackson Network
        # X:    (Throughput?)
        # Lk:   Returns the vector with the mean number of customers in each node (server) of a Closed Jackson Network
        # Wk:   Returns the vector with the mean time spend in each node (server) of a Closed Jackson Network
        # Xk:   (Throughput for each node)
        # ROk:  Reports a vector with each node (server) use of a Closed Jackson Network.
        
        
        sum <- summary(m_cjn1)
        
        # return all object as a list
        list(througput = result_throughput, mean_customers = m_cjn1$Lk, mean_time = result_mean_time, sum = sum, rok = m_cjn1$ROk, sla_time = sla_time)
    })
    
    output$imgDiagram <- renderImage({
        tmp_file <- diagram %>%
            image_annotate(input$c, color = "black", size = 30, location = "+40+20") %>%
            image_write(tempfile(fileext='png'), format = 'png')
            
        list(src = tmp_file, contentType = "image/png")
    }, deleteFile = TRUE)

    output$throughputPlot <- renderPlot({
        m = model()
        df <- data.frame(cust=c(1:input$c), thro=m$througput)
        ggplot(df, aes(x=cust, y=thro,)) + geom_point() + geom_smooth() + labs(title="Throughput evolution") + xlab("# Jobs") + ylab("Throughput (jobs/time unit)") + theme(text = element_text(size=18))
        #plot(1:input$c, m$througput, main = "Throughput evolution", ylab = "Throughput", xlab = "# Customers", type = "l", col = "blue")
    })
    
    output$responseTimePlot <- renderPlot({
        m = model()
        df <- data.frame(cust=c(1:input$c), thro=m$mean_time)
        ggplot(df, aes(x=cust, y=thro,)) + geom_point() + geom_smooth() + 
            geom_hline(yintercept=m$sla_time, linetype="dashed", color = "red", size = 1.5) + 
            geom_text(aes(0, m$sla_time,label = "SLA Limit", vjust = -1)) + 
            labs(title="Mean time evolution") + xlab("# Jobs") + ylab("Mean time") + 
            theme(text = element_text(size=18))
        #plot(1:input$c, m$mean_time, main = "Mean time evolution", ylab = "Mean Time", xlab = "# Customers", type = "l", col = "blue")
    })
    
    output$meanCustomersPlot <- renderPlot({
        m = model()
        df <- data.frame(cust=c(1:length(m$mean_customers)), thro=m$mean_customers)
        ggplot(df, aes(x=cust, y=thro)) + geom_bar(stat = "identity") + labs(title="Mean customers evolution") + xlab("# Jobs") + ylab("Mean jobs") + theme(text = element_text(size=18))
        #plot(1:input$c, model()$mean_customers, main = "Mean customers evolution", ylab = "Mean Customers", xlab = "# Customers", type = "l", col = "blue")
    })
    
    output$ROkPlot <- renderPlot({
        m = model()
        df <- data.frame(cust=c("ES", "PS", "DS", "OS", "CS", "FS"), thro=m$rok)
        ggplot(df, aes(x=cust, y=thro)) + geom_bar(stat = "identity") + labs(title="Node Usage") + xlab("Node") + ylab("Usage") + theme(text = element_text(size=18))
        #plot(1:input$c, model()$mean_customers, main = "Mean customers evolution", ylab = "Mean Customers", xlab = "# Customers", type = "l", col = "blue")
    })
    
    output$sum <- renderPrint({
        print(model()$sum, digits = 2)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
