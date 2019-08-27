
library(shiny)
library(dplyr)
library(numbers)

LETRAS <- c(LETTERS[1:14], "Ñ", LETTERS[15:26])

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    titlePanel("ESTALMAT - Criptografia de clave pública"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("p",
                        "Clave privada 'p':",
                        choices = c(3,5,7,11,13)),
            selectInput("q",
                        "Clave privada 'q':",
                        choices = c(3,5,7,11,13)),
            textInput("e",
                      "Clave pública 'e'"),
            textInput("d",
                      "Clave privada 'd'"),
            textInput("m",
                      "Mensaje a enviar: "),
            textInput("MM",
                      "Mensaje a desencriptar (espacios entre caracteres): "),
            tableOutput("letras")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            
            textOutput("N"),
            uiOutput("phi"),
            textOutput("e"),
            textOutput("cpublica"),
            textOutput("M"),
            textOutput("d"),
            textOutput("mm")
        )
    )
)

server <- function(input, output) {
    
    p <- reactive(as.numeric(input$p))
    q <- reactive(as.numeric(input$q))
    N <- reactive(p()*q())
    phi <- reactive((p()-1)*(q()-1))
    e <- reactive({
        if(input$e != ""){
            if(coprime(as.numeric(input$e), phi())){
                as.numeric(input$e)
            } else{
                stop("Error: phi(N) y e han de ser primos entre sí")
            }
        } else{
            "--"
        }
    })
    d <- reactive({
        dd <- as.numeric(input$d)
        if(input$d != ""){
            if((dd*e()) %% phi() == 1){
                dd
            }else{
                stop("Error: no se cumple d*e = 1 mod(phi)")
            }
        }else{
            "--"
        }
    })
    
    encriptar <- reactive({
        paste(sapply(unlist(strsplit(input$m, "")),
               function(x){
                   xx <- which(LETRAS == x)
                   (xx^e()) %% N()
               }),collapse = " ")
    })
    
    desencriptar <- reactive({
        paste(sapply(unlist(strsplit(input$MM, " ")),
                     function(x){
                         # xx <- which(LETRAS == x)
                         (as.numeric(x)^d()) %% N()
                     }),collapse = " ")
    })
    
    output$letras <- renderTable({
        
        data.frame(código = 1:27, row.names = LETRAS)
    },
    rownames = TRUE,
    colnames = FALSE)
    output$N <- renderText({
        paste("N = ", p()*q())
    })
    output$phi <- renderUI({
        withMathJax(helpText("\\(\\varphi(N)=(p-1)\\cdot(q-1)=", 
                             (p()-1), "\\cdot", (q()-1), "=",
                             (p()-1)*(q()-1),
                             "\\)"))
    })
    output$e <- renderText(paste("e =", e()))
    output$cpublica <- renderText(paste0("Clave pública: (", N(), ", ", e(), ")"))
    output$M <- renderText(paste("Mensaje codificado: ", encriptar()))
    output$d <- renderText(paste0("d = ", d()))
    output$mm <- renderText(paste("Mensaje descodificado: ", desencriptar()))
}

# Run the application 
shinyApp(ui = ui, server = server)
