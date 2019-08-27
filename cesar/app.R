#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


cesar <- function(x, n, direccion = 1){
    LETRAS <- c(LETTERS[1:14], "Ñ", LETTERS[15:26])
    cifrada <- ""
    x <- toupper(gsub("\ |[.]", "", x))
    for (i in 1:nchar(x)){
        letra <- substring(x, i, i)
        indice <- which(LETRAS == letra) - 1
        if (length(indice) == 0){
            stop("La letra ", letra, " no existe en mi alfabeto")
        } else{
            nuevoindice <- (indice + n*direccion) %% length(LETRAS)
            cifrada <- paste0(cifrada, LETRAS[nuevoindice + 1])
        }
    }
    cifrada
}

ui <- fluidPage(
    
    # Application title
    titlePanel("ESTALMAT - Cifrado de mensajes"),
    
    sidebarLayout(
        sidebarPanel(
            textInput("mensaje",
                      "Mensaje:",
                      "MENSAJE"),
            sliderInput("clave",
                        "Clave:",
                        min = 1,
                        max = 27,
                        value = 3),
            radioButtons("direccion",
                         "Dirección",
                         choices = c("Cifrado", "Descifrado"))
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Cifrado / descifrado",
                         textOutput("encabezado", container = h3),
                         textOutput("cifrado"),
                         br(), br(),
                         imageOutput("cesar")),
                
                         # img(src = "./cesar.jpg", alt = "Julio César")),
                tabPanel("Código",
                         verbatimTextOutput("codigo"))
            )
        )
    )
)

server <- function(input, output) {
    output$encabezado <- renderText(paste("Mensaje", input$direccion, ":"))
    output$cifrado <- renderText({
        cesar(input$mensaje, input$clave,
              ifelse(input$direccion == "Cifrado", 1, -1))
    })
    output$cesar <- renderImage(list(src = "cesar.jpg",
                                     contentType = 'image/jpg',
                                     width = 300,
                                     # height = 300,
                                     alt = "Julio César"), 
                                deleteFile = FALSE)
    output$codigo <- renderPrint(cesar)
}

# Run the application 
shinyApp(ui = ui, server = server)
