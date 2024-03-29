#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

## app.R ##
library(shiny)
library(shinydashboard)
library(plotly)
source("CurvaAprendizaje.R")
source("Corelap.R")
library(readxl)
ui <- dashboardPage(
    dashboardHeader(title = "Herramienta Funpro"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Curva de aprendizaje", tabName = "curva", icon = icon("chart-line")),
            menuItem("Distribución de planta", tabName = "distr", icon = icon("crop-alt"))
        )
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "curva",
                    sidebarLayout(
                        sidebarPanel(
                            fileInput( "inputCurva","Cargar archivo .xlsx con los datos de los operarios",accept=c(".xlsx")),
                            uiOutput("mejorOperarioTittle"),
                            verbatimTextOutput("mejorOperario"),
                            uiOutput("tableTittle"),
                            tags$style(".tabla {display: flex;}"),
                            tags$style("#curvaTable {margin-left: auto; margin-right: auto;}"),
                            div(
                                tableOutput('curvaTable'),
                                class="tabla"
                            )
                        ),
                        mainPanel(
                            plotlyOutput(outputId="curvas_plot")
                        )
                    )
            ),
            
            # Second tab content
            tabItem(tabName = "distr",
                sidebarLayout(
                    sidebarPanel(
                        fileInput( "inputCorelap","Cargar archivo .xlsx con los datos de la matriz de cercania",accept=c(".xlsx")),
                        uiOutput("foTittle"),
                        verbatimTextOutput("fo"),
                        uiOutput("foDescription")
                    ),
                    mainPanel(
                        tags$style("#corelapTable {display: flex;}"),
                        tags$style(".table {margin: 0 auto 0 auto;}"),
                        uiOutput("styles"),
                        div(
                            uiOutput("tableCorelapDescription"),
                            uiOutput("br"),
                            tableOutput('corelapTable'),
                            class="card"
                        )
                    )
                )
            )
        )
    )
)

server <- function(input, output) {
    #Curva de Aprendizaje
    datosCurva <- eventReactive(input$inputCurva, {
        archivoCurva <- input$inputCurva
        if (is.null(archivoCurva)) { return(NULL) }
        #Se lee el archivo 
        dataFile <-read_excel(archivoCurva$datapath,sheet=1, 
                              col_names = TRUE)
        #se guarda unicamente la segunda columna que son las demandas
        coeficientes<-generarAjusteDatos(data.frame(dataFile))
    })
    output$curvas_plot <- renderPlotly({
        if (is.null(datosCurva())) { return(NULL) }
        resultados=datosCurva()
        operarios=names(datosCurva())
        params=resultados[[1]]["parmMat"][[1]]
        fig <- plot_ly(x = c(1:100), y = generarDatos(params[[1]],params[[2]]), name = operarios[[1]], type = 'scatter', mode = 'lines+markers', height = 800)%>%
            layout(title = "Curvas de aprendizaje de los distintos trabajadores",
                   xaxis = list(title = "No. de ciclo"),
                   yaxis = list (title = "Tiempo de ciclo"),
                   margin = 2)
        for(i in 2:length(resultados)){
            params=resultados[[i]]["parmMat"][[1]]
            fig <- fig %>% add_trace(y = generarDatos(params[[1]],params[[2]]), name = operarios[[i]], mode = 'lines+markers') 
        }
        fig
    })
    output$mejorOperario <- renderText({
        mejorOperario(datosCurva())
    })
    output$mejorOperarioTittle <- renderUI({
        if (is.null(datosCurva())) { return(NULL) }
        tags$b("El mejor operario es:")
    })
    output$curvaTable <- renderTable({
            infoOperarios(datosCurva())
        },
        align="c"
    )
    
    output$tableTittle <- renderUI({
        if (is.null(datosCurva())) { return(NULL) }
        tags$p("A continuación, se presenta una tabla con la tasa de aprendizaje, la tasa de mejora y los parámetros k y n de la ecuación y = kx^n para cada operario.")
    })
    
    #Corelap
    datosCorelap <- eventReactive(input$inputCorelap, {
        archivoCorelap <- input$inputCorelap
        if (is.null(archivoCorelap)) { return(NULL) }
        #Se lee el archivo 
        dataFileCorelap <-read_excel(archivoCorelap$datapath,sheet=1, 
                              col_names = TRUE)
        #se guarda unicamente la segunda columna que son las demandas
        corelapData<-corelap(data.frame(dataFileCorelap))
    })
    output$foTittle <- renderUI({
        if (is.null(datosCorelap())) { return(NULL) }
        tags$b("El valor de la función objetivo es:")
    })
    output$fo <- renderText({
        datosCorelap()[["fo"]]
    })
    output$foDescription <- renderUI({
        if (is.null(datosCorelap())) { return(NULL) }
        tags$p("Esta función objetivo se calculó por medio de distancias manhattan, y asumiendo una distancia unitaria entre departamentos que sean ubicados adyacentemente.")
    })
    
    output$corelapTable <- renderTable({
        if (is.null(datosCorelap())) { return(NULL) }
        as.data.frame(datosCorelap()[["loc_mat"]])
    },colnames = FALSE,align="c",bordered=TRUE)
    
    output$tableCorelapDescription <- renderUI({
        if (is.null(datosCorelap())) { return(NULL) }
            tags$h4("La mejor distribución encontrada es:")
    })
    output$styles <- renderUI({
        if (is.null(datosCorelap())) { return(NULL) }
        tags$style(".card {background-color: white; padding: 20px}")
    })
    output$br <- renderUI({
        if (is.null(datosCorelap())) { return(NULL) }
        tags$br()
    })
}

shinyApp(ui, server)
