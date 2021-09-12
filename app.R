# import libraries
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(rgdal)) install.packages("rgdal", repos = "http://cran.us.r-project.org")
if(!require(sf)) install.packages("sf", repos = "http://cran.us.r-project.org")
if(!require(vroom)) install.packages("vroom", repos = "http://cran.us.r-project.org")
if(!require(htmltools)) install.packages("htmltools", repos = "http://cran.us.r-project.org")
if(!require(vegan)) install.packages("vegan", repos = "http://cran.us.r-project.org")

library(readxl)
library(readr)
library(tidyr)
library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(sf)
library(vegan)


# UI ----------------------------------------------------------------------------------

header <- dashboardHeader(
    title = "Occurrence Mapper"
)

sidebar <- dashboardSidebar(
    sidebarMenu(
        
        # navigation menu
        menuItem("Data", tabName = "dataset"),
        menuItem("Statistics", tabName = "statistics"),
        menuItem("Map", tabName = "map"),
        menuItem("Selection", tabName = "selection"),
        
        # styled download button
        tags$style(".skin-blue .sidebar .shiny-download-link { color: #444; }"),
        fileInput("upload", NULL, accept = c(".csv")),
        
        # pickers
        shinyWidgets::pickerInput(
            "order_select", label = "Order", choices = ""),
        shinyWidgets::pickerInput(
            "family_select", label = "Family", choices = ""),
        shinyWidgets::pickerInput(
            "genus_select", label = "Genus", choices = ""),
        shinyWidgets::pickerInput(
            "species_select", label = "Species", choices = "")
    )
)

body <- dashboardBody(
    tabItems(
        
        # first tab's content: unedited datatable
        tabItem(tabName = "dataset",
                fluidRow(
                    box(width = 12, solidHeader = TRUE, status = "primary", title = "Dataset",
                        column(width = 12,
                               DT::dataTableOutput("fulldata"),
                               style = "height: 520px; overflow-y: scroll; overflow-x: scroll;")) 
                )),
        
        # second tab's content: species discovery curve
        tabItem(
            tabName = "statistics",
            fluidRow(
                box(title = "Species Discovery Curve", status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("sdc", height = 500))
            )
        ),
        
        # third tab's content: interactive map
        tabItem(
            tabName = "map",
            uiOutput("rendermap")
        ),
        
        # fourth tab's content: datatable of selected points
        tabItem(tabName = "selection",
                downloadButton("downloaddata", "Download Selection"),
                fluidRow(
                    box(width = 12, solidHeader = TRUE, status = "primary", title = "Selected Points",
                        column(width = 12,
                               DT::DTOutput("selectedpoints"),
                               style = "height: 520px; overflow-y: scroll; overflow-x: scroll;"))
                ))
    )
)

ui <- dashboardPage(
    header,
    sidebar,
    body
)


# SERVER ----------------------------------------------------------------------------------

server <- function(input, output, session) {
    
    # read uploaded csv file into dataset
    data <- reactive({
        req(input$upload)
        
        ext <- tools::file_ext(input$upload$name)
        switch(ext,
               csv = vroom::vroom(input$upload$datapath, delim = ",", col_types = list(
                   id = "c", # make sure to read 'id' values as characters instead of numbers
                   sciname = "c",
                   genus = "c",
                   family = "c",
                   order = "c",
                   recorder = "c",
                   date = "?",
                   latitude = "?",
                   longitude = "?"
               )),
               validate("Invalid file type. Please upload a .csv file.")
        ) 
    })
    
    # render data in the first tab's datatable 
    output$fulldata <- DT::renderDT({
        data()
    })
    
    # create another dataset that has a column for the map popup's html
    popup <- reactive({
        data() %>% dplyr::mutate(popup_text = paste("<i>", sciname, "</i><br/>",
                                                    date, "<br/>",
                                                    "by", recorder, "<br/>",
                                                    id))
    })
    
    # create and render species discovery curve
    output$sdc <- renderPlot({
        df <- data()
        com <- data.frame(df$sciname)
        com$obs <- row.names(df)
        colnames(com) <- c("sp", "obs")
        
        com2 <- com %>%
            pivot_longer(cols = -obs) %>%
            group_by(obs, value) %>%
            summarise(N=n()) %>%
            pivot_wider(names_from = value, values_from = N) %>%
            replace(is.na(.), 0)
        
        com2$obs <- as.numeric(com$obs)
        
        curve <- vegan::specaccum(com2)
        plot(curve, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue",
             xlab="Number of Observations",
             ylab="Number of Species",
             main="Species Discovery Curve")
    })
    
    # create list of options for sidebar pickers
    ordernames <- reactive(unique(data()$order))
    familynames <- reactive(unique(data()$family))
    genusnames <- reactive(unique(data()$genus))
    speciesnames <- reactive(unique(data()$sciname))
    
    # upon file upload, update pickers to reflect new options
    observeEvent(input$upload, {
        shinyWidgets::updatePickerInput(session, inputId = "order_select", label = "Order",
                                        choices = c(ordernames()),
                                        options = pickerOptions(liveSearch = TRUE))
        shinyWidgets::updatePickerInput(session, inputId = "family_select", label = "Family",
                                        choices = c(familynames()),
                                        options = pickerOptions(liveSearch = TRUE))
        shinyWidgets::updatePickerInput(session, inputId = "genus_select", label = "Genus",
                                        choices = c(genusnames()),
                                        options = pickerOptions(liveSearch = TRUE))
        shinyWidgets::updatePickerInput(session, inputId = "species_select", label = "Species",
                                        choices = c(speciesnames()),
                                        options = pickerOptions(liveSearch = TRUE))
    })
    
    # render interactive map with title depending on picker input
    displayed <- reactiveVal(" ")
    output$rendermap <- renderUI({
        fluidRow(
            box(width = NULL, solidHeader = TRUE, status = "primary", title = displayed(),
                leafletOutput("occmap", height = 520))
        )
    })
    
    # upon picking order input, create and render leaflet map
    observeEvent(input$order_select, {
        displayed(input$order_select)
        output$occmap <- renderLeaflet({
            if (is.null(data())) {
                lf <-
                    leaflet() %>% 
                    setView(lng = 103.8198, lat = 1.3521, zoom = 11) %>%
                    addTiles(group = "Street") %>%
                    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
                    addProviderTiles(providers$CartoDB.Voyager, group = "Voyager") %>%
                    addLayersControl(baseGroups = c("Voyager", "Street", "Satellite"),
                                     options = layersControlOptions(collapsed = FALSE))
                return(lf)
            } else {
                df <- popup() %>% dplyr::filter(order == input$order_select)
                lf <- 
                    leaflet(data = df) %>%
                    setView(lng = 103.8198, lat = 1.3521, zoom = 11) %>%
                    addTiles(group = "Street") %>%
                    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
                    addProviderTiles(providers$CartoDB.Voyager, group = "Voyager") %>%
                    addCircleMarkers(data = df,
                                     ~longitude, ~latitude,
                                     label = lapply(df$popup_text, htmltools::HTML),
                                     radius = 4.2, stroke = TRUE, color = "white", weight = .5, opacity = 1,
                                     fillOpacity = 1, fillColor = "black") %>%
                    addLayersControl(baseGroups = c("Voyager", "Street", "Satellite"),
                                     options = layersControlOptions(collapsed = FALSE))
                return(lf)
            }
        })
    })
    
    # upon picking family input, create and render leaflet map
    observeEvent(input$family_select, {
        displayed(input$family_select)
        output$occmap <- renderLeaflet({
            if (is.null(data())) {
                lf <-
                    leaflet() %>% 
                    setView(lng = 103.8198, lat = 1.3521, zoom = 11) %>%
                    addTiles(group = "Street") %>%
                    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
                    addProviderTiles(providers$CartoDB.Voyager, group = "Voyager") %>%
                    addLayersControl(baseGroups = c("Voyager", "Street", "Satellite"),
                                     options = layersControlOptions(collapsed = FALSE))
                return(lf)
            } else {
                df <- popup() %>% dplyr::filter(family == input$family_select)
                lf <- 
                    leaflet(data = df) %>%
                    setView(lng = 103.8198, lat = 1.3521, zoom = 11) %>%
                    addTiles(group = "Street") %>%
                    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
                    addProviderTiles(providers$CartoDB.Voyager, group = "Voyager") %>%
                    addCircleMarkers(data = df,
                                     ~longitude, ~latitude,
                                     label = lapply(df$popup_text, htmltools::HTML),
                                     radius = 4.2, stroke = TRUE, color = "white", weight = .5, opacity = 1,
                                     fillOpacity = 1, fillColor = "black") %>%
                    addLayersControl(baseGroups = c("Voyager", "Street", "Satellite"),
                                     options = layersControlOptions(collapsed = FALSE))
                return(lf)
            }
        })
    })
    
    # upon picking genus input, create and render leaflet map
    observeEvent(input$genus_select, {
        displayed(input$genus_select)
        output$occmap <- renderLeaflet({
            if (is.null(data())) {
                lf <-
                    leaflet() %>% 
                    setView(lng = 103.8198, lat = 1.3521, zoom = 11) %>%
                    addTiles(group = "Street") %>%
                    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
                    addProviderTiles(providers$CartoDB.Voyager, group = "Voyager") %>%
                    addLayersControl(baseGroups = c("Voyager", "Street", "Satellite"),
                                     options = layersControlOptions(collapsed = FALSE))
                return(lf)
            } else {
                df <- popup() %>% dplyr::filter(genus == input$genus_select)
                lf <- 
                    leaflet(data = df) %>%
                    setView(lng = 103.8198, lat = 1.3521, zoom = 11) %>%
                    addTiles(group = "Street") %>%
                    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
                    addProviderTiles(providers$CartoDB.Voyager, group = "Voyager") %>%
                    addCircleMarkers(data = df,
                                     ~longitude, ~latitude,
                                     label = lapply(df$popup_text, htmltools::HTML),
                                     radius = 4.2, stroke = TRUE, color = "white", weight = .5, opacity = 1,
                                     fillOpacity = 1, fillColor = "black") %>%
                    addLayersControl(baseGroups = c("Voyager", "Street", "Satellite"),
                                     options = layersControlOptions(collapsed = FALSE))
                return(lf)
            }
        })
    })
    
    # upon picking species input, create and render leaflet map
    observeEvent(input$species_select, {
        displayed(input$species_select)
        output$occmap <- renderLeaflet({
            if (is.null(data())) {
                lf <-
                    leaflet() %>% 
                    setView(lng = 103.8198, lat = 1.3521, zoom = 11) %>%
                    addTiles(group = "Street") %>%
                    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
                    addProviderTiles(providers$CartoDB.Voyager, group = "Voyager") %>%
                    addLayersControl(baseGroups = c("Voyager", "Street", "Satellite"),
                                     options = layersControlOptions(collapsed = FALSE))
                return(lf)
            } else {
                df <- popup() %>% dplyr::filter(sciname == input$species_select)
                lf <- 
                    leaflet(data = df) %>%
                    setView(lng = 103.8198, lat = 1.3521, zoom = 11) %>%
                    addTiles(group = "Street") %>%
                    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
                    addProviderTiles(providers$CartoDB.Voyager, group = "Voyager") %>%
                    addCircleMarkers(data = df,
                                     ~longitude, ~latitude,
                                     label = lapply(df$popup_text, htmltools::HTML),
                                     radius = 4.2, stroke = TRUE, color = "white", weight = .5, opacity = 1,
                                     fillOpacity = 1, fillColor = "black") %>%
                    addLayersControl(baseGroups = c("Voyager", "Street", "Satellite"),
                                     options = layersControlOptions(collapsed = FALSE))
                return(lf)
            }
        })
    })
    
    # map's points change to show only selected orders
    observeEvent(input$order_select, {
        displayed(input$order_select)
            
        leafletProxy("occmap") %>% clearMarkers()
        df <- popup() %>% dplyr::filter(order == input$order_select)
        index <- which(df$order == input$order_select)
        
        leafletProxy("occmap") %>%
            addCircleMarkers(data = df,
                             lng = df$longitude[index], lat = df$latitude[index],
                             label = lapply(df$popup_text[index], htmltools::HTML),
                             radius = 4.2, stroke = TRUE, color = "white", weight = .5, opacity = 1,
                             fillOpacity = 1, fillColor = "black",
                             layerId = df$id[index],
                             group = "unselected") %>%
            addCircleMarkers(data = df,
                             ~longitude, ~latitude,
                             label = lapply(df$popup_text, htmltools::HTML),
                             radius = 1.2, stroke = TRUE,
                             opacity = 1, fillOpacity = 1, color = "red",
                             group = ~id) %>%
            hideGroup(group = df$id) %>%
            addLayersControl(baseGroups = c("Voyager", "Street", "Satellite"),
                             options = layersControlOptions(collapsed = FALSE))
    })
    
    # map's points change to show only selected families
    observeEvent(input$family_select, {
        displayed(input$family_select)
            
        leafletProxy("occmap") %>% clearMarkers()
        df <- popup() %>% dplyr::filter(family == input$family_select)
        index <- which(df$family == input$family_select)
        
        leafletProxy("occmap") %>%
            addCircleMarkers(data = df,
                             lng = df$longitude[index], lat = df$latitude[index],
                             label = lapply(df$popup_text[index], htmltools::HTML),
                             radius = 4.2, stroke = TRUE, color = "white", weight = .5, opacity = 1,
                             fillOpacity = 1, fillColor = "black",
                             layerId = df$id[index],
                             group = "unselected") %>%
            addCircleMarkers(data = df,
                             ~longitude, ~latitude,
                             label = lapply(df$popup_text, htmltools::HTML),
                             radius = 1.2, stroke = TRUE,
                             opacity = 1, fillOpacity = 1, color = "red",
                             group = ~id) %>%
            hideGroup(group = df$id) %>%
            addLayersControl(baseGroups = c("Voyager", "Street", "Satellite"),
                             options = layersControlOptions(collapsed = FALSE))
    })
    
    # map's points change to show only selected genus
    observeEvent(input$genus_select, {
        displayed(input$genus_select)
            
        leafletProxy("occmap") %>% clearMarkers()
        df <- popup() %>% dplyr::filter(genus == input$genus_select)
        index <- which(df$genus == input$genus_select)
        
        leafletProxy("occmap") %>%
            addCircleMarkers(data = df,
                             lng = df$longitude[index], lat = df$latitude[index],
                             label = lapply(df$popup_text[index], htmltools::HTML),
                             radius = 4.2, stroke = TRUE, color = "white", weight = .5, opacity = 1,
                             fillOpacity = 1, fillColor = "black",
                             layerId = df$id[index],
                             group = "unselected") %>%
            addCircleMarkers(data = df,
                             ~longitude, ~latitude,
                             label = lapply(df$popup_text, htmltools::HTML),
                             radius = 1.2, stroke = TRUE,
                             opacity = 1, fillOpacity = 1, color = "red",
                             group = ~id) %>%
            hideGroup(group = df$id) %>%
            addLayersControl(baseGroups = c("Voyager", "Street", "Satellite"),
                             options = layersControlOptions(collapsed = FALSE))
    })
    
    # map's points change to show only selected species
    observeEvent(input$species_select, {
        displayed(input$species_select)
            
        leafletProxy("occmap") %>% clearMarkers()
        df <- popup() %>% dplyr::filter(sciname == input$species_select)
        index <- which(df$sciname == input$species_select)
        
        leafletProxy("occmap") %>%
            addCircleMarkers(data = df,
                             lng = df$longitude[index], lat = df$latitude[index],
                             label = lapply(df$popup_text[index], htmltools::HTML),
                             radius = 4.2, stroke = TRUE, color = "white", weight = .5, opacity = 1,
                             fillOpacity = 1, fillColor = "black",
                             layerId = df$id[index],
                             group = "unselected") %>%
            addCircleMarkers(data = df,
                             ~longitude, ~latitude,
                             label = lapply(df$popup_text, htmltools::HTML),
                             radius = 1.2, stroke = TRUE,
                             opacity = 1, fillOpacity = 1, color = "red",
                             group = ~id) %>%
            hideGroup(group = df$id) %>%
            addLayersControl(baseGroups = c("Voyager", "Street", "Satellite"),
                             options = layersControlOptions(collapsed = FALSE))
    })
    
    # change map's point color to red when selected (clicked) and back to black when deselected
    selected <- reactiveValues(groups = vector())
    proxy <- leafletProxy("occmap")
    
    observeEvent(input$occmap_marker_click, {
        click <- input$occmap_marker_click
        if(click$group == "unselected") {
            selected$groups <- c(selected$groups, click$id)
            proxy %>% showGroup(group = click$id)
        } else {
            selected$groups <- setdiff(selected$groups, click$group)
            proxy %>% hideGroup(group = click$group)
        }
    })
    
    # render datatable of selected points
    output$selectedpoints <- DT::renderDT({
        DT::datatable(
            if(is.null(selected)) {
                data(NULL)
            } else {
                data() %>% dplyr::filter(id %in% selected$groups)
            }
        )
    })
    
    # render data of selected points to be ready for download
    output$downloaddata <- downloadHandler(
        filename = "downloadeddata.csv",
        content = function(file) {
            df <- data() %>% dplyr::filter(id %in% selected$groups)
            write.csv(df, file, row.names = FALSE)
        }
    )
}

shinyApp(ui, server)