# import libraries
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(rgdal)) install.packages("rgdal", repos = "http://cran.us.r-project.org")
if(!require(sf)) install.packages("sf", repos = "http://cran.us.r-project.org")
if(!require(vroom)) install.packages("vroom", repos = "http://cran.us.r-project.org")
if(!require(htmltools)) install.packages("htmltools", repos = "http://cran.us.r-project.org")
if(!require(vegan)) install.packages("vegan", repos = "http://cran.us.r-project.org")
if(!require(viridis)) iinstall.packages("viridis", repos = "http://cran.us.r-project.org")

library(readxl)
library(readr)
library(tidyr)
library(ggplot2)
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
        id = "tabs",
        
        # navigation menu
        menuItem("Map", tabName = "map"),
        menuItem("Selected Data", tabName = "selection"),
        menuItem("Statistics", tabName = "statistics"),
        menuItem("Original Data", tabName = "dataset"),
        menuItem("About", tabName = "about"),
        
        # styled download button
        tags$style(".skin-blue .sidebar .shiny-download-link { color: #444; }"),
        fileInput("upload", NULL, accept = c(".csv"))
    )
)

body <- dashboardBody(
    tabItems(
        
        # first tab's content: unedited datatable
        tabItem(tabName = "dataset",
                fluidRow(
                    column(width = 12,
                           DT::dataTableOutput("fulldata"),
                           style = "height: 800px; overflow-y: scroll; overflow-x: scroll;")
                )),
        
        # second tab's content: bar charts + species discovery curve
        tabItem(
            tabName = "statistics",
            fluidRow(
                box(solidHeader = TRUE, width = 3, # count of observations, species, etc.
                    DT::dataTableOutput("basic")),
                box(solidHeader = TRUE, width = 5, # species discovery curve
                    plotOutput("sdc", height = 303)),
                box(solidHeader = TRUE, width = 4, # count of missing data
                    DT::dataTableOutput("missing")),
                

                box(solidHeader = TRUE, width = 12, # taxonomic breakdown by orders and families
                    plotOutput("taxo", height = 500)),
                box(solidHeader = TRUE, width = 12, # bar chart of recorders, no. of observations submitted, and year of submission
                    plotOutput("rec", height = 500))
            )
        ),
        
        # third tab's content: interactive map
        tabItem(
            tabName = "map",
            div(class = "outer",
                tags$style(type = "text/css", ".outer {position: fixed; top: 50px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                leafletOutput("occmap", height = "100%", width = "100%")),
            tags$head(includeCSS("styles.css")),
            absolutePanel(id = "controls", 
                          #bottom = 140, right = 55, width = 250, 
                          fixed = TRUE,
                          draggable = TRUE, height = "auto",
                          
                          # pickers
                          shinyWidgets::pickerInput(
                              "order_select", label = "Order", choices = "", multiple = TRUE, options = list(`actions-box` = TRUE)),
                          shinyWidgets::pickerInput(
                              "family_select", label = "Family", choices = "", multiple = TRUE, options = list(`actions-box` = TRUE)),
                          shinyWidgets::pickerInput(
                              "genus_select", label = "Genus", choices = "", multiple = TRUE, options = list(`actions-box` = TRUE)),
                          shinyWidgets::pickerInput(
                              "species_select", label = "Species", choices = "", multiple = TRUE, options = list(`actions-box` = TRUE))),
            uiOutput("maptitle")
        ),
        
        # fourth tab's content: datatable of selected points
        tabItem(tabName = "selection",
                downloadButton("downloaddata", "Download as CSV"),
                tags$br(), tags$br(),
                fluidRow(
                    column(width = 12,
                           DT::DTOutput("selectedpoints"),
                           style = "height: 800px; overflow-y: scroll; overflow-x: scroll;")
                )),
        
        # fifth tab's content: about page
        tabItem(tabName = "about",
                tags$h4("About"),
                tags$p("This app accepts your occurrence data and returns its table view, statistical summaries and an interactive map.
                The pickers in the sidebar lets you filter the occurrences displayed on the map, based on their scientific name, genus, family or order.
                You can select points on the map and view your selection in a table, as well as download it."),
                tags$p("Created for the Ecological Adaptations Lab at Yale-NUS College."),
                tags$br(), 
                tags$h4("How to Use"), 
                tags$p("To upload your occurrence data, your file must be in .csv and requires the following column headers (case sensitive) 
                in no particular sequence. NAs are allowed in the data. Additional columns in your data can be left alone; they'll
                be displayed as well."),
                tags$br(),
                tags$table(
                    tags$tr(tags$th("Column Name"), tags$th("Description")),
                    tags$tr(tags$td("id"), tags$td("Unique identifier of record")),
                    tags$tr(tags$td("genus"), tags$td("Genus of observation")),
                    tags$tr(tags$td("family"), tags$td("Family of observation")),
                    tags$tr(tags$td("order"), tags$td("Order of observation")),
                    tags$tr(tags$td("date"), tags$td("Date of collection or record")),
                    tags$tr(tags$td("recorder"), tags$td("Name(s) of the persons who recorded the observation")),
                    tags$tr(tags$td("longitude"), tags$td("Longitude of record")),
                    tags$tr(tags$td("latitude"), tags$td("Latitude of record"))
                )
        )
    )
)

ui <- dashboardPage(
    skin = "black",
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
                   date = col_date(format = "%m/%d/%Y"),
                   latitude = "?",
                   longitude = "?"
               )) %>%
                   dplyr::relocate(id, sciname, genus, family, order, date, recorder, longitude, latitude) %>%
                   mutate_if(is.character, function(col) iconv(col, to = "UTF-8")),
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
    
    # create and render basic descriptive values
    output$basic <- DT::renderDT({
        df <- data()
        
        # total no of obs, species, genus, fam, orders, recorders
        obs <- nrow(df)
        species_count <- length(unique(df$sciname))
        genus_count <- length(unique(df$genus))
        family_count <- length(unique(df$family))
        order_count <- length(unique(df$order))
        recorder_count <- length(unique(df$recorder))
        summarydf <- rbind(obs, species_count, genus_count, family_count, order_count, recorder_count)
        rownames(summarydf) <- c("Observations", "Species", "Genus", "Families", "Orders", "Recorders")
        colnames(summarydf) <- c("Count")
        summarydf
    },
    options = list(dom = "t"),
    caption = htmltools::tags$caption(style = "capstion-side: top; text-align: center; color: black; font-size: 15px;",
                                      htmltools::withTags(div(HTML("<p><b>Dataset Summary</b></p>"))),
                                      paste("Dates:", min(data()$date), "to", max(data()$date))))
    
    # create and render missing data table
    output$missing <- DT::renderDT({
        df <- data()
        na_count <- sapply(df, function(y) sum(is.na(y))) %>%
            data.frame() %>%
            dplyr::filter(`.` > 0) %>%
            dplyr::rename("No. of NAs" = ".")
    },
    caption = htmltools::tags$caption(style = "capstion-side: top; text-align: center; color: black; font-size: 15px; font-weight: bold;", "Missing Data"),
    options = list(dom = "tp", pageLength = 6, pagingType = "simple"))
    
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
    
    # create and render bar chart of breakdown by taxonomy
    output$taxo <- renderPlot({
        df <- data()
        taxo_breakdown <-
            df %>%
            select(sciname, order, family) %>%
            group_by(order, family) %>%
            summarise(n = n()) %>%
            as.data.frame()
        
        ggplot(taxo_breakdown, aes(x = reorder(order, -n), y = n, fill = reorder(family, -n))) +
            geom_col(position = position_dodge2(width = .9)) +
            labs(title = "Breakdown of Orders and Families") +
            xlab("Order") + 
            ylab("Occurrences") +
            geom_text(aes(label = family),
                      position = position_dodge2(width = .9),
                      vjust = 0.25, hjust = -0.2,
                      angle = 90) +
            theme(
                plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
                legend.position = "none",
                panel.background = element_rect(fill = "white", color = "white"),
                panel.grid.major.y = element_line(color = "gray")) +
            ylim(0, 32) 
    })
    
    # create and render bar chart of recorders
    output$rec <- renderPlot({
        df <- data()
        recorders <- df %>% select(recorder) %>% unique()

        # examine top recorders and their record count; top recorder = no. of obs from them > 1
        top_recorders <-
            table(df$recorder) %>%
            sort(decreasing = TRUE) %>%
            as.data.frame() %>%
            dplyr::rename("recorder" = 1, "num_records" = 2) %>%
            dplyr::filter(num_records > 1) 
        
        top_records <- top_recorders %>%
            merge(y = df, by = "recorder", all.x = TRUE) %>%
            dplyr::group_by(recorder)

        # Break down the years of each person's contributions
        top_records_by_year <-
            top_records %>%
            mutate(year = as.factor(format(as.Date(date, format = "%m/%d/%Y"), format = "%Y"))) %>%
            mutate(month = as.factor(format(as.Date(date, format = "%m/%d/%Y"), format = "%m"))) %>%
            count(year) %>%
            merge(y = recorders, by = "recorder", all.x = TRUE) %>%
            rename("num_records" = "n")

        levels(top_records_by_year$year) <-
            levels(top_records_by_year$year)[order(as.factor(levels(top_records_by_year$year)))]

        ggplot(data = top_records_by_year, aes(fill = year, x = num_records, y = reorder(recorder, num_records, sum))) +
            geom_bar(position = "stack", stat = "identity") +
            labs(title = "Top Recorders by Year",
                 subtitle = "Top Recorder: No. of Records Submitted > 1 in Total") +
            xlab("Number of Records Submitted") + ylab("Recorder") +
            viridis::scale_fill_viridis(discrete = TRUE, direction = -1) +
            xlim(0, 450) +
            theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5),
                  plot.subtitle = element_text(size = 11, hjust = 0.5),
                  panel.background = element_rect(fill = "white", color = "white"),
                  panel.grid.major.x = element_line(color = "gray"))
    })
    
    # create list of options for sidebar pickers
    ordernames <- reactive(unique(data()$order))
    familynames <- reactive(unique(data()$family))
    genusnames <- reactive(unique(data()$genus))
    speciesnames <- reactive(unique(data()$sciname))
    
    # display empty map when no data has been uploaded
    output$occmap <- renderLeaflet({
        leaflet() %>% 
            setView(lng = 103.8198, lat = 1.3521, zoom = 11) %>%
            addTiles(group = "Street") %>%
            addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
            addProviderTiles(providers$CartoDB.Voyager, group = "Voyager") %>%
            addLayersControl(baseGroups = c("Voyager", "Street", "Satellite"),
                             options = layersControlOptions(collapsed = FALSE))
    }) 
    
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
    displayed <- reactiveVal()
    # output$rendermap <- renderUI({s
    #     div(class = "outer",
    #         tags$style(type = "text/css", ".outer {position: fixed; top: 50px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
    #         leafletOutput("occmap", height = "100%", width = "100%")
        # fluidRow(
        #     box(width = NULL, solidHeader = TRUE, title = displayed(),
        #         leafletOutput("occmap", height = 520))
        # )
        # )
    # })
    
    output$maptitle <- renderUI({
        absolutePanel(id = "title", bottom = 20, fixed = TRUE, draggable = FALSE,
                      tags$h4(HTML(paste0(displayed(), collapse = ", "))))
    })
    
    # upon picking order input, create and render leaflet map
    observeEvent(input$order_select, {
        displayed(input$order_select)
        output$occmap <- renderLeaflet({
            # if (is.null(data())) {
            #     lf <-
            #         leaflet() %>% 
            #         setView(lng = 103.8198, lat = 1.3521, zoom = 11) %>%
            #         addTiles(group = "Street") %>%
            #         addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
            #         addProviderTiles(providers$CartoDB.Voyager, group = "Voyager") %>%
            #         addLayersControl(baseGroups = c("Voyager", "Street", "Satellite"),
            #                          options = layersControlOptions(collapsed = FALSE))
            #     return(lf)
            # } else {
                df <- popup() %>% dplyr::filter(order %in% input$order_select)
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
            #}
        })
    })
    
    # upon picking family input, create and render leaflet map
    observeEvent(input$family_select, {
        displayed(input$family_select)
        output$occmap <- renderLeaflet({
            df <- popup() %>% dplyr::filter(family %in% input$family_select)
            if (is.null(df)) {
                leaflet() %>%
                    setView(lng = 103.8198, lat = 1.3521, zoom = 11) %>%
                    addTiles(group = "Street") %>%
                    addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
                    addProviderTiles(providers$CartoDB.Voyager, group = "Voyager") %>%
                    addLayersControl(baseGroups = c("Voyager", "Street", "Satellite"),
                                     options = layersControlOptions(collapsed = FALSE))
            } else {
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
            }
            # if (is.null(data())) {
            #     lf <-
            #         leaflet() %>% 
            #         setView(lng = 103.8198, lat = 1.3521, zoom = 11) %>%
            #         addTiles(group = "Street") %>%
            #         addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
            #         addProviderTiles(providers$CartoDB.Voyager, group = "Voyager") %>%
            #         addLayersControl(baseGroups = c("Voyager", "Street", "Satellite"),
            #                          options = layersControlOptions(collapsed = FALSE))
            #     return(lf)
            # } else {
            # df <- popup() %>% dplyr::filter(family %in% input$family_select) ##### when deselected, df -> NULL####
            # leaflet(data = df) %>%
            #     setView(lng = 103.8198, lat = 1.3521, zoom = 11) %>%
            #     addTiles(group = "Street") %>%
            #     addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
            #     addProviderTiles(providers$CartoDB.Voyager, group = "Voyager") %>%
            #     addCircleMarkers(data = df,
            #                      ~longitude, ~latitude,
            #                      label = lapply(df$popup_text, htmltools::HTML),
            #                      radius = 4.2, stroke = TRUE, color = "white", weight = .5, opacity = 1,
            #                      fillOpacity = 1, fillColor = "black") %>%
            #     addLayersControl(baseGroups = c("Voyager", "Street", "Satellite"),
            #                      options = layersControlOptions(collapsed = FALSE))
            # }
        })
    }, ignoreNULL = FALSE)
    
    # upon picking genus input, create and render leaflet map
    observeEvent(input$genus_select, {
        displayed(input$genus_select)
        output$occmap <- renderLeaflet({
            # if (is.null(data())) {
            #     lf <-
            #         leaflet() %>% 
            #         setView(lng = 103.8198, lat = 1.3521, zoom = 11) %>%
            #         addTiles(group = "Street") %>%
            #         addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
            #         addProviderTiles(providers$CartoDB.Voyager, group = "Voyager") %>%
            #         addLayersControl(baseGroups = c("Voyager", "Street", "Satellite"),
            #                          options = layersControlOptions(collapsed = FALSE))
            #     return(lf)
            # } else {
                df <- popup() %>% dplyr::filter(genus %in% input$genus_select)
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
            # }
        })
    })
    
    # upon picking species input, create and render leaflet map
    observeEvent(input$species_select, {
        displayed(input$species_select)
        output$occmap <- renderLeaflet({
            # if (is.null(data())) {
            #     lf <-
            #         leaflet() %>% 
            #         setView(lng = 103.8198, lat = 1.3521, zoom = 11) %>%
            #         addTiles(group = "Street") %>%
            #         addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
            #         addProviderTiles(providers$CartoDB.Voyager, group = "Voyager") %>%
            #         addLayersControl(baseGroups = c("Voyager", "Street", "Satellite"),
            #                          options = layersControlOptions(collapsed = FALSE))
            #     return(lf)
            # } else {
                df <- popup() %>% dplyr::filter(sciname %in% input$species_select)
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
            # }
        })
    })
    
    # map's points change to show only selected orders
    observeEvent(input$order_select, {
        displayed(input$order_select)
            
        leafletProxy("occmap") %>% clearMarkers()
        df <- popup() %>% dplyr::filter(order %in% input$order_select)
        index <- which(df$order %in% input$order_select)
        
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
        df <- popup() %>% dplyr::filter(family %in% input$family_select)
        index <- which(df$family %in% input$family_select)
        
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
        df <- popup() %>% dplyr::filter(genus %in% input$genus_select)
        index <- which(df$genus %in% input$genus_select)
        
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
        df <- popup() %>% dplyr::filter(sciname %in% input$species_select)
        index <- which(df$sciname %in% input$species_select)
        
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
        if(click$group == "unselected") { ####### check this #############
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