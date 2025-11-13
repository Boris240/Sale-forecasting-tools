#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.





options(error = recover)



#### CALL DATABASE


Data_Path <-  "Confidential_path"

# Initialize a temporary in  database 
connection<- dbConnect(SQLite(), Data_Path)


# Copy a data.frame into it
# dbWriteTable(connection, "Database", Database, overwrite = TRUE)


# Connection

table <- tbl(connection, "Database") %>% collect()









server <- function(input, output, session) {
  



  
  
  
  
  
  ## Import function which allow us to make the job
  
  source('Confidential_path/Forecast_job.R', local = TRUE)
  source('Confidential_path/Visualization_job.R', local = TRUE) 

  
  
  
  
  

  
     
  # Create function
  
      get_country_coords <- function(country_name) {
          
          coords <- world %>%
            filter(admin == country_name) %>% 
            st_point_on_surface() %>%
            st_coordinates()
          
          if (nrow(coords) == 0) {
            return(NULL)
          }
          
          return(list(
            lat = coords[2],
            lng = coords[1]
            
          ))
          
      }
      
      


      

 
# Create a reactive expression  

      

      world <- ne_countries(scale='small', returnclass = 'sf') 
      
      Driver_set <- reactiveValues()
      
      Buttonlabel <- reactiveVal('Simulate')
      
      Location = reactive({
        
                 req(input$Area)
                 coords <- get_country_coords(input$Area)
                 if (is.null(coords)) {
                   showNotification("Country not found", type = "warning")
                 }
                 coords
                 })
      
      Var_label <- reactive({
         
                   req(input$Area, input$Category)
                   result<- Build_job_data(input$Area, input$Category, data = table)
                   tab <- result$data 
                   var_names <- intersect(names(Var_labels), colnames(tab))
                   list(var_names = var_names, data = tab)
                   
                   })
      
      
     

   
      Var_label2 <- reactive({
        
        req(input$Area1, input$Category1)
        result<- lapply(input$Area1, function(area){res<-Build_job_data(area, input$Category1, data = table)
        res$data})
        
        names(result) <- input$Area1
        colname_sets <- lapply(input$Area1, function(area){colnames(result[[area]])})
        var_names <- Reduce(intersect, colname_sets)
        var_names <- intersect(names(Var_labels), var_names)
        list(var_names = var_names, data = result)
        
      })
      
      
      Visualization <- reactive({
        
        req(input$Area, input$Category, input$Drivers)
        VizInd(country = input$Area,  segment =  input$Category, Indicators  = c('SALES',input$Drivers), range=1980:2024)
        
      })
      
      
      
      forecast <- reactive({
        
                  req(input$Area, input$Category, input$Drivers)
                  Forecast_output(input$Area, input$Category, selected_feature = input$Drivers, table)
      
                  })
      
      
      backcast <- reactive({
        
        req(input$Area, input$Category, input$Drivers)
        Make_backcast(input$Area, input$Category, selected_feature = input$Drivers, table)
        
      })
      
      
      forecast1 <- reactive({
        
        req(input$Area1, input$Category1, input$Drivers1)
        result <- lapply(input$Area1, function(area){Forecast_output(area, input$Category1, selected_feature = input$Drivers1, table)})
        names(result) <- input$Area1
        result
        
      })
      
      
      Future_traject <- reactive({
        
        req(forecast1())
        data <- lapply(names(forecast1()), function(area){out <- forecast1()[[area]]
                                                          out$Table})
        names(data) <- input$Area1
        datasets <- lapply(names(data), function(area){df <- data[[area]]
                                                       df$Area <- area
                                                       df
                                        }
        
        )

        tab <- bind_rows(datasets)
        tab%>%
          mutate(
            YEAR = as.numeric(format(ds, "%Y")),  # convertir la date en année
                                                  # renommer pour simplifier côté D3
          ) %>%
          select(YEAR, mean, Area)
        
      })
      
      
      
      
      forecast2 <- reactive({
        
        req(input$Area1, input$Category1)
        simulation <- lapply(names(reactiveValuesToList(Driver_set)), function(DriverSet_name){
          Forecast_output(input$Area1, input$Category1, selected_feature = Driver_set[[DriverSet_name]], table)})
        names(simulation) <- names(reactiveValuesToList(Driver_set))
        simulation
        
        })
        

      
      
      
      job_data <- reactive({
        
        req(input$Area, input$Category)
        Build_job_data(input$Area, input$Category, table)
        
                 })
      
      
      correlation <- reactive({
        
        req(job_data())
        dataset <- job_data()$data
        correlation_matrix <- cor(dataset[, setdiff(colnames(dataset),c(Identificator,'Default feature 1'))], use = "pairwise.complete.obs", method = "pearson")
        correlation_matrix
        
      })
      
      
      forecast_detail <- reactive({
        
        req(input$Area, input$Category, input$Drivers)
        Make_forecast(input$Area, input$Category, selected_feature = input$Drivers, table)
        
      })
      
      
      forecast_detail2 <- reactive({
        
        req(input$Area1, input$Category1)
        simulation <- lapply(names(reactiveValuesToList(Driver_set)), function(DriverSet_name){
          Make_forecast(input$Area1, input$Category1, selected_feature = Driver_set[[DriverSet_name]], table)})
        names(simulation) <- names(reactiveValuesToList(Driver_set))
        simulation
        
      })
      
      Validation_detail <-  reactive({
        
        req(forecast_detail)
        checkresiduals(forecast_detail()$Fit_result$out_gaussian)
        
      })
      
      
    
## Set Event      
    
      
      
      observeEvent(Location(),
                   
                   {
                   
                   leafletProxy("Map", session) %>%
                     clearMarkers() %>%  # delete previous marker
                     addMarkers(lng = Location()$lng, lat = Location()$lat, label = input$pays) %>%
                     flyTo(lng = Location()$lng, lat = Location()$lat, zoom = 5)
                     
                   }
                   
      )
      
      
      observeEvent(Location(),
                   
                   {
                     
                     leafletProxy("Map1", session) %>%
                       clearMarkers() %>%  # delete previous marker
                       addMarkers(lng = Location()$lng, lat = Location()$lat, label = input$pays) %>%
                       flyTo(lng = Location()$lng, lat = Location()$lat, zoom = 5)
                   }
                   
      )
      
      
      observeEvent(input$switch,  {
        updateTabsetPanel(inputId = "results")
      })
      
      
      
  
  
      observeEvent(Var_label(), {
        
        labels <- Var_labels[Var_label()$var_names]
        df_PCA <- Var_label()$data[, grep('dim', names(Var_label()$data), ignore.case = TRUE)]
        label_pcavar <- colnames(df_PCA)
        updatePickerInput(session, 'Drivers', choices =  c(Var_label()$var_names, label_pcavar, 'Default feature 1'), choicesOpt = list(
          subtext = c(as.vector(unlist(labels)), label_pcavar, 'Default feature 1')), selected = "Default feature")
        
      })
      
      
      observeEvent(input$switch1, {
        updateTabsetPanel(inputId = "results1") 
      })
      
      

      
      observeEvent(Var_label2(), {
        
        req(input$Options)
        codes_available <- Var_label2()$var_names
        labels <- Var_labels[codes_available]
        updatePickerInput(session, 'Drivers1', choices =  c(codes_available,  'Default feature 1'), choicesOpt = list(
              subtext = c(as.vector(unlist(labels)),  'Default feature 1')), selected = "Default feature")
            
        
      })
      

      
      
      observeEvent(input$Area1, {
        
        if(input$Options == 'By driver'){
        
        
          if(length(input$Area1) >1){
            
            shinyalert(
              title = "Warning!",
              text = "Please select only one area.",
              type = "warning"

            )
            
            updatePickerInput(session, "Area1", selected = character(0))
            
          }
        
        }
        
      })
      
 
           
      observeEvent(input$switch2, {
        req(input$Options)
        
        if(Buttonlabel() == "Reset" & input$Options == "By driver"){
          
          # switch to simulate
          Driver_set <<- reactiveValues()
          Buttonlabel("Simulate")
          updatePickerInput(session, inputId = 'Drivers1', selected = character(0))
          updateActionButton(session, "switch2", label = Buttonlabel())
          
          # Reset
          output$Bydriver <- renderUI(NULL)
          lapply(names(reactiveValuesToList(Driver_set)), function(n) {
            output[[paste0("Gforecast_", n)]] <- renderHighchart(NULL)
          })
          
          
        } else if(Buttonlabel() == "Simulate" & input$Options == "By driver") {
          
          # Simulate workflow
          
          next_index <- length(reactiveValuesToList(Driver_set)) + 1
          set_name <- paste0("set", next_index)
          
          
          if(length(input$Drivers1) > 0){
            Driver_set[[set_name]] <- input$Drivers1
          }
          
          ask_confirmation(
            
            inputId = "AnotherDriversSet",
            title = "New driver set?",
            text = "Would you like to create another driver set for this simulation?",
            type = "question",
            confirmButtonText = "Yes",
            cancelButtonText = "No",
            btn_labels = c("No", "Yes")
            
          )
          
        } else if(Buttonlabel()== "Simulate" & input$Options == "By country"){
        
           # 
           
           output$Bycountry  <- renderUI({
             
             box(title = 'Future trajectories', d3Output('trajectory'))
             
           })
      }
        
      })
      
      

      
      observeEvent(input$AnotherDriversSet, {
        
        req(!is.null(input$AnotherDriversSet))
        
        if (isTRUE(input$AnotherDriversSet)){
          
          updatePickerInput(session, inputId = 'Drivers1', selected = character(0))
          
        }
        else{
          
          
          
          Buttonlabel('Reset')
          updateActionButton(session, inputId = 'switch2', label = Buttonlabel())
          
          output$Bydriver <- renderUI({
            

            
            
            Driver_sets <- names(reactiveValuesToList(Driver_set))
            
            if (length(Driver_sets) == 0) {
              return(NULL)
            }
            
            box_list <- lapply(Driver_sets, function(DriverSet_name){ 
              
              box(title = 'Drivers :', highchartOutput(paste0("Gforecast_", DriverSet_name)), tableOutput(paste0("Summary", DriverSet_name)))
              
            })
            
            if(length(box_list) == 0) {return(NULL)}
            
            
              
            do.call(tagList, box_list)
    
            
          }) 
          
          output$Bydriver_residual <- renderUI({
            
 
            
            Driver_sets <- names(reactiveValuesToList(Driver_set))
            
            if (length(Driver_sets) == 0) {
              return(NULL)
            }
            
            box_list <- lapply(Driver_sets, function(DriverSet_name){ 
              
              box(title = 'Drivers :', plotOutput(paste0("Residual", DriverSet_name)))
              
            })
            
            if(length(box_list) == 0) {return(NULL)}
            
            do.call(tagList, box_list)
            
          })
          
        }
        
      })
      
            
      
#### Set output 
  
     
  
      output$Map <-  renderLeaflet({
        
        req(Location())
        leaflet(world, options = leafletOptions(
          
          minZoom = 2.5 , 
          worldCopyJump = FALSE,
          maxBounds = list(
            c(-90, -180), # coin sud-ouest
            c(180, 180)    # coin nord-est
            
          ))) %>% 
          
          addTiles() %>%
          
          setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90) %>%
          
          # Centrer sur le monde
          setView(lng = 0, lat = 20, zoom = 2) %>%
          
          addPolygons(
            fillColor = "lightblue",   # couleur intérieure des pays
            color = "white",           # couleur des frontières
            weight = 1,                # épaisseur des lignes
            highlightOptions = highlightOptions(
              color = "black", weight = 2,
              bringToFront = TRUE
            ),
            label = ~name  # affiche le nom du pays quand on survole
          )   %>% 
          
          addMarkers(lng = Location()$lng  , lat = Location()$lat, group = "Markers") 
        
        
      })
      
      
      output$Map1 <-  renderLeaflet({
        
        req(Location())
        leaflet(world, options = leafletOptions(
          
          minZoom = 2.5 , 
          worldCopyJump = FALSE,
          maxBounds = list(
            c(-90, -180), # coin sud-ouest
            c(180, 180)    # coin nord-est
            
          ))) %>% 
          
          addTiles() %>%
          
          setMaxBounds(lng1 = -180, lat1 = -90, lng2 = 180, lat2 = 90) %>%
          
          # Centrer sur le monde
          setView(lng = 0, lat = 20, zoom = 2) %>%
          
          addPolygons(
            fillColor = "lightblue",   # couleur intérieure des pays
            color = "white",           # couleur des frontières
            weight = 1,                # épaisseur des lignes
            highlightOptions = highlightOptions(
              color = "black", weight = 2,
              bringToFront = TRUE
            ),
            label = ~name  # affiche le nom du pays quand on survole
          )   %>% 
          
          addMarkers(lng = Location()$lng  , lat = Location()$lat, group = "Markers")
        
        
      })
      
  
  
      output$Logo <- renderImage({
        
                   filename <- normalizePath(file.path('C:/Users/a535948/Desktop/SPP forecast project directory/Shiny App/LTFV_Volvo_group/Volvo_logo.svg')
                               )
        
                   # Return a list containing the filename
                   list(src = filename)
        
     
                                 }, deleteFile = FALSE 
        
                    )
      
      
      
      output$Gforecast <- renderHighchart({
                
                          req(forecast())
                          forecast()$Graph
                          
      })
      

    
      output$Gbackcast <- renderHighchart({
        
                          req(backcast())
                          backcast()$Graph
                          
        
      })
      
      
      output$Metrics <- renderTable({
        
        req(backcast())
        backcast()$Score
        
      })
      
    
      
      output$Tforecast <- renderDT({
        
                                  req(forecast())
                                  tab <- forecast()$Table1
                                  colnames(tab) <- c('Year', 'mean', 'lower', 'upper')
                                  tab$Year <- substr(tab$Year, 1, 4)
                                  tab
                          
                          })
      
      
      output$PCAres <- renderPlot({
      
                                  req(job_data())
                                  job_data()$PCA.summary$plot$macro$var
                                  
                        })
      
        
      output$Contrib <- renderPlot({
        
                                  req(job_data())
                                  job_data()$PCA.summary$plot$macro$contrib
                                  
                        })
      
      
      output$residuals <- renderPlot({
        
                         req(Validation_detail())
                         Validation_detail()
                         
                        })
      
      
      
      
      output$summaryBox <- renderUI({
        req(input$Area, input$Category)
        
        
        div(
          style = "font-size:16px; line-height:1.6; color:#2c3e50;",
          tags$b("Country : "), span(input$Area, style="color:#0073b7;"), br(),
          tags$b("Truck category : "), span(input$Category, style="color:#0073b7;"), br(),
          tags$b("Drivers list : ")
        )
        
        
      })

            
 
      observe({
        Driver_sets <- names(reactiveValuesToList(Driver_set))
        
        # Stop if empty
        req(length(Driver_sets) > 0)
        
        lapply(Driver_sets, function(set_name) {
          id <- paste0("Gforecast_", set_name)
          id1 <- paste0("Residual", set_name)
          id2 <- paste0("Summary", set_name)
          
          output[[id]] <- renderHighchart({
            forecast2()[[set_name]]$Graph
            
          })
          
          output[[id1]] <- renderPlot({
            
            checkresiduals(forecast_detail2()[[set_name]]$Fit_result$out_gaussian)
            
          })
          
          output[[id2]] <- renderTable({
            
            out <- forecast_detail2()[[set_name]]$Fit_result$out_gaussian
            fit <- forecast_detail2()[[set_name]]$Fit_result$fit
            k <- fit$model$dims$nPar
            
            criteria <- data.frame(
            
              LogLik = as.numeric(out$logLik)
 
            )
            
            criteria
            
          })
          
        })
        
      })
      
      
      output$Curves <- renderHighchart({
      
      req(Visualization())
      Visualization()  
      
      }) 
      
      
      output$Correlation <- renderHighchart({
        
        req(correlation())
        hchart(correlation())
        
      })
 

      output$Estimate <- renderPrint({
        
        req(forecast_detail())
        print(forecast_detail()$Fit_result$out_gaussian)
      
      })
      
      
      output$Test <- renderPrint({
          
          req(Validation_detail())
          print(Validation_detail())
          
      })
      
      
      output$trajectory <- renderD3({
        
        r2d3(
          data = Future_traject(),
          script = "D3Visualization.js"

        )
        
      })
      
      
      observe({
        print(Future_traject())
      })
       
      
      
      output$drivers_list <- renderUI({
        req(input$Drivers)
        
        # Extraire les labels correspondant aux drivers sélectionnés
        labels <- Var_labels[input$Drivers]
        
        # Créer la liste à puces
        tagList(
          HTML("<ul>"),
          lapply(seq_along(labels), function(i) {
            driver_name <- input$Drivers[i]
            driver_label <- labels[[i]]
            HTML(paste0("<li><b>", driver_name, ":</b> ", driver_label, "</li>"))
          }),
          HTML("</ul>")
        )
      })
      
      output$download <- downloadHandler(
        
        filename = function(){
          
          paste(input$Area, '&', input$Category, 'LTVF.csv', sep=',')
        },
        content = function(file){
          req(forecast())
          data <- forecast()$Table1
          write.csv(data, file)
          
        })
      
      }






