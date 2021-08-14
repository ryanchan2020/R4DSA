# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    # visNetwork
    output$network <- renderVisNetwork({
        
        nodes <- employees_records_cleaned_rename
        edges <- data.frame(from=email_records_cleaned_relevant_net_rename_agg$from,
                            to=email_records_cleaned_relevant_net_rename_agg$to,
                            value=email_records_cleaned_relevant_net_rename_agg$Weight)
        
        visNetwork(nodes, edges, main = "Relevant Email Exchanges between Employees (<10 recipients)") %>% 
            visEdges(arrows="to") %>%
            visIgraphLayout(layout = "layout_with_fr") %>%
            visExport() %>% 
            visOptions(highlightNearest = list(enabled = TRUE,
                                               algorithm="hierarchical",
                                               hideColor="rgba(200,200,200,0)"),
                       nodesIdSelection = TRUE,
                       selectedBy = "group",
                       collapse=F,
                       autoResize = T) %>%
            visLayout(randomSeed = 123) %>%
            visInteraction(navigationButtons = TRUE, multiselect = TRUE) %>% 
            visLegend()
    })
    
    # focus
    observe({
        visNetworkProxy("network") %>%
            visFocus(id = input$focus, scale = 1)
    })
    
    # highlight options
    observe({
        col <- paste0("rgba(200,200,200,", input$opahigh, ")")
        visNetworkProxy("network") %>%
            visOptions(highlightNearest = list(enabled = TRUE,
                                               hover = input$hover,
                                               algorithm = input$algorithm,
                                               degree = input$deg,
                                               hideColor = col)) %>% 
            visRedraw()
        
    })
    
    # Groups
    observe({
        col <- paste0("rgba(200,200,200,", input$opahigh, ")")
        visNetworkProxy("network") %>%
            visOptions(selectedBy = list(variable = "group",
                                         hideColor = col))
    })
    
    # Set Subtitle
    observe({
        visNetworkProxy("network") %>%
            visSetTitle(submain = list(text = input$input_submain, hidden = input$hidden_submain))
    })
    
    # Selected node relationships
    output$selected_node <- renderText({ 
        paste("The selected employee (",input$network_selected,") has email exchanges with ")
    })
    
    
    output$relationships <- renderText({ 
        paste(input$network_highlight_label_id, ",")
    })
    
    
    # Set Selected node birth nation
    
    filtered_node <- reactive({employees_records_cleaned_rename_impt %>% 
            filter(id == input$network_selected)})
    
    DT::dataTableOutput("filteredTable")
    
    output$filteredTable <- DT::renderDataTable({
        
        datatable(
            filtered_node(),
            options = list(searching=F,paging=F),
            selection = list(mode = "multiple")
        )
        
    }) 
    
    
    # Datatable of email correspondences
    
    output$email_analysis <- DT::renderDataTable({
        datatable(data = email_records_analysis2,
                  filter = 'top',
                  options = )
    })
    
    # Set search value to selected node
    
    
    observe({
        dataTableProxy("email_analysis") %>% 
            updateSearch(keywords = list(global=input$network_selected))
    })
    
    
    
}