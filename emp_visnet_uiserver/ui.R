# Define UI for application that draws a histogram
ui <- fluidPage (
    
    # Application title
    titlePanel("GASTech Employee Relationships"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "focus", 
                        label = "Focus on node:",
                        choices = employees_records_cleaned_rename$id),
            checkboxInput(inputId = "hover",
                          label = "Highlight when Hover?",
                          value = F),
            sliderInput(inputId = "deg",
                        label = "Highlight Degree",
                        min = 1,
                        max = 5,
                        value = c(1)),
            selectInput(inputId = "algorithm", 
                        label = "Highlight Algorithm:",
                        choices = c("hierarchical" = "hierarchical",
                                    "all" = "all")),
            sliderInput(inputId = "opahigh",
                        label = "Highlight Opacity",
                        min = 0,
                        max = 1,
                        value = c(0)),
            checkboxInput(inputId = "hidden_submain", label = "Hide Subtitle",value = TRUE),
            textInput(inputId = "input_submain",label = "Subtitle",placeholder = "Enter your subtitle here")
            
            #checkboxInput(inputId = "collapse",label = "Enable Collapse",value = TRUE),
            #checkboxInput(inputId = "reset_collapse", label = "Reset Highlight after Collapse",value = FALSE),
            #checkboxInput(inputId = "keep_coord", label = "Remember Node Coordinates",value = TRUE),
            #textInput(inputId = "labelSuffix",label = "Label Suffix on Cluster",placeholder = "Collapsed")
        ),
        
        # Show main panel
        mainPanel(
            visNetworkOutput("network"),
            br(),
            br(),
            h2("Email Exchanges for Selected Employee"),
            br(),
            textOutput("selected_node"),
            textOutput("relationships"),
            br(),
            br(),
            h2("Selected Employee Details"),
            br(),
            dataTableOutput(outputId = "filteredTable"),
            br(),
            br(),
            h2("List of Emails"),
            br(),
            DT::dataTableOutput(outputId = "email_analysis")
        )
    )
)
