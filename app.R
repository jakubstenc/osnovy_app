library(shiny)
library(googlesheets4)
library(jsonlite)
library(dplyr)
library(DT)

# Authenticate with the service account key
gs4_auth(path = "arboreal-cosmos-445214-n7-f23afaa5aff7.json")

# Google Sheet ID (from the URL of your sheet)
sheet_id <- "https://docs.google.com/spreadsheets/d/1cgdVmb48Ge_mWS4ubUmjjGFzJJj_UN6dV0l4aRGQSv8/edit"

# Load external data
plant_species <- read.csv("plant_species.csv")$species
cell_labels <- read.csv("cell_labels.csv")$cells

ui <- fluidPage(
  titlePanel("Field Data Collection App"),
  sidebarLayout(
    sidebarPanel(
      textInput("plot_id", "Enter Plot ID", value = "Plot1"),
      selectInput("cell_id", "Select Cell", choices = cell_labels, selected = cell_labels[1]),
      selectInput("species", "Select Plant Species", choices = plant_species),
      numericInput("count", "Count of Flowering Plants", value = 0, min = 0),
      actionButton("add_entry", "Add Entry"),
      actionButton("save_data", "Save to Google Sheets")
    ),
    mainPanel(
      h4("Selected Plot ID:"),
      textOutput("selected_plot_id", container = tags$h5),
      tableOutput("summary_table"),
      h4("Edit or Delete Entries:"),
      DTOutput("editable_table"),
      textOutput("save_status")
    )
  )
)
server <- function(input, output, session) {
  # Reactive value to store responses with Timestamp (only for saving)
  responses <- reactiveVal(data.frame(
    PlotID = character(0),
    CellID = character(0),
    Species = character(0),
    Count = numeric(0),
    stringsAsFactors = FALSE  # Remove Timestamp from visible columns
  ))
  
  # Add new entry with timestamp (but do not display it in the app)
  observeEvent(input$add_entry, {
    new_entry <- data.frame(
      PlotID = input$plot_id,
      CellID = input$cell_id,
      Species = input$species,
      Count = input$count,
      stringsAsFactors = FALSE
    )
    responses(rbind(responses(), new_entry))  # Add new entry without timestamp
  })
  
  # Display selected Plot ID
  output$selected_plot_id <- renderText({
    input$plot_id
  })
  
  # Generate summary table (excluding Timestamp column from display)
  output$summary_table <- renderTable({
    plot_data <- responses() %>% filter(PlotID == input$plot_id)
    
    cell_status <- data.frame(
      CellID = cell_labels,
      Status = ifelse(cell_labels %in% plot_data$CellID, "Recorded", "Not Recorded"),
      `Species Recorded` = sapply(cell_labels, function(cell) {
        species_list <- plot_data %>% filter(CellID == cell) %>% pull(Species)
        if (length(species_list) > 0) paste(species_list, collapse = ", ") else NA
      }),
      `Count of Flowering Plants` = sapply(cell_labels, function(cell) {
        total_count <- plot_data %>% filter(CellID == cell) %>% summarise(Total = sum(Count)) %>% pull(Total)
        if (length(total_count) > 0) total_count else 0
      }),
      stringsAsFactors = FALSE
    )
    
    cell_status %>%
      mutate(
        Status = ifelse(Status == "Recorded", 
                        paste0("<span style='color: green;'>", Status, "</span>"),
                        paste0("<span style='color: red;'>", Status, "</span>"))
      )
  }, sanitize.text.function = function(x) x)
  
  # Render the editable table (no Timestamp column)
  output$editable_table <- renderDT({
    datatable(responses(),
              editable = "cell", 
              extensions = "Buttons",
              options = list(dom = 'Bfrtip', buttons = c('copy', 'csv')))
  })
  
  # Handle cell edits in editable table
  observeEvent(input$editable_table_cell_edit, {
    info <- input$editable_table_cell_edit
    modified_data <- responses()  # Get current data
    modified_data[info$row, info$col] <- info$value  # Update edited cell
    responses(modified_data)  # Update reactive value
  })
  
  # Save data to Google Sheets (with timestamp)
  observeEvent(input$save_data, {
    saveData(responses())  # Save the updated data with timestamp
  })
  
  # Save data function to append to Google Sheets, with Timestamp
  saveData <- function(updated_data) {
    tryCatch({
      # Add timestamp column when saving data
      updated_data_with_timestamp <- updated_data
      updated_data_with_timestamp$Timestamp <- Sys.time()  # Add the current timestamp
      
      # Append new data to the Google Sheets
      sheet_append(updated_data_with_timestamp, ss = sheet_id)
      output$save_status <- renderText("Data successfully saved to Google Sheets!")
    }, error = function(e) {
      output$save_status <- renderText(paste("Error saving data:", e$message))
    })
  }
}

shinyApp(ui, server)
