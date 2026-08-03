library(shiny)
library(readr)
library(readxl)
library(lubridate)
library(dplyr)
source("helpers.R")

# ==============================================================================
# GLOBAL SETUP
# ==============================================================================
# Load the static background data required for X13 seasonal adjustments once
td <- read_csv("data/raw/td_var.csv", show_col_types = FALSE)
td_ts <- ts(td[, -1], start = c(year(min(td$date)), month(min(td$date))), frequency = 12)

preadj <- read_excel("data/raw/hol_preadj.xlsx") %>% mutate(date = as.Date(date))
hag_ts <- ts(preadj[, -1], start = c(year(min(preadj$date)), month(min(preadj$date))), frequency = 12)

# ==============================================================================
# UI
# ==============================================================================
ui <- fluidPage(
  titlePanel("GDP Nowcasting & Bridge Model"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("raw_data", "1. Upload Raw Data (Excel)",
                accept = c(".xlsx")),
      
      actionButton("diag_btn", "Check Best Factors & Lags", class = "btn-warning", style = "width: 100%; margin-bottom: 20px;"),
      
      numericInput("dfm_r", "DFM 'r' parameter (factors):", value = 4, min = 1, step = 1),
      numericInput("dfm_p", "DFM 'p' parameter (lags):", value = 3, min = 1, step = 1),
      
      actionButton("run_btn", "2. Run Pipeline", class = "btn-primary", style = "width: 100%; margin-bottom: 20px;"),
      
      uiOutput("download_ui"),
      uiOutput("download_csv_ui")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Execution Logs",
                 br(),
                 verbatimTextOutput("logs_output", placeholder = TRUE)
        ),
        tabPanel("Diagnostics",
                 br(),
                 uiOutput("diag_ui")
        )
      )
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {
  
  # Reactive values to hold logs and workbook
  rv <- reactiveValues(
    logs = character(0),
    report_wb = NULL,
    out_df = NULL,
    diag_res = NULL
  )
  
  # Create a temporary file to hold logs for real-time streaming
  log_file <- tempfile("shiny_logs_", fileext = ".txt")
  file.create(log_file)
  
  # Helper to append logs to both reactive value and the physical file
  append_log <- function(msg) {
    formatted_msg <- paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", msg)
    rv$logs <- c(rv$logs, formatted_msg)
    cat(paste0(formatted_msg, "\n"), file = log_file, append = TRUE)
  }
  
  # Read the log file every 500ms to update the UI while the main thread is blocked
  log_data <- reactiveFileReader(500, session, log_file, readLines)
  
  output$logs_output <- renderText({
    paste(log_data(), collapse = "\n")
  })
  
  # Observe Diagnostics Button
  observeEvent(input$diag_btn, {
    req(input$raw_data)
    
    rv$logs <- character(0)
    file.create(log_file)
    
    tryCatch({
      withProgress(message = 'Running Diagnostics...', value = 0, {
        
        append_log(">>> STEP 1: Fast data reading for diagnostics")
        # We can use the first step of run_transformations to just get the panel
        trans_res <- run_transformations(
          raw_data_path = input$raw_data$datapath,
          td_ts = td_ts,
          hag_ts = hag_ts,
          update_log = append_log,
          update_progress = function(val, msg) { incProgress(val * 0.2, detail = msg) }
        )
        
        append_log(">>> STEP 2: Running ICr and Grid Search")
        diag_res <- run_diagnostics(
          combined_panel = trans_res$combined_panel,
          update_log = append_log,
          update_progress = function(val, msg) { setProgress(value = 0.2 + (val * 0.8), detail = msg) }
        )
        
        rv$diag_res <- diag_res
        
        # Update numeric inputs with suggested values
        updateNumericInput(session, "dfm_r", value = diag_res$suggested_r)
        updateNumericInput(session, "dfm_p", value = diag_res$suggested_p)
        
        append_log(">>> DIAGNOSTICS COMPLETED SUCCESSFULLY")
        setProgress(1, detail = "Done!")
      })
      
    }, error = function(e) {
      append_log(paste("ERROR:", e$message))
    })
  })
  
  # Render Diagnostics UI
  output$diag_ui <- renderUI({
    if (is.null(rv$diag_res)) {
      return(h5("Please upload data and click 'Check Best Factors & Lags' to view diagnostics."))
    }
    
    fluidRow(
      column(12,
             h4("Diagnostics Recommendation"),
             htmlOutput("diag_text"),
             hr(),
             h4("Factor Selection (ICr Criteria)"),
             plotOutput("plot_icr"),
             hr(),
             h4("Lag Selection Grid Search (AIC / BIC)"),
             plotOutput("plot_lags")
      )
    )
  })
  
  output$diag_text <- renderText({
    req(rv$diag_res)
    res <- rv$diag_res
    paste0(
      "<b>Suggested Factors (r):</b> ", res$suggested_r, "<br>",
      "<b>Suggested Lags (p):</b> ", res$suggested_p, "<br><br>",
      "<i>How to choose:</i><br>",
      "<b>Factors (r):</b> The first plot shows information criteria (IC1, IC2, IC3) for different numbers of factors. You are looking for a 'knee' or elbow shape where the metric drops sharply and then levels off. The suggested value is mathematically derived from these IC criteria minimums.<br>",
      "<b>Lags (p):</b> The second plot shows the BIC (Bayesian Information Criterion) and AIC for fitting the model with the chosen number of factors across different lag lengths (1 to 6). We look for an 'elbow' point in the BIC curve—the point furthest from a straight line connecting the first and last points—as this represents the best trade-off between model fit and complexity."
    )
  })
  
  output$plot_icr <- renderPlot({
    req(rv$diag_res)
    plot(rv$diag_res$ic)
  })
  
  output$plot_lags <- renderPlot({
    req(rv$diag_res)
    res_df <- rv$diag_res$results_df
    
    ggplot(res_df, aes(x = p)) +
      geom_line(aes(y = BIC, color = "BIC"), size = 1) +
      geom_point(aes(y = BIC, color = "BIC"), size = 3) +
      geom_line(aes(y = AIC, color = "AIC"), size = 1, linetype = "dashed") +
      geom_point(aes(y = AIC, color = "AIC"), size = 3) +
      geom_vline(xintercept = rv$diag_res$suggested_p, color = "red", linetype = "dotted", size = 1.5) +
      annotate("text", x = rv$diag_res$suggested_p, y = min(res_df$BIC, na.rm=T), 
               label = paste("Suggested Elbow:", rv$diag_res$suggested_p), color = "red", vjust = -1, hjust = -0.1) +
      scale_color_manual(values = c("BIC" = "blue", "AIC" = "darkgray")) +
      labs(title = "Grid Search for Lags (p)", x = "Number of Lags (p)", y = "Information Criterion Value", color = "Metric") +
      theme_minimal()
  })
  
  # Observe Run Button
  observeEvent(input$run_btn, {
    req(input$raw_data)
    
    # Reset states and log file
    rv$logs <- character(0)
    rv$report_wb <- NULL
    rv$out_df <- NULL
    file.create(log_file) # Clear the file
    
    # Wrap entire execution in a tryCatch to log errors gracefully
    tryCatch({
      
      withProgress(message = 'Processing...', value = 0, {
        
        # 1. Transformations
        append_log(">>> STEP 1: Running Transformations")
        trans_res <- run_transformations(
          raw_data_path = input$raw_data$datapath,
          td_ts = td_ts,
          hag_ts = hag_ts,
          update_log = append_log,
          update_progress = function(val, msg) { incProgress(val * 0.33, detail = msg) }
        )
        
        # 2. DFM & XGBoost
        append_log(">>> STEP 2: Running DFM & XGBoost")
        dfm_xgb_res <- run_dfm_xgboost(
          combined_panel = trans_res$combined_panel,
          target_raw = trans_res$target_raw,
          r_val = req(input$dfm_r),
          p_val = req(input$dfm_p),
          update_log = append_log,
          update_progress = function(val, msg) { setProgress(value = 0.33 + (val * 0.33), detail = msg) }
        )
        
        # 3. Report Generation
        append_log(">>> STEP 3: Generating Final Report")
        wb <- generate_report(
          models_res = dfm_xgb_res,
          blocks_shifted = trans_res$blocks_shifted,
          r_val = req(input$dfm_r),
          p_val = req(input$dfm_p),
          update_log = append_log,
          update_progress = function(val, msg) { setProgress(value = 0.66 + (val * 0.33), detail = msg) }
        )
        
        rv$report_wb <- wb
        rv$out_df <- dfm_xgb_res$out_df
        append_log(">>> PIPELINE COMPLETED SUCCESSFULLY")
        setProgress(1, detail = "Done!")
      })
      
    }, error = function(e) {
      append_log(paste("ERROR:", e$message))
    })
  })
  
  # Conditionally show the download button only when report_wb is available
  output$download_ui <- renderUI({
    if (!is.null(rv$report_wb)) {
      downloadButton("download_report", "3. Download Excel Report", class = "btn-success", style = "width: 100%; margin-bottom: 10px;")
    }
  })
  
  output$download_csv_ui <- renderUI({
    if (!is.null(rv$out_df)) {
      downloadButton("download_csv", "4. Download Raw Predictions (CSV)", class = "btn-info", style = "width: 100%;")
    }
  })
  
  # Download Handler for the generated Excel workbook
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("Nowcast_Executive_Report_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      req(rv$report_wb)
      openxlsx::saveWorkbook(rv$report_wb, file, overwrite = TRUE)
    }
  )
  
  # Download Handler for the raw predictions CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      "nowcast_results.csv"
    },
    content = function(file) {
      req(rv$out_df)
      write.csv(rv$out_df, file, row.names = FALSE, na = "")
    }
  )
}

shinyApp(ui = ui, server = server)
