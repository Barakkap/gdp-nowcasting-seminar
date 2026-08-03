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
  tags$head(
    tags$style(HTML("
      /* Collapsible Details Styling */
      details summary {
        cursor: pointer;
        color: #0056b3;
        font-weight: 600;
        padding: 6px 8px;
        border-radius: 4px;
        transition: background-color 0.2s ease;
        list-style: none; /* Hide default browser marker */
      }
      details summary::-webkit-details-marker {
        display: none; /* Hide default webkit marker */
      }
      /* Custom animated dropdown arrow */
      details summary::before {
        content: '▶';
        display: inline-block;
        margin-right: 8px;
        font-size: 0.8em;
        transition: transform 0.2s ease;
      }
      details[open] summary::before {
        transform: rotate(90deg);
      }
      details summary:hover {
        background-color: #e2e6ea;
      }
      details[open] summary {
        background-color: #e2e6ea;
        margin-bottom: 8px;
      }
      /* Internal scrollable container for long documentation */
      .guide-container {
        max-height: 480px;
        overflow-y: auto;
        padding-right: 8px;
        margin-top: 8px;
      }
      .guide-table {
        font-size: 0.85em;
        margin-top: 10px;
      }
      .col-list {
        font-size: 0.88em;
        color: #444;
      }
    "))
  ),
  
  titlePanel("GDP Nowcasting & Bridge Model"),
  
  sidebarLayout(
    sidebarPanel(
      # --- COLLAPSIBLE DOCUMENTATION WELL ---
      div(
        class = "well",
        style = "background-color: #f5f5f5; padding: 12px; margin-bottom: 20px;",
        
        # Dropdown 1: Application Instructions
        tags$details(
          tags$summary("How to use this application"),
          div(
            style = "margin-top: 8px; padding-left: 5px;",
            tags$ol(
              tags$li("Upload your raw Excel file (.xlsx) conforming to the required 12-sheet structure."),
              tags$li("Click 'Check Best Factors & Lags' to inspect diagnostic recommendations."),
              tags$li("OPTIONAL: Adjust the DFM parameters (r = factors, p = lags). Click on \"About DFM Parameters\" to learn more."),
              tags$li("Click 'Run Pipeline' – track output in the Execution Logs tab."),
              tags$li("Download the generated Excel report or the raw predictions CSV upon completion.")
            ),
            p("The pipeline performs seasonal adjustment, data transformation, factor extraction, XGBoost modeling, and generates a nowcast report.", 
              style = "margin-top: 8px; font-size: 0.9em; color: #555;")
          )
        ),
        
        hr(style = "margin: 10px 0; border-top: 1px solid #ddd;"),
        
        # Dropdown 2: Model Parameter Guide & Diagnostics
        tags$details(
          tags$summary("About DFM Parameters"),
          div(
            style = "margin-top: 8px; padding-left: 5px;",
            p("Dynamic Factor Models (DFM) summarize information from a large panel of macroeconomic time series into a few unobserved common factors."),
            
            tags$ul(
              tags$li(
                tags$b("r (factors): "), 
                "Number of static/dynamic factors extracted from the monthly predictor panel.",
                tags$br(),
                tags$span("💡 4 factors is usually the correct amount to capture core macroeconomic trends without introducing excess noise.", style = "color: #555; font-size: 0.9em;")
              ),
              tags$li(
                tags$b("p (lags): "), 
                "Lag order of the vector autoregression (VAR) governing factor dynamics.",
                tags$br(),
                tags$span("💡 3 lags is usually best because the data is quarterly (monthly series mapped to quarterly GDP horizons).", style = "color: #555; font-size: 0.9em;")
              )
            ),
            
            hr(style = "margin: 10px 0; border-top: 1px dashed #ccc;"),
            
            tags$strong("How to Run Diagnostics:"),
            tags$ol(
              style = "margin-top: 4px;",
              tags$li("Click ", tags$b("'Check Best Factors & Lags'"), " in the sidebar."),
              tags$li("Click on the ", tags$b("'Diagnostics'"), " tab in the main panel and wait for the execution to finish.")
            ),
            
            # Warning Box
            div(
              style = "background-color: #fff3cd; color: #856404; padding: 10px; border-left: 4px solid #ffebaA; border-radius: 4px; margin-top: 10px; font-size: 0.88em;",
              tags$strong("⚠️ Important Warnings:"),
              tags$ul(
                style = "margin-bottom: 0; margin-top: 4px; padding-left: 18px;",
                tags$li(tags$b("Example Visuals: "), "The initial graphs displayed in the Diagnostics tab are illustrative examples, not your uploaded data's results."),
                tags$li(tags$b("Read Carefully: "), "Make sure to read the diagnostic output instructions carefully before selecting your final lags.")
              )
            )
          )
        ),
        
        hr(style = "margin: 10px 0; border-top: 1px solid #ddd;"),
        
        # Dropdown 3: Data Specification Guide
        tags$details(
          tags$summary("Data File Specification & Upload Guide"),
          
          div(
            class = "guide-container",
            
            p("To ensure the Nowcasting model runs smoothly without execution errors or pipeline failures, any uploaded Excel file (.xlsx) must strictly conform to the structure, sheet naming, and exact column headers outlined below."),
            
            h5(tags$b("General File Requirements")),
            tags$ul(
              tags$li(tags$b("File Format: "), "Standard Excel Workbook (.xlsx)."),
              tags$li(tags$b("Sheet Names: "), "Must contain all 12 required sheets using exact spelling (case-sensitive, no spaces)."),
              tags$li(tags$b("Date Column: "), "Every data sheet must have a ", tags$code("Date"), " column as its first column (YYYY-MM-DD)."),
              tags$li(tags$b("Data Types: "), "All indicator columns must contain pure numeric values (integers or decimals). Do NOT include currency symbols ($, ₪), commas as thousands separators (1,234.56), or text notes (N/A, null, -). Leave unobserved periods blank.")
            ),
            
            hr(style = "margin: 12px 0; border-top: 1px solid #ccc;"),
            
            h5(tags$b("Required Sheets & Exact Column Breakdown")),
            p("The workbook must contain 12 specific sheets with exact column matching:"),
            
            tags$strong("1. Configuration Sheet"),
            tags$ul(
              tags$li(tags$code("dataupdate"), " — 2-column key-value table (Category group name, Publication lag specification).")
            ),
            
            tags$strong("2. Monthly Economic Indicator Sheets"),
            tags$ul(class = "col-list",
                    tags$li(tags$code("personal_labor_income_taxes"), ": Date, Total Gross Income Tax Division, Total refunds from the Income Tax Department, Total Income Tax Division Net, Deductions and the capital market, Deduction from salary, Independents advances, Self-employed tax differences, Independent Cancellations, self employed returns, Capital Gains Tax Refunds, VAT Financial Institutions (Salary), Non-profit institution tax"),
                    tags$li(tags$code("corporate_business_tax"), ": Date, Companies advances, tax differential companies, Cancellation companies, Income tax for self-employed individuals and companies (advances and deductions), Companies returns, excess expenses, Bonds and dividends, Cancellations Deductions, Goods and services"),
                    tags$li(tags$code("consumption_tax"), ": Date, Gross local VAT, VAT refund autonomy and traders, Total net VAT, Net local sales tax, Gross fuel tax"),
                    tags$li(tags$code("import_trade_tax"), ": Date, Gross import VAT, Net customs, Net import purchase tax, Total import taxes"),
                    tags$li(tags$code("real_estate"), ": Date, Real estate taxation, Property tax, praise tax, Real estate purchase tax, praise tax returns, purchase returns, Apartments sold at an annual rate, Apartment Price Index (1993=100) - Mid-period reviewed"),
                    tags$li(tags$code("real_activity"), ": Date, cons_trust, madad meshulav, madad_cc_purchases_sa, madad_pedio, madad_yetzur_industrial, Oil, madad_hadash"),
                    tags$li(tags$code("labor"), ": Date, real salary, salaried jobs, unemployment rate, participation rate, employment rate"),
                    tags$li(tags$code("capital_markets"), ": Date, TA35, TA125, Nasdaq, sp500"),
                    tags$li(tags$code("FX_liqudity"), ": Date, Reer, Dollar, Foreign exchange reserves (millions of dollars)")
            ),
            
            tags$strong("3. Target & Macro Adjusters Sheets"),
            tags$ul(class = "col-list",
                    tags$li(tags$code("target"), ": Date, GDP (Quarterly frequency)"),
                    tags$li(tags$code("adjusters"), ": Date, VAT_rate, CPI (Monthly frequency)")
            ),
            
            hr(style = "margin: 12px 0; border-top: 1px solid #ccc;"),
            
            h5(tags$b("Common Causes of Pipeline Failure")),
            tags$table(
              class = "table table-bordered table-striped guide-table",
              tags$thead(
                tags$tr(
                  tags$th("Potential Error"),
                  tags$th("Cause"),
                  tags$th("Prevention / Fix")
                )
              ),
              tags$tbody(
                tags$tr(
                  tags$td(tags$code("KeyError: 'FX_liqudity'")),
                  tags$td("Sheet name misspelled (e.g., FX_liquidity)."),
                  tags$td("Keep sheet names identical to sample file.")
                ),
                tags$tr(
                  tags$td(tags$code("ValueError: string to float")),
                  tags$td("Numbers formatted as text or with commas (1,500.20)."),
                  tags$td("Set cell format to Number; remove thousands separators.")
                ),
                tags$tr(
                  tags$td(tags$code("Date Parsing Error")),
                  tags$td("Dates stored as text in mixed formats."),
                  tags$td("Format Date column as Short Date (YYYY-MM-DD).")
                ),
                tags$tr(
                  tags$td(tags$code("Missing Columns Error")),
                  tags$td("Existing feature columns deleted or renamed."),
                  tags$td("Maintain identical headers across updates.")
                )
              )
            ),
            
            hr(style = "margin: 12px 0; border-top: 1px solid #ccc;"),
            
            h5(tags$b("Pre-Upload Checklist")),
            tags$ul(
              tags$li("Saved as an .xlsx workbook."),
              tags$li("Contains all 12 required sheet names."),
              tags$li("First column in every sheet is named Date."),
              tags$li("Monthly sheets have contiguous monthly dates."),
              tags$li("target sheet contains quarterly dates & numeric GDP."),
              tags$li("All numeric cells are clean (no currency symbols/text).")
            )
          )
        )
      ),
      
      fileInput("raw_data", "1. Upload Raw Data (Excel)", accept = c(".xlsx")),
      
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
