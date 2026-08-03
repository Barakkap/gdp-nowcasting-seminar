library(shiny)
library(readr)
library(readxl)
library(lubridate)
library(dplyr)
source("helpers.R")

# ==============================================================================
# GLOBAL SETUP
# ==============================================================================
# Load static background data required for X13 seasonal adjustments once
td <- read_csv("data/raw/td_var.csv", show_col_types = FALSE)
td_ts <- ts(td[, -1], start = c(year(min(td$date)), month(min(td$date))), frequency = 12)

preadj <- read_excel("data/raw/hol_preadj.xlsx") %>% mutate(date = as.Date(date))
hag_ts <- ts(preadj[, -1], start = c(year(min(preadj$date)), month(min(preadj$date))), frequency = 12)

# ==============================================================================
# UI WRAPPER
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
      .rtl details summary::before {
        content: '◀';
        margin-right: 0px;
        margin-left: 8px;
      }
      details[open] summary::before {
        transform: rotate(90deg);
      }
      .rtl details[open] summary::before {
        transform: rotate(-90deg);
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
      /* RTL Support styling */
      .rtl {
        direction: rtl;
        text-align: right;
      }
      .rtl .pull-right {
        float: left !important;
      }
    "))
  ),
  
  # Dynamic Layout Wrapper for Language Direction
  uiOutput("app_ui")
)

# ==============================================================================
# SERVER LOGIC
# ==============================================================================
server <- function(input, output, session) {
  
  # Listen for a new file upload
  observeEvent(input$raw_data, {
    # Wipe the old reports so the download buttons disappear
    rv$report_wb <- NULL
    rv$out_df <- NULL
    
    # Clear the old diagnostics graphs
    rv$diag_res <- NULL
    
    # Clear the log screen
    rv$logs <- character(0)
    file.create(log_file)
    append_log(">>> New file uploaded. Ready to run Diagnostics or Pipeline.")
  })
  
  # ----------------------------------------------------------------------------
  # 1. LANGUAGE STATE & DYNAMIC UI
  # ----------------------------------------------------------------------------
  lang <- reactiveVal("en")
  
  observeEvent(input$toggle_lang, {
    if (lang() == "en") {
      lang("he")
    } else {
      lang("en")
    }
  })
  
  output$app_ui <- renderUI({
    is_he <- lang() == "he"
    rtl_class <- if (is_he) "rtl" else ""
    
    div(
      class = rtl_class,
      
      # Header Bar with Language Switcher
      div(
        style = "display: flex; justify-content: space-between; align-items: center; margin-top: 15px; margin-bottom: 15px;",
        h2(if (is_he) "מודל עכשיוי ותחזית תוצר (GDP Nowcasting & Bridge Model)" else "GDP Nowcasting & Bridge Model", style = "margin: 0;"),
        actionButton("toggle_lang", if (is_he) "🌐 Switch to English" else "🌐 שנה לעברית", class = "btn-info btn-sm")
      ),
      
      sidebarLayout(
        sidebarPanel(
          # --- COLLAPSIBLE DOCUMENTATION WELL ---
          div(
            class = "well",
            style = "background-color: #f5f5f5; padding: 12px; margin-bottom: 20px;",
            
            # Dropdown 1: Application Instructions
            tags$details(
              tags$summary(if (is_he) "כיצד להשתמש באפליקציה" else "How to use this application"),
              div(
                style = "margin-top: 8px; padding-left: 5px; padding-right: 5px;",
                tags$ol(
                  tags$li(if (is_he) "העלה קובץ Excel (.xlsx) התואם למבנה 12 הגיליונות הנדרש." else "Upload your raw Excel file (.xlsx) conforming to the required 12-sheet structure."),
                  tags$li(if (is_he) "לחץ על 'בדוק גורמים ופיגורים מומלצים' לבדיקת דיאגנוסטיקה." else "Click 'Check Best Factors & Lags' to inspect diagnostic recommendations."),
                  tags$li(if (is_he) "אופציונלי: התאם פרמטרי DFM (r = גורמים, p = פיגורים). לחץ על 'אודות פרמטרי DFM' למידע נוסף." else "OPTIONAL: Adjust the DFM parameters (r = factors, p = lags). Click on \"About DFM Parameters\" to learn more."),
                  tags$li(if (is_he) "לחץ על 'הרצ צינור עיבוד' – עקוב אחר הלוגים בלשונית Execution Logs." else "Click 'Run Pipeline' – track output in the Execution Logs tab."),
                  tags$li(if (is_he) "הורד את דוח התחזית ב-Excel או את תחזיות הגלם ב-CSV בסיום." else "Download the generated Excel report or the raw predictions CSV upon completion.")
                ),
                p(
                  if (is_he) "הצינור מבצע התאמה עונתית, טרנספורמציית נתונים, חילוץ גורמים, מידול XGBoost ומפיק דוח תחזית עכשיוית." 
                  else "The pipeline performs seasonal adjustment, data transformation, factor extraction, XGBoost modeling, and generates a nowcast report.", 
                  style = "margin-top: 8px; font-size: 0.9em; color: #555;"
                )
              )
            ),
            
            hr(style = "margin: 10px 0; border-top: 1px solid #ddd;"),
            
            # Dropdown 2: Model Parameter Guide & Calibration
            tags$details(
              tags$summary(if (is_he) "אודות פרמטרי DFM" else "About DFM Parameters"),
              div(
                style = "margin-top: 8px; padding-left: 5px; padding-right: 5px;",
                p(if (is_he) "מודלי גורמים דינמיים (DFM) מתמצית מידע מלוח נתונים רחב של סדרות עיתיות מאקרו-כלכליות למספר מצומצם של גורמים משותפים." 
                  else "Dynamic Factor Models (DFM) summarize information from a large panel of macroeconomic time series into a few unobserved common factors."),
                
                tags$ul(
                  tags$li(
                    tags$b(if (is_he) "r (גורמים / factors): " else "r (factors): "), 
                    if (is_he) "מספר הגורמים הסטטיים/דינמיים שחולצו מסדרות החיזוי החודשיות." else "Number of static/dynamic factors extracted from the monthly predictor panel.",
                    tags$br(),
                    tags$span(if (is_he) "💡 4 גורמים הוא בדרך כלל הטווח האופטימלי ללכידת מגמות מאקרו מרכזיות ללא הכנסת רעש." else "💡 4 factors is usually optimal to capture core macroeconomic trends without introducing excess noise.", style = "color: #555; font-size: 0.9em;")
                  ),
                  tags$li(
                    tags$b(if (is_he) "p (פיגורים / lags): " else "p (lags): "), 
                    if (is_he) "סדר הפיגור במודל ה-VAR המנהל את דינמיקת הגורמים." else "Lag order of the vector autoregression (VAR) governing factor dynamics.",
                    tags$br(),
                    tags$span(if (is_he) "💡 3 פיגורים בדרך כלל מומלצים מכיוון שהנתונים החודשיים ממופים לתוצר רבעוני (אופק של 3 חודשים)." else "💡 3 lags is usually best because the data is quarterly (monthly series mapped to quarterly GDP horizons).", style = "color: #555; font-size: 0.9em;")
                  )
                ),
                
                hr(style = "margin: 10px 0; border-top: 1px dashed #ccc;"),
                
                tags$strong(if (is_he) "כיול פרמטרים (Parameter Calibration):" else "Parameter Calibration:"),
                p(
                  if (is_he) "הרצת הדיאגנוסטיקה עוזרת למצוא את ערכי p ו-r שיביאו לתוצאות המאוזנות ביותר — מניעת התאמת יתר (Overfitting) או התאמת חסר (Underfitting). מכיוון שלכיול זה נדרש איזון בין קריטריוני מידע מרובים, התהליך אינו ניתן לאוטומציה מלאה ודורש עין מקצועית לבחינת התרשימים." 
                  else "Running diagnostics helps identify the exact values of p (lags) and r (factors) that deliver optimal model balance—stripping out noise without underfitting key macroeconomic trends. Because parameter selection balances multiple statistical criteria, this step cannot be automated and requires human eyes to evaluate visual trade-offs.", 
                  style = "margin-top: 4px; font-size: 0.88em; color: #333;"
                ),
                
                tags$ol(
                  style = "margin-top: 6px; font-size: 0.9em;",
                  tags$li(if (is_he) "לחץ על 'בדוק גורמים ופיגורים מומלצים' בסרגל הצד." else "Click 'Check Best Factors & Lags' in the sidebar."),
                  tags$li(if (is_he) "עבור ללשונית 'Diagnostics' בפאנל המרכזי והמתן לסיום ההרצה." else "Switch to the 'Diagnostics' tab in the main panel and wait for execution to complete."),
                  tags$li(if (is_he) "בחן את עקומות מבחני המידע (Bai & Ng / AIC / BIC) לבחירת הפרמטרים הסופיים." else "Visually evaluate the criteria curves to pick your final parameters.")
                ),
                
                div(
                  style = "background-color: #fff3cd; color: #856404; padding: 10px; border-left: 4px solid #ffebaA; border-right: 4px solid #ffebaA; border-radius: 4px; margin-top: 10px; font-size: 0.88em;",
                  tags$strong(if (is_he) "⚠️ אזהרות חשובות:" else "⚠️ Important Warnings:"),
                  tags$ul(
                    style = "margin-bottom: 0; margin-top: 4px; padding-left: 18px; padding-right: 18px;",
                    tags$li(tags$b(if (is_he) "קרא בעיון: " else "Read Carefully: "), if (is_he) "הקפד לקרוא את הוראות הדיאגנוסטיקה בעיון לפני בחירת הפיגורים הסופיים." else "Make sure to read the diagnostic output instructions carefully before selecting your final lags.")
                  )
                )
              )
            ),
            
            hr(style = "margin: 10px 0; border-top: 1px solid #ddd;"),
            
            # Dropdown 3: Data Specification Guide
            tags$details(
              tags$summary(if (is_he) "מפרט קובץ הנתונים ומדריך העלאה" else "Data File Specification & Upload Guide"),
              
              div(
                class = "guide-container",
                
                p(if (is_he) "כדי להבטיח שהמודל ירוץ בצורה חלקה ללא שגיאות, על קובץ ה-Excel המועלה (.xlsx) לעמוד בדיוק במבנה, בשמות הגיליונות ובשמות העמודות המפורטים להלן." 
                  else "To ensure the Nowcasting model runs smoothly without execution errors or pipeline failures, any uploaded Excel file (.xlsx) must strictly conform to the structure, sheet naming, and exact column headers outlined below."),
                
                h5(tags$b(if (is_he) "דרישות קובץ כלליות" else "General File Requirements")),
                tags$ul(
                  tags$li(tags$b(if (is_he) "פורמט קובץ: " else "File Format: "), "Standard Excel Workbook (.xlsx)."),
                  tags$li(tags$b(if (is_he) "שמות גיליונות: " else "Sheet Names: "), if (is_he) "חייב להכיל את כל 12 הגיליונות הנדרשים בדיוק באותו כתיב (רגיש לאותיות קטנות/גדלות, ללא רווחים)." else "Must contain all 12 required sheets using exact spelling (case-sensitive, no spaces)."),
                  tags$li(tags$b(if (is_he) "עמודת תאריך: " else "Date Column: "), if (is_he) "עמודה ראשונה בכל גיליון בשם Date (YYYY-MM-DD)." else "Every data sheet must have a Date column as its first column (YYYY-MM-DD)."),
                  tags$li(tags$b(if (is_he) "סוגי נתונים: " else "Data Types: "), if (is_he) "מספרים טהורים בלבד (ללא סימני מטבע $, ₪, פסיקים או טקסט). השאר ערכים חסרים ריקים." else "All indicator columns must contain pure numeric values (no currency symbols, commas, or text notes). Leave unobserved periods blank.")
                ),
                
                hr(style = "margin: 12px 0; border-top: 1px solid #ccc;"),
                
                h5(tags$b(if (is_he) "פירוט הגיליונות והעמודות הנדרשות" else "Required Sheets & Exact Column Breakdown")),
                
                tags$strong(if (is_he) "1. גיליון הגדרות" else "1. Configuration Sheet"),
                tags$ul(
                  tags$li(tags$code("dataupdate"), if (is_he) " — טבלת 2 עמודות (שם קבוצת קטגוריה, פיגור פרסום)." else " — 2-column key-value table (Category group name, Publication lag specification).")
                ),
                
                tags$strong(if (is_he) "2. גיליוני אינדיקטורים כלכליים חודשיים" else "2. Monthly Economic Indicator Sheets"),
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
                
                tags$strong(if (is_he) "3. גיליוני יעד ומדדי מאקרו" else "3. Target & Macro Adjusters Sheets"),
                tags$ul(class = "col-list",
                        tags$li(tags$code("target"), ": Date, GDP (Quarterly frequency)"),
                        tags$li(tags$code("adjusters"), ": Date, VAT_rate, CPI (Monthly frequency)")
                ),
                
                hr(style = "margin: 12px 0; border-top: 1px solid #ccc;"),
                
                h5(tags$b(if (is_he) "טבלת שגיאות נפוצות" else "Common Causes of Pipeline Failure")),
                tags$table(
                  class = "table table-bordered table-striped guide-table",
                  tags$thead(
                    tags$tr(
                      tags$th(if (is_he) "שגיאה אפשרית" else "Potential Error"),
                      tags$th(if (is_he) "סיבה" else "Cause"),
                      tags$th(if (is_he) "פתרון" else "Prevention / Fix")
                    )
                  ),
                  tags$tbody(
                    tags$tr(
                      tags$td(tags$code("KeyError: 'FX_liqudity'")),
                      tags$td(if (is_he) "שגיאת כתיב בשם גיליון." else "Sheet name misspelled (e.g., FX_liquidity)."),
                      tags$td(if (is_he) "שמור על שמות גיליונות זהים לקובץ הראשי." else "Keep sheet names identical to sample file.")
                    ),
                    tags$tr(
                      tags$td(tags$code("ValueError: string to float")),
                      tags$td(if (is_he) "מספרים שמורים כטקסט או עם פסיקים." else "Numbers formatted as text or with commas (1,500.20)."),
                      tags$td(if (is_he) "הגדר פורמט תאים כמספר והסר פסיקים." else "Set cell format to Number; remove thousands separators.")
                    ),
                    tags$tr(
                      tags$td(tags$code("Date Parsing Error")),
                      tags$td(if (is_he) "תאריכים שמורים כטקסט בפורמט מעורב." else "Dates stored as text in mixed formats."),
                      tags$td(if (is_he) "הגדר עמודת תאריך כ-Short Date (YYYY-MM-DD)." else "Format Date column as Short Date (YYYY-MM-DD).")
                    ),
                    tags$tr(
                      tags$td(tags$code("Missing Columns Error")),
                      tags$td(if (is_he) "עמודות קיימות נמחקו או שונה שמן." else "Existing feature columns deleted or renamed."),
                      tags$td(if (is_he) "שמור על כותרות עמודות זהות." else "Maintain identical headers across updates.")
                    )
                  )
                ),
                
                hr(style = "margin: 12px 0; border-top: 1px solid #ccc;"),
                
                h5(tags$b(if (is_he) "רשימת בדיקה לפני העלאה" else "Pre-Upload Checklist")),
                tags$ul(
                  tags$li(if (is_he) "נשמר כקובץ .xlsx." else "Saved as an .xlsx workbook."),
                  tags$li(if (is_he) "מכיל את כל 12 הגיליונות הנדרשים." else "Contains all 12 required sheet names."),
                  tags$li(if (is_he) "עמודה ראשונה בכל גיליון נקראת Date." else "First column in every sheet is named Date."),
                  tags$li(if (is_he) "לגיליונות חודשיים יש תאריכים חודשיים רציפים." else "Monthly sheets have contiguous monthly dates."),
                  tags$li(if (is_he) "גיליון target מכיל תאריכים רבעוניים ותוצר נומרי." else "target sheet contains quarterly dates & numeric GDP."),
                  tags$li(if (is_he) "כל התאים הנומריים נקיים (ללא סימני מטבע/טקסט)." else "All numeric cells are clean (no currency symbols/text).")
                )
              )
            )
          ),
          
          fileInput("raw_data", if (is_he) "1. העלאת נתוני גלם (Excel)" else "1. Upload Raw Data (Excel)", accept = c(".xlsx")),
          
          actionButton("diag_btn", if (is_he) "בדוק גורמים ופיגורים מומלצים" else "Check Best Factors & Lags", class = "btn-warning", style = "width: 100%; margin-bottom: 20px;"),
          
          numericInput("dfm_r", if (is_he) "פרמטר DFM 'r' (גורמים):" else "DFM 'r' parameter (factors):", value = 4, min = 1, step = 1),
          numericInput("dfm_p", if (is_he) "פרמטר DFM 'p' (פיגורים):" else "DFM 'p' parameter (lags):", value = 3, min = 1, step = 1),
          
          actionButton("run_btn", if (is_he) "2. הרץ צינור עיבוד" else "2. Run Pipeline", class = "btn-primary", style = "width: 100%; margin-bottom: 20px;"),
          
          uiOutput("download_ui"),
          uiOutput("download_csv_ui")
        ),
        
        mainPanel(
          tabsetPanel(
            id = "main_tabs",
            tabPanel(if (is_he) "יומן הרצה (Logs)" else "Execution Logs",
                     value = "logs",
                     br(),
                     verbatimTextOutput("logs_output", placeholder = TRUE)
            ),
            tabPanel(if (is_he) "דיאגנוסטיקה" else "Diagnostics",
                     value = "diagnostics",
                     br(),
                     # NEW: withMathJax renders mathematical formulas correctly
                     withMathJax(uiOutput("diag_ui"))
            )
          )
        )
      )
    )
  })
  
  # ----------------------------------------------------------------------------
  # 2. LOGGING & REACTIVE STORAGE
  # ----------------------------------------------------------------------------
  rv <- reactiveValues(
    logs = character(0),
    report_wb = NULL,
    out_df = NULL,
    diag_res = NULL
  )
  
  # Temporary file to stream real-time execution logs
  log_file <- tempfile("shiny_logs_", fileext = ".txt")
  file.create(log_file)
  
  append_log <- function(msg) {
    formatted_msg <- paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", msg)
    rv$logs <- c(rv$logs, formatted_msg)
    cat(paste0(formatted_msg, "\n"), file = log_file, append = TRUE)
  }
  
  log_data <- reactiveFileReader(500, session, log_file, readLines)
  
  output$logs_output <- renderText({
    paste(log_data(), collapse = "\n")
  })
  
  # ----------------------------------------------------------------------------
  # 3. DIAGNOSTICS EXECUTION & RENDERING
  # ----------------------------------------------------------------------------
  observeEvent(input$diag_btn, {
    req(input$raw_data)
    
    # Automatically switch active tab to Diagnostics
    updateTabsetPanel(session, "main_tabs", selected = "diagnostics")
    
    # Clear old Excel reports to hide download buttons
    rv$report_wb <- NULL
    rv$out_df <- NULL
    
    rv$logs <- character(0)
    file.create(log_file)
    
    # Log the button click
    append_log("=== INITIATING DIAGNOSTICS ===")
    append_log(paste(">>> File loaded:", input$raw_data$name))
    
    tryCatch({
      withProgress(message = 'Running Diagnostics...', value = 0, {
        
        append_log(">>> STEP 1: Fast data reading for diagnostics")
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
        
        # Update inputs with recommended values
        updateNumericInput(session, "dfm_r", value = diag_res$suggested_r)
        updateNumericInput(session, "dfm_p", value = diag_res$suggested_p)
        
        append_log(">>> DIAGNOSTICS COMPLETED SUCCESSFULLY")
        setProgress(1, detail = "Done!")
      })
      
    }, error = function(e) {
      append_log(paste("ERROR:", e$message))
    })
  })
  
  # --- NEW: Dynamic Diagnostics Page Layout ---
  output$diag_ui <- renderUI({
    is_he <- lang() == "he"
    if (is.null(rv$diag_res)) {
      msg <- if (is_he) "אנא העלה קובץ נתונים ולחץ על 'בדוק גורמים ופיגורים מומלצים' להצגת הדיאגנוסטיקה." else "Please upload data and click 'Check Best Factors & Lags' to view diagnostics."
      return(h5(msg))
    }
    
    fluidRow(
      column(12,
             h3(if (is_he) "מדריך פרשנות למבחני דיאגנוסטיקה" else "Diagnostic Interpretation Guide", style = "color: #1E3A8A; font-weight: bold;"),
             
             # 1. New Conceptual Example Plots
             plotOutput("plot_examples", height = "250px"),
             
             # 2. Comprehensive Math & Selection Guide
             htmlOutput("diag_text"),
             
             hr(style = "border-top: 2px solid #ccc; margin-top: 30px; margin-bottom: 30px;"),
             
             # 3. Actual Uploaded Data Results
             h3(if (is_he) "תוצאות הנתונים האמיתיים שלך" else "Your Actual Data Results", style = "color: #1E3A8A; font-weight: bold;"),
             h4(if (is_he) "בחירת גורמים (r) – קריטריוני Bai & Ng" else "Factor Selection (r) – Bai & Ng Criteria"),
             plotOutput("plot_icr", height = "350px"),
             hr(),
             h4(if (is_he) "בחירת פיגורים (p) – קריטריוני AIC / BIC" else "Lag Selection (p) – AIC / BIC Search"),
             plotOutput("plot_lags", height = "350px")
      )
    )
  })
  
  # --- NEW: Generate Conceptual "Knee" vs "Elbow" plots in Base R ---
  output$plot_examples <- renderPlot({
    is_he <- lang() == "he"
    par(mfrow = c(1, 2), mar = c(4, 4, 3, 1), bg = "#f8f9fa")
    
    # Example 1: The Knee (Diminishing Returns)
    x_knee <- 1:8
    y_knee <- c(10, 4, 1.8, 1.3, 1.1, 1.05, 1.02, 1.0)
    plot(x_knee, y_knee, type = "b", pch = 16, col = "darkblue", lwd = 2,
         xlab = if(is_he) "פרמטר (r או p)" else "Parameter (r or p)", 
         ylab = if(is_he) "קריטריון שגיאה" else "Error Criterion",
         main = if(is_he) "צורה 1: 'ברך' (Knee)\nתפוקה שולית פוחתת" else "Shape 1: 'Knee'\nDiminishing Returns")
    points(3, y_knee[3], col = "red", cex = 3, lwd = 3)
    text(4.5, 4, if(is_he) "בחר כאן:\nהעקומה מתיישרת" else "Pick Here:\nCurve flattens", col = "red")
    
    # Example 2: The Elbow/Min (U-Shape)
    x_min <- 1:6
    y_min <- (x_min - 3)^2 + 2
    plot(x_min, y_min, type = "b", pch = 16, col = "darkgreen", lwd = 2,
         xlab = if(is_he) "פרמטר (r או p)" else "Parameter (r or p)", 
         ylab = if(is_he) "קריטריון שגיאה" else "Error Criterion",
         main = if(is_he) "צורה 2: מינימום / מרפק (U-Shape)\nנקודת האיזון האופטימלית" else "Shape 2: Minimum (U-Shape)\nOptimal Balance Point")
    points(3, y_min[3], col = "red", cex = 3, lwd = 3)
    text(3, 6, if(is_he) "בחר כאן:\nהשפל המוחלט" else "Pick Here:\nAbsolute lowest point", col = "red")
  })
  
  # --- NEW: Re-written Text with MathJax Formulas and Non-Patronizing Explanations ---
  output$diag_text <- renderText({
    req(rv$diag_res)
    res <- rv$diag_res
    is_he <- lang() == "he"
    
    if (is_he) {
      paste0(
        "<div style='background-color: #f9f9f9; padding: 15px; border-radius: 5px; border: 1px solid #ddd; margin-top: 15px;'>",
        
        "<h4 style='color: #0056b3; margin-top:0;'>💡 המלצות המערכת (מחושבות אמפירית)</h4>",
        "<ul style='font-size: 1.1em;'>",
        "<li><b>מספר גורמים מומלץ (r): <span style='color:red;'>", res$suggested_r, "</span></b></li>",
        "<li><b>מספר פיגורים מומלץ (p): <span style='color:red;'>", res$suggested_p, "</span></b></li>",
        "</ul>",
        
        "<h4 style='color: #0056b3; margin-top: 20px;'>🔍 שלב 1: זיהוי צורות ויזואלי (תלוי נתונים)</h4>",
        "<p>לא ניתן להניח מראש איזו צורה תתקבל. ללא קשר לשאלה האם מדובר בגרף הגורמים או הפיגורים, עלייך לבחון את הגרפים האמיתיים שלך למטה ולחפש אחת משתי התופעות שמוצגות בדוגמאות למעלה:</p>",
        "<ul>",
        "<li><b>צורה 1 - 'ברך' (Knee):</b> ירידה חדה שמתיישרת לפתע. <b>החוק:</b> בחר את הנקודה שבה התוספת של גורם/פיגור מתחילה להיות זניחה. הוספת פרמטרים נוספים אחרי נקודה זו רק תייצר רעש.</li>",
        "<li><b>צורה 2 - מינימום (U-Shape):</b> גרף שיורד לנקודת שפל ואז עולה בחזרה. <b>החוק:</b> בחר את נקודת השפל המוחלטת (מרפק/עמק), שכן היא מייצגת את הקנס המינימלי למורכבות המודל.</li>",
        "</ul>",
        
        "<hr style='margin-top: 20px; margin-bottom: 20px;'>",
        
        "<h4 style='color: #0056b3;'>🧮 שלב 2: קריאת גרף הפיגורים (p) – AIC לעומת BIC</h4>",
        "<p>התרשים התחתון מציג שני קריטריונים שונים למציאת האיזון הנכון בין דיוק המודל למספר הפיגורים שלו. מה ההבדל ביניהם?</p>",
        "<ul>",
        "<li><b>AIC (Akaike Information Criterion - קו אפור):</b> קריטריון שסלחן יותר למורכבות. הוא מנסה למקסם כוח ניבוי טהור ולכן נוטה להמליץ על מודלים עם הרבה פיגורים. <br><b>מתי להשתמש?</b> רק כאשר יש לך עשרות שנות נתונים ברזולוציה יומית או חודשית, ואין חשש מהתאמת-יתר (Overfitting).</li>",
        "<li><b>BIC (Bayesian Information Criterion - קו כחול):</b> קריטריון שמטיל קנס כבד על הוספת משתנים למודל. הוא מחפש מודלים פשוטים וחסכוניים. <br><b>מתי להשתמש? <span style='color:red;'>זהו הסטנדרט לבחירת מודלי מאקרו.</span></b> הוא מונע התאמת יתר של התוצר כאשר מדגם הנתונים קטן יחסית. <b>לכן, תמיד התמקד בעקומת ה-BIC.</b></li>",
        "</ul>",
        
        "<hr style='margin-top: 20px; margin-bottom: 20px;'>",
        
        "<h4 style='color: #0056b3;'>📊 שלב 3: קריאת גרף הגורמים (r) – קריטריוני Bai & Ng (2002)</h4>",
        "<p>התרשים העליון מציג את שלושת קריטריוני המידע של Bai & Ng לבחירת גורמים מתוך פאנל נתונים רחב. שלושתם מנסים למזער את השונות הלא-מוסברת \\( V(r) \\) מול קנס מתמטי שהולך וגדל ככל שמוסיפים גורמים \\( r \\).</p>",
        
   
        "<p style='margin-top: 15px;'><b>איזה מהם להעדיף כאשר הקווים מציגים תוצאות שונות?</b></p>",
        "<ul>",
        "<li><b>IC2 (קו ירוק): <span style='color:red;'>הסטנדרט המועדף.</span></b> מאזן בצורה העקבית והנכונה ביותר בין כמות המשתנים (N) לאורך הזמן (T). המלצת המערכת נגזרת מקו זה.</li>",
        "<li><b>IC1 (קו שחור):</b> משמש בעיקר כגיבוי ווידוא ל-IC2 (לרוב הם ינועו ביחד).</li>",
        "<li><b>IC3 (קו תכלת):</b> הקריטריון המחמיר ביותר. הוא קונס מורכבות בצורה כל כך אגרסיבית שלפעמים הוא נוטה לבחור מעט מדי גורמים, במיוחד במדגמי מאקרו קטנים.</li>",
        "</ul>",
        
        "</div>"
      )
    } else {
      paste0(
        "<div style='background-color: #f9f9f9; padding: 15px; border-radius: 5px; border: 1px solid #ddd; margin-top: 15px;'>",
        
        "<h4 style='color: #0056b3; margin-top:0;'>💡 System Recommendations (Empirically Calculated)</h4>",
        "<ul style='font-size: 1.1em;'>",
        "<li><b>Suggested Factors (r): <span style='color:red;'>", res$suggested_r, "</span></b></li>",
        "<li><b>Suggested Lags (p): <span style='color:red;'>", res$suggested_p, "</span></b></li>",
        "</ul>",
        
        "<h4 style='color: #0056b3; margin-top: 20px;'>🔍 Step 1: Visual Shape Identification (Data-Dependent)</h4>",
        "<p>You cannot assume a chart will look a certain way. Regardless of whether you are looking at the Factors (r) or Lags (p) chart, examine your actual results below for one of the two empirical phenomena illustrated in the examples above:</p>",
        "<ul>",
        "<li><b>Shape 1 - The 'Knee':</b> A sharp drop that abruptly flattens.</li> <li><b>Rule:</b> Select the point exactly where diminishing returns set in (where adding a parameter yields almost zero extra drop in error). Adding parameters past the knee just introduces noise.</li>",
        "<li><b>Shape 2 - The Minimum (U-Shape):</b> A curve that descends to a clear low point and then rises again.</li> <li> <b>Rule:</b> Select the absolute minimum point (the elbow/valley), as this represents the lowest possible penalty for model complexity.</li>",
        "</ul>",
        
        "<hr style='margin-top: 20px; margin-bottom: 20px;'>",
        
        "<h4 style='color: #0056b3;'>🧮 Step 2: Reading the Lags (p) Chart – AIC vs. BIC</h4>",
        "<p>The bottom plot compares two different mathematical criteria that try to find the perfect balance between the accuracy of your model and how many lags it uses. What is the difference?</p>",
        "<ul>",
        "<li><b>AIC (Akaike Information Criterion - Gray line):</b> Forgiving of complexity. It maximizes pure predictive power, so it tends to suggest models with many lags. <br><b>When to use?</b> Only if you have massive, high-frequency datasets (decades of daily data) where overfitting is unlikely.</li>",
        "<li><b>BIC (Bayesian Information Criterion - Blue line):</b> Heavily penalizes adding extra variables. It looks for simple, parsimonious models. <br><b>When to use? <span style='color:red;'>This is the preferred standard for macroeconomic nowcasting.</span></b> It actively prevents overfitting GDP when sample sizes are small. <b>Always focus your decision on the BIC curve.</b></li>",
        "</ul>",
        
        "<hr style='margin-top: 20px; margin-bottom: 20px;'>",
        
        "<h4 style='color: #0056b3;'>📊 Step 3: Reading the Factors (r) Chart – Bai & Ng (2002) Criteria</h4>",
        "<p>The top plot displays the three Bai & Ng Information Criteria for extracting factors from a large dataset. They all aim to minimize the unexplained variance \\( V(r) \\) while mathematically penalizing you for extracting too many factors \\( r \\).</p>",
        
        "<div style='background-color: #fff; padding: 15px; border: 1px dashed #ccc; font-family: monospace; overflow-x: auto;'>",
        "$$ IC_{p1}(r) = \\ln(V(r)) + r \\left( \\frac{N+T}{NT} \\right) \\ln\\left( \\frac{NT}{N+T} \\right) $$",
        "$$ IC_{p2}(r) = \\ln(V(r)) + r \\left( \\frac{N+T}{NT} \\right) \\ln(\\min(N,T)) $$",
        "$$ IC_{p3}(r) = \\ln(V(r)) + r \\left( \\frac{\\ln(\\min(N,T))}{\\min(N,T)} \\right) $$",
        "</div>",
        "<p style='margin-top: 15px;'><b>Which one to trust when lines diverge?</b></p>",
        "<ul>",
        "<li><b>IC2 (Green line): <span style='color:red;'>The industry standard and default choice.</span></b> The system suggestion relies on this. It provides the most mathematically consistent scaling penalty across N (number of variables) and T (time points).</li>",
        "<li><b>IC1 (Black line):</b> Often tracks closely with IC2; used primarily as a robustness check.</li>",
        "<li><b>IC3 (Light Blue line):</b> The most aggressive penalty. It penalizes complexity so heavily that it can sometimes under-select factors in smaller macro panels.</li>",
        "</ul>",
        
        "</div>"
      )
    }
  })
  
  output$plot_icr <- renderPlot({
    req(rv$diag_res)
    plot(rv$diag_res$ic)
  })
  
  output$plot_lags <- renderPlot({
    req(rv$diag_res)
    res_df <- rv$diag_res$results_df
    
    ggplot(res_df, aes(x = p)) +
      geom_line(aes(y = BIC, color = "BIC"), linewidth = 1) +
      geom_point(aes(y = BIC, color = "BIC"), size = 3) +
      geom_line(aes(y = AIC, color = "AIC"), linewidth = 1, linetype = "dashed") +
      geom_point(aes(y = AIC, color = "AIC"), size = 3) +
      geom_vline(xintercept = rv$diag_res$suggested_p, color = "red", linetype = "dotted", linewidth = 1.5) +
      annotate("text", x = rv$diag_res$suggested_p, y = min(res_df$BIC, na.rm=TRUE), 
               label = paste("System Suggestion:", rv$diag_res$suggested_p), color = "red", vjust = -1, hjust = -0.1) +
      scale_color_manual(values = c("BIC" = "blue", "AIC" = "darkgray")) +
      labs(title = "Information Criteria for Lags (p)", x = "Number of Lags (p)", y = "Criterion Value", color = "Metric") +
      theme_minimal()
  })
  
  # ----------------------------------------------------------------------------
  # 4. PIPELINE EXECUTION ENGINE
  # ----------------------------------------------------------------------------
  observeEvent(input$run_btn, {
    req(input$raw_data)
    
    # Automatically snap back to the Execution Logs tab so they can watch it run
    updateTabsetPanel(session, "main_tabs", selected = "logs")
    
    rv$logs <- character(0)
    rv$report_wb <- NULL
    rv$out_df <- NULL
    file.create(log_file)
    
    # Log the button click
    append_log("=== INITIATING PIPELINE ===")
    append_log(paste(">>> File loaded:", input$raw_data$name))
    append_log(paste(">>> DFM Parameters Selected -> r:", req(input$dfm_r), "| p:", req(input$dfm_p)))
    
    tryCatch({
      
      withProgress(message = 'Processing...', value = 0, {
        
        # Step 1: Transformations
        append_log(">>> STEP 1: Running Transformations")
        trans_res <- run_transformations(
          raw_data_path = input$raw_data$datapath,
          td_ts = td_ts,
          hag_ts = hag_ts,
          update_log = append_log,
          update_progress = function(val, msg) { incProgress(val * 0.33, detail = msg) }
        )
        
        # Step 2: DFM & XGBoost
        append_log(">>> STEP 2: Running DFM & XGBoost")
        dfm_xgb_res <- run_dfm_xgboost(
          combined_panel = trans_res$combined_panel,
          target_raw = trans_res$target_raw,
          r_val = req(input$dfm_r),
          p_val = req(input$dfm_p),
          update_log = append_log,
          update_progress = function(val, msg) { setProgress(value = 0.33 + (val * 0.33), detail = msg) }
        )
        
        # Step 3: Report Generation
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
  
  # ----------------------------------------------------------------------------
  # 5. DYNAMIC DOWNLOAD BUTTONS & HANDLERS
  # ----------------------------------------------------------------------------
  output$download_ui <- renderUI({
    is_he <- lang() == "he"
    if (!is.null(rv$report_wb)) {
      downloadButton(
        "download_report", 
        if (is_he) "3. הורד דוח Excel" else "3. Download Excel Report", 
        class = "btn-success", 
        style = "width: 100%; margin-bottom: 10px;"
      )
    }
  })
  
  output$download_csv_ui <- renderUI({
    is_he <- lang() == "he"
    if (!is.null(rv$out_df)) {
      downloadButton(
        "download_csv", 
        if (is_he) "4. הורד תחזיות גולמיות (CSV)" else "4. Download Raw Predictions (CSV)", 
        class = "btn-info", 
        style = "width: 100%;"
      )
    }
  })
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste0("Nowcast_Executive_Report_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      req(rv$report_wb)
      append_log(">>> EXPORT: Generating and downloading Excel Executive Report...")
      openxlsx::saveWorkbook(rv$report_wb, file, overwrite = TRUE)
      append_log(">>> EXPORT: Excel Report downloaded successfully.")
    }
  )
  
  output$download_csv <- downloadHandler(
    filename = function() {
      "nowcast_results.csv"
    },
    content = function(file) {
      req(rv$out_df)
      append_log(">>> EXPORT: Generating and downloading Raw Predictions CSV...")
      write.csv(rv$out_df, file, row.names = FALSE, na = "")
      append_log(">>> EXPORT: CSV downloaded successfully.")
    }
  )
}

shinyApp(ui = ui, server = server)