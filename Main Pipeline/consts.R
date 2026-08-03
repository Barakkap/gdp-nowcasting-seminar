required_columns <- list(
  dataupdate = c("personal_labor_income_taxes", "1month lag"),   # adjust as needed
  personal_labor_income_taxes = c(
    "Date", "Total Gross Income Tax Division",
    "Total refunds from the Income Tax Department",
    "Total Income Tax Division Net",
    "Deductions and the capital market",
    "Deduction from salary", "Independents advances",
    "Self-employed tax differences", "Independent Cancellations",
    "self employed returns", "Capital Gains Tax Refunds",
    "VAT Financial Institutions (Salary)", "Non-profit institution tax"
  ),
  corporate_business_tax = c(
    "Date", "Companies advances", "tax differential companies",
    "Cancellation companies",
    "Income tax for self-employed individuals and companies (advances and deductions)",
    "Companies returns", "excess expenses", "Bonds and dividends",
    "Cancellations Deductions", "Goods and services"
  ),
  consumption_tax = c(
    "Date", "Gross local VAT", "VAT refund autonomy and traders",
    "Total net VAT", "Net local sales tax", "Gross fuel tax"
  ),
  import_trade_tax = c(
    "Date", "Gross import VAT", "Net customs",
    "Net import purchase tax", "Total import taxes"
  ),
  real_estate = c(
    "Date", "Real estate taxation", "Property tax", "praise tax",
    "Real estate purchase tax", "praise tax returns", "purchase returns",
    "Apartments sold at an annual rate",
    "Apartment Price Index (1993=100) - Mid-period reviewed"
  ),
  real_activity = c(
    "Date", "cons_trust", "madad meshulav", "madad_cc_purchases_sa",
    "madad_pedio", "madad_yetzur_industrial", "Oil", "madad_hadash"
  ),
  labor = c(
    "Date", "real salary", "salaried jobs", "unemployment rate",
    "participation rate", "employment rate"
  ),
  capital_markets = c("Date", "TA35", "TA125", "Nasdaq", "sp500"),
  FX_liqudity = c(
    "Date", "Reer", "Dollar", "Foreign exchange reserves (millions of dollars)"
  ),
  target = c("Date", "GDP"),
  adjusters = c("Date", "VAT_rate", "CPI")
)

# Define the required sheets (using the exact names you provided,
required_sheets <- c(
  "dataupdate",
  "personal_labor_income_taxes",
  "corporate_business_tax",
  "consumption_tax",
  "import_trade_tax",
  "real_estate",
  "real_activity",
  "labor",
  "capital_markets",
  "FX_liqudity",
  "target",
  "adjusters"
)

