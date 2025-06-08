library(shiny)
library(DT) # For interactive tables
library(ggplot2) # For plotting
library(shinythemes) # For a nice theme
library(dplyr) # For data manipulation (e.g., filter, select)
library(googlesheets4) # For reading/writing Google Sheets
library(googledrive) # For Google Drive authentication

# --- UI (User Interface) ---
ui <- fluidPage(
  theme = shinytheme("paper"), # Apply a pleasant theme
  
  # Custom CSS for styling
  tags$head(
    tags$style(HTML("
      body { padding-top: 20px; font-family: 'Arial', sans-serif; }
      .container-fluid { max-width: 1200px; margin: auto; }
      .well { background-color: #f8f8f8; border: 1px solid #e7e7e7; border-radius: 4px; padding: 20px; margin-bottom: 20px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
      h1, h2, h3, h4 { text-align: center; color: #337ab7; margin-bottom: 25px; font-weight: bold; }
      .tab-content { padding-top: 20px; }
      .dataTables_wrapper .dt-buttons { margin-bottom: 10px; }
      .shiny-notification { position:fixed; top: calc(50%); left: calc(50%); transform: translate(-50%, -50%); z-index: 1000; }
      .founder-list { list-style-type: none; padding: 0; text-align: center; }
      .founder-list li { margin-bottom: 5px; font-size: 1.1em; color: #555; }
      .btn-block { margin-top: 15px; } /* Space above buttons */
      .shiny-input-container { margin-bottom: 15px; } /* Space between inputs */
    "))
  ),
  
  titlePanel(h1("☕ Mug Life: Coffee Corner Ops")),
  
  tabsetPanel(
    id = "main_tabs",
    tabPanel("Running Costs",
             icon = icon("coffee"),
             br(),
             sidebarLayout(
               sidebarPanel(
                 wellPanel(
                   h3("Add Running Cost"),
                   dateInput("running_date", "Date:", value = Sys.Date(), width = '100%'),
                   textInput("running_item", "Item/Description:", "", placeholder = "e.g., Coffee Beans, Milk (500ml)"),
                   selectInput("running_category", "Category:",
                               choices = c("Coffee Beans", "Milk/Dairy", "Sweeteners", "Disposables (Cups, Stirrers)", "Equipment", "Other")),
                   numericInput("running_amount", "Amount (Rs):", value = 0, min = 0, step = 0.01, width = '100%'),
                   textInput("running_payer", "Bought By:", "", placeholder = "e.g., John Doe, Yourself"),
                   actionButton("add_running", "Add Running Cost", class = "btn-primary btn-block")
                 ),
                 wellPanel(
                   h4("Admin Actions"),
                   actionButton("clear_running_data", "Clear All Running Costs", class = "btn-danger btn-block")
                 )
               ),
               mainPanel(
                 h2("Running Costs Overview"),
                 wellPanel(
                   h3("Expense List"),
                   DTOutput("running_table")
                 ),
                 wellPanel(
                   h3("Summary & Budget"),
                   fluidRow(
                     column(6, tags$h4(textOutput("running_total"), style = "color: #5cb85c;")),
                     column(6, tags$h4(textOutput("running_remaining"), style = "color: #d9534f;"))
                   )
                 ),
                 wellPanel(
                   fluidRow(
                     column(6, plotOutput("running_category_plot")),
                     column(6, plotOutput("running_payer_plot"))
                   )
                 )
               )
             )
    ),
    tabPanel("Shared Expenses",
             icon = icon("users"),
             br(),
             sidebarLayout(
               sidebarPanel(
                 wellPanel(
                   h3("Add Shared Expense"),
                   dateInput("shared_date", "Date:", value = Sys.Date(), width = '100%'),
                   textInput("shared_description", "Description:", "", placeholder = "e.g., New Kettle, Party Supplies for Coffee"),
                   numericInput("shared_amount", "Total Amount (Rs):", value = 0, min = 0, step = 0.01, width = '100%'),
                   numericInput("shared_people", "Number of People Sharing:", value = 2, min = 2, step = 1, width = '100%'),
                   textInput("shared_payer", "Paid By:", "", placeholder = "e.g., Jane Smith, You"),
                   actionButton("add_shared", "Add Shared Expense", class = "btn-primary btn-block")
                 ),
                 wellPanel(
                   h4("Admin Actions"),
                   actionButton("clear_shared_data", "Clear All Shared Expenses", class = "btn-danger btn-block")
                 )
               ),
               mainPanel(
                 h2("Shared Expenses Overview"),
                 wellPanel(
                   h3("Shared Expenses List"),
                   DTOutput("shared_table")
                 ),
                 wellPanel(
                   h3("Individual Share Calculation"),
                   tags$h4(textOutput("shared_each"), style = "text-align: center; color: #337ab7;")
                 ),
                 wellPanel(
                   plotOutput("shared_plot")
                 )
               )
             )
    ),
    tabPanel("Contributions",
             icon = icon("hand-holding-usd"),
             br(),
             sidebarLayout(
               sidebarPanel(
                 wellPanel(
                   h3("Record New Contribution"),
                   dateInput("contribution_date", "Date:", value = Sys.Date(), width = '100%'),
                   textInput("contributor_name", "Contributor's Name:", "", placeholder = "e.g., Alice, Bob"),
                   numericInput("contribution_amount", "Amount Contributed (Rs):", value = 0, min = 0, step = 0.01, width = '100%'),
                   actionButton("add_contribution", "Add Contribution", class = "btn-primary btn-block")
                 ),
                 wellPanel(
                   h4("Admin Actions"),
                   actionButton("clear_contributions_data", "Clear All Contributions", class = "btn-danger btn-block")
                 )
               ),
               mainPanel(
                 h2("Collected Contributions"),
                 p("A big thank you to everyone who helps keep our coffee corner stocked!", style = "text-align: center; margin-bottom: 20px;"),
                 wellPanel(
                   DTOutput("contributions_table")
                 )
               )
             )
    ),
    tabPanel("Founding Beans",
             icon = icon("star"),
             br(),
             wellPanel(
               h2("The Legendary Founding Beans!"),
               p("Every great coffee corner starts with a spark... and a generous wallet! We extend our deepest, most caffeinated gratitude to these pioneers who dared to dream of endless brews and contributed to kickstart our communal coffee corner.",
                 style = "text-align: center; font-size: 1.1em; margin-bottom: 20px;"),
               p("Their selfless act ensured our office transitioned from a land of instant regret to a realm of aromatic bliss. May their cups always be full, and their energy levels never dip!",
                 style = "text-align: center; font-size: 1.1em; margin-bottom: 30px;"),
               h3("Our Esteemed Coffee Corner Connoisseurs:"),
               tags$ul(class = "founder-list",
                       tags$li(icon("mug-hot"), " Balan - The Original Roaster"),
                       tags$li(icon("mug-hot"), " Sudhanshu - The Grind Guru"),
                       tags$li(icon("mug-hot"), " Sanjeev - The Espresso Emperor"),
                       tags$li(icon("mug-hot"), " Mrittika - The Froth Fanatic"),
                       tags$li(icon("mug-hot"), " Sabeeha - The Decaf Defender (who secretly loves real coffee)"),
                       tags$li(icon("mug-hot"), " Pijus - The Sugar Shaman"),
                       tags$li(icon("mug-hot"), " Srishti - The Milk Mogul"),
                       tags$li(icon("mug-hot"), " Sarat - The Cup Keeper"),
                       tags$li(icon("mug-hot"), " Manish - The Stir Stick Sorcerer"),
                       tags$li(icon("mug-hot"), " Souradyuti - The Water Wizard"),
                       tags$li(icon("mug-hot"), " Priyadarshini - The Aroma Architect"),
                       tags$li(icon("mug-hot"), " Arun - The Perfect Pourer"),
                       tags$li(icon("mug-hot"), " Runa - The Condiment Commander")
               ),
               br(),
               h4("Special Mention"),
               p(icon("hands-helping"), strong("Santosh – The Brave Bean Bystander"), 
                 " – Allergic to coffee, yet still offered to contribute. A true act of caffeinated solidarity. Your spirit brews among us, even if your cup stays empty!", 
                 style = "margin-top: 10px;"),
               p("We didn’t accept his contribution because it’s the heart that counts, and his was already full of beans ❤️.",
                 style = "margin-top: -10px; margin-bottom: 20px;"),
               p("Thanks for bean-g amazing!", style = "text-align: center; font-style: italic; margin-top: 30px;")
             )
    ),
    tabPanel("Suggestions Box",
             icon = icon("lightbulb"),
             br(),
             sidebarLayout(
               sidebarPanel(
                 wellPanel(
                   h3("Submit Your Coffee Corner Ideas!"),
                   textInput("suggestion_name", "Your Name (Optional):", "", placeholder = "e.g., Coffee Lover"),
                   selectInput("suggestion_category", "Category of Suggestion:",
                               choices = c("Coffee Beans", "Equipment", "New Recipes", "Snacks/Treats", "Improvements", "Other")),
                   textAreaInput("suggestion_text", "Your Suggestion:", "",
                                 placeholder = "e.g., 'Try out some Arabica beans from Ethiopia!' or 'A French Press would be great for larger batches.'",
                                 rows = 5),
                   actionButton("add_suggestion", "Submit Suggestion", class = "btn-primary btn-block")
                 ),
                 wellPanel(
                   h4("Admin Actions"),
                   actionButton("clear_suggestions_data", "Clear All Suggestions", class = "btn-danger btn-block")
                 )
               ),
               mainPanel(
                 h2("Community Suggestions"),
                 p("Here are the brilliant ideas from our fellow coffee enthusiasts!", style = "text-align: center; margin-bottom: 20px;"),
                 wellPanel(
                   DTOutput("suggestions_table")
                 )
               )
             )
    )
  )
)

# --- Server Logic ---
server <- function(input, output, session) {
  
  # --- Google Sheets Setup ---
  google_sheet_id <- "12cp5H-E0bZA0mK_If139Hrvc6wILBuCOJqBewjeGLys"
  json_file_path <- "infra-ratio-461719-b6-6f61f9a007c4.json" # Ensure this file is in your app's root directory ("/cloud/project/")
  
  # It's good practice to set the cache location for tokens, if any are created (e.g., during interactive fallback)
  options(gargle_oauth_cache = ".secrets")
  
  # Explicitly de-authenticate any existing user session for googlesheets4
  # This helps ensure the service account is used if available.
  gs4_deauth()
  
  # Attempt to authenticate using the service account for Google Sheets
  authenticated_as_sa <- FALSE
  if (Sys.getenv("GOOGLE_SERVICE_ACCOUNT_KEY_JSON_CONTENT") != "") { # Using a more descriptive env var name
    # If service account key JSON *content* is set as an environment variable
    message("Attempting auth using GOOGLE_SERVICE_ACCOUNT_KEY_JSON_CONTENT environment variable.")
    temp_key_file <- tempfile(fileext = ".json")
    writeLines(Sys.getenv("GOOGLE_SERVICE_ACCOUNT_KEY_JSON_CONTENT"), temp_key_file)
    tryCatch({
      gs4_auth(path = temp_key_file)
      authenticated_as_sa <- TRUE
      message("Successfully authenticated using service account from environment variable.")
    }, error = function(e) {
      message(paste("Error during gs4_auth with env var key:", e$message))
    })
    # The tempfile will be automatically cleaned up when the R session ends.
  } else if (file.exists(json_file_path)) {
    # Fallback for local development if you have a service account JSON file in the app directory
    message(paste("Attempting gs4_auth with path:", json_file_path))
    tryCatch({
      gs4_auth(path = json_file_path)
      authenticated_as_sa <- TRUE
      message("Successfully authenticated using service account from local JSON file.")
    }, error = function(e) {
      message(paste("Error during gs4_auth with local JSON file:", e$message))
    })
  }
  
  if (!authenticated_as_sa) {
    # If service account authentication failed or no SA key was found,
    # gargle will likely attempt interactive auth when read_sheet is called.
    # You could also explicitly call gs4_auth() here to trigger it earlier if desired.
    message("Service account authentication not performed or failed. Will rely on default gargle behavior (may prompt for interactive auth).")
  }
  # --- End Google Sheets Setup ---
  
  # Reactive value to store all data.
  expenses <- reactiveVal(data.frame(
    # ... (your existing expenses reactiveVal definition)
  ))
  
  expected_cols <- c("Date", "Item", "Category", "Amount", "Payer", "Type",
                     "SuggestionName", "SuggestionText")
  
  # Load existing data from Google Sheet when the app starts
  observeEvent(TRUE, {
    showNotification("Attempting to load data from Google Sheet...", type = "message", duration = 5)
    tryCatch({
      # Ensure authentication has been attempted before reading.
      # The gs4_auth calls above should have set the state.
      loaded_data <- read_sheet(google_sheet_id, sheet = "CoffeeData",
                                col_types = "Dccdcccc", # Date, Char, Char, Double, Char, Char, Char, Char
                                range = "A:H")
      
      # ... (rest of your data loading and processing logic)
      # ... (make sure this part is identical to your original working code,
      #      including the handling for Type, Date, Amount conversions and column reordering)
      
      # Example of ensuring 'Type' column as in your original code:
      if (!("Type" %in% colnames(loaded_data))) {
        loaded_data$Type <- "Running" # Default if missing
      } else {
        loaded_data$Type[is.na(loaded_data$Type)] <- "Running" # Default NAs
      }
      loaded_data$Date <- as.Date(loaded_data$Date)
      loaded_data$Amount <- as.numeric(loaded_data$Amount)
      for (col in setdiff(expected_cols, colnames(loaded_data))) {
        if (col == "Date") { loaded_data[[col]] <- as.Date(NA_character_) }
        else if (col == "Amount") { loaded_data[[col]] <- NA_real_ }
        else { loaded_data[[col]] <- NA_character_ }
      }
      loaded_data <- loaded_data[expected_cols]
      
      
      expenses(loaded_data)
      showNotification("Data loaded from Google Sheet successfully!", type = "message", duration = 3)
      
    }, error = function(e) {
      warning("Could not load data from Google Sheet: ", e$message)
      expenses(data.frame( # Initialize with empty df as before
        Date = as.Date(character()), Item = character(), Category = character(),
        Amount = numeric(), Payer = character(), Type = character(),
        SuggestionName = character(), SuggestionText = character(),
        stringsAsFactors = FALSE
      ))
      showNotification(
        paste("Error loading data from Google Sheet. Please check Sheet ID, permissions, and authentication. Error:", e$message),
        type = "error", duration = NULL
      )
    })
  }, once = TRUE) # Run this observer only once on app startup
  
  # Function to save all data to Google Sheet
  save_expenses <- function(data) {
    # Ensure data has all expected columns before writing
    # This block is crucial for sheet_write to maintain column consistency
    for (col in setdiff(expected_cols, colnames(data))) {
      if (col == "Date") {
        data[[col]] <- as.Date(NA_character_)
      } else if (col == "Amount") {
        data[[col]] <- NA_real_
      } else { # For character columns
        data[[col]] <- NA_character_
      }
    }
    # Reorder columns to ensure consistent writing order
    data <- data[expected_cols]
    
    tryCatch({
      # Write to the specified sheet. sheet_write overwrites the entire tab.
      # Ensure 'CoffeeData' sheet exists in your Google Sheet.
      sheet_write(data, ss = google_sheet_id, sheet = "CoffeeData")
      # No notification here, as calls to save_expenses are followed by specific action notifications
    }, error = function(e) {
      warning("Could not save data to Google Sheet: ", e$message)
      showNotification(paste("Error saving data to Google Sheet:", e$message), type = "error", duration = NULL)
    })
  }
  
  # --- Running Costs ---
  observeEvent(input$add_running, {
    req(input$running_item, input$running_amount, input$running_payer)
    
    new_expense <- data.frame(
      Date = input$running_date,
      Item = trimws(input$running_item), # Remove leading/trailing whitespace
      Category = input$running_category,
      Amount = input$running_amount,
      Payer = trimws(input$running_payer), # Remove leading/trailing whitespace
      Type = "Running",
      SuggestionName = NA_character_,
      SuggestionText = NA_character_,
      stringsAsFactors = FALSE
    )
    current_expenses <- expenses()
    updated_expenses <- rbind(current_expenses, new_expense)
    expenses(updated_expenses)
    save_expenses(updated_expenses) # Save to Google Sheet
    showNotification("Running cost added successfully!", type = "message", duration = 3) # Changed type
    updateTextInput(session, "running_item", value = "")
    updateNumericInput(session, "running_amount", value = 0)
    updateTextInput(session, "running_payer", value = "")
  })
  
  # Clear all running costs
  observeEvent(input$clear_running_data, {
    showModal(modalDialog(
      title = "Confirm Data Deletion",
      "Are you sure you want to delete ALL Running Cost data? This action cannot be undone.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_clear_running", "Delete All Running Costs", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_clear_running, {
    removeModal()
    current_expenses <- expenses()
    # Keep all data that is NOT of Type "Running"
    updated_expenses <- filter(current_expenses, Type != "Running")
    expenses(updated_expenses)
    save_expenses(updated_expenses)
    showNotification("All running cost data cleared!", type = "warning", duration = 3)
  })
  
  output$running_table <- renderDT({
    # Ensure 'Type' column exists before filtering
    if ("Type" %in% colnames(expenses()) && nrow(expenses()) > 0) {
      display_data <- filter(expenses(), Type == "Running") %>% select(Date, Item, Category, Amount, Payer)
      if (nrow(display_data) == 0) {
        # Return empty DT with correct column names if no running data
        return(datatable(data.frame(Date=as.Date(character()), Item=character(), Category=character(),
                                    Amount=numeric(), Payer=character()),
                         options = list(pageLength = 10, autoWidth = TRUE),
                         rownames = FALSE, selection = 'none'))
      }
      datatable(display_data,
                options = list(pageLength = 10, autoWidth = TRUE,
                               columnDefs = list(list(width = '10%', targets = c(0, 3)))), # Adjust column widths if needed
                rownames = FALSE, selection = 'none')
    } else {
      # Initial empty table if expenses() is completely empty or Type column is missing
      datatable(data.frame(Date=as.Date(character()), Item=character(), Category=character(),
                           Amount=numeric(), Payer=character()),
                options = list(pageLength = 10, autoWidth = TRUE),
                rownames = FALSE, selection = 'none')
    }
  })
  
  output$running_total <- renderText({
    if ("Type" %in% colnames(expenses())) {
      total <- sum(filter(expenses(), Type == "Running")$Amount, na.rm = TRUE) # Add na.rm
      paste("Total Running Costs:", format(total, big.mark = ",", scientific = FALSE), "Rs")
    } else {
      "Total Running Costs: N/A"
    }
  })
  
  output$running_remaining <- renderText({
    if ("Type" %in% colnames(expenses())) {
      total_running_expenses <- sum(filter(expenses(), Type == "Running")$Amount, na.rm = TRUE)
      total_contributions <- sum(filter(expenses(), Type == "Contribution")$Amount, na.rm = TRUE)
      
      remaining <- total_contributions - total_running_expenses
      color_style <- ifelse(remaining < 0, "color: #d9534f;", "color: #5cb85c;") # Red if negative, green if positive
      
      paste("Remaining Budget: ",format(remaining, big.mark = ",", scientific = FALSE), "Rs")
    } else {
      "Remaining Budget: N/A (Add contributions to see budget)"
    }
  })
  
  output$running_category_plot <- renderPlot({
    if ("Type" %in% colnames(expenses()) && nrow(filter(expenses(), Type == "Running")) > 0) {
      running_data <- filter(expenses(), Type == "Running")
      category_summary <- aggregate(Amount ~ Category, data = running_data, sum, na.action = na.omit)
      ggplot(category_summary, aes(x = "", y = Amount, fill = Category)) +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y", start = 0) +
        theme_void() +
        labs(title = "Running Costs by Category") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
              legend.title = element_text(size = 12, face = "bold"),
              legend.text = element_text(size = 11)) +
        guides(fill = guide_legend(title = "Category"))
    } else {
      ggplot() +
        annotate("text", x = 0, y = 0, label = "No running cost data to show for categories yet.", size = 5, color = "grey50") +
        theme_void()
    }
  })
  
  output$running_payer_plot <- renderPlot({
    if ("Type" %in% colnames(expenses()) && nrow(filter(expenses(), Type == "Running")) > 0) {
      running_data <- filter(expenses(), Type == "Running")
      payer_summary <- aggregate(Amount ~ Payer, data = running_data, sum, na.action = na.omit)
      ggplot(payer_summary, aes(x = reorder(Payer, -Amount), y = Amount, fill = Payer)) +
        geom_bar(stat = "identity", fill = "#5cb85c") + # Fixed color for bars
        labs(title = "Running Costs by Payer", x = "Payer", y = "Amount (Rs)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(size = 14, face = "bold"),
              plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
              legend.position = "none") # Hide legend as fill is Payer
    } else {
      ggplot() +
        annotate("text", x = 0, y = 0, label = "No running cost data to show for payers yet.", size = 5, color = "grey50") +
        theme_void()
    }
  })
  
  
  # --- Shared Expenses ---
  observeEvent(input$add_shared, {
    req(input$shared_description, input$shared_amount, input$shared_people, input$shared_payer)
    
    new_expense <- data.frame(
      Date = input$shared_date,
      Item = trimws(input$shared_description),
      Category = "Shared Expense", # Fixed category for shared expenses
      Amount = input$shared_amount,
      Payer = trimws(input$shared_payer),
      Type = "Shared",
      SuggestionName = NA_character_,
      SuggestionText = NA_character_,
      stringsAsFactors = FALSE
    )
    current_expenses <- expenses()
    updated_expenses <- rbind(current_expenses, new_expense)
    expenses(updated_expenses)
    save_expenses(updated_expenses) # Save to Google Sheet
    showNotification("Shared expense added successfully!", type = "message", duration = 3) # Changed type
    updateTextInput(session, "shared_description", value = "")
    updateNumericInput(session, "shared_amount", value = 0)
    updateNumericInput(session, "shared_people", value = 2)
    updateTextInput(session, "shared_payer", value = "")
  })
  
  # Clear all shared expenses
  observeEvent(input$clear_shared_data, {
    showModal(modalDialog(
      title = "Confirm Data Deletion",
      "Are you sure you want to delete ALL Shared Expense data? This action cannot be undone.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_clear_shared", "Delete All Shared Expenses", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_clear_shared, {
    removeModal()
    current_expenses <- expenses()
    updated_expenses <- filter(current_expenses, Type != "Shared")
    expenses(updated_expenses)
    save_expenses(updated_expenses)
    showNotification("All shared expense data cleared!", type = "warning", duration = 3)
  })
  
  output$shared_table <- renderDT({
    if ("Type" %in% colnames(expenses()) && nrow(expenses()) > 0) {
      display_data <- filter(expenses(), Type == "Shared") %>% select(Date, Item, Amount, Payer)
      if (nrow(display_data) == 0) {
        return(datatable(data.frame(Date=as.Date(character()), Item=character(),
                                    Amount=numeric(), Payer=character()),
                         options = list(pageLength = 10, autoWidth = TRUE),
                         rownames = FALSE, selection = 'none'))
      }
      datatable(display_data,
                options = list(pageLength = 10, autoWidth = TRUE),
                rownames = FALSE, selection = 'none')
    } else {
      datatable(data.frame(Date=as.Date(character()), Item=character(), Amount=numeric(), Payer=character()),
                options = list(pageLength = 10, autoWidth = TRUE),
                rownames = FALSE, selection = 'none')
    }
  })
  
  output$shared_each <- renderText({
    if ("Type" %in% colnames(expenses()) && nrow(filter(expenses(), Type == "Shared")) > 0) {
      shared_data <- filter(expenses(), Type == "Shared")
      if (nrow(shared_data) > 0) {
        last_expense <- tail(shared_data, 1)
        # Ensure input$shared_people is numeric and not zero
        num_people <- as.numeric(input$shared_people)
        if (!is.na(num_people) && num_people > 0) {
          each_owes <- last_expense$Amount / num_people
          paste("Based on the LAST entry:", format(each_owes, big.mark = ",", scientific = FALSE), "Rs per person")
        } else {
          "Number of people sharing must be a positive number."
        }
      } else {
        "No shared expenses added yet. Enter details above to calculate."
      }
    } else {
      "No shared expenses added yet. Enter details above to calculate."
    }
  })
  
  output$shared_plot <- renderPlot({
    if ("Type" %in% colnames(expenses()) && nrow(filter(expenses(), Type == "Shared")) > 0) {
      shared_data <- filter(expenses(), Type == "Shared") %>% arrange(Date) # Ensure data is ordered by date
      ggplot(shared_data, aes(x = Date, y = Amount, fill = Payer)) +
        geom_bar(stat = "identity", position = "stack") +
        labs(title = "Shared Expenses Over Time", x = "Date", y = "Amount (Rs)") +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
              axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(size = 14, face = "bold"),
              legend.title = element_text(size = 12, face = "bold")) +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %Y")
    } else {
      ggplot() +
        annotate("text", x = 0, y = 0, label = "No shared expense data to show yet.", size = 5, color = "grey50") +
        theme_void()
    }
  })
  
  # --- Contributions Logic ---
  observeEvent(input$add_contribution, {
    req(input$contributor_name, input$contribution_amount)
    
    new_contribution <- data.frame(
      Date = input$contribution_date,
      Item = "Recurring Contribution",
      Category = "Fund",
      Amount = input$contribution_amount,
      Payer = trimws(input$contributor_name),
      Type = "Contribution",
      SuggestionName = NA_character_,
      SuggestionText = NA_character_,
      stringsAsFactors = FALSE
    )
    current_expenses <- expenses()
    updated_expenses <- rbind(current_expenses, new_contribution)
    expenses(updated_expenses)
    save_expenses(updated_expenses) # Save to Google Sheet
    showNotification("Contribution added successfully!", type = "message", duration = 3) # Changed type
    updateTextInput(session, "contributor_name", value = "")
    updateNumericInput(session, "contribution_amount", value = 0)
  })
  
  # Clear all contributions
  observeEvent(input$clear_contributions_data, {
    showModal(modalDialog(
      title = "Confirm Data Deletion",
      "Are you sure you want to delete ALL Contribution data? This action cannot be undone.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_clear_contributions", "Delete All Contributions", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_clear_contributions, {
    removeModal()
    current_expenses <- expenses()
    updated_expenses <- filter(current_expenses, Type != "Contribution")
    expenses(updated_expenses)
    save_expenses(updated_expenses)
    showNotification("All contribution data cleared!", type = "warning", duration = 3)
  })
  
  output$contributions_table <- renderDT({
    if ("Type" %in% colnames(expenses()) && nrow(filter(expenses(), Type == "Contribution")) > 0) {
      datatable(filter(expenses(), Type == "Contribution") %>%
                  select(Date, Payer, Amount) %>%
                  arrange(desc(Date)), # Sort by date descending
                options = list(pageLength = 10, autoWidth = TRUE),
                colnames = c("Date", "Contributor", "Amount (Rs)"),
                rownames = FALSE, selection = 'none')
    } else {
      datatable(data.frame(Date=as.Date(character()), Contributor=character(), `Amount (Rs)`=numeric()),
                options = list(pageLength = 10, autoWidth = TRUE),
                rownames = FALSE, selection = 'none')
    }
  })
  
  # --- Suggestions Logic ---
  observeEvent(input$add_suggestion, {
    req(input$suggestion_text, input$suggestion_category)
    
    new_suggestion <- data.frame(
      Date = Sys.Date(),
      Item = NA_character_, # Not applicable for suggestions
      Category = input$suggestion_category,
      Amount = NA_real_, # Not applicable for suggestions
      Payer = NA_character_, # Not applicable for suggestions
      Type = "Suggestion",
      SuggestionName = ifelse(trimws(input$suggestion_name) == "", "Anonymous", trimws(input$suggestion_name)),
      SuggestionText = trimws(input$suggestion_text),
      stringsAsFactors = FALSE
    )
    current_expenses <- expenses()
    updated_expenses <- rbind(current_expenses, new_suggestion)
    expenses(updated_expenses)
    save_expenses(updated_expenses) # Save to Google Sheet
    showNotification("Suggestion submitted! Thank you!", type = "message", duration = 3) # Changed type
    updateTextInput(session, "suggestion_name", value = "")
    updateTextAreaInput(session, "suggestion_text", value = "")
    updateSelectInput(session, "suggestion_category", selected = "Coffee Beans")
  })
  
  # Clear all suggestions
  observeEvent(input$clear_suggestions_data, {
    showModal(modalDialog(
      title = "Confirm Data Deletion",
      "Are you sure you want to delete ALL Suggestions data? This action cannot be undone.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_clear_suggestions", "Delete All Suggestions", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_clear_suggestions, {
    removeModal()
    current_expenses <- expenses()
    updated_expenses <- filter(current_expenses, Type != "Suggestion")
    expenses(updated_expenses)
    save_expenses(updated_expenses)
    showNotification("All suggestion data cleared!", type = "warning", duration = 3)
  })
  
  output$suggestions_table <- renderDT({
    if ("Type" %in% colnames(expenses()) && nrow(filter(expenses(), Type == "Suggestion")) > 0) {
      datatable(filter(expenses(), Type == "Suggestion") %>%
                  select(Date, SuggestionName, Category, SuggestionText) %>%
                  arrange(desc(Date)), # Sort by date descending
                options = list(pageLength = 10, autoWidth = TRUE,
                               columnDefs = list(list(width = '15%', targets = c(0, 1, 2)))),
                colnames = c("Date", "Suggested By", "Category", "Suggestion"),
                rownames = FALSE, selection = 'none')
    } else {
      datatable(data.frame(Date=as.Date(character()), `Suggested By`=character(),
                           Category=character(), Suggestion=character()),
                options = list(pageLength = 10, autoWidth = TRUE),
                rownames = FALSE, selection = 'none')
    }
  })
  
}

# --- Run the application ---
shinyApp(ui = ui, server = server)
