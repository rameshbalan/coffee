library(shiny)
library(DT)
library(ggplot2)
library(shinythemes)
library(dplyr) # Explicitly use dplyr for filter for clarity and robustness

# --- UI (User Interface) ---
ui <- fluidPage(
  theme = shinytheme("paper"), # Apply a shinytheme
  
  # Custom CSS for spacing and centering
  tags$head(
    tags$style(HTML("
      body { padding-top: 20px; }
      .container-fluid { max-width: 1200px; margin: auto; }
      .well { background-color: #f8f8f8; border: 1px solid #e7e7e7; border-radius: 4px; padding: 20px; }
      h1, h2, h3, h4 { text-align: center; color: #337ab7; margin-bottom: 25px; }
      .tab-content { padding-top: 20px; }
      .dataTables_wrapper .dt-buttons { margin-bottom: 10px; }
      .shiny-notification { position:fixed; top: calc(50%); left: calc(50%); transform: translate(-50%, -50%); }
      .founder-list { list-style-type: none; padding: 0; text-align: center; }
      .founder-list li { margin-bottom: 5px; font-size: 1.1em; color: #555; }
    "))
  ),
  
  titlePanel("☕ BrewBudget"),
  
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
                   textInput("running_item", "Item/Description:", "", placeholder = "e.g., Coffee Beans, Milk"),
                   selectInput("running_category", "Category:",
                               choices = c("Coffee Beans", "Milk/Dairy", "Sweeteners", "Disposables (Cups, Stirrers)", "Equipment", "Other")),
                   numericInput("running_amount", "Amount (Rs):", value = 0, min = 0, step = 0.01),
                   textInput("running_payer", "Bought By:", "", placeholder = "e.g., John Doe"),
                   actionButton("add_running", "Add Running Cost", class = "btn-success btn-block")
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
                     column(6, tags$h4(textOutput("running_total"), style = "color: #5cb85c;")), # Green text for total
                     column(6, tags$h4(textOutput("running_remaining"), style = "color: #d9534f;")) # Red text for remaining
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
                   textInput("shared_description", "Description:", "", placeholder = "e.g., New Kettle, Party Supplies"),
                   numericInput("shared_amount", "Total Amount (Rs):", value = 0, min = 0, step = 0.01),
                   numericInput("shared_people", "Number of People Sharing:", value = 2, min = 2, step = 1),
                   textInput("shared_payer", "Paid By:", "", placeholder = "e.g., Jane Smith"),
                   actionButton("add_shared", "Add Shared Expense", class = "btn-success btn-block")
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
    # --- New Tab for Contributions ---
    tabPanel("Contributions",
             icon = icon("hand-holding-usd"), # A hand-holding-usd icon for money collected
             br(),
             sidebarLayout(
               sidebarPanel(
                 wellPanel(
                   h3("Record New Contribution"),
                   dateInput("contribution_date", "Date:", value = Sys.Date(), width = '100%'),
                   textInput("contributor_name", "Contributor's Name:", "", placeholder = "e.g., Alice"),
                   numericInput("contribution_amount", "Amount Contributed (Rs):", value = 0, min = 0, step = 0.01),
                   actionButton("add_contribution", "Add Contribution", class = "btn-primary btn-block") # Primary blue button
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
                       tags$li(icon("mug-hot"), " Runa - The Condiment Commander")
               ),
               p("Thanks for bean-g amazing!", style = "text-align: center; font-style: italic; margin-top: 30px;")
             )
    ),
    # --- New Tab for Suggestions ---
    tabPanel("Suggestions Box",
             icon = icon("lightbulb"), # A lightbulb icon for ideas
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
                   actionButton("add_suggestion", "Submit Suggestion", class = "btn-info btn-block") # Blue, block-level button
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
  
  # Reactive value to store all data. Initialize with all expected columns including for suggestions.
  expenses <- reactiveVal(data.frame(
    Date = as.Date(character()),
    Item = character(),
    Category = character(),
    Amount = numeric(),
    Payer = character(),
    Type = character(), # "Running", "Shared", "Contribution", "Suggestion"
    # Additional columns for suggestions, will be NA for other types
    SuggestionName = character(),
    SuggestionText = character(),
    stringsAsFactors = FALSE
  ))
  
  # File path for storing expenses
  expenses_file <- "expenses.csv"
  
  # Load existing data from CSV when the app starts
  observe({
    if (file.exists(expenses_file)) {
      loaded_expenses <- read.csv(expenses_file, stringsAsFactors = FALSE)
      loaded_expenses$Date <- as.Date(loaded_expenses$Date)
      
      # Define all expected columns for the consolidated dataframe
      expected_cols <- c("Date", "Item", "Category", "Amount", "Payer", "Type",
                         "SuggestionName", "SuggestionText")
      
      # Add missing columns with appropriate default NA values
      for (col in setdiff(expected_cols, colnames(loaded_expenses))) {
        if (col %in% c("Date")) {
          loaded_expenses[[col]] <- as.Date(NA)
        } else if (col %in% c("Amount")) {
          loaded_expenses[[col]] <- NA_real_
        } else { # For character columns
          loaded_expenses[[col]] <- NA_character_
        }
      }
      
      # For older files without 'Type', assume they were 'Running' costs initially
      if ("Type" %in% colnames(loaded_expenses) && any(is.na(loaded_expenses$Type))) {
        loaded_expenses$Type[is.na(loaded_expenses$Type)] <- "Running"
      } else if (!("Type" %in% colnames(loaded_expenses))) {
        loaded_expenses$Type <- "Running" # Fallback if Type column was completely missing
      }
      
      # Ensure all rows have a non-NA Type
      loaded_expenses$Type[is.na(loaded_expenses$Type)] <- "Running" # Default to 'Running' for any remaining NA Types
      
      # Reorder columns to match the desired order
      loaded_expenses <- loaded_expenses[expected_cols]
      
      expenses(loaded_expenses)
    }
  })
  
  # Function to save all data to CSV
  save_expenses <- function(data) {
    write.csv(data, expenses_file, row.names = FALSE)
  }
  
  # --- Running Costs ---
  observeEvent(input$add_running, {
    req(input$running_item, input$running_amount, input$running_payer)
    
    new_expense <- data.frame(
      Date = input$running_date,
      Item = input$running_item,
      Category = input$running_category,
      Amount = input$running_amount,
      Payer = input$running_payer,
      Type = "Running",
      SuggestionName = NA_character_, # NA for non-suggestion types
      SuggestionText = NA_character_, # NA for non-suggestion types
      stringsAsFactors = FALSE
    )
    current_expenses <- expenses()
    updated_expenses <- rbind(current_expenses, new_expense)
    expenses(updated_expenses)
    save_expenses(updated_expenses)
    showNotification("Running cost added successfully!", type = "message", duration = 3)
    updateTextInput(session, "running_item", value = "")
    updateNumericInput(session, "running_amount", value = 0)
    updateTextInput(session, "running_payer", value = "")
  })
  
  # Render the interactive running expenses table
  output$running_table <- renderDT({
    if ("Type" %in% colnames(expenses()) && nrow(expenses()) > 0) {
      datatable(filter(expenses(), Type == "Running") %>% select(Date, Item, Category, Amount, Payer),
                options = list(pageLength = 10, autoWidth = TRUE,
                               columnDefs = list(list(width = '10%', targets = c(0, 3)))),
                rownames = FALSE, selection = 'none')
    } else {
      datatable(data.frame(Date=as.Date(character()), Item=character(), Category=character(),
                           Amount=numeric(), Payer=character()),
                options = list(pageLength = 10, autoWidth = TRUE),
                rownames = FALSE, selection = 'none')
    }
  })
  
  
  # Calculate and display total running expenses
  output$running_total <- renderText({
    if ("Type" %in% colnames(expenses())) {
      total <- sum(filter(expenses(), Type == "Running")$Amount)
      paste("Total Running Costs:", format(total, big.mark = ",", scientific = FALSE), "Rs")
    } else {
      "Total Running Costs: N/A"
    }
  })
  
  # Calculate and display remaining budget for running costs (UPDATED LOGIC)
  output$running_remaining <- renderText({
    if ("Type" %in% colnames(expenses())) {
      total_running_expenses <- sum(filter(expenses(), Type == "Running")$Amount)
      total_contributions <- sum(filter(expenses(), Type == "Contribution")$Amount)
      
      remaining <- total_contributions - total_running_expenses
      paste("Remaining Budget:", format(remaining, big.mark = ",", scientific = FALSE), "Rs")
    } else {
      "Remaining Budget: N/A (Add contributions to see budget)"
    }
  })
  
  # Plot running expenses by category (Pie Chart)
  output$running_category_plot <- renderPlot({
    if ("Type" %in% colnames(expenses()) && nrow(filter(expenses(), Type == "Running")) > 0) {
      running_data <- filter(expenses(), Type == "Running")
      category_summary <- aggregate(Amount ~ Category, data = running_data, sum)
      ggplot(category_summary, aes(x = "", y = Amount, fill = Category)) +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y", start = 0) +
        theme_void() +
        labs(title = "Running Costs by Category") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) +
        guides(fill = guide_legend(title = "Category"))
    } else {
      ggplot() +
        annotate("text", x = 0, y = 0, label = "No running cost data to show for categories yet.", size = 5, color = "grey50") +
        theme_void()
    }
  })
  
  # Plot running expenses by payer (Bar Chart)
  output$running_payer_plot <- renderPlot({
    if ("Type" %in% colnames(expenses()) && nrow(filter(expenses(), Type == "Running")) > 0) {
      running_data <- filter(expenses(), Type == "Running")
      payer_summary <- aggregate(Amount ~ Payer, data = running_data, sum)
      ggplot(payer_summary, aes(x = reorder(Payer, -Amount), y = Amount, fill = Payer)) +
        geom_bar(stat = "identity", fill = "#5cb85c") +
        labs(title = "Running Costs by Payer", x = "Payer", y = "Amount (Rs)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
              axis.text.y = element_text(size = 12),
              axis.title = element_text(size = 14, face = "bold"),
              plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
              legend.position = "none")
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
      Item = input$shared_description,
      Category = "Shared Expense",
      Amount = input$shared_amount,
      Payer = input$shared_payer,
      Type = "Shared",
      SuggestionName = NA_character_,
      SuggestionText = NA_character_,
      stringsAsFactors = FALSE
    )
    current_expenses <- expenses()
    updated_expenses <- rbind(current_expenses, new_expense)
    expenses(updated_expenses)
    save_expenses(updated_expenses)
    showNotification("Shared expense added successfully!", type = "message", duration = 3)
    updateTextInput(session, "shared_description", value = "")
    updateNumericInput(session, "shared_amount", value = 0)
    updateNumericInput(session, "shared_people", value = 2)
    updateTextInput(session, "shared_payer", value = "")
  })
  
  # Render the interactive shared expenses table
  output$shared_table <- renderDT({
    if ("Type" %in% colnames(expenses()) && nrow(expenses()) > 0) {
      datatable(filter(expenses(), Type == "Shared") %>% select(Date, Item, Amount, Payer),
                options = list(pageLength = 10, autoWidth = TRUE),
                rownames = FALSE, selection = 'none')
    } else {
      datatable(data.frame(Date=as.Date(character()), Item=character(), Amount=numeric(), Payer=character()),
                options = list(pageLength = 10, autoWidth = TRUE),
                rownames = FALSE, selection = 'none')
    }
  })
  
  
  # Calculate and display amount each person owes for the LAST shared expense added
  output$shared_each <- renderText({
    if ("Type" %in% colnames(expenses()) && nrow(filter(expenses(), Type == "Shared")) > 0) {
      shared_data <- filter(expenses(), Type == "Shared")
      last_expense <- tail(shared_data, 1)
      each_owes <- last_expense$Amount / input$shared_people
      paste("Based on the LAST entry:", format(each_owes, big.mark = ",", scientific = FALSE), "Rs per person")
    } else {
      "No shared expenses added yet. Enter details above to calculate."
    }
  })
  
  # Plot shared expenses over time (Bar Chart)
  output$shared_plot <- renderPlot({
    if ("Type" %in% colnames(expenses()) && nrow(filter(expenses(), Type == "Shared")) > 0) {
      shared_data <- filter(expenses(), Type == "Shared")
      shared_data <- shared_data[order(shared_data$Date), ]
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
  
  # --- Contributions Logic (NEW) ---
  observeEvent(input$add_contribution, {
    req(input$contributor_name, input$contribution_amount)
    
    new_contribution <- data.frame(
      Date = input$contribution_date,
      Item = "Recurring Contribution", # Fixed item description
      Category = "Fund", # Fixed category for contributions
      Amount = input$contribution_amount,
      Payer = input$contributor_name,
      Type = "Contribution",
      SuggestionName = NA_character_,
      SuggestionText = NA_character_,
      stringsAsFactors = FALSE
    )
    current_expenses <- expenses()
    updated_expenses <- rbind(current_expenses, new_contribution)
    expenses(updated_expenses)
    save_expenses(updated_expenses)
    showNotification("Contribution added successfully!", type = "message", duration = 3)
    updateTextInput(session, "contributor_name", value = "")
    updateNumericInput(session, "contribution_amount", value = 0)
  })
  
  # Render the contributions table (NEW)
  output$contributions_table <- renderDT({
    if ("Type" %in% colnames(expenses()) && nrow(filter(expenses(), Type == "Contribution")) > 0) {
      datatable(filter(expenses(), Type == "Contribution") %>%
                  select(Date, Payer, Amount), # Select relevant columns
                options = list(pageLength = 10, autoWidth = TRUE),
                colnames = c("Date", "Contributor", "Amount (Rs)"), # Rename columns for display
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
      Item = NA_character_,
      Category = input$suggestion_category,
      Amount = NA_real_,
      Payer = NA_character_,
      Type = "Suggestion",
      SuggestionName = ifelse(input$suggestion_name == "", "Anonymous", input$suggestion_name),
      SuggestionText = input$suggestion_text,
      stringsAsFactors = FALSE
    )
    current_expenses <- expenses()
    updated_expenses <- rbind(current_expenses, new_suggestion)
    expenses(updated_expenses)
    save_expenses(updated_expenses)
    showNotification("Suggestion submitted! Thank you!", type = "message", duration = 3)
    updateTextInput(session, "suggestion_name", value = "")
    updateTextAreaInput(session, "suggestion_text", value = "")
    updateSelectInput(session, "suggestion_category", selected = "Coffee Beans")
  })
  
  # Render the suggestions table
  output$suggestions_table <- renderDT({
    if ("Type" %in% colnames(expenses()) && nrow(filter(expenses(), Type == "Suggestion")) > 0) {
      datatable(filter(expenses(), Type == "Suggestion") %>%
                  select(Date, SuggestionName, Category, SuggestionText) %>%
                  arrange(desc(Date)),
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
