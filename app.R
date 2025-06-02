library(shiny)
library(DT) # For interactive tables
library(ggplot2) # For plotting

# --- UI (User Interface) ---
ui <- fluidPage(
  titlePanel(h1("Coffee Fund Expense Tracker")),

  sidebarLayout(
    sidebarPanel(
      h3("Add New Expense"),
      dateInput("expense_date", "Date:", value = Sys.Date()),
      textInput("item_description", "Item/Description:", ""),
      selectInput("category", "Category:",
                  choices = c("Coffee Beans", "Milk/Dairy", "Sweeteners", "Disposables (Cups, Stirrers)", "Equipment", "Other")),
      numericInput("amount", "Amount (Rs):", value = 0, min = 0, step = 0.01),
      # Changed from selectInput to textInput for Payer
      textInput("payer_name", "Bought By:", ""),
      actionButton("add_expense", "Add Expense", class = "btn-primary")
    ),

    mainPanel(
      h3("Expense List"),
      DTOutput("expense_table"),
      hr(),
      h3("Expense Summary"),
      fluidRow(
        column(6, textOutput("total_expenses")),
        column(6, textOutput("remaining_budget"))
      ),
      hr(),
      fluidRow(
        column(6, plotOutput("category_plot")),
        column(6, plotOutput("payer_plot"))
      )
    )
  )
)

# --- Server Logic ---
server <- function(input, output, session) {

  # Reactive value to store expenses. This is where your data lives.
  expenses <- reactiveVal(data.frame(
    Date = as.Date(character()),
    Item = character(),
    Category = character(),
    Amount = numeric(),
    Payer = character(),
    stringsAsFactors = FALSE
  ))

  # File path for storing expenses
  expenses_file <- "expenses.csv"

  # Load existing expenses from CSV when the app starts
  observe({
    if (file.exists(expenses_file)) {
      loaded_expenses <- read.csv(expenses_file, stringsAsFactors = FALSE)
      loaded_expenses$Date <- as.Date(loaded_expenses$Date) # Ensure Date column is proper date format
      expenses(loaded_expenses)
    }
  })

  # Function to save expenses to CSV
  save_expenses <- function(data) {
    write.csv(data, expenses_file, row.names = FALSE)
  }

  # Add new expense when 'Add Expense' button is clicked
  observeEvent(input$add_expense, {
    # Ensure essential fields are filled before adding
    req(input$item_description, input$amount, input$payer_name) # payer_name is now text, so ensure it's not empty

    new_expense <- data.frame(
      Date = input$expense_date,
      Item = input$item_description,
      Category = input$category,
      Amount = input$amount,
      Payer = input$payer_name, # Directly use text input for Payer
      stringsAsFactors = FALSE
    )
    current_expenses <- expenses()
    updated_expenses <- rbind(current_expenses, new_expense)
    expenses(updated_expenses)

    # Save to CSV immediately
    save_expenses(updated_expenses)

    # Clear input fields after adding
    updateTextInput(session, "item_description", value = "")
    updateNumericInput(session, "amount", value = 0)
    updateTextInput(session, "payer_name", value = "") # Clear payer text input
  })

  # Render the interactive expense table
  output$expense_table <- renderDT({
    datatable(expenses(),
              options = list(pageLength = 10,
                             autoWidth = TRUE,
                             columnDefs = list(list(width = '10%', targets = c(0, 3)))), # Adjust column widths if needed
              rownames = FALSE, # Hide row numbers
              selection = 'none' # No row selection
              )
  })

  # Calculate and display total expenses
  output$total_expenses <- renderText({
    total <- sum(expenses()$Amount)
    paste("Total Expenses:", format(total, big.mark = ",", scientific = FALSE), "Rs")
  })

  # Calculate and display remaining budget
  output$remaining_budget <- renderText({
    collected_amount <- 600 # Your fixed collected amount
    total_spent <- sum(expenses()$Amount)
    remaining <- collected_amount - total_spent
    paste("Remaining Budget:", format(remaining, big.mark = ",", scientific = FALSE), "Rs")
  })

  # Plot expenses by category (Pie Chart)
  output$category_plot <- renderPlot({
    if (nrow(expenses()) > 0) {
      category_summary <- aggregate(Amount ~ Category, data = expenses(), sum)
      ggplot(category_summary, aes(x = "", y = Amount, fill = Category)) +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y", start = 0) +
        theme_void() +
        labs(title = "Expenses by Category") +
        theme(plot.title = element_text(hjust = 0.5, face = "bold")) # Center and bold title
    } else {
      # Display a message if no data to plot
      ggplot() +
        annotate("text", x = 0, y = 0, label = "No data to show for categories yet.", size = 5, color = "grey50") +
        theme_void()
    }
  })

  # Plot expenses by payer (Bar Chart)
  output$payer_plot <- renderPlot({
    if (nrow(expenses()) > 0) {
      payer_summary <- aggregate(Amount ~ Payer, data = expenses(), sum)
      ggplot(payer_summary, aes(x = reorder(Payer, -Amount), y = Amount, fill = Payer)) +
        geom_bar(stat = "identity") +
        labs(title = "Expenses by Payer", x = "Payer", y = "Amount (Rs)") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels
              plot.title = element_text(hjust = 0.5, face = "bold"), # Center and bold title
              legend.position = "none") # Hide legend as fill is Payer
    } else {
      # Display a message if no data to plot
      ggplot() +
        annotate("text", x = 0, y = 0, label = "No data to show for payers yet.", size = 5, color = "grey50") +
        theme_void()
    }
  })
}

# --- Run the application ---
shinyApp(ui = ui, server = server)
