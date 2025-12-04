library(shiny)

ui <- fluidPage(
  titlePanel("FluentPro Offer Test"),
  
  uiOutput("page_ui")
)

server <- function(input, output, session) {
  
  # Randomly assign variant for this session (A/B)
  variant <- sample(c("A", "B"), size = 1)
  
  # Expose variant in UI
  output$page_ui <- renderUI({
    # Common inputs for both variants
    common_inputs <- tagList(
      textInput("resp_id", "Your respondent ID (e.g., S01, S02, initials):"),
      
      selectInput(
        "segment",
        "Which best describes you today?",
        choices = c(
          "Budget-conscious student",
          "Busy professional",
          "Just browsing / curious"
        )
      ),
      
      sliderInput(
        "intent",
        "How likely are you to sign up for FluentPro based on this page?",
        min = 0, max = 10, value = 5
      ),
      
      actionButton("submit", "Submit my response")
    )
    
    if (variant == "A") {
      # Discount offer
      tagList(
        h2("FluentPro: Learn a new language, faster"),
        h3("Limited-time offer: Save 20% on your first month"),
        p("Get full access to all courses, live practice sessions, and progress tracking."),
        p("Perfect for learners who want maximum value at a lower price."),
        hr(),
        h4("Tell us what you think"),
        common_inputs
      )
    } else {
      # Free-trial offer
      tagList(
        h2("FluentPro: Learn a new language, faster"),
        h3("Start with a 7-day free trial"),
        p("Try all courses, live practice sessions, and progress tracking with no commitment."),
        p("Perfect if you want to experience the product before paying."),
        hr(),
        h4("Tell us what you think"),
        common_inputs
      )
    }
  })
  
  observeEvent(input$submit, {
    # Basic validation: require an ID
    if (is.null(input$resp_id) || input$resp_id == "") {
      showModal(modalDialog(
        title = "Missing ID",
        "Please enter a respondent ID before submitting.",
        easyClose = TRUE
      ))
      return(NULL)
    }
    
    # Create a one-row data frame
    log_row <- data.frame(
      timestamp   = as.character(Sys.time()),
      session_id  = session$token,
      respondent_id = input$resp_id,
      segment     = input$segment,
      variant     = variant,
      intent_0_10 = input$intent,
      stringsAsFactors = FALSE
    )
    
    # Append to CSV on the server
    log_file <- "marketing_ab_log.csv"
    if (!file.exists(log_file)) {
      write.csv(log_row, log_file, row.names = FALSE)
    } else {
      write.table(
        log_row, log_file,
        sep = ",", row.names = FALSE,
        col.names = FALSE, append = TRUE
      )
    }
    
    showModal(modalDialog(
      title = "Thank you!",
      "Your response has been recorded. You can now close this tab.",
      easyClose = TRUE
    ))
  })
}

shinyApp(ui, server)
