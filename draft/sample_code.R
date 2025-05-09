library(shiny)
library(httr)
library(jsonlite)

ui <- fluidPage(
  titlePanel("Natural Language to ggplot2 (powered by Ollama)"),
  
  sidebarLayout(
    sidebarPanel(
      textAreaInput("prompt", "Enter your plot request:", 
                    value = "Create a scatter plot of mpg vs hp from mtcars, colored by cyl", 
                    rows = 4),
      actionButton("generate", "Generate Plot")
    ),
    
    mainPanel(
      verbatimTextOutput("code_output"),
      plotOutput("plot_output")
    )
  )
)

# Server function (updated with better handling)
server <- function(input, output, session) {
  
  generated_code <- eventReactive(input$generate, {
    req(input$prompt)
    
    # Send request to Ollama API
    response <- tryCatch({
      POST(
        url = "http://localhost:11434/api/generate",
        body = list(
          model = "mistral",
          prompt = paste(
            "Write only the R ggplot2 code. No explanations. Do not include Markdown Symbols. Use mtcars dataset if needed.\n",
            "Prompt: ", input$prompt
          ),
          stream = FALSE
        ),
        encode = "json"
      )
    }, error = function(e) {
      return(list(error = TRUE, message = e$message))
    })
    
    if (!is.null(response$error)) {
      return("Error: Could not reach Ollama.")
    }
    
    # Parse response content
    res_content <- content(response, as = "parsed", simplifyVector = TRUE)
    raw_code <- res_content$response
    
    # 🧽 Strip out markdown fences, backticks, and spaces
    code_clean <- gsub("(?s)```.*?```", "", raw_code, perl = TRUE)
    code_clean <- gsub("^```r\\s*|\\s*```$", "", raw_code)
    code_clean <- trimws(code_clean)  # Remove surrounding whitespace
    
    if (nchar(code_clean) == 0) {
      return("Error: Generated code is empty or invalid.")
    }
    
    return(code_clean)
  })
  
  # Display the generated code for debugging
  output$code_output <- renderText({
    generated_code()
  })
  
  # Plot the generated code (if valid)
  output$plot_output <- renderPlot({
    code <- generated_code()
    
    if (!is.null(code) && nzchar(code)) {
      tryCatch({
        code_clean <- gsub("[\u0060]+", "", code)  # remove backticks
        code_clean <- str_replace(code_clean,"R\\n","")
        eval(parse(text = code_clean))
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Error in generated code:\n", e$message), cex = 1.2)
      })
    }
    
  })
}

shinyApp(ui, server)