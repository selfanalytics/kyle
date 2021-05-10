#load libraries
library(shiny)
library(googlesheets4)

table <- "responses"



fieldsAll <- c("day", "diet","hobby1","hobby2","people", "drugs","workout", "sleep", "career","people","before_bed")

# which fields are mandatory
fieldsMandatory <- c("name", "favourite_pkg")

# add an asterisk to an input label
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# get current Epoch time
epochTime <- function() {
  return(as.integer(Sys.time()))
}

# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}

# save the results to a file
saveData <- function(data) {
  # The data must be a dataframe rather than a named vector
  data <- data %>% as.list() %>% data.frame()
  # Add the data as a new row
  sheet_append("1b6_XDzUveFNdZvhEM0hAlGM7BfFT-Eq1VzXagAFwP_I", data)
}


# load all responses into a data.frame
loadData <- function() {
  # Read the data
  read_sheet("1b6_XDzUveFNdZvhEM0hAlGM7BfFT-Eq1VzXagAFwP_I")
}

# directory where responses get stored
responsesDir <- getwd()

# CSS to use in the app
appCSS <-
  ".mandatory_star { color: red; }
   .shiny-input-container { margin-top: 25px; }
   #submit_msg { margin-left: 15px; }
   #error { color: red; }
   body { background: #fcfcfc; }
   #header { background: #fff; border-bottom: 1px solid #ddd; margin: -20px -15px 0; padding: 15px 15px 10px; }
  "

# usernames that are admins
adminUsers <- c("admin", "prof")


shinyApp(
  ui = fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    title = "Personal Development Nightly Survey",
    div(id = "header",
        h1("Personal Development Nightly Survey")
    ),
    
    fluidRow(
      column(6,
             div(
               id = "form",
               
               dateInput("date","What is the date?"),
               
               checkboxInput("Work day", "Did you work?", FALSE),
               sliderInput("career", "If so, hoow fulfilled did you feel in your work?", -2, 2, 0, ticks = FALSE),
               
               
               sliderInput("day", "How was your day, mood and mindset?", -2, 2, 0, ticks = FALSE),
               sliderInput("sleep", "How many hours of sleep did you get?", 0, 10, 7,step = 0.25, ticks = FALSE),
               sliderInput("diet", "How was your diet?", -1, 1, 0,ticks = FALSE),
               sliderInput("coffee", "How many coffees?", 0, 5, 0,ticks = FALSE),
               sliderInput("drinks", "How many drinks?", 0, 5, 0,ticks = FALSE),
               sliderInput("water ", "How many cups of water?", 0, 10, 0, ticks = FALSE),
               sliderInput("family", "Did you make an effort to spend quality time with the people you care about?", -2, 2, 0, ticks = FALSE),
               sliderInput("emotions", "How well did you control your emotions?", -2, 2, 0, ticks = FALSE),
               sliderInput("people", "How were your interactions with people?", -2, 2, 0, ticks = FALSE),
               checkboxInput("", "Did you make an effort to meet a new person?", FALSE),
               checkboxInput("", "Did you spend time reading?", FALSE),
               checkboxInput("mediatation", "Did you mediatate?", FALSE),
               checkboxInput("drugs", "Did you get high?", FALSE),
               checkboxInput("learning", "Did you learn something new?", FALSE),
               checkboxInput("independence", "Where you independent instead of relying on others?", FALSE),
               checkboxGroupInput("hobby", "Which of the following did you engage in?",
                                  c("Weights","Cardio","Skiing","Cycling","Hiking",
                                    "Camping","Golf","Walk")),
               selectInput("before_bed", "Activity before bed",
                           c("",  "Read", "TV", "Social Media","Sex","Other")),
               actionButton("submit", "Submit", class = "btn-primary"),
               
               shinyjs::hidden(
                 span(id = "submit_msg", "Submitting..."),
                 div(id = "error",
                     div(br(), tags$b("Error: "), span(id = "error_msg"))
                 )
               )
             ),
             
             shinyjs::hidden(
               div(
                 id = "thankyou_msg",
                 h3("Thanks, your response was submitted successfully!"),
                 actionLink("submit_another", "Submit another response")
               )
             )
      )
    )
  ),
  server = function(input, output, session) {
    
    
    # Gather all the form inputs (and add timestamp)
    formData <- reactive({
      data <- sapply(fieldsAll, function(x) input[[x]])
      data <- c(data, timestamp = epochTime())
      data <- t(data)
      data
    })    
    
    # When the Submit button is clicked, submit the response
    observeEvent(input$submit, {
      
      # User-experience stuff
      shinyjs::disable("submit")
      shinyjs::show("submit_msg")
      shinyjs::hide("error")
      
      # Save the data (show an error message in case of error)
      tryCatch({
        saveData(formData())
        shinyjs::reset("form")
        shinyjs::hide("form")
        shinyjs::show("thankyou_msg")
      },
      error = function(err) {
        shinyjs::html("error_msg", err$message)
        shinyjs::show(id = "error", anim = TRUE, animType = "fade")
      },
      finally = {
        shinyjs::enable("submit")
        shinyjs::hide("submit_msg")
      })
    })
    
    # submit another response
    observeEvent(input$submit_another, {
      shinyjs::show("form")
      shinyjs::hide("thankyou_msg")
    })
    
  }
)