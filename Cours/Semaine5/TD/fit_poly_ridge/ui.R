shinyUI(fluidPage(
  fluidRow(
    column(2,
           br(),
           selectInput("degree", label = "degree",  choices = (0:20), selected = 2),
           br(),
           radioButtons("opf", "time interval", c("observed" = FALSE,"extended" = TRUE)),
           br(),
            radioButtons("widget", "widget", c("Slider" = "slider","List"="list"),selected="slider"),
            br(),
conditionalPanel(condition = "input.widget == 'list'",
                 selectInput("lambda1", label = "lambda",  choices = c(0,0.001,0.01,0.1,1,10,100,1000), selected = 0)),
conditionalPanel(condition = "input.widget == 'slider'",
                 sliderInput("lambda2", "lambda:", value=0., min=0, max = 1, step=0.01))
    ),
    column(8,
           plotOutput("plot1")
    ),
    column(2,
           br(),
           tableOutput("tables"),
           br(),
           tableOutput("tabler")
    )
  )
))

