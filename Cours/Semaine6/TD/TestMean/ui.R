#-------------------------------------------------------------------------
#  This application is governed by the CeCILL-B license. 
#  You can  use, modify and/ or redistribute this code under the terms
#  of the CeCILL license:  http://www.cecill.info/index.en.html
#
#  Marc Lavielle, Inria Saclay
#  April 29th, 2015
#-------------------------------------------------------------------------

shinyUI(fluidPage(
  #   titlePanel(
  #     list(HTML('<p style="color:#4C0B5F; font-size:24px" fontsize=14>Survival model</p>' )),
  #     windowTitle="Survival model"),
  
  navbarPage("", selected="Plot", id="nav", inverse="FALSE", 
             title="Hypothesis testing",
             tabPanel("Plot",                      
                      fluidRow(
                        column(4,
                               fluidRow(
                                 column(6,
                                        radioButtons("xaxis",label = "x-axis",
                                                     c("n"="1","sigma"="2","mu"="3","level"="4","power"="5","critical value"="6"),
                                                     selected="3")
                                 ),
                                 column(6,
                                        radioButtons("yaxis",label = "y-axis",
                                                     c("n"="1","sigma"="2","mu"="3","level"="4","power"="5","critical value"="6"),
                                                     selected="5")
                                 )
                               ),
                               conditionalPanel(condition = "input.xaxis != 1 && input.yaxis != 1 ",
                                                sliderInput("n", label = "n", value=10, min=1, max=100)
                               ), 
                               conditionalPanel(condition = "input.xaxis != 2 && input.yaxis != 2 ",
                                                sliderInput("sigma", label="sigma", value=2, min=0.2, max=10, step=0.2)
                               ), 
                               conditionalPanel(condition = "input.xaxis != 3 && input.yaxis != 3 ",
                                                sliderInput("mu", label="mu", value=1, min=0, max=5, step=0.1)
                               ), 
                               conditionalPanel(condition = "input.xaxis != 4 && input.yaxis != 4 ",
                                                conditionalPanel(condition = "input.xaxis!=5 || input.yaxis!=6",
                                                                 conditionalPanel(condition = "input.xaxis!=6 || input.yaxis!=5",
                                                                                  sliderInput("level", label="level", value=0.05, min=0.01, max=0.5, step=0.01)
                                                                 ))), 
                               conditionalPanel(condition = "input.xaxis!=5 && input.yaxis!=5", 
                                                conditionalPanel(condition = "input.xaxis!=6 && input.yaxis!=6",                                                                  
                                                                 #                                conditionalPanel(condition = "input.xaxis!=4 || input.yaxis!=6",
                                                                 #                                conditionalPanel(condition = "input.xaxis!=6 || input.yaxis!=4",
                                                                 sliderInput("power", label="power", value=0.8, min=0.01, max=0.99, step=0.01)
                                                )),
                               # radioButtons("test",label = h5("test"),c("one-sided"="1","two-sided"="2"), inline=TRUE),
                               radioButtons("test",label = "test",c("one-sided"="1","two-sided"="2"), inline=TRUE),
                               checkboxGroupInput("sigmaest", label="sigma", choices=c("Known"=1,"Estimated"=2), selected = c(1), inline = TRUE),       
                               radioButtons("log",label = "y-scale",c("linear"="1","log"="2"), inline=TRUE)
                        ),
                        column(8,
                               #                                plotOutput("plot",  height="500px", hover=hoverOpts(id="plot_hover"))
                               plotOutput("plot",  height="500px")
                        )
                      )
                      #                        verbatimTextOutput("hover_info")
                      
             ),
             tabPanel("ReadMe", withMathJax(), includeMarkdown("readMe.Rmd"))
  )   
))