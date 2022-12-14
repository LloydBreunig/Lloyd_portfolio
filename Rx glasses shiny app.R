# Read Me
## This R Shiny app calculates the thickness of your Rx glasses.
## Cheaper lenses are thicker but have less chromatic aberration. 
## This color spiting at the peripheral of the lens like a prism is especially pronounced with LED read outs. 
## This aberration harms the resolution and usability of the lenses at the edges.
## Cheap lenses are always thicker which is why expensive lenses are often 
## encouraged by sales associates. However, if the lenses are about the 
## thickness as the frame or less, then the thickness does not matter.
## To the surprise of both me and my eye doctor, the cheap lenses do not 
## protrude past my frames. This saved me $150 and provided better vision quality.


# Load packages
library(shiny)
library(shinythemes)


## ui sets up the user interface with the data inputs the user needs to provide.
ui <- fluidPage(
  # set app's formatting theme i.e. fonts & colors
  theme = shinytheme("superhero"),
  fluidRow(
    
    # header built & organized
    column(11,
           tags$h1("Rx Lens ", tags$b("Thickness"), " Calculator"),
           tags$h6("by Lloyd Breunig")
    ),
    column(1,
                 tags$img(height = 50,
                          width = 50,
                          src = "https://yt3.ggpht.com/yti/APfAmoGJlkDeD3Oj0E6ijSMlB-XucHF_wc32GY-r8Bl8qA=s88-c-k-c0x00ffffff-no-rj-mo")
    ),
  ), 
  
  
  # horizontal rule (line)
  tags$hr(),
  
  # User data input and organization
  sidebarLayout(
    sidebarPanel(
      tags$h4("Parameters from your prescription."),
      numericInput(inputId = "Rx", 
                   label = tags$h5("Rx Sphere value"), 
                   value = -4.75),
      numericInput(inputId = "IP", 
                   label = tags$h5("IP (distance between pupils)"), 
                   value = 62),
      tags$hr(),
      tags$h4("Parameters from your frames."),
      numericInput(inputId = "LW", 
                   label = tags$h5("Lense width (Frame size ??-##-###)"), 
                   value = 48),
      numericInput(inputId = "BW", 
                   label = tags$h5("Bridge width (Frame size ##-??-###)"), 
                   value = 22),
      tags$hr(),
      numericInput(inputId = "Index", 
                   label = tags$h5("Index value from your chosen lens materia"), 
                   value = 1.50),
      width = 5),
    
    
    # graph placement and sizing
    mainPanel(
      plotOutput(outputId = "lensThicknessPlot"),
      width = 7,

      tags$h4("If lenses are too thick, try higher index lenses or frames with smaller lenses."),
      tags$h6('1.50 - Standard Index(+$0)'),
      tags$h6('1.61 - High Index (+$80)'),
      tags$h6('1.67 - High Index (+$100)'),
      tags$h6('1.74 - High Index (+$150)'),
      tags$h4("However, higher index lenses have worse chromatic aberation."),
      tags$img(height = 250,
               width = 260,
               src = "https://john-chapman.github.io/images/pseudo-lens-flare/chromatic_aberration.png"),
      ## image from https://john-chapman.github.io/2017/11/05/pseudo-lens-flare.html
      tags$img(height = 250,
               width = 260,
               src = "https://www.handprint.com/ASTRO/IMG/seidel1.gif")
      ## image from https://www.handprint.com/ASTRO/ae4.html
      
    )
  )  
)

## server provides the computer with instructions on how to process the input
## data from the ui and how to display the new data as tables & visualizations
## output.
server <- function(input, output) {
  
  ## This sets up 'lenseRange' as a function similar to how a variable in algebra would be used.
  ## This value is the plus and minus distance from your pupil to the edge of the lens.
  ## the pupil is the thinnest part of the lens.
  lenseRange <- reactive({seq.int(-((input$IP - input$BW)/2), (input$BW/2)+input$LW-(input$IP/2))})

  ## Graph
  output$lensThicknessPlot <- renderPlot({
    title <- "Lens Crosssectinoal Thickness"
    plot(
      # X-axis (seq.int sets the range which is the lens width where 0 is pupal)
      lenseRange(), 
      # Y-axis (uses the range from x-axis in place of a simple 'x' variable)
      ## Next line of code looks good but the input$index is not working in the UI section above so '1.6' is hard coded.
      ## the leseRange is divided by 1 and not 2 like the equations state since the lenseRange is ~1/2 the lense width
      abs(lenseRange()/1)^2*abs(input$Rx)/2000/(input$Index-1)+1.5,
      # label axis.
      xlab = "Lens width from pupil (mm)",
      ylab = "Lens thickness (mm)",
      # have a title defined above   
      main = title)
  })
}

# notes one the algibra formula.
# insideEdgeDist = (input$IP - input$BW)/2
# outsideEdgeDist = (input$BW/2)+input$LW-(input$IP/2)
# frameSizeXaxis = LW
# frameSizeYaxis = FT
# edgeThickness = centerThickness - (frontSagitta + backSagitta)
# maxThickness = minThickness+(diameter/2)^2*ABS(focalPower)/2000/(index-1)
# maxThickness = minThickness+(diameter/2)^2*ABS(Rx)/2000/(index-1)
# minThickness = 1


## Creat Shiny object with ui & server sections.
shinyApp(ui = ui, server = server)


# end of code


## reference section
# R Studio has an great tutorial video and cheat sheet for Shiny
### https://shiny.rstudio.com/tutorial/
### https://rstudio.cloud/learn/cheat-sheets
# Startpage.com is a great search engine for math related topics. This info was
# unsearchable with google or other search engines.
# Opticampus.com has really good document on lens thickness.
### http://opticampus.opti.vision/cecourse.php?url=high_powered/
# Opticampus.com even has a len thickness calculator like this one.
### http://opticampus.opti.vision/tools/thickness.php