library(shiny)

# between operator, nice to have.
`%between%`<-function(x,rng) x>rng[1] & x<rng[2]

# Load data
data(mtcars)

fit<-lm(mpg ~ cyl + hp + wt + qsec + am + gear, data=mtcars)


# Definition of the server functionality.
server <- function(input, output, session) {
    # Setup which variable will be used in the plot
    # "mpg ~ cyl" means cyl on the x-axis, mpg on the y-axis.
    formulaText <- reactive({
        paste("mpg ~", input$variable)
    })

    # Return the formula, this will be used by the ui label
    output$caption <- renderText({
        formulaText()
    })

    # Reactive expression for filtering the data.
    filtered_data <- reactive({
        mtcars[
            # Only use cylinder values checked in the filter tab
            mtcars$cyl %in% as.integer(input$cylinders) &

                # Only use gear values checked in the filter tab
                mtcars$gear %in% as.integer(input$gears) &

                # Only use transmissions checked in the filter tab
                mtcars$am %in% as.integer(input$transmission) &

                # Only use horse power in range on filter tab
                mtcars$hp %between% as.integer(input$hp) &

                # Only use weights in range on filter tab
                mtcars$wt %between% as.integer(input$weight) &

                # Only use qtr mile values in range on filter tab
                mtcars$qsec %between% as.integer(input$qtrmile)
            ,]
    })

    # Create a counter of the number of remaining cars in the filter.
    output$counter <- renderText({paste(nrow(filtered_data()), " cars remaining")})

    # Create the main plot to be used on the main page
    output$mpgPlot <- renderPlot({
        boxplot(as.formula(formulaText()),  # Translate the formulaText to a formula
                data = filtered_data(),     # Use the filtered data.
                outline = input$outliers)   # Only use outliers if outliers option is checked.
    })

} # End of server


# definition of ui, the web-page to be displayed.
ui <- fluidPage(

    # Create a tabset panel so we don't need to see everything at once.
    tabsetPanel(

        # Set up the main tab.
        tabPanel("Main", pageWithSidebar(

            # Title on the main page.
            titlePanel("Guess which variables affect MPG!"),

            # Use the sidebar for selecting the active variable to plot.
            sidebarPanel(
                # Radio-buttons for selecting the variable to use.
                # This can be accessed on the server side as input$variable.
                radioButtons("variable", label=h3("Variable"),
                          choices = list("Cylinders"="cyl",
                                         "Gears"="gear",
                                         "Transmission"="am",
                                         "Horse Power"="hp",
                                         "Weight, in tons"="wt",
                                         "Quarter Mile, in seconds"="qsec"),
                          selected="cyl"),

                # Checkbox for choosing if outliers should be shown or not.
                checkboxInput("outliers", "Show outliers", TRUE)
                ), # End of sidebar

            # Main panel of the main page.
            mainPanel(

                # Access the caption text that we rendered on the server.
                 h3(textOutput("caption")),

                 # Access the mpgPlot that we rendered on the server.
                 plotOutput("mpgPlot")
                 ) # End of main panel.
        )),

        # Secondary tab for filtering data.
        tabPanel("Filters",
                 # Put everything in a fluidRow.
                 fluidRow(

                     # First column, add checkboxes for cylinders, gears & transmission.
                     column(3,
                       checkboxGroupInput("cylinders", label=h3("Cylinders"),
                                          c("4 Cyl"=4, "6 Cyl"=6, "8 Cyl"=8),
                                          selected=c(4, 6, 8)),
                       checkboxGroupInput("gears", label = h3("Number of Gears"),
                                          c("3"= 3, "4"=4, "5"= 5),
                                          selected=c(3, 4, 5)),
                       checkboxGroupInput("transmission", label = h3("Transmission Type"),
                                          c("Manual"=1, "Automatic"=0),
                                          selected=c(0, 1))
                       ), # End of first column

                     # Second column, add sliders for horse power, weight & quarter mile.
                column(3,
                       sliderInput("hp", label = h3("Horse Power"),
                                   min = min(mtcars$hp), max = max(mtcars$hp), step=5,
                                   value = c(min(mtcars$hp), max(mtcars$hp))),
                       sliderInput("weight", label = h3("Weight, in tons"),
                                   min = min(mtcars$wt), max=max(mtcars$wt), step = .1,
                                   value =c(min(mtcars$wt), max(mtcars$wt))),
                       sliderInput("qtrmile", label = h3("Quarter Mile, in Seconds"),
                                   min = min(mtcars$qsec), max = max(mtcars$qsec), step = .1,
                                   value=c(min(mtcars$qsec), max(mtcars$qsec)))
                       ), # End of second column

                # Third column, just add the counter rendered text from the server.
                column(6, textOutput("counter"))
            )
        )
    )
)

shinyApp(ui=ui, server=server)
