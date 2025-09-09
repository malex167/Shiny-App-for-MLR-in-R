library(shiny)
library(dplyr)
library(ggplot2)

spruce <- read.csv("SPRUCE.csv")

plt1 <- spruce |>
  ggplot(aes(x = BHDiameter, y = Height)) +
  geom_point(col = 'blue') +
  ggtitle("Alex Force's BH Diameter to Height, LOESS fit") +
  geom_smooth(method = 'loess', formula = 'y ~ x')

plt2 <- spruce |>
  ggplot(aes(x = BHDiameter, y = Height)) +
  geom_point(col = 'blue') +
  ggtitle("Alex Force's BH Diameter to Height, lm fit") +
  geom_smooth(method = 'lm', formula = 'y ~ x')

ylm1 <- lm(formula = Height ~ BHDiameter, data = spruce)

ylm2 <- lm(formula = Height ~ BHDiameter + I(BHDiameter^2), data = spruce)

spruce2 <- spruce |>
  mutate(r_i = residuals(ylm1), yhat = 9.14684 + 0.48147 * BHDiameter)

plt3 <- spruce2 |>
  ggplot(aes(x = yhat, y = r_i)) +
  geom_point(col = 'blue') +
  geom_smooth(method = 'loess', formula = 'y ~ x') +
  ggtitle("Alex Force's y hat to residuals Plot")



ui <- fluidPage(

    mainPanel(
      plotOutput("plot1", brush = "plot1_brush"),
      plotOutput("plot2", brush = "plot2_brush"),
      plotOutput("plot3", brush = "plot3_brush"),
      textOutput("linear"),
      textOutput("quadratic")
    )
  )

server <- function(input, output) {

  observeEvent(input$plot1_brush, {

      BHD <- as.matrix(brushedPoints(spruce, input$plot1_brush))
      spruce_new <- spruce |>
        filter(!BHDiameter %in% c(BHD[,1]))

      plt1 <- spruce_new |>
        ggplot(aes(x = BHDiameter, y = Height)) +
        geom_point(col = 'blue') +
        ggtitle("Alex Force's BH Diameter to Height, LOESS fit") +
        geom_smooth(method = 'loess', formula = 'y ~ x')

      output$plot1 <- renderPlot({
        plt1
  })
})

  observeEvent(input$plot2_brush, {

    BHD <- as.matrix(brushedPoints(spruce, input$plot2_brush))
    spruce_new <- spruce |>
      filter(!BHDiameter %in% c(BHD[,1]))

    plt2 <- spruce_new |>
      ggplot(aes(x = BHDiameter, y = Height)) +
      geom_point(col = 'blue') +
      ggtitle("Alex Force's BH Diameter to Height, lm fit") +
      geom_smooth(method = 'lm', formula = 'y ~ x')

    ylm1 <- lm(formula = Height ~ BHDiameter, data = spruce_new)

    output$plot2 <- renderPlot({
      plt2
    })

    output$linear <- renderText({
      c(paste0("Estimating formula for linear fit: Height = ",
               round(ylm1$coefficients[1], 4),
               " + ",
               round(ylm1$coefficients[2], 4),
               " BH Diameter"))
    })
  })

  observeEvent(input$plot3_brush, {

    y <- as.matrix(brushedPoints(spruce2, input$plot3_brush))

    spruce_new <- spruce2 |>
      filter(!yhat %in% c(y[,4]))

    plt3 <- spruce_new |>
      ggplot(aes(x = yhat, y = r_i)) +
      geom_point(col = 'blue') +
      geom_smooth(method = 'loess', formula = 'y ~ x') +
      ggtitle("Alex Force's y hat to residuals Plot")

    ylm1 <- lm(formula = Height ~ BHDiameter, data = spruce_new)

    ylm2 <- lm(formula = Height ~ BHDiameter + I(BHDiameter^2), data = spruce_new)

    output$plot3 <- renderPlot({
      plt3
    })

    output$quadratic <- renderText({
      c(paste0("Estimating formula for quadratic fit: Height = ",
               round(ylm2$coefficients[1], 4),
               " + ",
               round(ylm2$coefficients[2], 4),
               " BH Diameter",
               round(ylm2$coefficients[3], 4),
               " BH Diameter^2"))
    })
  })

  output$plot1 <- renderPlot({
    plt1
  })
  output$plot2 <- renderPlot({
    plt2
  })
  output$plot3 <- renderPlot({
    plt3
  })
  output$linear <- renderText({
    c(paste0("Estimating formula for linear fit: Height = ",
             round(ylm1$coefficients[1], 4),
             " + ",
             round(ylm1$coefficients[2], 4),
             " BH Diameter"))
  })
  output$quadratic <- renderText({
    c(paste0("Estimating formula for quadratic fit: Height = ",
             round(ylm2$coefficients[1], 4),
             " + ",
             round(ylm2$coefficients[2], 4),
             " BH Diameter",
             round(ylm2$coefficients[3], 4),
             " BH Diameter^2"))
  })
}

shinyApp(ui = ui, server = server)
