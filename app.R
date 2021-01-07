#predicting the durability of glycaemic control among people with metformin failure
#DISCOVER study programme
#
#R shiny interactive tool
library(shiny)
ui <- fluidPage(
  
  titlePanel("Probability of sustained glycaemic control for 24 months"),
  sidebarLayout(
    sidebarPanel(
      numericInput("Age", "Age in years", value = 55, min = 22, max = 88),
      numericInput("HbA1c", "HbA1c in %",    value = 7.5, step = 0.1, min = 6, max =13.5),
      selectInput("Gender", "Gender", choices = list("Male" = 0, "Female" = -0.16261284), selected = 0),
      selectInput("CountryGroup", "Country income group", choices = list("High" =0, "Upper Middle" =0.16960017, "Lower Middle" = -0.389641957), selected = 0),
      selectInput("Ethnicity", "Ethnicity", choices = list("Caucasian" = 0, "Asian" = -0.055256883, "Other"= 0.099753732), selected = 0),
      selectInput("L2", "Second-Line therapy", choices = list("Monotherapy" = 0, "Insulin"= -0.554612676, "Metformin+DDP4"= 0.505266974, "Metformin+GLP1"= 0.64627586,
                                                              "Metformin+SGLT2"= 0.224915465, "Metformin+Sulphonylurea"= -0.143902684, "Metformin+Thiazolidinedione" = 0.362875502, 
                                                              "Other Dual therapy"= 0.234588233, "Triple therapy or more"=-0.077791117), selected = 0)),
    mainPanel(
      htmlOutput(outputId = "Probability"),
      tags$head(tags$style("#Probability{color: red;
                           font-size: 120px;
                           font-style: bold;}"))
    )
  )
)

hba1c <-read.csv("hba1c.csv")
server <-function(input, output) ({

  output$Probability <-renderText({
    b1<- 0.6482304      * input$HbA1c
    p <- (input$HbA1c-6)* 10+1
    p2<- hba1c$hba1c_2[p]
    p3<- hba1c$hba1c_3[p]
    p4<- hba1c$hba1c_4[p]
    b2<- -19.23596357  * p2
    b3<- 81.59652682   * p3
    b4<- -82.82814469  * p4
    a <- 0.00658351    * input$Age
    c <- as.numeric(input$Gender)
    d <- as.numeric(input$CountryGroup)
    e <- as.numeric(input$Ethnicity)
    f <- as.numeric(input$L2)
    totalexp <- -5.01331311992031 + a + b1 +b2 + b3 + b4 + c + d + e + f
    Probability <- (exp(totalexp)/(1+exp(totalexp))) * 100
    paste( "<font color=\"#FF0000\"><b>",  round(Probability, digits = 0), "%")
  }  )
})

shinyApp(ui, server)