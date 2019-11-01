building_price_labels <- c("20", "200", "2000", "20K","200K","2M","20M","200M")

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(rel="stylesheet", href="https://stackpath.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css", integrity="sha384-wvfXpqpZZVQGK6TAh5PVlGOfQNHSoD2xbE+QkPxCAFlNEevoEH3Sl0sibVcOQVnN", crossorigin="anonymous")
  ),
  fluidRow(
    column(4, class="store-panel",
                fluidRow(tags$h1("Store",style="text-align:center;"), id="store-title"),
                fluidRow(actionButton(inputId = "b1", paste("Grandma ( price: "       ,toString(building_price_labels[1]),"books )") )  ),
                fluidRow(actionButton(inputId = "b2", paste("Yardsale ( price: "      ,toString(building_price_labels[2]),"books )") )),
                fluidRow(actionButton(inputId = "b3", paste("The Bins ( price: "      ,toString(building_price_labels[3]),"books )") )),
                fluidRow(actionButton(inputId = "b4", paste("Book Store ( price: "    ,toString(building_price_labels[4]),"books )") )),
                fluidRow(actionButton(inputId = "b5", paste("Library ( price: "       ,toString(building_price_labels[5]),"books )") )),
                fluidRow(actionButton(inputId = "b6", paste("Amazon.com ( price: "    ,toString(building_price_labels[6]),"books )") )),
                fluidRow(actionButton(inputId = "b7", paste("Factory ( price: "       ,toString(building_price_labels[7]),"books )") )),
                fluidRow(actionButton(inputId = "b8", paste("Watzek Library ( price: ",toString(building_price_labels[8]),"books )") ))
              ),

    column(4, class="middle-panel",
                actionButton(inputId = "manual_add", tags$i(class="fa fa-book fa-5x")),
                fluidRow(tags$h3("Click Me!")),
                fluidRow(textOutput(outputId = "count")),
                fluidRow(textOutput(outputId = "BPS"))
              ),
    column(4, class="stats-panel",

                fluidRow(plotOutput(outputId = "count_to_time"))
              )
          ),
  fluidRow(
            column(4,plotOutput(outputId = "sources")),
            column(4,plotOutput(outputId = "pieChart")),
            column(4,plotOutput(outputId = "bps_to_time"))
          ),

  tags$script(HTML(
    "
    var time_elapsed = 0;
    window.onload = function()
    {
      setInterval(function(){time_elapsed+=1; Shiny.onInputChange(\"sec_elapsed\", String(time_elapsed),{priority:\"event\"})},1000);
    }
    "
  ))

)
