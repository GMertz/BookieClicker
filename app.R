library("shiny")
#library("plotrix")

# Book Clicker,
# Grandmother, Yardsale, The Bins, Book store, Library, Amazon, Factory, Watzek Libraryx
count <- c(0)
bps_per_second <- c(0)

seconds_elapsed <- 0
clicked_last_sec <- 0

building_CPS <- c(1,5,10,20,40,80,160,320)
building_cost <- c(2e1,2e2,2e3,2e4,2e5,2e6,2e7,2e8)
building_labels <- c("Grandmother", "Yardsale", "The Bins", "Book Store", "Library", "Amazon", "Factory", "Watzek")
building_price_labels <- c("20", "200", "2000", "20K","200K","2M","20M","200M")

vals <- reactiveValues(
                       "amount" = amount <- 0,
                       "building_counts" = c(0L,0L,0L,0L,0L,0L,0L,0L),
                       "on_tick" = "",
                       "time" = c(0)
                       )


do_tick = function(output)
{
  bps = sum(c(building_CPS*vals$building_counts))
  vals$amount <<- vals$amount + bps

  trueBPS <- bps+clicked_last_sec
  add_to(vals$amount, trueBPS)

  output$BPS <- renderText({
    paste("BPS: ",toString(trueBPS))
  })

  clicked_last_sec <<- 0
}

get_epoch = function()
{
  x <- as.POSIXct( Sys.time() )
  class(x)
  return (as.integer( x ))
}

add_to = function(books, bps)
{
  count <<- append(count,books)
  bps_per_second <<- append(bps_per_second, bps)

  vals$time <<- append(vals$time, seconds_elapsed)
  seconds_elapsed <<- seconds_elapsed + 1
}

handle_button = function(b_ind)
{
  if (vals$amount >= building_cost[b_ind])
  {
    vals$amount <<- vals$amount - building_cost[b_ind]
    vals$building_counts[b_ind] <<- vals$building_counts[b_ind] + 1L
  }
  else
  {
    print("Not enough books!")
  }
}



ui <- fluidPage(
  tags$head(
    htmlOutput(tags$title(id="shiny_title")),
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
                fluidRow(actionButton(inputId = "b8", paste("Watzek Library ( price: ",toString(building_price_labels[8]),"books )") )),
              ),

    column(4, class="middle-panel",
                actionButton(inputId = "manual_add", tags$i(class="fa fa-book fa-5x")),
                fluidRow(tags$h3("Click Me!")),
                fluidRow(textOutput(outputId = "count")),
                fluidRow(textOutput(outputId = "BPS")),
              ),
    column(4, class="stats-panel",

                fluidRow(plotOutput(outputId = "count_to_time")),
              )
          ),
  fluidRow(
            column(4,plotOutput(outputId = "buildings")),
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


server <-  function(input, output){

  observeEvent(input$sec_elapsed,{
    do_tick(output)
  })

  observeEvent(input$manual_add, {
    clicked_last_sec <<- clicked_last_sec + 1L
    vals$amount <<- vals$amount + 1L
  })

  # Handle Button Pressess =============
  observeEvent(input$b1, {
    handle_button(1)
  })
  observeEvent(input$b2, {
    handle_button(2)
  })
  observeEvent(input$b3, {
    handle_button(3)
  })
  observeEvent(input$b4, {
    handle_button(4)
  })
  observeEvent(input$b5, {
    handle_button(5)
  })
  observeEvent(input$b6, {
    handle_button(6)
  })
  observeEvent(input$b7, {
    handle_button(7)
  })
  observeEvent(input$b8, {
    handle_button(8)
  })
  # End Handle Button Presses ===================

  output$shiny_title <- renderText({
    paste("Test", toString(vals$amount))
  })

  output$count <- renderText({
    paste("Books: ",format(vals$amount))
  })

  output$buildings <- renderPlot({barplot(vals$building_counts,
                                       col="#00DDFF",
                                       border="#111111",
                                       names.arg=building_labels,
                                       ylim=c(0,max(vals$building_counts)+1L),
                                       xlim=c(0,8),
                                       space=0,
                                       main="Buildings"
                                       )})
  output$pieChart <- renderPlot({

    CPS=c(building_CPS*vals$building_counts)
    x = CPS[CPS != 0]
    labels=paste(building_labels[CPS != 0],"\n", x)

    if (length(x) == 0)
    {
      x = c(1)
      labels = c("none")
    }


    pie(
      x=x,
      labels=labels,
      main="BPS Contributors"
    )

  })

  output$count_to_time <- renderPlot({
    plot(vals$time,
         count,
         type="l",
         col="green",
         xlab="Time",
         ylab="Books",
         main="Books over Time"
       )
  })

  output$bps_to_time <- renderPlot({
    plot(vals$time,
         bps_per_second,
         type="l",
         col="green",
         xlab="Time",
         ylab="Books Per Second",
         main="BPS over time"
       )
  })
  output$BPS <- renderText({
    paste("BPS: ",toString(0))
  })

}
#shinyApp(ui = htmlTemplate('template.html'),server=server)
shinyApp(ui = ui, server = server)
