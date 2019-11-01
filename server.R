

count <- c(0)
bps_per_second <- c(0)

seconds_elapsed <- 0
clicked_last_sec <- 0

building_BPS <- c(1,5,10,20,40,80,160,320)
building_cost <- c(2e1,2e2,2e3,2e4,2e5,2e6,2e7,2e8)
building_labels <- c("Grandmother", "Yardsale", "The Bins", "Book Store", "Library", "Amazon", "Factory", "Watzek")
vals <- reactiveValues(
                       "amount" = amount <- 0,
                       "building_counts" = c(0L,0L,0L,0L,0L,0L,0L,0L),
                       "on_tick" = "",
                       "time" = c(0)
                       )


do_tick = function(output)
{
  bps = sum(c(building_BPS*vals$building_counts))
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



server <-  function(input, output){

  observeEvent(input$sec_elapsed,{
    do_tick(output)})

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

  output$sources <- renderPlot({barplot(vals$building_counts,
                                       col="#00DDFF",
                                       border="#111111",
                                       names.arg=building_labels,
                                       ylim=c(0,max(vals$building_counts)+1L),
                                       xlim=c(0,8),
                                       space=0,
                                       main="Book Sources"
                                       )})
  output$pieChart <- renderPlot({

    BPS=c(building_BPS*vals$building_counts)
    x = BPS[BPS != 0]
    labels=paste(building_labels[BPS != 0],"\n", x)

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
