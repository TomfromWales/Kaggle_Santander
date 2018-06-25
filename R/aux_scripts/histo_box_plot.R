library(plotly)

histo_box_plot <- function(
  data
  ,target
){
  
  # hack for a bug fix
  
  data$target = data[[target]]
  
  # axis styles
  ax <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = TRUE,
    showgrid = FALSE
  )
  
  aax <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE
  )
  
  # box on top / hist bottom
  s <- subplot(
    plot_ly(data, x = ~target, type = "box", name = target),
    plotly_empty(),
    plot_ly(data, x = ~target, type = "histogram", name = target) %>%
      layout(showlegend = FALSE, xaxis = ax, yaxis = ax),
    nrows = 2, heights = c(0.2, 0.8), widths = c(0.8, 0.2),
    shareX = TRUE
  )
  # show box/hist subplot
  layout(s)
  
  
  
  # show both as a subplot
  #p <- subplot(s,ss) %>%
   # layout(showlegend=FALSE)
  
  return(s)
}

# -- test -------

#histo_box_plot(data=mtcars,target="mpg")
