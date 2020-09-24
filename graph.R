graph_function <- function(function_values, colours = NULL, fill_under = FALSE){
  function_values_long <- melt(function_values, id.vars = "x", variable.name = "Function", 
                          value.name = "y")
  if(is.null(colours)){
  colours <- RColorBrewer::brewer.pal(8, "Paired")[(ncol(function_values)-1):1]
  }
  names(colours) <- colnames(function_values)[-1]
  gg <- ggplot(function_values_long) + 
    aes(x = x, y = y, colour = Function) +
    theme_minimal() +
    theme(axis.title.y = element_text(angle = 0), 
          axis.title.x = element_text(hjust = 1)) +
    scale_colour_manual("", values = colours)
  # Add lines for x = 0 and y = 0 if visible
  ybold <- min(function_values_long$x)<=0 & max(function_values_long$x)>=0
  xbold <- min(function_values_long$y)<=0 & max(function_values_long$y)>=0
  if(ybold){
    gg <- gg + geom_vline(xintercept = 0, colour = "grey60", size = rel(0.6))
  }
  if(xbold){
    gg <- gg + geom_hline(yintercept = 0, colour = "grey60", size = rel(0.6)) 
  }
  # If filled curves are requested
  if(fill_under){
    fills <- alpha(colours, 0.35)
    gg <- gg + geom_area(aes(fill = Function), size = rel(1)) +
      scale_fill_manual("", values = fills)
  } else{
  gg <- gg + geom_line(size = rel(1)) 
  } 
  gg 
}
