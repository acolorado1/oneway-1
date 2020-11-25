#' Plot one-way ANOVA
#' 
#' @description 
#' \code{plot.oneway} creates group comparisons for a one-way ANOVA and gives the user an option to choose among several types of graphs.
#'
#' @param x an object of class \code{oneway}
#' @param plot a character string representing the type of plot. Default is boxplot. Other types of plot: violin and density ridges. 
#' @param ... additional arguments passed to the chosen plot function.
#'
#' @export
#' 
#' @return NULL
#' 
#' @import ggplot2 ggridges
#'
#' @examples
#' fit <- oneway(mpg ~ cyl, mtcars)
#' plot(fit, plot = "boxplot")
#' plot(fit, plot = "violin")
#' plot(fit, plot = "density_ridges")

plot.oneway <- function(x, plot = "boxplot", ...){
  if(!inherits(x, "oneway"))
    stop("Must be class 'oneway")
  
  g <- as.character(x$anova$terms[[3]]) # "cyl"
  y <- as.character(x$anova$terms[[2]]) # "mpg"
  
  # box plot
  if(plot=="boxplot"){
    ggplot(x$anova$model, aes(x = factor(.data[[g]]), y= .data[[y]])) + 
      geom_boxplot(fill = "lightblue",...) + geom_dotplot(binaxis='y', stackdir='center', dotsize = .5, fill="pink") + labs(x=g) +
      theme(legend.position="none") + ggtitle("Box plot")
    
  # violin plot
  } else if(plot=="violin"){
    ggplot(x$anova$model, aes(x = factor(.data[[g]]), y= .data[[y]]), ...) + labs(x=g) + 
      theme(legend.position="none") + ggtitle("Violin plot") + geom_violin(fill = "lightblue",...) + geom_dotplot(binaxis='y', stackdir='center', dotsize = .5, fill="pink")
    
  # density ridges plot
  } else if(plot=="density_ridges"){
    ggplot(x$anova$model, aes(.data[[y]], .data[[g]],group=.data[[g]],fill=.data[[g]])) + 
      geom_density_ridges(...) + theme_ridges() + labs(x=g) + ggtitle("Density ridges plot")
  }
}