#' @title Plot Posterior 
#' @description Plot of the projection of 3-simplex points of a posterior 
#'     Dirichlet distribution into a 2D triangle. The function is used from the 
#'     package rNPBST. Since it is a numeric function, the code was copied for
#'     further use in this package. 
#' @param x Results of a Bayesian test. 
#' @param num.points Number of points to be shown in the plot. 
#' @references \url{https://github.com/JacintoCC/rNPBST}
plot_triangles <- function(x, num.points = nrow(x$sample), ...){
  sample <- x$sample
  # Reduce the number of points to plot
  if(num.points > nrow(sample))
    num.points = nrow(sample)
  subset <- sample[sample(nrow(sample), num.points), ]
  L <- subset[ ,1]
  rope <- subset[ ,2]
  R <- subset[ ,3]
  # Build
  df.points <- data.frame("L" = L, "rope" = rope, "R" = R, 
    "d" = grDevices::densCols(subset,
    colramp = grDevices::colorRampPalette(grDevices::heat.colors(20))))
  # Lines to separe regions
  lines <- data.frame(x = c(0.5, 0, 0.5), y = c(0, 0.5, 0.5),
    z = c(0.5, 0.5, 0), xend = c(1,1,1)/3, yend = c(1,1,1)/3, zend = c(1,1,1)/3)
  borders <- data.frame(x = c(1,0,0), y=c(0,1,0), z=c(0,0,1),
    xend = c(0,1,0), yend=c(0,0,1), zend=c(1,0,0))
  suppressWarnings(
    ggtern::ggtern(data = df.points, ggtern::aes(L, rope, R)) +
      ggplot2::geom_point(color = df.points$d) +
      ggplot2::geom_segment(data = lines,
        ggtern::aes(x = c(0.5, 0, 0.5), y = c(0, 0.5, 0.5),
          z = c(0.5, 0.5, 0), xend = c(1,1,1)/3, yend = c(1,1,1)/3, 
          zend = c(1,1,1)/3), color = 'orange', size = 0.5) +
      ggplot2::geom_segment(data = borders,
        ggtern::aes(x = c(1,0,0), y=c(0,1,0), z=c(0,0,1), xend = c(0,1,0), 
          yend=c(0,0,1), zend=c(1,0,0)), color = 'orange', size = 1))
}
