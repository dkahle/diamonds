ggmosaic <- function(x, y, data, 
  xlab = deparse(substitute(x)), 
  ylab = deparse(substitute(y))
){
  
  xName <- deparse(substitute(x))
  xv <- data[,xName]
  xLevels <- levels(xv)
  
  yName <- deparse(substitute(y))
  yv <- data[,yName]
  yLevels <- levels(yv)
  
  xFreqs <- table(xv) / length(xv)
  cumxFreqs <- unname(c(0,cumsum(xFreqs)))
  p <- length(cumxFreqs)
  
  props <- ddply(data, xName, function(df){
    y <- unname(
      cumsum(c(0, table(df[,yName]) / nrow(df)))
    )  
    n <- length(y)
    data.frame(ymin = y[1:(n-1)], ymax = y[2:n])
  })
  
  props[,yName] <- rep(yLevels, length(xLevels))
  props[,yName] <- factor(props[,yName], levels = yLevels, ordered = TRUE  )  
  
  props$xmin <- rep(cumxFreqs[1:(p-1)], each = length(yLevels))
  props$xmax <- rep(cumxFreqs[2:p], each = length(yLevels))
  
  xBreaks <- cumxFreqs[1:(p-1)] + diff(cumxFreqs)/2
  names(xBreaks) <- xLevels
  
  ggplot(data = props) +
    geom_rect(aes(
      xmin = xmin, xmax = xmax,
      ymin = ymin, ymax = ymax,
      fill = cut
    ), color = "white") +
    scale_color_discrete(guide = "none") +
    scale_fill_discrete(deparse(substitute(y))) +
    scale_x_continuous(deparse(substitute(x)), breaks = xBreaks, expand = c(0,0)) +
    scale_y_continuous("Relative Frequency", expand = c(0,0))
}

# for a more flexible, but not ggplot based mosaicplot function:
# library(vcd)
# tab <- table(diamonds[,c("clarity","cut")])
# mosaicplot(tab)