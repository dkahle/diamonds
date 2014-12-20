library(ggplot2)
library(plyr)
library(reshape2)
theme_set(theme_bw(14))

width  <- 7 # graphic width
height <- 3 # graphic height

#####################################################################
#################### basic elements              ####################
#####################################################################


write.csv(diamonds, "diamonds.csv", row.names = FALSE)
file.show("diamonds.csv")

length(unique(diamonds$price))

n <- nrow(diamonds)
p <- ncol(diamonds)

#####################################################################
#################### one-dim discrete            ####################
#####################################################################

table(diamonds$cut)
round(table(diamonds$cut) / n, 3)
sum(round(table(diamonds$cut) / n, 3)) # take the .001 off of ideal

qplot(cut, data = diamonds) +
   xlab("Cut") + ylab("Frequency")
ggsave("barchart.pdf", width = width, height = height)


ggplot(
    aes(x = 1, y = V1, fill = cut), 
   data = ddply(diamonds, "cut", nrow)
  ) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  scale_x_continuous("", breaks = NULL) +
  scale_y_continuous("", breaks = NULL)
ggsave("piechart.pdf", width = width, height = height)

#####################################################################
#################### one-dim continuous          ####################
#####################################################################

set.seed(1)
fiftyRows <- sample(n, 50)
fiftyDiamonds <- diamonds[fiftyRows,]

qplot(carat, 0L, data = fiftyDiamonds, size = I(8)) +
  geom_hline(yintercept = 0) +
  scale_x_continuous("Carat", lim = c(0,2.5)) +
  scale_y_continuous("", lim = c(-1,1), breaks = NULL)
ggsave("1dscatterplot.pdf", width = width, height = height)

fiftyDiamonds$yValues <- runif(50, -1, 1)
qplot(carat, yValues, data = fiftyDiamonds, size = I(8)) +
  geom_hline(yintercept = 0) +
  scale_x_continuous("Carat", lim = c(0,2.5)) +
  scale_y_continuous("", lim = c(-1,1), breaks = NULL)
ggsave("jittering.pdf", width = width, height = height)

qplot(carat, yValues, data = fiftyDiamonds, size = I(8), alpha = I(.33)) +
  geom_hline(yintercept = 0) +
  scale_x_continuous("Carat", lim = c(0,2.5)) +
  scale_y_continuous("", lim = c(-1,1), breaks = NULL)
ggsave("alpha.pdf", width = width, height = height)

qplot(carat, yValues, data = fiftyDiamonds, size = I(5), alpha = I(.33)) +
  geom_hline(yintercept = 0) +
  scale_x_continuous("Carat", lim = c(0,2.5)) +
  scale_y_continuous("", lim = c(-1,1), breaks = NULL)
ggsave("resized.pdf", width = width, height = height)

qplot(carat, runif(n, -1, 1), data = diamonds, size = I(5), alpha = I(.01)) +
  geom_hline(yintercept = 0) +
  scale_x_continuous("Carat", lim = c(0,2.5)) +
  scale_y_continuous("", lim = c(-1,1), breaks = NULL)
ggsave("allscatter.png", width = width, height = height)


## histograms
qplot(carat, data = diamonds) +
  scale_x_continuous("Carat", lim = c(0,5)) +
  ylab("Frequency")
ggsave("histogram1.pdf", width = width, height = height)

qplot(carat, data = diamonds, binwidth = 2) +
  scale_x_continuous("Carat", lim = c(0,5)) +
  ylab("Frequency")
ggsave("histogram2.pdf", width = width, height = height)

qplot(carat, data = diamonds, binwidth = .01) +
  scale_x_continuous("Carat", lim = c(0,5)) +
  ylab("Frequency") + geom_vline(xintercept = .9)
ggsave("histogram3.pdf", width = width, height = height)

slightlySmallerDiamonds <- subset(diamonds, .99 <= carat & carat < 1)
nrow(slightlySmallerDiamonds)
mean(slightlySmallerDiamonds$price)
median(slightlySmallerDiamonds$price)

slightlyBiggerDiamonds <- subset(diamonds, 1 <= carat & carat < 1.01)
nrow(slightlyBiggerDiamonds)
mean(slightlyBiggerDiamonds$price)
median(slightlyBiggerDiamonds$price)

qplot(price, data = slightlySmallerDiamonds)

qplot(price, data = slightlyBiggerDiamonds)

t.test(
  slightlyBiggerDiamonds$price,
  slightlySmallerDiamonds$price, 
  alternative = "greater"
)


## kernel density estimators
s <- seq(0, 2.5, length.out = 501)
little_densities <- lapply(as.list(fiftyDiamonds$carat), function(mu){
  dnorm(s, mu, sd = .05) / 50
})
df <- as.data.frame(t(plyr:::list_to_dataframe(little_densities)))
names(df) <- paste0("mu=", fiftyDiamonds$carat)
df$x <- s
mdf <- melt(df, id = "x")
kde <- ddply(mdf, "x", function(df) sum(df$value) )


ggplot() +
  geom_line(
    aes(x = x, y = value, group = variable),
    size = .2,
    data = mdf
  ) +
  geom_line(
    aes(x = x, y = V1),
    size = 1, color = "red",
    data = kde
  ) +
  scale_x_continuous("Carat", lim = c(0,2.5)) +
  ylab("Density") 
ggsave("kde.pdf", width = width, height = height)

qplot(carat, ..density.., data = fiftyDiamonds, geom = "histogram") +
  scale_x_continuous("Carat", lim = c(0,2.5)) +
  ylab("Density")
ggsave("kdeHist.pdf", width = width, height = height)



#####################################################################
#################### two-dim cont-cont           ####################
#####################################################################

qplot(carat, price, data = diamonds) +
  scale_x_continuous("Carat") +
  scale_y_continuous("Price")
ggsave("2dscatterplot.png", width = width, height = height)

qplot(carat, price, data = diamonds, alpha = I(.05), size = I(1)) +
  scale_x_continuous("Carat") +
  scale_y_continuous("Price")
ggsave("2dscatterplotalpha.png", width = width, height = height)

qplot(carat, price, data = diamonds, geom = "bin2d") +
  scale_x_continuous("Carat") +
  scale_y_continuous("Price") +
  scale_fill_continuous("Frequency")
ggsave("Histogram2dTopColor.pdf", width = width, height = height)

qplot(carat, price, data = diamonds, geom = "bin2d") +
  stat_smooth(color = "red", size = 2) +
  scale_x_continuous("Carat") +
  scale_y_continuous("Price") +
  scale_fill_continuous("Frequency")
ggsave("Histogram2dTopColorSmoother.pdf", width = width, height = height)


ggplot(aes(x = carat, y = price), data = diamonds) +
  stat_density2d() +
  scale_x_continuous("Carat", lim = c(0,4)) +
  scale_y_continuous("Price", lim = c(0,18000))
ggsave("ContourPlot.pdf", width = width, height = height)

#####################################################################
#################### two-dim disc-cont           ####################
#####################################################################

qplot(clarity, price, data = diamonds) +
  scale_x_discrete("Clarity") +
  scale_y_continuous("Price")
ggsave("disc-cont-1.png", width = width, height = height)

qplot(clarity, price, data = diamonds, geom = "jitter", alpha = I(.05)) +
  scale_x_discrete("Clarity") +
  scale_y_continuous("Price")
ggsave("disc-cont-2.png", width = width, height = height)

qplot(clarity, price, data = diamonds, geom = "boxplot") +
  scale_x_discrete("Clarity") +
  scale_y_continuous("Price")
ggsave("disc-cont-3.pdf", width = width, height = height)

qplot(clarity, price, data = diamonds, geom = "violin") +
  scale_x_discrete("Clarity") +
  scale_y_continuous("Price")
ggsave("disc-cont-4.pdf", width = width, height = height)

#####################################################################
#################### two-dim disc-disc           ####################
#####################################################################

qplot(clarity, cut, data = diamonds, geom = "jitter", 
    alpha = I(.05), size = I(1.5)
  ) +
  scale_x_discrete("Clarity") +
  scale_y_discrete("Cut")
ggsave("disc-disc-1.png", width = width, height = height)

qplot(clarity, data = diamonds, geom = "bar", 
  fill = cut, position = "dodge")
ggsave("disc-disc-2.png", width = width, height = height)

qplot(clarity, data = diamonds, geom = "bar", 
  fill = cut, position = "stack")
ggsave("disc-disc-3.png", width = width, height = height)







