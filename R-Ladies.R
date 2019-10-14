setwd("/Users/zhangzhuowen/desktop")
sheep <- read.csv("sheep.csv")
# diet   1=nolupin,2=lupin
# initwt   initial ewe weight (kg)
# wt       final ewe weight (kg)
# growth   wool growth (gm/100 sq cm/day)
# length   fibre length growth (mm/day)
# diam     fibre diameter (microns)
# prickle  prickle factor (%fibres > 30 microns)
# yield    wool yield (% of fleece that is wool)


# when considering some basic statistics of numeric variable
mean(sheep$length)

# when considering about the different categories, we can use
tapply(sheep$length,sheep$diet,mean)
median(sheep$length)
tapply(sheep$length,sheep$diet,median)
sd(sheep$length)
tapply(sheep$length,sheep$diet,sd)
summary(sheep$length)
tapply(sheep$length,sheep$diet,summary)

# boxplot is a visual display of the five-number summary
boxplot(sheep$length)

# It is a bad diagram, in order to show the data clearer, we need to add
# some elements to modify it.
boxplot(sheep$length,horizontal =T,
        main = "Boxplot of 30 ewes fibre length growth 1994, Mount Derrimut Experimental Farm",
        xlab = "Fibre Length Growth (mm/day)",
        las = 0)
# las shows the direction of x or y lab direction 
# 0-always Parallel to the coordinate axis,1-always horizontial
# 2-always perpendicular to the axis


# Here we can use histogram to present frequencies in different ranges of the values
# of a numerical variable
hist(sheep$length,
     main = "Histogram of 30 ewes fibre length growth 1994, Mount Derrimut Experimental Farm",
     xlab = "Fibre Length Growth (mm/day)",
     las = 1)

# Here is another plot related to histogram which is density plot
# Density plot is a smoothed histogram
# The subjectiveness of histograms (e.g. the choice of bin width and starting point) 
# is potentially unsatisfactory. One strategy is to try to smooth them out using kernel density estimators
# something we will not discuss in detail here. 
plot(density(sheep$length))

# about the category variable, usually we will focus on how many
# sheeps in each diet, we usually use table to show the distribution of 
# a categorial variable
table(sheep$diet)
# how to show it
barplot(sheep$diet)

barplot(table(sheep$diet))


# Now let's see two numeric variables for example
plot(yield ~ diam, data = sheep,
     main = "The relationship between wool yield and fibre diameter",
     xlab = "Fibre Diameter (microns)",
     ylab = "Wool Yield (% of fleece that is wool)",
     las = 1)

# The first argument represnts y the second one represents x, which is a scatterplot
# You can also use the plot in one variable for example, length
plot(sheep$length)
plot(sheep$length, type = "h")

# Think about relationship between wool yield and fibre diameter
cor(sheep$yield, sheep$diam)
# The correlation coefficient between these two variables is -0.4006844, 
# which means there is a negative correction relationship between 
# the wool yield and the fibre diameter.
# We can fit this using the R linear model function
(linearmodel1<-lm(sheep$yield ~ sheep$diam))
summary(linearmodel1)
abline(linearmodel1)

# There is another method to draw this fitting line, using the curve command
# Curve command is used to draw the digram for certain function
curve(103.131-0.792*x, add = TRUE)


# Check the distrbuition is normal or not, first draw the distribution of prickle
plot(density(sheep$prickle), 
     main = "Density Plot of 30 ewes prickle factor 1994, Mount Derrimut Experimental Farm",
     xlab = "Prickle Factor (%fibres > 30 microns)",
     las = 1)

# draw the related normal distribution which is decided by two parameter
curve(dnorm(x,mean = mean(sheep$prickle),sd=sd(sheep$prickle)),
      add=TRUE)

curve(dnorm(x,mean = mean(sheep$prickle),sd=sd(sheep$prickle)),
      add=TRUE, 
      col="blue",
      lwd=1)
# lwd means Line width
# it is hard to show the other people which line is about, no instructon
# a legend should present
# first is about position
# inset is the related position in legend
# cex is the size of legend
# lty is the type of lines, 1 means normal line
legend("topleft", inset = 0.05, 
       cex = 0.5,title= "Distribution Type",
       c("Prickle factor","Normal"),
       lty=c(1,1),
       col=c("black","blue"))

# another way is using qq plot which checks whether its dot follows the line
qqnorm(sheep$prickle,las = 1)
qqline(sheep$prickle)
# The dots in QQ-plot are close to a straight line 
# and the density plot is approximate the characteristic bell-shaped curve
# which both illustrate the data are likely to come from the normal distribution.

# now let's see about one numeric variable and one categorical variable
# using boxplot
boxplot(wt~diet, 
        varwidth = TRUE, 
        data = sheep,
        main = "The relationship between final ewe weight and diet",
        xlab = "Diet Treatment",
        ylab = "Final Ewe Weight (kg)",
        las =1)

sheep$dietlabel <- factor(sheep$diet,labels=c("no lupin", "lupin"))
boxplot(wt~dietlabel, 
        varwidth = TRUE, 
        data = sheep,
        main = "The relationship between final ewe weight and diet",
        xlab = "Diet Treatment",
        ylab = "Final Ewe Weight (kg)",
        las =1)

# Another way is using histograms
Diet1<-subset(sheep,sheep$dietlabel=="no lupin")
Diet2<-subset(sheep,sheep$dietlabel=="lupin")
par(mfrow = c(1,2))
hist(Diet1$wt,
     main = "Histogram of 15 ewes final weight without lupin supplement",
     xlab = "Final Ewe Weight (kg)",
     las = 1)
hist(Diet2$wt,
     main = "Histogram of 15 ewes final weight with lupin supplement",
     xlab = "Final Ewe Weight (kg)",
     las = 1)
