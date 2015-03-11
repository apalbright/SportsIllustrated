si.data <- read.csv('sicover.csv')

library(ggplot2)
library(RColorBrewer)
library(ggthemes)
library(gridExtra)

si.data$On.Cover <- factor(si.data$On.Cover, levels = c("Male Athletes/Coaches", "Other Men", "Female Athletes/Coaches", "Female Swimsuit Models", "Other Women", "No People"))

#year-by-year all covers plot
pal2 <- c("#737373", "#4daf4a", "#377eb8", "#e41a1c", "#ff7f00", "#984ea3")

a<-ggplot(data = si.data, aes(x = Year, y=Frequency, fill = On.Cover)) + 
	geom_bar(stat="identity")+ coord_flip()+ scale_fill_manual(values = pal2, guide_legend(title="On Cover")) +
	labs(title = "Who is Featured on 'Sports Illustrated' Covers? (Year-by-Year)") +
	theme_tufte(ticks=FALSE, base_family="Georgia")


#total all covers plot
si.data1 <- read.csv('sicovertot.csv')

si.data1$On.Cover <- factor(si.data1$On.Cover, levels = c("Male Athletes/Coaches", "Other Men", "Female Athletes/Coaches", "Female Swimsuit Models", "Other Women", "No People"))

b<-ggplot(data = si.data1, aes(x = Year, y=Frequency, fill = On.Cover)) + 
	geom_bar(stat="identity")+ coord_flip()+ scale_fill_manual(values = pal2, guide=FALSE) + 
	labs(title = "Who is Featured on 'Sports Illustrated' Covers? (Total)")+
	theme_tufte(ticks=FALSE, base_family="Georgia")

#women plots
si.data <- read.csv('justwomen.csv')

library(ggplot2)
library(RColorBrewer)

#year-by-year women plot
#want #8dd3c7, #ffffb3, #fdb462
pal1 <- c("#377eb8", "#e41a1c", "#ff7f00")

f<-c("#8dd3c7", "#ffffb3", "#fdb462")
c<-ggplot(data = si.data, aes(x = Year, y=Frequency, fill = On.Cover)) + 
    geom_bar(stat="identity")+ coord_flip()+ scale_fill_manual(values = pal1, guide_legend(title="On Cover")) + 
    labs(title = "Who are the Women on 'Sports Illustrated' Covers? (Year-by-Year)")+
	theme_tufte(ticks=FALSE, base_family="Georgia")

#total women plot
si.data1 <- read.csv('justwomentot.csv')
d<-ggplot(data = si.data1, aes(x = Year, y=Frequency, fill = On.Cover)) + 
    geom_bar(stat="identity")+ coord_flip()+ scale_fill_manual(values = pal1, guide=FALSE) + 
    labs(title = "Who are the Women on 'Sports Illustrated' Covers? (Total)")+
	theme_tufte(ticks=FALSE, base_family="Georgia")

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(a,b,c,d)
