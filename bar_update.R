library(ggplot2)
library(RColorBrewer)
library(ggthemes)
library(gridExtra)

setwd("raw_data")

#Create a function for the look of my charts
#Used minimaxir's code as base R code to work off of
my_theme <- function() {

  # Define colors for the chart
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[4]
  color.panel = palette[3]
  color.axis.text = palette[9]
  color.axis.title = palette[9]
  color.title = palette[9]

  # Create basic construction of chart
  theme_bw(base_size=9, base_family="Georgia") + 

  # Set the entire chart region to a light gray color
  theme(panel.background=element_rect(fill=color.panel, color=color.background)) +
  theme(plot.background=element_rect(fill=color.background, color=color.background)) +
  theme(panel.border=element_rect(color=color.background)) +

  # Format grid
  theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
  theme(panel.grid.minor=element_blank()) +
  theme(axis.ticks=element_blank()) +

  # Format legend
  theme(legend.position="right") +
  theme(legend.background = element_rect(fill=color.panel)) +
  theme(legend.title = element_text(size=11,color=color.axis.title)) +
  theme(legend.text = element_text(size=10,color=color.axis.title)) + 

  # Format title and axes labels these and tick marks
  theme(plot.title=element_text(color=color.title, size=15, vjust=0.5, hjust=0, face="bold")) +
  theme(axis.text.x=element_text(size=10,color=color.axis.text)) +
  theme(axis.text.y=element_text(size=10,color=color.axis.text)) +
  theme(axis.title.x=element_text(size=12,color=color.axis.title, vjust=-1, face="italic")) +
  theme(axis.title.y=element_text(size=12,color=color.axis.title, vjust=1.8, face="italic")) +

  # Plot margins
  theme(plot.margin = unit(c(.5, .5, .5, .5), "cm"))
}

 
si.data <- read.csv('sicover_update.csv')

si.data$On.Cover <- factor(si.data$On.Cover, levels = c("Male Athletes/Coaches", "Other Men", "Female Athletes/Coaches", "Female Swimsuit Models", "Other Women", "No People"))

#year-by-year all covers plot
pal2 <- c("#737373", "#4daf4a", "#377eb8", "#e41a1c", "#ff7f00", "#984ea3")

a<-ggplot(data = si.data, aes(x = Year, y=Frequency, fill = On.Cover)) + 
	scale_x_continuous(breaks=seq(2010,2015,1))+
	geom_bar(stat="identity")+ coord_flip()+ scale_fill_manual(values = pal2, guide_legend(title="On Cover")) +
	my_theme()+
	ylab("Count")+
	ggtitle(expression(atop(bold("2015: The Ascent of the 'Sports Illustrated' Female Athlete (Part I)"), atop(italic("Count of 'Sports Illustrated' Covers by Group, 2010-Present"), atop(italic("via Alex Albright 7-14-15 (thelittledataset.com/@AllbriteAllday)")),""))))

#a<-a+ scale_x_discrete(labels=c("2010", "2011", "2012", "2013","2014", "2015"))

#women plots
si.data <- read.csv('justwomen_update.csv')

library(ggplot2)
library(RColorBrewer)

#year-by-year women plot
#want #8dd3c7, #ffffb3, #fdb462
pal1 <- c("#377eb8", "#e41a1c", "#ff7f00")

f<-c("#8dd3c7", "#ffffb3", "#fdb462")
c<-ggplot(data = si.data, aes(x = Year, y=Frequency, fill = On.Cover)) + 
	scale_x_continuous(breaks=seq(2010,2015,1))+
    geom_bar(stat="identity")+ coord_flip()+ scale_fill_manual(values = pal1, guide_legend(title="On Cover")) + 
	my_theme()+
	ylab("Count")+
	ggtitle(expression(atop(bold("2015: The Ascent of the 'Sports Illustrated' Female Athlete (Part II)"), atop(italic("Count of 'Sports Illustrated' Covers by Female Group, 2010-Present"), atop(italic("via Alex Albright 7-14-15 (thelittledataset.com/@AllbriteAllday)")),""))))



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

multiplot(a,c)