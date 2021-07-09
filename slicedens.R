# x, y: data
# slices: number of horizontal slices through the data
# lboost: coefficient to increase the height of the lines
# gboost: coefficient to increase the height of the graph (ylim)
# xinc: horizontal offset for each succesive slice 
#		(typically something like 1/80)
# yinc: vertical offset for each succesive slice
# bcol: background color
# fcol: fill color for each slice (polygon)
# lcol: line color for each slice
# lwidth: line width
# extend: Boolean to extend lines to edge of plot area
# densopt: list of strings containing density() arguments that are passed verbatim.

# NB if you want to cycle slice colors through vectors, you
#	need to change the function code; it sounds like a
#	pretty bad idea to me, but each to their own.

slicedens<-function(x,y,slices=50,lboost=1,gboost=1,xinc=0,yinc=0.01,
		bcol="black",fcol="black",lcol="white",lwidth=1,extend=FALSE,
		densopt=NULL) {
	ycut<-min(y)+((0:(slices))*(max(y)-min(y))/slices)
	height<-gboost*((slices*yinc)+max(density(x)$y))
	plot(	c(min(x),max(x)+((max(x)-min(x))/4)),
		c(0,height),
		xaxt="n",yaxt="n",ylab="",xlab="")
	rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col=bcol)
	for(i in slices:1) {
		miny<-ycut[i]
		maxy<-ycut[i+1]
		gx<-(i-1)*(max(x)-min(x))*xinc
		gy<-(i-1)*(height)*yinc
		dd<-do.call(density,append(list(x=x[y>=miny & y<maxy]),
						   densopt))
		polygon(dd$x+gx,lboost*dd$y+gy,col=fcol)
		lines(dd$x+gx,lboost*dd$y+gy,col=lcol,lwd=lwidth)
		if(extend) {
			lines(c(par("usr")[1],dd$x[1]+gx),
				rep(lboost*dd$y[1]+gy,2),col=lcol)
			lines(c(dd$x[length(dd$x)]+gx,par("usr")[2]),
				rep(lboost*dd$y[length(dd$y)]+gy,2),col=lcol)
		}
	}
}

# Example 1:
y<-runif(5000,min=-1,max=1)
x<-runif(5000,min=-1,max=1)+rnorm(5000,mean=1/(y+1.1),sd=0.8-(y*0.5))
slicedens(x,y,lboost=0.8,fcol=rgb(0,0,0,200,maxColorValue=255))

# Example 2:
slicedens(x=iris$Sepal.Width,y=iris$Sepal.Length,slices=12,yinc=0.05,
		lboost=0.03,gboost=0.4,fcol=rgb(0,0,0,200,maxColorValue=255),
		extend=TRUE,densopt=list(kernel="cosine",adjust=0.5))
