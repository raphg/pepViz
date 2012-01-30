# TODO: Add comment
# 
# Author: wjiang2
###############################################################################

setMethod("drawLegend", signature("ProbeSequence"), function(gdObject) {
			

			pushViewport(viewport(layout = grid.layout(100, 1),
							layout.pos.col = 2, layout.pos.row = 2))
			
			fill <- getPar(gdObject,"bgColor")
			range <-getPar(gdObject,"ylim")
			legendSize<-getPar(gdObject,"legendSize")
			if(is.null(legendSize))
				legendSize<-0.7
			
			legendList<-list(list(type="scale",fill=fill,range=range))
			
			legendCex<-getPar(gdObject,"legendCex")
#						browser()
			if(is.null(legendCex))
				legendCex<-getPar(gdObject,"labelCex")
			
			.drawLegend(legendList,legendSize,legendCex)
			
			popViewport(1)
		})

setMethod("drawLegend", signature("GenericArray"), function(gdObject) {
			
			pushViewport(viewport(layout = grid.layout(100, 1),
							layout.pos.col = 2, layout.pos.row = 2))
			
			legendSize <- getPar(gdObject,"legendSize")
			if(is.null(legendSize))
				legendSize<-0.3
			#			browser()
			
			intensity = GenomeGraphs:::getIntensity(gdObject)
			lwd = GenomeGraphs:::getLwd(gdObject)
			lty = GenomeGraphs:::getLty(gdObject)
			color = GenomeGraphs:::getColor(gdObject)
			pch=   GenomeGraphs:::getPch(gdObject)
			
			legendCex<-getPar(gdObject,"legendCex")
			if(is.null(legendCex))
				legendCex<-getPar(gdObject,"labelCex")
			
			pointSize<-GenomeGraphs:::getPointSize(gdObject)
			trackOverlays<-GenomeGraphs:::getTrackOverlay(gdObject)
			type<-GenomeGraphs:::getType(gdObject)
			
			sampleNames<-colnames(intensity)
			nSample<-length(sampleNames)
			
			#cylce all the other display arguments 
			type<-rep(type,length.out=nSample)
			pointSize<-rep(pointSize,length.out=nSample)
			color<-rep(color,length.out=nSample)
			lwd<-rep(lwd,length.out=nSample)
			lty<-rep(lty,length.out=nSample)
			pch<-rep(pch,length.out=nSample)
			
			nOverLay<-length(trackOverlays)
#			nLegends<-nSample+nOverLay
#			browser()
			legendList<-NULL
			if(nSample<=10&&nSample>0)
			{
				#get legend info from array data
				
				legendList<-vector("list",nSample)
				for(i in 1:nSample)
				{
					legendList[[i]]<-list(legend=sampleNames[i]
										,type=type[i]
										,size=pointSize[i]
										,pch=pch[i]
										,dp=gpar(lwd=lwd[i]
												,lty=lty[i]
												,col=color[i]
												)
										)
					}
				
			}
			if(nOverLay>0)
			{
				#get legend info from track overlays
				legendList<-c(legendList
					,mapply(function(x,y){
																
								list(legend=ifelse(y=="",class(x),y)
										,type="line"
										,size=GenomeGraphs:::getSize(x)
										,pch=NULL
										,dp=gpar(col=GenomeGraphs:::getColor(x)
												,lwd=GenomeGraphs:::getLwd(x)
												,lty=GenomeGraphs:::getLty(x)
										
										)
									)
							},trackOverlays,names(trackOverlays),SIMPLIFY=F)
					)
			}
			
			if(!is.null(legendList))
				pepViz:::.drawLegend(legendList,legendSize,legendCex)
							
			
			
			popViewport(1)
})

.drawLegend<-function(legendList,legendSize,labelCex)
{
#	browser()
			
	legendSize<-legendSize*100
	
	startPos<-10#(100-legendSize)/2
	
	nLegends<-length(legendList)
	pushViewport(dataViewport(xscale=c(0,1), yscale=c(1, nLegends+1), extension = 0,
					layout.pos.row = c(startPos,startPos+legendSize)))
	
	for(i in 1:length(legendList)){
		x<-legendList[[i]]
		if(x$type%in%c("l","line"))
		{
						
			grid.lines(x=c(0.2,0.3), y=c(i,i)
					,default.units = "native"
					,gp = x$dp)
			
		}else
		{
			if(x$type=="scale")
			{
				range<-x$range
				fill<-x$fill
				nColor<-length(fill)
				legend<-rep("",nColor)
				legend[1]<-range[1]
				legend[nColor]<-range[2]
				step<-0
				boxHeight<-1/(nColor+nColor*step+2)
				actualGap<-boxHeight*step
				xPos<-1/2
				yStart <- nLegends+1 - boxHeight
				box.width<-1/3
				##reverse the order of scale
				fill<-rev(fill)
				range<-rev(range)
#				browser()				
				grid.text(label = formatC(range[1], format="f",digits=2), x=xPos+box.width , y = yStart,
						just = c("left","top"), default.units="native",gp=gpar(cex=labelCex))

				
				for(j in 1:nColor) {
					yPos <- yStart -(j-1)*(boxHeight+actualGap)
					grid.rect(x=xPos,y=yPos,width=box.width,height=boxHeight,
							gp=gpar(col ="transparent"
									, fill=fill[j]), default.units="native",
							just=c("left","top"))
					
				}
				
				grid.text(label = formatC(range[2], format="f",digits=2)
						, x=xPos+box.width,y =  yStart -nColor*(boxHeight+actualGap),
						just = c("left","top"), default.units="native",gp=gpar(cex=labelCex))

				

				
			}else
			{
				grid.points(x=0.2, y=i, default.units = "native",gp = x$dp
						,size=unit(x$size,"char")
						,pch=x$pch)	
			}
			
		}
		
		grid.text(label = x$legend, x=0.4 , y = i,
				just = c("left","center"), default.units="native"
				,gp=gpar(cex=labelCex
				)
		)
		
	}	
	popViewport(1)
}