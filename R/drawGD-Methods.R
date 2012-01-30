####################################################################################
#overwrite method for GenericArray to add font size control   
#and filter the trackoverlay by minBase and maxBase to fix the bug in the original method drawTrackOverlay
#################################################################################
setMethod("drawGD", signature("GenericArray"), function(gdObject, minBase, maxBase, vpPosition) {
			
			legendRatio<-getPar(gdObject,"legendRatio")
			trackGap<-getPar(gdObject,"trackGap")
			if(is.null(trackGap))
				trackGap<-0.1
			#create 3(two marginal rows+1 major row)*2(major column+legend column) grid 
#			browser()
			pushViewport(viewport(layout = grid.layout(nrow=3,ncol=2
														,widths=unit(c(1-legendRatio,legendRatio), "native")
														,height=unit(c(trackGap/2,1-trackGap,trackGap/2), "native")
														)
									,layout.pos.col = 1, layout.pos.row =vpPosition
								)
						)
			
			intensity = GenomeGraphs:::getIntensity(gdObject)
			probeStart = GenomeGraphs:::getProbeStart(gdObject)
						
			## abstract this operation for all functions :
			ylim <- getPar(gdObject, "ylim")
			xlim <- getPar(gdObject, "xlim")
			if (is.null(xlim)) xlim <- c(minBase, maxBase)
			if (is.null(ylim)) ylim <- range(intensity, na.rm=TRUE)
			
			##select [2][1] subsection as the current veiwport
			pushViewport(dataViewport(xData = xlim, yData = intensity, extension = 0,
							layout.pos.col=1, layout.pos.row = 2, yscale = ylim))
			
			lwd = GenomeGraphs:::getLwd(gdObject)
			lty = GenomeGraphs:::getLty(gdObject)
			color = GenomeGraphs:::getColor(gdObject)
			cex=GenomeGraphs:::getCex(gdObject)
			
			
			isDisplay<-probeStart>=minBase&probeStart<=maxBase
			probeStart<-probeStart[isDisplay]
			intensity<-intensity[isDisplay,,drop=FALSE]
			
			if(length(GenomeGraphs:::getProbeEnd(gdObject)) > 0) {
				probepos=cbind(getProbeStart(gdObject), getProbeEnd(gdObject))
				for(s in seq(along=intensity[1,])){
					for(p in seq(along=intensity[,1])){
						grid.lines(c(probepos[p,1],probepos[p,2]), c(intensity[p,s],intensity[p,s]),
								default.units = "native", gp = gpar(col=color[1], lwd = lwd, lty = lty))
					}
				}
			}else{
				if(GenomeGraphs:::getType(gdObject) == "line"){
					 
					ord = order(probeStart)
					probepos = probeStart[ord]
					intensity = intensity[ord,,drop=FALSE]
					for(p in seq(along=intensity[1,])){
						lwdInd = 1
						colInd = 1
						ltyInd = 1
						if(length(color) == length(intensity[1,])) colInd = p
						if(length(lwd) == length(intensity[1,])) lwdInd = p
						if(length(lty) == length(intensity[1,])) ltyInd = p
						grid.lines(probepos, intensity[,p], default.units = "native",
								gp = gpar(col=color[colInd], lwd = lwd[lwdInd], lty = lty[ltyInd]))
					}
				}else{
					
					pSize = GenomeGraphs:::getPointSize(gdObject)
					pch = GenomeGraphs:::getPch(gdObject)
					for(p in seq(along=intensity[1,])){
						pSizeInd = 1
						colInd = 1
						pchInd = 1
						if(length(color) == ncol(intensity)) colInd = p
						if(length(pSize) == ncol(intensity)) pSizeInd = p
						if(length(pch) == ncol(intensity)) pchInd = p
						
						grid.points(probeStart, intensity[, p], default.units = "native",
								gp = gpar(col=color[colInd]),size = unit(pSize[pSizeInd],"char"),
								pch = pch[pchInd])
					}
				}
			}

			
			
#			browser()
			##truncate any overlay and update gdObject with filtered overlay
			trackOverlays<-GenomeGraphs:::getTrackOverlay(gdObject)
			if(length(trackOverlays)>0)
			{	for(j in 1:length(trackOverlays))
				{
					
					if(class(trackOverlays[[j]])=="Smoothing")
					{
						x<-trackOverlays[[j]]@x
						y<-trackOverlays[[j]]@y
						overlayInd<-x>=minBase&x<=maxBase
						x<-x[overlayInd]
						y<-y[overlayInd]
						trackOverlays[[j]]@x<-x
						trackOverlays[[j]]@y<-y
					}
				}
			}
			gdObject@trackOverlay<-trackOverlays
			GenomeGraphs:::.drawTrackOverlays(gdObject, minBase, maxBase)
#			browser()
			axisCex<-getPar(gdObject,"axisCex")
			if(is.null(axisCex))
				axisCex<-getPar(gdObject,"labelCex")
			grid.yaxis(gp = gpar(cex=axisCex))
			popViewport(1)
			
			if(isLegend(gdObject))
			{
#				current.vpTree()
				
				
#				pushViewport(dataViewport(xData=c(1, 10), yscale=c(1, nLegends+1), extension = 0,
#					layout.pos.col = 2, layout.pos.row = 1))
			
				drawLegend(gdObject)
				
#				popViewport(1)
			}
			
			popViewport(1)
		})	
		
##
## overwrite the drawGD methods in order to add the littleTicksLabel option. 
##and it has to be renamed as a function  avoid masking issue from GenomeGraphs
	.drawGenomeAxis<- function(gdObject, minBase, maxBase, vpPosition) {
			pushViewport(dataViewport(xData=c(minBase, maxBase), yscale=c(-1, 1), extension = 0,
							layout.pos.col = 1, layout.pos.row = vpPosition))
			grid.lines(c(minBase, maxBase), c(0, 0), default.units = "native")
			
			## here we plot the top level ticks
			tck <- GenomeGraphs:::.ticks(c(minBase, maxBase))
			browser
			## reformat if byValue != 1
			tckText <- tck
			formatValue <- "d" #to be passed to formatC
			byValue <- getPar(gdObject, "byValue")
			littleTicksLabel<-getPar(gdObject,"littleTicksLabel")
			littleTicksLabel<-ifelse(is.null(littleTicksLabel),FALSE,littleTicksLabel)
			
			if(byValue != 1) {
				tckText <- tckText/byValue
				formatValue <- "g"
			}
			
			y <- getPar(gdObject,"distFromAxis")*rep(c(.4, -.4), length(tck))[1:length(tck)]
			labelPos <- getPar(gdObject,"labelPos")
			y <- switch(labelPos,"alternating" = y,"revAlternating" = -y,"above" = abs(y),"below"= -abs(y))
			
			grid.text(label = formatC(tckText, format = formatValue), x = tck, y = y, just = c("centre", "centre"),
					gp = gpar(cex=getPar(gdObject,"cex")), default.units = "native")
			
			if (GenomeGraphs:::plotLittleTicks(gdObject)) {
				if (!(minBase %in% tck))
					tck <- c(minBase, tck)
				if (!(maxBase %in% tck))
					tck <- c(tck, maxBase)
				
				if (mean(diff(tck)) > diff(range(tck))/10) {
					for (j in 1:(length(tck) - 1)) {
						stcks <- GenomeGraphs:::.ticks(tck[j:(j+1)])
						stcks <- stcks[c(-1, -length(stcks))]
						
						grid.segments(x0 = stcks, x1 = stcks, y0 = -0.1, y1 = 0.1,  default.units = "native")
						
						if(littleTicksLabel)
						{
							y <- rep(c(.15, -.15), length(stcks))[1:length(stcks)]
							grid.text(label=formatC(stcks, format="d"), x = stcks, y = y,
									just = c("centre", "centre"), gp = gpar(cex=getPar(gdObject,"cex")*0.8), default.units = "native")
						}
						
					}
				}
			}
			
			if(GenomeGraphs:::getAdd53(gdObject)){
				grid.text(label=formatC("5'", format="d"), x = minBase, y = 0.3,
						just = c("centre", "centre"), gp = gpar(cex=.75), default.units = "native")
				grid.text(label=formatC("3'", format="d"), x = maxBase , y = 0.3,
						just = c("centre", "centre"), gp = gpar(cex=.75), default.units = "native")
			}
			if(GenomeGraphs:::getAdd35(gdObject)){
				grid.text(label=formatC("3'", format="d"), x = minBase , y = -0.3,
						just = c("centre", "centre"), gp = gpar(cex=.75), default.units = "native")
				grid.text(label=formatC("5'", format="d"), x = maxBase , y = -0.3,
						just = c("centre", "centre"), gp = gpar(cex=.75), default.units = "native")
			}
			
			grid.segments(x0 = tck, x1 = tck, y0 = -0.25, y1 = 0.25,  default.units = "native")
			popViewport()
		}



setMethod("drawGD", signature("PepAxis"), function(gdObject, minBase, maxBase, vpPosition) {
#			browser()
			anno = getAnnotations(gdObject)
			
			legendRatio<-getPar(gdObject,"legendRatio")
			
			trackGap<-getPar(gdObject,"trackGap")
			if(is.null(trackGap))
				trackGap<-0.1
			#create 3(two marginal rows+1 major row)*2(major column+legend column) grid 
#			browser()
			pushViewport(viewport(layout = grid.layout(nrow=3,ncol=2
									,widths=unit(c(1-legendRatio,legendRatio), "native")
									,height=unit(c(trackGap/2,1-trackGap,trackGap/2), "native")
							)
							,layout.pos.col = 1, layout.pos.row =vpPosition
					)
			)
			
			pushViewport(dataViewport(xData=c(minBase, maxBase), yscale=c(-1, 1), extension = 0,
							layout.pos.col = 1, layout.pos.row = 2))
			
			gaxis1<-pepViz:::.makeGenomeAxis(add53 = FALSE, add35 = FALSE
					,littleTicks = GenomeGraphs:::plotLittleTicks(gdObject)
					, dp = gdObject@dp)
			
#						browser()
			showAxis<-getPar(gdObject,"showAxis")
			if(is.null(showAxis))showAxis=TRUE
			if(showAxis)
			{
				.drawGenomeAxis(gaxis1,minBase=minBase,maxBase=maxBase,vpPosition=vpPosition)
				ytext<-0.5
			}else
			{
				ytext<-0
			}

#			browser()		
			##add sequence if needed
			seqsAnno<-""
			if(nrow(anno)>0)
			{
				dna = getPar(gdObject,"dna")#getDNA(gdObject)
				if(dna)
					seqsAnno<-getDNA(anno[1,])[[1]]
				else
					seqsAnno<-getAA(anno[1,])[[1]]
				
			}
							
			seqs<-pepViz:::getAASeq(gdObject)
			if(length(seqs)==0)
			{
				seqs<-seqsAnno
				
									
			}else
			{
				diff1<-nchar(seqsAnno)-nchar(seqs)
				if(diff1>0)#if input seq is shorter than anno seq,then append - symbols
					seqs<-paste(seqs,paste(rep("-",diff1),collapse=""),sep="")	
			}
			
			seqs<-substr(seqs,minBase,maxBase)
			
			showSeq<-getPar(gdObject,"showSeq")
			
			width<-maxBase-minBase
#			browser()
			if(width<=150&&showSeq==TRUE&&nchar(seqs)>0)##show seq only when base number is not big so that letters won't get too crowded
			{
				for (j in 1:nchar(seqs))
				{
					xtext=minBase+as.integer((as.double(width)/nchar(seqs))*j)
					grid.text(label=substr(seqs,j,j),x=xtext,y=ytext,
							gp = gpar(cex = getPar(gdObject,"cex")), 
							default.units = "native")
				}
			}else
			{
				if(getNC(gdObject)){
					grid.text(label=formatC("NH", format="d"), x = minBase, y = 0.3,
							just = c("right", "bottom"), gp = gpar(cex=getPar(gdObject,"cex")), default.units = "native")
					grid.text(label=formatC("COOH", format="d"), x = maxBase , y = 0.3,
							just = c("left", "bottom"), gp = gpar(cex=getPar(gdObject,"cex")), default.units = "native")
				}
			}

			popViewport(2)
			
			
	})

	

setMethod("drawGD", signature("ProbeSequence"), function(gdObject, minBase, maxBase, vpPosition) {
			legendRatio<-getPar(gdObject,"legendRatio")
			
			trackGap<-getPar(gdObject,"trackGap")
			if(is.null(trackGap))
				trackGap<-0.1
			#create 3(two marginal rows+1 major row)*2(major column+legend column) grid 
#			browser()
			pushViewport(viewport(layout = grid.layout(nrow=3,ncol=2
									,widths=unit(c(1-legendRatio,legendRatio), "native")
									,height=unit(c(trackGap/2,1-trackGap,trackGap/2), "native")
							)
							,layout.pos.col = 1, layout.pos.row =vpPosition
					)
			)
			
			
			probeStart<-getProbeStart(gdObject)
			sequence<-getProbeSequence(gdObject)
			intensity<-getIntensity(gdObject)
			
			intRange<-getPar(gdObject,"ylim")
			if(is.null(intRange))
			{
				intRange<-range(intensity)
			}
			
			
			
			totalRows<-length(probeStart)
			
#			browser()
			##viewport for multipe probe subtracks
			pushViewport(viewport(layout = grid.layout(nrow=totalRows,ncol=1),
							layout.pos.col = 1, layout.pos.row =2))
		
			for(curR in 1:totalRows)
			{
#				totalRows
				curProbName<-names(probeStart)[curR]
				probeArray<-data.frame(probeStart = probeStart[[curR]]
						,sequence = sequence[[curR]]
						,intensity = intensity[[curR]])
				probeArray<-subset(probeArray,probeStart>=minBase&probeStart<=maxBase)
				
				
				color<-getPar(gdObject,"color")
				bgColor<-getPar(gdObject,"bgColor")
				alpha<-getPar(gdObject,"alpha")
				width<-maxBase-minBase
				nSeq<-width/15
				
				#group the 15mers by positions
				probeArray$posInterval <- cut(probeArray$probeStart,seq(minBase,maxBase,20),right=F,include.lowest=T)
				
				#decide the maximum tiers where sequences to be displayed
				nRows<-max(table(probeArray$posInterval))
				##viewport for the current probe subtrack
				pushViewport(dataViewport(xData=c(minBase, maxBase), yscale=c(1, nRows+1), extension = 0,
								layout.pos.col = 1, layout.pos.row = curR))
				
				showSeq<-ifelse(width<=200,T,F)
				
#			browser()
				
				##include the range value for cutting function
				intVec<-c(probeArray$intensity,intRange)								
				#get color by intensity
				probeArray$intensityInterval <- cut(intVec,breaks=length(bgColor),right=F,include.lowest=T)[1:nrow(probeArray)]
				
				idCex<-getPar(gdObject,"idCex")
				seqCex<-idCex/nSeq
				setPar(gdObject,"seqCex",seqCex)
				#calculate x,y coordinates of each character
				by(probeArray,probeArray$posInterval,function(curData){
							for(i in 1:nrow(curData))
							{
#							browser()
								
								AAsequence<-as.character(curData$sequence[i])
								xstart<-curData[i,"probeStart"]-0.5
								xright<-min(xstart+nchar(AAsequence),maxBase+0.5)
								grid.rect(x=xstart,y=i
										,width = xright-xstart,
										,height = 0.9#idCex/nSeq
										,gp =gpar(fill=bgColor[curData$intensityInterval[i]]
												,col="transparent"
												,alpha=alpha
										)
										,default.units = "native", just = c("left", "center"))
								if (showSeq)
								{
									for (j in 1:nchar(AAsequence))
									{
#									browser()
										xPos<-curData[i,"probeStart"]+j-1
										if(xPos<=maxBase)
										{
											grid.text(label=substr(AAsequence,j,j)
													,x=xPos
													,y=i,
													,gp = gpar(col=color
															,cex =seqCex
													#														,fontsize=8
													)
													,default.units = "native")	 
										}
									}
								}
							}
							
						})
				popViewport(1)
			}
			
			popViewport(1)
			
			
			
			
			if(isLegend(gdObject))
			{
#				current.vpTree()
#				pushViewport(dataViewport(xData=c(1, 10), yscale=c(1, length(bgColor)+1), extension = 0,
#					layout.pos.col = 2, layout.pos.row = 1))
#	browser()
				drawLegend(gdObject)			
				
#				lgTrack<-makeLegend(text=""
#										,dp= DisplayPars(size=0.05
#														,fill=mypalette
#														,cex=getPar(gdObject,"lCex")
##														,step=2
#														,horiz=F
#														,range=intRange
#														)
#										)
#				drawGD(lgTrack,minBase=1,maxBase=10,vpPosition=1)
#				popViewport(1)
				
				
			}

			
			popViewport(1)
			
			
})

setMethod("drawGD", signature("GenericAnno"), function(gdObject, minBase, maxBase, vpPosition) {
#			browser()
	legendRatio<-getPar(gdObject,"legendRatio")
	trackGap<-getPar(gdObject,"trackGap")
	if(is.null(trackGap))
		trackGap<-0.1
	#create 3(two marginal rows+1 major row)*2(major column+legend column) grid 
#			browser()
	pushViewport(viewport(layout = grid.layout(nrow=3,ncol=2
							,widths=unit(c(1-legendRatio,legendRatio), "native")
							,height=unit(c(trackGap/2,1-trackGap,trackGap/2), "native")
					)
					,layout.pos.col = 1, layout.pos.row =vpPosition
			)
	)
	
#	dd<-current.vpTree()
#	str(dd)
	anno = getAnnotations(gdObject)
	nAnno<-nrow(anno)
	probeStart = getProbeStart(gdObject)
	dna = getPar(gdObject, "dna")#getDNA(gdObject)
	showID = getPar(gdObject, "showID")#getShowID(gdObject)
#	showSeq = getPar(gdObject, "showSeq")#gdObject@showSeq ###--mike
#	isTips<- getPar(gdObject, "isTips")#gdObject@isTips
	idCex<- getPar(gdObject, "idCex")
	## abstract this operation for all functions :
	xlim <- getPar(gdObject, "xlim")
	ylim <- getPar(gdObject, "ylim")
	if (is.null(xlim)) xlim <- c(minBase, maxBase)
	if (is.null(ylim)) ylim <- c(-3, 3)
#	  browser()
			
	horiz = getPar(gdObject, "horiz")#getHoriz(gdObject)
	size<-getPar(gdObject,"size")
			  
	tmp<-vector(length=nAnno)
	for (i in seq(1:nAnno))
	   tmp[i]=0
	
	  # keep a list of slots available in the current row
	row_start=xlim[1]
	row_end=xlim[2]
  
	##check if overlap exists in the track,if not, then simply draw each region one by one
	##without any space calculations
	ranges<-getHXB2Coordinates(anno)#do.call("rbind",lapply(anno, getHXB2Coordinates))
	#order by start positions
	ranges<-ranges[order(ranges[,1]),,drop=FALSE]
	# posVec<-unmatrix(ranges,byrow=TRUE)
	posVec<-as.vector(t(ranges))
	NonOverlap<-all(sort(posVec)==posVec)

	showAnno=vector(length=nAnno)
	showAnno<-rep("center",length(showAnno))
	
#	tracGap<-getPar(gdObject,"trackGap")
	height<-10
	
#	if(size<=tracGap)
#		trackGap<-height/9 #force the gap equal 1/10 of the total size if the assigned value is not valid
#	else
#		trackGap<-tracGap*height/(size-tracGap)
#	browser()
  	if (!horiz)
  	{
#	  browser()
      nesting=0
      for (i in (nAnno:1))
      {
		start=getHXB2Coordinates(anno[i,])[1]-probeStart
		end=getHXB2Coordinates(anno[i,])[2]-probeStart
		if (!dna)
		{
		    start=as.integer((start)/3)
		    end=as.integer((end)/3)
		}
		for(j in (nAnno:1))
		{
		  start2=getHXB2Coordinates(anno[j,])[1]-probeStart
		  end2=getHXB2Coordinates(anno[j,])[2]-probeStart
		  if (!dna)
		  {
		    start2=as.integer((start2)/3)
		    end2=as.integer((end2)/3 + 1)
		  }
		  if (i != j & start <= start2 & end >= end2)
		  {
		    tmp[i]=max(tmp[i],tmp[j]+1)
		    if (tmp[i] > nesting)
			nesting=tmp[i]
		  }
		}
      }
	  yscale<-c(0, 15+10*nesting)
      pushViewport(dataViewport(xData = xlim, extension = 0,
				  clip = TRUE, yscale = yscale,
				layout.pos.col=1, layout.pos.row = 2))

 	}
  	else
  	{
#		browser()
      # count required number of lines for annotations (accounting for labels)
      totLines = 1
	  
      starts=vector(length=nAnno)
      ends=vector(length=nAnno)
      line=vector(length=nAnno)
      availableStart=matrix(nrow=nAnno,ncol=nAnno+1)
      availableEnd=matrix(nrow=nAnno,ncol=nAnno+1)
#	  
#	  browser()
	
	  
      for (i in 1:(nAnno))
      {
		 
		  
		line[i]=0
		for (j in 1:(nAnno+1))
		{
		    if (j==1)
		    {
			availableStart[i,j]=row_start
			availableEnd[i,j]=row_end
		    }
		    else
		    {
			availableStart[i,j]=-1
			availableEnd[i,j]=-1
		    }
		}
      }
	 
	  
	  if(NonOverlap)
	  {
#		  showAnno<-rep("center",length(showAnno))
	  }else
	  {
		  ##calculate space
		  for (i in (nAnno):1)
		  {
			  start=getHXB2Coordinates(anno[i,])[1]-probeStart
			  end=getHXB2Coordinates(anno[i,])[2]-probeStart
			  if (!dna)
			  {
				  start=as.integer((start)/3)
				  end=as.integer((end)/3)
			  }
#		browser()
			  # is this a candidate to display?
			  if (start <= row_end && end >= row_start)
			  { 
				  name=anno[[i]]@name
				  fits=FALSE
				  lines=1
				  while (!fits && lines <= nAnno) 
				  {
#				starts[i]=max(start,row_start)
#				ends[i]=min(end,row_end)
					  starts[i]=start
					  ends[i]=end
					  
					  # check each available piece of the current line
					  for (j in (1:(nAnno+1)))
					  {
						  if (availableStart[lines,j] < 0)
							  break
						  
						  start_buffer = starts[i] - availableStart[lines,j]
						  end_buffer = availableEnd[lines,j] - ends[i]
						  
						  # Does it span the whole row?
						  if (availableStart[lines,j] == row_start && 
								  availableEnd[lines,j] == row_end &&
								  starts[i] <= row_start && ends[i] >= row_end)
						  {
							  line[i]=lines
							  print(paste("Removing all slot ",availableStart[lines,j],availableEnd[lines,j]))
							  for (k in (j:(nAnno)))
							  {
								  availableStart[lines,k] = availableStart[lines,k+1]
								  availableEnd[lines,k] = availableEnd[lines,k+1]
							  }
							  availableStart[lines,nAnno+1]=-1
							  availableEnd[lines,nAnno+1]=-1
							  if (i > 1) lines=lines+1
							  totLines=max(totLines,lines)
							  showAnno[i]="center"
							  fits=TRUE
							  print("Fit: filled line")
							  if (row_end - row_start < 6*nchar(as.character(name)))
							  {
								  anno[[i]]@name=""
							  }
							  break
						  }
						  # If not, does the bar fit 
						  else if (starts[i] >= availableStart[lines,j] || 
								  ends[i] <= availableEnd[lines,j])
						  {
							  
							  ##if the reason buffer overflow is the axis boundary itself instead of overlapping
							  ## with other feature, then consider it as "fit" instead of moving to next line
							  if((starts[i] < availableStart[lines,j]&&availableStart[lines,j]==row_start)||
									  (ends[i] > availableEnd[lines,j]&&availableEnd[lines,j]==row_end)||
									  (starts[i] >= availableStart[lines,j]&&ends[i] <= availableEnd[lines,j]))
							  {	
								  print("bar fits")
								  # can we show the name in the bar itself?
								  if (!showID || (ends[i]-starts[i] > 6*nchar(as.character(name))))
								  {
									  line[i]=lines
									  showAnno[i]="center"
									  fits=TRUE
									  print("Fit: label center")
								  }
								  
								  # If label on outside, where do we put it?
								  else if (end_buffer > 6*nchar(as.character(name)))
								  {
									  end = ends[i] + 6*nchar(as.character(name))
									  ends[i]=min(end,row_end)
									  line[i]=lines
									  showAnno[i]="right"
									  fits=TRUE
									  print("Fit: label right")
								  }
								  else if (start_buffer > end_buffer && 
										  start_buffer > 6*nchar(as.character(name)))
								  {
									  start = starts[i] - 6*nchar(as.character(name))
									  starts[i]=max(start,row_start)
									  line[i]=lines
									  showAnno[i]="left"
									  fits=TRUE
									  print("Fit: label left")
								  } 
								  else if (availableStart[lines,j] == row_start && 
										  availableEnd[lines,j] == row_end)
								  {
									  line[i]=lines
									  showAnno[i]="center"
									  fits=TRUE
									  print("Fit: without label")
								  }
								  if (fits)
								  {
									  # shrink existing list of available or divide it
									  if (start_buffer == 0 && end_buffer == 0)
									  {
										  print(paste("Removing all slot ",availableStart[lines,j],availableEnd[lines,j]))
										  for (k in (j:(nAnno)))
										  {
											  availableStart[lines,k] = availableStart[lines,k+1]
											  availableEnd[lines,k] = availableEnd[lines,k+1]
										  }
										  availableStart[lines,nAnno+1]=-1
										  availableEnd[lines,nAnno+1]=-1
										  if (i > 1) lines=lines+1
										  totLines=max(totLines,lines)
									  }
									  else if (start_buffer == 0)
										  availableStart[lines,j]=ends[i]+1
									  else if (end_buffer == 0)
										  availableEnd[lines,j]=starts[i]-1
									  else
									  {
										  # remove current slot & replace 
										  # with slots on either side
										  print(paste("Removing partial slot ",starts[i],ends[i]))
										  tempList1<-list(c(availableStart[lines,j],starts[i]-1))
										  tempList2<-list(c(ends[i]+1,availableEnd[lines,j]))
										  print(paste("Adding slot ",tempList1[[1]][1],tempList1[[1]][2]))
										  availableStart[lines,j]=tempList1[[1]][1]
										  availableEnd[lines,j]=tempList1[[1]][2]
										  print(paste("Adding slot ",tempList2[[1]][1],tempList2[[1]][2]))
										  for (k in (j:nAnno+1))
										  {
											  if (availableStart[lines,k]<0)
											  {
												  availableStart[lines,k]=tempList2[[1]][1]
												  availableEnd[lines,k]=tempList2[[1]][2]
												  break
											  }
										  }
									  }
									  break
								  }
							  }
						  }
						  else
						  {
							  print("No Fit: moving to next piece")
						  }
					  }
					  # if we can not write the bar (or label), then it does not "fit" 
					  # so try on the next line...
					  if (!fits)
					  {
						  print("No Fit: ran out of pieces, moving to next line")
						  lines=lines+1
						  totLines=max(totLines,lines)
					  }
				  }
				  if (!fits && lines > nAnno) {
					  print("No Fit: ran out of lines")
				  }
			  }
			  else
			  {
				  print("No Fit: annotation outside bounds!")
			  }
		  }
	  }

#		height<-10
		#relative unit for actual feature region
		regionHight<-height*totLines
		#native unit for gap
		
		
#		browser()
#		if(size<=tracGap)
#			trackGap<-regionHight/9 #force the gap equal 1/10 of the total size if the assigned value is not valid
#		else
#			trackGap<-tracGap*regionHight/(size-tracGap)
	
		yscale<-regionHight#+trackGap#relative unit for the whole track
#		yscale<-5+regionHight
    	pushViewport(dataViewport(xData = xlim, extension = 0,
				  clip = TRUE
  				,height=size,
				
  				, yscale = c(0, yscale)
#				, yscale = c(0, 5*totLines)
				,layout.pos.col=1, layout.pos.row = 2))
  	}
	
  ###actual drawing
	#rectangle options
	gp1<-gpar(col = "transparent", fill = getPar(gdObject, "fill"),alpha=getPar(gdObject, "alpha"))
	#rectangle border options
	gp2<-gpar(lwd=getPar(gdObject,"lwd"),col=getPar(gdObject,"color"),lty=getPar(gdObject,"lty"))
	
	for(i in seq(length = nAnno)) 
  	{
  
		
		
		orig_start=getHXB2Coordinates(anno[i,])[1]-probeStart
		orig_end=getHXB2Coordinates(anno[i,])[2]-probeStart
		if(!dna)
    	{
			orig_start=as.integer((orig_start)/3)
			orig_end=as.integer((orig_end)/3)
	    }
    	start=max(orig_start,row_start)
    	end=min(orig_end,row_end)

		name=getName(anno[i,])
		name<-as.character(name)
		#show sequence either as a label (below name) or as a "tooltip"
#		seqs<-ifelse(dna,getGenome(anno[[i]]),getAA(anno[[i]]))
		##calculate the actual options of plotted regions before finally draw them 
	    if (horiz && showAnno[i] != "none")
	    {
#			height = 5
			width = end - start
#			rot = 0
#			ybox=5+10*(line[i]-1)
			if(NonOverlap)
				ybox=0
			else
				ybox=height*(line[i]-1)#ybox=trackGap+height*(line[i]-1)
			
							
			if (showID)
			{
			  if (showAnno[i] == "center")
			  {
				  xtext=start+as.integer(width/2)
				  jtext = c("center", "center")
			  }
			  else if (showAnno[i] == "right")
			  {
				  xtext=end
				  jtext = c("left", "center")
			  }
			  else if (showAnno[i] == "left")
			  {
				  xtext=starts[i]
				  jtext = c("left", "center")
			  }
			  
			  ytext=10*(line[i]-1)+as.integer(2*height/3)
#			  ytext=trackGap+10*(line[i]-1)+as.integer(2*height/3)
				 
			}
		}else if (!horiz)
    	{
#			browser()
			height = 10+10*tmp[i]
			width = end - start
			
			ybox=2
			jtext=c("center", "center")
			
			ytext=8+10*tmp[i]
			xtext=(as.double(start)+as.double(end))/2+6*tmp[i]
			
		}
		
		if(showAnno[i] != "none")
		{
			##add sequence as svg tooltops
#			if(isTips)
#			{
##				   browser()
#				setSVGShapeToolTip(title=pepViz:::uniEncodeString(as.character(name)),desc=seqs,sub.special=FALSE)
#			}

			
			###add rectangle		
#		browser()
			
			grid.rect(end, ybox, width = width,
					height = height, gp =gp1 ,
					default.units = "native", just = c("right", "bottom"))
			grid.lines(x=c(start,end),y=c(ybox+height,ybox+height), default.units = "native",gp=gp2)#top bound
			grid.lines(x=c(start,end),y=c(ybox,ybox), default.units = "native",gp=gp2)#bottom bound
			#draw left and right bounaries only when the respective feature boundarie falls in the current range
			if(orig_start>=row_start)
				grid.lines(x=c(start,start),y=c(ybox,ybox+height), default.units = "native",gp=gp2)#left bound
			if(orig_end<=row_end)
				grid.lines(x=c(end,end),y=c(ybox,ybox+height), default.units = "native",gp=gp2)#right bound
			
			
			##add label to rectangel
			if (showID)
			{
#			browser()
				grid.text(label=pepViz:::uniEncodeString(name),
						x=(start+end)/2  ##fix position at the center
						,y=(2*ybox+height)/2 ##fix position at the center
						
						,rot=getPar(gdObject,"idRotation")
						, gp = gpar(col=getPar(gdObject,"idColor"), cex = getPar(gdObject,"idCex")
						),
						default.units = "native", just = jtext)
			}
		}
		
	}
	popViewport(2)
	

})





