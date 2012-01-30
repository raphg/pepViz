###############################################################################
#recycle gdPlot function from genomeGraphs 
#in order to plot overlay before the other tracks
###############################################################################
pdPlot <- function(gdObjects, minBase = NA, maxBase = NA, highlightRegions = NULL,labelColor = "black", labelCex = 1, labelRot = 90,legendRatio=0.05) {
	
	grid.newpage()
	
	if (class(gdObjects) != "list")
		gdObjects <- list(gdObjects)
	
	if (missing(minBase) || missing(maxBase)) {
		gr <- sapply(gdObjects, getGenomicRange)
		minBase <- ifelse(all(is.na(gr[1,])), NA, min(gr[1,], na.rm = TRUE))
		maxBase <- ifelse(all(is.na(gr[2,])), NA, max(gr[2,], na.rm = TRUE))
	}
	if (is.na(minBase) || is.na(maxBase))
		stop("Need to define a suitable minBase and maxBase; cannot determine this from the objects.")
	
	vplayout <- sapply(gdObjects, getSize)
	names(vplayout) <- as.character(seq(along = gdObjects))
#	browser()
	#when none of the track has legend then force legendRatio=0
	if(!any(unlist(lapply(gdObjects,isLegend))))
		legendRatio<-0	
	## don't draw things that are of size 0.
	mask <- vplayout > 0
	vplayout <- vplayout[mask]
	gdObjects <- gdObjects[mask]
#	browser()
	if (!is.null(names(gdObjects))) {
		pushViewport(viewport(layout = grid.layout(1, 2, width = c(0.10, 0.9)),
						width = .90, height = .95))
		
		##draw track name in the first column
		pushViewport(viewport(layout=grid.layout(length(vplayout), 1, height=vplayout),
						layout.pos.col = 1, layout.pos.row = 1))
		
		for(i in seq(along=gdObjects)) {
			nm <- names(gdObjects)[i]
			if (nm != "") {
				pushViewport(viewport(layout.pos.col = 1, layout.pos.row = i))
				pushViewport(viewport(0, .5, width = .5, height = .5))
				
				## should there be an argument for 'text rotation?'
#				grid.text(label = formatC(nm, format = "d"),
#						just = "centre", rot = 90, 
#						default.units = "native")
				grid.text(label = formatC(nm, format = "d"),
						just = "centre", rot = labelRot,
						gp = gpar(col=labelColor, 
								cex = labelCex),
						default.units = "native")
				popViewport(2)
			}
		}
		popViewport(1)
		
		
		
		pushViewport(viewport(layout = grid.layout(length(vplayout), 1, height = vplayout),
						layout.pos.col = 2, layout.pos.row = 1))
	}else{
		## here you make the region smaller so that the axis fit. 
		pushViewport(viewport(layout = grid.layout(length(vplayout), 1, height = vplayout),
						width = .85, height = .95))
	}
#		browser()
	
	
#	if (!is.null(highlightRegions)) {
#		if (!is.list(highlightRegions))
#			highlightRegions <- list(highlightRegions)
#		
#		lapply(highlightRegions, function(highlightRegion) {
#					pepViz:::.drawHighlightRegion(highlightRegion, minBase, maxBase, vplayout,legendRatio)
#				})
#	}
#	browser()
	for (i in seq(along = gdObjects)) {
		gdObject<-gdObjects[[i]]

		setPar(gdObject, "legendRatio", legendRatio)
#		browser()
		drawGD(gdObject, minBase, maxBase, i)
	}
	
	if (!is.null(highlightRegions)) {
		if (!is.list(highlightRegions))
			highlightRegions <- list(highlightRegions)
		
		lapply(highlightRegions, function(highlightRegion) {
					pepViz:::.drawHighlightRegion(highlightRegion, minBase, maxBase, vplayout,legendRatio)
				})
	}
	##-- pop to the top of the viewport stack.
	popViewport(0)
}




uniEncodeString<-function(str1)
{
	##FIXME:only works for single element within bar braces 
	strs<-strsplit(str1,split="\\*")[[1]]
#	strs<-lapply(strs,sub,pattern="beta",replacement="0x03B2")
#	strs<-lapply(strs,sub,pattern="alpha",replacement="0x03B1")
	
	strs<-lapply(strs,function(x){

				if(length(grep("bar",x))>0)##convert bar to unicode of Combining Diacritical Marks
				{
					e<-strsplit(x,split="\\(")[[1]][2]
					e<-sub("\\)","",e)
					
					c(utf8ToInt(e),"0x0305")
				}else if(length(grep("beta",x))>0)
				{
					"0x03B2"
				}else if(length(grep("alpha",x))>0)
				{
					"0x03B1"
				}else
				{
					utf8ToInt(x)
				}
				
			})

	strs<-unlist(strs)
	paste(intToUtf8(strs,m=T),collapse="")
	
}

