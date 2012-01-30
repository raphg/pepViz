#overwrite the original function in GenomeGraphs in order to 
#1.align the highlight region in the new layout that has extralegend column and 
#2.extract color and line style from dp slot using getPar()
###############################################################################

.drawHighlightRegion <- function(hr, minBase, maxBase, vplayout,legendRatio) {

	pushViewport(viewport(layout = grid.layout(nrow=1,ncol=2,widths=unit(c(1-legendRatio,legendRatio), "native")),
					layout.pos.col = 1, layout.pos.row =NULL))
	pushViewport(viewport(layout = grid.layout(nrow=1,ncol=1),layout.pos.col = 1, layout.pos.row =1))
	switch(hr@coords,
			"genomic" = {
				ss <- (hr@start - minBase)/(maxBase - minBase) 
				ee <- (hr@end - minBase)/(maxBase - minBase) 
				if (is.null(hr@region)) {
					y0 <- 0
					height <- 1
				}
				else {
					region <- hr@region
					y0 <- 1 - sum(vplayout[1:region[2]])/sum(vplayout)
					height <- sum(vplayout[region[1]:region[2]])/sum(vplayout)
				}
			},
			"absolute" = {
				ss <- hr@start
				ee <- hr@end
				y0 <- hr@region[1]
				height <- hr@region[2] - hr@region[1]
			},
			stop("Unknown coordinate specification in HighlightRegion."))
	
	color <- rgb(t(col2rgb(getPar(hr,"color"))/255), alpha = getPar(hr, "alpha"))
	grid.rect(ss, y0, width = (ee - ss), height = height
			, gp = gpar(fill=color, lwd = getPar(hr,"lwd"), lty = getPar(hr,"lty")),
			just=c("left", "bottom"))
	popViewport(1)
	popViewport(1)
}