#########################################
setClass("GenericAnno", contains = "gdObject", 
         representation(anno = "RangedData"#"list",
#				 		isLegend="logical",
                        ,probeStart = "numeric"
						),
         prototype(
				 
                   dp = DisplayPars(color = "gold4",
                   lty = "solid",
                   pch = 16,
                   pointSize = .2,
                   lwd = 1,
                   size = 1,
				   alpha=1,
#                   type = "point",
					horiz = TRUE,
					dna = FALSE,
					showID = TRUE,
					idRotation = 0,
					idColor="black",
					idCex=0.5,
#					showSeq=FALSE,
					trackGap=0.1,
					isLegend=FALSE
#					,isTips=TRUE
			)
		   		   )
         );
		 
makeGenericAnno <- function(anno, probeStart, dp = NULL){
 pt <- getClass("GenericAnno")@prototype 
 if (is.null(dp))
   dp <- pt@dp
 if(missing(probeStart)) stop("Need probeStart argument to know where to plot the data on the genome")
 if(missing(anno)) stop("Need annotation argument!")
# browser()
 if(!inherits(anno,"RangedData"))
 	 stop("Annotation needs to be a RangedData!")
  if(!"name"%in%colnames(anno))
		 stop("Annotation needs to have a name column in values slot of RangedData!")
	new("GenericAnno", anno = anno, probeStart = probeStart,dp = dp)
}


setClass("PepAxis", contains = "GenomeAxis", 
		representation(
						anno = "RangedData"
#						isLegend="logical",
						,seq="character"
						,addNC="logical"
		),
		prototype(
#				isLegend=FALSE,
				addNC=FALSE
				,seq=character(0)
				,dp = DisplayPars(color = "black"
						,byValue=1
						,size = 1
						,distFromAxis = 1
						,cex = 0.7
						,labelPos = "below"
						,trackGap=0.1
						,isLegend=FALSE
						,dna = FALSE
						,showSeq=TRUE
						,showAxis=TRUE
						)
		)
);

makePepAxis <- function(anno=RangedData(),addNC=FALSE,seq=character(0),littleTicks=FALSE, dp = NULL){
	pt <- getClass("PepAxis")@prototype 
	if (is.null(dp))
		dp <- pt@dp
#	if(missing(probeStart)) stop("Need probeStart argument to know where to plot the data on the genome")
#	if(missing(anno)) stop("Need annotation argument to know what to label for the genome")
	new("PepAxis", anno = anno,addNC=addNC,seq=seq,littleTicks=littleTicks, dp = dp)
}
##overwrite the makeGenomeAxis function of GenomeGraphs which has to bug that does not pass littleTicks
.makeGenomeAxis <- function(add53 = FALSE, add35 = FALSE, littleTicks = FALSE, dp = NULL){
	if (is.null(dp))
		dp <- getClass("GenomeAxis")@prototype@dp
	new("GenomeAxis", add53 = add53, add35 = add35, littleTicks=littleTicks,dp = dp)
}



setClass("ProbeSequence", contains = "gdObject", 
		representation(sequence="list",
				intensity = "list",
				probeStart = "list",
				dp="DisplayPars"
		),
		prototype(
				sequence=list(),
				intensity = list(),
				probeStart = list(),
				dp = DisplayPars(color = "black",
						bgColor=heat.colors(8, alpha = 1),
						alpha=1,
						lwd=1,
						size = 1,
						idCex =1,
						isLegend=FALSE,
#						type="sequence"
						,trackGap=0.1
				)
		)
);

makeProbeSequence <- function(sequence,intensity,probeStart, dp = NULL){
	pt <- getClass("ProbeSequence")@prototype 
	if (is.null(dp))
		dp <- pt@dp
	if(missing(probeStart)) stop("Need probeStart argument to know where to plot the data on the genome")
	if(missing(sequence)) stop("Need sequence argument to know what to plot")
#	browser()
	if(class(sequence)!="list")
	{
		sequence<-list(sequence)
		intensity<-list(intensity)
		probeStart<-list(probeStart)
	}
	##check the consistancy of number of entires 
	if(!(identical(lapply(intensity,length),lapply(probeStart,length))&&identical(lapply(sequence,length),lapply(probeStart,length))))
		stop("sequence ,intensity and probeStart need have identifcal structure!")
	##check the types
	if(any(unlist(lapply(sequence,class))!="character"))
		stop("sequence has to be character vector!")
	if(any(unlist(lapply(intensity,class))!="numeric")||any(!unlist(lapply(probeStart,class))%in%c("numeric","integer")))
		stop("intensity and probeStart have to be numeric vectors!")
	new("ProbeSequence", sequence=sequence,intensity=intensity,probeStart=probeStart, dp = dp)
}



