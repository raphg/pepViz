setMethod("show",signature(object="GenericAnno"),
  function(object){
    len <- min(5, length(object@anno))
    cat("Object of class 'GenericAnno':\n")
    print(object@probeStart[1:len])
    cat("Annotations: \n")
#    print(object@anno[1:len])
#    res = paste("\n There are ",length(object@anno)-len," more rows\n", sep="")
#    cat(res)
	print(getAnnotations(object))	
    show(object@dp)
})


setMethod("getIndex", signature("gdObject"), function(obj) obj@index)

setMethod("getProbeStart",signature("GenericAnno"),function(obj) obj@probeStart)
setMethod("getAnnotations",signature("GenericAnno"),function(obj) obj@anno)

setMethod("isLegend",signature("gdObject"),function(obj){
			isLegend<-getPar(obj,"isLegend")
			ifelse(is.null(isLegend),FALSE,isLegend)
		})

