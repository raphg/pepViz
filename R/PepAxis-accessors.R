
setMethod("getAnnotations",signature("PepAxis"),function(obj) obj@anno)

setMethod("getNC",signature("PepAxis"),function(obj) obj@addNC)

setMethod("getAASeq",signature("PepAxis"),function(x,...) x@seq)
