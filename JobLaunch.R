args <- commandArgs(TRUE)
srcFile <- args[1]
str<-args[1]
if (length(args)>2)
{
  for (i in 1:length(args))
  {
    str<-paste(str,args[i],sep='.')
  }
}
outFile <- paste0(str, ".Rout")
args <- args[-1]

sink(outFile, split = TRUE)
source(srcFile, echo = F)