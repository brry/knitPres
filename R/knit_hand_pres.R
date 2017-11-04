#' @title knit handout and presentation version of .Rnw file
#' @description knit handout and presentation version of .Rnw file in parallel
#' @return ReturnValue
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Nov 2017
#' @seealso \code{\link{knit_single_pres}}, \code{\link{get_texlog_warnings}}
#' @keywords file
#' @importFrom parallel makeCluster parLapply stopCluster
#' @importFrom berryFunctions checkFile
#' @importFrom tools file_ext file_path_sans_ext
#' @export
#' @examples
#' \dontrun{## Exclude time consuming test from regular testing
#' rnwfile <- system.file("extdata/minimalpres.Rnw", package="knitPres")
#' dir.create("TempKnitFolder")
#' owd <- setwd("TempKnitFolder") ; getwd()
#' file.copy(rnwfile, "minimalpres.Rnw")
#' messages <- knit_hand_pres(file="minimalpres.Rnw")
#' messages
#' #    berryFunctions::openFile("minimalpres_pres.pdf")
#' setwd(owd) ; getwd()
#' unlink("TempKnitFolder", recursive=TRUE)
#' }
#'
#' @param file     Char: .Rnw file name to be knitted
#' @param presname Char: inset that will get appended to file name. DEFAULT: "_pres"
#' @param \dots    Further arguments passed to \code{\link{knit_single_pres}}
#'
knit_hand_pres <- function(file, presname="_pres", ...)
{
berryFunctions::checkFile(file)
if(presname=="") stop("presname cannot be empty charstring ''.")
starttime <- Sys.time()
# setwd needed?
# read lines, remove handout option, save pres file:
pres <- readLines(file)
nohandoutline <- gsub("handout","",pres[1])
if(pres[1] == nohandoutline) stop("Input file must contain 'handout' option ",
                                  "in first line, e.g. \\documentclass[handout]{beamer}.\n",
                                  "File is: '", file, "'.")
pres[1] <- nohandoutline
#
presfile <- paste0(tools::file_path_sans_ext(file), presname, ".", tools::file_ext(file))
writeLines(pres, presfile)
#
# knit docs in two child R instances / in parallel, while writing output to knitlogfile
cl <- makeCluster(2)
logs <- parLapply(X=c(file, presfile), cl=cl, fun=knit_single_pres, ...)
stopCluster(cl); rm(cl); gc()
message(logs[[1]])
#
# remove intermediate files of presentation version:
f2r <- dir(pattern=tools::file_path_sans_ext(presfile)) # files to remove
f2r <- f2r[!tools::file_ext(f2r) %in% c("pdf","knitlog")]
#message("files to remove: ", toString(f2r))
unlink(f2r)
comptime <- Sys.time()-starttime
message("Finished after ", format(unclass(comptime),digits=2), " ", attr(comptime, "units") )
return(invisible(logs))
}

