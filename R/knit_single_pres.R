#' @title knit an Rnw presentation, log progress + warnings
#' @description knit an Rnw presentation, write progress + tex warnings into a
#'              separate .knitlog file
#' @return log messages as a single character string, invisible
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Nov 2017
#' @seealso \code{\link{get_texlog_warnings}} for extracting messages from the TeX .log file
#' @keywords file
#' @importFrom knitr knit2pdf
#' @importFrom berryFunctions checkFile
#' @importFrom utils tail
#' @export
#' @examples
#' \dontrun{## Exclude time consuming test from regular testing
#' rnwfile <- system.file("extdata/minimalpres.Rnw", package="knitPres")
#' dir.create("TempKnitFolder")
#' owd <- setwd("TempKnitFolder") ; getwd()
#' file.copy(rnwfile, "minimalpres.Rnw")
#' messages <- knit_single_pres(file="minimalpres.Rnw")
#' cat(messages)
#' #    berryFunctions::openFile("minimalpres.pdf")
#' setwd(owd) ; getwd()
#' unlink("TempKnitFolder", recursive=TRUE)
#' }
#'
#' @param file  Filename
#' @param \dots Further arguments passed to \code{\link{plot}}
#'
knit_single_pres <- function(file)
{
berryFunctions::checkFile(file)
# sink messages etc into .knitlog file
knitlogfile <- sub(".Rnw", ".knitlog", file)
con <- file(knitlogfile)
sink(con, append=FALSE)
sink(con, append=TRUE, type="message")
# Restore output to console
on.exit(  {sink()  ;  sink(type="message")  ;
  message(paste(tail(readLines(knitlogfile), 5), collapse="\n"))}  )
# Time info
starttime <- Sys.time()
message("Starting knitr::knit2pdf ", as.character(starttime))
knitr::knit2pdf(file) # clean=grepl("pres", file)
comptime <- Sys.time()-starttime
message("Finished knitr::knit2pdf ", as.character(Sys.time()), ", after ",
        format(unclass(comptime), digits=2), " ", attr(comptime, "units") )
#
# Try to get some relevant information from latex log file:
texlogfile <- sub(".Rnw", ".log", file)
logwarnings <- get_texlog_warnings(texlogfile)
logwarning <- paste0("Messages in '", texlogfile, "':\n---\n",
                     paste(logwarnings, collapse="\n---\n")    )
message(logwarning)
# output:
return(invisible(logwarning))
}
