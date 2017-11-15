#' @title knit handout and presentation version of .Rnw file
#' @description knit handout and presentation version of .Rnw file,
#'              compile tex file in parallel,
#'              write progress + tex warnings into a separate .knitlog file
#' @return log entries obtained with \code{\link{get_texlog_warnings}}
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Nov 2017
#' @seealso \code{\link{get_texlog_warnings}} for extracting messages from the TeX .log file
#' @keywords file
#' @importFrom parallel makeCluster parLapply stopCluster
#' @importFrom berryFunctions checkFile
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom knitr knit2pdf
#' @importFrom utils tail

#' @export
#' @examples
#' \dontrun{## Exclude time consuming test from regular testing
#'
#' # Copy rnw file to temporary folder ---
#' rnwfile <- system.file("extdata/minimalpres.Rnw", package="knitPres")
#' dir.create("TempKnitFolder")
#' owd <- setwd("TempKnitFolder") ; getwd()
#' file.copy(rnwfile, "minimalpres.Rnw")
#'
#' # Actual knitting ----
#' messages <- knit_hand_pres(file="minimalpres.Rnw")
#' str(messages)
#' berryFunctions::openFile("minimalpres.pdf")
#' berryFunctions::openFile("minimalpres_pres.pdf")
#'
#' # clean up ----
#' setwd(owd) ; getwd()
#' unlink("TempKnitFolder", recursive=TRUE)
#' }
#'
#' @param file     Char: .Rnw file name to be knitted
#' @param presname Char: inset that will get appended to file name. DEFAULT: "_pres"
#' @param \dots    Further arguments passed to \code{tools::\link{texi2pdf}}
#'
knit_hand_pres <- function(file, presname="_pres", ...)
{
# check filename:
if(length(file)>1) stop("Length of file must be 1, not ", length(file))
berryFunctions::checkFile(file)
ext <- tools::file_ext(file)
if(ext!="Rnw") stop("file extenstion must be '.Rnw', not '.", ext,"'.")
if(presname=="") stop("presname cannot be empty charstring ''.")

# check handout option:
pres <- readLines(file, n=1)
if(!grepl("handout", pres)) stop("Input file must contain 'handout' option ",
  "in first line, e.g. \\documentclass[handout]{beamer}.\nfile is: '",file,"'.")

# set working directory, get filenames:
owd <- setwd(dirname(file))
on.exit(setwd(owd), add=TRUE)
    rnwfile <- basename(file)
knitlogfile <- sub(".Rnw", ".knitlog", rnwfile)
    texfile <- sub(".Rnw", ".tex",     rnwfile)
 texlogfile <- sub(".Rnw", ".log",     rnwfile)
   presfile <- paste0(tools::file_path_sans_ext(texfile), presname, ".tex")

# Timing information:
starttime <- Sys.time()
message("Creating '",texfile,"' with knitr::knit, starting ", as.character(starttime))
messtime <- function(diff) paste0(format(unclass(diff), digits=2), " ", attr(diff, "units"))

# sinking messages etc into .knitlog file, create .tex file:
con <- file(knitlogfile)
sink(con, append=FALSE)
sink(con, append=TRUE, type="message")
# Restore output to console after completing/aborting function:
knitfile <- try(knitr::knit(rnwfile))
sink()
sink(type="message")
# check texfile name:
if(knitfile != texfile) warning("knitr::knit output ('",knitfile,
                                "') != texfile ('",texfile,"')")

# intermediate message:
midtime <- Sys.time()
knittime <- midtime-starttime
message("Finished knitr::knit ", as.character(midtime), ", after ",
        messtime(knittime) )

message("Now running tools::texi2pdf on '",texfile,"' and '",presfile,"' in parallel.")

# read lines, remove handout option, save pres file:
pres <- readLines(texfile)
pres[1] <- gsub("handout","",pres[1])
writeLines(pres, presfile)

# compile docs in two child R instances / in parallel
cl <- makeCluster(2)
parLapply(X=c(texfile, presfile), cl=cl, fun=function(x)try(tools::texi2pdf(x, ...)))
stopCluster(cl)

# remove intermediate files of presentation version:
f2r <- dir(pattern=tools::file_path_sans_ext(presfile)) # files to remove
f2r <- f2r[tools::file_ext(f2r) != "pdf"]
unlink(f2r)

# Timing information:
textime <- Sys.time()-midtime
message("Finished tools::texi2pdf ", as.character(Sys.time()), ", after ",
        messtime(textime) )

comptime <- Sys.time()-starttime
message("Total time ", messtime(comptime), "\n" )

# Try to get some relevant information from latex log file:
logwarnings <- get_texlog_warnings(texlogfile)
logwarning <- paste0("Messages in '", texlogfile, "' at '",owd,"':\n---\n",
                     paste(logwarnings, collapse="\n---\n")    )
cat(logwarning, file=knitlogfile, append=TRUE)
message(logwarning)

# Open file(s) ### To debate
### berryFunctions:openFile

# output:
return(invisible(logwarning))
}
