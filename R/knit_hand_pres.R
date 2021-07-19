#' @title knit handout and presentation version of .Rnw file
#' @description knit handout and presentation version of .Rnw file,
#'              compile tex file in parallel,
#'              write progress + tex warnings into a separate .knitlog file
#' @return log entries obtained with \code{\link{get_texlog_warnings}}
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Nov 2017
#' @seealso \code{\link{texlog_warnings}} for extracting messages from the TeX .log file
#' @keywords file
#' @importFrom parallel makeCluster parLapply stopCluster
#' @importFrom berryFunctions checkFile openFile
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom knitr knit2pdf
#' @importFrom utils tail

#' @export
#' @examples
#' \dontrun{## Exclude time consuming test from regular testing
#'
#' # Copy Rnw file to temporary folder ---
#' rnwfile <- system.file("extdata/minimalpres.Rnw", package="knitPres")
#' dir.create("TempKnitFolder")
#' owd <- setwd("TempKnitFolder") ; getwd()
#' file.copy(rnwfile, "minimalpres.Rnw")
#'
#' # Actual knitting ----
#' knit_hand_pres(file="minimalpres.Rnw")
#'
#' # clean up ----
#' setwd(owd) ; getwd()
#' unlink("TempKnitFolder", recursive=TRUE)
#' }
#'
#' @param file     Char: .Rnw file name to be knitted
#' @param presname Char: inset that will get appended to file name. DEFAULT: "_pres"
#' @param open     Logical: Open the resulting pdfs with
#'                \code{berryFunctions::\link{openFile}}? DEFAULT: TRUE
#' @param cleanup  Logical: remove intermediate tex files? DEFAULT: TRUE
#' @param \dots    Further arguments passed to \code{tools::\link{texi2pdf}}
#'
knit_hand_pres <- function(
  file,
  presname="_pres",
  open=TRUE,
  cleanup=TRUE,
  ...)
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
    pdfhand <- sub(".tex", ".pdf",     texfile)
    pdfpres <- sub(".tex", ".pdf",     presfile)

# Timing information:
starttime <- Sys.time()
message("Creating '",texfile,"' with knitr::knit, starting ", as.character(starttime))
messtime <- function(diff) paste0(format(unclass(diff), digits=2), " ", attr(diff, "units"))

# sinking messages etc into .knitlog file, create .tex file:
con <- file(knitlogfile)
sink(con, append=FALSE)
sink(con, append=TRUE, type="message")
# Restore output to console after completing/aborting function:
message("Running knitr::knit on '",file,"', starting ", as.character(starttime))
knitfile <- try(knitr::knit(rnwfile))
midtime <- Sys.time()
knittime <- midtime-starttime
knitmes <- paste0("Finished knitr::knit ",as.character(midtime),", after ",messtime(knittime))
message("-------\n", knitmes)
sink()
sink(type="message")
# check texfile name:
if(knitfile != texfile) warning("knitr::knit output ('",knitfile,
                                "') != texfile ('",texfile,"')")
# intermediate messages:
message(knitmes)
message("Now running tools::texi2pdf on '",texfile,"' and '",presfile,"' in parallel.")

# read lines, remove handout option, save pres file:
pres <- readLines(texfile)
pres[1] <- gsub("handout","",pres[1])
writeLines(pres, presfile)

# compile docs in two child R instances / in parallel
cl <- makeCluster(2)
parLapply(X=c(texfile, presfile), cl=cl, fun=function(x)
         berryFunctions::tryStack(tools::texi2pdf(x, ...), file=knitlogfile))
stopCluster(cl)

# LaTeX Timing information:
textime <- Sys.time()-midtime
texmes <- paste0("Finished tools::texi2pdf ", as.character(Sys.time()),
                 ", after ", messtime(textime))
message(texmes)
cat(texmes, "\n-------\n", file=knitlogfile, append=TRUE)

# remove intermediate files of presentation version:
if(cleanup)
{
tex_clean( rnwfile)
tex_clean(presfile, c(".tex", ".log"))
}

# Display relevant information from latex log file:
output <- texlog_warnings(texlogfile)
cat(output, file=knitlogfile, append=TRUE)

comptime <- Sys.time()-starttime
message("Total time ", messtime(comptime))
cat("\nTotal time ", messtime(comptime), file=knitlogfile, append=TRUE)

# Open file(s)
if(open)
  {
  berryFunctions::openFile(pdfhand)
  berryFunctions::openFile(pdfpres)
  }

# output:
return(invisible(output))
}
