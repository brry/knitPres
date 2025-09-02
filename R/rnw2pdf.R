#' @title compile Rnw document to pdf
#' @description compile Rnw document to pdf, with nice error logging
#' @return invisible charstring with file name
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jul 2021
#' @importFrom berryFunctions checkFile openFile
#' @importFrom tools file_ext texi2pdf
#' @importFrom knitr knit2pdf
#' @export
#' @param file  Rnw file name.
#' @param usetinytex Logical. Use \code{tinytex::\link[tinytex]{pdflatex}}
#'              (instead of \code{tools::\link[tools]{texi2pdf}})?
#'              DEFAULT: TRUE when tinytex is available
#' @param open  Logical. Should pdf be opened with
#'              \code{berryFunctions::\link[berryFunctions]{openFile}}?
#'              DEFAULT: TRUE
#' @param clean Clean up temporary files?
#'              DEFAULT: TRUE (unlike in  \code{tools::\link[tools]{texi2pdf}})
#' @param \dots Further arguments passed to \code{knitr::\link[knitr]{knit}}
#'
rnw2pdf <- function(
file,
usetinytex=requireNamespace("tinytex", quietly=TRUE),
open=TRUE,
clean=TRUE,
...
)
{
# time:
starttime <- Sys.time()

# check file:
berryFunctions::checkFile(file)
ext <- tools::file_ext(file)
if(ext!="Rnw") stop("Must be .Rnw file, not '.", ext,"': ", file)

# set working directory:
owd <- setwd(dirname(file))
on.exit(setwd(owd), add=TRUE)

# Convert to tex:
message("Running knitr::knit on ",file," starting ",format(Sys.time(),"%F %T"),"...")
texfile <- knit_with_stop(knitr::knit(file, envir=new.env(), ...)) # see internal function below

# Convert to pdf:
if(usetinytex)
  {
  message("Running tinytex::pdflatex on ",texfile," starting ",format(Sys.time(),"%F %T"),"...")
  pdffile <- try(suppressWarnings(tinytex::pdflatex(texfile, clean=FALSE)), silent=TRUE)
  } else
  {
  message("Running tools::texi2pdf on ",texfile," starting ",format(Sys.time(),"%F %T"),"...")
  pdffile <- try(suppressWarnings(tools::texi2pdf(texfile, clean=FALSE)), silent=TRUE)
  }

if(inherits(pdffile, "try-error"))
  open <- FALSE else
  pdffile <- normalizePath(sub("\\.Rnw$",".pdf",file), winslash="/")

# print tex log warnings (if any are found):
texlog_warnings(file)
# cleanup:
if(clean) tex_clean(file)

# tell time + open pdf:
diff <- Sys.time()-starttime
diff <- paste0(format(unclass(diff), digits=2), " ", attr(diff, "units"))
message("Done after ",diff,  if(open)"\nOpening ",if(open)pdffile)
if(open) berryFunctions::openFile(pdffile)
# output:
return(invisible(file))
}


# adapted from codeoceanR:::rt_gives_echo
knit_with_stop <- function(expr)
{
sinkfile <- tempfile(fileext="_get_echo.txt")
# Open capturing:
con <- file(sinkfile, open="wt")
sink(con, type="output" , append=TRUE)
sink(con, type="message", append=TRUE)
# evaluate the expression:
failed <- FALSE
value <- try(expr, silent=TRUE)
if(inherits(value, "try-error"))
  {
  failed <- TRUE
  cat(value)
  }
# Close capturing:
sink(type="message")
sink()
close(con)
# Output:
if(!failed) return(value)
captured <- readLines(sinkfile, warn=FALSE)
sel <- tail(grep("label", captured),1)
if(length(sel)!=1) sel <- length(captured) - 5
cap <- captured[sel:length(captured)]
cap <- trimws(cap)
cap <- cap[cap!=""]
cap <- paste(cap, collapse="\n")
stop(cap, call.=FALSE)
}
