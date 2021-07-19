#' @title compile Rnw document to pdf
#' @description compile Rnw document to pdf, with nice error logging
#' @return invisible charstring with file name
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jul 2021
#' @importFrom berryFunctions checkFile openFile
#' @importFrom tools file_ext texi2pdf
#' @importFrom knitr knit2pdf
#' @export
#' @param file  Rnw file name.
#' @param open  Logical. Should pdf be opened with
#'              \code{berryFunctions::\link[berryFunctions]{openFile}}?
#'              DEFAULT: TRUE
#' @param clean Clean up temporary files?
#'              DEFAULT: TRUE (unlike in  \code{tools::\link[tools]{texi2pdf}})
#' @param \dots Further arguments passed to \code{knitr::\link[knitr]{knit}}
#'
rnw2pdf <- function(
file,
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
message("Running knitr::knit on ",file," starting ",as.character(Sys.time()),"...")
texfile <- try(knitr::knit(file, ...), silent=TRUE)
if(inherits(texfile, "try-error")) warning(texfile)

# Convert to pdf:
message("Running tools::texi2pdf on ",texfile," starting ",as.character(Sys.time()),"...")
pdffile <- try(tools::texi2pdf(texfile, clean=FALSE), silent=TRUE)
if(inherits(pdffile, "try-error"))
  open <- FALSE else
  pdffile <- normalizePath(pdffile, winslash="/")

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
