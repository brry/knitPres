#' @title get messages from TeX log file
#' @description get messages (notes, warnings, errors) from sometexdoc.log file
#' @return Vector of character strings, one per warning
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Nov 2017
#' @seealso \code{\link{get_from_log}}, \code{graphics::\link[graphics]{plot}}
#' @references Test log files from \url{https://github.com/stefanhepp/pplatex/tree/master/test}
#' @keywords file
# @importFrom package fun1 fun2
#' @export
#' @examples
#'
#' testlog <- function(logname, ...)
#'   {
#'   logf <- system.file(paste0("extdata/",logname,".log"), package="knitPres")
#'   cat("", get_texlog_warnings(logf, ...), sep="\n---\n")
#'   }
#'
#' testlog("no_messages")
#' testlog("blitzaerror")
#' testlog("lotsoferrors", nlines_error=3)
#' testlog("lotsoferrors", nlines_error=4) # more info, less duplicates
#' testlog("pgferror")
#' testlog("superseterror")
#' testlog("superseterror", nlines_box=0) # no over/underful box warnings
#' testlog("superseterror", nlines_warning=5)
#'
#' @param texlogfile     Char: file name
#' @param unique         Logical. Should duplicate messages be removed? DEFAULT: TRUE
#' @param nlines_error   Integer: number of lines for an error. DEFAULT: 4
#' @param nlines_box     Integer: number of lines for over/underfull box messages. DEFAULT: 7
#' @param nlines_warning Integer: number of lines for other messages. DEFAULT: 3 (may not always be enough)
#' @param \dots          Further arguments passed to \code{\link{readLines}}
#'
get_texlog_warnings <- function(
  texlogfile,
  unique=TRUE,
  nlines_error=4,
  nlines_box=7,
  nlines_warning=3,
  ...
)
{
texlog <- readLines(texlogfile, warn=FALSE, ...)
#
# https://tex.stackexchange.com/questions/32213
# https://tex.stackexchange.com/a/10564
out <- c(
 get_from_log(texlog, "overfull",    nlines=nlines_box),
 get_from_log(texlog, "underfull",   nlines=nlines_box),
 get_from_log(texlog, "TeX warning", nlines=nlines_warning),
 get_from_log(texlog, "warning:",    nlines=nlines_warning), # also covers Font Warnings
 get_from_log(texlog, ".vrb:",       nlines=nlines_warning), # for \rcode{a_b} instead of {a\_b}
 get_from_log(texlog, "TeX error",   nlines=nlines_error),
 get_from_log(texlog, "!",           nlines=nlines_error), # Errors at the end
 NULL
)
if(unique)
{
 l_all <- length(out)
 l_uni <- length(unique(out))
 out <- unique(out)
 if(l_uni < l_all) out <- c(out, paste0(l_all-l_uni, " messages (of ",l_all,
                                        ") were duplicate and are removed. Display all with ",
                                        "get_texlog_warnings(..., unique=FALSE)."))
} # end if unique
if(any(grepl("Missing $ inserted", out, fixed=T)))
  out <- c(out, "--BB: This may indicate a missing \\ in front of a _ or #.")
if(is.null(out)) out <- NULL # paste0("No errors found in '", texlogfile, "', but to be ",
                             # "sure,\nopen the corresponding tex file in TexMaker and click on 'View log'.")
else out <- c(out, paste0("There may be more messages in '", texlogfile, "'.\nTo ",
                         "check, open the corresponding tex file in TexMaker and click on 'View log'."))
out
}
