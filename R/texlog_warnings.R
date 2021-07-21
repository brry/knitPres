# messages for a single search string in content of TeX log file
get_from_log <- function(
texlog,       # Character string vector e.g. from readLines
pattern="",   # Char: pattern to be searched for.
nlines=1,     # Number of lines following match to be printed.
nchar_min=4)  # Lines with fewer characters are omitted from output.
{
line <- grep(pattern, texlog, ignore.case=TRUE)
out <- lapply(line, function(single_line)
  {
  if(nlines==0) return(NULL)
  out <- texlog[ single_line + 0:(nlines-1) ]
  outnospace <- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", out, perl=TRUE)
  out <- out[outnospace != ""]
  out <- out[nchar(out) > nchar_min]
  out <- paste(out, collapse="\n")
  if(out=="") return(NULL)
  out
  })
out <- unlist(out) # grep can return several lines
out
}
# log <- system.file("extdata/no_messages.log", package="knitPres")
# log <- readLines(log)
# cat(get_from_log(log, "plot_indo2-1.pdf  used", nlines=4))





#' @title Display tex log warnings
#' @description Display (if any are found) notes, warnings and errors from some_latex_file.log
#' @return Vector of character strings, one per warning
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Nov 2017 + Jul 2021
#' @references Test log files from \url{https://github.com/stefanhepp/pplatex/tree/master/test}
#' @keywords file
#' @export
#' @examples
#' testlog <- function(logname, ...)
#'   {
#'   logf <- system.file(paste0("extdata/",logname,".log"), package="knitPres")
#'   texlog_warnings(logf, ...)
#'   }
#' testlog("no_messages")
#' testlog("blitzaerror")
#' testlog("lotsoferrors", nlines_error=3)
#' testlog("lotsoferrors", nlines_error=4) # more info, less duplicates
#' testlog("pgferror")
#' testlog("superseterror")
#' testlog("superseterror", nlines_box=0) # no over/underful box warnings
#' testlog("superseterror", nlines_warning=5)
#'
#' @param file           Char: file name (.Rnw, .tex or .log)
#' @param nlines_error   Integer: number of lines for an error. DEFAULT: 4
#' @param nlines_box     Integer: number of lines for over/underfull box messages. DEFAULT: 7
#' @param nlines_warning Integer: number of lines for other messages. DEFAULT: 3 (may not always be enough)
#' @param \dots          Further arguments passed to \code{\link{readLines}}
#'
texlog_warnings <- function(
  file,
  nlines_error=4,
  nlines_box=7,
  nlines_warning=3,
  ...
)
{
file <- sub("\\.Rnw$", ".log", file)
file <- sub("\\.tex$", ".log", file)
texlog <- readLines(file, warn=FALSE, ...)
#
# https://tex.stackexchange.com/questions/32213
# https://tex.stackexchange.com/a/10564
wrn1 <- get_from_log(texlog, "TeX warning", nlines=nlines_warning)
wrn2 <- get_from_log(texlog, "warning:",    nlines=nlines_warning) # also covers Font Warnings
err1 <- get_from_log(texlog, "TeX error",   nlines=nlines_error)
err2 <- get_from_log(texlog, "error:",      nlines=nlines_error) # also covers xcolor errors
out <- c(
 get_from_log(texlog, "overfull",    nlines=nlines_box),
 get_from_log(texlog, "underfull",   nlines=nlines_box),
 unique(c(wrn1,wrn2)),
 get_from_log(texlog, ".vrb:",       nlines=nlines_warning), # for \rcode{a_b} instead of {a\_b}
 unique(c(err1,err2)),
 get_from_log(texlog, "!",           nlines=nlines_error), # Errors at the end
 NULL
)
if(is.null(out)) return(out)
#
# aggregate duplicates:
out <- sapply(unique(out), function(x)
  {
  s <- sum(out==x)
  if(s>1)
    paste0(s," times: ", x) else
    x
  }, USE.NAMES=FALSE)
#
if(any(grepl("Missing $ inserted", out, fixed=TRUE)))
  out <- c(out, "--BB: 'Missing $ inserted' may indicate a missing  \\  in front of  _ or #.")
if(any(grepl("\\endframe ->\\egroup", out, fixed=TRUE)))
  out <- c(out, "--BB: potentially, \\begin{eframe} is closed with \\end{frame} instead of \\end{eframe}.")

# message:
mes <- paste0("---\nMessages in '", normalizePath(file,"/"), ":\n---\n",
              paste(out, collapse="\n---\n"), "\n---")
message(mes)
# output:
return(invisible(out))
}
