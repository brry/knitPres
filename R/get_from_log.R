#' @title get messages for a single search string from TeX log file
#' @description get lines from log file content for a single search string.
#'              This is a basic function for \code{\link{get_texlog_warnings}}
#' @return Vector of character strings. For each line matching some pattern,
#'         the \code{nlines} following this line number are aggregated with
#'         \code{\link{paste}(lines_with_content, collapse="\\n")}.
#'         If nothing matches, NULL is returned.
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Nov 2017
#' @seealso \code{\link{get_texlog_warnings}}
#' @keywords file
# @importFrom package fun1 fun2
#' @export
#' @examples
#' log <- system.file("extdata/no_messages.log", package="knitPres")
#' log <- readLines(log)
#' cat(get_from_log(log, "plot_indo2-1.pdf  used", nlines=4))
#'
#' @param texlog   Character string vector e.g. from \code{\link{readLines}}
#' @param pattern  Char: pattern to be searched for. DEFAULT: ""
#' @param line     Integer: line number(s) to be returned.
#'                 DEFAULT: \code{grep(pattern, texlog, ignore.case=TRUE)}
#' @param nlines   Integer: Number of lines to be used, starting at \code{line}.
#'                 DEFAULT: 1
#' @param ncharmin Integer: Lines with fewer characters are omitted from output.
#'                 DEFAULT: 4
#'
get_from_log <- function(
texlog,
pattern="",
line=grep(pattern, texlog, ignore.case=TRUE),
nlines=1,
ncharmin=4)
{
log_one_line <- function(single_line)
  {
  if(nlines==0) return(NULL)
  out <- texlog[ single_line + 0:(nlines-1) ]
  outnospace <- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", out, perl=TRUE)
  out <- out[outnospace != ""]
  out <- out[nchar(out)>ncharmin]
  out <- paste(out, collapse="\n")
  if(out=="") out <- NULL
  out
  }
unlist(lapply(line, log_one_line)) # grep can return several lines
}

