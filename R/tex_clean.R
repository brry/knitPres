#' @title Clean up after tex run
#' @description Clean manually after tex run. .tex + .log files are kept.
#' @return output from \code{\link{unlink}}
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jul 2021
#' @importFrom tools file_path_sans_ext
#' @export
#'
#' @param file Filename, with or without extension
#' @param ext  Further extensions to also be removed, e.g. .tex/.log
#'
tex_clean <- function(file, ext=NULL)
{
ext2r <- c(".aux", "-concordance.tex", ".out", ".nav",
           ".snm", ".synctex.gz", ".toc", ".vrb")
if(!is.null(ext)) ext2r <- c(ext2r, ext)
f2r <- paste0(tools::file_path_sans_ext(file), ext2r)
unlink(f2r)
}
