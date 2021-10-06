#' @title Determine locations for textbox
#' @description Extract locations for textbox from pdf screenshot with reference markers
#' @return Invisible list with coordinates, the printed message contains ready-to-use code
#' @author Berry Boessenkool, \email{berry-b@@gmx.de}, Jul 2021
#' @importFrom png readPNG
#' @importFrom graphics rasterImage locator rect par
#' @importFrom berryFunctions round0
#' @export
#' @param img   Image file name, is read with \code{png::\link{readPNG}(img)}
#'              DEFAULT: "C:/Users/berry/Desktop/pdfscreenshot.PNG"
#' @param frame Show complete frame template? DEFAULT: TRUE
#' @param \dots Further arguments passed to \code{\link{plot}}
#'
locateTextbox <- function(
img="C:/Users/berry/Desktop/pdfscreenshot.PNG",
frame=TRUE,
...
)
{
# plot image
op <- par(mar=c(0,0,0,0))
on.exit(par(op), add=TRUE)
plot(0:1, xlim=c(0,1), type='n')
img <- png::readPNG(img)
rasterImage(img, 0,0, 1,1)

# Calibration
message("click topleft of page (0, 0)")
c1 <- locator(n=1, type="p", pch=3, lwd=3)
message("click bottom right of page (12.8, 9.6)")
c2 <- locator(n=1, type="p", pch=3, lwd=3)

# locations
message("click topleft, then bottom right of each desired textbox location (ESC to finish)")
tx <- ty <- bx <- by <- NULL
for(i in seq_len(100))
  {
  p1 <- locator(1)
  p2 <- locator(1)
  if(is.null(p2)) break # If user pressed ESC in Rstudio Graphics window
  if(p1$x > p2$x) {warning("second loc must be right of first loc, skipping this.") ; next}
  if(p1$y < p2$y) {warning("second loc must be below first loc, skipping this.") ; next}
  rect(p1$x, p2$y, p2$x, p1$y, border="orange")
  tx <- c(tx, p1$x)
  ty <- c(ty, p1$y)
  bx <- c(bx, p2$x)
  by <- c(by, p2$y)
  }

# scale to range
tx <- (tx-c1$x)/(c2$x-c1$x)*12.8
ty <- (ty-c1$y)/(c2$y-c1$y)*9.6
bx <- (bx-c1$x)/(c2$x-c1$x)*12.8
by <- (by-c1$y)/(c2$y-c1$y)*9.6

w <-  berryFunctions::round0(bx-tx, 2, pre=1)
h <-  berryFunctions::round0(by-ty, 2, pre=1)
tx <- berryFunctions::round0(tx,    2, pre=1)
ty <- berryFunctions::round0(ty,    2, pre=1)

# Output
out <- paste0("\\begin{textblock*}{",w,"cm}(",tx,"cm,",ty,"cm) \\vspace{",h,"cm} ~ \\end{textblock*}")
out <- paste(out, collapse="\n")
if(frame)
out <- paste0("\n\\begin{frame}[fragile]{Titel}
\\begin{overlayarea}{\\textwidth}{0.95\\textheight} % avoid vertical jumps  https://texwelt.de/fragen/83
\\pause
Slide_content_here
\\only<+->{
\\textblockrulecolour{red}
",out,"
}
\\end{overlayarea}
\\end{frame}")
message(out)
# output
invisible(list(x=tx, y=ty, width=w, height=h))
}
