% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rnw2pdf.R
\name{rnw2pdf}
\alias{rnw2pdf}
\title{compile Rnw document to pdf}
\usage{
rnw2pdf(
  file,
  usetinytex = requireNamespace("tinytex", quietly = TRUE),
  open = TRUE,
  clean = TRUE,
  ...
)
}
\arguments{
\item{file}{Rnw file name.}

\item{usetinytex}{Logical. Use \code{tinytex::\link[tinytex]{pdflatex}}
(instead of \code{tools::\link[tools]{texi2pdf}})?
DEFAULT: TRUE when tinytex is available}

\item{open}{Logical. Should pdf be opened with
\code{berryFunctions::\link[berryFunctions]{openFile}}?
DEFAULT: TRUE}

\item{clean}{Clean up temporary files?
DEFAULT: TRUE (unlike in  \code{tools::\link[tools]{texi2pdf}})}

\item{\dots}{Further arguments passed to \code{knitr::\link[knitr]{knit}}}
}
\value{
invisible charstring with file name
}
\description{
compile Rnw document to pdf, with nice error logging
}
\author{
Berry Boessenkool, \email{berry-b@gmx.de}, Jul 2021
}
