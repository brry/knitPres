### knitPres

knit both handout and presentation version of .Rnw slides with (relatively) good TeX error logging

### install

```R
if(!requireNamespace("devtools",quietly=TRUE)) install.packages("devtools")
devtools::install_github("brry/knitPres")
library("knitPres")
```

### usage

```R
knit_hand_pres("SomePresentation.Rnw")
```

`SomePresentation.Rnw` should contain the handout option in it's first line:  
`\documentclass[handout]{beamer}`

This will generate the files `SP.pdf`, `SP.tex` et al., `SP.knitlog`, `SP_pres.pdf`,
`SP_pres.knitlog` (SP means 'SomePresentation').

### package functions

- **get_texlog_warnings** to get messages from (La)TeX log files via helper function **get_from_log** 
- **knit_single_pres**    to knit a presentation, creating a separate .knitlog file
- **knit_hand_pres**      to knit both versions in parallel (handout and presentation)


### see also

Presentation templates with 
`berryFunctions::`[`createPres`](https://www.rdocumentation.org/packages/berryFunctions/topics/createPres?).
