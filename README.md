### knitPres

knit both handout and presentation version of .Rnw slides with (relatively) good TeX error logging

### install

```R
if(!requireNamespace("remotes", quitly=TRUE)) install.packages("remotes")
remotes::install_github("brry/knitPres")

library("knitPres")
?knit_hand_pres
```

### usage

```R
knit_hand_pres("SomePresentation.Rnw")
```

SomePresentation `SP.Rnw` should contain the handout option in it's first line:  
`\documentclass[handout]{beamer}`

This will generate the files `SP.pdf`, `SP.tex` et al., `SP.knitlog` and `SP_pres.pdf`.

### package functions

- **get_texlog_warnings** to get messages from (La)TeX log files via helper function **get_from_log** 
- **knit_hand_pres**      to knit both versions in parallel (handout and presentation)


### see also

Presentation templates with 
`berryFunctions::`[`createPres`](https://www.rdocumentation.org/packages/berryFunctions/topics/createPres?).
