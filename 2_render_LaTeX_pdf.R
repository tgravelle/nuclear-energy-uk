rm(list=ls())
getwd()

library(tinytex)

latexmk("./manuscript/main.tex", pdf_file = "./manuscript/SMNRs in the UK-draft.pdf")