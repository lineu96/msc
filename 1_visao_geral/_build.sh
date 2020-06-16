#!/bin/sh

# Rnw -> TeX
Rscript -e 'knitr::knit("slides.Rnw")'

# TeX -> PDF
pdflatex slides.tex

# Clean.
rm *.aux *.log *.nav *.out *.snm *.toc *.vrb
