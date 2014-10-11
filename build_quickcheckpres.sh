#!/bin/bash

~/.cabal/bin/lhs2TeX-hl -o quickcheck_presentation.fmt quickcheck_presentation.lhs

cat quickcheck_presentation.lhs | sed -e '/%%%del/,/%%%/d' > quickcheck_pres.lhs

~/.cabal/bin/lhs2TeX -o quickcheck_presentation.tex quickcheck_pres.lhs

pdflatex quickcheck_presentation.tex
