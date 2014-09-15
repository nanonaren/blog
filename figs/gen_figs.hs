{-# LANGUAGE NoMonomorphismRestriction #-}
module Main
    (
      main
    ) where

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

cell x nm = ((text (show x) # fontSize (Local 0.07)) <> square 0.2) # named nm

yt = vcat . map hcat . zipWith (\r -> zipWith (\c x -> cell x (r,c)) [(1::Int)..]) [(1::Int)..]

schensted_fig = vcat' (with & sep .~ 0.1)
    [ hcat' (with & sep .~ 0.1) [yt [[1,2,2],[3]], text "." # fontSizeN 0.05, yt [[3,5],[4]]]
    , hcat' (with & sep .~ 0.1) [text "=" # fontSizeN 0.05, yt [[1,2,2,4],[3]], text "." # fontSizeN 0.05, yt [[3,5]]]
    , hcat' (with & sep .~ 0.1) [text "=" # fontSizeN 0.05, yt [[1,2,2,3,5],[4]], text "." # fontSizeN 0.05, yt [[5]]]
    , hcat' (with & sep .~ 0.1) [text "=" # fontSizeN 0.05, yt [[1,2,2,3,5],[3,4]]]
    ]

main = multiMain
       [
         ("schensted_fig1",schensted_fig)
       ]
