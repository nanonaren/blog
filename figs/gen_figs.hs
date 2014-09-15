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

----

ex1 = [ ["0","0","X","0"]
      , ["0","X","0","1"]
      , ["X","0","1","1"]
      , ["0","1","1","1"]
      ]
ex2 = [ ["0","0","X","0(1)"]
      , ["0","X","0(2)","1"]
      , ["X","0(3)","1","1"]
      , ["0(4)","1","1","1"]]
ex3 = [ ["0","0","0","X"]
      , ["0","0","X","1"]
      , ["0","X","1","1"]
      , ["X","1","1","1"]]

draw = vcat . map (hcat . map (\x -> square 0.1 <> (text x # fontSize (Local 0.03))))

hungarian_fig = hcat' (with & sep .~ 0.1) [draw ex1,draw ex2,draw ex3]

main = multiMain
       [
         ("schensted_fig1",schensted_fig)
       , ("hungarian_fig1",hungarian_fig)
       ]
