{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

data Triangle = Triangle { scale' :: Double
                         , angle :: Double
                         }

multTup :: Num a => a -> (a, a) -> (a, a)
multTup a (x, y) = (a*x, a*y)
                             
display :: Triangle -> Diagram B
display (Triangle s a) = fromOffsets . map r2 $
                           map (multTup s)
                             [(cos(a),0), (0, sin(a)), (-cos(a), -sin(a))]

myTri = Triangle 10 (pi/8)

main = defaultMain $ display myTri
