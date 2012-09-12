module Nyatris.Shape (
    ShapeCell(..),
    Shape, shape, shapeCells,
    shapeMirrorH, shapeMirrorV,
    shapeLeftL, shapeRightL, shapeDownL, shapeUpL, shapeWidth, shapeHeight,
) where

import Control.Arrow
import Control.Applicative


newtype Shape = Shape { shapeCells :: [(Int, Int)] }

data ShapeCell = FilledCell | EmptyCell

-- Shape will be rotated around its middle cell, so you'll probably want
-- to add extra empty cells to get desired cell in the middle of the shape.
shape :: [[ShapeCell]] -> Shape
shape raw = Shape . concat $ map processRow $ zipWith (,) [startY, startY-1 ..] raw
    where width = maximum $ map length raw
          height = length raw
          startX = negate $ (width-1) `div` 2
          startY = (height-1) `div` 2
          isFilled FilledCell = True
          isFilled _          = False
          processRow (y, row) = map ((flip (,)) y .fst) (
                                    filter (isFilled . snd) (
                                        zipWith (,) [startX ..] row
                                    )
                                )

shapeMirrorH :: Shape -> Shape
shapeMirrorH = Shape . (map $ first negate) . shapeCells

shapeMirrorV :: Shape -> Shape
shapeMirrorV = Shape . (map $ second negate) . shapeCells

-- Linear sizes
shapeLeftL s  = negate $ minimum (fst <$> shapeCells s)
shapeRightL s = maximum (fst <$> shapeCells s)
shapeDownL s  = negate $ minimum (snd <$> shapeCells s)
shapeUpL s    = maximum (snd <$> shapeCells s)

shapeWidth  = (+1) . (uncurry (+)) . (shapeLeftL &&& shapeRightL)
shapeHeight = (+1) . (uncurry (+)) . (shapeDownL &&& shapeUpL)
