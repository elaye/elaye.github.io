{-# LANGUAGE OverloadedStrings #-}
module Grid
( gridField
) where

import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import System.FilePath (takeDirectory, normalise, (</>))
import Hakyll

import Img (imgField, heroField)

import Debug.Trace (traceShow)

data Grid a = Cell a | Row [Grid a] deriving (Show)

data Weight = Weight Int deriving (Show)

gridField :: String -> [Item String] -> Context String
gridField name items = field name $ \_ -> do
  liTpl <- loadBody "templates/grid-cell.html"
  ulTpl <- loadBody "templates/grid-group.html"
  let grid = snd $ fillGrid defLayout items
  itemBody <$> mkGridHtml Vert grid liTpl ulTpl

row211 :: Grid Weight
row211 = Row [Cell (Weight 2), Row [Cell (Weight 1), Cell (Weight 1)]]

row112 :: Grid Weight
row112 = Row [Row [Cell (Weight 1), Cell (Weight 1)], Cell (Weight 2)]

row11 :: Grid Weight
row11 = Row [Cell (Weight 1), Cell (Weight 1)]

-- No more than 3 tiles per row, otherwise it looks weird on tablet
defLayout :: Grid Weight
defLayout = Row
  [ row211
  -- , Row [Cell (Weight 1)]
  , row112
  -- , row11
  , row211
  ]

type Layout = Grid Weight

-- fillGrid :: (Show a) => Layout -> [a] -> ([a], Grid (Weight, a))
fillGrid :: Layout -> [a] -> ([a], Grid (Weight, a))
fillGrid (Cell _) [] = ([], Row [])
fillGrid (Cell w) (i:is) = (is, Cell (w, i))
fillGrid (Row r) items = foldl f initAcc r
  where
    f (is, grid) g = case is of
      [] -> ([], grid)
      ais@(i:iss) -> case g of
        Cell w -> (iss, gridConcat grid (Cell (w, i)))
        row -> (niss, gridConcat grid (Row [nrow]))
          where (niss, nrow) = fillGrid row ais
    initAcc = (items, Row [])

gridConcat :: Grid a -> Grid a -> Grid a
gridConcat (Row a) (Row b) = Row $ a ++ b
gridConcat (Row a) (Cell b) = Row $ a ++ [Cell b]
gridConcat (Cell a) (Cell b) = Row [Cell a, Cell b]
gridConcat (Cell a) (Row b) = Row $ Cell a:b


data Direction = Hor | Vert

mkGridHtml :: Direction -> Grid (Weight, Item String) -> Template -> Template -> Compiler (Item String)
mkGridHtml _ (Cell (w, c)) liTpl _ = applyTemplate liTpl (cellCtx w) c
mkGridHtml dir grid@(Row r) liTpl ulTpl = do
  ul <- mapM (\g -> mkGridHtml (ortho dir) g liTpl ulTpl) r :: (Compiler [Item String])
  let uls = concatMap itemBody ul
  makeItem uls >>= applyTemplate ulTpl (ulCtx dir grid)

ulCtx :: Direction -> Grid (Weight, Item String) -> Context String
ulCtx dir grid = ulField <> defaultContext
  where
    height = 100 * 9 / 16 * heightRatio grid
    ulField = case dir of
      Hor -> constField "class" "grid__row" <> constField "row-height" (show height ++ "vw")
      Vert -> constField "class" "grid__col" <> constField "row-height" "100%"

-- Calculate a ratio for the row height
-- depending on the flex-grow of its elements
-- and the total number of elements.
-- This ratio expresses the fact that the row height depends
-- on the height of its biggest element.
heightRatio :: Grid (Weight, Item a) -> Float
heightRatio (Cell _) = 1
heightRatio (Row row) = fromIntegral fMax / fromIntegral fSum
  where
    (fMax, fSum) = foldl f fInit row
    fInit = (0, 0) :: (Int, Int)
    f (flexMax, flexSum) e = case e of
      -- A sub-row is considered having a flex-grow of 1
      Row _ -> (flexMax, flexSum + 1)
      Cell (Weight w, _) -> (max flexMax w, flexSum + w)

ortho :: Direction -> Direction
ortho dir = case dir of
  Hor -> Vert
  Vert -> Hor

cellCtx ::  Weight -> Context String
cellCtx (Weight w) =
  constField "weight" (show w) <>
  heroField "hero" <>
  defaultContext
