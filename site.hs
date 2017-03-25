{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
import Data.Monoid ((<>))
import Hakyll

import Data.List (sortBy)

import Debug.Trace (traceShow)

data Grid a = Cell a | Row [Grid a] deriving (Show)

data Weight = Weight Int deriving (Show)

row211 :: Grid Weight
row211 = Row [Cell (Weight 2), Row [Cell (Weight 1), Cell (Weight 1)]]

row112 :: Grid Weight
row112 = Row [Row [Cell (Weight 1), Cell (Weight 1)], Cell (Weight 2)]

row11 :: Grid Weight
row11 = Row [Cell (Weight 1), Cell (Weight 1)]

row11111 :: Grid Weight
row11111 = Row [ Cell (Weight 1), Row [Row [Cell (Weight 1), Cell (Weight 1)], Row [Cell (Weight 1), Cell (Weight 1)]]]

defLayout :: Grid Weight
defLayout = Row
  [ row11111
  , row112
  , row11
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

main :: IO ()
main = do
  hakyll $ do
    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $
        -- No route here
        compile compressCssCompiler

    -- Put all css in one file
    create ["main.css"] $ do
        route idRoute
        compile $ do
            items <- loadAll "css/*" :: Compiler [Item String]
            -- We want reset.css to be first otherwise it will override our styles
            let
              orderedStyles = sortBy sortFn items
              sortFn a b = if itemIdentifier a == fromFilePath "css/reset.css" then LT else GT
            makeItem $ concatMap itemBody orderedStyles

    match (fromList ["about.html", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ getResourceBody
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "posts/code/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/code/*"
        liTpl <- loadBody "templates/grid-item.html"
        ulTpl <- loadBody "templates/grid.html"
        let grid = snd $ fillGrid defLayout posts
        gg <- mkGridHtml "grid" Vert grid liTpl ulTpl defaultContext
        let indexCtx = constField "grid" (itemBody gg) <> defaultContext

        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls


    match "templates/*" $ compile templateBodyCompiler

data Direction = Hor | Vert

mkGridHtml _ _ (Cell (w, c)) liTpl _ ctx = applyTemplate liTpl (cellCtx w) c
mkGridHtml cls dir grid@(Row r) liTpl ulTpl ctx = do
  ul <- mapM (\g -> mkGridHtml cls (ortho dir) g liTpl ulTpl ctx) r :: (Compiler [Item String])
  let uls = concatMap itemBody ul
  makeItem uls >>= applyTemplate ulTpl (ulCtx grid dir)

-- ulCtx :: Direction -> Context String
ulCtx grid dir = ulField <> defaultContext
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
heightRatio (Row row) = (fromIntegral max) / (fromIntegral sum)
  where
    (max, sum) = foldl f init row
    init = (0, 0) :: (Int, Int)
    f (flexMax, flexSum) e = case e of
      -- A sub-row is considered having a flex-grow of 1
      Row r -> (flexMax, flexSum + 1)
      Cell ((Weight w), _) -> ((Prelude.max flexMax w), flexSum + w)

ortho :: Direction -> Direction
ortho dir = case dir of
  Hor -> Vert
  Vert -> Hor

cellCtx ::  Weight -> Context String
cellCtx (Weight w) =
  constField "weight" (show w) <>
  defaultContext

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext
















