--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
import Data.Monoid ((<>))
import Hakyll

import Debug.Trace (traceShow)

-- data Grid a = Cell a | Row [Grid a] deriving (Foldable)
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
  -- [ Row [Cell (Weight 2), Row [Row [Cell (Weight 1), Cell (Weight 1)], Row [Cell (Weight 1), Cell (Weight 1)]]]
  [ row11111
  , row112
  , row11
  , row211
  ]

testList :: [String]
-- testList = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"]
testList = ["a", "b", "c", "d", "e", "f"]

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

--------------------------------------------------------------------------------
main :: IO ()
main = do
  -- let (l, grid) = fillGrid defLayout testList
  -- print grid
  -- print l
  hakyll $ do
    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        -- compile $ compressCssCompiler >>= relativizeUrls
        compile $ compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
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

    -- create ["archive.html"] $ do
    --     route idRoute
    --     compile $ do
    --         posts <- recentFirst =<< loadAll "posts/code/*"
    --         let archiveCtx =
    --                 listField "posts" postCtx (return posts) `mappend`
    --                 constField "title" "Archives"            `mappend`
    --                 defaultContext

    --         makeItem ""
    --             >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
    --             >>= loadAndApplyTemplate "templates/default.html" archiveCtx
    --             >>= relativizeUrls


    -- match "index.html" $ do
    --     route idRoute
    --     compile $ do
    --         posts <- recentFirst =<< loadAll "posts/code/*"
    --         let indexCtx =
    --                 listField "posts" postCtx (return posts) <>
    --                 -- gridField "posts" postCtx (return posts) <>
    --                 gridCtx "items" (snd (fillGrid defLayout posts)) <>
    --                 constField "title" "Home" <>
    --                 defaultContext

    --         getResourceBody
    --             >>= applyAsTemplate indexCtx
    --             >>= loadAndApplyTemplate "templates/default.html" indexCtx
    --             >>= relativizeUrls

    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/code/*"
        liTpl <- loadBody "templates/grid-item.html"
        -- ulTpl <- loadBody "templates/grid.html"
        let grid = snd $ fillGrid defLayout posts
        gg <- mkGridHtml "grid" Hor grid liTpl liTpl defaultContext
        let indexCtx = constField "grid" (itemBody gg) <> defaultContext

        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls


    match "templates/*" $ compile templateBodyCompiler

data Direction = Hor | Vert

-- mkGridHtml :: (Show a) => String -> Direction -> Grid (a, Item a) -> Template -> Template -> Context a -> Compiler (Item String)
mkGridHtml _ _ (Cell (w, c)) liTpl _ ctx = applyTemplate liTpl (cellCtx w) c
mkGridHtml cls dir grid@(Row r) liTpl ulTpl ctx = do
  let cl = case dir of
          Hor -> "grid__row"
          Vert -> "grid__col"
  ul <- mapM (\g -> mkGridHtml cl (ortho dir) g liTpl ulTpl ctx) r :: (Compiler [Item String])
  let uls = concatMap itemBody ul
  let height = case dir of
        -- Vert -> "height: calc(100vw * 9 / 16 * " ++ (show (heightRatio grid)) ++ ");"
        Vert -> "height: calc(100vw * 9 / 16 * " ++ (show (heightRatio grid)) ++ ");"
        Hor -> "height: 100%;"
  makeItem $ "<ul class=\"" ++ cls ++ "\" style=\"" ++ height ++ "\">" ++ uls ++ "</ul>"

ulCtx :: Context String
ulCtx = constField "class" "ul-class" <> defaultContext

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

-- 	-- flexRatio(layout) {
-- 		const r = layout.reduce((r, cell) => {
-- 			if (Array.isArray(cell)) {
-- 				// A sub-array is considered having a flex-grow of 1
-- 				r.flexSum += 1;
-- 				return r;
-- 			}
-- 			const { flexMax, flexSum } = r;
-- 			return {
-- 				flexMax: cell > flexMax ? cell : flexMax,
-- 				flexSum: flexSum + cell,
-- 			};
-- 		}, { flexMax: 0, flexSum: 0 });
-- 		return r.flexMax / r.flexSum;
-- }

ortho :: Direction -> Direction
ortho dir = case dir of
  Hor -> Vert
  Vert -> Hor

cellCtx ::  Weight -> Context String
cellCtx (Weight w) =
  constField "weight" (show w) <>
  defaultContext

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext
















