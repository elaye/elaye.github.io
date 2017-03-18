--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Monoid (mappend, (<>))
import Hakyll

import Debug.Trace (traceShow)

-- data Grid a = Cell a | Row [Grid a] deriving (Foldable)
data Grid a = Cell a | Row [Grid a] deriving (Show)

defLayout :: Grid Int
defLayout = Row
  [ Row [Cell 2, Row [Row [Cell 1, Cell 1], Row [Cell 1, Cell 1]]]
  , Row [Cell 1, Cell 1, Cell 1]
  ]

testList :: [String]
-- testList = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"]
testList = ["a", "b", "c", "d", "e", "f"]

testLayout :: Grid Int
testLayout = Row
  [ Cell 1, Row [Cell 1, Cell 2]
  -- [ Row [Cell 1]
  ]

type Weight = Int
type Layout = Grid Weight

fillGrid :: (Show a) => Layout -> [a] -> ([a], Grid (Weight, a))
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
gridConcat (Row a) (Cell b) = Row $ a ++ [(Cell b)]
gridConcat (Cell a) (Cell b) = Row [Cell a, Cell b]
gridConcat (Cell a) (Row b) = Row $ [Cell a] ++ b

--------------------------------------------------------------------------------
main :: IO ()
main = do
  let (l, grid) = fillGrid defLayout testList
  print grid
  print l
  hakyll $ do
    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

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


    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/code/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    -- gridField "posts" postCtx (return posts) <>
                    constField "title" "Home" <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext
    -- dateField "date" "%B %e, %Y" `mappend`
    -- metadataField `mappend`
    -- defaultContext

-- gridField :: String -> Context a -> Compiler [Item a] -> Context String
-- gridField name itemCtx items = listFieldWith "posts" itemCtx f
--   where
--     f item = do
--       is <- 
















