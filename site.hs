--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
import Data.Monoid ((<>))
import Hakyll

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

type Weight = Int
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
        itemTpl <- loadBody "templates/grid-item.html"
        let grid = snd $ fillGrid defLayout posts
        -- pandocCompiler >>= applyTemplate itemTpl defaultContext
        -- pandocCompiler >>= applyTemplateGrid itemTpl defaultContext grid
        mkGridHtml grid itemTpl defaultContext


    match "templates/*" $ compile templateBodyCompiler

-- applyTemplateGrid :: Template -> Context a -> Grid (Weight, Item a) -> Compiler (Item String)
-- applyTemplateGrid tpl ctx grid = map (applyTemplate ctx) grid

-- mkGridHtml :: (Show a) => Grid (Weight, Item a) -> String
mkGridHtml :: (Show a) => Grid (Weight, Item a) -> Template -> Context a -> Compiler (Item String)
-- mkGridHtml (Cell (_, c)) = show $ itemBody c
mkGridHtml (Cell (_, c)) tpl ctx = do
  li <- applyTemplate tpl ctx c
  makeItem $ "<li>" ++ (itemBody li) ++ "</li>"
mkGridHtml (Row r) tpl ctx = do
  -- let
  --   f acc g = makeItem $ acc ++ (itemBody r)
  --     where r = case g of
  --                 cell@(Cell _) -> do
  --                   li <- mkGridHtml cell tpl ctx
  --                   return $ makeItem $ "<li>" ++ (itemBody li) ++ "</li>"
  --                 row@(Row _) -> do
  --                   ul <- mkGridHtml cell tpl ctx
  --                   return $ makeItem $ "<ul>" ++ (itemBody ul) ++ "</ul>"

  -- return $ foldl f (makeItem "") r
  ul <- mapM (\g -> mkGridHtml g tpl ctx) r :: (Compiler [Item String])
  -- let uls = foldl (\i acc -> (itemBody i) ++ acc) "" ul
  let uls = foldl ulFolder "" ul :: String
  makeItem $ "<ul>" ++ uls ++ "</ul>"

-- ulFolder :: Item String -> String -> String
ulFolder :: String -> Item String -> String
-- ulFolder item acc = (itemBody item) ++ acc
ulFolder acc item = (itemBody item) ++ acc

-- -- mkGridHtml :: (Show a) => Grid (Weight, Item a) -> String
-- mkGridHtml :: (Show a) => Grid (Weight, Item a) -> Template -> Context a -> Compiler (Item String)
-- -- mkGridHtml (Cell (_, c)) = show $ itemBody c
-- mkGridHtml (Cell (_, c)) tpl ctx = applyTemplate tpl ctx c
-- mkGridHtml (Row r) tpl ctx =
--   let
--     f acc g = makeItem $ acc ++ (itemBody r)
--       where r = case g of
--                   cell@(Cell _) -> do
--                     li <- mkGridHtml cell tpl ctx
--                     return $ makeItem $ "<li>" ++ (itemBody li) ++ "</li>"
--                   row@(Row _) -> do
--                     ul <- mkGridHtml cell tpl ctx
--                     return $ makeItem $ "<ul>" ++ (itemBody ul) ++ "</ul>"

--   return $ foldl f (makeItem "") r

  -- where
  --   f acc g = compMap (\e -> acc ++ e) r
  --     where
  --       r = case g of
  --         -- Cell (w, c) -> "<li>" ++ show w ++ "</li>"
  --         -- row -> "<ul>" ++ mkGridHtml row ++ "</ul>"
  --         -- cell@(Cell (w, c)) -> "<li>" ++ (mkGridHtml cell tpl ctx) ++ "</li>"
  --         -- row -> "<ul>" ++ (mkGridHtml row tpl ctx) ++ "</ul>"
  --         cell@(Cell (w, c)) -> compMap (\e -> "<li>" ++ e ++ "</li>") (mkGridHtml cell tpl ctx)
  --         row -> compMap (\e -> "<ul>" ++ e ++ "</ul>") (mkGridHtml row tpl ctx)
  --       compMap f comp = map (\x -> map f x) comp

-- -- mkGridHtml :: (Show a) => Grid (Weight, Item a) -> String
-- mkGridHtml :: (Show a) => Grid (Weight, Item a) -> Template -> Context a -> Compiler (Item String)
-- -- mkGridHtml (Cell (_, c)) = show $ itemBody c
-- mkGridHtml (Cell (_, c)) tpl ctx = applyTemplate tpl ctx c
-- mkGridHtml (Row r) tpl ctx = foldl f (return (makeItem "")) r
--   where
--     f acc g = compMap (\e -> acc ++ e) r
--       where
--         r = case g of
--           -- Cell (w, c) -> "<li>" ++ show w ++ "</li>"
--           -- row -> "<ul>" ++ mkGridHtml row ++ "</ul>"
--           -- cell@(Cell (w, c)) -> "<li>" ++ (mkGridHtml cell tpl ctx) ++ "</li>"
--           -- row -> "<ul>" ++ (mkGridHtml row tpl ctx) ++ "</ul>"
--           cell@(Cell (w, c)) -> compMap (\e -> "<li>" ++ e ++ "</li>") (mkGridHtml cell tpl ctx)
--           row -> compMap (\e -> "<ul>" ++ e ++ "</ul>") (mkGridHtml row tpl ctx)
--         compMap f comp = map (\x -> map f x) comp

-- mkGridHtml :: (Show a) => Grid (Weight, Item a) -> String
-- -- mkGridHtml (Cell (_, c)) = show $ itemBody c
-- mkGridHtml (Cell (_, c)) = "body"
-- mkGridHtml (Row r) = foldl f "" r
--   where
--     f acc g = acc ++ r
--       where
--         r = case g of
--           Cell (w, c) -> "<li>" ++ show w ++ "</li>"
--           row -> "<ul>" ++ mkGridHtml row ++ "</ul>"

gridCtx :: (Show a) => String -> Grid (Weight, Item a) -> Context String
gridCtx name grid =
  constField name "TEst" <>
  -- constField name (mkGridHtml grid) <>
  defaultContext

-- gridCtx :: String -> Grid (Weight, Item a) -> Context String
-- gridCtx name grid =
--   constField name "TEst" <>
--   defaultContext


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
















