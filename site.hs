{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
import Data.Monoid ((<>))
import Hakyll

import Grid (gridField)
import Rating (ratingField)

import Data.List (sortBy)

main :: IO ()
main = hakyll $ do
    match "templates/*" $ compile templateBodyCompiler

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
        let ctx = gridField "grid" posts <> defaultContext
        getResourceBody
          >>= applyAsTemplate ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls

    match "about.html" $ do
        route   $ setExtension "html"
        compile $ do
          let ctx = ratingField "rating" <> defaultContext
          getResourceBody
            >>= applyAsTemplate ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls


postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext
















