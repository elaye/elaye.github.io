{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
import Data.Monoid ((<>))
import Hakyll
import Hakyll.Web.Sass (sassCompiler)

import Grid (gridField)
import Img (imgField)
import Rating (ratingField)

main :: IO ()
main = hakyll $ do
    match "templates/*" $ compile templateBodyCompiler

    match "images/**" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*.scss" $ do
      route $ setExtension "css"
      let compressCssItem = fmap compressCss
      compile (compressCssItem <$> sassCompiler)

    match "posts/*/*/images/**" $ do
      route idRoute
      compile copyFileCompiler

    match "posts/code/*/*" $ do
        route $ setExtension "html"
        let ctx = postCtx <> imgField "img"
        compile $ getResourceBody
            >>= applyAsTemplate ctx
            >>= loadAndApplyTemplate "templates/post.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/code/*/*"
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
