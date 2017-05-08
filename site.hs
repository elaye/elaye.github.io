{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
import Data.Monoid ((<>))
import Hakyll
import Hakyll.Web.Sass (sassCompiler)
import Hakyll.Favicon (faviconsField, faviconsRules)

import Grid (gridField)
import Link (extLinkField)
import Img (imageRules, imgField, heroField, pictureField)
import Rating (ratingField)


main :: IO ()
main = hakyll $ do
    match "templates/*" $ compile templateBodyCompiler

    -- match "images/**" $ do
    --     route   idRoute
    --     compile copyFileCompiler

    match "posts/*/*/images/**" $ do
      route idRoute
      compile copyFileCompiler

    imageRules "posts/**/resources/images/**.png"
    imageRules "posts/**/resources/images/**.jpg"

    faviconsRules "images/favicon.svg"

    match "css/*.scss" $ do
      route $ setExtension "css"
      let compressCssItem = fmap compressCss
      compile (compressCssItem <$> sassCompiler)

    match "posts/code/*/*" $ do
        route $ setExtension "html"
        let ctx = postCtx <> imgField "img" <> pictureField "picture"
        compile $ getResourceBody
            >>= applyAsTemplate ctx
            >>= loadAndApplyTemplate "templates/post.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

    match "posts/work/*/*" $ do
        route $ setExtension "html"
        let ctx = postCtx <> imgField "img" <> pictureField "picture" <> extLinkField "extLink"
        compile $ getResourceBody
            >>= applyAsTemplate ctx
            >>= loadAndApplyTemplate "templates/post-work.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/code/*/*"
        let ctx = gridField "grid" posts <> defaultCtx
        getResourceBody
          >>= applyAsTemplate ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls

    match "work.html" $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/work/*/*"
        let ctx = gridField "grid" posts <> defaultCtx
        getResourceBody
          >>= applyAsTemplate ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls

    match "about.html" $ do
        route   $ setExtension "html"
        compile $ do
          let ctx = ratingField "rating" <> defaultCtx
          getResourceBody
            >>= applyAsTemplate ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" <>
  defaultCtx

defaultCtx :: Context String
defaultCtx = faviconsField <> defaultContext
