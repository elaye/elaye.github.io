{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
import Data.Monoid ((<>))
import Hakyll
import Hakyll.Web.Sass (renderSass)
import Hakyll.Favicon (faviconsField, faviconsRules)

import Grid (gridField, row211, row112, row11, row111, Grid(..))
import Link (extLinkField)
import Img (imageRules, imgField, heroField, pictureField)
import Rating (ratingField)


main :: IO ()
main = hakyll $ do
    match "posts/*/*/images/**" $ do
      route idRoute
      compile copyFileCompiler

    imageRules "posts/**/resources/images/**.png"
    imageRules "posts/**/resources/images/**.jpg"

    faviconsRules "images/favicon.svg"

    match "images/**" $ do
      route idRoute
      compile copyFileCompiler

    match "posts/code/*/snippets/**" $ do
      route idRoute
      compile pandocCompiler

    match "css/*.scss" $ do
      compile getResourceBody

    scssDependencies <- makePatternDependency "css/*.scss"
    rulesExtraDependencies [scssDependencies] $ do
      create ["css/main.css"] $ do
        route idRoute
        compile $ do
          mainScss <- load "css/main.scss"
          let compressCssItem = fmap compressCss
          compressCssItem <$> renderSass mainScss

    match "templates/*" $ compile templateBodyCompiler

    match "posts/code/*/index.html" $ do
        route $ setExtension "html"
        let ctx = postCtx <> imgField "img" <> pictureField "picture" <> snippetField
        compile $ getResourceBody
            >>= applyAsTemplate ctx
            >>= loadAndApplyTemplate "templates/post.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

    match "posts/work/*/index.html" $ do
        route $ setExtension "html"
        let ctx = postCtx <> imgField "img" <> pictureField "picture" <> extLinkField "extLink"
        compile $ getResourceBody
            >>= applyAsTemplate ctx
            >>= loadAndApplyTemplate "templates/post-work.html" ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls


    -- TODO: make grid layout dynamic
    let codeGridLayout = Row
          -- [ row211
          [ row111
          , row11
          , row112
          , row11
          ]
    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/code/*/index.html"
        let ctx = gridField "grid" posts codeGridLayout <> defaultCtx
        getResourceBody
          >>= applyAsTemplate ctx
          >>= loadAndApplyTemplate "templates/default.html" ctx
          >>= relativizeUrls

    -- TODO: make grid layout dynamic
    let workGridLayout = Row
          [ row211
          , row112
          ]
    match "work.html" $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/work/*/*"
        let ctx = gridField "grid" posts workGridLayout <> defaultCtx
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
