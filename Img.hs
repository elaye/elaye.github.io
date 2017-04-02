{-# LANGUAGE OverloadedStrings #-}
module Img
( imgField
) where

import Hakyll
import Data.Monoid ((<>))

imgField :: String -> Context a
imgField name = functionField name $ \args item -> do
  let (src, alt) =
        if length args /= 2 then error "Missing argument to imgField"
        else (args !! 0, args !! 1)
  imgTpl <- loadBody "templates/img.html"
  let imgCtx = constField "src" src <> constField "alt" alt
  itemBody <$> applyTemplate imgTpl imgCtx item
