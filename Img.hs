{-# LANGUAGE OverloadedStrings #-}
module Img
( imgField
, imageRules
) where

import Hakyll
import Data.Monoid ((<>))

import System.FilePath (takeFileName, takeDirectory, joinPath, splitPath, splitExtensions, (</>), (<.>))
import Debug.Trace (traceShow)

data ImageSpec = ImageSpec String (Int, Int)

imageSpecs :: [ImageSpec]
imageSpecs =
  [ ImageSpec "mobile" (640, 360)
  , ImageSpec "tablet" (1024, 576)
  , ImageSpec "desktop" (1920, 1080)
  ]

imgField :: String -> Context a
imgField name = functionField name $ \args item -> do
  let (src, alt) =
        if length args /= 2 then error "Missing argument to imgField"
        else (args !! 0, args !! 1)
  imgTpl <- loadBody "templates/img.html"
  let imgCtx = constField "src" src <> constField "alt" alt
  itemBody <$> applyTemplate imgTpl imgCtx item


imageRules :: Pattern -> Rules ()
imageRules ptn = match ptn $ mapM_ processImage imageSpecs

processImage :: ImageSpec -> Rules ()
-- processImage (name, Nothing) = version name $ do
--   route $ customRoute (imageRoute name)
--   compile copyFileCompiler
processImage (ImageSpec name (x, y)) = version name $ do
  route $ customRoute (imageRoute name)
  let
    cmd = "convert"
    args =
      [ "-"
      , "-resize"
      -- , concat [show x, "x", show y, "^"]
      , concat [show x, "x", show y]
      , "-gravity"
      , "Center"
      , "-quality"
      , "50"
      , "-crop"
      , concat [show x, "x", show y, "+0+0"]
      , "+repage"
      , "jpeg:-"
      ]
  compile $ getResourceLBS >>= withItemBody (unixFilterLBS cmd (traceShow args args))

-- Take an image identifier arbitrarily nested inside a 'resources'
-- directory and returns its path without the resources directory
-- in the hierarchy and add a suffix
imageRoute :: String -> Identifier -> FilePath
imageRoute name ident = dir </> newName
  where
    path = toFilePath ident
    -- Remove 'resources' from the file path, keeping the same folder structure
    dir = joinPath $ filter (/= "resources/") $ splitPath . takeDirectory $ path
    (base, exts) = splitExtensions . takeFileName $ path
    -- newName = base ++ "-" ++ name <.> exts
    newName = base ++ "-" ++ name <.> "jpg"

