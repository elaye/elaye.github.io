{-# LANGUAGE OverloadedStrings #-}
module Img
( imgField
, heroField
, imageRules
) where

import Hakyll
import Data.Monoid ((<>))


import Data.Maybe (fromMaybe)
import System.FilePath (takeFileName, takeDirectory, normalise, joinPath, splitPath, splitExtensions, (</>), (<.>))
import Debug.Trace (traceShow)

data ImageSpec = ImageSpec String (Int, Int)

imageSpecs :: [ImageSpec]
imageSpecs =
  -- [ ImageSpec "mobile" (640, 360)
  [ ImageSpec "mobile" (320, 180)
  -- [ ImageSpec "mobile" (160, 90)
  , ImageSpec "tablet" (640, 360)
  -- , ImageSpec "phablet" (320, 180)
  -- , ImageSpec "tablet" (1024, 576)
  -- , ImageSpec "desktop" (1920, 1080)
  , ImageSpec "desktop" (1280, 720)
  -- , ImageSpec "desktop" (1024, 576)
  -- , ImageSpec "desktop" (640, 360)
  ]

heroField :: String -> Context String
heroField name = functionField name $ \args item -> do
  let iid = itemIdentifier item
  metadata <- getMetadata iid
  path <- fromMaybe "" <$> getRoute iid
  let
    missingMetadata name = error $ "Missing " ++ name ++ " for post " ++ show iid
    metadataVal name = fromMaybe (missingMetadata name) $ lookupString name metadata
    relHeroUrl = metadataVal "heroUrl"
    -- To allow relative path for heroUrl
    heroUrl = (takeDirectory path) </> (normalise relHeroUrl)
    title = metadataVal "title"
    cls = if null args then "" else head args
  imgTpl <- loadBody "templates/img.html"
  let imgCtx = srcCtx heroUrl <> constField "class" cls <> constField "alt" title
  itemBody <$> applyTemplate imgTpl imgCtx item

imgField :: String -> Context String
imgField name = functionField name $ \args item -> do
  let (src, alt) =
        if length args /= 2 then error "Missing argument to imgField"
        else (args !! 0, args !! 1)
  imgTpl <- loadBody "templates/img.html"
  let imgCtx = srcCtx src <> constField "alt" alt
    -- imgCtx = constField "src" src <> constField "alt" alt
    -- srcCtx = constField "src" "test"
  itemBody <$> applyTemplate imgTpl imgCtx item

srcCtx :: FilePath -> Context String
srcCtx path = mconcat $ map srcFromSpec imageSpecs
  where
    srcFromSpec (ImageSpec sizeName (w, _)) =
      constField (sizeName ++ "Src") (toConvertedPath sizeName path) <>
      constField (sizeName ++ "Width") (show w ++ "w")

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
      , concat [show x, "x", show y, "^"]
      -- , concat [show x, "x", show y]
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

-- Add suffix to file name and change extension to jpg
toConvertedPath :: String -> FilePath -> FilePath
toConvertedPath sizeName path = dir </> newName
  where
    -- dir = takeDirectory path
    dir = joinPath $ filter (/= "resources/") $ splitPath . takeDirectory $ path
    (base, _) = splitExtensions . takeFileName $ path
    newName = base ++ "-" ++ sizeName <.> "jpg"

-- Take an image identifier arbitrarily nested inside a 'resources'
-- directory and returns its path without the resources directory
-- in the hierarchy and add a suffix
imageRoute :: String -> Identifier -> FilePath
imageRoute sizeName ident = toConvertedPath sizeName $ toFilePath ident
  -- where
    -- path = toFilePath ident
    -- Remove 'resources' from the file path, keeping the same folder structure
    -- dir = joinPath $ filter (/= "resources/") $ splitPath . takeDirectory $ path
    -- newName = takeFileName . toConvertedPath sizeName $ path
    -- (base, exts) = splitExtensions . takeFileName $ path
    -- newName = base ++ "-" ++ sizeName <.> "jpg"

