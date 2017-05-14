{-# LANGUAGE OverloadedStrings #-}
module Img
( imgField
, pictureField
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
  [ ImageSpec "mobile" (320, 180)
  , ImageSpec "tablet" (640, 360)
  , ImageSpec "desktop" (1280, 720)
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
  imgTpl <- loadBody "templates/picture.html"
  let imgCtx = srcCtx heroUrl <> constField "imgClass" cls <> constField "alt" title
  itemBody <$> applyTemplate imgTpl imgCtx item

parsePictureFieldArguments :: [String] -> (String, String, Maybe String, Maybe String)
parsePictureFieldArguments args = if length args < 2 then error "Missing argument to pictureField"
  else (src, alt, imgCls, picCls)
    where
      src = args !! 0
      alt = args !! 1
      imgCls = if length args > 2 then Just (args !! 2) else Nothing
      picCls = if length args > 3 then Just (args !! 3) else Nothing

pictureField :: String -> Context String
pictureField name = functionField name $ \args item -> do
  let (src, alt, imgCls, picCls) = parsePictureFieldArguments args
  imgTpl <- loadBody "templates/picture.html"
  let
    imgCtx = case imgCls of
      Just c -> constField "imgClass" c
      Nothing -> mempty
    picCtx = case picCls of
      Just c -> constField "picClass" c
      Nothing -> mempty
    ctx = imgCtx <> picCtx <> srcCtx src <> constField "alt" alt <> defaultContext
  itemBody <$> applyTemplate imgTpl ctx item

imgField :: String -> Context String
imgField name = functionField name $ \args item -> do
  let (src, alt) =
        if length args /= 2 then error "Missing argument to imgField"
        else (args !! 0, args !! 1)
  imgTpl <- loadBody "templates/img.html"
  let imgCtx = constField "src" src <> constField "alt" alt
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
processImage (ImageSpec name (x, y)) = version name $ do
  route $ customRoute (imageRoute name)
  let
    cmd = "convert"
    args =
      [ "-"
      , "-resize"
      , concat [show x, "x", show y, "^"]
      , "-gravity"
      , "Center"
      , "-quality"
      , "50"
      , "+repage"
      , "jpeg:-"
      ]
  compile $ getResourceLBS >>= withItemBody (unixFilterLBS cmd args)

-- Add suffix to file name, change extension to jpg and
-- remove 'resources' directory from hierarchy if it exists
toConvertedPath :: String -> FilePath -> FilePath
toConvertedPath sizeName path = dir </> newName
  where
    dir = joinPath $ filter (/= "resources/") $ splitPath . takeDirectory $ path
    (base, _) = splitExtensions . takeFileName $ path
    newName = base ++ "-" ++ sizeName <.> "jpg"

-- Take an image identifier arbitrarily nested inside a 'resources'
-- directory and returns its path without the resources directory
-- in the hierarchy and add a suffix
imageRoute :: String -> Identifier -> FilePath
imageRoute sizeName ident = toConvertedPath sizeName $ toFilePath ident

