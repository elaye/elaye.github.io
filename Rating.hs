module Rating
( ratingField
) where

import Hakyll

import Debug.Trace (traceShow)

ratingField :: String -> Context a
ratingField name = functionField name $ \args _ -> do
  let n = case args of
            [] -> 0
            (x:_) -> read x :: Int
  return $ mkRatingHtml n

mkRatingHtml :: Int -> String
mkRatingHtml n = "<div class=\"rating\">" ++ rating ++ "</div>"
  where
    rating = concat $ filled ++ empty
    filled = replicate n "<span>&#x25C9;</span>" 
    empty = replicate (5 - n) "<span>&#x25CB;</span>" 
