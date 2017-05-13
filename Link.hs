module Link
( extLinkField
) where

import Hakyll

extLinkField :: String -> Context a
extLinkField name = functionField name $ \args _ -> if length args < 2
  then error ("Missing argument to field '" ++ name ++ "'")
  else return $ mkLinkHtml (args !! 0) (args !! 1)

mkLinkHtml :: String -> String -> String
mkLinkHtml txt link = txtLink ++ " " ++ arrowLink
  where
    txtLink = "<a href=\"" ++ link ++ "\" target=\"_blank\">" ++ txt ++ "</a>"
    arrowLink = "<a href=\"" ++ link ++ "\" target=\"_blank\" style=\"text-decoration: none;\">&#10230;</a>"
