module Link
( extLinkField
) where

import Hakyll

extLinkField :: String -> Context a
extLinkField name = functionField name $ \args _ -> if length args < 2
  then error ("Missing argument to field '" ++ name ++ "'")
  else return $ mkLinkHtml (args !! 0) (args !! 1)

mkLinkHtml :: String -> String -> String
mkLinkHtml txt link = "<a href=\"" ++ link ++ "\" target=\"_blank\">" ++ txt ++ " &#10230;</a>"
