module Metadata
( metadataListField
) where

import Hakyll
import Control.Applicative (empty)

getMetadataListField :: MonadMetadata m => Identifier -> String -> m (Maybe [String])
getMetadataListField identifier key = do
  metadata <- getMetadata identifier
  return $ lookupStringList key metadata

metadataListField :: Context a
metadataListField = Context $ \k _ i -> do
   values <- getMetadataListField (itemIdentifier i) k
   case values of
      Just vs -> do
                 listItems <- mapM makeItem vs
                 return $ ListField (field "item" (return.itemBody)) listItems
      Nothing -> empty
