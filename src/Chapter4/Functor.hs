module Chapter4.Functor where

newtype FMaybe a =
  FMaybe (Maybe a)
  deriving (Show)

instance Functor FMaybe where
  fmap f (FMaybe (Just v)) = FMaybe $ Just $ f v
  fmap _ _ = FMaybe Nothing
  
instance Foldable FMaybe where
  foldMap f (FMaybe (Just v)) = f v
  foldMap _ _ = mempty