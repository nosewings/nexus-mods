module NexusMods.Internal.Util (
  catEithers,
  liftEither,
  mapLeft,
) where

-- | Like @catMaybes@ on @Right@s, but fails on the first @Left@.
catEithers :: [Either a b] -> Either a [b]
catEithers = go []
 where
  go acc [] = Right (reverse acc)
  go _ (Left err : _) = Left err
  go acc (Right x : xs) = go (x : acc) xs

-- | The usual @liftEither@ works on @MonadError@, but this works on
-- @MonadFail@.
liftEither :: MonadFail m => Either String a -> m a
liftEither = either fail return

mapLeft :: (a -> a') -> Either a b -> Either a' b
mapLeft f (Left x) = Left (f x)
mapLeft _ (Right y) = Right y
