module NexusMods.Internal.Indexed (
  fmap,
  return,
  (<*>),
  (>>),
  (>>=),
  join,
  fail,
  mfix,
) where

import Control.Monad.Indexed
import Control.Monad.Indexed.Fix
import Data.Functor.Indexed
import Prelude (String, const)

fmap :: IxFunctor f => (a -> b) -> f i j a -> f i j b
fmap = imap

return :: IxPointed m => a -> m i i a
return = ireturn

infixl 4 <*>

(<*>) :: IxApplicative f => f i j (a -> b) -> f j k a -> f i k b
(<*>) = (<<*>>)

infixl 1 >>

(>>) :: IxApplicative f => f i j a -> f j k b -> f i k b
(>>) = (*>>)

infixl 1 >>=

(>>=) :: IxMonad m => m i j a -> (a -> m j k b) -> m i k b
(>>=) = (>>>=)

join :: IxMonad m => m i j (m j k a) -> m i k a
join = ijoin

fail :: IxMonadZero m => String -> m i j a
fail = const imzero

mfix :: IxMonadFix m => (a -> m i i a) -> m i i a
mfix = imfix
