module NexusMods.Surgery (
  toOR',
  fromOR,
  removeRField,
  insertRField',
  modifyRField,
  animate,
) where

import Control.Monad.Indexed.State
import GHC.Generics
import Generic.Data.Surgery (Data, FromORRep, InsRField, ModRField, OR, RmvRField, ToOR)
import Generic.Data.Surgery qualified as GDS
import NexusMods.Indexed qualified as Indexed
import Prelude

toOR' :: forall f l x. ToOR f l => IxState (Data f x) (OR l x) ()
toOR' = imodify GDS.toOR'

fromOR :: forall a l x. (Generic a, FromORRep a l) => IxState (OR l x) a ()
fromOR = imodify GDS.fromOR

removeRField :: forall fd n t lt l x. RmvRField fd n t lt l => IxState (OR lt x) (OR l x) t
removeRField = IxState (GDS.removeRField @fd @n @t @lt @l @x)

insertRField' :: forall fd n t lt l x. InsRField fd n t lt l => t -> IxState (OR l x) (OR lt x) ()
insertRField' = imodify . GDS.insertRField' @fd @n @t @lt @l @x

modifyRField :: forall fd n t t' lt lt' l x. ModRField fd n t t' lt lt' l => (t -> t') -> IxState (OR lt x) (OR lt' x) ()
modifyRField = imodify . GDS.modifyRField @fd @n @t @t' @lt @lt' @l @x

-- | Given a synthetic object, perform an operation and bring it to
-- life.
animate :: (Generic a, ToOR f l, FromORRep a l') => IxState (OR l x) (OR l' x) () -> Data f x -> a
animate k =
  snd . runIxState Indexed.do
    toOR'
    k
    fromOR
