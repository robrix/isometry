module Foreign.Storable.Lift
( S.Storable(S.sizeOf, S.alignment)
, peek
, poke
) where

import Foreign.Ptr
import qualified Foreign.Storable as S
import Control.Effect.Lift

peek :: (Has (Lift IO) sig m, S.Storable a) => Ptr a -> m a
peek = sendIO . S.peek

poke :: (Has (Lift IO) sig m, S.Storable a) => Ptr a -> a -> m ()
poke p = sendIO . S.poke p
