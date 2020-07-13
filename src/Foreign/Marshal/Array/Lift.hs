module Foreign.Marshal.Array.Lift
( mallocArray
, allocaArray
, peekArray
, pokeArray
, withArray
, withArrayLen
) where

import           Control.Carrier.Lift
import qualified Foreign.Marshal.Array as A
import           Foreign.Ptr
import           Foreign.Storable

mallocArray :: (Storable a, Has (Lift IO) sig m) => Int -> m (Ptr a)
mallocArray = sendIO . A.mallocArray
{-# INLINABLE mallocArray #-}

allocaArray :: (Has (Lift IO) sig m, Storable a) => Int -> (Ptr a -> m b) -> m b
allocaArray n with = liftWith $ \ hdl ctx -> A.allocaArray n (hdl . (<$ ctx) . with)
{-# INLINABLE allocaArray #-}

peekArray :: (Has (Lift IO) sig m, Storable a) => Int -> Ptr a -> m [a]
peekArray n = sendM . A.peekArray n
{-# INLINABLE peekArray #-}

pokeArray :: (Has (Lift IO) sig m, Storable a) => Ptr a -> [a] -> m ()
pokeArray p = sendM . A.pokeArray p
{-# INLINABLE pokeArray #-}

withArray :: (Has (Lift IO) sig m, Storable a) => [a] -> (Ptr a -> m b) -> m b
withArray as with = liftWith $ \ hdl ctx -> A.withArray as (hdl . (<$ ctx) . with)
{-# INLINABLE withArray #-}

withArrayLen :: (Has (Lift IO) sig m, Storable a) => [a] -> (Int -> Ptr a -> m b) -> m b
withArrayLen as with = liftWith $ \ hdl ctx -> A.withArrayLen as (\ n -> hdl . (<$ ctx) . with n)
{-# INLINABLE withArrayLen #-}
