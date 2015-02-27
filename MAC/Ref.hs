{-# LANGUAGE Trustworthy #-}

-- | Mutuable state (references)
module MAC.Ref
    (
       MACRef
     , newMACRef
     , readMACRef
     , writeMACRef
    )

where

import MAC.Lattice
import Data.IORef

-- Trustworthy
import MAC.MAC
-- Unsafe
import MAC.Effects

-- | Labeled references
type MACRef l a = Res l (IORef a)

-- | Creation of labeled references
newMACRef :: Less l l' => a -> MAC l (MACRef l' a)
newMACRef = create . newIORef

-- | Reading labeled references
readMACRef :: Less l' l => MACRef l' a -> MAC l a
readMACRef = readdown readIORef

-- | Writing labeled references
writeMACRef :: Less l l' => MACRef l' a -> a -> MAC l ()
writeMACRef secref v = writeup (flip writeIORef v) secref
