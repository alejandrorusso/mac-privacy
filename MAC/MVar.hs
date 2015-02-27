{-# LANGUAGE Trustworthy #-}

-- | Synchronization primitives
module MAC.MVar
    (
       MACMVar
     , newMACMVar
     , newMACEmptyMVar
     , takeMACMVar
     , putMACMVar
    )

where

import MAC.Lattice
-- Trustworthy
import MAC.MAC
-- Unsafe
import MAC.Effects

import Control.Concurrent.MVar

-- | Labeled @MVars@
type MACMVar l a = Res l (MVar a)

-- | Creation of a labeled @MVar@
newMACMVar :: Less l l' => a -> MAC l (MACMVar l' a)
newMACMVar = create . newMVar

-- | Creation of an empty labeled @MVar@
newMACEmptyMVar :: Less l l' => MAC l (MACMVar l' a)
newMACEmptyMVar = create newEmptyMVar

-- | Securely taking a labeled @MVar@
takeMACMVar :: Less l l => MACMVar l a -> MAC l a
takeMACMVar = rw_read takeMVar

-- | Securely writing into a labeled @MVar@
putMACMVar :: Less l l => MACMVar l a -> a -> MAC l ()
putMACMVar secmv v = rw_write (flip putMVar v) secmv
