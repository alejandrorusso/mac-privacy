{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}

-- | Provide primitives to communicate among family members. It provides an API for sequential 'linkMAC' and concurrent ('forkMAC') setting
module MAC.Control
    (
     -- Defined here
       linkMAC      -- Secure communication for sequential programs
     , forkMAC     -- Spawing threads
     , forkMACMVar -- Returning futures
    )

where

import MAC.Lattice
import MAC.Core (MAC(),ioTCB,runMAC)
import MAC.Exception
import MAC.Labeled
import MAC.MVar

import Control.Exception
import Control.Concurrent

{-|
   Primitive which allows family members to safely communicate. The function
   finishes even if an exception is raised---the exception is rethrown when
   the returned value gets inspected
-}
linkMAC :: (Less l l') => MAC l' a -> MAC l (Labeled l' a)
linkMAC m = (ioTCB . runMAC)
              (catchMAC m (\(e :: SomeException) -> throwMAC e)) >>= label


-- | Safely spawning new threads
forkMAC :: Less l l' => MAC l' () -> MAC l ()
forkMAC m = (ioTCB . forkIO . runMAC) m >> return ()

{-|
   Safely spawning new threads. The function returns a labeled 'MVar' where
   the outcome of the thread is stored
-}
forkMACMVar :: (Less l' l', Less l l') => MAC l' a -> MAC l (MACMVar l' a)
forkMACMVar m = do lmv <- newMACEmptyMVar
                   forkMAC (m >>= putMACMVar lmv)
                   return lmv
