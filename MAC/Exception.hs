{-# LANGUAGE Trustworthy #-}

-- | Exceptions
module MAC.Exception
    (
       throwMAC
     , catchMAC
    )

where

import MAC.Core (MAC(MkMAC), ioTCB, runMAC)
import Control.Exception

{-|
   Throwing exceptions
-}
throwMAC :: Exception e => e -> MAC l a
throwMAC = ioTCB . throw


{-|
   Throwing and catching exceptions are done among family members with the
   same labels
-}
catchMAC :: Exception e => MAC l a -> (e -> MAC l a) -> MAC l a
catchMAC (MkMAC io) hd = ioTCB $ catch io (runMAC . hd)
