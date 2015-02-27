{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Unsafe #-}

-- | It provides functions which map read and write effects into security checks.
module MAC.Effects
    (
       create
     , writeup
     , readdown
     , fix
     , read_and_fix
     , write_and_fix
     , rw_read
     , rw_write
    )

where

import MAC.Lattice
import MAC.Core

{-|
    It lifts functions which create resources into secure functions which
    create labeled resources
-}
create :: Less l l' => IO (d a) -> MAC l (Res l' (d a))
create io = ioTCB io >>= return . MkRes


{-|
    It lifts an 'IO'-action which writes into a data type @d a@
    into a secure function which writes into a labeled resource
-}
writeup :: Less l l' => (d a -> IO ()) -> Res l' (d a) -> MAC l ()
writeup io (MkRes a) = ioTCB $ io a

{-|
    It lifts an 'IO'-action which reads from a data type @d a@
    into a secure function which reads from a labeled resource
-}
readdown :: Less l' l => (d a -> IO a) -> Res l' (d a) -> MAC l a
readdown io (MkRes da) = ioTCB $ io da

-- | Proxy function to set the index of the family member 'MAC'
fix :: l -> MAC l ()
fix _ = return ()

-- | Auxiliary function. A combination of 'fix' and 'readdown'.
read_and_fix :: Less l l => (d a -> IO a) -> Res l (d a) -> MAC l a
read_and_fix io r = fix (labelOf r) >> readdown io r

-- | Auxiliary function. A combination of 'fix' and 'readdown'.
write_and_fix :: Less l' l' => (d a -> IO ()) -> Res l' (d a) -> MAC l' ()
write_and_fix io r = fix (labelOf r) >> writeup io r

{-|
    It lifts an operation which perform a read on data type @d a@, but
    it also performs a write on it as side-effect
-}
rw_read :: (Less l l', Less l' l) => (d a -> IO a) -> Res l' (d a) -> MAC l a
rw_read io r = writeup (\_ -> return ()) r >> readdown io r

{-|
    It lifts an operation which perform a write on data type @d a@, but
    it also performs a read on it as side-effect
-}
rw_write :: (Less l l', Less l' l) => (d a -> IO ()) -> Res l' (d a) -> MAC l ()
rw_write io r = readdown (\_ -> return undefined) r >> writeup io r
