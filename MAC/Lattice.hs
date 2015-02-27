{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE EmptyDataDecls #-}

{-# LANGUAGE Safe #-}

-- | Encodes a security lattice.
module MAC.Lattice
    (
      Less ()
    , H
    , L
    )
where


-- | Label for public data
data L
-- | Label for secrets
data H

-- Pablo's trick to avoid instances
-- Define a super-class
-- | Type class used to avoid arbitrary instances by attackers (Pablo's trick)
class CanFlowTo l l' where

-- | Type class encoding security lattices
class CanFlowTo l l' => Less l l' where

instance Less L L where
instance Less L H where
instance Less H H where

instance CanFlowTo L L where
instance CanFlowTo L H where
instance CanFlowTo H H where
