module Numeric.Unum where

import Data.Bits ((.|.),
                  shiftL)
import qualified Data.BitVector as BV
import Data.Int

data BitArray = BitArray

esizesize :: Int
esizesize = 4

fsizesize :: Int
fsizesize = 5

-- |A general bound.
--
--  This is a helper data structure that stores various
--  low-level bits of information about unums
data Gbound = Gbound {-# UNPACK #-} !Bool
                     {-# UNPACK #-} !BV.BitVector
                     {-# UNPACK #-} !Bool
                     {-# UNPACK #-} !BV.BitVector
                     {-# UNPACK #-} !Bool
                     {-# UNPACK #-} !Bool

-- |A Unum, stored as a bit array.
--  A Unum is very much like an IEEE float, with the difference that
--  it stores the sizes of its exponent and fraction in additional metadata
--  at the right end.
--
--  Going from left to right, the format is as follows:
--
-- * The first bit is the sign.
-- * @esize@ bits are the exponent.
-- * @fsize@ bits are the fraction.
-- * 1 bit is the ubit that is 1 iff the number is exact.
-- * @esizesize@ bits are the exponent size @esize@.
-- * @fsizesize@ bits are the fraction size @fsize@.
newtype Unum = Unum BV.BitVector

-- |A bitmask.
type Bitmask = BV.BitVector

-- |A UBound.
--  TODO
data Ubound = Ubound

--------------------------------------------------------------------------------

-- |The maximum exponent size.
esizemax :: Int
esizemax = 2^esizesize

-- |The maximum fraction size.
fsizemax :: Int
fsizemax = 2^fsizesize

-- |The size of the utag, in bits.
utagsize :: Int
utagsize = 1 + fsizesize + esizesize

-- |The maximum number of bits a ubit can take.
maxubits :: Int
maxubits = 1 + esizemax + fsizemax + utagsize

-- |A bitmask to get only the ubit out of a unum.
ubitmask :: Bitmask
ubitmask = shiftL (BV.bitVec (utagsize - 1) 1) (utagsize - 1)

-- |A bitmask to only get the fsize-part from a unum.
fsizemask :: Bitmask
fsizemask = BV.bitVec fsizesize (fsizemax - 1)

-- |A bitmask to only get the esize-part of a unum
esizemask :: Bitmask
esizemask = shiftL (BV.bitVec (fsizesize + esizesize) (esizemax - 1)) fsizesize

-- |A bitmask to get the fsize-part and the esize-part from a enum.
--
-- @
-- efsizemask = fsizemask | esizemask
-- @
efsizemask :: Bitmask
efsizemask = fsizemask .|. esizemask

-- |A bitmask to get the entire utag from an enum.
--
-- @
-- utagmask = efsizemask | ubitmask
-- @
utagmask :: Bitmask
utagmask = ubitmask .|. efsizemask

-- |The least significant bit of a unum's fraction.
ulpu :: Bitmask
ulpu = shiftL (BV.bitVec 1 1) utagsize

-- |The smallest positive number that can be represented.
smallnormalu :: Unum
smallnormalu =
   Unum $! BV.bitVec maxubits 0
           .|. efsizemask
           .|. shiftL ubitmask 1

-- |A bitmask for the sign of a unum, provided the unum has 'maxubits' bits.
signbigu :: Bitmask
signbigu = shiftL (BV.bitVec 1 1) (maxubits - 1)

-- |Positive infinity.
posinfu :: Unum
posinfu = Unum $! BV.ones maxubits `BV.nand` (signbigu .|. ubitmask)

-- |Negative infinity.
neginfu :: Unum
neginfu = Unum $! BV.ones maxubits `BV.nand` ubitmask

-- |The largest positive real number that can be represented.
maxrealu :: Unum
maxrealu = Unum $! inf `BV.nand` ulpu
   where
      (Unum inf) = posinfu

-- |The largest negative real number that can be represented.
minrealu :: Unum
minrealu = Unum $! (inf `BV.nand` ulpu) .|. signbigu
   where
      (Unum inf) = posinfu

-- |Alias for 'minrealu'. This was included because it appeared in the prototype.
negbigu :: Unum
negbigu = minrealu

-- |Quiet NaN.
qNaNu :: Unum
qNaNu = Unum $! BV.ones maxubits `BV.nand` signbigu

-- |Signalling NaN.
sNaNu :: Unum
sNaNu = Unum $! BV.ones maxubits

-- |The open, @negative infinity@ lower bound for an interval.
negopeninfu :: Unum
negopeninfu = if utagsize == 1 then Unum $! BV.bitVec 4 13
              else Unum $! shiftL (BV.bitVec 4 15) (utagsize - 1)

-- |The open, @positive infinity@ upper bound of an interval.
posopeninfu :: Unum
posopeninfu = if utagsize == 1 then Unum $! BV.bitVec 4 5
              else Unum $! shiftL (BV.bitVec 4 7) (utagsize - 1)

-- |The open, @0@ upper bound of an interval.
negopenzerou :: Unum
negopenzerou = Unum $! shiftL (BV.bitVec 4 9) (utagsize - 1)

--------------------------------------------------------------------------------


-- |The open, @0@ lower bound of an interval.
-- 
-- @
-- bits posopenzerou = ubitmask
-- @
posopenzerou :: Unum
posopenzerou = Unum $! ubitmask



u2g :: Unum -> Gbound
u2g (Unum x) = undefined


instance Eq Unum where
   (==) x y = undefined


--instance Ord Unum
--instance Eq Ubound
--instance Ord Ubound

--instance Num Unum
--instance Num Ubound


-- use bv library
