{-# LANGUAGE
   BangPatterns
   #-}

module Numeric.Unum (
   -- *Types
   GBound(..),
   Unum(..),
   UBound(..),
   Bitmask(..),
   -- *Bitmasks
   -- |Bitmasks for low-level bit-fiddling around Unums.
   ubitmask,
   esizemask,
   fsizemask,
   efsizemask,
   utagmask,
   signmask,
   ulpmask,
   -- *Constants
   -- |Various constants. These are dependent on the environment.
   smallSubnormal,
   posInf,
   negInf,
   posOpenInf,
   negOpenInf,
   maxReal,
   minReal,
   negBig,
   qNaN,
   sNaN,
   posOpenZero,
   negOpenZero,
   ) where

import Data.Bits ((.|.),
                  shiftL)
import qualified Data.BitVector as BV

esizesize :: Int
esizesize = 3

fsizesize :: Int
fsizesize = 2

-- |A general bound.
--
--  This is a helper data structure that stores various
--  low-level bits of information about unums.
--
--  The fields are:
--
-- * Is the number a NaN?
-- * Bits of the fraction.
-- * Is the fraction negative?
-- * Bits of the exponent.
-- * Is the exponent negative?
-- * Is the interval endpoint open?
-- * Is the number infinite?
data GBound = GBound {-# UNPACK #-} !Bool
                     {-# UNPACK #-} !BV.BitVector
                     {-# UNPACK #-} !Bool
                     {-# UNPACK #-} !BV.BitVector
                     {-# UNPACK #-} !Bool
                     {-# UNPACK #-} !Bool
                     {-# UNPACK #-} !Bool

-- |A Unum, stored as a variable-size bit array.
--
--  A Unum is very much like an IEEE float, with the difference that
--  iit stores three pieces of metadata after the LSB:
--
--  * A __ubit__ that's 0 iff the unum is exact;
--  * the exponent size hat stores how many bits the exponent has;
--  * the fraction size that stores how many bits the fraction has.
--
--  Thus, going from left to right, the format is as follows:
--
-- * __1 bit__: the sign.
-- * __esize bits__: the exponent.
-- * __fsize bits__: the fraction.
-- * __1 bit__: the ubit.
-- * __esizesize bits__: the exponent size @esize@.
-- * __fsizesize bits__: the fraction size @fsize@.
--
-- esizesize and fsizesize are not stored in the unum and depend
-- on the environment.
newtype Unum = Unum{fromUnum :: BV.BitVector}

-- |A bitmask over a unum.
type Bitmask = BV.BitVector

-- |A UBound.
--  TODO
data UBound = UBound

-- Basic machinery of unums; bit-fiddling.
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
ubitmask = shiftL (BV.bitVec utagsize 1) (utagsize - 1)

-- |A bitmask to only get the esize-part of a unum
esizemask :: Bitmask
esizemask = shiftL (BV.bitVec (fsizesize + esizesize) (esizemax - 1)) fsizesize

-- |A bitmask to only get the fsize-part from a unum.
fsizemask :: Bitmask
fsizemask = BV.bitVec fsizesize (fsizemax - 1)

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

-- |A bitmask for the sign of a unum, provided the unum has 'maxubits' bits.
signmask :: Bitmask
signmask = shiftL (BV.bitVec maxubits 1) (maxubits - 1)

-- |The least significant bit of a unum's fraction.
ulpmask :: Bitmask
ulpmask = shiftL (BV.bitVec (utagsize + 1) 1) utagsize


-- Constants
--------------------------------------------------------------------------------

-- |The smallest positive number that can be represented.
smallSubnormal :: Unum
smallSubnormal =
   Unum $! BV.bitVec maxubits 0
           .|. efsizemask
           .|. shiftL ubitmask 1

-- |Positive infinity.
posInf :: Unum
posInf = Unum $! BV.ones maxubits `BV.nand` (signmask .|. ubitmask)

-- |Negative infinity.
negInf :: Unum
negInf = Unum $! BV.ones maxubits `BV.nand` ubitmask

-- |The open, @positive infinity@ upper bound of an interval.
posOpenInf :: Unum
posOpenInf = if utagsize == 1 then Unum $! BV.bitVec 4 5
              else Unum $! shiftL (BV.bitVec 4 7) (utagsize - 1)
              
-- |The open, @negative infinity@ lower bound for an interval.
negOpenInf :: Unum
negOpenInf = if utagsize == 1 then Unum $! BV.bitVec 4 13
              else Unum $! shiftL (BV.bitVec 4 15) (utagsize - 1)

-- |The largest positive real number that can be represented.
maxReal :: Unum
maxReal = Unum $! BV.ones maxubits `BV.nand` (signmask .|. ulpmask .|. ubitmask)

-- |The largest negative real number that can be represented.
minReal :: Unum
minReal = Unum $! BV.ones maxubits `BV.nand` (ulpmask .|. ubitmask)

-- |Alias for 'minreal'. This was included because it appeared in the prototype.
negBig :: Unum
negBig = minReal

-- |Quiet NaN.
qNaN :: Unum
qNaN = Unum $! BV.ones maxubits `BV.nand` signmask

-- |Signalling NaN.
sNaN :: Unum
sNaN = Unum $! BV.ones maxubits

-- |The open, @0@ lower bound of an interval.
-- 
-- @
-- bits posOpenZero = ubitmask
-- @
posOpenZero :: Unum
posOpenZero = Unum $! ubitmask

-- |The open, @0@ upper bound of an interval.
negOpenZero :: Unum
negOpenZero = Unum $! shiftL (BV.bitVec 4 9) (utagsize - 1)

--------------------------------------------------------------------------------

-- |Prints the bits of a 'BV.BitVector' as a sequence of 1s and 0s,
--  starting with the MSB.
showBits :: BV.BitVector -> String
showBits = map to10 . BV.toBits
   where
      to10 True = '1'
      to10 False = '0'

-- |Extracts various pieces of information from a unum.
--  These low-level data are then used for calculations.
u2g :: Unum -> GBound
u2g !(Unum !x) = 
   if fromUnum sNaN == (signmask .|. x)
   then GBound True
               BV.nil
               False
               BV.nil
               False
               True
               False

   else undefined
               


instance Eq Unum where
   (==) x y = undefined
      where
         gx = u2g x
         gy = u2g y

{- data Gbound = Gbound {-# UNPACK #-} !Bool -- ^Is the number a NaN?
                     {-# UNPACK #-} !BV.BitVector -- ^Bits of the fraction.
                     {-# UNPACK #-} !Bool -- ^Is the fraction negative?
                     {-# UNPACK #-} !BV.BitVector -- ^Bits of the exponent.
                     {-# UNPACK #-} !Bool -- ^Is the exponent negative?
                     {-# UNPACK #-} !Bool -- ^Is the interval endpoint open?
                     {-# UNPACK #-} !Bool -- ^Is the number infinite? -}


--instance Ord Unum
--instance Eq Ubound
--instance Ord Ubound

--instance Num Unum
--instance Num Ubound


-- use bv library
