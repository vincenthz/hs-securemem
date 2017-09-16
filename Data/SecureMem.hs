-- |
-- Module      : Data.SecureMem
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : Stable
-- Portability : GHC
--
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE CPP #-}
module Data.SecureMem
    ( SecureMem
    , secureMemGetSize
    , secureMemCopy
    , ToSecureMem(..)
    -- * Allocation and early termination
    , allocateSecureMem
    , createSecureMem
    , unsafeCreateSecureMem
    , finalizeSecureMem
    -- * Pointers manipulation
    , withSecureMemPtr
    , withSecureMemPtrSz
    , withSecureMemCopy
    -- * convertion
    , secureMemFromByteString
    , secureMemFromByteable
    ) where

import           Foreign.ForeignPtr (withForeignPtr)
import           Foreign.Ptr
import           Data.Word (Word8)
#if MIN_VERSION_base(4,9,0)
import           Data.Semigroup
import           Data.Foldable (toList)
#else
import           Data.Monoid
#endif
import           Control.Applicative
import           Data.Byteable

import           Data.ByteString (ByteString)
import           Data.ByteArray  (ScrubbedBytes)
import qualified Data.ByteArray as B
import qualified Data.Memory.PtrMethods as B (memSet)

import qualified Data.ByteString.Internal as BS

#if MIN_VERSION_base(4,4,0)
import System.IO.Unsafe (unsafeDupablePerformIO)
#else
import System.IO.Unsafe (unsafePerformIO)
#endif

pureIO :: IO a -> a
#if MIN_VERSION_base(4,4,0)
pureIO = unsafeDupablePerformIO
#else
pureIO = unsafePerformIO
#endif

-- | SecureMem is a memory chunk which have the properties of:
--
-- * Being scrubbed after its goes out of scope.
--
-- * A Show instance that doesn't actually show any content
--
-- * A Eq instance that is constant time
--
newtype SecureMem = SecureMem ScrubbedBytes

secureMemGetSize :: SecureMem -> Int
secureMemGetSize (SecureMem scrubbedBytes) = B.length scrubbedBytes

secureMemEq :: SecureMem -> SecureMem -> Bool
secureMemEq (SecureMem sm1) (SecureMem sm2) = sm1 == sm2

secureMemAppend :: SecureMem -> SecureMem -> SecureMem
secureMemAppend (SecureMem s1) (SecureMem s2) = SecureMem (s1 `mappend` s2)

secureMemConcat :: [SecureMem] -> SecureMem
secureMemConcat = SecureMem . mconcat . map unSecureMem
  where unSecureMem (SecureMem sb) = sb

secureMemCopy :: SecureMem -> IO SecureMem
secureMemCopy (SecureMem src) =
    SecureMem `fmap` B.copy src (\_ -> return ())

withSecureMemCopy :: SecureMem -> (Ptr Word8 -> IO ()) -> IO SecureMem
withSecureMemCopy (SecureMem src) f = SecureMem `fmap` B.copy src f

instance Show SecureMem where
    show _ = "<secure-mem>"

instance Byteable SecureMem where
    toBytes        = secureMemToByteString
    byteableLength = secureMemGetSize
    withBytePtr    = withSecureMemPtr

instance Eq SecureMem where
    (==) = secureMemEq

#if MIN_VERSION_base(4,9,0)
instance Semigroup SecureMem where
    (<>)    = secureMemAppend
    sconcat = secureMemConcat . toList
#endif

instance Monoid SecureMem where
    mempty  = unsafeCreateSecureMem 0 (\_ -> return ())
#if !(MIN_VERSION_base(4,11,0))
    mappend = secureMemAppend
    mconcat = secureMemConcat
#endif

-- | Types that can be converted to a secure mem object.
class ToSecureMem a where
    toSecureMem :: a -> SecureMem

instance ToSecureMem SecureMem where
    toSecureMem a = a
instance ToSecureMem ByteString where
    toSecureMem bs = secureMemFromByteString bs

-- | Allocate a new SecureMem
--
-- The memory is allocated on the haskell heap, and will be scrubed
-- before being released.
allocateSecureMem :: Int -> IO SecureMem
allocateSecureMem sz = SecureMem <$> B.create sz (\_ -> return ())

-- | Create a new secure mem and running an initializer function
createSecureMem :: Int -> (Ptr Word8 -> IO ()) -> IO SecureMem
createSecureMem sz f = SecureMem `fmap` B.create sz f

-- | Create a new secure mem using inline perform IO to create a pure
-- result.
unsafeCreateSecureMem :: Int -> (Ptr Word8 -> IO ()) -> SecureMem
unsafeCreateSecureMem sz f = pureIO (createSecureMem sz f)
{-# NOINLINE unsafeCreateSecureMem #-}

-- | This is a way to look at the pointer living inside a foreign object. This
-- function takes a function which is applied to that pointer. The resulting IO
-- action is then executed
--
-- this is similary to withForeignPtr for a ForeignPtr
withSecureMemPtr :: SecureMem -> (Ptr Word8 -> IO b) -> IO b
withSecureMemPtr (SecureMem sm) f = B.withByteArray sm f

-- | similar to withSecureMem but also include the size of the pointed memory.
withSecureMemPtrSz :: SecureMem -> (Int -> Ptr Word8 -> IO b) -> IO b
withSecureMemPtrSz (SecureMem sm) f = B.withByteArray sm (f (B.length sm))

-- | Finalize a SecureMem early
finalizeSecureMem :: SecureMem -> IO ()
finalizeSecureMem (SecureMem sb) = B.withByteArray sb $ \p ->
    B.memSet p 0 (B.length sb)

-- | Create a bytestring from a Secure Mem
secureMemToByteString :: SecureMem -> ByteString
secureMemToByteString sm =
    BS.unsafeCreate sz $ \dst ->
    withSecureMemPtr sm $ \src ->
    BS.memcpy dst src (fromIntegral sz)
  where !sz = secureMemGetSize sm

-- | Create a SecureMem from a bytestring
secureMemFromByteString :: ByteString -> SecureMem
secureMemFromByteString b = pureIO $ do
    sm <- allocateSecureMem len
    withSecureMemPtr sm $ \dst -> withBytestringPtr $ \src -> BS.memcpy dst src (fromIntegral len)
    return sm
  where (fp, off, !len) = BS.toForeignPtr b
        withBytestringPtr f = withForeignPtr fp $ \p -> f (p `plusPtr` off)
{-# NOINLINE secureMemFromByteString #-}

-- | Create a SecureMem from any byteable object
secureMemFromByteable :: Byteable b => b -> SecureMem
secureMemFromByteable bs = pureIO $ do
    sm <- allocateSecureMem len
    withSecureMemPtr sm $ \dst -> withBytePtr bs $ \src -> BS.memcpy dst src (fromIntegral len)
    return sm
  where len = byteableLength bs
{-# NOINLINE secureMemFromByteable #-}
