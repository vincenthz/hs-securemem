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
    ) where

import Foreign.C.Types
import Foreign.ForeignPtr (withForeignPtr, finalizeForeignPtr)
import Foreign.Ptr
#if MIN_VERSION_base(4,6,0)
import GHC.Types (Int(..))
import GHC.ForeignPtr (ForeignPtr(..), ForeignPtrContents(..), mallocForeignPtrBytes, addForeignPtrConcFinalizer)
import GHC.Prim (sizeofMutableByteArray#)
import GHC.Ptr (Ptr(..))
#else
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes)
#endif
import Data.Word (Word8)
import Data.Monoid
import Control.Applicative
import Control.Monad (foldM, void)
import Data.ByteString (ByteString)
import Data.Byteable
import qualified Data.ByteString.Internal as B

-- | SecureMem is a memory chunk which have the properties of:
--
-- * Being scrubbed after its goes out of scope.
--
-- * A Show instance that doesn't actually show any content
--
-- * A Eq instance that is constant time
--
#if MIN_VERSION_base(4,6,0)
newtype SecureMem = SecureMem { getForeignPtr :: ForeignPtr Word8 }
#else
data SecureMem = SecureMem { getForeignPtr :: ForeignPtr Word8, secureMemGetSize :: Int }
#endif

#if MIN_VERSION_base(4,6,0)
-- | Return the size of the memory allocated by this secure mem.
secureMemGetSize :: SecureMem -> Int
secureMemGetSize (SecureMem (ForeignPtr _ fpc)) =
    case fpc of
        MallocPtr mba _ -> I# (sizeofMutableByteArray# mba)
        _               -> error "cannot happen"
#endif

secureMemEq :: SecureMem -> SecureMem -> Bool
secureMemEq sm1 sm2 = sz1 == sz2 && B.inlinePerformIO meq
  where meq = withSecureMemPtr sm1 $ \ptr1 ->
              withSecureMemPtr sm2 $ \ptr2 ->
              feq ptr1 ptr2
        !sz1 = secureMemGetSize sm1
        !sz2 = secureMemGetSize sm2
        feq | sz1 == 8   = compareEq8
            | sz1 == 16  = compareEq16
            | sz1 == 24  = compareEq24
            | sz1 == 32  = compareEq32
            | sz1 == 64  = compareEq64
            | sz1 == 128 = compareEq128
            | sz1 == 256 = compareEq256
            | otherwise  = compareEq (fromIntegral sz1)

secureMemAppend :: SecureMem -> SecureMem -> SecureMem
secureMemAppend s1 s2 =
    unsafeCreateSecureMem (sz1+sz2) $ \dst -> do
        withSecureMemPtr s1 $ \sp1 -> B.memcpy dst sp1 (fromIntegral sz1)
        withSecureMemPtr s2 $ \sp2 -> B.memcpy (dst `plusPtr` sz1) sp2 (fromIntegral sz2)
  where !sz1 = secureMemGetSize s1
        !sz2 = secureMemGetSize s2

secureMemConcat :: [SecureMem] -> SecureMem
secureMemConcat l = unsafeCreateSecureMem total $ \dst -> void $ foldM copy dst l
  where total = sum $ map secureMemGetSize l
        copy dst s = withSecureMemPtr s $ \sp -> do
                    B.memcpy dst sp (fromIntegral sz)
                    return (dst `plusPtr` sz)
          where sz = secureMemGetSize s

secureMemCopy :: SecureMem -> IO SecureMem
secureMemCopy src =
    createSecureMem sz $ \dst ->
    withSecureMemPtr src $ \s -> B.memcpy dst s (fromIntegral sz)
  where sz = secureMemGetSize src

withSecureMemCopy :: SecureMem -> (Ptr Word8 -> IO ()) -> IO SecureMem
withSecureMemCopy sm f = do
    sm2 <- secureMemCopy sm
    withSecureMemPtr sm2 f
    return sm2

instance Show SecureMem where
    show _ = "<secure-mem>"

instance Byteable SecureMem where
    toBytes        = secureMemToByteString
    byteableLength = secureMemGetSize
    withBytePtr    = withSecureMemPtr

instance Eq SecureMem where
    (==) = secureMemEq

instance Monoid SecureMem where
    mempty  = unsafeCreateSecureMem 0 (\_ -> return ())
    mappend = secureMemAppend
    mconcat = secureMemConcat

type Finalizer = Ptr Word8 -> IO ()
type FinalizerWithSize = CInt -> Ptr Word8 -> IO ()

-- | Types that can be converted to a secure mem object.
class ToSecureMem a where
    toSecureMem :: a -> SecureMem

instance ToSecureMem SecureMem where
    toSecureMem a = a
instance ToSecureMem ByteString where
    toSecureMem bs = secureMemFromByteString bs

-- | Generic eq function
foreign import ccall "compare_eq"    compareEq    :: CInt -> Ptr Word8 -> Ptr Word8 -> IO Bool

-- | Specialized eq functions which are much faster
foreign import ccall "compare_eq8"   compareEq8   :: Ptr Word8 -> Ptr Word8 -> IO Bool
foreign import ccall "compare_eq16"  compareEq16  :: Ptr Word8 -> Ptr Word8 -> IO Bool
foreign import ccall "compare_eq24"  compareEq24  :: Ptr Word8 -> Ptr Word8 -> IO Bool
foreign import ccall "compare_eq32"  compareEq32  :: Ptr Word8 -> Ptr Word8 -> IO Bool
foreign import ccall "compare_eq64"  compareEq64  :: Ptr Word8 -> Ptr Word8 -> IO Bool
foreign import ccall "compare_eq128" compareEq128 :: Ptr Word8 -> Ptr Word8 -> IO Bool
foreign import ccall "compare_eq256" compareEq256 :: Ptr Word8 -> Ptr Word8 -> IO Bool

#if MIN_VERSION_base(4,6,0)
-- | Scruber finalizers
foreign import ccall "finalizer_scrub8" finalizerScrub8     :: Finalizer
foreign import ccall "finalizer_scrub16" finalizerScrub16   :: Finalizer
foreign import ccall "finalizer_scrub24" finalizerScrub24   :: Finalizer
foreign import ccall "finalizer_scrub32" finalizerScrub32   :: Finalizer
foreign import ccall "finalizer_scrub64" finalizerScrub64   :: Finalizer
foreign import ccall "finalizer_scrub128" finalizerScrub128 :: Finalizer
foreign import ccall "finalizer_scrub256" finalizerScrub256 :: Finalizer
foreign import ccall "finalizer_scrubvar" finalizerScrubVar :: FinalizerWithSize

szToScruber :: Int -> Ptr Word8 -> IO ()
szToScruber 0   = \_ -> return ()
szToScruber 8   = finalizerScrub8
szToScruber 16  = finalizerScrub16
szToScruber 24  = finalizerScrub24
szToScruber 32  = finalizerScrub32
szToScruber 64  = finalizerScrub64
szToScruber 128 = finalizerScrub128
szToScruber 256 = finalizerScrub256
szToScruber n   = finalizerScrubVar (fromIntegral n)
#endif

-- | Allocate a foreign ptr which will be scrubed before memory free.
-- the memory is allocated on the haskell heap
allocateScrubedForeignPtr :: Int -> IO (ForeignPtr a)
allocateScrubedForeignPtr sz = do
#if MIN_VERSION_base(4,6,0)
    fptr@(ForeignPtr addr _) <- mallocForeignPtrBytes sz
    addForeignPtrConcFinalizer fptr (scruber (Ptr addr))
    return fptr
  where !scruber = szToScruber sz
#else
    mallocForeignPtrBytes sz
#endif

-- | Allocate a new SecureMem
--
-- The memory is allocated on the haskell heap, and will be scrubed
-- before being released.
allocateSecureMem :: Int -> IO SecureMem
#if MIN_VERSION_base(4,6,0)
allocateSecureMem sz = SecureMem <$> allocateScrubedForeignPtr sz
#else
allocateSecureMem sz = SecureMem <$> allocateScrubedForeignPtr sz <*> pure sz
#endif

-- | Create a new secure mem and running an initializer function
createSecureMem :: Int -> (Ptr Word8 -> IO ()) -> IO SecureMem
createSecureMem sz f = do
    sm <- allocateSecureMem sz
    withSecureMemPtr sm f
    return sm

-- | Create a new secure mem using inline perform IO to create a pure
-- result.
unsafeCreateSecureMem :: Int -> (Ptr Word8 -> IO ()) -> SecureMem
unsafeCreateSecureMem sz f = B.inlinePerformIO (createSecureMem sz f)

-- | This is a way to look at the pointer living inside a foreign object. This
-- function takes a function which is applied to that pointer. The resulting IO
-- action is then executed
--
-- this is similary to withForeignPtr for a ForeignPtr
withSecureMemPtr :: SecureMem -> (Ptr Word8 -> IO b) -> IO b
withSecureMemPtr sm f = withForeignPtr (getForeignPtr sm) f

-- | similar to withSecureMem but also include the size of the pointed memory.
withSecureMemPtrSz :: SecureMem -> (Int -> Ptr Word8 -> IO b) -> IO b
withSecureMemPtrSz sm f = withForeignPtr (getForeignPtr sm) (f (secureMemGetSize sm))

-- | Finalize a SecureMem early
finalizeSecureMem :: SecureMem -> IO ()
finalizeSecureMem sm = finalizeForeignPtr $ getForeignPtr sm

-- | Create a bytestring from a Secure Mem
secureMemToByteString :: SecureMem -> ByteString
secureMemToByteString sm =
    B.unsafeCreate sz $ \dst ->
    withSecureMemPtr sm $ \src ->
    B.memcpy dst src (fromIntegral sz)
  where !sz = secureMemGetSize sm

-- | Create a SecureMem from a bytestring
secureMemFromByteString :: ByteString -> SecureMem
secureMemFromByteString b = B.inlinePerformIO $ do
    sm <- allocateSecureMem len 
    withSecureMemPtr sm $ \dst -> withBytestringPtr $ \src -> B.memcpy dst src (fromIntegral len)
    return sm
  where (fp, off, !len) = B.toForeignPtr b
        withBytestringPtr f = withForeignPtr fp $ \p -> f (p `plusPtr` off)
