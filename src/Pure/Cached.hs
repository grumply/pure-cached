{-# language BangPatterns, LambdaCase, RecordWildCards #-}
module Pure.Cached (Cached,cache,forkCache,cached) where

import Pure.Data.Time

import Control.Concurrent
import Control.Monad
import System.Mem.Weak

type Duration = Time

data Cached a = Cached
  { construct :: IO (Time,a)
  , value :: MVar (Time,a)
  , expensive :: Bool
  }

cache_ :: Bool -> Duration -> IO a -> IO (Cached a)
cache_ expensive d ma = do
  let 
    construct = do
      !next <- (+ d) <$> time
      !a    <- ma
      pure (next,a)

  value <- newMVar =<< construct
  _value <- mkWeakMVar value (pure ())

  let
    monitor = do
      delay d
      mvalue <- deRefWeak _value
      case mvalue of
        Nothing -> pure ()
        Just v -> do
          !tma <- construct
          swapMVar v tma

          -- Does looping in this way actually discard 
          -- our reference to v, as desired?
          monitor

  when expensive $ void $ 
    forkIO monitor

  pure Cached {..}

-- | Construct a cached value. Use this method for
-- values that are cheap to re-compute.
-- 
-- Often used with `unsafePerformIO` to create a global 
-- cache.
--
-- > {-# NOINLINE c #-}
-- > c :: Cached value
-- > c = unsafePerform $ cache (Hours 1 0) valueProducer
cache :: Duration -> IO a -> IO (Cached a)
cache = cache_ False

-- | Fork a thread to manage a cached value. Use this 
-- method for values that are expensive to re-compute.
-- 
-- Often used with `unsafePerformIO` to create a global 
-- cache.
--
-- > {-# NOINLINE c #-}
-- > c :: Cached value
-- > c = unsafePerform $ forkCache (Hours 1 0) valueProducer
forkCache :: Duration -> IO a -> IO (Cached a)
forkCache = cache_ True

-- | Read a cached value, re-constructing it if the 
-- cache has expired. If the value is expensive to
-- re-compute and the cache has expired, the expired 
-- value is returned with the expectation that the 
-- a monitoring thread will handle re-computation.
cached :: Cached a -> IO a
cached Cached {..} = time >>= \now -> modifyMVar value $ \case
  v | now > fst v
    , not expensive -> construct >>= \v -> pure (v,snd v)
    | otherwise     -> pure (v,snd v)
