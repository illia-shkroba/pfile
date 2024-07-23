{- |
Module:      PFile.Error
Copyright:   (c) 2024 Illia Shkroba
License:     BSD3
Maintainer:  Illia Shkroba <is@pjwstk.edu.pl>
Stability:   unstable
Portability: non-portable (Non-Unix systems are not supported)

Functions for error handling.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module PFile.Error
  ( modifyError
  , tellError
  , liftIOWithError
  , onIOError
  , consumeWriterT
  , fallback
  , untilError
  ) where

import           Control.Monad.Writer (MonadWriter (..), WriterT (..))
import           Protolude

-- | Modify error in @ExceptT e1 m@ with a @(e1 -> e2)@ function and then
-- 'throwError' the new error in @m@.
--
-- @since 0.1.0.0
modifyError :: MonadError e2 m => (e1 -> e2) -> ExceptT e1 m a -> m a
modifyError f = runExceptT >=> either (throwError . f) pure

-- | Modify error in @ExceptT e1 m@ with a @(e1 -> e2)@ function and then
-- 'tell' the new error as a singleton list in @m@.
--
-- @since 0.1.0.0
tellError :: MonadWriter [e2] m => (e1 -> e2) -> ExceptT e1 m a -> m ()
tellError f = runExceptT >=> either (tell . (: []) . f) (const $ pure ())

-- | Catch 'IOException' of 'IO', modify it with a @(IOException -> e)@
-- function and then 'throwError' the new error in @m@ (lifted 'IO').
--
-- @since 0.1.0.0
liftIOWithError :: (MonadError e m, MonadIO m) => IO a -> (IOException -> e) -> m a
liftIOWithError action f = liftIO (try action) >>= either (throwError . f) pure

-- | Catch 'IOException' of the first 'IO', ignore it and call the second 'IO'.
-- The second 'IO' will not be called if the first 'IO' doesn't throw.
--
-- @since 0.1.0.0
onIOError :: IO a -> IO a -> IO a
onIOError action f = action `catch` const @_ @IOException f

-- | Unpack inner @WriterT w m@ of @ExceptT e (WriterT w m)@ and consume its
-- @w@ with @(w -> m ())@ function.
--
-- @since 0.1.0.0
consumeWriterT ::
     Monad m => (w -> m ()) -> ExceptT e (WriterT w m) a -> ExceptT e m a
consumeWriterT f = mapExceptT $ runWriterT >=> \(y, w) -> y <$ f w

-- | When @ExceptT e (WriterT w m)@ throws an error, pass its error @e@ and
-- writer's result @w@ to @(e -> w -> m b)@ function. A result of @(e -> w ->
-- m b)@ is ignored. Always return @w@.
--
-- @since 0.1.0.0
fallback :: Monad m => (e -> w -> m b) -> ExceptT e (WriterT w m) a -> m w
fallback f action = do
  (maybeCause, result) <- untilError action
  maybeCause & maybe (pure result) (\cause -> result <$ f cause result)

-- | Unpack @ExceptT e (WriterT w m)@. A result @a@ of @ExceptT e (WriterT w m)
-- a@ is ignored.
--
-- @since 0.1.0.0
untilError :: Functor m => ExceptT e (WriterT w m) a -> m (Maybe e, w)
untilError action = action & runWriterT . runExceptT <&> first leftToMaybe
