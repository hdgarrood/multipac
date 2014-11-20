module MVar where

import Data.Maybe
import Control.Monad.Eff
import Control.Monad.Eff.Ref

newtype MVar a = MVar (RefVal (Maybe a))

type EffM a = forall e. Eff (ref :: Ref | e) a

newEmptyMVar :: forall e a. EffM (MVar a)
newEmptyMVar = MVar <$> newRef Nothing

newMVar :: forall e a. a -> EffM (MVar a)
newMVar x = MVar <$> newRef (Just x)
