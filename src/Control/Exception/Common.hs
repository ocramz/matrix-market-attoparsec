module Control.Exception.Common where

import Control.Exception
import Control.Monad.Catch (MonadThrow (..))
import Data.Typeable -- (TypeRep, Typeable, typeRep)


data ImportError = FileParseError String String deriving (Eq, Typeable)
instance Show ImportError where
  show (FileParseError s s2) = unwords [s, ": File parse error:", s2]
instance Exception ImportError
