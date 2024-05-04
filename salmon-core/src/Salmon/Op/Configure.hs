module Salmon.Op.Configure where

newtype Configure m seed a
    = Configure {gen :: seed -> m a}
