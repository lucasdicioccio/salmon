module Salmon.Op.Configure where

{- | Configure is a newtype wrapper around functions that effectfully generate
 a value from an input. The type is so general and that we legitimately question why we need a dedicated type.
 Well, in Salmon we want to capture the specific effect turning a
 configuration seed into an operation graph.  While operations are
 IO-heavy in nature, we may not want the configuration step to be IO-bearing.
 And, when both the configuration and the execution steps are in IO, we want them
 to be treated as two separate and hermetic steps.
-}
newtype Configure m seed a
    = Configure {gen :: seed -> m a}
