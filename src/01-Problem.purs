module Problem where

import Data.Maybe (Maybe(..))

{-
PureScript guarantees global coherence. One of the impliciations of this guarantee
is disallowing Orphan Instances (e.g. a type class instance defined in Module 3
where the type class is TC and the type is X
where TC is defined in Module 1 and
where X is defined in Module 2).

In my understanding, languages that enable orphan instances do so to solve two problems:
- allowing a type to have multiple instances of the same class
    (the instance used depends on what is "in scope")
- allowing the end developer to define an instance for a type class they don't define
    for a library's type they don't define.

Anytime one of these two problems occurs, the only way to solve it
is to wrap the type in a newtype and write an instance for that newtype.

-}

class SomeLawlessTypeClass0 a where
  someLawlessValue0 :: a

instance notOrphanInstance :: SomeLawlessTypeClass0 String where
  someLawlessValue0 = "String's implementation"

instance alsoNotOrphanInstance :: SomeLawlessTypeClass0 Int where
  someLawlessValue0 = 0

-- If we wanted to define a `SomeLawlessTypeClass0` instance for `Maybe String` outside of this module,
-- and outside of the `Data.Maybe` module, it would be an orphan instance.
-- So, we'd have to newtype it as shown below:

newtype MaybeString = MaybeString (Maybe String)

instance workaround :: SomeLawlessTypeClass0 MaybeString where
  someLawlessValue0 = MaybeString (Just "foo")

-- Personally, I think disallowing Orphan Instances is a good idea.
-- However, the rest of this repo's source code explores ways
-- for working around this limitation.
