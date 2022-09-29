module Step1 where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Console (log)
import Safe.Coerce (coerce)

{-

The first thing we'll need to do is redefine the type class member from a value to a function.
If we can provide an implementation externally, we can use that implementation if it is
provided via a function argument.

Before:
  someLawlessValue :: a

After:
  buildSomeLawlessValue :: extern -> a

Since this function argument's type needs to be known, we'll push
its type variable into the type class head. Since this argument could change
between usages, we cannot specify a functional dependency between the two type variables.

That gets us this type class

-}

class SomeLawlessTypeClass1 extern a where
  buildSomeLawlessValue1 :: extern -> a

-- String's instance just ignores the argument since we already know what it's implementation should be.
instance SomeLawlessTypeClass1 extern String where
  buildSomeLawlessValue1 _ = "String's implementation"

instance SomeLawlessTypeClass1 extern Int where
  buildSomeLawlessValue1 _ = 0

{-

Below is what we'd like to write in a separate module, but we can't because that would be an orphan instance.

  instance SomeLawlessTypeClass0 extern (Maybe String) where
    buildSomeLawlessValue1 _ = Just "foo"

However, we could define a special type that means, "Use the input to implement my instance!"
It'll just delegate its instance to the actual argument and then wrap that value in the newtype.
Since this is just a newtype, we can easily remove it using `coerce` later.

-}

newtype SpecialType a = SpecialType a

derive instance Newtype (SpecialType a) _

instance SomeLawlessTypeClass1 (Maybe String) (SpecialType (Maybe String)) where
  buildSomeLawlessValue1 input = SpecialType input

main :: Effect Unit
main = do
  log "Step 1:\n"
  log $ (buildSomeLawlessValue1 unit :: String)
  let
    example = buildSomeLawlessValue1 (Just "foo")

    dropSpecialType
      :: SpecialType (Maybe String)
      -> Maybe String
    dropSpecialType = coerce

  log $ show $ dropSpecialType example

{-

While the above code works for our current example, it also only allows us to simulate
an orphan instance for just 1 type: `Maybe String`.

What would we need to do to support multiple types? What if we want to override `Maybe Int`?

-}