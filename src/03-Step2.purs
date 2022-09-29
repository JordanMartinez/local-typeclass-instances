module Step2 where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Prim.Row as Row
import Record as Record
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))

{-

Our previous example's flaw was that only one type can have its implementation provided
via the input argument. 

What we'd actually like to do is support multiple types. Thus, rather than passing in
a single value, we need to pass in multiple values. That can be done as either
an `Array` or a `Record`. Since PureScript `Array`s must be homogeneous and the `a`
type below can change between implementations, we should use a `Record`.

-}

class SomeLawlessTypeClass2 externRecord a where
  buildSomeLawlessValue2 :: externRecord -> a

instance SomeLawlessTypeClass2 extern String where
  buildSomeLawlessValue2 _ = "String's implementation"

instance SomeLawlessTypeClass2 extern Int where
  buildSomeLawlessValue2 _ = 0

-- Tuple's instance is provided so we can show multiple usages below.
instance
  ( SomeLawlessTypeClass2 extern a
  , SomeLawlessTypeClass2 extern b
  ) =>
  SomeLawlessTypeClass2 extern (Tuple a b) where
  buildSomeLawlessValue2 input = do
    let
      aValue = buildSomeLawlessValue2 input
      bValue = buildSomeLawlessValue2 input
    Tuple aValue bValue

-- So, how does that affect `SpecialType`? If we're using a `Record` now,
-- we need to know which label within the record stores the implementation for some type.
--
-- Thus, we'll update our `SpecialType` to use another type parameter to track that information.

newtype SpecialType2 :: Symbol -> Type -> Type
newtype SpecialType2 labelInRecord a = SpecialType2 a

derive instance Newtype (SpecialType2 labelInRecord a) _

-- Now we can write an instance for `SpecialType2` that says:
-- 1. Get the corresponding label from the input record
-- 2. Use that value to implement my instance
instance
  ( Row.Cons labelInRecord a tail rows
  , IsSymbol labelInRecord
  ) =>
  SomeLawlessTypeClass2 { | rows } (SpecialType2 labelInRecord a) where
  buildSomeLawlessValue2 inputRecord = do
    let
      someLawlessValue2 :: a
      someLawlessValue2 = Record.get (Proxy :: Proxy labelInRecord) inputRecord

    SpecialType2 someLawlessValue2

main :: Effect Unit
main = do
  log "\n\nStep 2:\n"
  demonstrateNoOverrides
  demonstrateOneOverride
  demonstrateTwoOverrides
  demonstrateManyCombos

demonstrateNoOverrides :: Effect Unit
demonstrateNoOverrides = do
  let
    noOverrides :: String
    noOverrides = buildSomeLawlessValue2 unit
  log noOverrides

demonstrateOneOverride :: Effect Unit
demonstrateOneOverride = do
  let
    oneOverride = buildSomeLawlessValue2 { maybeString: Just "foo" }

    dropSpecialType
      :: SpecialType2 "maybeString" (Maybe String)
      -> Maybe String
    dropSpecialType = coerce

  log $ show $ dropSpecialType oneOverride

demonstrateTwoOverrides :: Effect Unit
demonstrateTwoOverrides = do
  let
    twoOverrides = buildSomeLawlessValue2
      { maybeStr1: Just "foo"
      , maybeStr2: Just "bar"
      }

    dropSpecialType
      :: Tuple (SpecialType2 "maybeStr1" (Maybe String)) (SpecialType2 "maybeStr2" (Maybe String))
      -> Tuple (Maybe String) (Maybe String)
    dropSpecialType = coerce

  log $ show $ dropSpecialType twoOverrides

-- Rather than using nested Tuples, we define this type so that it's easier to see the code.
-- The instances for `Show` and `SomeLawlessTypeClass2` are at the bottom of this file.
data Tuple5 a b c d e = Tuple5 a b c d e

demonstrateManyCombos :: Effect Unit
demonstrateManyCombos = do
  let
    manyCombos
      :: Tuple5
           (Tuple String String)
           (SpecialType2 "str1" String)
           (SpecialType2 "maybeStr1" (Maybe String))
           (Tuple String (SpecialType2 "int" Int))
           (SpecialType2 "maybeStr2" (Maybe String))
    manyCombos = buildSomeLawlessValue2
      { str1: "Got string 1"
      , str2: "Got string 2"
      , int: 4
      , maybeStr1: Just "foo"
      , maybeStr2: Just "bar"
      }

    dropSpecialType
      :: Tuple5
           (Tuple String String)
           (SpecialType2 "str1" String)
           (SpecialType2 "maybeStr1" (Maybe String))
           (Tuple String (SpecialType2 "int" Int))
           (SpecialType2 "maybeStr2" (Maybe String))
      -> Tuple5
           (Tuple String String)
           String
           (Maybe String)
           (Tuple String Int)
           (Maybe String)
    dropSpecialType = coerce
  log $ show $ dropSpecialType manyCombos

{-

While the above works (hurray!), there's still another limitation we need to lift.
Our local overrides only work on types with kind `Type`. For example, notice how every
usage of `Maybe` above requires us to know the type that `Maybe` wraps?

`Tuple` has kind `Type -> Type` and `Tuple5` has kind `Type -> Type -> Type -> Type -> Type -> Type -> Type`.
Each will defer to the inner type's `SomeLawlessTypeClass2` instance and then wrap it in the constructor.

Why can't we define an override for `Maybe` that works like the instances for `Tuple` and `Tuple5`?
In other words, can we support higher-kinded types by defining a local override that works like this:

  localOveride :: forall a. SomeLawlessTypeClass2 extern a => extern -> Maybe a
  localOveride externRec =
    let
      theA :: a
      theA = buildSomeLawlessValue2 externRec
    Just theA

-}

instance (Show a, Show b, Show c, Show d, Show e) => Show (Tuple5 a b c d e) where
  show (Tuple5 a b c d e) =
    "Tuple5(" <> show a <> " " <> show b <> " " <> show c <> " " <> show d <> " " <> show e <> ")"

instance
  ( SomeLawlessTypeClass2 extern a
  , SomeLawlessTypeClass2 extern b
  , SomeLawlessTypeClass2 extern c
  , SomeLawlessTypeClass2 extern d
  , SomeLawlessTypeClass2 extern e
  ) =>
  SomeLawlessTypeClass2 extern (Tuple5 a b c d e) where
  buildSomeLawlessValue2 input = do
    Tuple5
      (buildSomeLawlessValue2 input)
      (buildSomeLawlessValue2 input)
      (buildSomeLawlessValue2 input)
      (buildSomeLawlessValue2 input)
      (buildSomeLawlessValue2 input)