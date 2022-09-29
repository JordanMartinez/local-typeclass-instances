module Step3 where

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

Our previous example's flaw was that only types with kind `Type` are supported. This file
shows how to support higher-kinded types.

-}

class SomeLawlessTypeClass3 externRecord a where
  buildSomeLawlessValue3 :: externRecord -> a

instance SomeLawlessTypeClass3 extern String where
  buildSomeLawlessValue3 _ = "String's implementation"

instance SomeLawlessTypeClass3 extern Int where
  buildSomeLawlessValue3 _ = 0

instance
  ( SomeLawlessTypeClass3 extern a
  , SomeLawlessTypeClass3 extern b
  ) =>
  SomeLawlessTypeClass3 extern (Tuple a b) where
  buildSomeLawlessValue3 input = do
    let
      aValue = buildSomeLawlessValue3 input
      bValue = buildSomeLawlessValue3 input
    Tuple aValue bValue

{-
Generalizing the previous example's idea, we're trying to define a local override
that looks like this. `f` could be `Maybe` or anything else with a similar kind:

    localOveride :: forall f a. SomeLawlessTypeClass3 extern a => extern -> f a
    localOveride externRec =
      let
        theA :: a
        theA = buildSomeLawlessValue3 externRec
      -- doSomethingWith theA

If we take some inspiration from the `Tuple` instance...

    instance
      ( SomeLawlessTypeClass3 extern a
      , SomeLawlessTypeClass3 extern b
      ) =>
      SomeLawlessTypeClass3 extern (Tuple a b) where
      buildSomeLawlessValue3 input = do
        let
          aValue = buildSomeLawlessValue3 input
          bValue = buildSomeLawlessValue3 input
        Tuple aValue bValue

... we'll see that the type class instance for `Tuple` requires a type class instance for each
of its members. For types with kind `Type`, no such requirements were needed, so this
issue hadn't come up before. But for higher-kinded types, now we need to take this into consideration.

At this point, we need to split `SpecialNewtype2` into multiple newtypes, one for each kind.
To make it easier to see what kind of type we're working with, we'll call them `KindX` where `X`
represents the number of `Type` arguments it takes. 

Then, when implementing their instances, we'll use the kind number to indicate how many type parameters
the value takes and thus how many additional `SomeLawlessTypeClass3` are needed for each of those parameters.
-}

-- Same as `SpecialType2`, just named differently now
newtype Kind0 :: Symbol -> Type -> Type
newtype Kind0 labelInRecord a = Kind0 a

derive instance Newtype (Kind0 labelInRecord a) _

-- Since this is `Kind0`, we don't need the additional `SomeLawlessTypeClass3` constraint.
instance
  ( Row.Cons labelInRecord a tail rows
  , IsSymbol labelInRecord
  ) =>
  SomeLawlessTypeClass3 { | rows } (Kind0 labelInRecord a) where
  buildSomeLawlessValue3 inputRecord = do
    let
      someLawlessValue3 :: a
      someLawlessValue3 = Record.get (Proxy :: Proxy labelInRecord) inputRecord

    Kind0 someLawlessValue3

-- The `fa` here is actually `f a`
newtype Kind1 :: Symbol -> Type -> Type
newtype Kind1 labelInRecord fa = Kind1 fa

derive instance Newtype (Kind1 labelInRecord fa) _

instance
  -- Since this is `Kind1`, we ask the compiler to provide the `a` implementation.
  ( SomeLawlessTypeClass3 { | rows } a
  -- Notice how we're expecting the record's value to be a function now.
  , Row.Cons labelInRecord (a -> f a) tail rows
  , IsSymbol labelInRecord
  ) =>
  -- Notice how `Kind1` is wrapping an `f a` value.
  SomeLawlessTypeClass3 { | rows } (Kind1 labelInRecord (f a)) where
  buildSomeLawlessValue3 inputRecord = do
    let
      theA :: a
      theA = buildSomeLawlessValue3 inputRecord

      localBuildSomeLawlessValue3 :: a -> f a
      localBuildSomeLawlessValue3 = Record.get (Proxy :: Proxy labelInRecord) inputRecord

      someLawlessValue3 :: f a
      someLawlessValue3 = localBuildSomeLawlessValue3 theA

    Kind1 someLawlessValue3

main :: Effect Unit
main = do
  log "\n\nStep 3:\n"
  demonstrateNoOverrides
  demonstrateKind0
  demonstrateKind1

demonstrateNoOverrides :: Effect Unit
demonstrateNoOverrides = do
  let
    noOverrides :: String
    noOverrides = buildSomeLawlessValue3 unit
  log noOverrides

demonstrateKind0 :: Effect Unit
demonstrateKind0 = do
  let
    kind0Override = buildSomeLawlessValue3 { str: "foo" }

    dropSpecialType
      :: Tuple String (Kind0 "str" String)
      -> Tuple String String
    dropSpecialType = coerce

  log $ show $ dropSpecialType kind0Override

demonstrateKind1 :: Effect Unit
demonstrateKind1 = do
  let
    kind1Override = buildSomeLawlessValue3
      { useJust: \theA -> Just theA
      , useNothing: \_ -> Nothing
      }

    dropSpecialType
      :: Tuple (Kind1 "useJust" (Maybe String)) (Kind1 "useNothing" (Maybe String))
      -> Tuple (Maybe String) (Maybe String)
    dropSpecialType = coerce

  log $ show $ dropSpecialType kind1Override

-- It works! Except I'm tricking you right now. It only works because each usage of `Maybe` is wrapping
-- the same type: `String`. Thus, the `useJust` and `useNothing` functions do not have a kind signature of
-- 
--    { useJust :: forall a. a -> Maybe a
--    , useNothing :: forall a. a -> Maybe a
--    }
--
-- Rather, the `a` is always `String`
--
--    { useJust :: String -> Maybe String
--    , useNothing :: String -> Maybe String
--    }
--
-- This becomes clearer when we try to make one of the `Maybe`'s wrap an `Int`.
-- The commented out code below does not compile but instead produces this error:
{-

  Could not match type

    Int

  with type

    String


while trying to match type Function Int
  with type Function String
while solving type class constraint

  Prim.Row.Cons "maybe"
                (Int -> Maybe Int)
                t0
                ( maybe :: String -> Maybe String
                )

while inferring the type of buildSomeLawlessValue3 { maybe: \theA ->
                                                              ...
                                                   }
in value declaration demonstrateMultipleKinds

where t0 is an unknown type
-}
-- demonstrateMultipleKinds :: Effect Unit
-- demonstrateMultipleKinds = do
--   let
--     differendWrappedTypes = buildSomeLawlessValue3 { maybe: \theA -> Just theA }

--     dropSpecialType
--       :: Tuple (Kind1 "maybe" (Maybe String)) (Kind1 "maybe" (Maybe Int))
--       -> Tuple (Maybe String) (Maybe Int)
--     dropSpecialType = coerce

--   log $ show $ dropSpecialType differendWrappedTypes

-- Ideally, we would want higher-kinded types' overrides to work for all `a` types. Besides
-- allowing us to use `Maybe` to wrap different types, we could also stack multiple local overrides
-- on one another. For example, having our code work on a type like this:
--
--    Kind1 "maybe" (Maybe (Kind0 "customString" String))
