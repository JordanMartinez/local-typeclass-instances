module Step4 where

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
import Unsafe.Coerce (unsafeCoerce)

{-

In our previous example, we could not use two local overrides in the same code.
This file shows how to workaround that problem by using existential types.

-}

class SomeLawlessTypeClass4 externRecord a where
  buildSomeLawlessValue4 :: externRecord -> a

instance SomeLawlessTypeClass4 extern String where
  buildSomeLawlessValue4 _ = "String's implementation"

instance
  ( SomeLawlessTypeClass4 extern a
  , SomeLawlessTypeClass4 extern b
  ) =>
  SomeLawlessTypeClass4 extern (Tuple a b) where
  buildSomeLawlessValue4 input = do
    let
      aValue = buildSomeLawlessValue4 input
      bValue = buildSomeLawlessValue4 input
    Tuple aValue bValue

{-

You might think the solution to our previous example is to just add the `forall a.` part to the record.
In other words, changing the code via this diff:

```diff
    let
-     differendWrappedTypes = buildSomeLawlessValue3 { maybe: \theA -> Just theA }
+     differendWrappedTypes = buildSomeLawlessValue3
+       ({ maybe: \theA -> Just theA } :: { maybe :: forall a. a -> Maybe a})
```

However, we then get a new compiler error:

      Could not match type

        a1

      with type

        String


    while trying to match type Function a1
      with type Function String
    while solving type class constraint

      Prim.Row.Cons "maybe"
                    (String -> Maybe String)
                    t0
                    ( maybe :: forall (a :: Type). a -> Maybe a
                    )

    while inferring the type of buildSomeLawlessValue3 { maybe: \theA ->
                                                                  ...
                                                      }
    in value declaration demonstrateMultipleKinds

The code for our input record is correct, but we can't prove it to the type system.
So, we will bypass the type system by hiding our code in existential types
called `ProvideX` where `X` again indicates the number of type parameters
the underlying type takes.

-}

foreign import data Provide0 :: Type -> Type

provide0 :: forall rows a. ({ | rows } -> a) -> Provide0 a
provide0 = unsafeCoerce

consume0 :: forall rows a. Provide0 a -> ({ | rows } -> a)
consume0 = unsafeCoerce

newtype Kind0 :: Symbol -> Type -> Type
newtype Kind0 labelInRecord a = Kind0 a

derive instance Newtype (Kind0 labelInRecord a) _

-- Notice that the value in the record at the given label is `Provide0 a` now.
instance
  ( Row.Cons labelInRecord (Provide0 a) tail rows
  , IsSymbol labelInRecord
  ) =>
  SomeLawlessTypeClass4 { | rows } (Kind0 labelInRecord a) where
  buildSomeLawlessValue4 inputRecord = do
    let
      provideLocalBuildSomeLawlessValue4 :: Provide0 a
      provideLocalBuildSomeLawlessValue4 = Record.get (Proxy :: Proxy labelInRecord) inputRecord

      -- Here we re-expose the underlying value hidden existentially by `Provide0`
      localBuildSomeLawlessValue4 :: { | rows } -> a
      localBuildSomeLawlessValue4 = consume0 provideLocalBuildSomeLawlessValue4

      someLawlessValue4 :: a
      someLawlessValue4 = localBuildSomeLawlessValue4 inputRecord

    Kind0 someLawlessValue4

foreign import data Provide1 :: (Type -> Type) -> Type

provide1 :: forall f a. (a -> f a) -> Provide1 f
provide1 = unsafeCoerce

consume1 :: forall f a. Provide1 f -> (a -> f a)
consume1 = unsafeCoerce

-- The `a` here is actually `f a`
newtype Kind1 :: Symbol -> Type -> Type
newtype Kind1 labelInRecord a = Kind1 a

derive instance Newtype (Kind1 labelInRecord fa) _

instance
  ( SomeLawlessTypeClass4 { | rows } a
  -- Again, the record's value is now `Provide1`
  , Row.Cons labelInRecord (Provide1 f) tail rows
  , IsSymbol labelInRecord
  ) =>
  SomeLawlessTypeClass4 { | rows } (Kind1 labelInRecord (f a)) where
  buildSomeLawlessValue4 inputRecord = do
    let
      theA :: a
      theA = buildSomeLawlessValue4 inputRecord

      provideLocalBuildSomeLawlessValue4 :: Provide1 f
      provideLocalBuildSomeLawlessValue4 = Record.get (Proxy :: Proxy labelInRecord) inputRecord

      -- Here we re-expose the underlying value hidden existentially by `Provide1`
      localBuildSomeLawlessValue4 :: a -> f a
      localBuildSomeLawlessValue4 = consume1 provideLocalBuildSomeLawlessValue4

      someLawlessValue4 :: f a
      someLawlessValue4 = localBuildSomeLawlessValue4 theA

    Kind1 someLawlessValue4

main :: Effect Unit
main = do
  log "\n\nStep 4:\n"
  demonstrateNoOverrides
  demonstrateKind0
  demonstrateKind1
  demonstrateMultipleKinds

demonstrateNoOverrides :: Effect Unit
demonstrateNoOverrides = do
  let
    noOverrides :: String
    noOverrides = buildSomeLawlessValue4 unit
  log noOverrides

demonstrateKind0 :: Effect Unit
demonstrateKind0 = do
  let
    kind0Override = buildSomeLawlessValue4 { str: provide0 $ const "foo" }

    dropSpecialType
      :: Tuple String (Kind0 "str" String)
      -> Tuple String String
    dropSpecialType = coerce

  log $ show $ dropSpecialType kind0Override

demonstrateKind1 :: Effect Unit
demonstrateKind1 = do
  let
    kind1Override = buildSomeLawlessValue4
      { useJust: provide1 \theA -> Just theA
      , useNothing: provide1 \_ -> Nothing
      }

    dropSpecialType
      :: Tuple (Kind1 "useJust" (Maybe String)) (Kind1 "useNothing" (Maybe String))
      -> Tuple (Maybe String) (Maybe String)
    dropSpecialType = coerce

  log $ show $ dropSpecialType kind1Override

-- And now our code works!
demonstrateMultipleKinds :: Effect Unit
demonstrateMultipleKinds = do
  let
    simultaneousOverrides = buildSomeLawlessValue4
      { maybe1: provide1 $ \theA -> Just theA
      , customString: provide0 $ const "What1? Two local overrides at once!? That's pretty cool..."
      , int: provide0 $ const 4
      }

    dropSpecialType
      :: Tuple
           (Kind1 "maybe1" (Maybe (Kind0 "customString" String)))
           (Kind1 "maybe1" (Maybe (Kind1 "maybe1" (Maybe (Kind0 "int" Int)))))
      -> Tuple
           (Maybe String)
           (Maybe (Maybe Int))
    dropSpecialType = coerce

  log $ show $ dropSpecialType simultaneousOverrides