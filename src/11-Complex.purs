module Complex where

import Prelude

import Control.Monad.Reader (ReaderT(..), ask, local, runReader, runReaderT)
import Data.Array as Array
import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Prim.Row as Row
import Record as Record
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- This file demonstrates even more complex local overrides.
--
-- In this example, local overrides can use additional information stored
-- within the input record to determine what their implementation will be.
--
-- In other words, each local override implementation
-- has access to either the original input record
-- or some modified version of it. As a result,
-- the input record type will be recursive since its labels
-- can reference itself. Thus, it will need to be
-- wrapped in a newtype to support this recursive nature.
--
-- This file makes two changes from our previous examples:
-- 1. it replaces the `externRec -> a` type signature with `ReaderT externRec Identity a`.
-- 2. the `externRec` is assumed to be a newtyped record.
-- 3. the `SomeLawlessTypeClass` is now called `Delegate`

class Delegate newtypedRecord a where
  delegate :: ReaderT newtypedRecord Identity a

instance Delegate newtypedRecord Unit where
  delegate = pure unit

instance Delegate newtypedRecord String where
  delegate = pure "default String implementation"

instance (Delegate newtypedRecord a) => Delegate newtypedRecord (Maybe a) where
  delegate = Just <$> delegate

instance (Delegate newtypedRecord a, Delegate newtypedRecord b) => Delegate newtypedRecord (Tuple a b) where
  delegate = Tuple <$> delegate <*> delegate

newtype Kind0 :: Symbol -> Type -> Type
newtype Kind0 sym a = Kind0 a

derive instance Newtype (Kind0 sym a) _

-- | `Provide0 a` existentially hides `ReaderT newtypedRecord Identity a`
foreign import data Provide0 :: Type -> Type

mkProvide0 :: forall newtypedRecord a. ReaderT newtypedRecord Identity a -> Provide0 a
mkProvide0 = unsafeCoerce

runProvide0 :: forall newtypedRecord a. Provide0 a -> ReaderT newtypedRecord Identity a
runProvide0 = unsafeCoerce

instance
  -- The Newtype constraint here is new!
  ( Newtype newtypedRecord { | rows }
  , Row.Cons sym (Provide0 a) tail rows
  , IsSymbol sym
  ) =>
  Delegate newtypedRecord (Kind0 sym a) where
  delegate :: ReaderT newtypedRecord Identity (Kind0 sym a)
  delegate = ReaderT \newtypedRecord -> do
    let
      -- and we need to unwrap the newtype before we can operate on the actual record.
      record :: { | rows }
      record = unwrap newtypedRecord

      existentiallyHiddenImplementation :: Provide0 a
      existentiallyHiddenImplementation = Record.get (Proxy :: Proxy sym) record

      externalImplementation :: ReaderT newtypedRecord Identity a
      externalImplementation = runProvide0 existentiallyHiddenImplementation

      wrapInKind0
        :: ReaderT newtypedRecord Identity a
        -> ReaderT newtypedRecord Identity (Kind0 sym a)
      wrapInKind0 = coerce

    -- This is where things get interesting. This code enables
    -- local overrides to access the original input record.
    -- However, since we can use `local` to modify that record
    -- in a top-down direction, perhaps we'll receive a modified
    -- version of the record? Wait until we get to the example below.
    runReaderT (wrapInKind0 externalImplementation) newtypedRecord

newtype Kind1 :: Symbol -> Type -> Type
newtype Kind1 sym fa = Kind1 fa

derive instance Newtype (Kind1 sym fa) _

-- | `Provide1 f` existentially hides `forall a. ReaderT { | rows } Identity a -> ReaderT { | rows } Identity (f a)`
foreign import data Provide1 :: (Type -> Type) -> Type

mkProvide1
  :: forall newtypedRecord f a
   . (ReaderT newtypedRecord Identity a -> ReaderT newtypedRecord Identity (f a))
  -> Provide1 f
mkProvide1 = unsafeCoerce

runProvide1
  :: forall newtypedRecord f a
   . Provide1 f
  -> (ReaderT newtypedRecord Identity a -> ReaderT newtypedRecord Identity (f a))
runProvide1 = unsafeCoerce

instance
  ( Newtype newtypedRecord { | rows }
  , Delegate newtypedRecord a
  , Row.Cons sym (Provide1 f) tail rows
  , IsSymbol sym
  ) =>
  Delegate newtypedRecord (Kind1 sym (f a)) where
  delegate :: ReaderT newtypedRecord Identity (Kind1 sym (f a))
  delegate = ReaderT \newtypedRecord -> do
    let
      record :: { | rows }
      record = unwrap newtypedRecord

      existentiallyHiddenImplementation :: Provide1 f
      existentiallyHiddenImplementation = Record.get (Proxy :: Proxy sym) record

      externalImplementation :: ReaderT newtypedRecord Identity a -> ReaderT newtypedRecord Identity (f a)
      externalImplementation = runProvide1 existentiallyHiddenImplementation

      delegateAImplementation :: ReaderT newtypedRecord Identity a
      delegateAImplementation = delegate

      wrapInKind1
        :: ReaderT newtypedRecord Identity (f a)
        -> ReaderT newtypedRecord Identity (Kind1 sym (f a))
      wrapInKind1 = coerce

    runReaderT (wrapInKind1 (externalImplementation delegateAImplementation)) newtypedRecord

newtype NoLocalOverrides = NoLocalOverrides {}

derive instance Newtype NoLocalOverrides _

main :: Effect Unit
main = do
  log $ "Normal example: " <> show (runReader delegate (NoLocalOverrides {}) :: Tuple String (Tuple String String))

  tryMoreComplexExample

newtype Extern2 = Extern2
  { str :: Provide0 String
  , maybe :: Provide1 Maybe
  -- Oh look! Extra info that we can reference and modify in our local overrides...
  -- I wonder what code could be written with this possibility...
  , extraInfo :: Int
  }

derive instance Newtype Extern2 _

tryMoreComplexExample :: Effect Unit
tryMoreComplexExample = do
  log "\n\nComplex:\n"
  let
    base :: Extern2
    base = Extern2
      -- We'll start this number at 0.
      { extraInfo: 0
      , maybe: mkProvide1 \delegateA -> do
          { extraInfo } <- ask
          -- If we're less than 2, keep going by wrapping something in `Just`
          -- Otherwise, we'll just return Nothing
          if extraInfo < 3 then
            Just <$> (local (\r -> r { extraInfo = r.extraInfo + 1 }) delegateA)
          else
            pure Nothing
      , str: mkProvide0 do
          { extraInfo } <- ask
          -- Similarly, if we're less than 2, we'll use the default instance
          -- Otherwise, we'll provide a custom one
          if extraInfo < 2 then
            delegate
          else
            pure "new string representation"
      }

    dropExternRef
      :: Tuple6
           (Maybe (Kind0 "str" String))
           (Kind1 "maybe" (Maybe (Kind0 "str" String)))
           (Kind1 "maybe" (Maybe (Kind1 "maybe" (Maybe (Kind0 "str" String)))))
           -- You can see my pattern, but oof! This is getting hard to read.
           -- Now would be a good time to define a type alias. I've defined one
           -- after this block of code, but I'll restate it here in a comment for readability:
           --     MaybeX a == Kind1 "maybe" (Maybe a)
           (MaybeX (MaybeX (MaybeX (Kind0 "str" String))))
           (MaybeX (MaybeX (MaybeX (MaybeX (Kind0 "str" String)))))
           (MaybeX (MaybeX (MaybeX (MaybeX (MaybeX (Kind0 "str" String))))))
      -> Tuple6
           (Maybe String)
           (Maybe String)
           (Maybe (Maybe String))
           (Maybe (Maybe (Maybe String)))
           (Maybe (Maybe (Maybe (Maybe String))))
           (Maybe (Maybe (Maybe (Maybe (Maybe String)))))
    dropExternRef = coerce

  log $ show $ dropExternRef $ runReader delegate base

type MaybeX a = Kind1 "maybe" (Maybe a)

data Tuple6 a b c d e f = Tuple6 a b c d e f

instance
  ( Delegate newtypedRecord a
  , Delegate newtypedRecord b
  , Delegate newtypedRecord c
  , Delegate newtypedRecord d
  , Delegate newtypedRecord e
  , Delegate newtypedRecord f
  ) =>
  Delegate newtypedRecord (Tuple6 a b c d e f) where
  delegate = Tuple6 <$> delegate <*> delegate <*> delegate <*> delegate <*> delegate <*> delegate

instance
  ( Show a
  , Show b
  , Show c
  , Show d
  , Show e
  , Show f
  ) =>
  Show (Tuple6 a b c d e f) where
  show (Tuple6 a b c d e f) =
    "Tuple6(\n  " <> (Array.intercalate "\n  " [ show a, show b, show c, show d, show e, show f ]) <> "\n)"
