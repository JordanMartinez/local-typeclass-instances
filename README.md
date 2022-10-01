# Local Typeclass Instances

## General Idea

This project demonstrates a "design pattern" and explains the step-by-step approach to getting this design by solving problems as they arise.

There's a few ways to describe the "design pattern":
- allowing one to insert a runtime value as a type class instance constraint
- type class instances parameterized by runtime configuration that is not in the type class declaration

Alternative approaches to this idea are:
- [Functional Pearl: Implicit Configurations -- or, Type Classes Reflect the Values of Types](https://okmij.org/ftp/Haskell/tr-15-04.pdf)
- [`purescript-reflection`](https://github.com/paf31/purescript-reflection), a PureScript port of the Haskell [`reflection`](https://hackage.haskell.org/package/reflection), which implements the ideas of the above paper

## Concrete Example

The following example will help explain this idea without jargon.

In the domain of decoding `Json`, PureScripters often use the `DecodeJson` type class. At various times, one may want to decode `Json` into a `Maybe a` type. There are two common ways this can be done:
- the tagged approach where the JSON looks like this: `{ tag: "Just", value: <jsonForA> }` and `{ tag: "Nothing" }`
- the nullary approach where the JSON looks like this: `<jsonForA>` and `null`

Yet, because PureScript disallows orphan instances due to global coherence, a type may only have one instance for any given type class. Thus, when defining a typeclass-based `Json` decoder, one must choose between one of those two representations for decoding `Maybe a`. If one JSON value uses both of these encoding, then one has only two options:
1. don't use a typeclass-based codec like `DecodeJson`; use a value-based codec instead
2. decode the value to a different type and then convert the result to the desired type (e.g. use a newtype).

The first option removes the biggest reason why people use typeclass-based codecs: eliminating boilerplate. However, the second option is also dreaded because one must define a newtype, write its `DecodeJson` instance, and then wrap/unwrap that newtype before/after calling `decodeJson`.

With this "decode Json" context in mind, this "design pattern" enables one to decode `Json` to `Maybe a` using both the tagged approach and the nullary approach in the same `decodeJson` call. Assuming `Maybe`'s actual instance for `DecodeJson` used the tagged approach, here's what the code might look like if the `DecodeJson` type class was updated to use this design pattern:

```purs
decodeValue
  :: Json
  -> { useTaggedApproach :: Maybe String
     , useNullaryApproach :: Maybe Int
     }
decodeValue j = 
  map toDesiredType $ runReader (decodeJson j) { maybeNullary: provide1 $ \decodeA -> decodeNull <|> Just <$> decodeA }
  where
  toDesiredType
    :: Either err
        { useTaggedApproach :: Maybe String
        , useNullaryApproach :: Kind1 "maybeNullary" (Maybe Int)
        }
    -> Either err
        { useTaggedApproach :: Maybe String
        , useNullaryApproach :: Maybe Int
        }
  toDesiredType = coerce
```

So, what's going on in the above code? Here's how it works:
1. The user calls `decodeJson j`. Normally, this would return back an `Either err a`. Here, it returns a `ReaderT record Identity (Either err a)`. (For those unfamiliar with monad transformers, this is the same as `record -> Identity (Either err a)`.)
2. When solving the `DecodeJson` instance for `Kind1`, the instance looks up the value stored under the label `"maybeNullary"` in the record provided by the `ReaderT`. It then uses this value to implement the `Kind1`'s instance. The `Kind1` type indicates the value it wraps will have kind `Type -> Type`. Thus, in `Kind1`'s instance, the compiler must also provide the `DecodeJson` instance for the type wrapped by the higher-kinded type (i.e. the `a` in `Maybe a`). This instance is then passed as an argument (i.e. `decodeA`) to the function stored in the input record.
3. When calling `decodeJson`, the user provides a record containing the implementation to use when the `maybeNullary` label is referenced. Since we're using a higher-kinded type, the function's argument is the `decodeJson` instance for the `a` that the higher-kinded type (i.e. `Maybe`) wraps in that particular usage.
4. By using `toDesiredType`/`coerce` at the end, the developer
    1. does not pay a performance cost
    2. informs the compiler what the output type should be (i.e. the return type of `toDesiredType`)
    3. informs the compiler where to override some type class instance with the input record's local implementation, and
    4. accomplishes all of this in a boilerplate-minimal way

Moreover, rather than defining a newtype and its `DecodeJson` instance for every situation an override on a type with kind `Type -> Type` needs to occur, `Kind1 labelInInputRecord f` is a library-provided newtype with a predefined instance that would work for all types of kind `Type -> Type` that expect the implementation to have the type signature of
```
forall a
   . DecodeJson record a
  => (Json -> ReaderT record Identity (Either err a))
  -> (Json -> ReaderT record Identity (Either err (f a)))
```

## Tradeoffs

- Pros
    - Types which do not have an instance for a type class can have an implementation provided via the call site's input record (i.e. runtime-configured type class constraint)
    - Local overrides can reuse a type's normal type class instance in their implementation
    - Local overrides can provide multiple, different implementations depending on the state of the input record (see the `Complex.purs` file's example)
    - No usages of unsafe-but-coincidentally-works code
- Cons
    - This approach only works if the type class is defined using this approach. In other words, it does not enable one to provide "local overrides" for pre-existing type classes that should not change (e.g. `Semigroup`/`Monoid`)
    - Those using a type class defined in this way will pay the cost of a `ReaderT` and its type classes, even if they don't need the local overrides.
