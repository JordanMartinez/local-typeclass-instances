{ name = "my-project"
, dependencies =
  [ "arrays"
  , "console"
  , "effect"
  , "identity"
  , "maybe"
  , "newtype"
  , "prelude"
  , "record"
  , "safe-coerce"
  , "transformers"
  , "tuples"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
