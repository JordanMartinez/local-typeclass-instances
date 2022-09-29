{ name = "my-project"
, dependencies =
  [ "console"
  , "effect"
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
