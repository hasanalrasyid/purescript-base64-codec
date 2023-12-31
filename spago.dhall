{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "base64-codec"
, dependencies =
  [ "arraybuffer-types"
  , "console"
  , "debug"
  , "effect"
  , "newtype"
  , "prelude"
  , "psci-support"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
