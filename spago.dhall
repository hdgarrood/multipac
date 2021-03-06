{ name = "multipac"
, dependencies =
  [ "argonaut-codecs"
  , "argonaut-core"
  , "argonaut-generic"
  , "canvas"
  , "console"
  , "effect"
  , "foldable-traversable"
  , "generics-rep"
  , "js-timers"
  , "newtype"
  , "node-fs"
  , "node-http"
  , "node-process"
  , "ordered-collections"
  , "profunctor-lenses"
  , "psci-support"
  , "refs"
  , "safely"
  , "smolder"
  , "smolder-dom"
  , "transformers"
  , "tuples"
  , "web-dom"
  , "web-html"
  , "web-socket"
  , "web-storage"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
