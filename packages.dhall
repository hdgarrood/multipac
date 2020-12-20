let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.13.8-20201217/packages.dhall sha256:f46d45e29977f3b57717b56d20a5ceac12532224516eea3012a4688f22ac1539

let additions =
      { smolder-dom =
          { repo = "https://github.com/hdgarrood/purescript-smolder-dom"
          , version = "96a96bdea49171f4108cc384fbdca0cbaebe0ce3"
          , dependencies = [ "smolder" ]
          }
      }

in  upstream // additions
