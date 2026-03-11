module Main where

import qualified Attoparsec.Data as Attoparsec
import qualified Data.Text as Text
import qualified Optima
import Prelude

main :: IO ()
main = try @SomeException parseOpts >>= print
  where
    parseOpts =
      Optima.params
        "Demo"
        ( liftA3
            (,,)
            ( Optima.param
                (Just 'a')
                "arg-a"
                ( Optima.value
                    "Description of A"
                    (Optima.showable True)
                    Optima.unformatted
                    (Optima.explicitlyParsed Attoparsec.bool)
                )
            )
            ( Optima.group
                "group1"
                ( liftA2
                    (,)
                    ( Optima.member
                        "bb"
                        ( Optima.value
                            "Description of B"
                            Optima.defaultless
                            Optima.unformatted
                            (Optima.explicitlyParsed Attoparsec.text)
                        )
                    )
                    (Optima.member "d" textParam)
                )
            )
            ( Optima.group
                "group-of-alternatives"
                ( asum
                    [ Optima.member "e" textParam,
                      Optima.subgroup
                        "subgroup"
                        ( asum
                            [ Optima.member "f" textParam,
                              Optima.member "g" (flag $> "slkdfjsdkj"),
                              Text.concat <$> some (Optima.member "h" textParam)
                            ]
                        )
                    ]
                )
            )
        )
      where
        textParam = Optima.value "Text param" Optima.defaultless Optima.unformatted (Optima.explicitlyParsed Attoparsec.text)
        flag = Optima.flag "Flag type 1"
