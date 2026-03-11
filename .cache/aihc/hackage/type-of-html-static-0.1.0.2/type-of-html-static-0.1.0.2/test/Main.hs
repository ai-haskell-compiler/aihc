{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE DataKinds        #-}

module Main where

import Html
import Html.Static

import Data.Proxy

main :: IO ()
main = pure ()
  where
    _t_ =
      ( div_ "a" :: 'Div > String
      , $(static (div_ "a")) :: Proxy "<div>a</div>"
      , $(static (map td_ [1..3::Int])) :: Proxy "<td>1</td><td>2</td><td>3</td>"
      , $(static (div_ "<")) :: Proxy "<div>&lt;</div>"
      )

helloWorld
  :: ('Title ?> a)
  => a
  -> 'Html > (('Head > ('Title > a)) # ('Body > ('P > Proxy "Hello World!")))
helloWorld x = $(optimize [|
  html_
    ( head_
      ( title_ x
      )
    # body_
      ( p_ "Hello World!"
      )
    )
  |])
