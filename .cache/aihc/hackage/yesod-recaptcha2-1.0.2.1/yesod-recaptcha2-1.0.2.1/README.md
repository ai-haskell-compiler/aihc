# yesod-recaptcha2

[![Hackage](https://img.shields.io/hackage/v/yesod-recaptcha2.svg)](https://hackage.haskell.org/package/yesod-recaptcha2)
[![yesod-recaptcha2 on Stackage LTS](http://stackage.org/package/yesod-recaptcha2/badge/lts)](http://stackage.org/lts/package/yesod-recaptcha2)
[![test](https://github.com/ncaq/yesod-recaptcha2/actions/workflows/test.yml/badge.svg)](https://github.com/ncaq/yesod-recaptcha2/actions/workflows/test.yml)

It support new Google [reCAPTCHA(v2, v3)](https://www.google.com/recaptcha/about/) for [yesod-form](https://hackage.haskell.org/package/yesod-form) instead [yesod-recaptcha](https://hackage.haskell.org/package/yesod-recaptcha) beacuse original yesod-recaptcha is dead.

And it support reCAPTCHA new version(v2, v3).

# Example

## Setup

~~~hs
import Yesod.ReCaptcha2
~~~

~~~hs
instance YesodReCaptcha App where
  reCaptchaSiteKey = pure "foo"
  reCaptchaSecretKey = pure "bar"
  reCaptchaLanguage = pure Nothing
~~~

## Append to applicative form

~~~hs
buildForm :: Form MyForm
buildForm = renderDivs $ MyForm <$>
  areq textField "foo" Nothing <*
  reCaptcha
~~~
