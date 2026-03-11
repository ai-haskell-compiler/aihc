# Google-oauth2-jwt ![CI](https://github.com/MichelBoucey/google-oauth2-jwt/actions/workflows/haskell-ci.yml/badge.svg) [![Hackage](https://img.shields.io/hackage/v/google-oauth2-jwt.svg)](https://hackage.haskell.org/package/google-oauth2-jwt)

Google-oauth2-jwt implements the creation of the signed JWT for Google Service Accounts,
to make authorized calls to Google APIs from server to server. All details here:

- [https://developers.google.com/identity/protocols/OAuth2ServiceAccount](https://developers.google.com/identity/protocols/OAuth2ServiceAccount)

Advice: be sure that the machine time is well synchronized to successfully make the access token request and get a Bearer token.
