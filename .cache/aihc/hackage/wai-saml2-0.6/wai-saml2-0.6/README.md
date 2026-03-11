# wai-saml2

![GitHub](https://img.shields.io/github/license/mbg/wai-saml2)
![Haskell CI](https://github.com/mbg/wai-saml2/workflows/Haskell/badge.svg?branch=master)
![stackage-nightly](https://github.com/mbg/wai-saml2/workflows/stackage-nightly/badge.svg)
[![Hackage](https://img.shields.io/hackage/v/wai-saml2)](https://hackage.haskell.org/package/wai-saml2)

A Haskell library which implements SAML2 assertion validation as WAI middleware. This can be used by a Haskell web application (the service provider, SP) to perform identity provider (IdP) initiated authentication, i.e. SAML2-based authentication where the authentication begins at the IdP-end, the IdP authenticates the user, and then gets the user to submit a SAML2 assertion back to the SP (known as "unsolicited SSO" within e.g. [the Shibboleth project](https://wiki.shibboleth.net/confluence/display/IDP30/UnsolicitedSSOConfiguration#UnsolicitedSSOConfiguration-SAML2.0)).

## Completeness

There are currently a number of limitations to this library:

* The library implements IdP-initiated authentication and has some support for SP-initiated authentication (See the documentation for the `Network.Wai.SAML2.Request` module)

* The library does not currently support the full SAML2 specification and makes certain assumptions about what the IdP's responses contain. It will most likely fail with any IdPs which do not send responses in the same format. If you wish to use this library and encounter problems with your IdP, please open an issue or a pull request which implements support accordingly.

## Security

The library is estimated to be sufficiently robust for use in a production environment. If you wish to implement this middleware, please note the following:

* You __must__ store IDs of assertions you see. If an assertion is successfully validated by this library, you __must__ check that you have not previously seen the assertion ID in order to prevent replay attacks.

* You __must not__ expose any errors to a client as that could severely compromise the security of the system as attackers may be able to use the errors to narrow down valid SAML responses. You __should__ log and monitor errors though as they may indicate attacks on your system. Ensure that log files containing errors from the SAML2 middleware are stored securely.

## Usage

### Preliminaries

You need to have registered your service provider with the identity provider. You need to have access to the IdP's metadata, which will contain the public key used for signature validation.

### Configuration

The `saml2Config` function may be used to construct `SAML2Config` values. It expects at least the SP's private key and the IdP's public key as arguments (even when mandatory encryption is disabled) but you should almost certainly customise the configuration further. The private and public keys can be loaded with functions from the `Data.X509` and `Data.X509.File` modules (from the `x509` and `x509-store` packages, respectively):

```haskell
(saml2Config spPrivateKey idpPublicKey){
    saml2AssertionPath = "/sso/assert",
    saml2ExpectedIssuer = Just "https://idp.sp.com/saml2",
    saml2ExpectedDestination = Just "https://example.com/sso/assert",
}
```

The configuration options are documented in the Haddock documentation for the `Network.Wai.SAML2.Config` module.

### Implementation

Two interfaces to the middleware are provided. See the Haddock documentation for the `Network.Wai.SAML2` module for full usage examples. An example using the `saml2Callback` variant is shown below, where `cfg` is a `SAML2Config` value and `app` is your existing WAI application:

```haskell
saml2Callback cfg callback mainApp
 where callback (Left err) app req sendResponse = do
           -- a POST request was made to the assertion endpoint, but
           -- something went wrong, details of which are provided by
           -- the error: this should probably be logged as it may
           -- indicate that an attack was attempted against the
           -- endpoint, but you *must* not show the error
           -- to the client as it would severely compromise
           -- system security
           --
           -- you may also want to return e.g. a HTTP 400 or 401 status
       callback (Right result) app req sendResponse = do
           -- a POST request was made to the assertion endpoint and the
           -- SAML2 response was successfully validated:
           -- you *must* check that you have not encountered the
           -- assertion ID before; we assume that there is a
           -- computation tryRetrieveAssertion which looks up
           -- assertions by ID in e.g. a database
           result <- tryRetrieveAssertion (assertionId (assertion result))

           case result of
               Just something -> -- a replay attack has occurred
               Nothing -> do
                   -- store the assertion id somewhere
                   storeAssertion (assertionId (assertion result))

                   -- the assertion is valid and you can now e.g.
                   -- retrieve user data from your database
                   -- before proceeding with the request by e.g.
                   -- redirecting them to the main view
```

## Contributions

Please see [CONTRIBUTING.md](CONTRIBUTING.md)

## References

* [SAML2 specification](http://docs.oasis-open.org/security/saml/Post2.0/sstc-saml-tech-overview-2.0.html)
* [Exclusive XML Canonicalisation](https://www.w3.org/TR/xml-exc-c14n/)
* [XML Signature Syntax and Processing](https://www.w3.org/TR/xmldsig-core1/)
* [XML Encryption Syntax and Processing](https://www.w3.org/TR/2002/REC-xmlenc-core-20021210/Overview.html)
