--------------------------------------------------------------------------------
-- SAML2 Middleware for WAI                                                   --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

-- | Implements WAI 'Middleware' for SAML2 service providers. Two different
-- interfaces are supported (with equivalent functionality): one which simply
-- stores the outcome of the validation process in the request vault and one
-- which passes the outcome to a callback.
module Network.Wai.SAML2 (
    -- * Callback-based middleware
    --
    -- $callbackBasedMiddleware
    Result(..),
    saml2Callback,

    -- * Vault-based middleware
    --
    -- $vaultBasedMiddleware
    assertionKey,
    errorKey,
    saml2Vault,
    relayStateKey,

    -- * Re-exports
    module Network.Wai.SAML2.Config,
    module Network.Wai.SAML2.Error,
    module Network.Wai.SAML2.Assertion
) where

--------------------------------------------------------------------------------

import qualified Data.ByteString as BS
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import qualified Data.Vault.Lazy as V

import Network.Wai
import Network.Wai.Parse
import Network.Wai.SAML2.Config
import Network.Wai.SAML2.Validation
import Network.Wai.SAML2.Assertion
import Network.Wai.SAML2.Error
import qualified Network.Wai.SAML2.Response as SAML2

import System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------------------------------

-- | Checks whether the request method of @request@ is @"POST"@.
isPOST :: Request -> Bool
isPOST = (=="POST") . requestMethod

--------------------------------------------------------------------------------

-- $callbackBasedMiddleware
--
-- This 'Middleware' provides a SAML2 service provider (SP) implementation
-- that can be wrapped around an existing WAI 'Application'. The middleware is
-- parameterised over the SAML2 configuration and a callback. If the middleware
-- intercepts a request made to the endpoint given by the SAML2 configuration,
-- the result of validating the SAML2 response contained in the request body
-- will be passed to the callback.
--
-- > saml2Callback cfg callback mainApp
-- >  where callback (Left err) app req sendResponse = do
-- >            -- a POST request was made to the assertion endpoint, but
-- >            -- something went wrong, details of which are provided by
-- >            -- the error: this should probably be logged as it may
-- >            -- indicate that an attack was attempted against the
-- >            -- endpoint, but you *must* not show the error
-- >            -- to the client as it would severely compromise
-- >            -- system security
-- >            --
-- >            -- you may also want to return e.g. a HTTP 400 or 401 status
-- >
-- >        callback (Right result) app req sendResponse = do
-- >            -- a POST request was made to the assertion endpoint and the
-- >            -- SAML2 response was successfully validated:
-- >            -- you *must* check that you have not encountered the
-- >            -- assertion ID before; we assume that there is a
-- >            -- computation tryRetrieveAssertion which looks up
-- >            -- assertions by ID in e.g. a database
-- >            result <- tryRetrieveAssertion (assertionId (assertion result))
-- >
-- >            case result of
-- >                Just something -> -- a replay attack has occurred
-- >                Nothing -> do
-- >                    -- store the assertion id somewhere
-- >                    storeAssertion (assertionId (assertion result))
-- >
-- >                    -- the assertion is valid and you can now e.g.
-- >                    -- retrieve user data from your database
-- >                    -- before proceeding with the request by e.g.
-- >                    -- redirecting them to the main view

-- | 'saml2Callback' @config callback@ produces SAML2 'Middleware' for
-- the given @config@. If the middleware intercepts a request to the
-- endpoint given by @config@, the result will be passed to @callback@.
saml2Callback :: SAML2Config
              -> (Either SAML2Error Result -> Middleware)
              -> Middleware
saml2Callback cfg callback app req sendResponse = do
    let path = rawPathInfo req

    -- check if we need to handle this request
    if path == saml2AssertionPath cfg && isPOST req then do
            -- default request parse options, but do not allow files;
            -- we are not expecting any
            let bodyOpts = setMaxRequestNumFiles 0
                         $ setMaxRequestFileSize 0
                         $ defaultParseRequestBodyOptions

            -- parse the request
            (body, _) <- parseRequestBodyEx bodyOpts lbsBackEnd req

            case lookup "SAMLResponse" body of
                Just val -> do
                    let rs = lookup "RelayState" body
                    result <- validateResponse cfg val <&>
                                  fmap (\(assertion, response) ->
                                            Result{
                                                assertion = assertion,
                                                relayState = rs,
                                                response = response
                                            })

                    -- call the callback
                    callback result app req sendResponse
                -- the request does not contain the expected payload
                Nothing -> callback (Left InvalidRequest) app req sendResponse

    -- not one of the paths we need to handle, pass the request on to the
    -- inner application
    else app req sendResponse

--------------------------------------------------------------------------------

-- $vaultBasedMiddleware
--
-- This is a simpler-to-use 'Middleware' which stores the outcome of a request
-- made to the assertation endpoint in the request vault. The inner WAI
-- application can then check of the presence of an assertion or an error with
-- 'V.lookup' and 'assertionKey' or 'errorKey' respectively. At most one of
-- the two locations will be populated for a given request, i.e. it is not
-- possible for an assertion to be validated and an error to occur.
--
-- > saml2Vault cfg $ \app req sendResponse -> do
-- >    case V.lookup errorKey (vault req) of
-- >        Just err ->
-- >            -- log the error, but you *must* not show the error
-- >            -- to the client as it would severely compromise
-- >            -- system security
-- >        Nothing -> pure () -- carry on
-- >
-- >    case V.lookup assertionKey (vault req) of
-- >        Nothing -> pure () -- carry on
-- >        Just assertion -> do
-- >            -- a valid assertion was processed by the middleware,
-- >            -- you *must* check that you have not encountered the
-- >            -- assertion ID before; we assume that there is a
-- >            -- computation tryRetrieveAssertion which looks up
-- >            -- assertions by ID in e.g. a database
-- >            result <- tryRetrieveAssertion (assertionId assertion)
-- >
-- >            case result of
-- >                Just something -> -- a replay attack has occurred
-- >                Nothing -> do
-- >                    -- store the assertion id somewhere
-- >                    storeAssertion (assertionId assertion)
-- >
-- >                    -- the assertion is valid

-- | 'assertionKey' is a vault key for retrieving assertions from
-- request vaults if the 'saml2Vault' 'Middleware' is used.
assertionKey :: V.Key Assertion
assertionKey = unsafePerformIO V.newKey

-- | 'relayStateKey' is a vault key for retrieving the relay state
-- from request vaults if the 'saml2Vault' 'Middleware' is used
-- and the assertion is valid.
relayStateKey :: V.Key BS.ByteString
relayStateKey = unsafePerformIO V.newKey

-- | 'errorKey' is a vault key for retrieving SAML2 errors from request vaults
-- if the 'saml2Vault' 'Middleware' is used.
errorKey :: V.Key SAML2Error
errorKey = unsafePerformIO V.newKey

-- | 'saml2Vault' @config@ produces SAML2 'Middleware' for the given @config@.
saml2Vault :: SAML2Config -> Middleware
saml2Vault cfg = saml2Callback cfg callback
    -- if the middleware intercepts a request containing a SAML2 response at
    -- the configured endpoint, the outcome of processing response will be
    -- passed to this callback: we store the result in the corresponding
    -- entry in the request vault
    where callback (Left err) app req sendResponse = do
            app req{
                vault = V.insert errorKey err (vault req)
            } sendResponse
          callback (Right result) app req sendResponse = do
            let mRelayState = relayState result
            let vlt = vault req

            app req{
                vault = V.insert assertionKey (assertion result)
                      $ fromMaybe vlt $ mRelayState >>= \rs ->
                            pure $ V.insert relayStateKey rs vlt
            } sendResponse

--------------------------------------------------------------------------------

-- | Represents the result of validating a SAML2 response.
data Result = Result {
    -- | An optional relay state, as provided in the POST request.
    relayState :: !(Maybe BS.ByteString),
    -- | The assertion obtained from the response that has been validated.
    assertion :: !Assertion,
    -- | The full response obtained from the IdP.
    --
    -- @since 0.4
    response :: !SAML2.Response
} deriving (Eq, Show)

--------------------------------------------------------------------------------
