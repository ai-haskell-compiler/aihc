module Data.Api.InternalPure
  ( mkExchangePublicTokenEnv
  , mkCreatePublicTokenEnv
  , mkGetBalanceEnv
  , mkGetAuthEnv
  , mkCreateTransactionsGetEnv
  , mkCreateIdentityGetEnv
  , mkCreateIncomeGetEnv
  ) where

import           Data.Common
import           Data.Proof.Proof
import           Data.Time        (Day)

mkGetAuthEnv
  :: PlaidEnv
  -> AccessToken
  -> Either PlaidError (PlaidBody AuthGet)
mkGetAuthEnv env atoken =
  name env (\namedEnv ->
    name atoken (\namedAccessToken ->
      let proof = proveAuthGetBody namedEnv namedAccessToken
          body = mempty & plaidBodyEnv .~ env
                        & plaidBodyAccessToken ?~ atoken
      in either (Left . PlaidError) (const (Right body)) proof
  ))

mkGetBalanceEnv
  :: PlaidEnv
  -> AccessToken
  -> Either PlaidError (PlaidBody GetBalance)
mkGetBalanceEnv env atoken =
  name env (\namedEnv ->
    name atoken (\namedAccessToken ->
      let proof = proveGetBalanceBody namedEnv namedAccessToken
          body = mempty & plaidBodyEnv .~ env
                        & plaidBodyAccessToken ?~ atoken
       in either (Left . PlaidError) (const (return body)) proof
  ))

-- | Creates a Public Token
-- https://plaid.com/docs/#exchange-token-flow
mkCreatePublicTokenEnv
  :: PlaidEnv
  -> InstitutionId
  -> [PlaidProduct]
  -> Maybe PlaidOptions
  -> Either PlaidError (PlaidBody PublicTokenCreate)
mkCreatePublicTokenEnv env instId products options =
  name env (\namedEnv ->
    name instId (\namedInstId ->
      name products (\namedProducts ->
        name options (\namedOptions ->
          let proof = proveCreatePublicTokenBody namedEnv namedInstId namedProducts namedOptions
              body = mempty & plaidBodyEnv .~ env
                            & plaidBodyOptions .~ options
                            & plaidBodyInstitutionId ?~ instId
                            & plaidBodyInitialProducts ?~ products
           in either (Left . PlaidError) (const (Right body)) proof
  ))))

mkExchangePublicTokenEnv
  :: PlaidEnv
  -> PublicToken
  -> Either PlaidError (PlaidBody PlaidTokenExchange)
mkExchangePublicTokenEnv _ (PublicToken "") = Left $ PlaidError "PublicToken cannot be empty"
mkExchangePublicTokenEnv env publicToken =
  Right $ mempty & plaidBodyEnv .~ env
                 & plaidBodyPublicToken ?~ publicToken


-- | Creates request body for /transactions/get call
-- https://plaid.com/docs/#retrieve-transactions-request
mkCreateTransactionsGetEnv
  :: PlaidEnv
  -> AccessToken
  -> Day
  -> Day
  -> Maybe PlaidOptions
  -> Maybe PlaidPaginationOptions
  -> Either PlaidError (PlaidBody PlaidTransactionsGet)
mkCreateTransactionsGetEnv env accessToken startDate endDate options paginationOptions =
  name env (\namedEnv ->
    name accessToken (\namedAccessToken ->
      name startDate (\namedStartDate ->
        name endDate (\namedEndDate ->
          name options (\namedOptions ->
            name paginationOptions (\namedPaginationOptions ->
              let proof =
                    proveTransactionsGetBody namedEnv namedAccessToken namedStartDate namedEndDate namedOptions namedPaginationOptions
                  body = mempty & plaidBodyEnv .~ env
                                & plaidBodyAccessToken ?~ accessToken
                                & plaidBodyOptions .~ options
                                & plaidBodyStartDate ?~ startDate
                                & plaidBodyEndDate ?~ endDate
                                & plaidBodyPaginationOptions ?~ defPaginationOptions

              in either (Left . PlaidError) (const (Right body)) proof
  ))))))

-- | Creates request body for /identity/get call
-- https://plaid.com/docs/#identity
mkCreateIdentityGetEnv
  :: PlaidEnv
  -> AccessToken
  -> Either PlaidError (PlaidBody PlaidIdentityGet)
mkCreateIdentityGetEnv env accessToken =
  name env (\namedEnv ->
    name accessToken (\namedAccessToken ->
      let proof = proveIdentityGetBody namedEnv namedAccessToken
          body = mempty & plaidBodyEnv .~ env
                        & plaidBodyAccessToken ?~ accessToken

      in either (Left . PlaidError) (const (Right body)) proof
  ))

-- | Creates request body for /income/get call
-- https://plaid.com/docs/#income
mkCreateIncomeGetEnv
  :: PlaidEnv
  -> AccessToken
  -> Either PlaidError (PlaidBody PlaidIncomeGet)
mkCreateIncomeGetEnv env accessToken =
  name env (\namedEnv ->
    name accessToken (\namedAccessToken ->
      let proof = proveIncomeGetBody namedEnv namedAccessToken
          body = mempty & plaidBodyEnv .~ env
                        & plaidBodyAccessToken ?~ accessToken

      in either (Left . PlaidError) (const (Right body)) proof
  ))
