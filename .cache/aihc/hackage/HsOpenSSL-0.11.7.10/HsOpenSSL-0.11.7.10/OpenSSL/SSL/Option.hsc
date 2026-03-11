{-# LANGUAGE DeriveDataTypeable #-}
-- | See https://www.openssl.org/docs/ssl/SSL_CTX_set_options.html
module OpenSSL.SSL.Option
    ( SSLOption(..)
    , optionToIntegral
    )
    where
import Data.Typeable

#include <openssl/ssl.h>

-- | The behaviour of the SSL library can be changed by setting
-- several options. During a handshake, the option settings of the
-- 'OpenSSL.Session.SSL' object are used. When a new
-- 'OpenSSL.Session.SSL' object is created from a
-- 'OpenSSL.Session.SSLContext', the current option setting is
-- copied. Changes to 'OpenSSL.Session.SSLContext' do not affect
-- already created 'OpenSSL.Session.SSL' objects.
data SSLOption
    = -- | As of OpenSSL 1.0.0 this option has no effect.
      SSL_OP_MICROSOFT_SESS_ID_BUG
      -- | As of OpenSSL 1.0.0 this option has no effect.
    | SSL_OP_NETSCAPE_CHALLENGE_BUG
      -- | As of OpenSSL 0.9.8q and 1.0.0c, this option has no effect.
    | SSL_OP_NETSCAPE_REUSE_CIPHER_CHANGE_BUG
      -- | As of OpenSSL 1.0.1h and 1.0.2, this option has no effect.
    | SSL_OP_SSLREF2_REUSE_CERT_TYPE_BUG
      -- | As of OpenSSL 1.1.0 this option has no effect.
    | SSL_OP_MICROSOFT_BIG_SSLV3_BUFFER
#if defined(SSL_OP_SAFARI_ECDHE_ECDSA_BUG)
      -- | Don't prefer ECDHE-ECDSA ciphers when the client appears to
      -- be Safari on OS X. OS X 10.8..10.8.3 has broken support for
      -- ECDHE-ECDSA ciphers.
    | SSL_OP_SAFARI_ECDHE_ECDSA_BUG
#endif
      -- | As of OpenSSL 1.1.0 this option has no effect.
    | SSL_OP_SSLEAY_080_CLIENT_DH_BUG
      -- | As of OpenSSL 1.1.0 this option has no effect.
    | SSL_OP_TLS_D5_BUG
      -- | As of OpenSSL 1.1.0 this option has no effect.
    | SSL_OP_TLS_BLOCK_PADDING_BUG
#if defined(SSL_OP_DONT_INSERT_EMPTY_FRAGMENTS)
      -- | Disables a countermeasure against a SSL 3.0/TLS 1.0
      -- protocol vulnerability affecting CBC ciphers, which cannot be
      -- handled by some broken SSL implementations. This option has
      -- no effect for connections using other ciphers.
    | SSL_OP_DONT_INSERT_EMPTY_FRAGMENTS
#endif
#if defined(SSL_OP_TLSEXT_PADDING)
      -- | Adds a padding extension to ensure the ClientHello size is
      -- never between 256 and 511 bytes in length. This is needed as
      -- a workaround for some implementations.
    | SSL_OP_TLSEXT_PADDING
#endif
      -- | Default set of options
    | SSL_OP_ALL
#if defined(SSL_OP_TLS_ROLLBACK_BUG)
      -- | Disable version rollback attack detection.
      --
      -- During the client key exchange, the client must send the same
      -- information about acceptable SSL/TLS protocol levels as
      -- during the first hello. Some clients violate this rule by
      -- adapting to the server's answer. (Example: the client sends a
      -- SSLv2 hello and accepts up to SSLv3.1=TLSv1, the server only
      -- understands up to SSLv3. In this case the client must still
      -- use the same SSLv3.1=TLSv1 announcement. Some clients step
      -- down to SSLv3 with respect to the server's answer and violate
      -- the version rollback protection.)
    | SSL_OP_TLS_ROLLBACK_BUG
#endif
      -- | As of OpenSSL 1.1.0 this option has no effect.
    | SSL_OP_SINGLE_DH_USE
      -- | As of OpenSSL 1.0.1k and 1.0.2, this option has no effect.
    | SSL_OP_EPHEMERAL_RSA
#if defined(SSL_OP_CIPHER_SERVER_PREFERENCE)
      -- | When choosing a cipher, use the server's preferences
      -- instead of the client preferences. When not set, the SSL
      -- server will always follow the clients preferences. When set,
      -- the SSLv3/TLSv1 server will choose following its own
      -- preferences. Because of the different protocol, for SSLv2 the
      -- server will send its list of preferences to the client and
      -- the client chooses.
    | SSL_OP_CIPHER_SERVER_PREFERENCE
#endif
      -- | As of OpenSSL 1.0.1 this option has no effect.
    | SSL_OP_PKCS1_CHECK_1
      -- | As of OpenSSL 1.0.1 this option has no effect.
    | SSL_OP_PKCS1_CHECK_2
      -- | As of OpenSSL 1.1.0 this option has no effect.
    | SSL_OP_NETSCAPE_CA_DN_BUG
      -- | As of OpenSSL 1.1.0 this option has no effect.
    | SSL_OP_NETSCAPE_DEMO_CIPHER_CHANGE_BUG
      -- | As of OpenSSL 1.1.0 this option has no effect.
    | SSL_OP_NO_SSLv2
      -- | Do not use the SSLv3 protocol.
      -- As of OpenSSL 1.1.0, this option is deprecated
    | SSL_OP_NO_SSLv3
      -- | Do not use the TLSv1 protocol.
      -- As of OpenSSL 1.1.0, this option is deprecated
    | SSL_OP_NO_TLSv1
      -- | Do not use the TLSv1.1 protocol.
      -- As of OpenSSL 1.1.0, this option is deprecated
    | SSL_OP_NO_TLSv1_1
      -- | Do not use the TLSv1.2 protocol.
      -- As of OpenSSL 1.1.0, this option is deprecated
    | SSL_OP_NO_TLSv1_2
      -- | Do not use the TLSv1.3 protocol.
      -- As of OpenSSL 1.1.0, this option is deprecated
    | SSL_OP_NO_TLSv1_3
      -- | Do not use the DTLSv1 protocol.
      -- As of OpenSSL 1.1.0, this option is deprecated
    | SSL_OP_NO_DTLSv1
      -- | Do not use the DTLSv1.2 protocol.
      -- As of OpenSSL 1.1.0, this option is deprecated
    | SSL_OP_NO_DTLSv1_2
#if defined(SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION)
      -- | When performing renegotiation as a server, always start a
      -- new session (i.e., session resumption requests are only
      -- accepted in the initial handshake). This option is not needed
      -- for clients.
    | SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION
#endif
      -- | Normally clients and servers will, where possible,
      -- transparently make use of
      -- <http://tools.ietf.org/html/rfc4507 RFC 4507> tickets for
      -- stateless session resumption.
      --
      -- If this option is set this functionality is disabled and
      -- tickets will not be used by clients or servers.
    | SSL_OP_NO_TICKET
#if defined(SSL_OP_ALLOW_UNSAFE_LEGACY_RENEGOTIATION)
      -- | Allow legacy insecure renegotiation between OpenSSL and
      -- unpatched clients or servers. See
      -- <https://www.openssl.org/docs/ssl/SSL_CTX_set_options.html#secure_renegotiation SECURE RENEGOTIATION>
      -- for more details.
    | SSL_OP_ALLOW_UNSAFE_LEGACY_RENEGOTIATION
#endif
#if defined(SSL_OP_LEGACY_SERVER_CONNECT)
      -- | Allow legacy insecure renegotiation between OpenSSL and
      -- unpatched servers _only_. See
      -- <https://www.openssl.org/docs/ssl/SSL_CTX_set_options.html#secure_renegotiation SECURE RENEGOTIATION>
      -- for more details.
    | SSL_OP_LEGACY_SERVER_CONNECT
#endif
#if defined(SSL_OP_NO_EXTENDED_MASTER_SECRET)
      -- | Disable Extended master secret.
      -- Only available on OpenSSL 3.0.0 and later.
    | SSL_OP_NO_EXTENDED_MASTER_SECRET
#endif
#if defined(SSL_OP_CLEANSE_PLAINTEXT)
      -- | Cleanse plaintext copies of data.
      -- Only available on OpenSSL 3.0.0 and later.
    | SSL_OP_CLEANSE_PLAINTEXT
#endif
#if defined(SSL_OP_ENABLE_KTLS)
      -- | Enble support for Kernel TLS
      -- Only available on OpenSSL 3.0.0 and later
    | SSL_OP_ENABLE_KTLS
#endif
#if defined(SSL_OP_IGNORE_UNEXPECTED_EOF)
    | SSL_OP_IGNORE_UNEXPECTED_EOF
#endif
#if defined(SSL_OP_ALLOW_CLIENT_RENEGOTIATION)
    | SSL_OP_ALLOW_CLIENT_RENEGOTIATION
#endif
#if defined(SSL_OP_DISABLE_TLSEXT_CA_NAMES)
    | SSL_OP_DISABLE_TLSEXT_CA_NAMES
#endif
    | SSL_OP_CISCO_ANYCONNECT
    | SSL_OP_NO_ANTI_REPLAY
    | SSL_OP_PRIORITIZE_CHACHA
    | SSL_OP_ALLOW_NO_DHE_KEX
    | SSL_OP_NO_ENCRYPT_THEN_MAC
    | SSL_OP_NO_QUERY_MTU
    | SSL_OP_COOKIE_EXCHANGE
    | SSL_OP_NO_COMPRESSION
    | SSL_OP_ENABLE_MIDDLEBOX_COMPAT
    | SSL_OP_NO_RENEGOTIATION
    | SSL_OP_CRYPTOPRO_TLSEXT_BUG
      deriving (Eq, Ord, Show, Typeable)

optionToIntegral :: Integral a => SSLOption -> a
optionToIntegral SSL_OP_MICROSOFT_SESS_ID_BUG                  = #const SSL_OP_MICROSOFT_SESS_ID_BUG
optionToIntegral SSL_OP_NETSCAPE_CHALLENGE_BUG                 = #const SSL_OP_NETSCAPE_CHALLENGE_BUG
optionToIntegral SSL_OP_NETSCAPE_REUSE_CIPHER_CHANGE_BUG       = #const SSL_OP_NETSCAPE_REUSE_CIPHER_CHANGE_BUG
optionToIntegral SSL_OP_SSLREF2_REUSE_CERT_TYPE_BUG            = #const SSL_OP_SSLREF2_REUSE_CERT_TYPE_BUG
optionToIntegral SSL_OP_MICROSOFT_BIG_SSLV3_BUFFER             = #const SSL_OP_MICROSOFT_BIG_SSLV3_BUFFER
#if defined(SSL_OP_SAFARI_ECDHE_ECDSA_BUG)
optionToIntegral SSL_OP_SAFARI_ECDHE_ECDSA_BUG                 = #const SSL_OP_SAFARI_ECDHE_ECDSA_BUG
#endif
optionToIntegral SSL_OP_SSLEAY_080_CLIENT_DH_BUG               = #const SSL_OP_SSLEAY_080_CLIENT_DH_BUG
optionToIntegral SSL_OP_TLS_D5_BUG                             = #const SSL_OP_TLS_D5_BUG
optionToIntegral SSL_OP_TLS_BLOCK_PADDING_BUG                  = #const SSL_OP_TLS_BLOCK_PADDING_BUG
#if defined(SSL_OP_DONT_INSERT_EMPTY_FRAGMENTS)
optionToIntegral SSL_OP_DONT_INSERT_EMPTY_FRAGMENTS            = #const SSL_OP_DONT_INSERT_EMPTY_FRAGMENTS
#endif
#if defined(SSL_OP_TLSEXT_PADDING)
optionToIntegral SSL_OP_TLSEXT_PADDING                         = #const SSL_OP_TLSEXT_PADDING
#endif
optionToIntegral SSL_OP_ALL                                    = #const SSL_OP_ALL
#if defined(SSL_OP_TLS_ROLLBACK_BUG)
optionToIntegral SSL_OP_TLS_ROLLBACK_BUG                       = #const SSL_OP_TLS_ROLLBACK_BUG
#endif
optionToIntegral SSL_OP_SINGLE_DH_USE                          = #const SSL_OP_SINGLE_DH_USE
optionToIntegral SSL_OP_EPHEMERAL_RSA                          = #const SSL_OP_EPHEMERAL_RSA
#if defined(SSL_OP_CIPHER_SERVER_PREFERENCE)
optionToIntegral SSL_OP_CIPHER_SERVER_PREFERENCE               = #const SSL_OP_CIPHER_SERVER_PREFERENCE
#endif
optionToIntegral SSL_OP_PKCS1_CHECK_1                          = #const SSL_OP_PKCS1_CHECK_1
optionToIntegral SSL_OP_PKCS1_CHECK_2                          = #const SSL_OP_PKCS1_CHECK_2
optionToIntegral SSL_OP_NETSCAPE_CA_DN_BUG                     = #const SSL_OP_NETSCAPE_CA_DN_BUG
optionToIntegral SSL_OP_NETSCAPE_DEMO_CIPHER_CHANGE_BUG        = #const SSL_OP_NETSCAPE_DEMO_CIPHER_CHANGE_BUG
optionToIntegral SSL_OP_NO_SSLv2                               = #const SSL_OP_NO_SSLv2
optionToIntegral SSL_OP_NO_SSLv3                               = #const SSL_OP_NO_SSLv3
optionToIntegral SSL_OP_NO_TLSv1                               = #const SSL_OP_NO_TLSv1
optionToIntegral SSL_OP_NO_TLSv1_1                             = #const SSL_OP_NO_TLSv1_1
optionToIntegral SSL_OP_NO_TLSv1_2                             = #const SSL_OP_NO_TLSv1_2
#if defined(SSL_OP_NO_TLSv1_3)
optionToIntegral SSL_OP_NO_TLSv1_3                             = #const SSL_OP_NO_TLSv1_3
#endif
#if defined(SSL_OP_NO_DTLSv1)
optionToIntegral SSL_OP_NO_DTLSv1                              = #const SSL_OP_NO_DTLSv1
#endif
#if defined(SSL_OP_NO_DTLSv1_2)
optionToIntegral SSL_OP_NO_DTLSv1_2                            = #const SSL_OP_NO_DTLSv1_2
#endif
#if defined(SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION)
optionToIntegral SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION = #const SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION
#endif
optionToIntegral SSL_OP_NO_TICKET                              = #const SSL_OP_NO_TICKET
#if defined(SSL_OP_ALLOW_UNSAFE_LEGACY_RENEGOTIATION)
optionToIntegral SSL_OP_ALLOW_UNSAFE_LEGACY_RENEGOTIATION      = #const SSL_OP_ALLOW_UNSAFE_LEGACY_RENEGOTIATION
#endif
#if defined(SSL_OP_LEGACY_SERVER_CONNECT)
optionToIntegral SSL_OP_LEGACY_SERVER_CONNECT                  = #const SSL_OP_LEGACY_SERVER_CONNECT
#endif
#if defined(SSL_OP_NO_EXTENDED_MASTER_SECRET)
optionToIntegral SSL_OP_NO_EXTENDED_MASTER_SECRET              = #const SSL_OP_NO_EXTENDED_MASTER_SECRET
#endif
#if defined(SSL_OP_CLEANSE_PLAINTEXT)
optionToIntegral SSL_OP_CLEANSE_PLAINTEXT                      = #const SSL_OP_CLEANSE_PLAINTEXT
#endif
#if defined(SSL_OP_ENABLE_KTLS)
optionToIntegral SSL_OP_ENABLE_KTLS                            = #const SSL_OP_ENABLE_KTLS
#endif
#if defined(SSL_OP_IGNORE_UNEXPECTED_EOF)
optionToIntegral SSL_OP_IGNORE_UNEXPECTED_EOF                  = #const SSL_OP_IGNORE_UNEXPECTED_EOF
#endif
#if defined(SSL_OP_ALLOW_CLIENT_RENEGOTIATION)
optionToIntegral SSL_OP_ALLOW_CLIENT_RENEGOTIATION             = #const SSL_OP_ALLOW_CLIENT_RENEGOTIATION
#endif
#if defined(SSL_OP_DISABLE_TLSEXT_CA_NAMES)
optionToIntegral SSL_OP_DISABLE_TLSEXT_CA_NAMES                = #const SSL_OP_DISABLE_TLSEXT_CA_NAMES
#endif
optionToIntegral SSL_OP_NO_ANTI_REPLAY                         = #const SSL_OP_NO_ANTI_REPLAY
optionToIntegral SSL_OP_PRIORITIZE_CHACHA                      = #const SSL_OP_PRIORITIZE_CHACHA
optionToIntegral SSL_OP_ENABLE_MIDDLEBOX_COMPAT                = #const SSL_OP_ENABLE_MIDDLEBOX_COMPAT
optionToIntegral SSL_OP_NO_ENCRYPT_THEN_MAC                    = #const SSL_OP_NO_ENCRYPT_THEN_MAC
optionToIntegral SSL_OP_ALLOW_NO_DHE_KEX                       = #const SSL_OP_ALLOW_NO_DHE_KEX
optionToIntegral SSL_OP_NO_QUERY_MTU                           = #const SSL_OP_NO_QUERY_MTU 
optionToIntegral SSL_OP_COOKIE_EXCHANGE                        = #const SSL_OP_COOKIE_EXCHANGE
optionToIntegral SSL_OP_NO_COMPRESSION                         = #const SSL_OP_NO_COMPRESSION
optionToIntegral SSL_OP_NO_RENEGOTIATION                       = #const SSL_OP_NO_RENEGOTIATION
optionToIntegral SSL_OP_CRYPTOPRO_TLSEXT_BUG                   = #const SSL_OP_CRYPTOPRO_TLSEXT_BUG
optionToIntegral SSL_OP_CISCO_ANYCONNECT                       = #const SSL_OP_CISCO_ANYCONNECT