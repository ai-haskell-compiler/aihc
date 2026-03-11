{-# LANGUAGE OverloadedStrings, CPP #-}
import Test.Hspec
import Database.PostgreSQL.Simple.Options

main :: IO ()
main = hspec $ describe "connection string parser" $ do
  it "fails on empty" $ parseConnectionString "" `shouldBe` Left "MalformedScheme NonAlphaLeading"

  it "parses a single keyword" $ parseConnectionString "host=localhost"
    `shouldBe` Right (mempty { host = return "localhost" })

  it "parses all keywords" $ parseConnectionString
    "host=localhost port=1234 user=jonathan password=open dbname=dev" `shouldBe`
      Right (mempty
        { host = return "localhost"
        , port = return 1234
        , user = return "jonathan"
        , password = return "open"
        , dbname = return "dev"
        })

  it "parses host only connection string" $ parseConnectionString "postgresql://localhost"
    `shouldBe` Right (mempty { host = return "localhost" })

  it "parses all params" $ parseConnectionString "postgresql://jonathan:open@localhost:1234/dev"
    `shouldBe` Right (mempty
        { host = return "localhost"
        , port = return 1234
        , user = return "jonathan"
        , password = return "open"
        , dbname = return "dev"
        })

  it "parses all params using query params" $ parseConnectionString "postgresql:///dev?host=localhost&port=1234"
    `shouldBe` Right (mempty
      { host = return "localhost"
      , port = return 1234
      , dbname = return "dev"
      })

  it "decodes a unix port" $ parseConnectionString "postgresql://%2Fvar%2Flib%2Fpostgresql/dbname"
    `shouldBe` Right (mempty
        { host = return "/var/lib/postgresql"
        , dbname = return "dbname"
        })

  it "accepts postgres" $ parseConnectionString "postgres://localhost"
    `shouldBe` Right (mempty { host = return "localhost" })
