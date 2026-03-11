module Hinfo.Options.Internal.Types where

import Options.Applicative

data AppOption = Name
                | Version
                | Github
                | Author
                | Maintainer
                | Copyright
                | ExtraSourceFiles
                | Synopsis
                | Category
                | Description
                | Dependencies


appOpts :: ParserInfo AppOption
appOpts = info(appOptionParser <**> helper)
  ( fullDesc
  <> progDesc "Report Information On A Haskell App"
  <> header "hinfo - information on haskell apps")

appOptionParser :: Parser AppOption
appOptionParser = nameInput 
  <|> versionInput
  <|> githubInput
  <|> authorInput
  <|> maintainerInput
  <|> copyrightInput
  <|> extraSourceFilesInput
  <|> synopsisInput
  <|> categoryInput
  <|> descriptionInput
  <|> dependenciesInput

nameInput :: Parser AppOption
nameInput = flag' Name
  ( long "name" <> short 'n' <> help "Report Application Name")

versionInput :: Parser AppOption
versionInput = flag' Version
  ( long "version" <> short 'v' <> help "Report Application Version")

githubInput :: Parser AppOption
githubInput = flag' Github
  (long "github" <> help "Report Github Repository")


authorInput :: Parser AppOption
authorInput = flag' Author
  (long "author" <> help "Report Author")

maintainerInput :: Parser AppOption
maintainerInput = flag' Maintainer
  (long "maintainer" <> help "Report Maintainer")

copyrightInput :: Parser AppOption
copyrightInput = flag' Copyright
  (long "copyright" <> help "Report Copyright Holder")

extraSourceFilesInput :: Parser AppOption
extraSourceFilesInput = flag' ExtraSourceFiles
  (long "extra-source-files" <> help "Report Extra Source Files")

synopsisInput :: Parser AppOption
synopsisInput = flag' Synopsis
  (long "synopsis" <> help "Report Synopsis")

categoryInput :: Parser AppOption
categoryInput = flag' Category
  (long "category" <> help "Report Category")

descriptionInput :: Parser AppOption
descriptionInput = flag' Description
  (long "description" <> help "Report Application Description")

dependenciesInput :: Parser AppOption
dependenciesInput = flag' Dependencies
  (long "dependencies" <> help "Report Dependencies")