module Language.Alloy.ResourceNames (
  alloyJarName,
  className,
  classPackage,
  commonsCliJarName,
  slf4jJarName,
  ) where

alloyJarName :: String
alloyJarName = "org.alloytools.alloy.dist.jar"

commonsCliJarName :: String
commonsCliJarName = "commons-cli-1.5.0.jar"

slf4jJarName :: String
slf4jJarName = "slf4j-simple-1.7.36.jar"

className :: String
className = "RunAlloy"

classPackage :: String
classPackage = "alloy"
