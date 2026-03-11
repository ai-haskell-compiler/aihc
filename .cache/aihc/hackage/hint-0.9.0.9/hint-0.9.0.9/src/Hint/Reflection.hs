module Hint.Reflection (
      ModuleElem(..), Id, name, children,
      getModuleExports,
) where

import Data.List
import Data.Maybe

import Hint.Base
import qualified Hint.GHC as GHC

-- | An Id for a class, a type constructor, a data constructor, a binding, etc
type Id = String

data ModuleElem = Fun Id | Class Id [Id] | Data Id [Id]
  deriving (Read, Show, Eq)

name :: ModuleElem -> Id
name (Fun f)     = f
name (Class c _) = c
name (Data d _)  = d

children :: ModuleElem -> [Id]
children (Fun   _)     = []
children (Class _ ms)  = ms
children (Data  _ dcs) = dcs

-- | Gets an abstract representation of all the entities exported by the module.
--   It is similar to the @:browse@ command in GHCi.
getModuleExports :: MonadInterpreter m => ModuleName -> m [ModuleElem]
getModuleExports mn =
    do module_  <- findModule mn
       mod_info <- mayFail $ runGhc $ GHC.getModuleInfo module_
       exports  <- mapM (\n -> runGhc $ GHC.lookupName n) (GHC.modInfoExports mod_info)
       dflags   <- runGhc GHC.getSessionDynFlags
       --
       return $ asModElemList dflags (catMaybes exports)

asModElemList :: GHC.DynFlags -> [GHC.TyThing] -> [ModuleElem]
asModElemList df xs = concat [
                        cs,
                        ts,
                        ds \\ concatMap (map Fun . children) ts,
                        fs \\ concatMap (map Fun . children) cs
                      ]
    where cs = [Class (getUnqualName df tc) (filter (alsoIn fs) $ getUnqualName df <$> GHC.classMethods c)
               | GHC.ATyCon tc <- xs, Just c  <- [GHC.tyConClass_maybe tc]]
          ts = [Data  (getUnqualName df tc) (filter (alsoIn ds) $ getUnqualName df <$> GHC.tyConDataCons tc)
               | GHC.ATyCon tc <- xs, Nothing <- [GHC.tyConClass_maybe tc]]
          ds = [Fun $ getUnqualName df dc | GHC.AConLike (GHC.RealDataCon dc) <- xs]
          fs = [Fun $ getUnqualName df f  | GHC.AnId f                        <- xs]
          alsoIn es = (`elem` map name es)

getUnqualName :: GHC.NamedThing a => GHC.DynFlags -> a -> String
getUnqualName dfs = GHC.showSDoc dfs . GHC.pprParenSymName
