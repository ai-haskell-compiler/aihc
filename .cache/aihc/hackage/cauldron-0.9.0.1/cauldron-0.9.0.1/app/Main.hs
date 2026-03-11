{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLists #-}

-- | We have a bunch of datatypes, and a single recipe (constructor) for each
-- datatype. This means the wiring can be type-directed: we don't have to make a
-- decision at the term level about which constructor to use.
--
-- We wire the constructors in two ways: manually, and using a bit of dynamic
-- typing magic from the "Cauldron" module.
module Main where

import Cauldron

{-
  HERE ARE A BUNCH OF DATATYPES.

  The idea is that each datatype represents a bean, a component of our application.

  In a real application, these datatypes would be records containing effectful functions.

-}

data A = A deriving (Show)

data B = B deriving (Show)

data C = C deriving (Show)

data D = D deriving (Show)

data E = E deriving (Show)

data F = F deriving (Show)

data G = G deriving (Show)

data H = H deriving (Show)

data I = I deriving (Show)

data J = J deriving (Show)

data K = K deriving (Show)

data L = L deriving (Show)

data M = M deriving (Show)

data N = N deriving (Show)

data O = O deriving (Show)

data P = P deriving (Show)

data Q = Q deriving (Show)

data R = R deriving (Show)

data S = S deriving (Show)

data T = T deriving (Show)

data U = U deriving (Show)

data V = V deriving (Show)

data W = W deriving (Show)

data X = X deriving (Show)

data Y = Y deriving (Show)

data Z = Z deriving (Show)

{-
  These beans are a bit special: they are secondary "aggregate" beans which are optionally
  produced by the constructors of other beans.

  They have Monoid instances. The values returned by all the constructors that
  produce them will be combined.
-}

-- | A window into the internal state of some bean.
newtype Inspector = Inspector {inspect :: IO [String]}
  deriving newtype (Semigroup, Monoid)

-- | And initialization action which some beans might register.
newtype Initializer = Initializer {runInitializer :: IO ()}
  deriving newtype (Semigroup, Monoid)

{-
  HERE ARE A BUNCH OF CONSTRUCTORS AND DECORATORS.

-}
makeA :: A
makeA = A

-- A primary bean 'B' with an aggregate bean 'Inspector'.
--
-- aggregate beans can be used to implement some generic introspection mechanism
-- for an app, or perhaps some effectful action that sets up worker threads.
makeB :: (Inspector, B)
makeB = (Inspector (pure ["B stuff"]), B)

makeC :: C
makeC = C

makeD :: D
makeD = D

makeE :: A -> E
makeE = \_ -> E

makeF :: B -> C -> (Inspector, F)
makeF = \_ _ -> (Inspector (pure ["F stuff"]), F)

-- | A bean with a self-dependency!
--
-- We need these self-dependencies in order for self-invocations to be decorated.
--
-- The 'Fire' we use to 'cook' the 'Cauldron' might allow or disallow self-dependencies.
makeG :: E -> F -> G -> G
makeG _ _ _ = G

-- | A decorator.
--
-- Decorators are basically normal constructors, only that they take
-- the bean they return as a parameter.
--
-- This is not the same as a bean self-dependency! These receive the completed
-- bean from the future, while decorators work with the in-construction version
-- of the bean.
--
-- Because they are normal constructors, they can be effectful, and they
-- might have dependencies of their own.
makeGDeco1 :: A -> G -> G
makeGDeco1 _ g = g

-- | A primary bean 'H' with two aggregate beans 'Initializer' and 'Inspector'.
makeH :: A -> D -> G -> (Initializer, Inspector, H)
makeH _ _ _ = (Initializer (putStrLn "H init"), Inspector (pure ["H stuff"]), H)

-- | Notice that this bean has "Inspector" as a dependency. Inspector is an
-- aggregate bean; its value is aggregated across all the constructor that
-- produce it.
--
-- Why would a bean depend on an aggregate bean? Well, for example, a server
-- bean might want to publish diagnostic information (exposed using an uniform
-- interface) that is collected from the constructors that register it.
makeZ :: Inspector -> D -> H -> Z
makeZ _ _ _ = Z

makeZDeco1 :: B -> E -> Z -> Z
makeZDeco1 _ _ z = z

-- | A decorator for 'Z' which produces an aggregate bean 'Initializer'.
makeZDeco2 :: (F -> Z -> (Initializer, Z))
makeZDeco2 = \_ z -> (Initializer (putStrLn "Z deco init"), z)

data Result = Result Initializer Inspector Z

boringWiring :: IO Result
boringWiring = do
  let -- We have to remember to collect the monoidal registrations.
      initializer = init1 <> init2
      -- We have to remember to collect the monoidal registrations.
      inspector = inspector1 <> inspector2 <> inspector3
      -- Now let's tie the constructors together.
      a = makeA
      (inspector1, b) = makeB
      c = makeC
      d = makeD
      e = makeE a
      (inspector2, f) = makeF b c
      g0 = makeG e f g
      g1 = makeGDeco1 a g0
      g = g1
      -- Here we apply a single decorator.
      (init1, inspector3, h) = makeH a d g
      -- Compose the decorators before applying them.
      z0 = makeZ inspector d h
      z1 = makeZDeco1 b e z0
      (init2, z2) = makeZDeco2 f z1
      z = z2
  pure $ Result initializer inspector z

-- | Here we don't have to worry about positional parameters. We throw all the
-- constructors into the 'Cauldron' and taste the bean values at the end, plus a
-- graph we may want to draw.
--
-- Note that we detect wiring errors *before* running the effectful constructors.
coolWiring :: Either CookingError (IO Result)
coolWiring = cook allowSelfDeps cauldron

cauldron :: Cauldron IO
cauldron :: Cauldron IO =
    [ recipe @A $ val $ pure makeA,
      recipe @B $ val $ pure makeB,
      recipe @C $ val $ wire makeC,
      recipe @D $ val $ wire makeD,
      recipe @E $ val $ wire makeE,
      recipe @F $ val $ wire makeF,
      recipe @G $
        Recipe
          { bare = val $ wire makeG,
            decos =
              [ val $ wire makeGDeco1
              ]
          },
      recipe @H $ val $ wire makeH,
      recipe @Z
        Recipe
          { bare = val $ wire makeZ,
            decos =
              [ val $ wire makeZDeco1,
                val $ wire makeZDeco2
              ]
          },
      recipe @Result $ val $ wire Result
    ]

main :: IO ()
main = do
  -- "manual" wiring
  do
    Result (Initializer {runInitializer}) (Inspector {inspect}) z <- boringWiring
    inspection <- inspect
    print inspection
    print z
    runInitializer
  -- wiring with Cauldron
  merr <- case coolWiring of
    Left badBeans -> do
      putStrLn $ prettyCookingError badBeans
      pure $ Just badBeans
    Right action -> do
      Result (Initializer {runInitializer}) (Inspector {inspect}) z <- action
      inspection <- inspect
      print inspection
      print z
      runInitializer
      pure $ Nothing
  let depGraph = getDependencyGraph [cauldron]
  writeAsDot (defaultStyle merr) "beans.dot" depGraph
  writeAsDot (defaultStyle merr) "beans-no-agg.dot" $ removeAggregates $ depGraph
  writeAsDot (defaultStyle merr) "beans-no-agg-no-decos.dot" $ removeDecos $ removeAggregates $ depGraph
  writeAsDot (defaultStyle merr) "beans-simple.dot" $ collapseBeans $ removeDecos $ removeAggregates $ depGraph
  writeAsDot (defaultStyle merr) "beans-simple-with-decos.dot" $ collapseBeans $ removeAggregates $ depGraph
