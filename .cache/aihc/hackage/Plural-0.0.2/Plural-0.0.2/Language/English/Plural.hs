module Language.English.Plural where

import Data.Map (Map)
import Text.Regex.TDFA

import qualified Data.Map as M

-- | Get plural format.
plural :: String -> String
plural xs = do
  let ys = M.filterWithKey (\k _ -> xs =~ k :: Bool) pluralKnowledge
  case M.minViewWithKey ys of
    -- Get plural format if found match.
    Just ((k, a), _) -> 
        do
          let (before, match, _) = xs =~ k :: (String, String, String)
          if a =~ "^\\\\&" :: Bool 
            -- Plural mix `match` part and special suffix if `a` match \\&
            -- At this, \\& is speical regex for replace `match` part
            -- Example, word `day` with `("[aeiou]y", "\\&s")`, `match` part
            -- is `ay`, then `\\&` replace `ay`, and result is `days`.
            then do
              let (_, _, s) = a =~ "^\\\\&" :: (String,String,String)
              before ++ match ++ s
            -- Otherwise plural is suffix string.
            else before ++ a
    -- Otherwise return origianl one.
    Nothing -> xs

-- | Try get plural format of word with number.
tryPlural :: Int -> String -> String
tryPlural n s
    | n `elem` [0, 1] = s
    | otherwise       = plural s

-- | The knowledge for plural process.
pluralKnowledge :: Map String String
pluralKnowledge = 
    M.fromAscList
         [("ss$", "sses")
         ,("zz$", "zzes")       -- Example: "buzzes".
         ,("sh$", "shes")
         ,("tch$", "tches")
         ,("eaf$", "eaves")
         ,("ief$", "ieves")     -- Example: "thieves".
         ,("roof$", "roofs")
         ,("oof$", "ooves")
         ,("ife$", "ives")
         ,("lf$", "lves")
         ,("[aeiou]y$", "\\&s") -- \\& for replace match string
         ,("ndum$", "nda")      -- Example: "addendum".
         ,("um$", "a")          -- Example: "media", "criteria", "symposia",
                                -- "crania", curriculum", "data".
         ,("^die$", "dice")
         ,("dogma$", "dogmas")   -- exception to -ma rule.
         ,("lemma$", "lemmas")   -- exception to -ma rule.
         ,("schema$", "schemas") -- exception to -ma rule.
         ,("ia$", "ium")         -- Example: "bacteria".
         ,("ma$", "mata")        -- Example: "stigma".
         ,("na$", "nae")         -- Example: "antenna".
         ,("ta$", "tum")         -- Example: "strata".
         ,("Atlas$", "Atlantes") -- Case-sensitive
         ,("atlas$", "atlases")
         ,("Harry$", "Harrys")  -- Case-sensitive
         ,("aircraft$", "aircraft")
         ,("alga$", "algae")
         ,("alumna$", "alumnae")
         ,("alumnus$", "alumni")
         ,("ameoba$", "ameobae")
         ,("automaton$", "automata")
         ,("bacillus$", "bacilli")
         ,("banjo$", "banjos")
         ,("beau$", "beaux")
         ,("cactus$", "cacti")  -- Or "cactuses".
         ,("cannon$", "cannon") -- Or "cannons".
         ,("canto$", "cantos")
         ,("cargo$", "cargos")
         ,("cattle$", "cattle")
         ,("child$", "children")
         ,("cod$", "cod")
         ,("corpus$", "corpora")
         ,("dwarf$", "dwarves")
         ,("cs$", "csen")       -- Example: "emacsen".
         ,("foot$", "feet")
         ,("formula$", "formulae")
         ,("graffito$", "graffiti")
         ,("rion$", "ria")      -- Example: "criteria".
         ,("deer$", "deer")
         ,("focus$", "foci")
         ,("genus$", "genera")
         ,("goose$", "geese")
         ,("hedron$", "hedra")  -- Example: "polyhedron".
         ,("hippopotamus$", "hippopotami")
         --    ("index$", "indices") -- "indexes" is also acceptable.
         ,("insigne$", "insignia")
         ,("life$", "lives")
         ,("louse$", "lice")
         ,("mackerel$", "mackerel")
         ,("man$", "men")
         ,("matrix$", "matrices")
         ,("moose$", "moose")
         ,("motto$", "mottos")
         ,("mouse$", "mice")
         ,("nucleus$", "nuclei")
         ,("octopus$", "octopi") -- Or "octopuses".
         ,("offspring", "offspring")
         ,("opus$", "opera")
         ,("\\box$", "oxen")
         ,("panino$", "panini")
         ,("paparazzo$", "paparazzi")
         ,("phalanx$", "phalanges")
         ,("phenomenon$", "phenomena")
         ,("people$", "people")
         ,("perch$", "perch") -- Can certain uses of "perch" be plural?
         ,("piano$", "pianos")
         ,("police$", "police")
         ,("portico$", "porticos")
         ,("quarto$", "quartos")
         ,("radius$", "radii")
         ,("rhinoceros$", "rhinoceri") -- Or "rhinoceroses".
                                       --    ("series$", "series") -- Already has an "s".
         ,("sheep$", "sheep")
         --    ("species$", "species") -- Already has an "s".
         ,("solo$", "solos")
         ,("syllabus$", "syllabi")
         ,("terminus$", "termini")
         ,("ulus$", "uli")      -- Example: "stimuli".
         ,("trout$", "trout")
         ,("tooth$", "teeth")
         ,("uterus$", "uteri")  -- Or "uteruses".
         ,("virtuoso", "virtuosi")
         ,("viscus$", "viscera")
         --    ("woman$", "women") -- See "man$".
         --    ("e$", "es") -- Fall-through to "[^s]$".
         ,("is$", "es")         -- Example: "axes", "crises", "testes".
         ,("us$", "uses")       -- Example: "campuses", "platypuses", "prospectuses".
         ,("io$", "ios")
         ,("oo$", "oos")
         ,("o$", "oes")
         ,("y$", "ies")
         ,("[ei]x$", "ices")    -- Example: "vertices".
         ,("x$", "xes")
         ,("[^s]$", "\\&s")]    -- \\& for replace match string