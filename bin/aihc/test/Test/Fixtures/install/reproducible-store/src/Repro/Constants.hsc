module Repro.Constants (configured) where

#include "ReproConfig.h"

configured :: Int
configured = #const REPRO_CONFIGURED
