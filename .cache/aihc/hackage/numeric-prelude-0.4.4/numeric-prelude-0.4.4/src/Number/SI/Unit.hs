{-# LANGUAGE RebindableSyntax #-}
{- |
Special physical units: SI unit system
-}

module Number.SI.Unit where

import qualified Algebra.Transcendental      as Trans
import qualified Algebra.Field               as Field

import qualified Number.Physical.Unit         as Unit
import qualified Number.Physical.UnitDatabase as UnitDatabase
import Number.Physical.UnitDatabase(initScale, initUnitSet)
import Data.Maybe(catMaybes)

import NumericPrelude.Base hiding (length)
import NumericPrelude.Numeric hiding (one)

data Dimension =
   Length | Time | Mass | Charge |
   Angle | Temperature | Information
      deriving (Eq, Ord, Enum, Show)


-- | Some common quantity classes.
angle, angularSpeed, -- needs explicit signature because it does not occur in the database
 length, distance, area, volume, time,
 frequency, speed, acceleration, mass,
 force, pressure, energy, power,
 charge, current, voltage, resistance,
 capacitance, temperature,
 information, dataRate
  :: Unit.T Dimension

length       = Unit.fromVector [ 1, 0, 0, 0, 0, 0, 0]
-- synonym for 'length' which is distinct from List.length
distance     = Unit.fromVector [ 1, 0, 0, 0, 0, 0, 0]
area         = Unit.fromVector [ 2, 0, 0, 0, 0, 0, 0]
volume       = Unit.fromVector [ 3, 0, 0, 0, 0, 0, 0]
time         = Unit.fromVector [ 0, 1, 0, 0, 0, 0, 0]
frequency    = Unit.fromVector [ 0,-1, 0, 0, 0, 0, 0]
speed        = Unit.fromVector [ 1,-1, 0, 0, 0, 0, 0]
acceleration = Unit.fromVector [ 1,-2, 0, 0, 0, 0, 0]
mass         = Unit.fromVector [ 0, 0, 1, 0, 0, 0, 0]
force        = Unit.fromVector [ 1,-2, 1, 0, 0, 0, 0]
pressure     = Unit.fromVector [-1,-2, 1, 0, 0, 0, 0]
energy       = Unit.fromVector [ 2,-2, 1, 0, 0, 0, 0]
power        = Unit.fromVector [ 2,-3, 1, 0, 0, 0, 0]
charge       = Unit.fromVector [ 0, 0, 0, 1, 0, 0, 0]
current      = Unit.fromVector [ 0,-1, 0, 1, 0, 0, 0]
voltage      = Unit.fromVector [ 2,-2, 1,-1, 0, 0, 0]
resistance   = Unit.fromVector [ 2,-1, 1,-2, 0, 0, 0]
capacitance  = Unit.fromVector [-2, 2,-1, 2, 0, 0, 0]
angle        = Unit.fromVector [ 0, 0, 0, 0, 1, 0, 0]
angularSpeed = Unit.fromVector [ 0,-1, 0, 0, 1, 0, 0]
temperature  = Unit.fromVector [ 0, 0, 0, 0, 0, 1, 0]
information  = Unit.fromVector [ 0, 0, 0, 0, 0, 0, 1]
dataRate     = Unit.fromVector [ 0,-1, 0, 0, 0, 0, 1]


percent, fourth, half, threeFourth   :: Field.C a => a

secondsPerMinute, secondsPerHour, secondsPerDay, secondsPerYear, 
 meterPerInch, meterPerFoot, meterPerYard,
 meterPerAstronomicUnit, meterPerParsec, 
 accelerationOfEarthGravity,
 k2, deg180, grad200, bytesize       :: Field.C a => a

radPerDeg, radPerGrad                :: Trans.C a => a

mach, speedOfLight, electronVolt,
 calorien, horsePower                :: Field.C a => a

yocto, zepto, atto, femto, pico,
 nano, micro, milli, centi, deci,
 one, deca, hecto, kilo, mega,
 giga, tera, peta, exa, zetta, yotta :: Field.C a => a

-- | Common constants
percent      = 0.01
fourth       = 0.25
half         = 0.50
threeFourth  = 0.75

-- | Conversion factors
secondsPerMinute = 60
secondsPerHour   = 60*secondsPerMinute
secondsPerDay    = 24*secondsPerHour  -- 86400.0
secondsPerYear   = 365.2422*secondsPerDay

meterPerInch           = 0.0254
meterPerFoot           = 0.3048
meterPerYard           = 0.9144
meterPerAstronomicUnit = 149.6e6
meterPerParsec         = 30.857e12

k2           = 1024
deg180       = 180
grad200      = 200
radPerDeg    = pi/deg180;
radPerGrad   = pi/grad200;
bytesize     = 8



-- | Physical constants
accelerationOfEarthGravity = 9.80665
mach                       = 332.0
speedOfLight               = 299792458.0
electronVolt               = 1.602e-19
calorien                   = 4.19
horsePower                 = 736.0

-- | Prefixes used for SI units
yocto = 1.0e-24
zepto = 1.0e-21
atto  = 1.0e-18
femto = 1.0e-15
pico  = 1.0e-12
nano  = 1.0e-9
micro = 1.0e-6
milli = 1.0e-3
centi = 1.0e-2
deci  = 1.0e-1
one   = 1.0e0
deca  = 1.0e1
hecto = 1.0e2
kilo  = 1.0e3
mega  = 1.0e6
giga  = 1.0e9
tera  = 1.0e12
peta  = 1.0e15
exa   = 1.0e18
zetta = 1.0e21
yotta = 1.0e24



{- | UnitDatabase.T of units and their common scalings -}
databaseRead, databaseShow :: Trans.C a => UnitDatabase.T Dimension a
databaseRead = map UnitDatabase.createUnitSet database
databaseShow =
   map UnitDatabase.createUnitSet $
      catMaybes $ map UnitDatabase.showableUnit database

database :: Trans.C a => [UnitDatabase.InitUnitSet Dimension a]
database = [
    (initUnitSet Unit.scalar False [
      (initScale "pi"    pi                        False False),
      (initScale "e"     (exp 1)                   False False),
      (initScale "i"     (sqrt (-1))               False False),
      (initScale "%"     percent                   False False),
      (initScale "\188"  fourth                    False False),
      (initScale "\189"  half                      False False),
      (initScale "\190"  threeFourth               False False)
    ]),
    (initUnitSet angle False [
      (initScale "''"    (radPerDeg/secondsPerHour)   True  False),
      (initScale "'"     (radPerDeg/secondsPerMinute) True  False),
      (initScale "grad"  radPerGrad                False False),
      (initScale "\176"  radPerDeg                 True  True ),
      (initScale "rad"   one                       False False)
    ]),
    (initUnitSet frequency True [
      (initScale "bpm"   (one/secondsPerMinute)    False False),
      (initScale "Hz"    one                       True  True ),
      (initScale "kHz"   kilo                      True  False),
      (initScale "MHz"   mega                      True  False),
      (initScale "GHz"   giga                      True  False)
    ]),
    (initUnitSet time False [
      (initScale "ns"    nano                      True  False),
      (initScale "\181s" micro                     True  False),
      (initScale "ms"    milli                     True  False),
      (initScale "s"     one                       True  True ),
      (initScale "min"   secondsPerMinute          True  False),
      (initScale "h"     secondsPerHour            True  False),
      (initScale "d"     secondsPerDay             True  False),
      (initScale "a"     secondsPerYear            True  False)
    ]),
--    (initUnitSet distance False [
    (initUnitSet length False [
      (initScale "nm"    nano                      True  False),
      (initScale "\181m" micro                     True  False),
      (initScale "mm"    milli                     True  False),
      (initScale "cm"    centi                     True  False),
      (initScale "dm"    deci                      True  False),
      (initScale "m"     one                       True  True ),
      (initScale "km"    kilo                      True  False)
    ]),
    (initUnitSet area False [
      (initScale "ha"    (hecto*hecto)             False False)
    ]),
    (initUnitSet volume False [
      (initScale "ml"    (milli*milli)             False False),
      (initScale "cl"    (milli*centi)             False False),
      (initScale "l"     milli                     False False)
    ]),
    (initUnitSet speed False [
      (initScale "mach"  mach                      False False),
      (initScale "c"     speedOfLight              False False)
    ]),
    (initUnitSet acceleration False [
      (initScale "G"     accelerationOfEarthGravity False False)
    ]),
    (initUnitSet mass False [
      (initScale "\181g" nano                      True  False),
      (initScale "mg"    micro                     True  False),
      (initScale "g"     milli                     True  False),
      (initScale "kg"    one                       True  True ),
      (initScale "dt"    hecto                     True  False),
      (initScale "t"     kilo                      True  False),
      (initScale "kt"    mega                      True  False)
    ]),
    (initUnitSet force False [
      (initScale "N"     one                       True  True ),
      (initScale "kp"    accelerationOfEarthGravity False False),
      (initScale "kN"    kilo                      True  False)
    ]),
    (initUnitSet pressure False [
      (initScale "Pa"    one                       True  True ),
      (initScale "mbar"  hecto                     False False),
      (initScale "kPa"   kilo                      True  False),
      (initScale "bar"   (hecto*kilo)              False False)
    ]),
    (initUnitSet energy False [
      (initScale "eV"    electronVolt              False False),
      (initScale "J"     one                       True  True ),
      (initScale "cal"   calorien                  False False),
      (initScale "kJ"    kilo                      True  False),
      (initScale "kcal"  (kilo*calorien)           False False),
      (initScale "MJ"    mega                      True  False)
    ]),
    (initUnitSet power False [
      (initScale "mW"    milli                     True  False),
      (initScale "W"     one                       True  True ),
      (initScale "HP"    horsePower                False False),
      (initScale "kW"    kilo                      True  False),
      (initScale "MW"    mega                      True  False)
    ]),
    (initUnitSet charge False [
      (initScale "C"     one                       True  True )
    ]),
    (initUnitSet current False [
      (initScale "\181A" micro                     True  False),
      (initScale "mA"    milli                     True  False),
      (initScale "A"     one                       True  True )
    ]),
    (initUnitSet voltage False [
      (initScale "mV"    milli                     True  False),
      (initScale "V"     one                       True  True ),
      (initScale "kV"    kilo                      True  False),
      (initScale "MV"    mega                      True  False),
      (initScale "GV"    giga                      True  False)
    ]),
    (initUnitSet resistance False [
      (initScale "Ohm"   one                       True  True ),
      (initScale "kOhm"  kilo                      True  False),
      (initScale "MOhm"  mega                      True  False)
    ]),
    (initUnitSet capacitance False [
      (initScale "pF"    pico                      True  False),
      (initScale "nF"    nano                      True  False),
      (initScale "\181F" micro                     True  False),
      (initScale "mF"    milli                     True  False),
      (initScale "F"     one                       True  True )
    ]),
    (initUnitSet temperature False [
      (initScale "K"     one                       True  True )
    ]),
    (initUnitSet information False [
      (initScale "bit"   one                       True  True ),
      (initScale "B"     bytesize                  True  False),
      (initScale "kB"    (kilo*bytesize)           False False),
      (initScale "KB"    (k2*bytesize)             True  False),
      (initScale "MB"    (k2*k2*bytesize)          True  False),
      (initScale "GB"    (k2*k2*k2*bytesize)       True  False)
    ]),
    (initUnitSet dataRate True [
      (initScale "baud"  one                       True  True ),
      (initScale "kbaud" kilo                      False False),
      (initScale "Kbaud" k2                        True  False),
      (initScale "Mbaud" (k2*k2)                   True  False),
      (initScale "Gbaud" (k2*k2*k2)                True  False)
    ])
  ]
