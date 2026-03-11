{-# LANGUAGE RebindableSyntax #-}
{- |
Special physical units: SI unit system
-}

module Number.DimensionTerm.SI (
    second, minute, hour, day, year,
    hertz,
    meter,
    -- liter,
    gramm, tonne,
    -- newton,
    -- pascal,
    -- bar,
    -- joule,
    -- watt,
    coulomb,
    -- ampere,
    volt,
    -- ohm,
    -- farad,
    kelvin,
    bit, byte,
    -- baud,

    inch, foot, yard, astronomicUnit, parsec,

    SI.yocto, SI.zepto, SI.atto,  SI.femto, SI.pico, SI.nano,
    SI.micro, SI.milli, SI.centi, SI.deci,  SI.one,  SI.deca,
    SI.hecto, SI.kilo,  SI.mega,  SI.giga,  SI.tera, SI.peta,
    SI.exa,   SI.zetta, SI.yotta,
    ) where

import qualified Algebra.Field               as Field

import qualified Number.DimensionTerm  as DN
import qualified Number.SI.Unit as SI

-- aimport NumericPrelude.Base hiding (length)
import NumericPrelude.Numeric hiding (one)


second  :: Field.C a => DN.Time        a
second  = DN.time        1e+0
minute  :: Field.C a => DN.Time        a
minute  = DN.time        SI.secondsPerMinute
hour    :: Field.C a => DN.Time        a
hour    = DN.time        SI.secondsPerHour
day     :: Field.C a => DN.Time        a
day     = DN.time        SI.secondsPerDay
year    :: Field.C a => DN.Time        a
year    = DN.time        SI.secondsPerYear
hertz   :: Field.C a => DN.Frequency a
hertz   = DN.frequency   1e+0
meter   :: Field.C a => DN.Length      a
meter   = DN.length      1e+0
-- liter   :: Field.C a => DN.Volume      a
-- liter   = DN.volume      1e-3
gramm   :: Field.C a => DN.Mass        a
gramm   = DN.mass        1e-3
tonne   :: Field.C a => DN.Mass        a
tonne   = DN.mass        1e+3
-- newton  :: Field.C a => DN.Force       a
-- newton  = DN.force       1e+0
-- pascal  :: Field.C a => DN.Pressure    a
-- pascal  = DN.pressure    1e+0
-- bar     :: Field.C a => DN.Pressure    a
-- bar     = DN.pressure    1e+5
-- joule   :: Field.C a => DN.Energy      a
-- joule   = DN.energy      1e+0
-- watt    :: Field.C a => DN.Power       a
-- watt    = DN.power       1e+0
coulomb :: Field.C a => DN.Charge      a
coulomb = DN.charge      1e+0
-- ampere  :: Field.C a => DN.Current     a
-- ampere  = DN.current     1e+0
volt    :: Field.C a => DN.Voltage     a
volt    = DN.voltage     1e+0
-- ohm     :: Field.C a => DN.Resistance  a
-- ohm     = DN.resistance  1e+0
-- farad   :: Field.C a => DN.Capacitance a
-- farad   = DN.capacitance 1e+0
kelvin  :: Field.C a => DN.Temperature a
kelvin  = DN.temperature 1e+0
bit     :: Field.C a => DN.Information a
bit     = DN.information 1e+0
byte    :: Field.C a => DN.Information a
byte    = DN.information SI.bytesize
-- baud    :: Field.C a => DN.DataRate    a
-- baud    = DN.dataRate    1e+0

inch, foot, yard, astronomicUnit, parsec
   :: Field.C a => DN.Length a

inch           = DN.length SI.meterPerInch
foot           = DN.length SI.meterPerFoot
yard           = DN.length SI.meterPerYard
astronomicUnit = DN.length SI.meterPerAstronomicUnit
parsec         = DN.length SI.meterPerParsec

{-
accelerationOfEarthGravity :: Field.C a => DN.Acceleration    a
accelerationOfEarthGravity = DN.acceleration SI.accelerationOfEarthGravity

mach         :: Field.C a => DN.Speed a
speedOfLight :: Field.C a => DN.Speed a
electronVolt :: Field.C a => DN.Energy a
calorien     :: Field.C a => DN.Energy a
horsePower   :: Field.C a => DN.Power a

mach         = DN.speed        SI.mach
speedOfLight = DN.speed        SI.speedOfLight
electronVolt = DN.energy       SI.electronVolt
calorien     = DN.energy       SI.calorien
horsePower   = DN.power        SI.horsePower
-}
