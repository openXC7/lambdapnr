{- | Static timing analysis types.

Mirror of the timing-related declarations in nextpnr's
@common\/kernel\/nextpnr_types.h@ (the @TimingPortClass@ enum,
@ClockEdge@ and @TimingClockingInfo@). The analyser itself
(@TimingAnalyser@, @common\/kernel\/timing.cc@) arrives with the
placed-netlist milestone; these types are the Arch API surface the
cell-timing queries return.
-}
module Lambdapnr.Kernel.Timing (
    TimingPortClass (..),
    ClockEdge (..),
    TimingClockingInfo (..),
) where

import Lambdapnr.Kernel.Delay (DelayPair, DelayQuad)
import Lambdapnr.Kernel.IdString (IdString)

{- | Port timing class, matching the C++ @TimingPortClass@ enum order
exactly (it is part of the checksum serialization and the STA
classification).
-}
data TimingPortClass
    = TmgClockInput -- ^ Clock input to a sequential cell
    | TmgGenClock -- ^ Generated clock output (PLL, DCC, etc)
    | TmgRegisterInput -- ^ Input to a register, with an associated clock
    | TmgRegisterOutput -- ^ Output from a register
    | TmgCombInput -- ^ Combinational input, no paths end here
    | TmgCombOutput -- ^ Combinational output, no paths start here
    | TmgStartpoint -- ^ Unclocked primary startpoint (IO cell output)
    | TmgEndpoint -- ^ Unclocked primary endpoint (IO cell input)
    | TmgIgnore -- ^ Asynchronous / don't care (false path)
    deriving (Eq, Ord, Show, Enum)

-- | Clock edge (@RISING_EDGE = 0@, @FALLING_EDGE = 1@).
data ClockEdge = RisingEdge | FallingEdge
    deriving (Eq, Ord, Show, Enum)

{- | Clocking information of a register port: the clock port name, the
sensitive edge, the input setup/hold checks, and the output
clock-to-Q time.
-}
data TimingClockingInfo = TimingClockingInfo
    { tciClockPort :: !IdString -- ^ Port name of the clock domain
    , tciEdge :: !ClockEdge
    , tciSetup :: !DelayPair -- ^ Input timing checks
    , tciHold :: !DelayPair
    , tciClockToQ :: !DelayQuad -- ^ Output clock-to-Q time
    }
    deriving (Eq, Show)
