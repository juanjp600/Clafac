module Clafac.NumberRepresentation.FloatImplementations.Ieee754Single

open System
open Clafac.NumberRepresentation.SoftwareFloat

let Format : Format = {
    ExponentBits = 8u; MantissaBits = 23u
}

type Float(sign, exponent, mantissa) =
    member this.Value =
        makeFloat sign exponent mantissa Format
    member this.HardwareValue
        with get() = BitConverter.UInt32BitsToSingle(uint32 (packedForm this.Value))
    new() = Float(false, minExponent Format, 0UL)

let Zero = Float()
