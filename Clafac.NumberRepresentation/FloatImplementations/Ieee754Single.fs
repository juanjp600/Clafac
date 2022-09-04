module Clafac.NumberRepresentation.FloatImplementations.Ieee754Single

open System
open Clafac.NumberRepresentation.SoftwareFloat

let Format : Format = {
    ExponentBits = 8u; MantissaBits = 23u
}

type Float(sign, exponent, mantissa) =
    member this.Value =
        makeFloat sign exponent mantissa Format

    member this.ExponentRepresentation
        with get() =
            let minRepresentedValue = int32 (minExponent Format) - 1
            if mantissa < implicitMantissaBit Format
                then 0
                else (int32 exponent) - minRepresentedValue
    
    member this.PackedValue
        with get() =
            (if sign then 1u <<< int32 (Format.MantissaBits + Format.ExponentBits) else 0u)
            ||| (uint32 this.ExponentRepresentation <<< int32 Format.MantissaBits)
            ||| uint32 (mantissa &&& mantissaMask Format)

    member this.HardwareValue
        with get() = BitConverter.UInt32BitsToSingle(this.PackedValue)

    new() = Float(false, minExponent Format, 0UL)

let Zero = Float()
