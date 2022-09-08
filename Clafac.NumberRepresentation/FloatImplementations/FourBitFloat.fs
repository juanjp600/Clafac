module Clafac.NumberRepresentation.FloatImplementations.FourBitFloat

open Clafac.NumberRepresentation.SoftwareFloat

let Format : Format = {
    ExponentBits = 2u; MantissaBits = 1u
}

type Float(sign, exponent, mantissa) =
    member this.Value =
        makeFloat sign exponent mantissa Format

    new() = Float(Plus, minExponent Format, 0UL)
