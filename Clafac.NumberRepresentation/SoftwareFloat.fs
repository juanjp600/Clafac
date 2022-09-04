module rec Clafac.NumberRepresentation.SoftwareFloat

open System
open Clafac.NumberRepresentation.MathUtil

type Format = {
    ExponentBits: uint
    MantissaBits: uint
}

type Float = {
    Sign: bool; Exponent: int64; Mantissa: uint64
    Format: Format
}

type AlignedMantissas = {
    SharedExponent: int64
    Mantissa1: uint64
    Mantissa2: uint64
}

let minExponent fmt = -(1L <<< int32(fmt.ExponentBits-1u))+2L
let implicitMantissaBit fmt = (1UL <<< int32(fmt.MantissaBits))
let mantissaMask fmt = (implicitMantissaBit fmt)-1UL

let isNormal f = not (minExponent f.Format = f.Exponent)

let alignMantissas a b =
    let maxExp = Math.Max(a.Exponent, b.Exponent)
    let shiftMantissa f =
        f.Mantissa >>> int32(maxExp - f.Exponent)
    {
        SharedExponent = maxExp
        Mantissa1 = shiftMantissa a
        Mantissa2 = shiftMantissa b
    }

let makeFloat sign exp m fmt =
    let minExp = (minExponent fmt)
    
    let target = int(fmt.MantissaBits) + 1
    let amount = target - (maxSetBit m)
    let shifter =
        if (amount > 0)
            then (<<<)
            else (>>>)
    let newMantissa = (shifter m (Math.Abs amount))
    let newExp = exp - int64(amount)
    
    let finalExp = Math.Max(minExp, newExp)
    let finalMantissa = newMantissa >>> int32(finalExp - newExp)
    {
        Sign = sign
        Exponent = finalExp
        Mantissa = finalMantissa
        Format = fmt
    }

let add a b =
    let aligned = alignMantissas a b
    let op = if a.Sign = b.Sign then (+) else (-)

    let ma = aligned.Mantissa1
    let mb = aligned.Mantissa2
    
    let (sign, mantissa) =
        if ma > mb
            then (a.Sign, op ma mb)
            else (b.Sign, op mb ma)
    
    if mantissa > 0UL
        then makeFloat sign aligned.SharedExponent mantissa a.Format
        else { Format = a.Format; Sign = a.Sign; Exponent = minExponent a.Format; Mantissa = 0UL }

let subtract a b =
    let b' = { b with Sign = not b.Sign }
    add a b'
