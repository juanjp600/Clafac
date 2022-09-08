module Clafac.NumberRepresentation.SoftwareFloat

open System
open Clafac.NumberRepresentation.MathUtil

type Format = {
    ExponentBits: uint
    MantissaBits: uint
}

type Sign = Plus | Minus

let flipSign = function
    | Plus -> Minus
    | Minus -> Plus

let combineSigns (a: Sign) (b: Sign) =
    if a <> b then Minus else Plus
    
let signToString = function
    | Plus -> ""
    | Minus -> "-"

type Float = {
    Sign: Sign; Exponent: int64; Mantissa: uint64
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

let isNormal f = f.Mantissa >= implicitMantissaBit f.Format

let packedForm f =
    let denormalExpRepresentation = int32 (minExponent f.Format) - 1
    
    let exponentRepresentation =
        if isNormal f
            then (int32 f.Exponent) - denormalExpRepresentation
            else 0
    
    (if f.Sign = Minus then 1UL <<< int32 (f.Format.MantissaBits + f.Format.ExponentBits) else 0UL)
    ||| (uint64 exponentRepresentation <<< int32 f.Format.MantissaBits)
    ||| uint64 (f.Mantissa &&& mantissaMask f.Format)

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
    let b' = { b with Sign = flipSign b.Sign }
    add a b'

let multiply a b =
    let bma = bigint a.Mantissa
    let bmb = bigint b.Mantissa
    let product = uint64 ((bma * bmb) >>> int32(a.Format.MantissaBits))
    let newExp = a.Exponent + b.Exponent
    makeFloat (combineSigns a.Sign b.Sign) newExp product a.Format

let divide a b =
    let bma = (bigint a.Mantissa) <<< int32(a.Format.MantissaBits)
    let bmb = (bigint b.Mantissa)
    let quotient = uint64(bma / bmb)
    let newExp = a.Exponent - b.Exponent
    makeFloat (combineSigns a.Sign b.Sign) newExp quotient a.Format

let toString f =
    let bm = (bigint f.Mantissa)
    let shiftAmount = int32(int64 f.Format.MantissaBits - f.Exponent)
    let wholePortion =
        uint64 (if shiftAmount > 0
            then bm >>> shiftAmount
            else bm <<< -shiftAmount)
    let fractionMask = allOnes shiftAmount
    let fractionPortion =
        if wholePortion > 0UL
            then bm &&& bigint fractionMask
            else bm
    let flipBits acc i isSet =
        acc ||| (if isSet then (bigint 1 <<< (shiftAmount-i-1)) else bigint 0)
    let flippedFraction = foldBits shiftAmount fractionPortion flipBits
    let handleBit acc i isSet =
        let shiftedAcc = acc * bigint 10
        if isSet
            then shiftedAcc + (bigint 5) ** (i+1)
            else shiftedAcc
    let decimalFraction = foldBits shiftAmount flippedFraction handleBit
    let decimalStr = decimalFraction.ToString()
    let decimalStr = decimalStr.PadLeft(shiftAmount, '0')

    let signStr = signToString f.Sign
    $"{signStr}{wholePortion}.{decimalStr}".TrimEnd('0').TrimEnd('.')
