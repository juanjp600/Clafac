open Clafac.NumberRepresentation.FloatImplementations
open Clafac.NumberRepresentation.SoftwareFloat

let myOnePointFive = Ieee754Single.Float(false, 0, 0b11000000_00000000_00000000UL) //1.5 * 2^0
let myDenormal = Ieee754Single.Float(false, -126, 0b01000000_00000000_00000000UL) //0.5 * 2^(-126)

let makeIeee f = Ieee754Single.Float(f.Sign, f.Exponent, f.Mantissa)

let printStats (f: Ieee754Single.Float) =
    printf $"%s{f.HardwareValue.ToString()}\t%s{toString f.Value}\n"

let test1 (myInitial: Ieee754Single.Float) =
    let myDoubled = makeIeee (add myInitial.Value myInitial.Value)
    let myTripled = makeIeee (add myDoubled.Value myInitial.Value)
    let myRestored = makeIeee (subtract myTripled.Value myDoubled.Value)
    let myZeroed = makeIeee (subtract myTripled.Value myTripled.Value)

    printStats myInitial
    printStats myDoubled
    printStats myTripled
    printStats myRestored
    printStats myZeroed
    printf "**************************\n"

test1 myDenormal
test1 myOnePointFive

let myOne = Ieee754Single.Float(false, 0, 0b10000000_00000000_00000000UL) //1 * 2^0
let myHalf = Ieee754Single.Float(false, -1, 0b10000000_00000000_00000000UL) //1 * 2^(-1)
let myFive = Ieee754Single.Float(false, 2, 0b10100000_00000000_00000000UL) //1.25 * 2^2

let test2 (numerator: Ieee754Single.Float) (denominator: Ieee754Single.Float) =
    let result = makeIeee (divide numerator.Value denominator.Value)

    printStats numerator
    printStats denominator
    printStats result
    printf "**************************\n"

test2 myFive myOne
test2 myFive myHalf
test2 myFive myOnePointFive
