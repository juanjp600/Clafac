open Clafac.NumberRepresentation.FloatImplementations
open Clafac.NumberRepresentation.SoftwareFloat

let myOnePointFive = Ieee754Single.Float(false, 0, 0b11000000_00000000_00000000UL)
let myDenormal = Ieee754Single.Float(false, -126, 0b01000000_00000000_00000000UL)

let makeIeee f = Ieee754Single.Float(f.Sign, f.Exponent, f.Mantissa)

let printStats (f: Ieee754Single.Float) =
    printf $"%s{f.HardwareValue.ToString()}\t%d{f.Value.Exponent}\t{f.ExponentRepresentation}\n"

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

let myOne = Ieee754Single.Float(false, 0, 0b10000000_00000000_00000000UL)
let myHalf = Ieee754Single.Float(false, -1, 0b10000000_00000000_00000000UL)

let test2 (myInitial: Ieee754Single.Float) =
    let myDoubled = makeIeee (add myInitial.Value myInitial.Value)
    let myTripled = makeIeee (add myDoubled.Value myInitial.Value)
    let mySquaredOne = makeIeee (multiply myInitial.Value myInitial.Value)
    let mySquaredTwo = makeIeee (multiply myDoubled.Value myDoubled.Value)
    let mySquaredThree = makeIeee (multiply myTripled.Value myTripled.Value)
    let myThreeTimesInitial = makeIeee (multiply myInitial.Value myTripled.Value)

    printStats myInitial
    printStats myDoubled
    printStats myTripled
    printStats mySquaredOne
    printStats mySquaredTwo
    printStats mySquaredThree
    printStats myThreeTimesInitial
    printf "**************************\n"

test2 myOne
test2 myHalf
