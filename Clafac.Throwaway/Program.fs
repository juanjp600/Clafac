open Clafac.NumberRepresentation.FloatImplementations
open Clafac.NumberRepresentation.SoftwareFloat

let myOnePointFive = Ieee754Single.Float(false, 0, 0b11000000_00000000_00000000UL)
let myDenormal = Ieee754Single.Float(false, -126, 0b01000000_00000000_00000000UL)

let makeIeee f = Ieee754Single.Float(f.Sign, f.Exponent, f.Mantissa)

let test (myInitial: Ieee754Single.Float) =
    let myDoubled = makeIeee (add myInitial.Value myInitial.Value)
    let myTripled = makeIeee (add myDoubled.Value myInitial.Value)
    let myRestored = makeIeee (subtract myTripled.Value myDoubled.Value)
    let myZeroed = makeIeee (subtract myTripled.Value myTripled.Value)

    let printStats (f: Ieee754Single.Float) =
        printf $"%s{f.HardwareValue.ToString()}\t%d{f.Value.Exponent}\t{f.ExponentRepresentation}\n"
    
    printStats myInitial
    printStats myDoubled
    printStats myTripled
    printStats myRestored
    printStats myZeroed

    printf "**************************\n"

test myDenormal
test myOnePointFive
