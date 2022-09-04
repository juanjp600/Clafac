open System
open Clafac.NumberRepresentation.FloatImplementations
open Clafac.NumberRepresentation.SoftwareFloat

let myOnePointFive = Ieee754Single.Float(false, 0, 0b11000000_00000000_00000000UL)
let myDenormal = Ieee754Single.Float(false, -126, 0b1000000_00000000_00000000UL)

let makeIeee f = Ieee754Single.Float(f.Sign, f.Exponent, f.Mantissa)

let test (myInitial: Ieee754Single.Float) =
    let myDoubled = makeIeee (add myInitial.Value myInitial.Value)
    let myTripled = makeIeee (add myDoubled.Value myInitial.Value)
    let myRestored = makeIeee (subtract myTripled.Value myDoubled.Value)
    let myZeroed = makeIeee (subtract myTripled.Value myTripled.Value)

    printf $"%s{myInitial.HardwareValue.ToString()}\t%d{myInitial.Value.Exponent}\n"
    printf $"%s{myDoubled.HardwareValue.ToString()}\t%d{myDoubled.Value.Exponent}\n"
    printf $"%s{myTripled.HardwareValue.ToString()}\t%d{myTripled.Value.Exponent}\n"
    printf $"%s{myRestored.HardwareValue.ToString()}\t%d{myRestored.Value.Exponent}\n"
    printf $"%s{myZeroed.HardwareValue.ToString()}\t%d{myZeroed.Value.Exponent}\n"
    printf "**************************\n"

test myDenormal
test myOnePointFive
