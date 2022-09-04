module Clafac.NumberRepresentation.MathUtil

let maxSetBit =
    let rec loop a = function
        | 0UL -> a
        | n -> loop (a + 1) (n >>> 1)
    loop 0
