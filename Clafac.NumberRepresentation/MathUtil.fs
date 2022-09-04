module Clafac.NumberRepresentation.MathUtil

open System.Linq

let maxSetBit =
    let rec loop a = function
        | 0UL -> a
        | n -> loop (a + 1) (n >>> 1)
    loop 0

let inline foldBits numBits n f =
    let f' acc i = f acc i ((n >>> i) &&& LanguagePrimitives.GenericOne > LanguagePrimitives.GenericZero) 
    
    List.ofSeq (Enumerable.Range(0, numBits))
    |> List.fold f' LanguagePrimitives.GenericZero

let allOnes n =
    if n > 0
        then (1UL <<< n)-1UL
        else 0UL
