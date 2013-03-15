module PartialHashInversion
open System
open System.Security.Cryptography

let explode (s:string) =
    [for c in s -> byte c]

let characterSet = explode "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ+/="

let rec replace list position item result =
    match position, list with
    | 0, H :: T -> List.append result <| List.append [item] T
    | _, H :: T -> replace T (position-1) item <| List.append result [H]
    | _, [] -> result

let refreshHash (hash:byte list) prefixLength =
    let newChar = List.head <| List.sortBy (fun x -> Guid.NewGuid()) characterSet
    let position = List.head <| List.sortBy (fun x -> Guid.NewGuid()) [prefixLength..hash.Length]
    replace hash position newChar []

let computeHashNet (provider:HashAlgorithm) stamp =
    List.toArray stamp |> provider.ComputeHash |> List.ofArray

let sha = new SHA1Managed()
let computeHash = computeHashNet <| sha

let rec collides list length =
    match list, length with
    | _, 0 -> true
    | 48uy :: T, _ -> collides T (length-1)
    | _, _ -> false

let stampCollides stamp length =
    collides (computeHash stamp) length

let rec findHash stamp prefixLength length =
    match stampCollides stamp length with
    | true -> stamp
    | false -> findHash (refreshHash stamp prefixLength) prefixLength length