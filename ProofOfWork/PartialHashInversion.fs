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

let refreshHash (hash:byte list) =
    let newChar = List.head <| List.sortBy (fun x -> Guid.NewGuid()) characterSet
    let position = List.head <| List.sortBy (fun x -> Guid.NewGuid()) [0..hash.Length]
    replace hash position newChar []

let rec collides (hash:byte list) length =
    match hash, length with
    | _, 0 -> true
    | H :: T, _ -> collides T (length-1)
    | _, _ -> false

let rec computeHash (stamp:byte list) length =
    match collides stamp length with
    | true -> hash
    | false -> computeHash (refreshHash stamp) length