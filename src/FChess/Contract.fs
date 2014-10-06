module FChess.Contract

let require(value: bool) =
  if value <> true then raise (System.Exception("error"))

