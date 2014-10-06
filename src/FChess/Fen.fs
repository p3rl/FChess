module FChess.Fen
open System

let parseCastlingRights(fen: char[]) =
  Array.map (fun c -> 
    match c with
    | 'K' -> CastlingRights.WhiteKingSide
    | 'Q' -> CastlingRights.WhiteQueenSide
    | 'k' -> CastlingRights.BlackKingSide
    | 'q' -> CastlingRights.BlackQueenSide
    | _ -> CastlingRights.None) fen
    |> Array.fold (fun total rights -> total ||| rights) CastlingRights.None

let parseCastlingRightsFromString(fen: string) = parseCastlingRights(fen.ToCharArray())

let parseBoard(pieces: char[]) =
  Array.map (fun c -> 
    match c with
    | 'p' -> [| Piece.BlackPawn |]
    | 'r' -> [| Piece.BlackRook |]
    | 'n' -> [| Piece.BlackKnight |]
    | 'b' -> [| Piece.BlackBishop |]
    | 'q' -> [| Piece.BlackQueen |]
    | 'k' -> [| Piece.BlackKing |]
    | 'P' -> [| Piece.WhitePawn |]
    | 'R' -> [| Piece.WhiteRook |]
    | 'N' -> [| Piece.WhiteKnight |]
    | 'B' -> [| Piece.WhiteBishop |]
    | 'Q' -> [| Piece.WhiteQueen |]
    | 'K' -> [| Piece.WhiteKing |]
    | '/' -> Array.create 8 Piece.None
    | '8' -> Array.create 8 Piece.None
    | '7' -> Array.create 7 Piece.None
    | '6' -> Array.create 6 Piece.None
    | '5' -> Array.create 5 Piece.None
    | '4' -> Array.create 4 Piece.None
    | '3' -> Array.create 3 Piece.None
    | '2' -> Array.create 3 Piece.None
    | '1' -> Array.create 3 Piece.None
    | _ -> [|Piece.None|]) pieces
    |> Array.collect (fun p -> p)
    |> Board.Create

let Read(fen: string) = 
  let dataFields = fen.Split(' ')
  assert (dataFields.Length = 6)
  let boardData = (dataFields.[0] + "/").ToCharArray()
  let board = parseBoard(boardData)
  let activeColor = if dataFields.[1] = "w" then Piece.White else Piece.Black
  let castlingRights = parseCastlingRightsFromString(dataFields.[2])
  let enpassant = 
    if dataFields.[3] = "-" then Position.Undefined 
    else Enum.Parse(typeof<Position>, dataFields.[3].ToUpper()) :?> Position

  let halfMoveClock = System.Int32.Parse(dataFields.[4])
  let fullMoveCount = System.Int32.Parse(dataFields.[5])
  { 
    Board = board; 
    ActiveColor = activeColor; 
    CastlingRights = castlingRights;
    Enpassant = enpassant;
    HalfMoveClock = halfMoveClock; 
    FullMoveCount = fullMoveCount 
  }

let getFenPiece(piece: Piece) =
  match piece with
  | Piece.BlackPawn -> 'p'
  | Piece.BlackRook -> 'r'
  | Piece.BlackKnight -> 'n'
  | Piece.BlackBishop -> 'b'
  | Piece.BlackQueen -> 'q'
  | Piece.BlackKing -> 'k'
  | Piece.WhitePawn -> 'P'
  | Piece.WhiteRook -> 'R'
  | Piece.WhiteKnight -> 'N'
  | Piece.WhiteBishop -> 'B'
  | Piece.WhiteQueen -> 'Q'
  | Piece.WhiteKing -> 'K'
  | _ -> ' '

let getFenBoard(board: Board) =  
  seq {    
    for row = 0 to 7 do
      let clock = ref 0
      for file = 0 to 7 do
        let index = row * 16 + file
        let piece = board.GetPieceAt(index)

        if Piece.IsNone(piece) then
          clock := !clock + 1
        elif !clock <> 0 then
          yield (!clock).ToString().[0]
          clock := 0
        else
          yield getFenPiece(piece)
        
        if file = 7 then
          if !clock <> 0 then
            yield (!clock).ToString().[0]
          if row <> 7 then yield '/'
        
  } |> Array.ofSeq

let getFenCastlingRights(castlingRights: CastlingRights) =
  seq {
    if (castlingRights &&& CastlingRights.WhiteKingSide) = CastlingRights.WhiteKingSide then
      yield 'K'
    
    if (castlingRights &&& CastlingRights.WhiteQueenSide) = CastlingRights.WhiteQueenSide then
      yield 'Q'
    
    if (castlingRights &&& CastlingRights.BlackKingSide) = CastlingRights.BlackKingSide then
      yield 'k'
    
    if (castlingRights &&& CastlingRights.BlackQueenSide) = CastlingRights.BlackQueenSide then
      yield 'q'    
  } |> Array.ofSeq

let Write(gameState: ChessGameState) =
  let board = String(getFenBoard(gameState.Board))
  let activeColor = if gameState.ActiveColor = Piece.White then "w" else "b"
  let castlingRights = String(getFenCastlingRights(gameState.CastlingRights))
  let enpassant = if gameState.Enpassant = Position.Undefined then "-" else gameState.Enpassant.ToString()
  let halfMoveClock = gameState.HalfMoveClock.ToString()
  let fullMoveCount = gameState.FullMoveCount.ToString()
  System.String.Join(" ", board, activeColor, castlingRights, enpassant, halfMoveClock, fullMoveCount)