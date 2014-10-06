module FChess.Movement
open System

let allPossibleForSlidingPiece(from: Position, board: Board, directions: Direction[]) =
  let attacker = board.GetPiece(from)
  seq { 
    for i = 0 to directions.Length - 1 do
      let dir = int directions.[i]
      let pos = ref ((int from) + dir)
      while !pos &&& 0x88 = 0 do       
        let defender = board.GetPieceAt(!pos)
        if Piece.IsNone(defender) then
          yield !pos
          pos := !pos + dir
        elif Piece.IsOfOppositeColor(attacker, defender) then
          yield !pos
          pos := 128 
        else
          pos := 128 }

let allPossibleForQueen(from: Position, board: Board) =
  allPossibleForSlidingPiece(from, board, [| Direction.N; Direction.NE; Direction.E; Direction.SE; Direction.S; Direction.SW; Direction.W; Direction.NW |])

let allPossibleForRook(from: Position, board: Board) =
  allPossibleForSlidingPiece(from, board, [| Direction.N; Direction.S; Direction.E; Direction.W |])

let allPossibleForBishop(from: Position, board: Board) =
  allPossibleForSlidingPiece(from, board, [| Direction.NE; Direction.SE; Direction.SW; Direction.NW |])

let allPossibleForPawn(from: Position, board: Board) =
  let index = int from
  let attacker = board.GetPiece(from)  
  let attackingColor = attacker &&& Piece.Color
  let direction = if attackingColor = Piece.White then -1 else 1
  let rank = Position.GetRank(from)
  seq {
    let fwd = index + direction * 16
    let fwdfwd = index + direction * 32
    let fwdRight = index + direction * 17
    let fwdLeft = index + direction * 15

    if board.IsEmptyAt(fwd) then
      yield fwd
    if board.IsEmptyAt(fwdfwd) && (rank = 6 || rank = 1)  then
      yield fwdfwd
    if board.IsDefenderAt(fwdRight, attacker) then
      yield fwdRight
    if board.IsDefenderAt(fwdLeft, attacker) then
      yield fwdLeft }

let allPossibleForKnight(from: Position, board: Board) =
  let attacker = board.GetPiece(from)
  assert Piece.IsKnight(attacker)
  [| Direction.NNW; Direction.NNE; Direction.SSE; Direction.SSW; |] :> seq<Direction>
  |> Seq.map (fun direction -> (int from) + (int direction))
  |> Seq.filter (fun square -> board.IsEmptyOrDefenderAt(square, attacker))

let allPossibleForKing(from: Position, board: Board) =
  let attacker = board.GetPiece(from)
  assert Piece.IsKing(attacker)
  [| Direction.N; Direction.NE; Direction.E; Direction.SE; Direction.S; Direction.SW; Direction.W; Direction.NW; |] :> seq<Direction>
  |> Seq.map (fun direction -> (int from) + (int direction))
  |> Seq.filter (fun square -> board.IsEmptyOrDefenderAt(square, attacker))

let AllPossibleFrom(from: Position, board: Board) =
  let attacker = board.GetPiece(from)
  let piece = attacker &&& Piece.Type
  match piece with
  | Piece.Pawn -> allPossibleForPawn(from, board)
  | Piece.Rook -> allPossibleForRook(from, board)
  | Piece.Knight -> allPossibleForKnight(from, board)
  | Piece.Bishop -> allPossibleForBishop(from, board)
  | Piece.Queen -> allPossibleForQueen(from, board)
  | Piece.King -> allPossibleForKing(from, board)
  | _ -> Seq.empty

let isPossibleForPawn(fromPosition: Position, toPosition: Position, board: Board) =      
  allPossibleForPawn(fromPosition, board) |> Seq.exists (fun p -> p = int toPosition)

let isPossibleForRook(fromPosition: Position, toPosition: Position, board: Board) =  
  allPossibleForSlidingPiece(fromPosition, board, [| Direction.N; Direction.S; Direction.E; Direction.W |])
  |> Seq.exists (fun p -> p = int toPosition)

let isPossibleForBishop(fromPosition, toPosition, board) =  
  allPossibleForSlidingPiece(fromPosition, board, [| Direction.NE; Direction.SE; Direction.SW; Direction.NW |])
  |> Seq.exists (fun p -> p = int toPosition)

let isPossibleForQueen(fromPosition, toPosition, board) =  
  allPossibleForQueen(fromPosition, board) |> Seq.exists (fun p -> p = int toPosition)

let isPossibleForKing(fromPosition, toPosition, board) =
  allPossibleForKing(fromPosition, board) |> Seq.exists (fun p -> p = int toPosition)

let isPossibleForKnight(fromPosition, toPosition, board) =
  allPossibleForKnight(fromPosition, board) |> Seq.exists (fun p -> p = int toPosition)

let IsPossible(fromPosition: Position, toPosition: Position, board: Board) =
  let attacker = board.GetPiece(fromPosition)
  let piece = attacker &&& Piece.Type
  match piece with
  | Piece.Pawn -> isPossibleForPawn(fromPosition, toPosition, board)
  | Piece.Rook -> isPossibleForRook(fromPosition, toPosition, board)
  | Piece.Knight -> isPossibleForKnight(fromPosition, toPosition, board)
  | Piece.Bishop -> isPossibleForBishop(fromPosition, toPosition, board)
  | Piece.Queen -> isPossibleForQueen(fromPosition, toPosition, board)
  | Piece.King -> isPossibleForKing(fromPosition, toPosition, board)
  | _ -> false

let isUnderAttackBy(color: Piece, target: Position, board: Board) =
  Positions.A8ToH1 :> seq<Position>
  |> Seq.filter (fun pos -> Piece.IsOfColor(color, board.GetPiece(pos)))
  |> Seq.filter (fun pos -> pos <> target)
  |> Seq.exists (fun pos -> IsPossible(pos, target, board))

let isNotUnderAttackBy(color: Piece, target: Position, board: Board) = isUnderAttackBy(color, target, board) |> not

let getCastlingRights(board: Board) =  
    
  let wks = if board.HasNotMoved([|Position.E1; Position.H1|]) &&                          
               board.IsEmpty([|Position.F1; Position.G1|]) &&                  
               isNotUnderAttackBy(Piece.Black, Position.E1, board) &&
               isNotUnderAttackBy(Piece.Black, Position.F1, board.SetPiece(Position.F1, Piece.WhiteKing)) &&
               isNotUnderAttackBy(Piece.Black, Position.G1, board.SetPiece(Position.G1, Piece.WhiteKing))
            then CastlingRights.WhiteKingSide else CastlingRights.None
  
  let wqs = if board.HasNotMoved([|Position.E1; Position.A1|]) &&
               board.IsEmpty([|Position.B1; Position.C1; Position.D1|]) &&
               isNotUnderAttackBy(Piece.Black, Position.E1, board) &&
               isNotUnderAttackBy(Piece.Black, Position.D1, board.SetPiece(Position.D1, Piece.WhiteKing)) &&
               isNotUnderAttackBy(Piece.Black, Position.C1, board.SetPiece(Position.C1, Piece.WhiteKing)) &&
               isNotUnderAttackBy(Piece.Black, Position.B1, board.SetPiece(Position.B1, Piece.WhiteKing))
            then CastlingRights.WhiteQueenSide else CastlingRights.None

  let bks = if board.HasNotMoved([|Position.E8; Position.H8|]) &&
               board.IsEmpty([|Position.F8; Position.G8|]) &&              
               isNotUnderAttackBy(Piece.White, Position.E8, board) &&
               isNotUnderAttackBy(Piece.White, Position.F8, board.SetPiece(Position.F8, Piece.BlackKing)) &&
               isNotUnderAttackBy(Piece.White, Position.G8, board.SetPiece(Position.G8, Piece.BlackKing))
            then CastlingRights.BlackKingSide else CastlingRights.None

  let bqs = if board.HasNotMoved([|Position.E8; Position.A8|]) &&
               board.IsEmpty([|Position.B8; Position.C8; Position.D8|]) &&
               isNotUnderAttackBy(Piece.White, Position.E8, board) &&
               isNotUnderAttackBy(Piece.White, Position.D8, board.SetPiece(Position.D8, Piece.BlackKing)) &&
               isNotUnderAttackBy(Piece.White, Position.C8, board.SetPiece(Position.C8, Piece.BlackKing)) &&
               isNotUnderAttackBy(Piece.White, Position.B8, board.SetPiece(Position.B8, Piece.BlackKing))
            then CastlingRights.BlackQueenSide else CastlingRights.None
  
  wks ||| wqs ||| bks ||| bqs



  