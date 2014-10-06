module FChess.GamePlay

let private getFullMoveCount(attacker: Piece, count: int) =
  if Piece.IsBlack(attacker) then count + 1 else count

let private getNextPlayer(attacker: Piece) =
  match Piece.GetColor(attacker) with
  | Piece.White -> Piece.Black
  | Piece.Black -> Piece.White
  | _ -> raise (System.Exception("Undefined color"))

let private getHalfMoveClock(defender: Piece, currentClock: int) =
  if defender = Piece.None then currentClock + 1 else 0

let private getEnpassantSquare(fromPos: int, toPos: int, board: Board) =
  let attacker = board.GetPieceAt(fromPos)
  let distance = System.Math.Abs(toPos - fromPos)
  let backward = if Piece.IsWhite(attacker) then 16 else -16
  match Piece.IsPawn(attacker) with
  | true when distance = 32 -> enum<Position>(toPos + backward)
  | _ -> Position.Undefined
  
let private moveRookIfCastlingMove(fromPos: Position, toPos: Position, board: Board, rights: CastlingRights) =
  (board, rights)

let MakeMove(fromPosition: Position, toPosition: Position, game: ChessGameState) =  
  let attacker = game.Board.GetPiece(fromPosition)
  let defender = game.Board.GetPiece(toPosition)
  
  Contract.require(Piece.GetColor(attacker) = game.ActiveColor)
  Contract.require(Movement.IsPossible(fromPosition, toPosition, game.Board))
  
  let board = game.Board.MovePiece(fromPosition, toPosition)
  let boardAndRights = moveRookIfCastlingMove(fromPosition, toPosition, board, game.CastlingRights)
  let enpassantSquare = getEnpassantSquare(int fromPosition, int toPosition, game.Board)
  let halfMoveClock = getHalfMoveClock(defender, game.HalfMoveClock)
  let fullMoveCount = getFullMoveCount(attacker, game.FullMoveCount)
  let castlingRights = Movement.getCastlingRights(game.Board)

  {
    Board = fst boardAndRights
    ActiveColor = getNextPlayer(attacker)
    CastlingRights = castlingRights
    HalfMoveClock = halfMoveClock
    FullMoveCount = fullMoveCount
    Enpassant = enpassantSquare
  }
      

