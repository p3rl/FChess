namespace FChess

open System

/// <summary>
/// Represents castling availability.
/// </summary>
[<Flags>]
type CastlingRights = 
  | None            = 0x00y
  | WhiteKingSide   = 0x01y
  | WhiteQueenSide  = 0x02y
  | BlackKingSide   = 0x04y
  | BlackQueenSide  = 0x08y
  | All             = 0x0Fy

/// <summary>
/// Represents a chess piece.
/// </summary>
/// <remarks>
/// 
/// W: White
/// B: Black
/// P: Piece
/// N: Moved
/// X: Not used
///                 * *
///             X N P P P P B W    Dec    Hex
/// Pawn        0 0 0 0 0 1 0 0     4     0x04
/// WhitePawn   0 0 0 0 0 1 0 1     5     0x05
/// BlackPawn   0 0 0 0 0 1 1 0     6     0x06
/// Knight      0 0 0 0 1 0 0 0     8     0x08
/// WhiteKnight 0 0 0 0 1 0 0 1     9     0x09
/// BlackKnight 0 0 0 0 1 0 1 0    10     0x0A 
/// King        0 0 0 0 1 1 0 0    12     0x0C
/// WhiteKing   0 0 0 0 1 1 0 1    13     0x0D
/// BlackKing   0 0 0 0 1 1 1 0    14     0x0E
/// Bishop      0 0 0 1 0 0 0 0    16     0x10
/// WhiteBishop 0 0 0 1 0 0 0 1    17     0x11
/// BlackBishop 0 0 0 1 0 0 1 0    18     0x12
/// Rook        0 0 1 0 0 0 0 0    32     0x20
/// WhiteRook   0 0 1 0 0 0 0 1    33     0x21
/// BlackRook   0 0 1 0 0 0 1 0    34     0x22
/// Queen       0 0 1 1 0 0 0 0    48     0x30
/// WhiteQueen  0 0 1 1 0 0 0 1    49     0x31
/// BlackQueen  0 0 1 1 0 0 1 0    50     0x32
/// Type        0 0 1 1 1 1 0 0    60     0x3C
/// Moved       0 1 0 0 0 0 0 0    64     0x40
/// 
/// The first two bits indicates piece color. The next four represents the different pieces.
/// All sliding pieces is represented in the 2 upper piece bits. The N bit indicates that the
/// piece has been moved. The bit marked X is not used.
/// </remarks>
[<Flags>]
type Piece = 
  | None        = 0x00y
  | White       = 0x01y
  | Black       = 0x02y
  | Color       = 0x03y
  | Pawn        = 0x04y
  | WhitePawn   = 0x05y
  | BlackPawn   = 0x06y
  | Knight      = 0x08y
  | WhiteKnight = 0x09y
  | BlackKnight = 0x0Ay
  | King        = 0x0Cy
  | WhiteKing   = 0x0Dy
  | BlackKing   = 0x0Ey
  | Bishop      = 0x10y
  | WhiteBishop = 0x11y
  | BlackBishop = 0x12y
  | Rook        = 0x20y
  | WhiteRook   = 0x21y
  | BlackRook   = 0x22y
  | Queen       = 0x30y
  | WhiteQueen  = 0x31y
  | BlackQueen  = 0x32y
  | Type        = 0x3Cy
  | Moved       = 0x40y

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Piece =
  let IsWhite(piece: Piece) = int (piece &&& Piece.White) <> 0
  let IsBlack(piece: Piece) = int (piece &&& Piece.Black) <> 0
  let IsNone(piece: Piece) = int piece = 0
  let IsNotMoved(piece: Piece) = int (piece &&& Piece.Moved) = 0
  let IsPawn(piece: Piece) = piece &&& Piece.Type = Piece.Pawn
  let IsKnight(piece: Piece) = piece &&& Piece.Type = Piece.Knight
  let IsKing(piece: Piece) = piece &&& Piece.Type = Piece.King  
  let IsOfOppositeColor(attacker: Piece, defender: Piece) = (attacker &&& Piece.Color) <> (defender &&& Piece.Color)
  let IsOfColor(color: Piece, piece: Piece) = color = (piece &&& Piece.Color)
  let GetColor(piece: Piece) = piece &&& Piece.Color

/// <summary>Represents a chess board position</summary>
type public Position = 
  | A8 = 0x00
  | B8 = 0x01
  | C8 = 0x02
  | D8 = 0x03
  | E8 = 0x04
  | F8 = 0x05
  | G8 = 0x06
  | H8 = 0x07
  | A7 = 0x10
  | B7 = 0x11
  | C7 = 0x12
  | D7 = 0x13
  | E7 = 0x14
  | F7 = 0x15
  | G7 = 0x16
  | H7 = 0x17
  | A6 = 0x20
  | B6 = 0x21
  | C6 = 0x22
  | D6 = 0x23
  | E6 = 0x24
  | F6 = 0x25
  | G6 = 0x26
  | H6 = 0x27
  | A5 = 0x30
  | B5 = 0x31
  | C5 = 0x32
  | D5 = 0x33
  | E5 = 0x34
  | F5 = 0x35
  | G5 = 0x36
  | H5 = 0x37
  | A4 = 0x40
  | B4 = 0x41
  | C4 = 0x42
  | D4 = 0x43
  | E4 = 0x44
  | F4 = 0x45
  | G4 = 0x46
  | H4 = 0x47
  | A3 = 0x50
  | B3 = 0x51
  | C3 = 0x52
  | D3 = 0x53
  | E3 = 0x54
  | F3 = 0x55
  | G3 = 0x56
  | H3 = 0x57
  | A2 = 0x60
  | B2 = 0x61
  | C2 = 0x62
  | D2 = 0x63
  | E2 = 0x64
  | F2 = 0x65
  | G2 = 0x66
  | H2 = 0x67
  | A1 = 0x70
  | B1 = 0x71
  | C1 = 0x72
  | D1 = 0x73
  | E1 = 0x74
  | F1 = 0x75
  | G1 = 0x76
  | H1 = 0x77
  | Undefined = 0x78

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module Position =
  let GetRank(position: Position) = int position >>> 4
  let GetFile(position: Position) = int position &&& 7
  let GetRankFile(position: Position) = (GetRank(position), GetFile(position))

type Direction =
  | N = -16  
  | NW = -17
  | NNW = -33
  | W = -1
  | SW = 15
  | SSW = 31
  | S = 16
  | SE = 17
  | SSE = 33
  | E = 1
  | NE = -15
  | NNE = -31

/// <summary>
/// Represents a 0x88 chess board
/// </summary>
[<Struct>]
type public Board private (pieces: Piece[]) =  
  member x.Pieces = pieces
    
  member this.SetPiece(position:Position, piece:Piece) =
    let pieces = Array.copy this.Pieces
    pieces.[(int)position] <- piece
    Board(pieces)

  member this.MovePiece(fromPosition: Position, toPosition: Position) =
    let pieces = Array.copy this.Pieces
    let attacker = pieces.[int fromPosition]
    pieces.[int fromPosition] <- Piece.None
    pieces.[int toPosition] <- attacker ||| Piece.Moved
    Board(pieces)

  member inline this.GetPiece(position: Position) =
    this.Pieces.[int position]
  
  member inline this.GetPieceAt(square: int) = this.Pieces.[square]

  member inline this.IsEmpty(position: Position) = this.Pieces.[int position] = Piece.None

  member inline this.IsEmpty(positions: Position[]) =
    let pieces = this.Pieces
    Array.forall (fun pos -> pieces.[int pos] = Piece.None) positions
  
  member inline this.IsEmptyAt(square: int) = square &&& 0x88 = 0 && this.Pieces.[square] = Piece.None

  member inline this.HasMoved(position: Position) = this.Pieces.[int position] &&& Piece.Moved = Piece.Moved

  member inline this.HasNotMoved(positions: Position[]) = 
    let pieces = this.Pieces
    Array.forall (fun pos -> 
      let piece = pieces.[int pos]
      piece <> Piece.None && (piece &&& Piece.Moved <> Piece.Moved)) positions

  member inline this.IsDefenderAt(square: int, attacker: Piece) = 
    if square &&& 0x88 <> 0 then false else
      let defender = this.GetPieceAt(square)
      defender <> Piece.None && Piece.GetColor(defender) <> Piece.GetColor(attacker)

  member inline this.IsEmptyOrDefenderAt(square: int, attacker: Piece) =
    if square &&& 0x88 <> 0 then false else
      let defender = this.Pieces.[square]
      defender = Piece.None || (defender &&& Piece.Color) <> (attacker &&& Piece.Color)

  static member Create(pieces: Piece[]) =
    assert (pieces.Length = 128)
    Board(pieces)
    
  static member Empty() = Board(Array.create 128 Piece.None)

  static member New() =
    Board.Create(
      [| Piece.BlackRook; Piece.BlackKnight; Piece.BlackBishop; Piece.BlackQueen; Piece.BlackKing; Piece.BlackBishop; Piece.BlackKnight; Piece.BlackRook; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None;
          Piece.BlackPawn; Piece.BlackPawn; Piece.BlackPawn; Piece.BlackPawn; Piece.BlackPawn; Piece.BlackPawn; Piece.BlackPawn; Piece.BlackPawn; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None;
          Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None;
          Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None;           
          Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None;
          Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None;
          Piece.WhitePawn; Piece.WhitePawn; Piece.WhitePawn; Piece.WhitePawn; Piece.WhitePawn; Piece.WhitePawn; Piece.WhitePawn; Piece.WhitePawn; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None;
          Piece.WhiteRook; Piece.WhiteKnight; Piece.WhiteBishop; Piece.WhiteQueen; Piece.WhiteKing; Piece.WhiteBishop; Piece.WhiteKnight; Piece.WhiteRook; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; Piece.None; |])

/// <summary>
/// Represents a chess game state
/// </summary>
type ChessGameState = 
  {
    Board : Board
    ActiveColor : Piece
    CastlingRights : CastlingRights
    Enpassant : Position
    HalfMoveClock : int
    FullMoveCount : int }

