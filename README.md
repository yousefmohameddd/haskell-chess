# Haskell Chess
## Description
This project is a recreation of the classic game of chess, implemented using Haskell. It has all the rules of regular chess except promotion and castling. The game is played through the CLI and has five possible commands:
1) setBoard:: Board: Takes no inputs and returns a board with the initial configuration
2) visualizeBoard :: Board -> String: Takes as input a board and returns a visual representation of the board
3) isLegal :: Piece -> Board -> Location -> Bool: Takes a piece, board, and location as input, and returns true if the piece can move to that location; otherwise false
4) suggestMove :: Piece -> Board -> [Location]: Takes a piece and a board, and outputs a list of possible legal moves
5) move :: Piece -> Location -> Board -> Board: Takes a piece, location, and board, and returns an updated board with that move ONLY if it is legal; otherwise, an error is thrown

## How to Install
1) Download any Haskell compiler (Winhugs for Windows, GHC for Linux, or Mac)
2) Clone the repository
3) Open up the compiler and select the chess-haskell.hs file to run
4) Use the CLI to play

## How to Play
1) Use the command setBoard to get an initial configuration of the board
2) Take the board and use it as input to visualizeBoard to see the board
3) To find possible moves for any piece, use suggestMove <your_piece_here> <current_board_here>, which will return a list of possible moves for that piece 
4) To finalize a move, use move <your_piece_here> <location_to_move_at> <current_board_here>, which will return an updated board with that move (only if it is legal)
5) Using that updated board, repeat steps 2 to 5 until the game ends


