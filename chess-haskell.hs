type Location = (Char, Int)
data Player = White | Black deriving (Show, Eq)
data Piece = P Location | N Location | K Location | Q Location | R Location| B Location deriving (Show, Eq)
type Board = (Player, [Piece], [Piece])


--a
setBoard :: Board
setBoard =(White,[R('h',1),N('g',1),B('f',1),K('e',1),Q('d',1),B('c',1),N('b',1),R('a',1),P ('h',2),P ('g',2),P ('f',2),P ('e',2),P ('d',2),P('c',2),P('b',2),P ('a',2)],[R ('h',8),N ('g',8),B ('f',8),K ('e',8),Q ('d',8),B('c',8),N('b',8),R ('a',8),P ('h',7),P ('g',7),P ('f',7),P ('e',7),P ('d',7),P ('c',7),P ('b',7),P ('a',7)])


--b
visualizeWhite:: Piece->String
visualizeWhite (P l)="PW"
visualizeWhite (N l)="NW"
visualizeWhite (B l)="BW"
visualizeWhite (K l)="KW"
visualizeWhite (Q l)="QW"
visualizeWhite (R l)="RW"

visualizeBlack:: Piece->String
visualizeBlack (P l)="PB"
visualizeBlack (N l)="NB"
visualizeBlack (B l)="BB"
visualizeBlack (K l)="KB"
visualizeBlack (Q l)="QB"
visualizeBlack (R l)="RB"


getLocation::Piece->Location
getLocation (P l)=l
getLocation (N l)=l
getLocation (B l)=l
getLocation (K l)=l
getLocation (Q l)=l
getLocation (R l)=l


checkFind::[Piece]->Location->Bool
checkFind [] l2=False
checkFind (h:t) l2=if ((getLocation h)==l2) then True else checkFind t l2


getpiece ::[Piece]->Location->Piece
getpiece [] l=error "Cannot find"
getpiece (h:t) l =if((getLocation h) ==l) then h else getpiece t l

visualizeLetters ="    a    b    c    d    e    f    g    h "	
visualizeBoard:: Board->String
visualizeBoard (player,w,b)= visualizeLetters++"\n8 " ++helpvisualizeBoard (player,w,b) ('a',8) ++"\nTurn:"++show player

						
helpvisualizeBoard::Board->Location->String
helpvisualizeBoard (player,l1,l2) (c,0)= ""




helpvisualizeBoard (player,(h:t),(h2:t2)) (c,i) |c>'h' =if(i/=1) then ("|\n"++show (i-1)++" "++helpvisualizeBoard (player,(h:t),(h2:t2)) ('a',i-1))else "|\n"
											   |checkFind (h:t)  (c,i) = "| " ++ visualizeWhite (getpiece (h:t) (c,i)) ++" " ++ helpvisualizeBoard (player,(h:t),(h2:t2)) (succ c,i)
											   |checkFind (h2:t2) (c,i) = "| " ++ visualizeBlack (getpiece (h2:t2) (c,i)) ++ " "++ helpvisualizeBoard (player,(h:t),(h2:t2)) (succ c,i)
											   |otherwise = "|    "++helpvisualizeBoard (player,(h:t),(h2:t2)) (succ c,i)
--siiiiuuuuuuuuu
getPiece (P l)= 'P' 
getPiece (N l)= 'N'
getPiece (B l)= 'B'
getPiece (K l)= 'K'
getPiece (Q l)= 'Q'
getPiece (R l)= 'R'

getNumber 'a' = 1
getNumber 'b' = 2
getNumber 'c' = 3
getNumber 'd' = 4
getNumber 'e' = 5
getNumber 'f' = 6
getNumber 'g' = 7
getNumber 'h' = 8

getCharacter 1 = 'a'
getCharacter 2 = 'b'
getCharacter 3 = 'c'
getCharacter 4 = 'd'
getCharacter 5 = 'e'
getCharacter 6 = 'f'
getCharacter 7 = 'g'
getCharacter 8 = 'h'

isFree allPieces move = if move `elem` (map getLocation allPieces) then False else True 
getColor l (_, white, black) = if l `elem` white then White else Black

notFree:: Player -> Board -> Location -> Bool
notFree color board move = if color==White then checkWhite move board else checkBlack move board

checkWhite:: Location -> Board -> Bool
checkBlack:: Location -> Board -> Bool
checkWhite move (_,white,_) = if move `elem` (map getLocation white) then True else False 
checkBlack move (_,_,black) = if move `elem` (map getLocation black) then True else False 

hasEnemyPiece listOfPieces move = move `elem` (map getLocation listOfPieces)
pawnMove (P (xCurr,yCurr)) (xMove,yMove) White (_,wh,bl) | (yCurr==2) && (xCurr ==xMove) && (yMove - yCurr == 2) && isFree (wh++bl) (xCurr,yCurr+1) && isFree (wh++bl) (xCurr,yCurr+2) = True-- if moving two spaces, check if there's a piece inbetween
													 | xCurr==xMove && yMove - yCurr == 1 && isFree (wh++bl) (xMove,yMove)	 = True --moving one turn forward
													 | (succ xCurr) == xMove && (yCurr+1)==yMove && hasEnemyPiece bl (xMove,yMove) = True --should attack both squares infront
													 | (pred xCurr) == xMove && (yCurr+1)==yMove && hasEnemyPiece bl (xMove,yMove) = True --same comment as above
													 | otherwise = False
													 
pawnMove (P (xCurr,yCurr)) (xMove,yMove) Black (_,wh,bl) | yCurr==7 && xCurr==xMove &&  yCurr-yMove == 2 && isFree (wh++bl) (xCurr,yCurr-1) && isFree (wh++bl) (xCurr,yCurr-2) = True -- if moving two spaces, check if there's a piece inbetween
													 | xCurr==xMove && yCurr-yMove == 1 && isFree (wh++bl) (xMove,yMove) = True --moving one turn forward
													 | (succ xCurr) == xMove && (yCurr-1)==yMove  && hasEnemyPiece wh (xMove,yMove)= True --should attack both squares infront
													 | (pred xCurr) == xMove && (yCurr-1)==yMove  && hasEnemyPiece wh (xMove,yMove) = True --same comment as above
													 | otherwise = False								

kingMove (K (xCurr,yCurr)) (xMove,yMove)  = if abs (yCurr - yMove) <= 1 && abs(getNumber xCurr - getNumber xMove) <=1 then True else False--if its adjacent

knightMove (N (xCurr,yCurr)) (xMove,yMove)	| (getNumber xCurr) + 1 == (getNumber xMove) && yCurr + 2 == yMove = True --topmost right
											| (getNumber xCurr) + 1 == (getNumber xMove) && yCurr - 2 == yMove = True	--bottommost right
											| (getNumber xCurr) - 1 == (getNumber xMove) && yCurr + 2 == yMove = True --topmost left
											| (getNumber xCurr) - 1 == (getNumber xMove) && yCurr - 2 == yMove = True --bottommost left
											| (getNumber xCurr) + 2 == (getNumber xMove) && yCurr + 1 == yMove = True --lesstop right
											| (getNumber xCurr) + 2 == (getNumber xMove) && yCurr - 1 == yMove = True	--lessbottom right
											| (getNumber xCurr) - 2 == (getNumber xMove) && yCurr + 1 == yMove = True --lesstop left
											| (getNumber xCurr) - 2 == (getNumber xMove) && yCurr - 1 == yMove = True --lessbottom left
											| otherwise = False
											
checkUp (xCurr,yCurr) (xMove,yMove) board	| yCurr==yMove = True
											| isFree board (xCurr,yCurr) = checkUp (xCurr,(yCurr+1)) (xMove,yMove) board
											| otherwise = False

checkDown (xCurr,yCurr) (xMove,yMove) board	| yCurr==yMove = True
											| isFree board (xCurr,yCurr) = checkDown (xCurr,(yCurr-1)) (xMove,yMove) board
											| otherwise = False
											
checkRight (xCurr,yCurr) (xMove,yMove) board	| xCurr==xMove = True
												| isFree board (xCurr,yCurr) = checkRight (succ xCurr,yCurr) (xMove,yMove) board
												| otherwise = False		
	
checkLeft (xCurr,yCurr) (xMove,yMove) board	| xCurr==xMove = True
											| isFree board (xCurr,yCurr) = checkLeft (succ xCurr,yCurr) (xMove,yMove) board
											| otherwise = False	
rookMove:: Piece -> Location -> Board -> Bool	
rookMove (R (xCurr,yCurr)) (xMove,yMove) (_,wh,bl)	| (getNumber xCurr) == (getNumber xMove) && (yCurr - yMove)<0 = checkUp (xCurr,(yCurr+1)) (xMove,yMove) (wh++bl) --move is upwards
													| (getNumber xCurr) == (getNumber xMove) && (yCurr - yMove)>0 = checkDown (xCurr,(yCurr-1)) (xMove,yMove) (wh++bl)--move is downwards
													| yCurr == yMove && ((getNumber xCurr) - (getNumber xMove))<0 = checkRight (succ xCurr,yCurr) (xMove,yMove) (wh++bl)
													| yCurr == yMove && ((getNumber xCurr) - (getNumber xMove))<0 = checkLeft (pred xCurr,yCurr) (xMove,yMove) (wh++bl)
													| otherwise = False 

checkUpperRightDiagonal (xCurr,yCurr) (xMove,yMove) board	| yMove==yCurr && xCurr==xMove = True
															| isFree board (xCurr,yCurr) = checkUpperRightDiagonal (succ xCurr,yCurr+1) (xMove,yMove) board
															| otherwise = False 
checkLowerLeftDiagonal (xCurr,yCurr) (xMove,yMove) board	| yMove==yCurr && xCurr==xMove = True
															| isFree board (xCurr,yCurr) = checkLowerLeftDiagonal (pred xCurr,yCurr-1) (xMove,yMove) board
															| otherwise = False 
checkUpperLeftDiagonal (xCurr,yCurr) (xMove,yMove) board	| yMove==yCurr && xCurr==xMove = True
															| isFree board (xCurr,yCurr) = checkUpperLeftDiagonal (pred xCurr,yCurr+1) (xMove,yMove) board
															| otherwise = False 
checkLowerRightDiagonal (xCurr,yCurr) (xMove,yMove) board	| yMove==yCurr && xCurr==xMove = True
															| isFree board (xCurr,yCurr) = checkLowerRightDiagonal (succ xCurr,yCurr-1) (xMove,yMove) board
															| otherwise = False 
bishopMove:: Piece -> Location -> Board -> Bool
bishopMove (B (xCurr,yCurr)) (xMove,yMove) (_,wh,bl) | abs ((getNumber xCurr) - (getNumber xMove)) /= abs (yCurr - yMove) = False
													 | ((getNumber xCurr) - (getNumber xMove)) <0 && (yCurr - yMove) >0 = checkLowerRightDiagonal (succ xCurr,yCurr-1) (xMove, yMove) (wh++bl)
													 | ((getNumber xCurr) - (getNumber xMove)) <0 && (yCurr - yMove) <0 = checkUpperRightDiagonal (succ xCurr,yCurr+1) (xMove, yMove) (wh++bl)
													 | ((getNumber xCurr) - (getNumber xMove)) >0 && (yCurr - yMove) >0 = checkLowerLeftDiagonal (pred xCurr,yCurr-1) (xMove, yMove) (wh++bl)
													 | ((getNumber xCurr) - (getNumber xMove)) >0 && (yCurr - yMove) <0 = checkUpperLeftDiagonal (pred xCurr,yCurr+1) (xMove, yMove) (wh++bl)
													 | otherwise = False
queenMove:: Piece -> Location -> Board -> Bool
queenMove (Q (xCurr,yCurr)) (xMove,yMove) (_,wh,bl)	| abs (yCurr - yMove) <= 1 && abs(getNumber xCurr - getNumber xMove) <=1 = True -- check adjacency
													| (getNumber xCurr) == (getNumber xMove) && (yCurr - yMove)<0 = checkUp (xCurr,(yCurr+1)) (xMove,yMove) (wh++bl) --move is upwards
													| (getNumber xCurr) == (getNumber xMove) && (yCurr - yMove)>0 = checkDown (xCurr,(yCurr-1)) (xMove,yMove) (wh++bl)--move is downwards
													| yCurr == yMove && ((getNumber xCurr) - (getNumber xMove))<0 = checkRight (succ xCurr,yCurr) (xMove,yMove) (wh++bl) --move is right
													| yCurr == yMove && ((getNumber xCurr) - (getNumber xMove))<0 = checkLeft (pred xCurr,yCurr) (xMove,yMove) (wh++bl) --move is left
													| abs ((getNumber xCurr) - (getNumber xMove)) /= abs (yCurr - yMove) = False --move is one of the diagonals if this is true
													| ((getNumber xCurr) - (getNumber xMove)) <0 && (yCurr - yMove) >0 = checkLowerRightDiagonal (succ xCurr,yCurr-1) (xMove, yMove) (wh++bl)
													| ((getNumber xCurr) - (getNumber xMove)) <0 && (yCurr - yMove) <0 = checkUpperRightDiagonal (succ xCurr,yCurr+1) (xMove, yMove) (wh++bl)
													| ((getNumber xCurr) - (getNumber xMove)) >0 && (yCurr - yMove) >0 = checkLowerLeftDiagonal (pred xCurr,yCurr-1) (xMove, yMove) (wh++bl)
													| ((getNumber xCurr) - (getNumber xMove)) >0 && (yCurr - yMove) <0 = checkUpperLeftDiagonal (pred xCurr,yCurr+1) (xMove, yMove) (wh++bl)
													| otherwise = False

--in order to know if a move is legal, first we must check if the cell is either free or occupied by the opponents' piece, as if its our own then we cannot move there (enemy pieces can be taken)
--after that, we check if thats a possible move our piece can take ex: bishop can only go diagnoals, so if the move is forward, then it is not legal
--finally, we check that no piece is blocking our way
isLegal:: Piece -> Board -> Location -> Bool
isLegal currentPiece board nextMove = if (notFree (getColor currentPiece board) board nextMove) then False --check if the place in moving to has another piece of my own color
else if (getPiece currentPiece == 'P') then pawnMove currentPiece nextMove (getColor currentPiece board) board
else if (getPiece currentPiece == 'K') then kingMove currentPiece nextMove 
else if (getPiece currentPiece == 'N') then knightMove currentPiece nextMove
else if (getPiece currentPiece == 'R') then rookMove currentPiece nextMove board
else if (getPiece currentPiece == 'B') then bishopMove currentPiece nextMove board
else if (getPiece currentPiece == 'Q') then queenMove currentPiece nextMove board
else False


--d
suggestMove:: Piece -> Board -> [Location]
suggestMove (curr) (player,w,b) = helpsuggest (curr) (player,w,b) ('a',8)


helpsuggest::Piece -> Board -> Location->[Location]
helpsuggest (curr) board (c,0)=[]
helpsuggest  (curr) board (c,i) 	   |c>'h'=helpsuggest (curr) board ('a',i-1)
									   |isLegal (curr) board (c,i)=(c,i):helpsuggest (curr) board (succ c,i)
									   |otherwise = helpsuggest (curr) board (succ c,i)

--e
movePiece :: Piece -> Location -> Piece
movePiece (P _) loc = (P loc)
movePiece (N _) loc = (N loc)
movePiece (K _) loc = (K loc)
movePiece (Q _) loc = (Q loc)
movePiece (R _) loc = (R loc)
movePiece (B _) loc = (B loc)


replace:: Piece -> Location -> [Piece] -> [Piece]
replace  _ _  [] = []
replace  p loc  (p2:xs)
  | p==p2          = movePiece p loc:replace p loc xs
  | otherwise      = p2:replace p loc xs

move :: Piece -> Location -> Board -> Board
move p loc (player, wpiece, bpiece)
  | player == White && not (p `elem` wpiece) = error "This is White player's turn, Black can't move." 
  | player == Black && not (p `elem` bpiece) = error "This is Black player's turn, White can't move."
  | not (isLegal p (player, wpiece, bpiece) loc) = error ("Illegal move for piece " ++ show p)
  | otherwise = if player == White 
                  then (Black, replace p loc wpiece, bpiece )
                  else (White, wpiece , replace p loc bpiece)

 

