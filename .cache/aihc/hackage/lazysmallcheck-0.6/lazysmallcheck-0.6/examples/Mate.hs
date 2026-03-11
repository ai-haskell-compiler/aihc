module Mate where

import Test.LazySmallCheck
import List

data Kind = King | Queen | Rook | Bishop | Knight | Pawn
  deriving (Eq, Show)

data Colour = Black | White
  deriving (Eq, Show)

type Piece = (Colour,Kind)
type Square = (Int,Int)

data Board = Board
		[(Kind,Square)] -- white
		[(Kind,Square)] -- black
  deriving Show

pieceAt :: Board -> Square -> Maybe Piece
pieceAt (Board wkss bkss) sq =
        pieceAtWith White (pieceAtWith Black Nothing bkss) wkss
	where
	pieceAtWith c n [] = n
	pieceAtWith c n ((k,s):xs) = if s==sq then Just (c,k) else pieceAtWith c n xs

emptyAtAll :: Board -> (Square->Bool) -> Bool
emptyAtAll (Board wkss bkss) e =
	emptyAtAllAnd (emptyAtAllAnd True bkss) wkss
	where
	emptyAtAllAnd b []         = b
	emptyAtAllAnd b ((_,s):xs) = not (e s) && emptyAtAllAnd b xs

rmPieceAt White sq (Board wkss bkss) = Board (rPa sq wkss) bkss
rmPieceAt Black sq (Board wkss bkss) = Board wkss (rPa sq bkss)

rPa sq (ks@(k,s):kss) = if s==sq then kss else ks : rPa sq kss

putPieceAt sq (White,k) (Board wkss bkss) = Board ((k,sq):wkss) bkss
putPieceAt sq (Black,k) (Board wkss bkss) = Board wkss ((k,sq):bkss)

kingSquare :: Colour -> Board -> Square
kingSquare White (Board kss _) = kSq kss
kingSquare Black (Board _ kss) = kSq kss

kSq ((King,s):_)   = s
kSq (       _:kss) = kSq kss 

opponent Black = White
opponent White = Black

colourOf :: Piece -> Colour
colourOf (c,_) = c

kindOf :: Piece -> Kind
kindOf (_,k) = k

onboard :: Square -> Bool
onboard (p,q) = 1<=p && p<=8 && 1<=q && q<=8

forcesColoured White (Board kss _) = kss
forcesColoured Black (Board _ kss) = kss

emptyBoard = Board [] []

data Move = Move 
    Square    -- to here
    (Maybe Piece) -- capturing this
    (Maybe Piece) -- gaining promotion to this
    
data MoveInFull = MoveInFull Piece Square Move

tryMove :: Colour -> (Kind,Square) -> Move -> Board -> Maybe (MoveInFull,Board)
tryMove c ksq@(k,sq) m@(Move sq' mcp mpp) bd =
  if not (kingincheck c bd2) then Just (MoveInFull p sq m, bd2)
  else Nothing 
  where
  p   =   (c,k)
  bd1 = rmPieceAt c sq bd
  p'  = maybe p id mpp
  bd2 = maybe (putPieceAt sq' p' bd1)
          (const (putPieceAt sq' p' (rmPieceAt (opponent c) sq' bd1)))
          mcp

moveDetailsFor :: Colour -> Board -> [(MoveInFull,Board)]
moveDetailsFor c bd =
  foldr ( \ksq ms ->
    foldr (\rm ms' -> maybe id (:) (tryMove c ksq rm bd) ms')
                   ms
                   (rawmoves c ksq bd) )
        []
              (forcesColoured c bd)


-- NB raw move = might illegally leave the king in check.
rawmoves :: Colour -> (Kind,Square) -> Board -> [Move]
rawmoves c (k,sq) bd = m c sq bd
	where
        m = case k of
	    King   -> kingmoves
	    Queen  -> queenmoves
	    Rook   -> rookmoves
	    Bishop -> bishopmoves
	    Knight -> knightmoves
	    Pawn   -> pawnmoves

bishopmoves :: Colour -> Square -> Board -> [Move]
bishopmoves c sq bd =
	( moveLine bd c sq (\(x,y) -> (x-1,y+1)) $
	  moveLine bd c sq (\(x,y) -> (x+1,y+1)) $
	  moveLine bd c sq (\(x,y) -> (x-1,y-1)) $
	  moveLine bd c sq (\(x,y) -> (x+1,y-1)) id
        ) []

rookmoves :: Colour -> Square -> Board -> [Move]
rookmoves c sq bd =
	( moveLine bd c sq (\(x,y) -> (x-1,y)) $
	  moveLine bd c sq (\(x,y) -> (x+1,y)) $
	  moveLine bd c sq (\(x,y) -> (x,y-1)) $
	  moveLine bd c sq (\(x,y) -> (x,y+1)) id
        ) []

moveLine :: Board -> Colour -> Square -> (Square->Square) -> ([Move]->a) -> [Move] -> a
moveLine bd c sq inc cont = ml sq
	where
	ml sq ms =
		let sq' = inc sq in
		if onboard sq' then
			case pieceAt bd sq' of
			Nothing -> ml sq' (Move sq' Nothing Nothing : ms)
			Just p' -> if colourOf p' /= c then
					cont (Move sq' (Just p') Nothing : ms)
                                   else cont ms
		else cont ms

kingmoves :: Colour -> Square -> Board -> [Move]
kingmoves c (p,q) bd =
	sift c bd []     [(p-1,q+1), (p,q+1), (p+1,q+1),
	  	 	  (p-1,q),            (p+1,q),
		 	  (p-1,q-1), (p,q-1), (p+1,q-1)]

knightmoves :: Colour -> Square -> Board -> [Move]
knightmoves c (p,q) bd =
	sift c bd [] [	  	 (p-1,q+2),(p+1,q+2),
			  (p-2,q+1),		  (p+2,q+1),
                          (p-2,q-1),		  (p+2,q-1),
		  		 (p-1,q-2),(p+1,q-2) ]

sift :: Colour -> Board -> [Move] -> [Square] -> [Move]
sift _ _  ms [] = ms
sift c bd ms (sq:sqs) =
	if onboard sq then
		case pieceAt bd sq of
                Nothing -> sift c bd (Move sq Nothing Nothing : ms) sqs
		Just p' -> if colourOf p' == c then sift c bd ms sqs
                           else sift c bd (Move sq (Just p') Nothing : ms) sqs
	else sift c bd ms sqs

pawnmoves :: Colour -> Square -> Board -> [Move]
pawnmoves c (p,q) bd = movs ++ caps
	where
	movs =	let on1 = (p,q+fwd)
		    on2 = (p,q+2*fwd) in
		if pieceAt bd on1 == Nothing then
			promote on1 Nothing ++
			if (q==2 && c==White || q==7 && c==Black) &&
			 	pieceAt bd on2 == Nothing then [Move on2 Nothing Nothing] 
			else []
		else []
	caps =	concat [ promote sq mcp
                       | sq <- [(p+1,q+fwd), (p-1,q+fwd)],
                         mcp@(Just p') <- [pieceAt bd sq], colourOf p'/=c ]
	fwd  =	case c of
       		White -> 1
		Black -> -1
	promote sq@(x,y) mcp =  
		if (c==Black && y==1 || c==White && y==8) then
			map (Move sq mcp . Just)
			    [(c,Queen), (c,Rook), (c,Bishop), (c,Knight)]
		else [Move sq mcp Nothing]

queenmoves :: Colour -> Square -> Board -> [Move]
queenmoves c sq bd = bishopmoves c sq bd ++ rookmoves c sq bd

kingincheck :: Colour -> Board -> Bool
kingincheck c bd =
	any givesCheck (forcesColoured (opponent c) bd)
	where
	givesCheck (k,(x,y)) = kthreat k
		where
		kthreat King =
			abs (x-xk) <= 1 && abs (y-yk) <= 1
		kthreat Queen =
			kthreat Rook || kthreat Bishop
		kthreat Rook =
			x==xk &&
                        emptyAtAll bd (\(xe,ye) -> xe==xk && min y yk < ye && ye < max y yk) ||
			y==yk &&
                        emptyAtAll bd (\(xe,ye) -> ye==yk && min x xk < xe && xe < max x xk)
		kthreat	Bishop =
			x+y==xk+yk &&
			emptyAtAll bd (\(xe,ye) -> xe+ye==xk+yk && min x xk < xe && xe < max x xk) ||
			x-y==xk-yk &&
			emptyAtAll bd (\(xe,ye) -> xe-ye==xk-yk && min x xk < xe && xe < max x xk)
		kthreat	Knight =
			abs (x-xk) == 2 && abs (y-yk) == 1 ||
			abs (x-xk) == 1 && abs (y-yk) == 2
		kthreat Pawn =
			abs (x-xk) == 1 &&
			case c of
			Black -> yk == y+1
			White -> yk == y-1
	(xk,yk) = kingSquare c bd

checkmate :: Colour -> Board -> Bool
checkmate col b = null (moveDetailsFor col b) && kingincheck col b

-- Board generator

allDiff [] = True
allDiff (x:xs) = x `notElem` xs && allDiff xs

onBoard (p, q) = 1 <= p && p <= 8 && 1 <= q && q <= 8

one p [] = False
one p (x:xs) = if p x then all (not . p) xs else one p xs

kingsDontTouch ws bs =
     (bx > succ wx || wx > succ bx || by > succ wy || wy > succ by)
  where
    (wx, wy) = kSq ws
    (bx, by) = kSq bs

validBoard (Board ws bs) =
     one ((== King) . fst) ws
  && one ((== King) . fst) bs
  && all onBoard sqs
  && kingsDontTouch ws bs
  && allDiff sqs
  where
    sqs = map snd (ws ++ bs)

-- Property

infixr 0 -->
False --> _ = True
True --> x = x

prop_checkmate b = 
      (  length ws == 2
      && Pawn `elem` (map fst ws)
      && validBoard b
      )
  ==> not (checkmate Black b)
  where
    ws = forcesColoured White b
