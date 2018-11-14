data R = R Double Double Double Double deriving (Eq, Show)
data P = P Double Double Double deriving (Eq, Show)

data Piece = Piece P P R deriving (Show)
data Cube = Cube [Piece] deriving (Show)

cube = Cube [(Piece (P 0 0 0) (P 0 0 0) (R 0 0 0 0))]

--                    Face    Direction
rotateFace :: Cube -> Char -> Int -> Cube
rotateFace (Cube pieces) face dir = (Cube (oldPieces ++ newPieces))
    where oldPieces = (filter (not . isPartOfFace face) pieces)
          newPieces = map (rotatePiece (faceToRotation face dir)) (filter (isPartOfFace face) pieces)

rotatePiece :: [P] -> Piece -> Piece
rotatePiece rotation (Piece s p r) = (Piece s (rotatePoint rotation p) r)


rotatePoint :: [P] -> P -> P
rotatePoint rot p = (P (rotated !! 0) (rotated !! 1) (rotated !! 2))
    where rotated = mm rot p

isPartOfFace :: Char -> Piece -> Bool
isPartOfFace 'R' (Piece _ (P x y z) _) = x == 1
isPartOfFace 'L' (Piece _ (P x y z) _) = x == -1
isPartOfFace 'U' (Piece _ (P x y z) _) = z == 1
isPartOfFace 'D' (Piece _ (P x y z) _) = z == -1
isPartOfFace 'B' (Piece _ (P x y z) _) = y == 1
isPartOfFace 'F' (Piece _ (P x y z) _) = y == -1

checkSolved :: Cube -> Bool
checkSolved (Cube []) = True
checkSolved (Cube ((Piece ps p r):pieces)) = ps == p && (checkSolved (Cube pieces))

-- matrix multiply
-- takes in a matrix mxn and a vector mx1 and multiplies them
mm :: [P] -> P -> [Double]
mm [] _ = []
mm ((P rx ry rz):ps) (P x y z) = [rx*x + ry*y + rz*z] ++ (mm ps (P x y z))

ma :: P -> P -> P
ma (P x1 y1 z1) (P x2 y2 z2) = P (x1+x2) (y1+y2) (z1+z2)

mq :: R -> R -> R
mq (R n1 i1 j1 k1) (R n2 i2 j2 k2) = (R n i j k)
    where n = (n1 * n2) - (i1 * i2) - (j1 * j2) - (k1 * k2)
          i = (n1 * i2) + (n2 * i1) + (k2 * j1) - (k1 * j2)
          j = (n1 * j2) + (n2 * j1) - (i1 * k2) + (k1 * i2)
          k = (n1 * k2) + (n2 * k1) + (i1 * j2) - (i2 * j1)