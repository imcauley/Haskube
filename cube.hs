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
mm :: [P] -> P -> P
mm [] _ = (P 0 0 0)
mm ((P rx ry rz):ps) (P x y z) = ma (mm ps (P (x*rx) (y*ry) (z*rz))) (P x y z)

ma :: P -> P -> P
ma (P x1 y1 z1) (P x2 y2 z2) = P (x1+x2) (y1+y2) (z1+z2)

createRotationMatrix :: Char -> Double -> [P]
createRotationMatrix 'X' t = [(P 1 0 0), (P 0 (cos t) (-(sin t))), (P 0 (sin t) (cos t))]
createRotationMatrix 'Y' t = [(P (cos t) 0 (sin t)), (P 0 1 0), (P (-(sin t)) 0 (cos t))]
createRotationMatrix 'Z' t = [(P (cos t) (-(sin t)) 0), (P (sin t) (cos t) 0), (P 0 0 1)]