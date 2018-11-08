data R = R Double Double Double Double deriving (Show)
data P = P Double Double Double deriving (Show)

data Piece = Piece P P R deriving (Show)
data Cube = Cube [Piece] deriving (Show)

cube = Cube [(Piece (P 0 0 0) (P 0 0 0) (R 0 0 0 0))]

--                    Face    Direction
rotateFace :: Cube -> Char -> Int -> Cube
rotateFace c 'F' 0 = c

mm :: [P] -> P -> P
mm [] _ = (P 0 0 0)
mm ((P rx ry rz):ps) (P x y z) = ma (mm ps (P (x*rx) (y*ry) (z*rz))) (P x y z)

ma :: P -> P -> P
ma (P x1 y1 z1) (P x2 y2 z2) = P (x1+x2) (y1+y2) (z1+z2)

createRotationMatrix :: Char -> Double -> [P]
createRotationMatrix 'X' t = [(P 1 0 0), (P 0 (cos t) (-(sin t))), (P 0 (sin t) (cos t))]
createRotationMatrix 'Y' t = [(P (cos t) 0 (sin t)), (P 0 1 0), (P (-(sin t)) 0 (cos t))]
createRotationMatrix 'Z' t = [(P (cos t) (-(sin t)) 0), (P (sin t) (cos t) 0), (P 0 0 1)]