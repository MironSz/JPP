module Mon where

infixl 5 ><

class Mon m where
  m1 :: m
  (><) :: m -> m -> m

-- ** Properties:
-- * leftUnit x = m1 >< x == x
-- * rightUnit x =  x >< m1 == x
-- * assoc x y z = (x >< y) >< z == x >< (y >< z)
type R = Rational

-- typ R objaśniony w tekście poniżej
type R2 = (R, R)

data Vec =
  Vec R2 -- wektor 2D

data Point =
  Point R2 -- punkt 2D

vectorToPoint :: Vec -> Point
vectorToPoint (Vec r) = Point r

pointToVector :: Point -> Vec
pointToVector (Point r) = Vec r

instance Eq Vec where
  (==) (Vec (a, b)) (Vec (a2, b2)) = (a == a2) && (b == b2)

instance Eq Point where
  (==) p1 p2 = pointToVector (p1) == pointToVector (p2)

instance Show Vec where
  show (Vec (a, b)) = "Vec (" ++ show (a) ++ "," ++ show (b) ++ ")"

instance Show Point where
  show (Point (a, b)) = "Point (" ++ show (a) ++ "," ++ show (b) ++ ")"

point :: R2 -> Point
point = Point

pointToIntWithScale :: Int -> Point -> (Int, Int)
pointToIntWithScale s (Point (x, y)) = (round (toRational s * x), round (toRational s * y))

vec :: R2 -> Vec
vec = Vec

data Matrix =
  Matrix R
         R
         R
         R
instance Show Matrix where
  show (Matrix a b c d) = "Matrix ("++ show a++", "++ show b++", "++show c++", "++show d ++")"
addVec :: Vec -> Vec -> Vec
addVec (Vec (a, b)) (Vec (a2, b2)) = Vec (a + a2, b + b2)

matmulmat :: Matrix -> Matrix -> Matrix
matmulmat (Matrix a11 a12 a21 a22) (Matrix b11 b12 b21 b22) =
  Matrix (a11 * b11 + a12 * b21) (a11 * b12 + a12 * b22) (a21 * b11 + a22 * b21) (a21 * b12 + a22 * b22)

matmulvec :: Matrix -> Vec -> Vec
matmulvec (Matrix a11 a12 a21 a22) (Vec (b1, b2)) = Vec (a11 * b1 + a12 * b2, a21 * b1 + a22 * b2)

matmul :: Matrix -> Vec -> Vec
matmul (Matrix a11 a12 a21 a22) (Vec (b1, b2)) = Vec (a11 * b1 + a12 * b2, a21 * b1 + a22 * b2)

onemat :: Matrix
onemat = Matrix 1 0 0 1

instance Mon Vec where
  m1 = Vec (0, 0)
  (><) (Vec (x1, y1)) (Vec (x2, y2)) = Vec (x1 + x2, y1 + y2)

data Line =
  Line Point
       Point

data Picture
  = Simple Line
  | Rectangle Line
              Line
              Line
              Line
  | Complex Picture
            Picture
  | Transformed Transform
                Picture

-- odcinek pomiędzy punktami o podanych współrzędnych
line :: (R, R) -> (R, R) -> Picture
line (a, b) (c, d) = Simple (Line (point (a, b)) (point (c, d)))

lline :: R -> R -> R -> R -> Line
lline a b c d = Line (point (a, b)) (point (c, d))

-- prostokąt o podanej szerokości i wysokości zaczepiony w (0,0)
rectangle :: R -> R -> Picture
rectangle w h = Rectangle (lline 0 0 0 h) (lline 0 h w h) (lline w h w 0) (lline w 0 0 0)

-- suma (nałożenie) dwóch rysunków
(&) :: Picture -> Picture -> Picture
(&) = Complex

type IntLine = ((Int, Int), (Int, Int))

type IntRendering = [IntLine]

--TODO make it tail recursion
rRenderScaled :: Int -> Picture -> IntRendering -> Transform -> IntRendering
rRenderScaled s (Complex p1 p2) acc f = rRenderScaled s p1 (rRenderScaled s p2 acc f) f
rRenderScaled s (Transformed t p) acc f = rRenderScaled s p acc (f >< t)
rRenderScaled s (Simple (Line p1 p2)) acc f = (pointToIntWithScale s p12, pointToIntWithScale s p22) : acc
  where
    p12 = trpoint f p1
    p22 = trpoint f p2
rRenderScaled s (Rectangle l1 l2 l3 l4) acc f = rRenderScaled s (Simple l1) acc1 f where
  acc1 = rRenderScaled s (Simple l2) acc2 f where
  acc2 = rRenderScaled s (Simple l3) acc3 f where
  acc3 = rRenderScaled s (Simple l4) acc f



renderScaled :: Int -> Picture -> IntRendering
renderScaled s p = rRenderScaled s p [] (m1::Transform)

--renderScaled s (Simple (Line p1 p2)) = [(pointToIntWithScale s p1, pointToIntWithScale s p2)]
--renderScaled s (Rectangle l1 l2 l3 l4) =
--  renderScaled s (Simple l1) ++ renderScaled s (Simple l2) ++ renderScaled s (Simple l3) ++ renderScaled s (Simple l4)
--renderScaled s (Complex p1 p2) = renderScaled s p1 ++ renderScaled s p2

myPi :: R
myPi = toRational 314/ toRational 100

mySin :: R -> R
mySin x = (toRational 16 * (myPi - x)) / (toRational 5 * myPi * myPi - 4 * x * (myPi - x))

myCos :: R -> R
myCos x = (myPi * myPi - 4 * x * x) / (myPi * myPi + x * x)

data Transform =
  F Matrix
    Vec

translate :: Vec -> Transform
translate = F onemat

rotate :: R -> Transform
rotate x = F (Matrix (myCos x) (-1 * mySin x) (mySin x) (myCos x)) m1

fullCircle :: R -- wartość odpowiadająca 1 pełnemu obrotowi (360 stopni)
fullCircle = 2 * myPi

instance Mon Transform where
  m1 = F onemat (Vec (0, 0))
  (><) (F a b) (F a2 b2) = F (matmulmat a a2) ((matmulvec a2 b) >< b2)

trvec :: Transform -> Vec -> Vec
trvec (F m b) v = addVec (matmulvec m v) b

trpoint :: Transform -> Point -> Point
trpoint t p = vectorToPoint (trvec t (pointToVector p))

transform :: Transform -> Picture -> Picture
transform t (Transformed t2 p) = Transformed (t >< t2) p
transform t p                  = Transformed t p
