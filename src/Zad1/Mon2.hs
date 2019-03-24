module Mon2 where

--------Moje "prywatne" funkcje i deklaracje. Gdybyśmy mogli wysłać więcej plików, znalazłyby się w osobnym module -----
vectorToPoint :: Vec -> Point
vectorToPoint (Vec r) = Point r

pointToVector :: Point -> Vec
pointToVector (Point r) = Vec r

pointToIntWithScale :: Int -> Point -> (Int, Int)
pointToIntWithScale s (Point (x, y)) = (round (toRational s * x), round (toRational s * y))

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

instance Mon RawTransform where
  m1 = F onemat (Vec (0, 0))
  (><) (F a b) (F a2 b2) = F (matmulmat a a2) ((matmulvec a2 b) >< b2)

toRawTransform :: Transform -> RawTransform
toRawTransform Identity = F onemat (Vec (0, 0))
toRawTransform (Move v ts) = (rawTranslate v) >< (toRawTransform ts)
toRawTransform (Rotate alfa ts) = (rawRotate alfa) >< (toRawTransform ts)

lline :: R -> R -> R -> R -> Line
lline a b c d = Line (point (a, b)) (point (c, d))

rRenderScaled :: Int -> Picture -> IntRendering -> RawTransform -> IntRendering
rRenderScaled s (Complex p1 p2) acc f = rRenderScaled s p1 (rRenderScaled s p2 acc f) f
rRenderScaled s (Transformed t p) acc f = rRenderScaled s p acc (f >< (toRawTransform t))
rRenderScaled s (Simple (Line p1 p2)) acc f = (pointToIntWithScale s p12, pointToIntWithScale s p22) : acc
  where
    p12 = rawTrpoint f p1
    p22 = rawTrpoint f p2
rRenderScaled s (Rectangle l1 l2 l3 l4) acc f = rRenderScaled s (Simple l1) acc1 f where
  acc1 = rRenderScaled s (Simple l2) acc2 f where
  acc2 = rRenderScaled s (Simple l3) acc3 f where
  acc3 = rRenderScaled s (Simple l4) acc f


data RawTransform =
  F Matrix
    Vec

myPi :: R
myPi = toRational 314159/ toRational 100000

mySin :: R -> R
mySin x = ((toRational 16 )*x* (myPi - x)) / (toRational 5 * myPi * myPi - 4 * x * (myPi - x))

myCos :: R -> R
myCos x = (myPi * myPi - 4 * x * x) / (myPi * myPi + x * x)


rawTranslate :: Vec -> RawTransform
rawTranslate = F onemat

rawRotate :: R -> RawTransform
rawRotate x = F (Matrix (myCos x) (-1 * mySin x) (mySin x) (myCos x)) m1

rawTrvec :: RawTransform -> Vec -> Vec
rawTrvec (F m b) v = addVec (matmulvec m v) b

rawTrpoint :: RawTransform -> Point -> Point
rawTrpoint t p = vectorToPoint (rawTrvec t (pointToVector p))


--------Koniec moich prywatnych deklaracji, początek właściwego rozwiązania


infixl 5 ><

class Mon m where
  m1 :: m
  (><) :: m -> m -> m

type R = Rational

type R2 = (R, R)

data Vec =
  Vec R2 -- wektor 2D

data Point =
  Point R2 -- punkt 2D

instance Eq Transform where
  (==) Identity Identity = True
  (==) (Move v1 t1) (Move v2 t2) = (v1==v2) && (t1==t2)
  (==) (Rotate a1 t1) (Rotate a2 t2) = (a1==a2) && (t1==t2)
  (==) (Rotate _ _) (Move _ _) = False


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

vec :: R2 -> Vec
vec = Vec

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

line :: (R, R) -> (R, R) -> Picture
line (a, b) (c, d) = Simple (Line (point (a, b)) (point (c, d)))

rectangle :: R -> R -> Picture
rectangle w h = Rectangle (lline 0 0 0 h) (lline 0 h w h) (lline w h w 0) (lline w 0 0 0)

(&) :: Picture -> Picture -> Picture
(&) = Complex

type IntLine = ((Int, Int), (Int, Int))

type IntRendering = [IntLine]

renderScaled :: Int -> Picture -> IntRendering
renderScaled s p = rRenderScaled s p [] (m1::RawTransform)

data Transform = Identity | Rotate R Transform | Move Vec Transform

translate :: Vec -> Transform
translate (Vec (0,0)) = Identity
translate v = Move v Identity

rotate :: R -> Transform
rotate 0 = Identity
rotate alfa = Rotate alfa Identity

fullCircle :: R
fullCircle = 2 * myPi

instance Mon Transform where
  m1 = Identity
  (><) Identity t = t
  (><) t Identity = t
  (><) (Rotate alfa1 Identity) (Rotate alfa2 ts) = (rotate (alfa1+alfa2)) >< ts
  (><) (Move v1 Identity) (Move v2 ts) = ( translate (addVec v1 v2)) >< ts
  (><) (Move v Identity) (Rotate alfa ts) = Move v (Rotate alfa ts)
  (><) (Rotate alfa Identity) (Move v ts)  = Rotate alfa (Move v  ts)
  (><) (Rotate alfa t1) t2 = (Rotate alfa Identity) >< (t1 >< t2)
  (><) (Move v t1) t2 = (Move v Identity) >< (t1><t2)

instance Show Transform where
  show Identity = "Identity"
  show (Move v t) = "Move "++show(v)++" "++show(t)
  show (Rotate a t) = "Rotate "++show(a)++" "++show(t)


transform :: Transform -> Picture -> Picture
transform  t (Transformed t2 p) = Transformed (t >< t2) p
transform t p                  = Transformed t p


trvec :: Transform -> Vec -> Vec
trvec t v = rawTrvec (toRawTransform t) v

trpoint :: Transform -> Point -> Point
trpoint t p = rawTrpoint (toRawTransform t) p


