module Vec3 where

data Vec3 = Vec3 (Float, Float, Float) deriving (Eq, Show)

x :: Vec3 -> Float
x (Vec3 (a,_,_)) = a

y :: Vec3 -> Float
y (Vec3 (_,a,_)) = a

z :: Vec3 -> Float
z (Vec3 (_,_,a)) = a

r :: Vec3 -> Float
r = x

g :: Vec3 -> Float
g = y

b :: Vec3 -> Float
b = z

neg :: Vec3 -> Vec3
neg (Vec3 (a,b,c)) = Vec3 (-1*a, -1*b, -1*c)

atIndex :: Vec3 -> Int -> Maybe Float
atIndex (Vec3 (a,_,_)) 0 = Just a
atIndex (Vec3 (_,a,_)) 1 = Just a
atIndex (Vec3 (_,_,a)) 2 = Just a
atIndex _ _ = Nothing 

add :: Vec3 -> Vec3 -> Vec3
add (Vec3 (a1,b1,c1)) (Vec3 (a2,b2,c2)) = Vec3 (a1+a2, b1+b2, c1+c2)

sub :: Vec3 -> Vec3 -> Vec3
sub (Vec3 (a1,b1,c1)) (Vec3 (a2,b2,c2)) = Vec3 (a1-a2, b1-b2, c1-c2)

vecMul :: Vec3 -> Vec3 -> Vec3
vecMul (Vec3 (a1,b1,c1)) (Vec3 (a2,b2,c2)) = Vec3 (a1*a2, b1*b2, c1*c2)

vecDiv :: Vec3 -> Vec3 -> Vec3
vecDiv (Vec3 (a1,b1,c1)) (Vec3 (a2,b2,c2)) = Vec3 (a1/a2, b1/b2, c1/c2)

scalarMul :: Vec3 -> Float -> Vec3
scalarMul (Vec3 (a,b,c)) n = Vec3 (a*n, b*n, c*n)

scalarDiv :: Vec3 -> Float -> Vec3
scalarDiv (Vec3 (a,b,c)) n = Vec3 (a/n, b/n, c/n)

squaredLength :: Vec3 -> Float
squaredLength (Vec3 (a,b,c)) = a*a + b*b + c*c

len :: Vec3 -> Float
len v = sqrt $ squaredLength v

unitVector :: Vec3 -> Vec3
unitVector v = scalarDiv v (len v)

dot :: Vec3 -> Vec3 -> Float
dot (Vec3 (a1,b1,c1)) (Vec3 (a2,b2,c2)) = a1*a2 + b1*b2 + c1*c2

cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 (a1,b1,c1)) (Vec3 (a2,b2,c2)) = Vec3 (b1*c2 - c1*b2, -1*(a1*c2 - c1*a2), a1*b2 - b1*a2)

transform :: (Float -> Float) -> Vec3 -> Vec3
transform f (Vec3 (a,b,c)) = Vec3 (f a, f b, f c)

