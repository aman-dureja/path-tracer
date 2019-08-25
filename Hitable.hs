module Hitable where
import qualified Ray as R
import qualified Vec3 as V

data HitRecord = HitRecord { t        :: Float
                           , p        :: V.Vec3
                           , normal   :: V.Vec3
                           , material :: Material
                           }

instance Eq HitRecord where
    (HitRecord { t=t0, p=p0, normal=n0, material=m0 }) == (HitRecord { t=t1, p=p1, normal=n1, material=m1 }) =
        t0 == t1 && p0 == p1 && n0 == n1

data Hitable = Hitable {
    hit :: R.Ray -> Float -> Float -> Maybe HitRecord
}

data Material = Material { scatter :: R.Ray -> HitRecord -> Maybe (V.Vec3, R.Ray) }

