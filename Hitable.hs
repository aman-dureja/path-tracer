module Hitable where
import qualified Ray as R
import qualified Vec3 as V

data HitRecord = HitRecord { t      :: Float
                           , p      :: V.Vec3
                           , normal :: V.Vec3
                           } deriving (Eq)

class Hitable a where
    hit :: a -> R.Ray -> Float -> Float -> Maybe HitRecord 

