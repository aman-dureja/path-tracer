module Sphere where
import Hitable 
import qualified Ray as R
import qualified Vec3 as V

data Sphere = Sphere V.Vec3 Float

instance Hitable Sphere where
    hit (Sphere center radius) r tMin tMax = 
        let oc = V.sub (R.origin r) center
            a = V.dot (R.direction r) (R.direction r)
            b = V.dot oc (R.direction r)
            c = (V.dot oc oc) - radius*radius
            discriminant = b*b - a*c
            temp0 = (-1*b - sqrt (b*b - a*c)) / a
            temp1 = (-1*b + sqrt (b*b - a*c)) / a
            p0 = R.pointAtParameter r temp0
            p1 = R.pointAtParameter r temp1
            rec0 = HitRecord {t = temp0, p = p0, normal = V.scalarDiv (V.sub p0 center) radius}
            rec1 = HitRecord {t = temp1, p = p1, normal = V.scalarDiv (V.sub p1 center) radius}
        in if discriminant <= 0 then Nothing 
           else if temp0 < tMax && temp0 > tMin then Just rec0
           else if temp1 < tMax && temp1 > tMin then Just rec1
           else Nothing

