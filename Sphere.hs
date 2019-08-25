module Sphere where
import Hitable as H 
import qualified Ray as R
import qualified Vec3 as V

randomInUnitSphere :: Maybe V.Vec3 -> [Float] -> (V.Vec3, [Float])
randomInUnitSphere maybeVec randomNums =
    let randX = head randomNums
        randY = (head . tail) randomNums
        randZ = (head . tail . tail) randomNums
        newVec = Just (V.Vec3(randX, randY, randZ))
    in case maybeVec of
        Nothing -> randomInUnitSphere newVec ((tail . tail . tail) randomNums)
        Just vec -> if V.squaredLength vec < 1 then (vec, (tail . tail . tail) randomNums)
                    else randomInUnitSphere newVec ((tail . tail . tail) randomNums)

hit :: V.Vec3 -> Float -> H.Material -> R.Ray -> Float -> Float -> Maybe H.HitRecord
hit center radius material r tMin tMax =
    let oc = V.sub (R.origin r) center
        a = V.dot (R.direction r) (R.direction r)
        b = V.dot oc (R.direction r)
        c = (V.dot oc oc) - radius*radius
        discriminant = b*b - a*c
        temp0 = (-1*b - sqrt (b*b - a*c)) / a
        temp1 = (-1*b + sqrt (b*b - a*c)) / a
        p0 = R.pointAtParameter r temp0
        p1 = R.pointAtParameter r temp1
        rec0 = H.HitRecord { H.t = temp0, H.p = p0, H.normal = V.scalarDiv (V.sub p0 center) radius, H.material = material }
        rec1 = H.HitRecord { H.t = temp1, H.p = p1, H.normal = V.scalarDiv (V.sub p1 center) radius, H.material = material }
    in if discriminant <= 0 then Nothing 
       else if temp0 < tMax && temp0 > tMin then Just rec0
       else if temp1 < tMax && temp1 > tMin then Just rec1
       else Nothing

sphere :: V.Vec3 -> Float -> H.Material -> H.Hitable
sphere center radius material = H.Hitable {
    H.hit = Sphere.hit center radius material
}

