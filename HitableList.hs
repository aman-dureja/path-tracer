module HitableList where
import qualified Hitable as H
import qualified Vec3 as V
import qualified Ray as R
import qualified Lambertian as L

data HitableList = HitableList {
    items :: [H.Hitable],
    hit :: R.Ray -> Float -> Float -> Maybe H.HitRecord
}

hit' :: [H.Hitable] -> R.Ray -> Float -> Float -> Maybe H.HitRecord
hit' [] _ _ _ = Nothing
hit' (x:xs) r tMin tMax = hit'' (x:xs) r tMin tMax defaultRec tMax
    where hit'' :: [H.Hitable] -> R.Ray -> Float -> Float -> H.HitRecord -> Float -> Maybe H.HitRecord 
          hit'' [] _ _ _ record _ = if record == defaultRec then Nothing else Just record
          hit'' (x:xs) r tMin tMax tempRec closestSoFar =
            let newRec = H.hit x r tMin closestSoFar
            in case newRec of Nothing -> hit'' xs r tMin tMax tempRec closestSoFar
                              Just actualRec -> hit'' xs r tMin tMax actualRec (H.t actualRec)
          defaultRec = H.HitRecord { H.t=0, H.p=V.Vec3(0,0,0), H.normal=V.Vec3(0,0,0), H.material=L.lambertian (V.Vec3(0,0,0)) }

hitableList :: [H.Hitable] -> HitableList
hitableList hitables = HitableList {
    items = hitables,
    hit = hit' hitables 
}

