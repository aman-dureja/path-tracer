module HitableList where
import Hitable
import qualified Vec3 as V
import qualified Ray as R
import Debug.Trace

instance Hitable a => Hitable [a] where
    hit [] _ _ _ = Nothing
    hit (x:xs) r tMin tMax = hit' (x:xs) r tMin tMax defaultRec tMax
        where hit' [] _ _ _ rec _ = if rec == defaultRec then Nothing else Just rec 
              hit' (x:xs) r tMin tMax tempRec closestSoFar =
                let newRec = hit x r tMin closestSoFar
                in case newRec of Nothing -> hit' xs r tMin tMax tempRec closestSoFar
                                  Just actualRec -> hit' xs r tMin tMax actualRec (t actualRec)
              defaultRec = HitRecord{t=0, p=V.Vec3(0,0,0), normal=V.Vec3(0,0,0)}

