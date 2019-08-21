module Ray where
import qualified Vec3 as V

data Ray = Ray (V.Vec3, V.Vec3)

origin :: Ray -> V.Vec3
origin (Ray (a,_)) = a

direction :: Ray -> V.Vec3
direction (Ray (_,b)) = b

pointAtParameter :: Ray -> Float -> V.Vec3
pointAtParameter (Ray (a,b)) t = V.add a (V.scalarMul b t)

color :: Ray -> V.Vec3
color r = let unitDirection = V.unitVector $ direction r
              t = 0.5 * (V.y unitDirection + 1.0)
          in V.add (V.scalarMul (V.Vec3(1.0, 1.0, 1.0)) (1.0 - t)) (V.scalarMul (V.Vec3(0.5, 0.7, 1.0)) t) 

