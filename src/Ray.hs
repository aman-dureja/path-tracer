module Ray where
import qualified Vec3 as V

data Ray = Ray (V.Vec3, V.Vec3)

origin :: Ray -> V.Vec3
origin (Ray (a,_)) = a

direction :: Ray -> V.Vec3
direction (Ray (_,b)) = b

pointAtParameter :: Ray -> Float -> V.Vec3
pointAtParameter (Ray (a,b)) t = V.add a (V.scalarMul b t)

hitSphere :: V.Vec3 -> Float -> Ray -> Float 
hitSphere center radius r = let oc = V.sub (origin r) center
                                a = V.dot (direction r) (direction r)
                                b = (V.dot oc (direction r)) * 2.0
                                c = (V.dot oc oc) - radius*radius
                                discriminant = b*b - 4*a*c
                            in if discriminant < 0 then -1.0
                               else (-b - sqrt discriminant) / (2.0 * a)

color :: Ray -> V.Vec3
color r = let unitDirection = V.unitVector $ direction r
              t1 = 0.5 * (V.y unitDirection + 1.0)
              t0 = hitSphere (V.Vec3(0,0,-1)) 0.5 r 
              n = V.unitVector (V.sub (pointAtParameter r t0) (V.Vec3(0,0,-1)))
          in if t0 > 0.0 then V.scalarMul (V.Vec3(1 + V.x n, 1 + V.y n, 1 + V.z n)) 0.5 
             else V.add (V.scalarMul (V.Vec3(1.0, 1.0, 1.0)) (1.0 - t1)) (V.scalarMul (V.Vec3(0.5, 0.7, 1.0)) t1) 

