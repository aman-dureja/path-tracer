module Camera where
import qualified Vec3 as V
import qualified Ray as R

data Camera = Camera { lowerLeftCorner :: V.Vec3
                     , horizontal :: V.Vec3
                     , vertical :: V.Vec3
                     , origin :: V.Vec3 }

getRay :: Camera -> Float -> Float -> R.Ray
getRay c u v = R.Ray (origin c, foldl V.add (V.Vec3(0,0,0)) [lowerLeftCorner c, V.scalarMul (horizontal c) u, V.scalarMul (vertical c) v, V.scalarMul (origin c) (-1)])

