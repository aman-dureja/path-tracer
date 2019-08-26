module Camera where
import qualified Vec3 as V
import qualified Ray as R

data Camera = Camera { lowerLeftCorner :: V.Vec3
                     , horizontal :: V.Vec3
                     , vertical :: V.Vec3
                     , origin :: V.Vec3 }

getRay :: Camera -> Float -> Float -> R.Ray
getRay c u v = R.Ray (origin c, foldl V.add (V.Vec3(0,0,0)) [lowerLeftCorner c, V.scalarMul (horizontal c) u, V.scalarMul (vertical c) v, V.scalarMul (origin c) (-1)])

camera :: V.Vec3 -> V.Vec3 -> V.Vec3 -> Float -> Float -> Camera
camera lookfrom lookat vup vfov aspect =
    let theta = vfov * pi / 180
        halfHeight = tan (theta / 2)
        halfWidth = aspect * halfHeight
        w = V.unitVector (V.sub lookfrom lookat)
        u = V.unitVector (V.cross vup w)
        v = V.cross w u
    in Camera { lowerLeftCorner = V.sub (V.sub lookfrom (V.scalarMul u halfWidth)) (V.add (V.scalarMul v halfHeight) w)
              , horizontal = V.scalarMul u (2 * halfWidth)
              , vertical = V.scalarMul v (2 * halfHeight)
              , origin = lookfrom }

