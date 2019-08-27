module Material.Dielectric where
import qualified Ray as R
import qualified Vec3 as V
import qualified Hitable as H
import Material.Metal (reflect)

refract :: V.Vec3 -> V.Vec3 -> Float -> Maybe V.Vec3
refract v n niOverNt =
    let uv = V.unitVector v
        dt = V.dot uv n
        discriminant = 1.0 - (niOverNt * niOverNt * (1 - (dt * dt)))
        refracted = V.sub (V.scalarMul (V.sub uv (V.scalarMul n dt)) niOverNt) (V.scalarMul n (sqrt discriminant))
    in if discriminant <= 0 then Nothing 
       else Just refracted

schlick :: Float -> Float -> Float
schlick cosine refIdx = let r0 = (1 - refIdx) / (1 + refIdx)
                            r1 = r0 * r0
                        in r1 + (1 - r1) * ((1 - cosine) ^^ 5)

dielectric :: Float -> Float -> H.Material
dielectric ri randomNum = H.Material { H.scatter = scatter' }
    where 
        scatter' r record = 
            let reflected = reflect (R.direction r) (H.normal record)
                attenuation = V.Vec3(1,1,1)
                dotProduct = V.dot (R.direction r) (H.normal record)
                outwardNormal = if dotProduct > 0 then (V.scalarMul (H.normal record) (-1)) else (H.normal record)
                niOverNt = if dotProduct > 0 then ri else (1 / ri)
                cosine = if dotProduct > 0 then (ri * (dotProduct / (V.len $ R.direction r))) else ((-1) * (dotProduct / (V.len $ R.direction r)))
                maybeRefracted = refract (R.direction r) outwardNormal niOverNt
                reflectProb = case maybeRefracted of Nothing -> 1.0
                                                     Just _ -> schlick cosine ri
                reflectScatter = R.Ray (H.p record, reflected)
                scattered = case maybeRefracted of
                             Nothing -> reflectScatter
                             Just refracted -> if randomNum < reflectProb then reflectScatter else R.Ray (H.p record, refracted)
            in Just (attenuation, scattered)

