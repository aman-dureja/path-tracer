module ShapesWorld where
import Vec3 (Vec3(..))
import Material.Lambertian (lambertian)
import Material.Metal (metal)
import Material.Dielectric (dielectric)
import Sphere (sphere)
import HitableList (HitableList, hitableList)

world :: HitableList
world = hitableList [sphere (Vec3(0,-1000,0)) 1000 (lambertian (Vec3(0.5,0.5,0.5))), sphere (Vec3(-0.151021526229,0.2,0.871128339427)) 0.2 (lambertian (Vec3(0.00510435456393,0.21980128686,0.293251378173))), sphere (Vec3(0,1,0)) 1 (dielectric 1.5 0.526653658439), sphere (Vec3(-4,1,0)) 1 (lambertian (Vec3(0.4,0.2,0.1))), sphere (Vec3(4,1,0)) 1 (metal (Vec3(0.7,0.6,0.5)) 0)]
