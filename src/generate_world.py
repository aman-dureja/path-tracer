from random import random
import math

HASKELL_FILE_OUTPUT_STRING = '\n'.join([
        "module ShapesWorld where",
        "import Vec3 (Vec3(..))",
        "import Material.Lambertian (lambertian)",
        "import Material.Metal (metal)",
        "import Material.Dielectric (dielectric)",
        "import Sphere (sphere)",
        "import HitableList (HitableList, hitableList)",
        "",
        "world :: HitableList",
        "world = hitableList [{}]"
])

def vec(x, y, z):
    return "Vec3({},{},{})".format(x, y, z)

def lambertian(albedo_vec):
    return "lambertian ({})".format(albedo_vec)

def metal(albedo_vec, fuzz):
    return "metal ({}) {}".format(albedo_vec, fuzz)

def dielectric(ri, random_num):
    return "dielectric {} {}".format(ri, random_num)

def sphere(center_vec, radius, material):
    return "sphere ({}) {} ({})".format(center_vec, radius, material)

def generate_world():
    shapes = [sphere(vec(0, -1000, 0), 1000, lambertian(vec(0.5, 0.5, 0.5)))]

    for a in range(-1, 0):
        for b in range(-1, 0):
            choose_mat = random()
            center_x = a + 0.9 * random()
            center_y = 0.2
            center_z = b + 0.9 + random()
            center = vec(center_x, center_y, center_z)
            if math.sqrt((center_x - 4)**2 + (center_y - 0.2)**2 + (center_z)**2) > 0.9:
                if choose_mat < 0.8: # diffuse
                    shapes.append(sphere(center, 0.2, lambertian(vec(random()*random(), random()*random(), random()*random()))))
                elif choose_mat < 0.95: # metal
                    shapes.append(sphere(center, 0.2, metal(vec(0.5*(1+random()), 0.5*(1+random()), 0.5*(1+random())), 0.5*random())))
                else: # glass
                    shapes.append(sphere(center, 0.2, dielectric(1.5, random())))
                    
    shapes.append(sphere(vec(0, 1, 0), 1, dielectric(1.5, random())))
    shapes.append(sphere(vec(-4,1,0), 1, lambertian(vec(0.4, 0.2, 0.1))))
    shapes.append(sphere(vec(4, 1, 0), 1, metal(vec(0.7, 0.6, 0.5), 0)))

    return ", ".join(shapes)

if __name__ == "__main__":
    print(HASKELL_FILE_OUTPUT_STRING.format(generate_world()))

