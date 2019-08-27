# Ray Tracing in One Weekend

A path tracer written in pure Haskell. It implements multiple materials like metal, diffuse, and dielectrics, and supports customizable fields of view and viewpoints. As of now, it renders spheres.

Built following along with [Peter Shirley's guide](http://www.realtimerendering.com/raytracing/Ray%20Tracing%20in%20a%20Weekend.pdf).

### Usage

The GHC Haskell compiler is required. To build and run the program, type `make`. The output will be `out.ppm`, which you should be able to open using an image viewer of your choice.

In order to change the way the world looks and have more/less spheres of varying sizes and materials, you can modify `src/generate_world.py`. This script outputs Haskell code to create the `ShapesWorld.hs` module, which exports the `world` function to provide a list of items to render. The following methods are defined in `src/generate_world.py`, where each returns a string corresponding to valid Haskell code:

```python
vec(x, y, z) # vector
lambertian(albedo_vec) # diffuse material with given albedo vector
metal(albedo_vec, fuzz) # metal material with given albedo vector and Float fuzz level
dielectric(ri, random_num) # dielectric material with given and refractive index ri and random number for ray scattering
sphere(center_vec, radius, material) # sphere with center at center_vec, radius, and given material

# example of a lambertian sphere centered at (0,0,0) with radius 5
sphere(vec(0, 0, 0), 5, lambertian(vec(-1,0,-1)))
```

Modify the `generate_world` method in `src/generate_world.py` to produce a list of custom shapes.

