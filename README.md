# SCRIP
SCRIP is a software package which computes addresses and weights for remapping and interpolating fields between grids in 
spherical coordinates. It was written originally for remapping fields to other grids in a coupled climate model, but is 
sufficiently general that it can be used in other applications as well. The package should work for any grid on the surface 
of a sphere. SCRIP currently supports five remapping options:

* Conservative remapping: First- and second-order conservative remapping as described in Jones (1999, Monthly Weather Review, 127, 2204-2210).
* Bilinear interpolation: Slightly generalized to use a local bilinear approximation (only logically-rectangular grids).
* Bicubic interpolation: Similarly generalized (only logically-rectangular grids).
* Distance-weighted averaging: Inverse-distance-weighted average of a user-specified number of nearest neighbor values.
* Particle remapping: A conservative particle (Monte-Carlo-like) remapping scheme
