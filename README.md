# Banded Drift - Precision Ag Drift Method

Michael Bird and David Patterson


## Purpose

Explore the possibility of using existing regulatory drift curves to create new
curves based off banded pesticide applications. This idea is potentially of use
to precision agriculture.

## Motiviating Idea

Imagine a ditch that is one meter downwind of a field of wheat. For now, assume
the field is long and wide and that the water body runs along the entire edge of
the field.

If a boom sprayer sprays up to the edge of the wheat, we can use FOCUS SW 
drift curves to calculate:

  - The deposition curve over the ditch
  - The total deposition in the water body assuming we know the ditch width and
    the PPP application rate

If a boom sprayer moved itself 1m into the field (i.e) a 1 m crop buffer, we 
could similarly calculate deposition curve. The idea is that if you subtracted
the 1 m curve from the edge of field curve, you would be left with the drift that
resulted from spraying the 1 m wide strip from field edge into the field.

There is the potential to use this idea to refine drift depositions for banded 
applications where bands run parallel to the ditch

## Assumptions

 - That it's possible to accurately spray a band of a desired width (this 
   ignores the realities of nozzle distribution over the boom)
 - That the drift resulting from a given band is equal to the drift from a full
   field application minus the drift from a full field applications offset by the 
   band width. 
 - That the drift curve from a non-edge-of-field band will be equivalent to the 
   drift curve from an edge-of-field band. This will not be true as drift from 
   inner bands (especially from vertical crops) will have additional canopy 
   effects, reducing drift. This means this assumption is a conservative one for
   RA
   


## Todo

  - Implement R functions that allow for calculating deposition curves aligning
    with FOCUS SW.


## Priorities

### Priority 1 

plot all combination all combinations of row/inter from 50cm to 3m at inc of
~20cm contour plot (or something sensible) for
- no spray buffer
- then maybe buffers like focus?

Done, see `plotting.R` script

### Priority 2 

All Crops and Numapps

Not done, all functions now take an object from `get_params`
needs to have varying z_1 and z_2 based off of tables david sent for pond/ditch/stream
 and focus crop

### Prioirty 3 

 Usability for GUI, bake in focus assumptions on dimensions etc

### Think about 

  - "I have a map of treatments for square field, how would you translate that to 
     this methodology"
  - "parallel vs perpendicular"
  - regular spot "grid" take percentage of vertical band for what percent of 
  vertical band is treated
  - irregular spot, discretise the field into 1cm vertical band then scale
  down drift based of % of col treated
