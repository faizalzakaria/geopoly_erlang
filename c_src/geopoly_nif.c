#include "erl_nif.h"

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>

#include "GeopolyLib/geopoly.h"

/* vector_in_polygon(lat, lng, polygon_size, [[lat, lng], [lat, lng], ..., [lat, lng]]) */
static ERL_NIF_TERM user_in_region(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  unsigned polygon_size;

  if (argc < 1) {
    goto badarg;
  }

  // Polygon size
  if (!enif_get_uint(env, argv[0], &polygon_size)) {
    goto badarg;
  }

  GP_Point p = {10.0, 40.0};
  GP_Point poly_points[] = {
                            { 0.0,  0.0},
                            { 0.0, 60.0},
                            {60.0, 60.0},
                            {60.0,  0.0},
  };

  GP_Polygon poly;
  poly.data = poly_points;
  poly.size = 4;

  printf("Point {%f %f} is in poly: %d\n", p.lat, p.lon, GP_is_geopoint_in_polygon(p, poly, GP_SPHERE, 0.0));

  return enif_make_uint(env, 1);

 badarg:
  return enif_make_badarg(env);
}

static ErlNifFunc nif_funcs[] =
  {
   {"user_in_region", 4, user_in_region, 0}
  };

ERL_NIF_INIT(geopoly_nif, nif_funcs, NULL, NULL, NULL, NULL)
