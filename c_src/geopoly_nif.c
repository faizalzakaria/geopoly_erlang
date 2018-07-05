#include "erl_nif.h"

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <unistd.h>

#include "geopoly.h"

static int get_number(ErlNifEnv* env, ERL_NIF_TERM term, double* dp);

static ERL_NIF_TERM user_in_region(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if (argc < 4) {
    goto badarg;
  }

  GP_Point point;
  GP_Polygon polygon;
  unsigned ret;

  // User location
  if (!get_number(env, argv[0], &point.lat) || !get_number(env, argv[1], &point.lon)) {
    goto badarg;
  }

  // Polygon size
  if (!enif_get_uint(env, argv[2], &polygon.size)) {
    goto badarg;
  }

  polygon.data = enif_alloc(sizeof(GP_Point) * polygon.size);

  // Get the polygon data
  ERL_NIF_TERM list, row, v;
  list = argv[3];

  // Polygon
  for (unsigned i = 0; i < polygon.size; i++) {
    if (!enif_get_list_cell(env, list, &row, &list)) {
      goto badarg;
    }

    if (!enif_get_list_cell(env, row, &v, &row) ||
        !get_number(env, v, &polygon.data[i].lat)) {
      goto badarg;
    }

    if (!enif_get_list_cell(env, row, &v, &row) ||
        !get_number(env, v, &polygon.data[i].lon)) {
      goto badarg;
    }
  }

  ret = GP_is_geopoint_in_polygon(point, polygon, GP_SPHERE, 0.0);

  if (polygon.data != NULL) {
    enif_free(polygon.data);
  }

  return enif_make_uint(env, ret);

 badarg:
  if (polygon.data != NULL) {
    enif_free(polygon.data);
  }

  return enif_make_badarg(env);
}

static int get_number(ErlNifEnv* env, ERL_NIF_TERM term, double* dp)
{
  long i;
  return enif_get_double(env, term, dp) ||
    (enif_get_long(env, term, &i) && (*dp=(double)i, 1));
}

static ErlNifFunc nif_funcs[] =
  {
   {"user_in_region", 4, user_in_region, 0}
  };

ERL_NIF_INIT(geopoly_nif, nif_funcs, NULL, NULL, NULL, NULL)
