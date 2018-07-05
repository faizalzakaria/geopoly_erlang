/// geopoly
/// Created by griff on 19/06/18.
/// Copyright © 2018 BottlesXO. All rights reserved.
///
/// geopoly C interface

#ifndef GP_GEOPOPY_H
#define GP_GEOPOPY_H

// Defualt maximum geodesic line segment length in m
const double GP_GEO_LINE_SEGMENT_LENGTH = 5000.0;

typedef enum {
    GP_SPHERE,
    GP_WGS84,
} GP_GeodesicType;

typedef struct {
    double lat;
    double lon;
} GP_Point;

typedef struct {
    GP_Point *data;
    unsigned size;
} GP_Polygon;

///
/// Test if geo point is inside a geo polygon
///
/// Argumetns
/// `geo_point`                     point to test, expects latitude in [-90, 90] range
/// `polygon`                       C style array of polygon points which form a polygon
/// `geodesic_type`                 `sphere`(GP_SPHERE) or `wgs84` (GP_WGS84)
///                                 `shere` is just `wgs84` with flattening 0.0.
///                                 Looks like GoogleMaps online uses spherical geometry,
///                                 so better use `sphere` if you expect polygon shape to
///                                 be even closer to the one in Google Maps.
/// `geodesic_line_segment_length`  Maximum length in meters of a geodesic line.
///                                 Geodesic line is splited in segments of this length.
///                                 The smaller is a length, the more accurate is result,
///                                 and slower execution. Use 0.0 to get a default length
/// Return
/// -1 - error occured
///  0 - point is not in a polygon
///  1 - point is in a polygon
///
int GP_is_geopoint_in_polygon(
        GP_Point geo_point,
        GP_Polygon polygon,
        GP_GeodesicType geodesic_type,
        double geodesic_line_segment_length);

#endif
