/// geopoly
/// Created by griff on 19/06/18.
/// Copyright © 2018 BottlesXO. All rights reserved.
///
#include <string>
#include <iostream>
#include <vector>
#include <exception>
#include <GeographicLib/GeodesicLine.hpp>

extern "C" {
    #include "geopoly.h"
}

// Expect geodesic points in geo polygon to be at least min distance far away, in m
static const double GEO_MIN_DISTANCE = 30.0;
// Geodesic epsilon for equal comparison
static const double GEO_EPSILON = 0.00001;

using namespace std;

/// Vec2 structure to keep a geodesic or flat point
struct Vec2 {
    union {
        struct {
            double x; // flat x
            double y; // flat y
        };
        struct {
            double lat; // latitude
            double lon; // longitude
        };
    };

    Vec2(): x(0.0), y(0.0) { }
    Vec2(double x, double y): x(x), y(y) { }
    Vec2(const GP_Point &p): x(p.lat), y(p.lon) { }

    string to_string() const {
        return std::to_string(lat) + " " + std::to_string(lon);
    }
};

/// Check if point has NANs
bool isnan(const Vec2 &p) {
    return isnan(p.x) || isnan(p.y);
}

/// Check if geo coordinate lat is in [-90; 90] range
void check_lat(const Vec2 &geo_p) {
    if (geo_p.lat < -90.0 || geo_p.lat > 90.0) {
        throw std::invalid_argument(string("Error: point latitude {") + geo_p.to_string() + "} is out of range [-90; 90]");
    }
}

/// Normalize longitude
double normalize_lon(double lon) {
  return fmod(lon - 180.0, 360.0) + 180.0;
}

/// pnpoly algorithm on flat surface
/// https://wrf.ecse.rpi.edu//Research/Short_Notes/pnpoly.html
bool is_point_in_polygon(const Vec2 &p, vector<Vec2> &poly) {
    bool c = false;
    int size = poly.size();
    for (int i = 0, j = size - 1; i < size; j = i++) {
        const Vec2 pi = poly[i];
        const Vec2 pj = poly[j];
        if (((pi.y > p.y) != (pj.y > p.y)) &&
            (p.x < (pj.x - pi.x) * (p.y - pi.y) / (pj.y - pi.y) + pi.x)) {
            c = !c;
        }
    }
    return c;
}

/// Euclidean squared distance (flat)
double distance2(const GP_Point &p0, const GP_Point &p1) {
    double x = p1.lat - p0.lat;
    double y = p1.lon - p0.lon;
    return x * x + y * y;
}

/// Split geodesic line into segments of max length
void split_geodesic_line(vector<Vec2> &out, const GeographicLib::Geodesic &geod, const Vec2 &p0, const Vec2 &p1, double segment_length) {
    GeographicLib::GeodesicLine line = geod.InverseLine(p0.lat, p0.lon, p1.lat, p1.lon);
    double line_distance = line.Distance();

    if (line_distance < GEO_MIN_DISTANCE) {
        throw std::invalid_argument("Error: polygon points {" + p0.to_string() + "} and {" + p1.to_string() + "} are too closer than " + to_string(GEO_MIN_DISTANCE) + " meters.");
    }

    int num_intervals = int(ceil(line_distance / segment_length));
    double ds = line_distance / num_intervals;

    out.reserve(out.size() + num_intervals);
    for (int i = 0; i < num_intervals + 1; ++i) {
        Vec2 geo_p;
        line.Position(i * ds, geo_p.lat, geo_p.lon);
        check_lat(geo_p);
        out.push_back(geo_p);
    }
}

/// Returns Geodesic for type sring
GeographicLib::Geodesic geodesic_for_type(GP_GeodesicType type) {
    switch(type) {
        case GP_WGS84: return GeographicLib::Geodesic::WGS84();
        case GP_SPHERE: return GeographicLib::Geodesic(GeographicLib::Constants::WGS84_a(), 0.0);
    }
    throw std::invalid_argument(
            "Error: bad geodesic type \"" + to_string(type) + "\".\n"
            "Supported types are: \"sphere\" = " + to_string(GP_SPHERE) + " or \"wgs84\" = " + to_string(GP_WGS84) + ".");
}

extern "C" int GP_is_geopoint_in_polygon(
        GP_Point geo_point,
        GP_Polygon polygon,
        GP_GeodesicType geodesic_type,
        double geodesic_line_segment_length) {
    try {
        Vec2 geo_test_p(geo_point);
        check_lat(geo_test_p);

        // geodesic
        const GeographicLib::Geodesic &geod = geodesic_for_type(geodesic_type);

        // geo line segemnt length
        double segment_length = geodesic_line_segment_length < GEO_EPSILON ? GP_GEO_LINE_SEGMENT_LENGTH : geodesic_line_segment_length;

        // if fist point == last point we remove the last point, as assumed by pnpoly algorithm
        if (polygon.size > 1 && distance2(polygon.data[0], polygon.data[polygon.size - 1]) < GEO_EPSILON * GEO_EPSILON) {
            --polygon.size;
        }

        // polygon has at least 3 points
        if (polygon.size < 3) {
            throw std::invalid_argument("Error: polygon points count is < 3");
        }

        // split long polygon edges
        vector<Vec2> split_poly;
        split_poly.reserve(polygon.size);
        for (int size = polygon.size, i = 0, j = size - 1; i < size; j = i++) {
            Vec2 pj(polygon.data[j]);
            Vec2 pi(polygon.data[i]);
            split_geodesic_line(split_poly, geod, pj, pi, segment_length);
        }

        // normalize poly
        for (Vec2 &p: split_poly) {
            p.lon = normalize_lon(p.lon);
        }
        geo_test_p.lon = normalize_lon(geo_test_p.lon);

        return is_point_in_polygon(geo_test_p, split_poly);
    } catch (const exception &e) {
        cerr << "Caught exception: " << e.what() << endl;
        return -1;
    }
    return -1;
}

