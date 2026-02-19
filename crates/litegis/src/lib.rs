#![forbid(clippy::unwrap_used, unsafe_code)]
#![allow(clippy::needless_return)]

use std::str::FromStr;

use rusqlite::functions::{Context, FunctionFlags};
use rusqlite::types::{FromSqlError, Value, ValueRef};
use rusqlite::{Connection, Error};

use geos::{CoordSeq, Geom, Geometry, GeometryTypes, WKBWriter};

#[inline]
fn err_mapper(err: geos::Error) -> Error {
  return Error::UserFunctionError(err.into());
}

struct ErrWrapper(Error);

impl From<ErrWrapper> for Error {
  #[inline]
  fn from(err: ErrWrapper) -> Self {
    return err.0;
  }
}

impl From<geos::Error> for ErrWrapper {
  #[inline]
  fn from(err: geos::Error) -> Self {
    return Self(Error::UserFunctionError(err.into()));
  }
}

fn geom_from_text_1(ctx: &Context) -> Result<Vec<u8>, Error> {
  return Ok(geom_from_wkt(ctx.get_raw(0).as_str()?, None, None)?);
}

fn geom_from_text_2(ctx: &Context) -> Result<Vec<u8>, Error> {
  return Ok(geom_from_wkt(
    ctx.get_raw(0).as_str()?,
    Some(ctx.get_raw(1).as_i64()?),
    None,
  )?);
}

fn geom_from_wkt(
  text: &str,
  srid: Option<i64>,
  expected: Option<GeometryTypes>,
) -> Result<Vec<u8>, ErrWrapper> {
  let mut geometry = Geometry::new_from_wkt(text)?;
  if let Some(expected) = expected
    && geometry.geometry_type() != expected
  {
    return Err(ErrWrapper(Error::UserFunctionError(
      format!("expected: {expected:?}").into(),
    )));
  }

  let mut writer = WKBWriter::new()?;
  if let Some(srid) = srid {
    geometry.set_srid(srid as usize);
    writer.set_include_SRID(true);
  }
  return Ok(writer.write_wkb(&geometry)?.into());
}

fn point_from_text_1(ctx: &Context) -> Result<Vec<u8>, Error> {
  return Ok(geom_from_wkt(
    ctx.get_raw(0).as_str()?,
    None,
    Some(GeometryTypes::Point),
  )?);
}

fn point_from_text_2(ctx: &Context) -> Result<Vec<u8>, Error> {
  return Ok(geom_from_wkt(
    ctx.get_raw(0).as_str()?,
    Some(ctx.get_raw(1).as_i64()?),
    Some(GeometryTypes::Point),
  )?);
}

fn line_from_text_1(ctx: &Context) -> Result<Vec<u8>, Error> {
  return Ok(geom_from_wkt(
    ctx.get_raw(0).as_str()?,
    None,
    Some(GeometryTypes::LineString),
  )?);
}

fn line_from_text_2(ctx: &Context) -> Result<Vec<u8>, Error> {
  return Ok(geom_from_wkt(
    ctx.get_raw(0).as_str()?,
    Some(ctx.get_raw(1).as_i64()?),
    Some(GeometryTypes::LineString),
  )?);
}

fn polygon_from_text_1(ctx: &Context) -> Result<Vec<u8>, Error> {
  return Ok(geom_from_wkt(
    ctx.get_raw(0).as_str()?,
    None,
    Some(GeometryTypes::Polygon),
  )?);
}

fn polygon_from_text_2(ctx: &Context) -> Result<Vec<u8>, Error> {
  return Ok(geom_from_wkt(
    ctx.get_raw(0).as_str()?,
    Some(ctx.get_raw(1).as_i64()?),
    Some(GeometryTypes::Polygon),
  )?);
}

fn parse_from_geojson_1(ctx: &Context) -> Result<Value, Error> {
  let Some(text) = ctx.get_raw(0).as_str_or_null()? else {
    return Ok(Value::Null);
  };
  return Ok(parse_from_geojson(text)?);
}

fn parse_from_geojson(text: &str) -> Result<Value, ErrWrapper> {
  let json =
    geos::geojson::Geometry::from_str(text).map_err(|err| ErrWrapper(Error::UserFunctionError(err.into())))?;
  let geometry: Geometry = json.try_into()?;

  let mut writer = WKBWriter::new()?;
  if geometry.get_srid().ok().is_some() {
    writer.set_include_SRID(true);
  }

  return Ok(Value::Blob(writer.write_wkb(&geometry)?.into()));
}

#[inline]
fn get_f64(ctx: &Context, i: usize) -> Result<f64, FromSqlError> {
  return match ctx.get_raw(i) {
    ValueRef::Real(f) => Ok(f),
    ValueRef::Integer(i) => Ok(i as f64),
    _ => Err(FromSqlError::InvalidType),
  };
}

fn make_point_2(ctx: &Context) -> Result<Vec<u8>, Error> {
  let point = make_point(get_f64(ctx, 0)?, get_f64(ctx, 1)?)?;
  let mut writer = WKBWriter::new().map_err(err_mapper)?;
  return Ok(writer.write_wkb(&point).map_err(err_mapper)?.into());
}

fn make_point_srid_3(ctx: &Context) -> Result<Vec<u8>, Error> {
  let mut point = make_point(get_f64(ctx, 0)?, get_f64(ctx, 1)?)?;
  point.set_srid(ctx.get_raw(4).as_i64()? as usize);

  let mut writer = WKBWriter::new().map_err(err_mapper)?;
  writer.set_include_SRID(true);
  return Ok(writer.write_wkb(&point).map_err(err_mapper)?.into());
}

fn make_point(x: f64, y: f64) -> Result<Geometry, ErrWrapper> {
  let coords = CoordSeq::new_from_vec(&[&[x, y]])?;
  return Ok(Geometry::create_point(coords)?);
}

fn make_envelope_4(ctx: &Context) -> Result<Vec<u8>, Error> {
  return Ok(make_envelope(
    get_f64(ctx, 0)?,
    get_f64(ctx, 1)?,
    get_f64(ctx, 2)?,
    get_f64(ctx, 3)?,
    None,
  )?);
}

fn make_envelope_5(ctx: &Context) -> Result<Vec<u8>, Error> {
  return Ok(make_envelope(
    get_f64(ctx, 0)?,
    get_f64(ctx, 1)?,
    get_f64(ctx, 2)?,
    get_f64(ctx, 3)?,
    Some(ctx.get_raw(4).as_i64()?),
  )?);
}

fn make_envelope(xmin: f64, ymin: f64, xmax: f64, ymax: f64, srid: Option<i64>) -> Result<Vec<u8>, ErrWrapper> {
  let mut envelope =
    Geometry::create_multipoint(vec![make_point(xmin, ymin)?, make_point(xmax, ymax)?])?.envelope()?;

  let mut writer = WKBWriter::new()?;
  if let Some(srid) = srid {
    envelope.set_srid(srid as usize);
    writer.set_include_SRID(true);
  }
  return Ok(writer.write_wkb(&envelope)?.into());
}

fn get_type(ctx: &Context) -> Result<Value, Error> {
  let Some(blob) = ctx.get_raw(0).as_blob_or_null()? else {
    return Ok(Value::Null);
  };
  let geometry = Geometry::new_from_wkb(blob).map_err(err_mapper)?;
  return Ok(Value::Text(geometry.get_type().map_err(err_mapper)?));
}

fn get_dimension(ctx: &Context) -> Result<Value, Error> {
  let Some(blob) = ctx.get_raw(0).as_blob_or_null()? else {
    return Ok(Value::Null);
  };
  let geometry = Geometry::new_from_wkb(blob).map_err(err_mapper)?;
  return Ok(Value::Integer(
    geometry.get_num_dimensions().map_err(err_mapper)? as i64
  ));
}

fn is_valid(ctx: &Context) -> Result<bool, Error> {
  return match ctx.get_raw(0).as_blob_or_null() {
    Ok(Some(blob)) => Ok(Geometry::new_from_wkb(blob).is_ok_and(|g| g.is_valid())),
    // Consider NULL a valid geometry.
    Ok(None) => Ok(true),
    Err(_) => Ok(false),
  };
}

fn get_srid(ctx: &Context) -> Result<Value, Error> {
  let Some(blob) = ctx.get_raw(0).as_blob_or_null()? else {
    return Ok(Value::Null);
  };
  let geometry = Geometry::new_from_wkb(blob).map_err(err_mapper)?;
  return match geometry.get_srid() {
    Ok(r) => Ok(Value::Integer(r as i64)),
    _ => Ok(Value::Null),
  };
}

fn set_srid(ctx: &Context) -> Result<Vec<u8>, Error> {
  let mut geometry = Geometry::new_from_wkb(ctx.get_raw(0).as_blob()?).map_err(err_mapper)?;
  geometry.set_srid(ctx.get_raw(1).as_i64()? as usize);

  let mut writer = WKBWriter::new().map_err(err_mapper)?;
  writer.set_include_SRID(true);
  return Ok(writer.write_wkb(&geometry).map_err(err_mapper)?.into());
}

#[macro_export]
macro_rules! relation {
  ($name:ident) => {
    fn $name(ctx: &Context) -> Result<Value, Error> {
      let (Some(a), Some(b)) = (
        ctx.get_raw(0).as_blob_or_null()?,
        ctx.get_raw(1).as_blob_or_null()?,
      ) else {
        return Ok(Value::Null);
      };

      let result = Geometry::new_from_wkb(a)
        .map_err(err_mapper)?
        .$name(&Geometry::new_from_wkb(b).map_err(err_mapper)?)
        .map_err(err_mapper)?;

      return Ok(Value::Integer(result as i64));
    }
  };
}

relation!(contains);
relation!(covered_by);
relation!(covers);
relation!(disjoint);
relation!(equals);
relation!(intersects);
relation!(overlaps);
relation!(touches);
relation!(within);

pub fn register(conn: &Connection) -> Result<(), Error> {
  let default_flags: FunctionFlags =
    FunctionFlags::SQLITE_UTF8 | FunctionFlags::SQLITE_DETERMINISTIC | FunctionFlags::SQLITE_INNOCUOUS;

  // PG: 7.3 Geometry Constructors
  conn.create_scalar_function("ST_MakePoint", 2, default_flags, make_point_2)?;
  conn.create_scalar_function("ST_MakeEnvelope", 4, default_flags, make_envelope_4)?;
  conn.create_scalar_function("ST_MakeEnvelope", 5, default_flags, make_envelope_5)?;
  conn.create_scalar_function("ST_Point", 2, default_flags, make_point_2)?;
  conn.create_scalar_function("ST_Point", 3, default_flags, make_point_srid_3)?;

  // PG: 7.4. Geometry Accessors
  conn.create_scalar_function("ST_GeometryType", 1, default_flags, get_type)?;
  conn.create_scalar_function("ST_Dimension", 1, default_flags, get_dimension)?;

  // PG: 7.6. Geometry Validation
  conn.create_scalar_function("ST_IsValid", 1, default_flags, is_valid)?;

  // PG: 7.7. Spatial Reference System Functions
  conn.create_scalar_function("ST_SRID", 1, default_flags, get_srid)?;
  conn.create_scalar_function("ST_SetSRID", 2, default_flags, set_srid)?;

  // PG: 7.8 Geometry Input

  // WKT
  conn.create_scalar_function("ST_GeomFromText", 1, default_flags, geom_from_text_1)?;
  conn.create_scalar_function("ST_GeomFromText", 2, default_flags, geom_from_text_2)?;
  conn.create_scalar_function("ST_PointFromText", 1, default_flags, point_from_text_1)?;
  conn.create_scalar_function("ST_PointFromText", 2, default_flags, point_from_text_2)?;
  conn.create_scalar_function("ST_LineFromText", 1, default_flags, line_from_text_1)?;
  conn.create_scalar_function("ST_LineFromText", 2, default_flags, line_from_text_2)?;
  conn.create_scalar_function("ST_PolygonFromText", 1, default_flags, polygon_from_text_1)?;
  conn.create_scalar_function("ST_PolygonFromText", 2, default_flags, polygon_from_text_2)?;

  // GeoJSON
  conn.create_scalar_function("ST_GeomFromGeoJSON", 1, default_flags, parse_from_geojson_1)?;

  // PG: 7.9 Geometry Output
  // QUESTION: Should we do this in SQLite or instead offer library functions, e.g. BLOB -> GeoJSON.

  // PG: 7.11. Spatial Relationships
  conn.create_scalar_function("ST_Contains", 2, default_flags, contains)?;
  conn.create_scalar_function("ST_CoveredBy", 2, default_flags, covered_by)?;
  conn.create_scalar_function("ST_Covers", 2, default_flags, covers)?;
  conn.create_scalar_function("ST_Disjoint", 2, default_flags, disjoint)?;
  conn.create_scalar_function("ST_Equals", 2, default_flags, equals)?;
  conn.create_scalar_function("ST_Intersects", 2, default_flags, intersects)?;
  conn.create_scalar_function("ST_Overlaps", 2, default_flags, overlaps)?;
  conn.create_scalar_function("ST_Touches", 2, default_flags, touches)?;
  conn.create_scalar_function("ST_Within", 2, default_flags, within)?;

  // PG: 7.12. Measurement Functions
  // PG: 7.13. Overlay Functions (set operations)

  // PG: 7.21. Version Functions
  conn.create_scalar_function("LiteGIS_GEOS_Version", 0, default_flags, |_ctx| {
    return geos::version().map_err(err_mapper);
  })?;
  conn.create_scalar_function("LiteGIS_Version", 0, default_flags, |_ctx| {
    let version_info = rustc_tools_util::get_version_info!();
    return Ok(version_info.to_string());
  })?;

  return Ok(());
}

#[cfg(test)]
mod tests {
  use rusqlite::fallible_iterator::FallibleIterator;
  use std::fmt::Display;

  use super::*;

  fn setup_connection() -> Connection {
    let conn = Connection::open_in_memory().unwrap();
    register(&conn).unwrap();
    return conn;
  }

  struct LngLat {
    lng: f64,
    lat: f64,
  }

  impl Display for LngLat {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      return write!(f, "{} {}", self.lng, self.lat);
    }
  }

  #[test]
  fn test_basic_geometry_constructors() {
    let conn = setup_connection();

    conn
      .execute_batch("SELECT ST_GeomFromText('POLYGON ((12 40, 12 42, 13 42, 13 40, 12 40))');")
      .unwrap();
    conn
      .execute_batch("SELECT ST_GeomFromText('POLYGON ((12 40, 12 42, 13 42, 13 40, 12 40))', 4326);")
      .unwrap();

    for (expected, geometry) in [
      ("Point", "ST_PointFromText('POINT (12 40)')"),
      ("LineString", "ST_LineFromText('LINESTRING(10 20, 20 30)')"),
      (
        "Polygon",
        "ST_PolygonFromText('POLYGON ((12 40, 12 42, 13 42, 13 40, 12 40))')",
      ),
    ] {
      assert_eq!(
        expected,
        conn
          .query_row(&format!("SELECT ST_GeometryType({geometry})"), (), |row| row
            .get::<_, String>(
            0
          ))
          .unwrap()
      );
    }
  }

  #[test]
  fn test_basic_geometry_operations() {
    const COLOSSEO: LngLat = LngLat {
      lng: 12.4924,
      lat: 41.8902,
    };

    let conn = setup_connection();
    conn
      .execute_batch(&format!(
        "CREATE TABLE points (
           id        INTEGER PRIMARY KEY,
           desc      TEXT,
           geom      BLOB CHECK(ST_IsValid(geom))
         ) STRICT;

         CREATE INDEX _points_geom ON points(geom);

         INSERT INTO points (id, desc, geom) VALUES
           (0, 'colosseo',       ST_GeomFromText('POINT({COLOSSEO})', 4326)),
           (1, 'line',           ST_GeomFromText('LINESTRING(10 41, 20 41)', 4326)),
           (2, 'br-quadrant',    ST_MakeEnvelope(0, -0, 180, -90)),
           (3, 'null',           NULL)
         ;"
      ))
      .unwrap();

    let mut srid_stmt = conn
      .prepare("SELECT ST_SRID(geom) FROM points ORDER BY id LIMIT 4;")
      .unwrap();

    let rows = srid_stmt.query([]).unwrap();
    assert_eq!(
      vec![Some(4326), Some(4326), None, None],
      rows
        .map(|row| row.get::<_, Option<i64>>(0))
        .unwrap()
        .collect::<Vec<_>>()
    );

    // Check `within` bounding box.
    for (expected, geometry) in [
      (1, "ST_MakeEnvelope(12, 40, 13, 42, 4326)"),
      (0, "ST_MakeEnvelope(-12, -42, -11, -40, 4326)"),
    ] {
      assert_eq!(
        expected,
        conn
          .query_row(
            &format!("SELECT COUNT(*) FROM points WHERE ST_Within(geom, {geometry});"),
            (),
            |row| row.get::<_, i64>(0),
          )
          .unwrap()
      );
    }

    // Check `intersects` bounding box.
    for (expected, geometry) in [
      (2, "ST_MakeEnvelope(12, 40, 13, 42, 4326)"),
      (0, "ST_MakeEnvelope(-12, -42, -11, -40, 4326)"),
    ] {
      assert_eq!(
        expected,
        conn
          .query_row(
            &format!("SELECT COUNT(*) FROM points WHERE ST_Intersects(geom, {geometry});"),
            (),
            |row| row.get::<_, i64>(0),
          )
          .unwrap()
      );
    }

    // Check `contains` bounding box.
    for (expected, geometry) in [(0, "ST_MakePoint(12, 40)"), (1, "ST_MakePoint(12, -40)")] {
      assert_eq!(
        expected,
        conn
          .query_row(
            &format!("SELECT COUNT(*) FROM points WHERE ST_Contains(geom, {geometry});"),
            (),
            |row| row.get::<_, i64>(0),
          )
          .unwrap()
      );
    }
  }

  #[test]
  fn test_versions_accessors() {
    let conn = setup_connection();

    let geos: String = conn
      .query_one("SELECT LiteGIS_GEOS_Version()", (), |row| row.get(0))
      .unwrap();
    assert!(geos.starts_with("3"));

    let litegis: String = conn
      .query_one("SELECT LiteGIS_Version()", (), |row| row.get(0))
      .unwrap();
    assert!(litegis.starts_with("litegis"))
  }
}
