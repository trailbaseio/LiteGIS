# LiteGIS

A GIS extension for SQLite, similar to [PostGIS](https://postgis.net/) for
Postgres and [SpatiaLite](https://www.gaia-gis.it/fossil/libspatialite).

The APIs are modeled closely on PostGIS but very incomplete at this point. At
the end of the day, this is a thin wrapper around the amazing
[GEOS](https://libgeos.org/).

## Building & Usage

If you have a recent Rust toolchain installed, simply run

```sh
cargo build -p litegis-so --release
```

This will yield a shared library for your host platform, e.g:
`target/release/liblitegis.so`.
For other platforms, use the `--target` flag.

The library can then be loaded using `sqlite3`:

```sh
sqlite3 ↩️

sqlite> .load target/release/liblitegis
sqlite> SELECT LiteGIS_GEOS_Version();
3.13.1-CAPI-1.19.2
```

, or programmatically:

```sql
SELECT load_extension('target/release/liblitegis');
```

## References

- PostGIS: https://postgis.net/docs/reference.html
- SpatiaLite: https://gaia-gis.it/gaia-sins/spatialite-sql-5.1.0.html
