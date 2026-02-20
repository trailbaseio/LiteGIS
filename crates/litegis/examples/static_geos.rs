use rusqlite::Connection;

fn main() {
  let conn = Connection::open_in_memory().unwrap();

  litegis::register(&conn).unwrap();

  let litegis_version: String = conn
    .query_one("SELECT LiteGIS_Version()", (), |row| row.get(0))
    .unwrap();

  println!("{litegis_version}");

  let geos_version: String = conn
    .query_one("SELECT LiteGIS_GEOS_Version()", (), |row| row.get(0))
    .unwrap();

  println!("GEOS: {geos_version}");
}
