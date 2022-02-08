#![feature(once_cell, box_syntax)]

use eodbc::{db::*, text_rows};
use odbc_api::{
    Cursor, ResultSetMetadata,
};

fn main() -> anyhow::Result<()> {
    let dsn_t = DSN::from_parts([("DSN", "DBWEST")]);
    println!("{}", dsn_t.0);
    let dsn = dsn_t.with_db("CC");
    println!("{}", dsn.0);

    let conn = dsn.connect()?;

    println!("{}", conn.database_management_system_name()?);
    println!("{}", conn.current_catalog()?);

    let cursor = conn.tables(None, None, None, None)?;

    let cn: Vec<String> = cursor
        .column_names()
        .expect("Has column names")
        .map(|x| x.unwrap_or("NONE".to_string()))
        .collect();
    println!("{:?}", cn);

    text_rows!(cursor, row, {
        println!(
            "{:?}",
            row.into_iter()
                .map(|x| String::from_utf8_lossy(x.unwrap_or("NULL".as_bytes())).to_string())
                .collect::<Vec<String>>()
        );
    });

    Ok(())
}

// let bd = buffer_descriptors!(cursor);

// let data_cols = buffer_from_description(1000, bd.into_iter());

// let mut row_data = cursor.bind_buffer(data_cols)?;

// while let Some(batch) = row_data.fetch()? {
//     let col = batch.column(0);
//     match col {
//         odbc_api::buffers::AnyColumnView::Text(mut x) => {
//             while let Some(Some(r)) = x.next() {
//                 println!("{:?}", String::from_utf8_lossy(&r))
//             }
//         },
//         _ => {}
//     }
// }
