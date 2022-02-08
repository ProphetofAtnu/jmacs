#[macro_export]
macro_rules! text_rows {
    ($cur:expr, $row:ident, $bl:block) => {{
        use odbc_api::ResultSetMetadata;
        let buffer = odbc_api::buffers::TextRowSet::for_cursor(1000, &$cur, Some(4096));
        if let Ok(buf) = buffer {
            let mut row_set = $cur.bind_buffer(buf)?;

            while let Some(batch) = row_set.fetch()? {
                for ir in 0..batch.num_rows() {
                    let $row: Vec<Option<&[u8]>> =
                        (0..batch.num_cols()).map(|x| batch.at(x, ir)).collect();
                    $bl
                }
            }
        }
        ()
    }};
}

#[macro_export]
macro_rules! buffer_descriptors {
    ($cur:expr) => {{
        let mut column_description = Default::default();

        let buffer_description: Vec<_> = (0..$cur.num_result_cols()?)
            .map(|index| {
                $cur.describe_col(index as u16 + 1, &mut column_description)?;
                Ok(odbc_api::buffers::BufferDescription {
                    nullable: matches!(
                        column_description.nullability,
                        odbc_api::Nullability::Unknown | odbc_api::Nullability::Nullable
                    ),
                    // Use reasonable sized text, in case we do not know the buffer type.
                    kind: odbc_api::buffers::BufferKind::from_data_type(
                        column_description.data_type,
                    )
                    .unwrap_or(odbc_api::buffers::BufferKind::Text { max_str_len: 255 }),
                })
            })
            .collect::<Result<_, Error>>()?;

        buffer_description
    }};
}

