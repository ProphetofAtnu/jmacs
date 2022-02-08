#![allow(unused)]
use std::{lazy::SyncLazy, sync::Arc};

use anyhow::format_err;
use emacs::{FromLisp, IntoLisp, Value};
use odbc_api::{buffers::TextRowSet, Connection, Cursor, Environment, ResultSetMetadata};

static ENVIRONMENT: SyncLazy<Arc<Box<Environment>>> =
    SyncLazy::new(|| Arc::new(box Environment::new().unwrap()));

#[derive(Debug, Clone)]
pub struct DSN(pub String);

// Result<Connection<'a>, odbc_api::Error>
impl<'a> TryInto<Connection<'a>> for DSN {
    type Error = odbc_api::Error;

    fn try_into(self) -> Result<Connection<'a>, Self::Error> {
        self.connect()
    }
}

// Query functions
impl DSN {
    pub fn to_string(&self) -> String {
        let DSN(cstr) = self;
        cstr.to_string()
    }

    pub fn to_parts(&self) -> Vec<(String, String)> {
        let mut parts: Vec<(String, String)> = Vec::new();
        let cstr = self.to_string();

        for part in cstr.split(";") {
            let mut kv = part.split("=");
            if let (Some(key), Some(val)) = (kv.next(), kv.next()) {
                parts.push((key.trim().into(), val.trim().into()))
            }
        }
        parts
    }

    pub fn from_parts<T, KV>(parts: T) -> DSN
    where
        T: IntoIterator<Item = (KV, KV)>,
        KV: ToString,
    {
        let cstr = parts
            .into_iter()
            .map(|(k, v)| format!("{}={}", k.to_string(), v.to_string()));
        DSN(cstr.collect::<Vec<String>>().join(";"))
    }

    pub fn with_db(&self, db_name: &str) -> DSN {
        let parts = self.to_parts();
        let mut no_db: Vec<(String, String)> =
            parts.into_iter().filter(|(k, _)| k != "Database").collect();
        no_db.push(("Database".into(), db_name.into()));
        DSN::from_parts(no_db)
    }

    pub fn connect<'a>(&self) -> Result<Connection<'a>, odbc_api::Error> {
        let DSN(cstr) = self;
        return ENVIRONMENT.connect_with_connection_string(cstr);
    }
}

impl DSN {
    pub fn exec_to_string(&self, query: &str) -> anyhow::Result<String> {
        let conn = self.connect()?;

        let result = conn.execute(&query, ())?;

        match result {
            Some(cursor) => {
                let mut buffers = TextRowSet::for_cursor(5000, &cursor, Some(4096))?;
                let columns: Vec<String> = cursor
                    .column_names()?
                    .map(|x| x.unwrap_or("NULL".to_string()))
                    .collect();

                let mut row_set = cursor.bind_buffer(buffers)?;

                let mut result = String::with_capacity(20_000);

                result.push_str(&columns.join("|"));
                result.push('\n');

                while let Some(batch) = row_set.fetch()? {
                    for ir in 0..batch.num_rows() {
                        let row: Vec<String> = (0..batch.num_cols())
                            .map(|x| {
                                batch
                                    .at(x, ir)
                                    .and_then(|x| Some(String::from_utf8_lossy(x).to_string()))
                                    .unwrap_or("NULL".to_string())
                            })
                            .collect();
                        result.push_str(&row.join("|"));
                        result.push('\n')
                    }
                }

                Ok(result)
            }
            _ => Ok("".to_string()),
        }
    }

    pub fn exec_to_vec(&self, query: &str) -> anyhow::Result<Vec<Vec<String>>> {
        let conn = self.connect()?;

        let result = conn.execute(&query, ())?;

        match result {
            Some(cursor) => {
                let mut buffers = TextRowSet::for_cursor(5000, &cursor, Some(4096))?;
                let columns: Vec<String> = cursor
                    .column_names()?
                    .map(|x| x.unwrap_or("NULL".to_string()))
                    .collect();

                let mut row_set = cursor.bind_buffer(buffers)?;

                let mut res: Vec<Vec<String>> = Vec::with_capacity(5000);

                res.push(columns);

                while let Some(batch) = row_set.fetch()? {
                    for ir in 0..batch.num_rows() {
                        let row: Vec<String> = (0..batch.num_cols())
                            .map(|x| {
                                batch
                                    .at(x, ir)
                                    .and_then(|x| Some(String::from_utf8_lossy(x).to_string()))
                                    .unwrap_or("NULL".to_string())
                            })
                            .collect();
                        res.push(row);
                    }
                }

                Ok(res)
            }
            _ => Ok(vec![]),
        }
    }
}
