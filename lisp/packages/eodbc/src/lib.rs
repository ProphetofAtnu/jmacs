#![feature(once_cell, box_syntax)]
pub mod db;
pub mod util;


use std::lazy::SyncLazy;

use db::DSN;
use emacs::{defun, Env, IntoLisp, Result, Value};

emacs::plugin_is_GPL_compatible!();


// static LOOP: SyncLazy<Arc<Box<runner::Runner>>> =
//     SyncLazy::new(|| Arc::new(box runner::Runner::new()));

#[emacs::module]
fn init(env: &Env) -> Result<Value<'_>> {
    ().into_lisp(env)
}

#[defun]
fn run_sql(dsn: String, query: String) -> Result<String> {
    let dsn_val = DSN(dsn);
    Ok(dsn_val.exec_to_string(&query)?)
}


#[defun]
fn run_sql_to_lisp(env: &Env, dsn: String, query: String) -> Result<Value> {
    let dsn_val = DSN(dsn);
    let vec_data = dsn_val.exec_to_vec(&query)?;

    env.call(env.intern("read")?, [serde_lexpr::to_string(&vec_data)?.into_lisp(env)?])
}


#[defun]
fn run_sql_in_db(dsn: String, db: String, query: String) -> Result<String> {
    let dsn_val = DSN(dsn).with_db(&db);
    let str_res = dsn_val.exec_to_string(&query)?;
    Ok(str_res)
}


#[defun]
fn run_sql_to_lisp_in_db(env: &Env, dsn: String, db: String, query: String) -> Result<Value> {
    let dsn_val = DSN(dsn).with_db(&db);
    let vec_data = dsn_val.exec_to_vec(&query)?;

    env.call(env.intern("read")?, [serde_lexpr::to_string(&vec_data)?.into_lisp(env)?])
}
