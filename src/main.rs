use clap::{Parser, ValueEnum};
use std::{ffi::OsString, io::Read};
use tracing::{debug, error, info, level_filters::LevelFilter, Level};
use tracing_subscriber::{layer::SubscriberExt, util::SubscriberInitExt};
use utf8_decode::Decoder;

#[derive(clap::Parser)]
struct Cli {
    file: OsString,
    #[arg(value_enum, default_value_t = Action::Run)]
    command: Action,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
enum Action {
    Run,
    Tokens,
}

use lua_int::{
    error::LuaErrorType, prog::LuaScope, tokenizing::Tokenizer, value::Value, LuaError, Prog,
};

fn main() {
    tracing_subscriber::registry()
        .with(tracing_subscriber::fmt::layer())
        .with(tracing_subscriber::filter::EnvFilter::from_default_env())
        .init();
    let args = Cli::parse();
    let s = std::io::BufReader::new(std::fs::File::open(args.file).unwrap())
        .bytes()
        .filter_map(Result::ok);
    let d = Decoder::new(s).filter_map(Result::ok);
    let t = Tokenizer::from(d);
    if args.command == Action::Tokens {
        let res = t.collect::<Vec<_>>();
        debug!("{res:#?}");
        return;
    }
    let mut global_scope = LuaScope::default();
    let mut p = Prog::new(t, &mut global_scope);
    let clos = |args: Vec<Value>, _: &mut LuaScope| -> Result<Value, LuaError> {
        if args.len() != 1 {
            return Err(LuaError::new_without_span(LuaErrorType::WrongArgumentCount));
        };
        println!("{}", args[0]);
        Ok(Value::Nil)
    };
    p.register_function(&"print", clos);
    match p.run() {
        Ok(_) => (),
        Err(e) => error!(
            "error_type: {:#?}, span: {:?}, backtrace: {:#?}",
            e.error_type, e.span, e.backtrace
        ),
    };
}
