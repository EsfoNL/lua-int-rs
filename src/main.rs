use clap::{Parser, Subcommand, ValueEnum};
use std::{ffi::OsString, io::Read};
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
    error::LuaErrorType, prog::LuaScope, tokenizing::tokenize, value::Value, LuaError, Prog,
};

fn main() {
    let args = Cli::parse();
    let s = std::io::BufReader::new(std::fs::File::open(args.file).unwrap())
        .bytes()
        .filter_map(Result::ok);
    let d = Decoder::new(s).filter_map(Result::ok);
    let t = tokenize(d);
    if args.command == Action::Tokens {
        println!("{:?}", t.collect::<Vec<_>>());
        return;
    }
    let mut p = Prog::from(t);
    let clos = |args: Vec<Value>, _: &mut LuaScope| -> Result<Value, LuaError> {
        if args.len() != 1 {
            return Err(LuaError::new_without_span(LuaErrorType::WrongArgumentCount));
        };
        println!(
            "{}",
            args[0]
                .string()
                .ok_or(Into::<LuaError>::into(LuaErrorType::ExpectedValue))?
        );
        Ok(Value::Nil)
    };
    p.register_function("print".to_owned(), clos);
    p.run().expect("failed to run");
    println!("{:#?}", p);
}
