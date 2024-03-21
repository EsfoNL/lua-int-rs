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

use lua_int::{tokenizing::tokenize, Prog};

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
    p.run().expect("failed to run");
    println!("{:#?}", p);
}
