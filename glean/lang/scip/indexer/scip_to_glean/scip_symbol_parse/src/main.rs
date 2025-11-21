/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! CLI tool for parsing SCIP symbols and outputting them as JSON.
//!
//! This tool takes SCIP symbol strings as input and parses them using the
//! scip_symbol library, then outputs the parsed structure as JSON.
//!
//! Usage:
//!   scip-symbol-parse <symbol>
//!   scip-symbol-parse --stdin
//!
//! Examples:
//!   scip-symbol-parse "rust-analyzer cargo std v1.0 io/IsTerminal#"
//!   echo "local myVar" | scip-symbol-parse --stdin

use std::io::BufRead;
use std::io::{self};

use anyhow::Result;
use clap::Parser;
use scip_symbol::parse_scip_symbol;

#[derive(Parser, Debug)]
#[clap(name = "scip-symbol-parse")]
#[clap(about = "Parse SCIP symbols and output as JSON", long_about = None)]
struct Args {
    /// SCIP symbol to parse
    #[clap(value_name = "SYMBOL")]
    symbol: Option<String>,

    /// Read symbols from stdin (one per line)
    #[clap(short, long)]
    stdin: bool,

    /// Pretty-print JSON output
    #[clap(short, long)]
    pretty: bool,
}

fn main() -> Result<()> {
    let args = Args::parse();

    if args.stdin {
        // Read from stdin, one symbol per line
        let stdin = io::stdin();
        for line in stdin.lock().lines() {
            let line = line?;
            let line = line.trim();
            if line.is_empty() {
                continue;
            }
            parse_and_output(line, args.pretty)?;
        }
    } else if let Some(symbol) = &args.symbol {
        // Parse single symbol from command line argument
        parse_and_output(symbol, args.pretty)?;
    } else {
        eprintln!("Error: Either provide a symbol as an argument or use --stdin");
        std::process::exit(1);
    }

    Ok(())
}

fn parse_and_output(symbol: &str, pretty: bool) -> Result<()> {
    let parsed = parse_scip_symbol(symbol);

    let json = if pretty {
        serde_json::to_string_pretty(&parsed)?
    } else {
        serde_json::to_string(&parsed)?
    };

    println!("{}", json);
    Ok(())
}
