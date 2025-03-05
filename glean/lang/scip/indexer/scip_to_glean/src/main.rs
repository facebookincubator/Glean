// (c) Meta Platforms, Inc. and affiliates. Confidential and proprietary.

use std::fs::File;
use std::fs::OpenOptions;
use std::io::Read;
use std::path::Path;
use std::path::PathBuf;

use anyhow::anyhow;
use anyhow::Context;
use anyhow::Error;
use anyhow::Result;
use clap::Args;
use clap::Parser;
use clap::Subcommand;
use fbinit::FacebookInit;
use proto_rust::scip::Index;
use protobuf::Message;
use serde::Deserialize;
use serde::Serialize;
use tracing::info;

use crate::angle::Env;
use crate::lsif::LanguageId;

mod angle;
mod lsif;
mod output;

/// PoC CLI for converting SCIP to Glean facts
#[derive(Parser, Debug)]
#[command(
    author = "rl_code_authoring",
    about = "PoC CLI for converting SCIP to Glean facts"
)]
pub struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Debug, Subcommand)]
enum Commands {
    BuildJson(BuildJsonArgs),
}
#[derive(Debug, Args)]
struct BuildJsonArgs {
    #[arg(short, long)]
    input: PathBuf,
    #[arg(short, long)]
    output: PathBuf,

    #[arg(
        long,
        help = "Ignored. This argument exists for compatibility with Haskell. The only effect of --infer-language in the Haskell code is to detect .kt as Kotlin. The Rust version does this unconditionally."
    )]
    infer_language: bool,

    #[arg(
        long,
        help = "The default language to use for files without a recognized extension."
    )]
    language: Option<String>,

    #[arg(long, help = "Prefix to prepend to filepaths.")]
    root_prefix: Option<String>,
}

#[cli::main("scip-aosp", error_logging(user(default_level = "info")))]
async fn main(_fb: FacebookInit, cli: Cli) -> Result<cli::ExitCode> {
    match cli.command {
        Commands::BuildJson(args) => {
            build_json(args)?;
        }
    };

    Ok(cli::ExitCode::SUCCESS)
}

fn build_json(args: BuildJsonArgs) -> Result<()> {
    println!("{:?}", args);
    let default_language = args
        .language
        .as_ref()
        .and_then(|s| LanguageId::new(s).known());

    info!("Loading documents");
    let scip_index = read_scip_file(&args.input)
        .with_context(|| format!("Error opening input file {}", args.input.display()))?;
    info!("Loaded {} documents", scip_index.documents.len());

    let mut env = Env::new();
    if let Some(metadata) = scip_index.metadata.into_option() {
        env.decode_scip_metadata(metadata);
    }
    for doc in scip_index.documents {
        env.decode_scip_doc(default_language, args.root_prefix.as_deref(), doc)?;
    }

    let write = OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open(&args.output)
        .with_context(|| format!("Error creating output file {}", args.output.display()))?;
    let writer = std::io::BufWriter::new(write);

    env.output().write(writer)?;
    Ok(())
}

#[derive(PartialEq, Debug)]
enum Suffix {
    SymUnspecifiedSuffix,
    SymPackage,
    SymType,
    SymTerm,
    SymMethod,
    SymTypeParameter,
    SymParameter,
    SymMeta,
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields, rename_all = "camelCase")]
struct ToolInfo {
    tool_args: Vec<String>,
    tool_name: String,
    version: String,
}

#[derive(Serialize, Deserialize)]
#[serde(deny_unknown_fields, rename_all = "camelCase")]
struct GleanRange {
    column_begin: u64,
    column_end: u64,
    line_begin: u64,
    line_end: u64,
}

fn decode_scip_range(range: &[i32]) -> Result<GleanRange> {
    let mut range = match range {
        [line_begin, column_begin, column_end] => GleanRange {
            line_begin: (*line_begin + 1) as u64,
            column_begin: (*column_begin + 1) as u64,
            line_end: (*line_begin + 1) as u64,
            column_end: (*column_end) as u64,
        },
        [line_begin, column_begin, line_end, column_end] => GleanRange {
            line_begin: (*line_begin + 1) as u64,
            column_begin: (*column_begin + 1) as u64,
            line_end: (*line_end + 1) as u64,
            column_end: (*column_end) as u64,
        },
        _ => {
            return Err(anyhow!("bad range: {:#?}", range));
        }
    };
    range.column_end = std::cmp::max(range.column_begin, range.column_end);
    Ok(range)
}

fn read_scip_file(file: &Path) -> Result<Index, Error> {
    let scip_file = File::open(file)?;
    let mut reader = std::io::BufReader::new(scip_file);
    Index::parse_from_reader(&mut reader).context("Failed to deserialize scip file")
}
