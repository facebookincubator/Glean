/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

use std::fs::File;
use std::fs::OpenOptions;
use std::path::Path;
use std::path::PathBuf;

use anyhow::Context;
use anyhow::Error;
use anyhow::Result;
use anyhow::anyhow;
use clap::Parser;
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

/// CLI for converting SCIP to Glean facts json
#[derive(Parser, Debug)]
#[command(
    author = "rl_code_authoring",
    about = "CLI for converting SCIP to Glean facts json"
)]
struct BuildJsonArgs {
    #[arg(short, long)]
    input: Vec<PathBuf>,
    #[arg(short, long)]
    output: PathBuf,

    #[arg(
        long,
        help = "Infer language for .java and .hk files when language is not set"
    )]
    infer_language: bool,

    #[arg(
        long,
        help = "The default language to use for files without a recognized extension."
    )]
    language: Option<String>,

    #[arg(long, help = "Prefix to prepend to filepaths.")]
    root_prefix: Option<String>,

    #[arg(
        long,
        help = "Shards the JSON graph into subgraphs. Subgraphs will be approximately the size specified. Uses the --output argument as a directory, and writes one file per shard"
    )]
    shard: Option<usize>,
}

#[cli::main("scip_to_glean", error_logging(user(default_level = "info")))]
async fn main(_fb: FacebookInit, args: BuildJsonArgs) -> Result<cli::ExitCode> {
    build_json(args)?;
    Ok(cli::ExitCode::SUCCESS)
}

fn decode_scip_data(
    env: &mut Env,
    path: &Path,
    default_language: Option<LanguageId>,
    infer_language: bool,
    path_prefix: Option<&str>,
) -> Result<()> {
    info!("Loading documents from {}", path.display());
    let scip_index = read_scip_file(path)
        .with_context(|| format!("Error opening input file {}", path.display()))?;
    info!("Loaded {} documents", scip_index.documents.len());

    if let Some(metadata) = scip_index.metadata.into_option() {
        env.decode_scip_metadata(metadata);
    }
    for doc in scip_index.documents {
        env.decode_scip_doc(default_language, infer_language, path_prefix, doc)?;
    }
    Ok(())
}

fn build_json(args: BuildJsonArgs) -> Result<()> {
    println!("{:?}", args);
    let default_language = args
        .language
        .as_ref()
        .and_then(|s| LanguageId::new(s).known());

    let mut env = Env::new();
    for input in &args.input {
        decode_scip_data(
            &mut env,
            input,
            default_language,
            args.infer_language,
            args.root_prefix.as_deref(),
        )?;
    }

    let output_facts = env.output();
    info!("Found {} facts total", output_facts.total_facts_count());
    let shards = if let Some(shard_size) = args.shard {
        let shards = output_facts.shard(shard_size);
        // pad the output files for correct numerical sorting
        let padding = shards.len().to_string().len();
        shards
            .into_iter()
            .enumerate()
            .map(|(i, shard)| {
                let output = args
                    .output
                    .join(format!("{:0width$}.json", i, width = padding));
                (output, shard)
            })
            .collect()
    } else {
        vec![(args.output, output_facts)]
    };

    let num_files = shards.len();
    for (file, shard) in shards {
        let write = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(&file)
            .with_context(|| format!("Error creating output file {}", file.display()))?;
        let writer = std::io::BufWriter::new(write);

        shard.write(writer)?;
    }
    info!("Wrote {} files", num_files);

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

#[derive(Serialize, Deserialize, Clone, Eq, PartialEq, Hash)]
#[serde(deny_unknown_fields, rename_all = "camelCase")]
struct ToolInfo {
    tool_args: Vec<String>,
    tool_name: String,
    version: String,
}

#[derive(Serialize, Deserialize, Clone, Eq, PartialEq, Hash)]
#[serde(deny_unknown_fields, rename_all = "camelCase")]
struct GleanRange {
    column_begin: u64,
    column_end: u64,
    line_begin: u64,
    line_end: u64,
}

fn decode_scip_range(range: &[i32]) -> Result<GleanRange> {
    // Some scip files have invalid ranges, the best we can do is to fix them up to 0 or 1
    // If we don't this will cause integer overflow when we convert to glean facts
    let mut range = match range {
        [line_begin, column_begin, column_end] => GleanRange {
            line_begin: std::cmp::max(*line_begin + 1, 1) as u64,
            column_begin: std::cmp::max(*column_begin + 1, 1) as u64,
            line_end: std::cmp::max(*line_begin + 1, 1) as u64,
            column_end: std::cmp::max(*column_end, 0) as u64,
        },
        [line_begin, column_begin, line_end, column_end] => GleanRange {
            line_begin: std::cmp::max(*line_begin + 1, 1) as u64,
            column_begin: std::cmp::max(*column_begin + 1, 1) as u64,
            line_end: std::cmp::max(*line_end + 1, 1) as u64,
            column_end: std::cmp::max(*column_end, 0) as u64,
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

#[cfg(test)]
mod tests {
    use tempfile::NamedTempFile;

    use super::*;

    #[test]
    fn test_write_blank_scip() {
        let scip_file = NamedTempFile::new().expect("unable to create temp file");
        let output_json = NamedTempFile::new().expect("unable to create temp file");

        let args = BuildJsonArgs {
            input: vec![scip_file.path().to_path_buf()],
            output: output_json.path().to_path_buf(),
            infer_language: true,
            language: None,
            root_prefix: None,
            shard: None,
        };

        build_json(args).expect("failure building JSON");

        let output_json =
            std::fs::read_to_string(output_json.path()).expect("unable to read output");

        assert_eq!(output_json, "[]\n");
    }

    #[test]
    fn test_write_sharded_scip() {
        let scip_file = NamedTempFile::new().expect("unable to create temp file");
        let output_json_dir = tempfile::TempDir::new().expect("Unable to create temp dir");

        let args = BuildJsonArgs {
            input: vec![scip_file.path().to_path_buf()],
            output: output_json_dir.path().to_path_buf(),
            infer_language: true,
            language: None,
            root_prefix: None,
            shard: Some(100),
        };

        build_json(args).expect("failure building JSON");

        let files_in_dir: Vec<_> = std::fs::read_dir(output_json_dir.path())
            .expect("unable to read output directory")
            .collect();
        let file_count = files_in_dir.len();
        assert_eq!(
            file_count, 1,
            "Expected exactly one file in the output directory"
        );

        let output_file_path = files_in_dir[0].as_ref().unwrap().path();
        let output_json = std::fs::read_to_string(output_file_path).expect("unable to read output");

        assert_eq!(output_json, "[]\n");
    }
}
