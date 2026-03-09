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
#[cfg(feature = "facebook")]
use fbinit::FacebookInit;
use protobuf::Message;
use serde::Deserialize;
use serde::Serialize;
use tracing::info;

#[cfg(not(feature = "facebook"))]
mod proto {
    include!(concat!(env!("OUT_DIR"), "/proto_gen/mod.rs"));
}
#[cfg(not(feature = "facebook"))]
use proto::scip::Index;
#[cfg(feature = "facebook")]
use proto_rust::scip::Index;

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

    #[arg(long, help = "Strip this prefix from the filepaths")]
    strip_prefix: Option<String>,

    #[arg(
        long,
        help = "Root directory for source files. When set, enables src.FileLines emission by reading files from disk at <source-root>/<relative-path>"
    )]
    source_root: Option<PathBuf>,

    #[arg(
        long,
        help = "Shards the JSON graph into subgraphs. Subgraphs will be approximately the size specified. Uses the --output argument as a directory, and writes one file per shard"
    )]
    shard: Option<usize>,
}

#[cfg(feature = "facebook")]
#[cli::main("scip_to_glean", error_logging(user(default_level = "info")))]
async fn main(_fb: FacebookInit, args: BuildJsonArgs) -> Result<cli::ExitCode> {
    build_json(args)?;
    Ok(cli::ExitCode::SUCCESS)
}

#[cfg(not(feature = "facebook"))]
fn main() -> Result<()> {
    env_logger::init();
    let args = BuildJsonArgs::parse();
    build_json(args)
}

fn decode_scip_data(
    env: &mut Env,
    path: &Path,
    default_language: Option<LanguageId>,
    infer_language: bool,
    path_prefix: Option<&str>,
    strip_prefix: Option<&str>,
    source_root: Option<&Path>,
) -> Result<()> {
    info!("Loading documents from {}", path.display());
    let scip_index = read_scip_file(path)
        .with_context(|| format!("Error opening input file {}", path.display()))?;
    info!("Loaded {} documents", scip_index.documents.len());

    if let Some(metadata) = scip_index.metadata.into_option() {
        env.decode_scip_metadata(metadata);
    }
    for doc in scip_index.documents {
        env.decode_scip_doc(
            default_language,
            infer_language,
            path_prefix,
            strip_prefix,
            source_root,
            doc,
        )?;
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
            args.strip_prefix.as_deref(),
            args.source_root.as_deref(),
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

fn decode_scip_range(range: &[i32]) -> Result<Option<GleanRange>> {
    // Some scip files have invalid ranges, the best we can do is to fix them up to 0 or 1
    // If we don't this will cause integer overflow when we convert to glean facts
    let mut range = match range {
        [] => None,
        [line_begin, column_begin, column_end] => Some(GleanRange {
            line_begin: std::cmp::max(*line_begin + 1, 1) as u64,
            column_begin: std::cmp::max(*column_begin + 1, 1) as u64,
            line_end: std::cmp::max(*line_begin + 1, 1) as u64,
            column_end: std::cmp::max(*column_end, 0) as u64,
        }),
        [line_begin, column_begin, line_end, column_end] => Some(GleanRange {
            line_begin: std::cmp::max(*line_begin + 1, 1) as u64,
            column_begin: std::cmp::max(*column_begin + 1, 1) as u64,
            line_end: std::cmp::max(*line_end + 1, 1) as u64,
            column_end: std::cmp::max(*column_end, 0) as u64,
        }),
        _ => {
            return Err(anyhow!("bad range: {:#?}", range));
        }
    };
    if let Some(ref mut r) = range {
        r.column_end = std::cmp::max(r.column_begin, r.column_end);
    }
    Ok(range)
}

fn read_scip_file(file: &Path) -> Result<Index, Error> {
    let scip_file = File::open(file)?;
    let mut reader = std::io::BufReader::new(scip_file);
    Index::parse_from_reader(&mut reader).context("Failed to deserialize scip file")
}

#[cfg(test)]
mod tests {
    #[cfg(feature = "facebook")]
    use proto_rust::scip::Document;
    use tempfile::NamedTempFile;

    #[cfg(not(feature = "facebook"))]
    use super::proto::scip::Document;
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
            strip_prefix: None,
            source_root: None,
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
            strip_prefix: None,
            source_root: None,
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

    /// Helper to create a SCIP index file with a single document
    fn write_scip_index(scip_file: &mut impl std::io::Write, doc: Document) {
        let mut index = Index::new();
        index.documents.push(doc);
        index
            .write_to_writer(scip_file)
            .expect("failed to write SCIP index");
    }

    /// Helper to find a predicate's facts array in the output JSON
    fn find_predicate_facts(json: &str, predicate: &str) -> Option<serde_json::Value> {
        let parsed: serde_json::Value =
            serde_json::from_str(json).expect("output is not valid JSON");
        let entries = parsed.as_array().expect("top-level should be array");
        entries
            .iter()
            .find(|entry| entry["predicate"].as_str() == Some(predicate))
            .map(|entry| entry["facts"].clone())
    }

    #[test]
    fn test_file_lines_from_document_text() {
        let mut scip_file = NamedTempFile::new().expect("unable to create temp file");
        let output_json = NamedTempFile::new().expect("unable to create temp file");

        let mut doc = Document::new();
        doc.relative_path = "test.go".to_string();
        doc.language = "go".to_string();
        doc.text = "package main\n\nfunc main() {\n}\n".to_string();
        write_scip_index(&mut scip_file, doc);

        let args = BuildJsonArgs {
            input: vec![scip_file.path().to_path_buf()],
            output: output_json.path().to_path_buf(),
            infer_language: false,
            language: None,
            root_prefix: None,
            strip_prefix: None,
            source_root: None,
            shard: None,
        };
        build_json(args).expect("failure building JSON");

        let output = std::fs::read_to_string(output_json.path()).expect("unable to read output");

        // Verify src.FileLines predicate is present
        let file_lines_facts = find_predicate_facts(&output, "src.FileLines.1")
            .expect("src.FileLines.1 predicate not found in output");
        let facts = file_lines_facts.as_array().expect("facts should be array");
        assert_eq!(facts.len(), 1, "expected exactly one FileLines fact");

        let fact = &facts[0]["key"];
        // "package main\n" = 13, "\n" = 1, "func main() {\n" = 14, "}\n" = 2
        let lengths: Vec<u64> = fact["lengths"]
            .as_array()
            .unwrap()
            .iter()
            .map(|v| v.as_u64().unwrap())
            .collect();
        assert_eq!(lengths, vec![13, 1, 14, 2]);
        assert_eq!(fact["endsInNewline"], true);
        assert_eq!(fact["hasUnicodeOrTabs"], false);
    }

    #[test]
    fn test_file_lines_from_source_root() {
        let source_dir = tempfile::TempDir::new().expect("unable to create temp dir");
        let source_file_path = source_dir.path().join("hello.go");
        std::fs::write(&source_file_path, "package hello\n\tfmt.Println()\n")
            .expect("failed to write source file");

        let mut scip_file = NamedTempFile::new().expect("unable to create temp file");
        let output_json = NamedTempFile::new().expect("unable to create temp file");

        // Document with no text — FileLines should come from disk via source_root
        let mut doc = Document::new();
        doc.relative_path = "hello.go".to_string();
        doc.language = "go".to_string();
        write_scip_index(&mut scip_file, doc);

        let args = BuildJsonArgs {
            input: vec![scip_file.path().to_path_buf()],
            output: output_json.path().to_path_buf(),
            infer_language: false,
            language: None,
            root_prefix: None,
            strip_prefix: None,
            source_root: Some(source_dir.path().to_path_buf()),
            shard: None,
        };
        build_json(args).expect("failure building JSON");

        let output = std::fs::read_to_string(output_json.path()).expect("unable to read output");

        let file_lines_facts = find_predicate_facts(&output, "src.FileLines.1")
            .expect("src.FileLines.1 predicate not found in output");
        let facts = file_lines_facts.as_array().expect("facts should be array");
        assert_eq!(facts.len(), 1, "expected exactly one FileLines fact");

        let fact = &facts[0]["key"];
        // "package hello\n" = 14, "\tfmt.Println()\n" = 15
        let lengths: Vec<u64> = fact["lengths"]
            .as_array()
            .unwrap()
            .iter()
            .map(|v| v.as_u64().unwrap())
            .collect();
        assert_eq!(lengths, vec![14, 15]);
        assert_eq!(fact["endsInNewline"], true);
        assert_eq!(fact["hasUnicodeOrTabs"], true); // has tab character
    }

    #[test]
    fn test_no_file_lines_without_source_root_or_text() {
        let mut scip_file = NamedTempFile::new().expect("unable to create temp file");
        let output_json = NamedTempFile::new().expect("unable to create temp file");

        // Document with no text and no source_root — no FileLines should be emitted
        let mut doc = Document::new();
        doc.relative_path = "test.go".to_string();
        doc.language = "go".to_string();
        write_scip_index(&mut scip_file, doc);

        let args = BuildJsonArgs {
            input: vec![scip_file.path().to_path_buf()],
            output: output_json.path().to_path_buf(),
            infer_language: false,
            language: None,
            root_prefix: None,
            strip_prefix: None,
            source_root: None,
            shard: None,
        };
        build_json(args).expect("failure building JSON");

        let output = std::fs::read_to_string(output_json.path()).expect("unable to read output");

        // src.FileLines should NOT be present
        assert!(
            find_predicate_facts(&output, "src.FileLines.1").is_none(),
            "src.FileLines should not be emitted without source_root or document text"
        );
    }
}
