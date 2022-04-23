/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

// rust-glean/tests/integration.rs

use assert_cmd::Command as AssertCommand;
use once_cell::sync::OnceCell;
use serde_json::Value;
use std::env;
use std::env::consts::EXE_EXTENSION;
use std::fs::File;
use std::io::{BufRead, BufWriter, Read};
use std::path::PathBuf;
use std::process::Command;

fn should_update_test_expectations() -> bool {
    static VARIABLE: OnceCell<bool> = OnceCell::new();
    *VARIABLE.get_or_init(|| match env::var("GLEAN_RUST_UPDATE_TEST_EXPECTATIONS") {
        Err(_) => false,
        Ok(value) => {
            let value = value.to_lowercase();
            if value == "on" || value == "yes" || value == "true" {
                true
            } else {
                match value.parse::<i32>() {
                    Err(_) => false,
                    Ok(value) => value > 0,
                }
            }
        }
    })
}

fn compiler_path() -> &'static PathBuf {
    static VARIABLE: OnceCell<PathBuf> = OnceCell::new();
    VARIABLE.get_or_init(|| match env::var_os("GLEAN_RUST_COMPILER_PATH") {
        None => PathBuf::from("rustc"),
        Some(string) => PathBuf::from(string),
    })
}

// Copied from `assert_cmd`.
fn target_dir() -> PathBuf {
    env::current_exe()
        .ok()
        .map(|mut path| {
            path.pop();
            if path.ends_with("deps") {
                path.pop();
            }
            path
        })
        .unwrap()
}

fn run_integration_test(test_name: &str) {
    // Generate path to source file.
    let cargo_manifest_dir = PathBuf::from(env::var_os("CARGO_MANIFEST_DIR").expect(
        "`CARGO_MANIFEST_DIR` environment variable must be set (make sure you're running via \
        `cargo test`)",
    ));
    let mut src_dir_path = PathBuf::from(env::current_dir().unwrap());
    src_dir_path.push(&cargo_manifest_dir);
    src_dir_path.push("resources");
    src_dir_path.push("tests");
    let mut src_path = src_dir_path.clone();
    src_path.push(test_name);
    src_path.set_extension("rs");

    // Create temporary directory.
    let temp_out_dir = tempfile::tempdir().unwrap();

    // Run rustc.
    let exit_status = Command::new(compiler_path())
        .arg("--crate-type")
        .arg("lib")
        .arg("-Z")
        .arg("save-analysis")
        .arg("--remap-path-prefix")
        .arg(&format!("{}={}", src_dir_path.display(), "."))
        .arg("-o")
        .arg(test_name)
        .arg(src_path.as_os_str())
        .env("RUSTC_BOOTSTRAP", "1")
        .current_dir(&temp_out_dir)
        .spawn()
        .unwrap()
        .wait()
        .unwrap();
    if !exit_status.success() {
        panic!("Failed to run rustc: {:?}", exit_status)
    }

    // Determine analysis JSON path.
    let mut analysis_json_path = temp_out_dir.path().to_owned();
    analysis_json_path.push("save-analysis-temp");
    analysis_json_path.push(&format!("lib{}.json", test_name));

    // Determine `rust-glean` path.
    let rust_glean_path = match env::var("RUST_GLEAN_BINARY_PATH") {
        Ok(value) => PathBuf::from(value),
        Err(_) => {
            let mut rust_glean_path = target_dir();
            rust_glean_path.push("rust-glean");
            rust_glean_path.set_extension(EXE_EXTENSION);
            rust_glean_path
        }
    };

    // Run `rust-glean`.
    let output = AssertCommand::new(rust_glean_path)
        .arg(&analysis_json_path)
        .output()
        .unwrap();
    let mut json_output: Vec<Value> = vec![];
    for line in output.stdout.lines() {
        if let Ok(line) = line {
            json_output.push(serde_json::from_str(&line).unwrap());
        }
    }

    // Determine expectation JSON path.
    let mut expectation_json_path = src_path.clone();
    expectation_json_path.set_extension("json");

    // Check, or write expected JSON if requested.
    if should_update_test_expectations() {
        let mut src_json_file = BufWriter::new(File::create(&expectation_json_path).unwrap());
        serde_json::to_writer_pretty(&mut src_json_file, &json_output).unwrap();
    } else {
        let actual = serde_json::to_string_pretty(&mut json_output).unwrap();
        let mut expected = String::new();
        File::open(&expectation_json_path)
            .unwrap()
            .read_to_string(&mut expected)
            .unwrap();
        assert_eq!(actual, expected);
    }
}

#[test]
#[ignore]
fn test_const() {
    run_integration_test("const");
}
#[test]
#[ignore]
fn test_enum() {
    run_integration_test("enum");
}
#[test]
#[ignore]
fn test_foreign_function() {
    run_integration_test("foreign_function");
}
#[test]
#[ignore]
fn test_foreign_static() {
    run_integration_test("foreign_static");
}
#[test]
#[ignore]
fn test_function() {
    run_integration_test("function");
}
#[test]
#[ignore]
fn test_impl() {
    run_integration_test("impl");
}
#[test]
#[ignore]
fn test_module() {
    run_integration_test("module");
}
#[test]
#[ignore]
fn test_static() {
    run_integration_test("static");
}
#[test]
#[ignore]
fn test_struct() {
    run_integration_test("struct");
}
#[test]
#[ignore]
fn test_trait() {
    run_integration_test("trait");
}
#[test]
#[ignore]
fn test_typedef() {
    run_integration_test("typedef");
}
#[test]
#[ignore]
fn test_union() {
    run_integration_test("union");
}
