/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

//! SCIP symbol parsing according to the specification at third-party/scip/scip.proto
//!
//! This module handles parsing of SCIP symbols, which follow the format:
//! - Local symbols: `local <local-id>`
//! - Global symbols: `<scheme> <manager> <package-name> <version> <descriptors>+`
//!
//! Key features:
//! - Handles escaped identifiers (backtick-enclosed with double-backtick escaping)
//! - Parses all descriptor types (namespace, type, term, method, parameter, etc.)
//! - Supports the "." placeholder for empty package fields

#[allow(dead_code)]
pub enum ScipSymbol {
    Local {
        id: String,
    },
    Global {
        scheme: String,
        package: Package,
        descriptors: Vec<Descriptor>,
    },
}

#[allow(dead_code)]
pub struct Package {
    pub manager: Option<String>,
    pub name: Option<String>,
    pub version: Option<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Descriptor {
    pub name: String,
    pub kind: DescriptorKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DescriptorKind {
    Namespace,              // name/
    Type,                   // name#
    Term,                   // name.
    Method(Option<String>), // name(<disambiguator>?).
    TypeParameter,          // [name]
    Parameter,              // (name)
    Meta,                   // name:
    Macro,                  // name!
}

/// Parses a SCIP symbol string according to the SCIP specification.
///
/// # Format
/// Symbols follow this format:
/// - Local: `local <local-id>`
/// - Global: `<scheme> <manager> <package-name> <version> <descriptors>+`
///
/// Where:
/// - Spaces in scheme/manager/package-name/version are escaped with double spaces
/// - Single spaces delimit fields
/// - "." is used as a placeholder for empty values in package fields
/// - Descriptors can be: namespace (/), type (#), term (.), method ((.).),
///   type-parameter ([]), parameter (()), meta (:), or macro (!)
///
/// # Examples
/// ```
/// use scip_symbol::parse_scip_symbol;
///
/// let symbol = "rust-analyzer cargo std v1.0 io/IsTerminal#";
/// let parsed = parse_scip_symbol(symbol);
/// ```
pub fn parse_scip_symbol(symbol: &str) -> ScipSymbol {
    // Handle local symbols: "local <local-id>"
    if let Some(("local", rest)) = symbol.split_once(' ') {
        return ScipSymbol::Local {
            id: rest.to_string(),
        };
    }

    // Parse global symbols: "<scheme> <manager> <package-name> <version> <descriptors>+"
    // Each field is space-escaped (double spaces represent single spaces)
    // Single spaces delimit fields

    let (scheme, rest) = parse_space_escaped_field(symbol);
    let (manager, rest) = parse_space_escaped_field(rest);
    let (pkgname, rest) = parse_space_escaped_field(rest);
    let (version, descriptors_str) = parse_space_escaped_field(rest);

    // Convert "." placeholder to None according to SCIP spec
    let package = Package {
        manager: if manager == "." { None } else { Some(manager) },
        name: if pkgname == "." { None } else { Some(pkgname) },
        version: if version == "." { None } else { Some(version) },
    };

    let descriptors = parse_descriptors(descriptors_str);

    ScipSymbol::Global {
        scheme,
        package,
        descriptors,
    }
}

/// Parses a space-escaped field according to SCIP spec.
///
/// According to the SCIP spec, spaces in scheme/manager/package-name/version
/// are escaped with double spaces. This function:
/// 1. Reads until it finds a single space (field delimiter)
/// 2. Converts double spaces to single spaces in the result
///
/// Returns (field, remaining_string)
fn parse_space_escaped_field(s: &str) -> (String, &str) {
    let mut chars = s.char_indices().peekable();

    while let Some((i, ch)) = chars.next() {
        if ch == ' ' {
            // Check if this is a double space (escaped space)
            if let Some(&(_, next_ch)) = chars.peek() {
                if next_ch == ' ' {
                    // Skip the second space (this is an escaped space within the field)
                    chars.next();
                    continue;
                }
            }
            // Single space - this is the field delimiter
            let rest_start = i + 1;
            return (unescape_spaces(&s[..i]), &s[rest_start..]);
        }
    }

    // No delimiter found, return entire string
    (unescape_spaces(s), "")
}

/// Unescapes double spaces in a string field.
///
/// According to SCIP spec, double spaces in scheme/manager/package-name/version
/// represent a single space character.
fn unescape_spaces(s: &str) -> String {
    s.replace("  ", " ")
}

/// Parses an identifier, handling both simple and escaped identifiers.
///
/// According to SCIP spec:
/// - Simple identifiers: contain only '_', '+', '-', '$', or ASCII alphanumeric chars
/// - Escaped identifiers: surrounded by backticks, backticks escaped as ``, can contain any UTF-8
///
/// Returns (unescaped_name, bytes_consumed)
fn parse_identifier(s: &str, start: usize) -> (String, usize) {
    let substr = &s[start..];
    let mut chars = substr.char_indices().peekable();

    if let Some(&(_, '`')) = chars.peek() {
        // Escaped identifier: parse until closing backtick
        let mut result = String::new();
        chars.next(); // consume the opening backtick

        while let Some((byte_offset, ch)) = chars.next() {
            if ch == '`' {
                // Check if this is a double backtick (escaped backtick)
                if let Some(&(_, next_ch)) = chars.peek() {
                    if next_ch == '`' {
                        // Escaped backtick - add single backtick to result
                        result.push('`');
                        chars.next(); // consume the second backtick
                        continue;
                    }
                }
                // Single backtick - this closes the identifier
                // byte_offset is relative to start of substr, pointing to closing backtick
                // Return: byte_offset + 1 (to include the closing backtick)
                return (result, byte_offset + 1);
            } else {
                result.push(ch);
            }
        }

        // If we get here, no closing backtick was found
        // Return what we have and consume to end
        (result, s.len() - start)
    } else {
        // Simple identifier: parse until we hit a non-identifier character
        // According to spec, identifier-character is ASCII only: '_' | '+' | '-' | '$' | ASCII letter or digit
        let mut last_valid_byte_offset = 0;

        for (byte_offset, ch) in chars {
            if ch.is_ascii() {
                let byte = ch as u8;
                match byte {
                    b'_' | b'+' | b'-' | b'$' | b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' => {
                        last_valid_byte_offset = byte_offset + 1;
                    }
                    _ => break,
                }
            } else {
                // Non-ASCII character - stop parsing here (this violates spec but we handle gracefully)
                log::warn!(
                    "Non-ASCII character {} found in identifier in SCIP symbol {}",
                    ch,
                    s
                );
                break;
            }
        }

        // Return the slice directly (no unescaping needed for simple identifiers)
        (
            s[start..start + last_valid_byte_offset].to_string(),
            last_valid_byte_offset,
        )
    }
}

/// Parses a type parameter descriptor: [name]
///
/// Returns (descriptor, new_position)
fn parse_type_parameter(descriptors_str: &str, start_pos: usize) -> (Descriptor, usize) {
    let bytes = descriptors_str.as_bytes();
    let mut i = start_pos + 1; // Skip '['

    let (name, consumed) = parse_identifier(descriptors_str, i);
    i += consumed;

    // Skip to ']'
    while i < bytes.len() && bytes[i] != b']' {
        i += 1;
    }
    i += 1; // Skip ']'

    (
        Descriptor {
            name,
            kind: DescriptorKind::TypeParameter,
        },
        i,
    )
}

/// Parses a parameter descriptor: (name)
///
/// Returns (descriptor, new_position)
fn parse_parameter(descriptors_str: &str, start_pos: usize) -> (Descriptor, usize) {
    let bytes = descriptors_str.as_bytes();
    let mut i = start_pos + 1; // Skip '('

    let (name, consumed) = parse_identifier(descriptors_str, i);
    i += consumed;

    // Skip to ')'
    while i < bytes.len() && bytes[i] != b')' {
        i += 1;
    }
    i += 1; // Skip ')'

    (
        Descriptor {
            name,
            kind: DescriptorKind::Parameter,
        },
        i,
    )
}

/// Parses a method descriptor: name(<disambiguator>?).
///
/// Returns (descriptor, new_position)
fn parse_method(descriptors_str: &str, name: String, start_pos: usize) -> (Descriptor, usize) {
    let bytes = descriptors_str.as_bytes();
    let mut i = start_pos + 1; // Skip '('

    let disambiguator_start = i;
    while i < bytes.len() && bytes[i] != b')' {
        i += 1;
    }
    let disambiguator_end = i;

    let disambiguator = if disambiguator_start == disambiguator_end {
        None
    } else {
        Some(descriptors_str[disambiguator_start..disambiguator_end].to_string())
    };

    i += 1; // Skip ')'
    if i < bytes.len() && bytes[i] == b'.' {
        i += 1; // Skip '.'
    }

    (
        Descriptor {
            name,
            kind: DescriptorKind::Method(disambiguator),
        },
        i,
    )
}

/// Parses a simple descriptor with a single-char suffix: /, #, ., :, or !
///
/// Returns (descriptor, new_position)
fn parse_simple_descriptor(name: String, suffix_char: u8, pos: usize) -> (Descriptor, usize) {
    let kind = match suffix_char {
        b'/' => DescriptorKind::Namespace,
        b'#' => DescriptorKind::Type,
        b'.' => DescriptorKind::Term,
        b':' => DescriptorKind::Meta,
        b'!' => DescriptorKind::Macro,
        _ => unreachable!(),
    };

    // For macros, include the '!' in the name
    let final_name = if matches!(kind, DescriptorKind::Macro) {
        format!("{}!", name)
    } else {
        name
    };

    (
        Descriptor {
            name: final_name,
            kind,
        },
        pos + 1, // Skip suffix
    )
}

/// Parses a chain of SCIP descriptors from a descriptor string.
///
/// Returns a vector of all descriptors found in the string.
fn parse_descriptors(descriptors_str: &str) -> Vec<Descriptor> {
    if descriptors_str.is_empty() {
        return Vec::new();
    }

    let mut result = Vec::new();
    let bytes = descriptors_str.as_bytes();
    let mut i = 0;

    while i < bytes.len() {
        if bytes[i] == b'[' {
            // Type parameter: [name]
            let (descriptor, new_pos) = parse_type_parameter(descriptors_str, i);
            result.push(descriptor);
            i = new_pos;
        } else if bytes[i] == b'(' {
            // Parameter: (name)
            let (descriptor, new_pos) = parse_parameter(descriptors_str, i);
            result.push(descriptor);
            i = new_pos;
        } else {
            // Regular descriptor: name followed by a single-char suffix or method
            let start = i;
            let (name, consumed) = parse_identifier(descriptors_str, start);
            i = start + consumed;

            if i < bytes.len() {
                let c = bytes[i];
                match c {
                    b'/' | b'#' | b'.' | b':' | b'!' => {
                        let (descriptor, new_pos) = parse_simple_descriptor(name, c, i);
                        result.push(descriptor);
                        i = new_pos;
                    }
                    b'(' => {
                        // Method: name(<disambiguator>?).
                        let (descriptor, new_pos) = parse_method(descriptors_str, name, i);
                        result.push(descriptor);
                        i = new_pos;
                    }
                    _ => {
                        // No recognized suffix, skip this character
                        // We need to skip by the full UTF-8 character length to avoid
                        // landing in the middle of a multi-byte character
                        if let Some(ch) = descriptors_str[i..].chars().next() {
                            i += ch.len_utf8();
                        } else {
                            i += 1;
                        }
                    }
                }
            }
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symbol_from_string_global_type() {
        let symbol = "rust-analyzer cargo std https://github.com/rust-lang/rust/library/std io/stdio/IsTerminal#";
        let symbol = parse_scip_symbol(symbol);
        let ScipSymbol::Global {
            scheme,
            package,
            descriptors,
        } = symbol
        else {
            panic!("expected global symbol");
        };

        assert_eq!(scheme, "rust-analyzer");
        assert_eq!(package.manager, Some("cargo".to_string()));
        assert_eq!(package.name, Some("std".to_string()));
        assert_eq!(
            package.version,
            Some("https://github.com/rust-lang/rust/library/std".to_string())
        );

        assert_eq!(descriptors.len(), 3);
        assert_eq!(descriptors[0].name, "io");
        assert_eq!(descriptors[0].kind, DescriptorKind::Namespace);
        assert_eq!(descriptors[1].name, "stdio");
        assert_eq!(descriptors[1].kind, DescriptorKind::Namespace);
        assert_eq!(descriptors[2].name, "IsTerminal");
        assert_eq!(descriptors[2].kind, DescriptorKind::Type);
    }

    #[test]
    fn test_symbol_from_string_global_unspecified() {
        let symbol =
            "rust-analyzer cargo std https://github.com/rust-lang/rust/library/std macros/println!";
        let symbol = parse_scip_symbol(symbol);
        let ScipSymbol::Global {
            scheme,
            package,
            descriptors,
        } = symbol
        else {
            panic!("expected global symbol");
        };

        assert_eq!(scheme, "rust-analyzer");
        assert_eq!(package.manager, Some("cargo".to_string()));
        assert_eq!(package.name, Some("std".to_string()));
        assert_eq!(
            package.version,
            Some("https://github.com/rust-lang/rust/library/std".to_string())
        );

        assert_eq!(descriptors.len(), 2);
        assert_eq!(descriptors[0].name, "macros");
        assert_eq!(descriptors[0].kind, DescriptorKind::Namespace);
        assert_eq!(descriptors[1].name, "println!");
        assert_eq!(descriptors[1].kind, DescriptorKind::Macro);
    }

    #[test]
    fn test_symbol_from_string_method() {
        // Test method with no disambiguator
        let symbol = "scip . . . MyClass#myMethod().";
        let symbol = parse_scip_symbol(symbol);
        let ScipSymbol::Global { descriptors, .. } = symbol else {
            panic!("expected global symbol");
        };

        assert_eq!(descriptors.len(), 2);
        assert_eq!(descriptors[0].name, "MyClass");
        assert_eq!(descriptors[0].kind, DescriptorKind::Type);
        assert_eq!(descriptors[1].name, "myMethod");
        assert_eq!(descriptors[1].kind, DescriptorKind::Method(None));
    }

    #[test]
    fn test_symbol_from_string_type_parameter() {
        let symbol = "scip . . . List#[T]";
        let symbol = parse_scip_symbol(symbol);
        let ScipSymbol::Global { descriptors, .. } = symbol else {
            panic!("expected global symbol");
        };

        assert_eq!(descriptors.len(), 2);
        assert_eq!(descriptors[0].name, "List");
        assert_eq!(descriptors[0].kind, DescriptorKind::Type);
        assert_eq!(descriptors[1].name, "T");
        assert_eq!(descriptors[1].kind, DescriptorKind::TypeParameter);
    }

    #[test]
    fn test_symbol_from_string_parameter() {
        let symbol = "scip . . . func().(param)";
        let symbol = parse_scip_symbol(symbol);
        let ScipSymbol::Global { descriptors, .. } = symbol else {
            panic!("expected global symbol");
        };

        assert_eq!(descriptors.len(), 2);
        assert_eq!(descriptors[0].name, "func");
        assert_eq!(descriptors[0].kind, DescriptorKind::Method(None));
        assert_eq!(descriptors[1].name, "param");
        assert_eq!(descriptors[1].kind, DescriptorKind::Parameter);
    }

    #[test]
    fn test_symbol_from_string_term() {
        let symbol = "scip . . . package/MyClass.field.";
        let symbol = parse_scip_symbol(symbol);
        let ScipSymbol::Global { descriptors, .. } = symbol else {
            panic!("expected global symbol");
        };

        assert_eq!(descriptors.len(), 3);
        assert_eq!(descriptors[0].name, "package");
        assert_eq!(descriptors[0].kind, DescriptorKind::Namespace);
        assert_eq!(descriptors[1].name, "MyClass");
        assert_eq!(descriptors[1].kind, DescriptorKind::Term);
        assert_eq!(descriptors[2].name, "field");
        assert_eq!(descriptors[2].kind, DescriptorKind::Term);
    }

    #[test]
    fn test_symbol_from_string_meta() {
        let symbol = "scip . . . annotation:";
        let symbol = parse_scip_symbol(symbol);
        let ScipSymbol::Global { descriptors, .. } = symbol else {
            panic!("expected global symbol");
        };

        assert_eq!(descriptors.len(), 1);
        assert_eq!(descriptors[0].name, "annotation");
        assert_eq!(descriptors[0].kind, DescriptorKind::Meta);
    }

    #[test]
    fn test_symbol_from_string_local() {
        let symbol = "local myVar";
        let symbol = parse_scip_symbol(symbol);
        let ScipSymbol::Local { id } = symbol else {
            panic!("expected local symbol");
        };

        assert_eq!(id, "myVar");
    }

    #[test]
    fn test_symbol_from_string_package_placeholder() {
        // Test that "." is converted to None for package fields
        let symbol = "rust-analyzer . . . io/stdio/IsTerminal#";
        let symbol = parse_scip_symbol(symbol);
        let ScipSymbol::Global { package, .. } = symbol else {
            panic!("expected global symbol");
        };

        assert_eq!(package.manager, None);
        assert_eq!(package.name, None);
        assert_eq!(package.version, None);
    }

    #[test]
    fn test_escaped_identifier_simple() {
        // Test escaped identifier with space
        let symbol = "scip . . . `has space`/";
        let symbol = parse_scip_symbol(symbol);
        let ScipSymbol::Global { descriptors, .. } = symbol else {
            panic!("expected global symbol");
        };

        assert_eq!(descriptors.len(), 1);
        assert_eq!(descriptors[0].name, "has space");
        assert_eq!(descriptors[0].kind, DescriptorKind::Namespace);
    }

    #[test]
    fn test_escaped_identifier_with_backticks() {
        // Test escaped identifier with backticks (double backtick escaping)
        let symbol = "scip . . . `name``with``backticks`#";
        let symbol = parse_scip_symbol(symbol);
        let ScipSymbol::Global { descriptors, .. } = symbol else {
            panic!("expected global symbol");
        };

        assert_eq!(descriptors.len(), 1);
        assert_eq!(descriptors[0].name, "name`with`backticks");
        assert_eq!(descriptors[0].kind, DescriptorKind::Type);
    }

    #[test]
    fn test_escaped_identifier_method() {
        // Test escaped identifier in method name
        let symbol = "scip . . . MyClass#`special method`().";
        let symbol = parse_scip_symbol(symbol);
        let ScipSymbol::Global { descriptors, .. } = symbol else {
            panic!("expected global symbol");
        };

        assert_eq!(descriptors.len(), 2);
        assert_eq!(descriptors[0].name, "MyClass");
        assert_eq!(descriptors[0].kind, DescriptorKind::Type);
        assert_eq!(descriptors[1].name, "special method");
        assert_eq!(descriptors[1].kind, DescriptorKind::Method(None));
    }

    #[test]
    fn test_method_with_disambiguator() {
        // Test method with disambiguator
        let symbol = "scip . . . MyClass#myMethod(disambig).";
        let symbol = parse_scip_symbol(symbol);
        let ScipSymbol::Global { descriptors, .. } = symbol else {
            panic!("expected global symbol");
        };

        assert_eq!(descriptors.len(), 2);
        assert_eq!(descriptors[0].name, "MyClass");
        assert_eq!(descriptors[0].kind, DescriptorKind::Type);
        assert_eq!(descriptors[1].name, "myMethod");
        assert_eq!(
            descriptors[1].kind,
            DescriptorKind::Method(Some("disambig".to_string()))
        );
    }

    #[test]
    fn test_space_escaped_scheme() {
        // Test scheme with space (escaped as double space)
        let symbol = "my  scheme cargo pkg v1.0 MyClass#";
        let symbol = parse_scip_symbol(symbol);
        let ScipSymbol::Global {
            scheme, package, ..
        } = symbol
        else {
            panic!("expected global symbol");
        };

        assert_eq!(scheme, "my scheme");
        assert_eq!(package.manager, Some("cargo".to_string()));
        assert_eq!(package.name, Some("pkg".to_string()));
        assert_eq!(package.version, Some("v1.0".to_string()));
    }

    #[test]
    fn test_space_escaped_manager() {
        // Test manager with space (escaped as double space)
        let symbol = "scip my  manager pkg v1.0 MyClass#";
        let symbol = parse_scip_symbol(symbol);
        let ScipSymbol::Global { package, .. } = symbol else {
            panic!("expected global symbol");
        };

        assert_eq!(package.manager, Some("my manager".to_string()));
        assert_eq!(package.name, Some("pkg".to_string()));
        assert_eq!(package.version, Some("v1.0".to_string()));
    }

    #[test]
    fn test_space_escaped_package_name() {
        // Test package name with space (escaped as double space)
        let symbol = "scip cargo my  package v1.0 MyClass#";
        let symbol = parse_scip_symbol(symbol);
        let ScipSymbol::Global { package, .. } = symbol else {
            panic!("expected global symbol");
        };

        assert_eq!(package.manager, Some("cargo".to_string()));
        assert_eq!(package.name, Some("my package".to_string()));
        assert_eq!(package.version, Some("v1.0".to_string()));
    }

    #[test]
    fn test_space_escaped_version() {
        // Test version with space (escaped as double space)
        let symbol = "scip cargo pkg v1.0  beta MyClass#";
        let symbol = parse_scip_symbol(symbol);
        let ScipSymbol::Global { package, .. } = symbol else {
            panic!("expected global symbol");
        };

        assert_eq!(package.manager, Some("cargo".to_string()));
        assert_eq!(package.name, Some("pkg".to_string()));
        assert_eq!(package.version, Some("v1.0 beta".to_string()));
    }

    #[test]
    fn test_multiple_space_escapes() {
        // Test multiple spaces in multiple fields
        let symbol = "my  scheme my  manager my  package my  version MyClass#";
        let symbol = parse_scip_symbol(symbol);
        let ScipSymbol::Global {
            scheme, package, ..
        } = symbol
        else {
            panic!("expected global symbol");
        };

        assert_eq!(scheme, "my scheme");
        assert_eq!(package.manager, Some("my manager".to_string()));
        assert_eq!(package.name, Some("my package".to_string()));
        assert_eq!(package.version, Some("my version".to_string()));
    }

    #[test]
    fn test_space_escapes_multiple_spaces() {
        // Test multiple consecutive spaces (4 spaces = 2 spaces in result)
        let symbol = "my    scheme cargo pkg v1.0 MyClass#";
        let symbol = parse_scip_symbol(symbol);
        let ScipSymbol::Global { scheme, .. } = symbol else {
            panic!("expected global symbol");
        };

        assert_eq!(scheme, "my  scheme");
    }

    #[test]
    fn test_rubbish_objc() {
        // Test multiple consecutive spaces (4 spaces = 2 spaces in result)
        let symbol_str = "c:(cm)ArtemisMockDeviceScanner_get_interface_plugin_MockDeviceScanners";
        let symbol = parse_scip_symbol(symbol_str);
        let ScipSymbol::Global { scheme, .. } = symbol else {
            panic!("expected global symbol");
        };
        assert_eq!(scheme, symbol_str);
    }

    #[test]
    fn test_rubbish_swift() {
        // Test multiple consecutive spaces (4 spaces = 2 spaces in result)
        let symbol_str = "s:010ContactRowA10CacheTestsAAC04msgrA013FWAShareSheet0A4InfoVvg";
        let symbol = parse_scip_symbol(symbol_str);
        let ScipSymbol::Global { scheme, .. } = symbol else {
            panic!("expected global symbol");
        };
        assert_eq!(scheme, symbol_str);
    }

    #[test]
    fn test_utf8_escaped_identifier_mixed() {
        // Test escaped identifier with mixed UTF-8 characters and backtick escaping
        let symbol = "scip . . . `hello``世界``🌍`#";
        let symbol = parse_scip_symbol(symbol);
        let ScipSymbol::Global { descriptors, .. } = symbol else {
            panic!("expected global symbol");
        };

        assert_eq!(descriptors.len(), 1);
        assert_eq!(descriptors[0].name, "hello`世界`🌍");
        assert_eq!(descriptors[0].kind, DescriptorKind::Type);
    }

    #[test]
    fn test_utf8_in_manager() {
        // Test UTF-8 characters in manager field
        let symbol = "scip cargo🌍 pkg v1.0 MyClass#";
        let symbol = parse_scip_symbol(symbol);
        let ScipSymbol::Global { package, .. } = symbol else {
            panic!("expected global symbol");
        };

        assert_eq!(package.manager, Some("cargo🌍".to_string()));
    }

    #[test]
    fn test_production_case_non_ascii_in_simple_identifier() {
        // Test the real production case that was causing panics
        // Symbol contains 'Å' (U+00C5, 2-byte UTF-8: 0xC3 0x85) in ROTÅTION
        // Even though this violates the spec (simple identifiers should be ASCII only),
        // we should handle it gracefully without panicking
        let symbol = "com/instagram/feed/opencarousel/consumption/OpenCarouselConstantsUtil#Companion#MEDIA_CARD_STACK_ROTÅTION.";
        let symbol = parse_scip_symbol(symbol);
        let ScipSymbol::Global {
            scheme,
            descriptors,
            ..
        } = symbol
        else {
            panic!("expected global symbol");
        };

        // This entire string is treated as the scheme since there's no space to delimit it
        assert_eq!(
            scheme,
            "com/instagram/feed/opencarousel/consumption/OpenCarouselConstantsUtil#Companion#MEDIA_CARD_STACK_ROTÅTION."
        );

        // No descriptors will be parsed since the entire string is consumed as the scheme
        assert_eq!(descriptors.len(), 0);
    }

    #[test]
    fn test_non_ascii_in_simple_identifier_with_proper_delimiter() {
        // Test that we handle non-ASCII gracefully when it appears in a simple identifier
        // with proper space delimiters. Uses Å (2-byte UTF-8) to test character boundary handling.
        // This violates the SCIP spec (simple identifiers should be ASCII only), but we handle
        // it gracefully to avoid panics in production.
        let symbol = "scip . . . ROTÅTION.";
        let symbol = parse_scip_symbol(symbol);
        let ScipSymbol::Global { descriptors, .. } = symbol else {
            panic!("expected global symbol");
        };

        // The parser stops at non-ASCII 'Å', skips it, and continues parsing.
        // It parses "TION" as a separate term after skipping the invalid UTF-8 in the simple identifier.
        // While not ideal, this behavior avoids panics on malformed input.
        assert_eq!(descriptors.len(), 1);
        assert_eq!(descriptors[0].name, "TION");
        assert_eq!(descriptors[0].kind, DescriptorKind::Term);
    }
}
