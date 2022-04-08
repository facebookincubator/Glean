/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree.
 */

// Package leaphash is a utility package for computing the hash value of the
// official leap-second.list document
package leaphash

import (
	"crypto/sha1"
	"fmt"
	"strings"
)

// Compute returns the sha1 sum of all non whitespace characters in data
// excluding comments. Since it includes logic for special lines specific
// to the leap-second.list format its not a general purpose function and
// should be only used to verify the integrity of an official leap-second.list
// document.
func Compute(data string) string {
	lines := strings.Split(string(data), "\n")

	var filtered string
	filterBlanks := func(r rune) rune {
		if r == ' ' || r == '\t' {
			return -1
		}
		return r
	}

	for i := 0; i < len(lines); i++ {
		if strings.HasPrefix(lines[i], "#$") || strings.HasPrefix(lines[i], "#@") {
			// "#$" - last modification time
			// "#@" - the expiration time of the file
			filtered += strings.Map(filterBlanks, lines[i][2:len(lines[i])])
		} else if strings.HasPrefix(lines[i], "#") {
			// comment
		} else {
			// leap second lines, without comments and without any blank characters
			line := lines[i]

			commentPos := strings.Index(line, "#")
			if commentPos != -1 {
				line = line[0 : commentPos-1]
			}

			filtered += strings.Map(filterBlanks, line)
		}
	}

	// checksum
	hash := fmt.Sprintf("%x", sha1.Sum([]byte(filtered)))

	var groupedHash string
	// group checksum by 8 characters
	for i := 0; i < 5; i++ {
		if groupedHash != "" {
			groupedHash += " "
		}
		groupedHash += hash[i*8 : (i+1)*8]
	}

	return groupedHash
}
