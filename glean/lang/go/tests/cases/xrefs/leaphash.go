/*
Copyright (c) Facebook, Inc. and its affiliates.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
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
