---
id: lsp
title: Setting up the LSP server
sidebar_label: Setting up the LSP server
---

import {OssOnly, FbInternalOnly} from 'internaldocs-fb-helpers';
import {SrcFile,SrcFileLink} from '@site/utils';

Glean comes with a generic LSP server that can be used for browsing
large codebases in [VS Code](https://code.visualstudio.com/), or any
IDE that supports the LSP protocol. This LSP server is intended to
be used in a read-only way: it doesn't currently support updating
the data if the source code is edited, but it can be useful for
navigating a large codebase using familiar IDE features.

The steps to set it up are:

1. [Build and install Glean and glean-lsp](building.md)
2. [Index your source code](indexer/intro.md) into a local DB
2. Follow the instructions in `glean-lsp`'s <SrcFileLink
file="glean/lsp/README.md">README.md</SrcFileLink> to set up the LSP server with VS Code, or your IDE.
