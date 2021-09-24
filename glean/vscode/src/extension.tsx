/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 */

import {getLogger} from 'nuclide-logging';
import {configureExtensionLogging} from 'nuclide-commons/extensionLogging';
import * as vscode from 'vscode';

const logger = getLogger('Glean');

export async function activate(context: vscode.ExtensionContext): Promise<void> {
  await configureExtensionLogging(context, 'Glean');
  logger.info(`Activated Glean development Support extension`);
}
