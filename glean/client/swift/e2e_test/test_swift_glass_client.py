#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
# All rights reserved.
#
# This source code is licensed under the BSD-style license found in the
# LICENSE file in the root directory of this source tree.


"""
End-to-end tests for swift_glass_client binary.

This test suite starts the swift_glass_client binary and communicates with it
via stdin/stdout to test various request scenarios.
"""

import json
import os
import subprocess
import time
import unittest
from pathlib import Path
from typing import Any, Dict


class SwiftGlassClientE2ETest(unittest.TestCase):
    """End-to-end tests for swift_glass_client binary."""

    def setUp(self):
        """Set up test by determining binary path."""
        # The binary path is provided through the SWIFT_GLASS_CLIENT_BINARY environment variable
        # which is set by Buck using $(location ...) in the BUCK file
        self._binary_path = os.environ.get("SWIFT_GLASS_CLIENT_BINARY")

        if not self._binary_path:
            raise RuntimeError(
                "SWIFT_GLASS_CLIENT_BINARY environment variable not set. "
                "Make sure the BUCK file is configured correctly with env parameter."
            )

        if not Path(self._binary_path).exists():
            raise RuntimeError(
                f"Binary not found at path: {self._binary_path}. "
                f"Make sure fbcode//glean/client/swift:swift_glass_client is properly built."
            )

        # Get current revision dynamically
        self.revision = subprocess.check_output(["hg", "id"], text=True).strip()
        print(f"Using swift_glass_client binary at: {self._binary_path}")

    def _start_process(self):
        """Start a new swift_glass_client process for the current test."""
        # Start the swift_glass_client binary directly using the pre-built path
        process = subprocess.Popen(
            [self._binary_path],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            bufsize=0,  # Unbuffered for immediate communication
        )

        # Wait for Glass connection initialization
        self._wait_for_glass_initialization(process)
        return process

    def _stop_process(self, process):
        """Stop the swift_glass_client process by sending an empty line."""
        if process and process.poll() is None:
            # Send empty line to signal termination
            process.stdin.write("\n")
            process.stdin.flush()

            # Wait for process to terminate gracefully
            try:
                process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                # If it doesn't terminate gracefully, force kill
                process.terminate()
                process.wait()

    def _wait_for_glass_initialization(self, process):
        """
        Wait for Glass connection initialization logs in stderr.

        Waits for:
        1. "Warming up Glass connection..." log
        2. "Glass connection established successfully" log

        Fails if initialization doesn't complete within 5 minutes.
        """
        timeout_seconds = 1 * 60  # 1 minute
        start_time = time.time()

        warming_up_found = False
        connection_established_found = False

        while time.time() - start_time < timeout_seconds:
            # Check if process has terminated
            if process.poll() is not None:
                self.fail(
                    "swift_glass_client process terminated before Glass initialization"
                )

            # Read stderr with a short timeout to avoid blocking indefinitely
            try:
                # Use a non-blocking approach by checking if data is available
                import select

                ready, _, _ = select.select([process.stderr], [], [], 0.1)

                if ready:
                    line = process.stderr.readline()
                    if line:
                        line = line.strip()

                        if "Warming up Glass connection" in line:
                            warming_up_found = True
                            print(f"Found warming up log: {line}")

                        if "Glass connection established successfully" in line:
                            connection_established_found = True
                            print(f"Found connection established log: {line}")

                        # If both logs are found, initialization is complete
                        if warming_up_found and connection_established_found:
                            print(
                                "Glass connection initialization completed successfully"
                            )
                            return

                # Small sleep to avoid busy waiting
                time.sleep(0.1)

            except Exception:
                # Continue trying if there's an error reading stderr
                time.sleep(0.1)
                continue

        # Timeout reached
        if not warming_up_found:
            self.fail(
                "Timeout: 'Warming up Glass connection' log not found within 5 minutes"
            )
        elif not connection_established_found:
            self.fail(
                "Timeout: 'Glass connection established successfully' log not found within 5 minutes"
            )

    def send_request_and_get_response(
        self, process, request: Dict[str, Any]
    ) -> Dict[str, Any]:
        """
        Send a JSON request to the process and get the response.

        Args:
            process: The subprocess to send the request to
            request: Dictionary representing the JSON request

        Returns:
            Dictionary representing the JSON response
        """
        # Send request
        request_str = json.dumps(request)
        process.stdin.write(request_str + "\n")
        process.stdin.flush()

        # Read response with 10 second timeout
        import select

        ready, _, _ = select.select([process.stdout], [], [], 10.0)  # 10 second timeout
        if not ready:
            self.fail(
                "Timeout: No response received from swift_glass_client within 10 seconds"
            )

        response_line = process.stdout.readline().strip()
        if not response_line:
            self.fail("No response received from swift_glass_client")

        try:
            return json.loads(response_line)
        except json.JSONDecodeError as e:
            self.fail(f"Failed to parse response as JSON: {response_line}. Error: {e}")

    def send_invalid_request_and_get_response(
        self, process, invalid_request: str
    ) -> Dict[str, Any]:
        """
        Send an invalid (non-JSON) request to the process and get the response.

        Args:
            process: The subprocess to send the request to
            invalid_request: String representing the invalid request

        Returns:
            Dictionary representing the JSON response
        """
        # Send invalid request
        process.stdin.write(invalid_request + "\n")
        process.stdin.flush()

        # Read response
        response_line = process.stdout.readline().strip()
        if not response_line:
            self.fail("No response received from swift_glass_client")

        try:
            return json.loads(response_line)
        except json.JSONDecodeError as e:
            self.fail(f"Failed to parse response as JSON: {response_line}. Error: {e}")

    def test_usr_to_definition_with_mode_test_not_found(self):
        """Test USRToDefinition request with mode=test, expecting not_found response."""
        process = self._start_process()
        try:
            request = {
                "id": 1,
                "method": "USRToDefinition",
                "value": "s:someUSR",
                "mode": "test",
            }

            response = self.send_request_and_get_response(process, request)

            # Verify response structure
            self.assertIn("id", response)
            self.assertEqual(response["id"], 1)

            # Should have result field with empty array (not found)
            self.assertIn("result", response)
            self.assertEqual(response["result"], [])

            # Should not have error field for successful not_found case
            self.assertNotIn("error", response)
        finally:
            self._stop_process(process)

    def test_invalid_request_parse_error(self):
        """Test sending invalid request, expecting parse error response."""
        process = self._start_process()
        try:
            invalid_request = "invalid_request"

            response = self.send_invalid_request_and_get_response(
                process, invalid_request
            )

            # Verify response structure for parse error
            self.assertIn("id", response)
            # For parse errors, id should be an empty object
            self.assertEqual(response["id"], {})

            # Should have error field
            self.assertIn("error", response)
            error = response["error"]

            # Verify error structure
            self.assertIn("code", error)
            self.assertEqual(error["code"], -32700)  # JSON-RPC parse error code

            self.assertIn("message", error)
            # The error message should contain "Parse error" and mention the invalid input
            self.assertIn("Parse error", error["message"])
            self.assertIn("json parse error", error["message"].lower())
        finally:
            self._stop_process(process)

    def test_unknown_method_error(self):
        """Test sending request with unknown method, expecting method not found error."""
        process = self._start_process()
        try:
            request = {
                "id": 4,
                "method": "UnknownMethod",
                "value": "s:someUSR",
                "mode": "test",
            }

            response = self.send_request_and_get_response(process, request)

            # Verify response structure
            self.assertIn("id", response)
            self.assertEqual(response["id"], 4)

            # Should have error field
            self.assertIn("error", response)
            error = response["error"]

            # Verify error structure
            self.assertIn("code", error)
            self.assertEqual(
                error["code"], -32601
            )  # JSON-RPC method not found error code

            self.assertIn("message", error)
            self.assertEqual(error["message"], "Method not found")
        finally:
            self._stop_process(process)

    def test_malformed_json_request(self):
        """Test sending malformed JSON request, expecting parse error response."""
        process = self._start_process()
        try:
            malformed_json = '{"id": 1, "method": "USRToDefinition", "value": "s:someUSR"'  # Missing closing brace

            response = self.send_invalid_request_and_get_response(
                process, malformed_json
            )

            # Verify response structure for parse error
            self.assertIn("id", response)
            # For parse errors, id should be an empty object
            self.assertEqual(response["id"], {})

            # Should have error field
            self.assertIn("error", response)
            error = response["error"]

            # Verify error structure
            self.assertIn("code", error)
            self.assertEqual(error["code"], -32700)  # JSON-RPC parse error code

            self.assertIn("message", error)
            # The error message should contain "Parse error"
            self.assertIn("Parse error", error["message"])
        finally:
            self._stop_process(process)

    def _test_usr_to_definition_swift_class(
        self, process, with_revision: bool, test_id: int
    ):
        """Helper method to test USRToDefinition request for Swift class."""
        # USR value for Swift class
        # https://www.internalfb.com/code/fbsource/fbobjc/Extra/GleanTests/Swift/CIBCanvas/CIBCanvasView.swift
        swift_usr = "s:9CIBCanvas0A4ViewC5frameACSo6CGRectV_tcfcADL_AFvp"

        request = {
            "id": test_id,
            "method": "USRToDefinition",
            "value": swift_usr,
            "mode": "test",
        }

        if with_revision:
            request["revision"] = self.revision

        response = self.send_request_and_get_response(process, request)

        # Verify response structure
        self.assertIn("id", response)
        self.assertEqual(response["id"], test_id)

        # Should have result field
        self.assertIn("result", response)
        result = response["result"]

        # Should not have error field for successful case
        self.assertNotIn("error", response)

        # If definitions are found, verify they point to Swift files
        if result:
            for location in result:
                self.assertIn("uri", location)
                uri = location["uri"]
                # Should point to a Swift file
                self.assertTrue(
                    uri.endswith(".swift"), f"Expected .swift file, got: {uri}"
                )

    def test_usr_to_definition_swift_class_with_revision(self):
        """Test USRToDefinition request for Swift class with revision field."""
        process = self._start_process()
        try:
            self._test_usr_to_definition_swift_class(
                process, with_revision=True, test_id=1
            )
        finally:
            if process:
                process.terminate()
                process.wait()

    def test_usr_to_definition_swift_class_without_revision(self):
        """Test USRToDefinition request for Swift class without revision field."""
        process = self._start_process()
        try:
            self._test_usr_to_definition_swift_class(
                process, with_revision=False, test_id=2
            )
        finally:
            if process:
                process.terminate()
                process.wait()


if __name__ == "__main__":
    # Run the tests
    unittest.main(verbosity=2)
