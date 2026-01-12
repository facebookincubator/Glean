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
        # Set environment variable to ensure test mode is used for all requests,
        # including parse error responses
        env = os.environ.copy()
        env["SWIFT_GLASS_CLIENT_MODE"] = "test"

        # Start the swift_glass_client binary directly using the pre-built path
        process = subprocess.Popen(
            [self._binary_path, "--test-run"],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            bufsize=0,  # Unbuffered for immediate communication
            env=env,  # Pass the environment with test mode
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

    def test_parse_error_with_escaped_symbols(self):
        """Test that parse error messages properly escape backslashes and single quotes."""
        process = self._start_process()
        try:
            # Test cases for different escaping scenarios
            test_cases = [
                {
                    "name": "single_quotes",
                    "input": "{'invalid': 'json' with single quotes}",
                    "expected_escaped": "{\\'invalid\\': \\'json\\' with single quotes}",
                },
                {
                    "name": "backslashes",
                    "input": '{"path": "C:\\temp\\file"}invalid',
                    "expected_escaped": '{"path": "C:\\\\temp\\\\file"}invalid',
                },
                {
                    "name": "backslash_and_quotes",
                    "input": "{'path': 'C:\\temp\\'file'}",
                    "expected_escaped": "{\\'path\\': \\'C:\\\\temp\\\\\\'file\\'}",
                },
                {
                    "name": "long_input_truncation",
                    "input": "'" + "x" * 600 + "'",  # Over 500 chars to test truncation
                    "expected_contains": "...[truncated]",
                },
            ]

            for test_case in test_cases:
                with self.subTest(test_case=test_case["name"]):
                    response = self.send_invalid_request_and_get_response(
                        process, test_case["input"]
                    )

                    # Verify response structure for parse error
                    self.assertIn("id", response)
                    self.assertEqual(response["id"], {})

                    # Should have error field
                    self.assertIn("error", response)
                    error = response["error"]

                    # Verify error structure
                    self.assertIn("code", error)
                    self.assertEqual(error["code"], -32700)  # JSON-RPC parse error code

                    self.assertIn("message", error)
                    error_message = error["message"]

                    # The error message should contain "Parse error" and mention the input
                    self.assertIn("Parse error", error_message)
                    self.assertIn("on input:", error_message)

                    if "expected_escaped" in test_case:
                        # Verify the input is properly escaped in the error message
                        self.assertIn(test_case["expected_escaped"], error_message)

                    if "expected_contains" in test_case:
                        # Verify truncation for long inputs
                        self.assertIn(test_case["expected_contains"], error_message)
                        # Verify message length is reasonable (not the full 600+ chars)
                        self.assertLess(len(error_message), 800)

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

    def test_not_implemented_method_error(self):
        """Test sending request with valid structure but unimplemented method."""
        process = self._start_process()
        try:
            # Test a reasonable API extension that's not implemented yet
            request = {
                "id": 5,
                "method": "USRToReferences",  # Not implemented yet
                "value": "s:9CIBCanvas0A4ViewC",
                "mode": "test",
                "revision": self.revision,
            }

            response = self.send_request_and_get_response(process, request)

            # Verify response structure
            self.assertIn("id", response)
            self.assertEqual(response["id"], 5)

            # Should have error field for not implemented method
            self.assertIn("error", response)
            error = response["error"]

            # Verify error structure
            self.assertIn("code", error)
            self.assertEqual(
                error["code"], -32601
            )  # JSON-RPC method not found error code (represents "not implemented")

            self.assertIn("message", error)
            self.assertEqual(error["message"], "Method not found")

            # Should not have result field for error case
            self.assertNotIn("result", response)
        finally:
            self._stop_process(process)

    def test_request_with_extra_fields(self):
        """Test sending request with extra/unexpected fields that should be ignored."""
        process = self._start_process()
        try:
            # Test a valid request structure with unimplemented method and extra fields
            request = {
                "id": 6,
                "method": "USRToReferences",  # Not implemented yet
                "value": "s:9CIBCanvas0A4ViewC",
                "mode": "test",
                "unexpected": "foo",  # Extra field that should be ignored
                "another_extra": 123,  # Another extra field
                "revision": self.revision,
                "extra_array": [1, 2, 3],  # Additional extra field
                "nested_object": {"key": "value"},  # Nested extra field
            }

            response = self.send_request_and_get_response(process, request)

            # Verify response structure - should handle extra fields gracefully
            self.assertIn("id", response)
            self.assertEqual(response["id"], 6)

            # Should have error field for unimplemented method
            self.assertIn("error", response)
            error = response["error"]

            # Verify error structure - extra fields should not affect error handling
            self.assertIn("code", error)
            self.assertEqual(
                error["code"], -32601
            )  # JSON-RPC method not found error code

            self.assertIn("message", error)
            self.assertEqual(error["message"], "Method not found")

            # Should not have result field for error case
            self.assertNotIn("result", response)
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
            # Verify the enhanced error message includes the original input for better debugging
            self.assertIn("on input:", error["message"])
            self.assertIn(malformed_json, error["message"])
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

    def test_usr_to_definition_swift_class_with_delay_async(self):
        """Test USRToDefinition request with delay to verify async mode functionality."""
        process = self._start_process()
        try:
            # USR value for Swift class (same as in _test_usr_to_definition_swift_class)
            swift_usr = "s:9CIBCanvas0A4ViewC5frameACSo6CGRectV_tcfcADL_AFvp"

            # Send 3 requests with different delays to test async processing
            # Request 1: 2000ms delay (should arrive last)
            request1 = {
                "id": 1,
                "method": "USRToDefinition",
                "value": swift_usr,
                "mode": "test",
                "delay": 2000,
                "revision": self.revision,
            }

            # Request 2: 1000ms delay (should arrive second)
            request2 = {
                "id": 2,
                "method": "USRToDefinition",
                "value": swift_usr,
                "mode": "test",
                "delay": 1000,
                "revision": self.revision,
            }

            # Request 3: no delay (should arrive first)
            request3 = {
                "id": 3,
                "method": "USRToDefinition",
                "value": swift_usr,
                "mode": "test",
                "revision": self.revision,
            }

            # Record start time
            start_time = time.time()

            # Send all requests quickly in sequence
            request1_str = json.dumps(request1)
            process.stdin.write(request1_str + "\n")
            process.stdin.flush()

            request2_str = json.dumps(request2)
            process.stdin.write(request2_str + "\n")
            process.stdin.flush()

            request3_str = json.dumps(request3)
            process.stdin.write(request3_str + "\n")
            process.stdin.flush()

            # Collect responses in order they arrive
            responses = []
            response_times = []

            for i in range(3):
                # Read response with timeout
                import select

                ready, _, _ = select.select(
                    [process.stdout], [], [], 15.0
                )  # 15 second timeout
                if not ready:
                    self.fail(
                        f"Timeout: No response received for request {i + 1} within 15 seconds"
                    )

                response_line = process.stdout.readline().strip()
                if not response_line:
                    self.fail(f"No response received for request {i + 1}")

                try:
                    response = json.loads(response_line)
                    responses.append(response)
                    response_times.append(time.time() - start_time)
                except json.JSONDecodeError as e:
                    self.fail(
                        f"Failed to parse response {i + 1} as JSON: {response_line}. Error: {e}"
                    )

            # Verify we got exactly 3 responses
            self.assertEqual(len(responses), 3, "Expected exactly 3 responses")

            # Extract response IDs in order received
            response_ids = [resp["id"] for resp in responses]

            # Verify responses arrived in reverse order: id 3 (no delay), id 2 (1000ms), id 1 (2000ms)
            expected_order = [3, 2, 1]
            self.assertEqual(
                response_ids,
                expected_order,
                f"Expected responses in order {expected_order}, but got {response_ids}",
            )

            # Verify timing constraints
            # Response 1 (id=3, no delay) should arrive first (around 0ms)
            # Response 2 (id=2, 1000ms delay) should arrive around 1000ms
            # Response 3 (id=1, 2000ms delay) should arrive around 2000ms

            tolerance_ms = 900  # 900ms tolerance for processing overhead

            # First response (id=3) should arrive quickly
            self.assertLess(
                response_times[0] * 1000,
                tolerance_ms,
                f"First response (id=3) took {response_times[0] * 1000}ms, expected < {tolerance_ms}ms",
            )

            # Second response (id=2) should arrive around 1000ms
            expected_time_2 = 1000
            actual_time_2 = response_times[1] * 1000
            self.assertGreaterEqual(
                actual_time_2,
                expected_time_2 - tolerance_ms,
                f"Second response (id=2) arrived at {actual_time_2}ms, expected >= {expected_time_2 - tolerance_ms}ms",
            )
            self.assertLessEqual(
                actual_time_2,
                expected_time_2 + tolerance_ms,
                f"Second response (id=2) arrived at {actual_time_2}ms, expected <= {expected_time_2 + tolerance_ms}ms",
            )

            # Third response (id=1) should arrive around 2000ms
            expected_time_1 = 2000
            actual_time_1 = response_times[2] * 1000
            self.assertGreaterEqual(
                actual_time_1,
                expected_time_1 - tolerance_ms,
                f"Third response (id=1) arrived at {actual_time_1}ms, expected >= {expected_time_1 - tolerance_ms}ms",
            )
            self.assertLessEqual(
                actual_time_1,
                expected_time_1 + tolerance_ms,
                f"Third response (id=1) arrived at {actual_time_1}ms, expected <= {expected_time_1 + tolerance_ms}ms",
            )

            # Verify all responses have correct structure
            for i, response in enumerate(responses):
                self.assertIn("id", response, f"Response {i + 1} missing id field")
                self.assertIn(
                    "result", response, f"Response {i + 1} missing result field"
                )
                self.assertNotIn(
                    "error", response, f"Response {i + 1} has unexpected error field"
                )

                # If definitions are found, verify they point to Swift files
                result = response["result"]
                if result:
                    for location in result:
                        self.assertIn("uri", location)
                        uri = location["uri"]
                        # Should point to a Swift file
                        self.assertTrue(
                            uri.endswith(".swift"), f"Expected .swift file, got: {uri}"
                        )

            print(
                f"✓ Async test passed: responses received in order {response_ids} at times {[f'{t * 1000:.0f}ms' for t in response_times]}"
            )

        finally:
            if process:
                process.terminate()
                process.wait()


if __name__ == "__main__":
    # Run the tests
    unittest.main(verbosity=2)
