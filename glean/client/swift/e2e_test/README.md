# Swift Glass Client E2E Tests

This directory contains end-to-end tests for the `swift_glass_client` binary.

## Overview

The test suite starts the `swift_glass_client` binary and communicates with it via stdin/stdout to test various request scenarios including:

1. **Valid USRToDefinition requests** with different mode values (test/production)
2. **Invalid JSON requests** that should trigger parse errors
3. **Unknown method requests** that should return method not found errors
4. **Malformed JSON requests** that should be handled gracefully

## Test Cases

### 1. USRToDefinition with mode="test"
- Sends a valid request with `mode: "test"`
- Expects a successful response with empty result array (not found)

### 2. Invalid request
- Sends `"invalid_request"` (non-JSON string)
- Expects parse error response with code -32700

### 3. Unknown method
- Sends request with unknown method name
- Expects method not found error with code -32601

### 4. Malformed JSON
- Sends malformed JSON (missing closing brace)
- Expects parse error response with code -32700

### 5. USRToDefinition for Swift class (with revision)
- Tests Swift class USR: `"s:9CIBCanvas0A4ViewC5frameACSo6CGRectV_tcfcADL_AFvp"`
- Includes revision field from `hg id` command
- Validates that any returned locations point to `.swift` files
- USR value is configurable via variable for easy maintenance

### 6. USRToDefinition for Swift class (without revision)
- Tests the same Swift class USR without the revision field
- Validates backward compatibility when revision is not provided
- Ensures the same behavior as the revision variant

## Running the Tests

### Using Buck (Recommended)
```bash
buck test fbcode//glean/client/swift/e2e_test:swift_glass_client_e2e_test
```

### Using Python directly
```bash
cd /data/users/ivanmurashko/fbsource/fbcode/glean/client/swift/e2e_test
python3 test_swift_glass_client.py
```

## Test Implementation Details

- The test uses `subprocess.Popen` to start the `swift_glass_client` binary
- Communication happens via stdin/stdout with line-buffered JSON messages
- Each test case sends a request and validates the response structure and content
- The process is properly cleaned up after each test

## Expected Responses

### Successful USRToDefinition (not found)
```json
{
  "id": 1,
  "result": []
}
```

### Parse Error
```json
{
  "id": {},
  "error": {
    "code": -32700,
    "message": "Parse error: json parse error on line 0 near `invalid_request': expected json value"
  }
}
```

### Method Not Found Error
```json
{
  "id": 4,
  "error": {
    "code": -32601,
    "message": "Method not found"
  }
}
```
