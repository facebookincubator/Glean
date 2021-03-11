#pragma once

#include <stdint.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct glean_json_document_t glean_json_document_t;
typedef struct glean_json_value_t glean_json_value_t;

const char *glean_json_parse(
  const char *text,
  size_t size,
  glean_json_document_t **document);
void glean_json_document_free(glean_json_document_t *);

glean_json_value_t *glean_json_document_root(glean_json_document_t *);

int glean_json_value_type(glean_json_value_t *);

int64_t glean_json_value_get_int(glean_json_value_t *value);
int glean_json_value_get_bool(glean_json_value_t *value);
void glean_json_value_get_string(
  glean_json_value_t *value,
  const char **text,
  size_t *size);
size_t glean_json_value_get_size(
  glean_json_value_t *value);
glean_json_value_t *glean_json_value_get_array_element(
  glean_json_value_t *value,
  size_t index);
glean_json_value_t *glean_json_value_get_object_field(
  glean_json_value_t *value,
  const char *key_name,
  size_t key_size);
void glean_json_value_index_object_field(
  glean_json_value_t *value,
  char **key_name,
  size_t *key_size,
  glean_json_value_t **field);


const char *glean_json_encode(
  glean_json_value_t *value,
  char **out,
  size_t *size);
const char *glean_json_encode_string(
  const char *text,
  size_t text_size,
  char **out,
  size_t *out_size);

size_t glean_json_encode_number(
  int64_t number,
  char *out);

size_t glean_json_string_escaped_size(
  const uint8_t *text,
  size_t text_size);
void glean_json_string_escape(
    const uint8_t *text,
    size_t text_size,
    char *out,
    size_t escaped_size);
size_t glean_json_mangled_string_escaped_size(
  const uint8_t *text,
  size_t text_size);
void glean_json_mangled_string_escape(
    const uint8_t *text,
    size_t text_size,
    char *out,
    size_t escaped_size);

#ifdef __cplusplus
}
#endif
