<?hh
/**
 * Autogenerated by Thrift
 *
 * DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING
 *  @generated
 *  @codegen-source: test.thrift
 */

class test_CONSTANTS implements \IThriftConstants {
  /**
   * Original thrift constant:-
   * string ENGINE_STRMATCH
   */
  const string ENGINE_STRMATCH = "apr_strmatch";

  /**
   * Original thrift constant:-
   * string ENGINE_RE2
   */
  const string ENGINE_RE2 = "re2";

  /**
   * Original thrift constant:-
   * string ENGINE_FILENAME
   */
  const string ENGINE_FILENAME = "filename";

  /**
   * Original thrift constant:-
   * string ENGINE_FUZZY
   */
  const string ENGINE_FUZZY = "fuzzy";


  public static function getAllStructuredAnnotations()[write_props]: dict<string, dict<string, \IThriftStruct>> {
    return dict[
    ];
  }
}
