<?hh
/**
 * Test case generated manually using
 * buck2 run //thrift/compiler:thrift -- -out ~/fbsource/fbcode/glean/lang/codemarkup/tests/hack/glass/hack-xlang --gen hack ~/fbsource/fbcode/glean/lang/codemarkup/tests/hack/glass/hack-xlang/test.thrift
 *
 *  @generated
 *  @codegen-source: test.thrift
 */

/**
 * Original thrift enum:-
 * CountOnlyMode
 */
enum CountOnlyMode: int {
  Disabled = 0;
  HitCount = 1;
  FileCount = 2;
}

class CountOnlyMode_TEnumStaticMetadata implements \IThriftEnumStaticMetadata {
  public static function getEnumMetadata()[]: \tmeta_ThriftEnum {
    return tmeta_ThriftEnum::fromShape(
      shape(
        "name" => "test.CountOnlyMode",
        "elements" => dict[
          0 => "Disabled",
          1 => "HitCount",
          2 => "FileCount",
        ],
      )
    );
  }

  public static function getAllStructuredAnnotations()[write_props]: \TEnumAnnotations {
    return shape(
      'enum' => dict[],
      'constants' => dict[
      ],
    );
  }
}

enum ResponseCardEnum: int {
  _EMPTY_ = 0;
  static_card = 1;
}

/**
 * Original thrift union:-
 * ResponseCard
 */
class ResponseCard implements \IThriftSyncStruct, \IThriftStructMetadata, \IThriftUnion<ResponseCardEnum> {
  use \ThriftUnionSerializationTrait;

  const \ThriftStructTypes::TSpec SPEC = dict[
    1 => shape(
      'var' => 'static_card',
      'union' => true,
      'type' => \TType::STRING,
    ),
  ];
  const dict<string, int> FIELDMAP = dict[
    'static_card' => 1,
  ];

  const type TConstructorShape = shape(
    ?'static_card' => ?string,
  );

  const int STRUCTURAL_ID = 5012578871550363343;
  /**
   * Original thrift field:-
   * 1: string static_card
   */
  public ?string $static_card;
  protected ResponseCardEnum $_type = ResponseCardEnum::_EMPTY_;

  public function __construct(?string $static_card = null)[] {
    $this->_type = ResponseCardEnum::_EMPTY_;
    if ($static_card !== null) {
      $this->static_card = $static_card;
      $this->_type = ResponseCardEnum::static_card;
    }
  }

  public static function withDefaultValues()[]: this {
    return new static();
  }

  public static function fromShape(self::TConstructorShape $shape)[]: this {
    return new static(
      Shapes::idx($shape, 'static_card'),
    );
  }

  public function getName()[]: string {
    return 'ResponseCard';
  }

  public function getType()[]: ResponseCardEnum {
    return $this->_type;
  }

  public function reset()[write_props]: void {
    switch ($this->_type) {
      case ResponseCardEnum::static_card:
        $this->static_card = null;
        break;
      case ResponseCardEnum::_EMPTY_:
        break;
    }
    $this->_type = ResponseCardEnum::_EMPTY_;
  }

  public function set_static_card(string $static_card)[write_props]: this {
    $this->reset();
    $this->_type = ResponseCardEnum::static_card;
    $this->static_card = $static_card;
    return $this;
  }

  public function get_static_card()[]: ?string {
    return $this->static_card;
  }

  public function getx_static_card()[]: string {
    invariant(
      $this->_type === ResponseCardEnum::static_card,
      'get_static_card called on an instance of ResponseCard whose current type is %s',
      (string)$this->_type,
    );
    return $this->static_card as nonnull;
  }

  public static function getStructMetadata()[]: \tmeta_ThriftStruct {
    return tmeta_ThriftStruct::fromShape(
      shape(
        "name" => "test.ResponseCard",
        "fields" => vec[
          tmeta_ThriftField::fromShape(
            shape(
              "id" => 1,
              "type" => tmeta_ThriftType::fromShape(
                shape(
                  "t_primitive" => tmeta_ThriftPrimitiveType::THRIFT_STRING_TYPE,
                )
              ),
              "name" => "static_card",
            )
          ),
        ],
        "is_union" => true,
      )
    );
  }

  public static function getAllStructuredAnnotations()[write_props]: \TStructAnnotations {
    return shape(
      'struct' => dict[],
      'fields' => dict[
      ],
    );
  }

  public function getInstanceKey()[write_props]: string {
    return \TCompactSerializer::serialize($this);
  }

}

/**
 * Original thrift struct:-
 * GetNavigationRequest
 */
class GetNavigationRequest implements \IThriftSyncStruct, \IThriftStructMetadata {
  use \ThriftSerializationTrait;

  const \ThriftStructTypes::TSpec SPEC = dict[
    1 => shape(
      'var' => 'identifier',
      'type' => \TType::STRING,
    ),
  ];
  const dict<string, int> FIELDMAP = dict[
    'identifier' => 1,
  ];

  const type TConstructorShape = shape(
    ?'identifier' => ?string,
  );

  const int STRUCTURAL_ID = 4737529851577780631;
  /**
   * Original thrift field:-
   * 1: string identifier
   */
  public string $identifier;

  public function __construct(?string $identifier = null)[] {
    $this->identifier = $identifier ?? '';
  }

  public static function withDefaultValues()[]: this {
    return new static();
  }

  public static function fromShape(self::TConstructorShape $shape)[]: this {
    return new static(
      Shapes::idx($shape, 'identifier'),
    );
  }

  public function getName()[]: string {
    return 'GetNavigationRequest';
  }

  public static function getStructMetadata()[]: \tmeta_ThriftStruct {
    return tmeta_ThriftStruct::fromShape(
      shape(
        "name" => "test.GetNavigationRequest",
        "fields" => vec[
          tmeta_ThriftField::fromShape(
            shape(
              "id" => 1,
              "type" => tmeta_ThriftType::fromShape(
                shape(
                  "t_primitive" => tmeta_ThriftPrimitiveType::THRIFT_STRING_TYPE,
                )
              ),
              "name" => "identifier",
            )
          ),
        ],
        "is_union" => false,
      )
    );
  }

  public static function getAllStructuredAnnotations()[write_props]: \TStructAnnotations {
    return shape(
      'struct' => dict[],
      'fields' => dict[
      ],
    );
  }

  public function getInstanceKey()[write_props]: string {
    return \TCompactSerializer::serialize($this);
  }

}

/**
 * Original thrift struct:-
 * GetNavigationResponse
 */
class GetNavigationResponse implements \IThriftSyncStruct, \IThriftStructMetadata {
  use \ThriftSerializationTrait;

  const \ThriftStructTypes::TSpec SPEC = dict[
    1 => shape(
      'var' => 'tab_group',
      'type' => \TType::STRING,
    ),
  ];
  const dict<string, int> FIELDMAP = dict[
    'tab_group' => 1,
  ];

  const type TConstructorShape = shape(
    ?'tab_group' => ?string,
  );

  const int STRUCTURAL_ID = 4388748585342348029;
  /**
   * Original thrift field:-
   * 1: string tab_group
   */
  public string $tab_group;

  public function __construct(?string $tab_group = null)[] {
    $this->tab_group = $tab_group ?? '';
  }

  public static function withDefaultValues()[]: this {
    return new static();
  }

  public static function fromShape(self::TConstructorShape $shape)[]: this {
    return new static(
      Shapes::idx($shape, 'tab_group'),
    );
  }

  public function getName()[]: string {
    return 'GetNavigationResponse';
  }

  public static function getStructMetadata()[]: \tmeta_ThriftStruct {
    return tmeta_ThriftStruct::fromShape(
      shape(
        "name" => "test.GetNavigationResponse",
        "fields" => vec[
          tmeta_ThriftField::fromShape(
            shape(
              "id" => 1,
              "type" => tmeta_ThriftType::fromShape(
                shape(
                  "t_primitive" => tmeta_ThriftPrimitiveType::THRIFT_STRING_TYPE,
                )
              ),
              "name" => "tab_group",
            )
          ),
        ],
        "is_union" => false,
      )
    );
  }

  public static function getAllStructuredAnnotations()[write_props]: \TStructAnnotations {
    return shape(
      'struct' => dict[],
      'fields' => dict[
      ],
    );
  }

  public function getInstanceKey()[write_props]: string {
    return \TCompactSerializer::serialize($this);
  }

}

/**
 * Original thrift exception:-
 * GetNavigationException
 */
class GetNavigationException extends \TException implements \IThriftSyncStruct, \IThriftExceptionMetadata {
  use \ThriftSerializationTrait;

  const \ThriftStructTypes::TSpec SPEC = dict[
    1 => shape(
      'var' => 'message_detail',
      'type' => \TType::STRING,
    ),
  ];
  const dict<string, int> FIELDMAP = dict[
    'message_detail' => 1,
  ];

  const type TConstructorShape = shape(
    ?'message_detail' => ?string,
  );

  const int STRUCTURAL_ID = 8543554701141001810;
  /**
   * Original thrift field:-
   * 1: string message_detail
   */
  public string $message_detail;

  public function __construct(?string $message_detail = null)[] {
    parent::__construct();
    $this->message_detail = $message_detail ?? '';
  }

  public static function withDefaultValues()[]: this {
    return new static();
  }

  public static function fromShape(self::TConstructorShape $shape)[]: this {
    return new static(
      Shapes::idx($shape, 'message_detail'),
    );
  }

  public function getName()[]: string {
    return 'GetNavigationException';
  }

  public static function getExceptionMetadata()[]: \tmeta_ThriftException {
    return tmeta_ThriftException::fromShape(
      shape(
        "name" => "test.GetNavigationException",
        "fields" => vec[
          tmeta_ThriftField::fromShape(
            shape(
              "id" => 1,
              "type" => tmeta_ThriftType::fromShape(
                shape(
                  "t_primitive" => tmeta_ThriftPrimitiveType::THRIFT_STRING_TYPE,
                )
              ),
              "name" => "message_detail",
            )
          ),
        ],
      )
    );
  }

  public static function getAllStructuredAnnotations()[write_props]: \TStructAnnotations {
    return shape(
      'struct' => dict[],
      'fields' => dict[
      ],
    );
  }

  public function getInstanceKey()[write_props]: string {
    return \TCompactSerializer::serialize($this);
  }

}

/**
 * Original thrift struct:-
 * GetCardRequest
 */
class GetCardRequest implements \IThriftSyncStruct, \IThriftStructMetadata {
  use \ThriftSerializationTrait;

  const \ThriftStructTypes::TSpec SPEC = dict[
    1 => shape(
      'var' => 'identifier',
      'type' => \TType::STRING,
    ),
    2 => shape(
      'var' => 'card_name',
      'type' => \TType::STRING,
    ),
  ];
  const dict<string, int> FIELDMAP = dict[
    'identifier' => 1,
    'card_name' => 2,
  ];

  const type TConstructorShape = shape(
    ?'identifier' => ?string,
    ?'card_name' => ?string,
  );

  const int STRUCTURAL_ID = 3933215187375616539;
  /**
   * Original thrift field:-
   * 1: string identifier
   */
  public string $identifier;
  /**
   * Original thrift field:-
   * 2: string card_name
   */
  public string $card_name;

  public function __construct(?string $identifier = null, ?string $card_name = null)[] {
    $this->identifier = $identifier ?? '';
    $this->card_name = $card_name ?? '';
  }

  public static function withDefaultValues()[]: this {
    return new static();
  }

  public static function fromShape(self::TConstructorShape $shape)[]: this {
    return new static(
      Shapes::idx($shape, 'identifier'),
      Shapes::idx($shape, 'card_name'),
    );
  }

  public function getName()[]: string {
    return 'GetCardRequest';
  }

  public static function getStructMetadata()[]: \tmeta_ThriftStruct {
    return tmeta_ThriftStruct::fromShape(
      shape(
        "name" => "test.GetCardRequest",
        "fields" => vec[
          tmeta_ThriftField::fromShape(
            shape(
              "id" => 1,
              "type" => tmeta_ThriftType::fromShape(
                shape(
                  "t_primitive" => tmeta_ThriftPrimitiveType::THRIFT_STRING_TYPE,
                )
              ),
              "name" => "identifier",
            )
          ),
          tmeta_ThriftField::fromShape(
            shape(
              "id" => 2,
              "type" => tmeta_ThriftType::fromShape(
                shape(
                  "t_primitive" => tmeta_ThriftPrimitiveType::THRIFT_STRING_TYPE,
                )
              ),
              "name" => "card_name",
            )
          ),
        ],
        "is_union" => false,
      )
    );
  }

  public static function getAllStructuredAnnotations()[write_props]: \TStructAnnotations {
    return shape(
      'struct' => dict[],
      'fields' => dict[
      ],
    );
  }

  public function getInstanceKey()[write_props]: string {
    return \TCompactSerializer::serialize($this);
  }

}

/**
 * Original thrift struct:-
 * GetCardResponse
 */
class GetCardResponse implements \IThriftSyncStruct, \IThriftStructMetadata {
  use \ThriftSerializationTrait;

  const \ThriftStructTypes::TSpec SPEC = dict[
    1 => shape(
      'var' => 'card',
      'type' => \TType::STRUCT,
      'class' => ResponseCard::class,
    ),
  ];
  const dict<string, int> FIELDMAP = dict[
    'card' => 1,
  ];

  const type TConstructorShape = shape(
    ?'card' => ?ResponseCard,
  );

  const int STRUCTURAL_ID = 5322548212259685563;
  /**
   * Original thrift field:-
   * 1: test.ResponseCard card
   */
  public ?ResponseCard $card;

  public function __construct(?ResponseCard $card = null)[] {
    $this->card = $card;
  }

  public static function withDefaultValues()[]: this {
    return new static();
  }

  public static function fromShape(self::TConstructorShape $shape)[]: this {
    return new static(
      Shapes::idx($shape, 'card'),
    );
  }

  public function getName()[]: string {
    return 'GetCardResponse';
  }

  public static function getStructMetadata()[]: \tmeta_ThriftStruct {
    return tmeta_ThriftStruct::fromShape(
      shape(
        "name" => "test.GetCardResponse",
        "fields" => vec[
          tmeta_ThriftField::fromShape(
            shape(
              "id" => 1,
              "type" => tmeta_ThriftType::fromShape(
                shape(
                  "t_struct" => tmeta_ThriftStructType::fromShape(
                    shape(
                      "name" => "test.ResponseCard",
                    )
                  ),
                )
              ),
              "name" => "card",
            )
          ),
        ],
        "is_union" => false,
      )
    );
  }

  public static function getAllStructuredAnnotations()[write_props]: \TStructAnnotations {
    return shape(
      'struct' => dict[],
      'fields' => dict[
      ],
    );
  }

  public function getInstanceKey()[write_props]: string {
    return \TCompactSerializer::serialize($this);
  }

}

/**
 * Original thrift exception:-
 * GetCardException
 */
class GetCardException extends \TException implements \IThriftSyncStruct, \IThriftExceptionMetadata {
  use \ThriftSerializationTrait;

  const \ThriftStructTypes::TSpec SPEC = dict[
    1 => shape(
      'var' => 'message_detail',
      'type' => \TType::STRING,
    ),
  ];
  const dict<string, int> FIELDMAP = dict[
    'message_detail' => 1,
  ];

  const type TConstructorShape = shape(
    ?'message_detail' => ?string,
  );

  const int STRUCTURAL_ID = 8543554701141001810;
  /**
   * Original thrift field:-
   * 1: string message_detail
   */
  public string $message_detail;

  public function __construct(?string $message_detail = null)[] {
    parent::__construct();
    $this->message_detail = $message_detail ?? '';
  }

  public static function withDefaultValues()[]: this {
    return new static();
  }

  public static function fromShape(self::TConstructorShape $shape)[]: this {
    return new static(
      Shapes::idx($shape, 'message_detail'),
    );
  }

  public function getName()[]: string {
    return 'GetCardException';
  }

  public static function getExceptionMetadata()[]: \tmeta_ThriftException {
    return tmeta_ThriftException::fromShape(
      shape(
        "name" => "test.GetCardException",
        "fields" => vec[
          tmeta_ThriftField::fromShape(
            shape(
              "id" => 1,
              "type" => tmeta_ThriftType::fromShape(
                shape(
                  "t_primitive" => tmeta_ThriftPrimitiveType::THRIFT_STRING_TYPE,
                )
              ),
              "name" => "message_detail",
            )
          ),
        ],
      )
    );
  }

  public static function getAllStructuredAnnotations()[write_props]: \TStructAnnotations {
    return shape(
      'struct' => dict[],
      'fields' => dict[
      ],
    );
  }

  public function getInstanceKey()[write_props]: string {
    return \TCompactSerializer::serialize($this);
  }

}

/**
 * Original thrift struct:-
 * ReservationHotfixConfig
 */
class ReservationHotfixConfig implements \IThriftSyncStruct, \IThriftStructMetadata {
  use \ThriftSerializationTrait;

  const \ThriftStructTypes::TSpec SPEC = dict[
    -1 => shape(
      'var' => 'capabilities_to_remove',
      'type' => \TType::MAP,
      'ktype' => \TType::STRING,
      'vtype' => \TType::SET,
      'key' => shape(
        'type' => \TType::STRING,
      ),
      'val' => shape(
        'type' => \TType::SET,
        'etype' => \TType::STRING,
        'elem' => shape(
          'type' => \TType::STRING,
        ),
        'format' => 'collection',
      ),
      'format' => 'collection',
    ),
    -2 => shape(
      'var' => 'preferences_to_add',
      'type' => \TType::MAP,
      'ktype' => \TType::STRING,
      'vtype' => \TType::MAP,
      'key' => shape(
        'type' => \TType::STRING,
      ),
      'val' => shape(
        'type' => \TType::MAP,
        'ktype' => \TType::STRING,
        'vtype' => \TType::I64,
        'key' => shape(
          'type' => \TType::STRING,
        ),
        'val' => shape(
          'type' => \TType::I64,
        ),
        'format' => 'collection',
      ),
      'format' => 'collection',
    ),
  ];
  const dict<string, int> FIELDMAP = dict[
    'capabilities_to_remove' => -1,
    'preferences_to_add' => -2,
  ];

  const type TConstructorShape = shape(
    ?'capabilities_to_remove' => ?Map<string, Set<string>>,
    ?'preferences_to_add' => ?Map<string, Map<string, int>>,
  );

  const int STRUCTURAL_ID = 4417832200969255328;
  /**
   * Original thrift field:-
   * -1: map<string, set<string>> capabilities_to_remove
   */
  public Map<string, Set<string>> $capabilities_to_remove;
  /**
   * Original thrift field:-
   * -2: map<string, map<string, i64>> preferences_to_add
   */
  public Map<string, Map<string, int>> $preferences_to_add;

  public function __construct(?Map<string, Set<string>> $capabilities_to_remove = null, ?Map<string, Map<string, int>> $preferences_to_add = null)[] {
    $this->capabilities_to_remove = $capabilities_to_remove ?? Map {
    };
    $this->preferences_to_add = $preferences_to_add ?? Map {
    };
  }

  public static function withDefaultValues()[]: this {
    return new static();
  }

  public static function fromShape(self::TConstructorShape $shape)[]: this {
    return new static(
      Shapes::idx($shape, 'capabilities_to_remove'),
      Shapes::idx($shape, 'preferences_to_add'),
    );
  }

  public function getName()[]: string {
    return 'ReservationHotfixConfig';
  }

  public static function getStructMetadata()[]: \tmeta_ThriftStruct {
    return tmeta_ThriftStruct::fromShape(
      shape(
        "name" => "test.ReservationHotfixConfig",
        "fields" => vec[
          tmeta_ThriftField::fromShape(
            shape(
              "id" => -1,
              "type" => tmeta_ThriftType::fromShape(
                shape(
                  "t_map" => tmeta_ThriftMapType::fromShape(
                    shape(
                      "keyType" => tmeta_ThriftType::fromShape(
                        shape(
                          "t_primitive" => tmeta_ThriftPrimitiveType::THRIFT_STRING_TYPE,
                        )
                      ),
                      "valueType" => tmeta_ThriftType::fromShape(
                        shape(
                          "t_set" => tmeta_ThriftSetType::fromShape(
                            shape(
                              "valueType" => tmeta_ThriftType::fromShape(
                                shape(
                                  "t_primitive" => tmeta_ThriftPrimitiveType::THRIFT_STRING_TYPE,
                                )
                              ),
                            )
                          ),
                        )
                      ),
                    )
                  ),
                )
              ),
              "name" => "capabilities_to_remove",
            )
          ),
          tmeta_ThriftField::fromShape(
            shape(
              "id" => -2,
              "type" => tmeta_ThriftType::fromShape(
                shape(
                  "t_map" => tmeta_ThriftMapType::fromShape(
                    shape(
                      "keyType" => tmeta_ThriftType::fromShape(
                        shape(
                          "t_primitive" => tmeta_ThriftPrimitiveType::THRIFT_STRING_TYPE,
                        )
                      ),
                      "valueType" => tmeta_ThriftType::fromShape(
                        shape(
                          "t_map" => tmeta_ThriftMapType::fromShape(
                            shape(
                              "keyType" => tmeta_ThriftType::fromShape(
                                shape(
                                  "t_primitive" => tmeta_ThriftPrimitiveType::THRIFT_STRING_TYPE,
                                )
                              ),
                              "valueType" => tmeta_ThriftType::fromShape(
                                shape(
                                  "t_primitive" => tmeta_ThriftPrimitiveType::THRIFT_I64_TYPE,
                                )
                              ),
                            )
                          ),
                        )
                      ),
                    )
                  ),
                )
              ),
              "name" => "preferences_to_add",
            )
          ),
        ],
        "is_union" => false,
      )
    );
  }

  public static function getAllStructuredAnnotations()[write_props]: \TStructAnnotations {
    return shape(
      'struct' => dict[],
      'fields' => dict[
      ],
    );
  }

  public function getInstanceKey()[write_props]: string {
    return \TCompactSerializer::serialize($this);
  }

}
