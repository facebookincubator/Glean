<?hh
/**
 * Test case generated manually using
 * buck2 run //thrift/compiler:thrift -- -out ~/fbsource/fbcode/glean/lang/codemarkup/tests/hack/glass/hack-xlang --gen hack ~/fbsource/fbcode/glean/lang/codemarkup/tests/hack/glass/hack-xlang/test.thrift
 *
 *  @generated
 *  @codegen-source: test.thrift
 */

/**
 * Original thrift service:-
 * TestService
 */
interface TestServiceAsyncIf extends \IThriftAsyncIf {
  /**
   * Original thrift definition:-
   * GetNavigationResponse
   *   thrift1(1: GetNavigationRequest request)
   *   throws (1: GetNavigationException get_navigation_exception);
   */
  public function thrift1(?GetNavigationRequest $request): Awaitable<GetNavigationResponse>;

  /**
   * Original thrift definition:-
   * GetCardResponse
   *   thrift2(1: GetCardRequest request)
   *   throws (1: GetCardException get_card_exception);
   */
  public function thrift2(?GetCardRequest $request): Awaitable<GetCardResponse>;
}

/**
 * Original thrift service:-
 * TestService
 */
interface TestServiceIf extends \IThriftSyncIf {
  /**
   * Original thrift definition:-
   * GetNavigationResponse
   *   thrift1(1: GetNavigationRequest request)
   *   throws (1: GetNavigationException get_navigation_exception);
   */
  public function thrift1(?GetNavigationRequest $request): GetNavigationResponse;

  /**
   * Original thrift definition:-
   * GetCardResponse
   *   thrift2(1: GetCardRequest request)
   *   throws (1: GetCardException get_card_exception);
   */
  public function thrift2(?GetCardRequest $request): GetCardResponse;
}

/**
 * Original thrift service:-
 * TestService
 */
interface TestServiceAsyncClientIf extends TestServiceAsyncIf {
}

/**
 * Original thrift service:-
 * TestService
 */
interface TestServiceClientIf extends \IThriftSyncIf {
  /**
   * Original thrift definition:-
   * GetNavigationResponse
   *   thrift1(1: GetNavigationRequest request)
   *   throws (1: GetNavigationException get_navigation_exception);
   */
  public function thrift1(?GetNavigationRequest $request): Awaitable<GetNavigationResponse>;

  /**
   * Original thrift definition:-
   * GetCardResponse
   *   thrift2(1: GetCardRequest request)
   *   throws (1: GetCardException get_card_exception);
   */
  public function thrift2(?GetCardRequest $request): Awaitable<GetCardResponse>;
}

/**
 * Original thrift service:-
 * TestService
 */
trait TestServiceClientBase {
  require extends \ThriftClientBase;

}

class TestServiceAsyncClient extends \ThriftClientBase implements TestServiceAsyncClientIf {
  use TestServiceClientBase;

  /**
   * Original thrift definition:-
   * GetNavigationResponse
   *   thrift1(1: GetNavigationRequest request)
   *   throws (1: GetNavigationException get_navigation_exception);
   */
  public async function thrift1(?GetNavigationRequest $request): Awaitable<GetNavigationResponse> {
    $hh_frame_metadata = $this->getHHFrameMetadata();
    if ($hh_frame_metadata !== null) {
      \HH\set_frame_metadata($hh_frame_metadata);
    }
    $rpc_options = $this->getAndResetOptions() ?? \ThriftClientBase::defaultOptions();
    $args = TestService_thrift1_args::fromShape(shape(
      'request' => $request,
    ));
    await $this->asyncHandler_->genBefore("TestService", "thrift1", $args);
    $currentseqid = $this->sendImplHelper($args, "thrift1", false);
    return await $this->genAwaitResponse(TestService_thrift1_result::class, "thrift1", false, $currentseqid, $rpc_options);
  }

  /**
   * Original thrift definition:-
   * GetCardResponse
   *   thrift2(1: GetCardRequest request)
   *   throws (1: GetCardException get_card_exception);
   */
  public async function thrift2(?GetCardRequest $request): Awaitable<GetCardResponse> {
    $hh_frame_metadata = $this->getHHFrameMetadata();
    if ($hh_frame_metadata !== null) {
      \HH\set_frame_metadata($hh_frame_metadata);
    }
    $rpc_options = $this->getAndResetOptions() ?? \ThriftClientBase::defaultOptions();
    $args = TestService_thrift2_args::fromShape(shape(
      'request' => $request,
    ));
    await $this->asyncHandler_->genBefore("TestService", "thrift2", $args);
    $currentseqid = $this->sendImplHelper($args, "thrift2", false);
    return await $this->genAwaitResponse(TestService_thrift2_result::class, "thrift2", false, $currentseqid, $rpc_options);
  }

}

class TestServiceClient extends \ThriftClientBase implements TestServiceClientIf {
  use TestServiceClientBase;

  /**
   * Original thrift definition:-
   * GetNavigationResponse
   *   thrift1(1: GetNavigationRequest request)
   *   throws (1: GetNavigationException get_navigation_exception);
   */
  public async function thrift1(?GetNavigationRequest $request): Awaitable<GetNavigationResponse> {
    $hh_frame_metadata = $this->getHHFrameMetadata();
    if ($hh_frame_metadata !== null) {
      \HH\set_frame_metadata($hh_frame_metadata);
    }
    $rpc_options = $this->getAndResetOptions() ?? \ThriftClientBase::defaultOptions();
    $args = TestService_thrift1_args::fromShape(shape(
      'request' => $request,
    ));
    await $this->asyncHandler_->genBefore("TestService", "thrift1", $args);
    $currentseqid = $this->sendImplHelper($args, "thrift1", false);
    return await $this->genAwaitResponse(TestService_thrift1_result::class, "thrift1", false, $currentseqid, $rpc_options);
  }

  /**
   * Original thrift definition:-
   * GetCardResponse
   *   thrift2(1: GetCardRequest request)
   *   throws (1: GetCardException get_card_exception);
   */
  public async function thrift2(?GetCardRequest $request): Awaitable<GetCardResponse> {
    $hh_frame_metadata = $this->getHHFrameMetadata();
    if ($hh_frame_metadata !== null) {
      \HH\set_frame_metadata($hh_frame_metadata);
    }
    $rpc_options = $this->getAndResetOptions() ?? \ThriftClientBase::defaultOptions();
    $args = TestService_thrift2_args::fromShape(shape(
      'request' => $request,
    ));
    await $this->asyncHandler_->genBefore("TestService", "thrift2", $args);
    $currentseqid = $this->sendImplHelper($args, "thrift2", false);
    return await $this->genAwaitResponse(TestService_thrift2_result::class, "thrift2", false, $currentseqid, $rpc_options);
  }

  /* send and recv functions */
  public function send_thrift1(?GetNavigationRequest $request): int {
    $args = TestService_thrift1_args::fromShape(shape(
      'request' => $request,
    ));
    return $this->sendImplHelper($args, "thrift1", false);
  }
  public function recv_thrift1(?int $expectedsequenceid = null): GetNavigationResponse {
    return $this->recvImplHelper(TestService_thrift1_result::class, "thrift1", false, $expectedsequenceid);
  }
  public function send_thrift2(?GetCardRequest $request): int {
    $args = TestService_thrift2_args::fromShape(shape(
      'request' => $request,
    ));
    return $this->sendImplHelper($args, "thrift2", false);
  }
  public function recv_thrift2(?int $expectedsequenceid = null): GetCardResponse {
    return $this->recvImplHelper(TestService_thrift2_result::class, "thrift2", false, $expectedsequenceid);
  }
}

// HELPER FUNCTIONS AND STRUCTURES

class TestService_thrift1_args implements \IThriftSyncStruct, \IThriftStructMetadata {
  use \ThriftSerializationTrait;

  const \ThriftStructTypes::TSpec SPEC = dict[
    1 => shape(
      'var' => 'request',
      'type' => \TType::STRUCT,
      'class' => GetNavigationRequest::class,
    ),
  ];
  const dict<string, int> FIELDMAP = dict[
    'request' => 1,
  ];

  const type TConstructorShape = shape(
    ?'request' => ?GetNavigationRequest,
  );

  const int STRUCTURAL_ID = 7168563228070420850;
  public ?GetNavigationRequest $request;

  public function __construct(?GetNavigationRequest $request = null)[] {
    $this->request = $request;
  }

  public static function withDefaultValues()[]: this {
    return new static();
  }

  public static function fromShape(self::TConstructorShape $shape)[]: this {
    return new static(
      Shapes::idx($shape, 'request'),
    );
  }

  public function getName()[]: string {
    return 'TestService_thrift1_args';
  }

  public static function getStructMetadata()[]: \tmeta_ThriftStruct {
    return tmeta_ThriftStruct::fromShape(
      shape(
        "name" => "test.thrift1_args",
        "fields" => vec[
          tmeta_ThriftField::fromShape(
            shape(
              "id" => 1,
              "type" => tmeta_ThriftType::fromShape(
                shape(
                  "t_struct" => tmeta_ThriftStructType::fromShape(
                    shape(
                      "name" => "test.GetNavigationRequest",
                    )
                  ),
                )
              ),
              "name" => "request",
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

class TestService_thrift1_result extends \ThriftSyncStructWithResult implements \IThriftStructMetadata {
  use \ThriftSerializationTrait;

  const type TResult = GetNavigationResponse;

  const \ThriftStructTypes::TSpec SPEC = dict[
    0 => shape(
      'var' => 'success',
      'type' => \TType::STRUCT,
      'class' => GetNavigationResponse::class,
    ),
    1 => shape(
      'var' => 'get_navigation_exception',
      'type' => \TType::STRUCT,
      'class' => GetNavigationException::class,
    ),
  ];
  const dict<string, int> FIELDMAP = dict[
    'success' => 0,
    'get_navigation_exception' => 1,
  ];

  const type TConstructorShape = shape(
    ?'success' => ?this::TResult,
    ?'get_navigation_exception' => ?GetNavigationException,
  );

  const int STRUCTURAL_ID = 975397503191165025;
  public ?this::TResult $success;
  public ?GetNavigationException $get_navigation_exception;

  public function __construct(?this::TResult $success = null, ?GetNavigationException $get_navigation_exception = null)[] {
    $this->success = $success;
    $this->get_navigation_exception = $get_navigation_exception;
  }

  public static function withDefaultValues()[]: this {
    return new static();
  }

  public static function fromShape(self::TConstructorShape $shape)[]: this {
    return new static(
      Shapes::idx($shape, 'success'),
      Shapes::idx($shape, 'get_navigation_exception'),
    );
  }

  public function getName()[]: string {
    return 'TestService_thrift1_result';
  }

  public static function getStructMetadata()[]: \tmeta_ThriftStruct {
    return tmeta_ThriftStruct::fromShape(
      shape(
        "name" => "test.TestService_thrift1_result",
        "fields" => vec[
          tmeta_ThriftField::fromShape(
            shape(
              "id" => 0,
              "type" => tmeta_ThriftType::fromShape(
                shape(
                  "t_struct" => tmeta_ThriftStructType::fromShape(
                    shape(
                      "name" => "test.GetNavigationResponse",
                    )
                  ),
                )
              ),
              "name" => "success",
            )
          ),
          tmeta_ThriftField::fromShape(
            shape(
              "id" => 1,
              "type" => tmeta_ThriftType::fromShape(
                shape(
                  "t_struct" => tmeta_ThriftStructType::fromShape(
                    shape(
                      "name" => "test.GetNavigationException",
                    )
                  ),
                )
              ),
              "name" => "get_navigation_exception",
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

  public function checkForException(): ?\TException {
    if ($this->get_navigation_exception !== null) {
      return $this->get_navigation_exception;
    }
    return null;
  }
}

class TestService_thrift2_args implements \IThriftSyncStruct, \IThriftStructMetadata {
  use \ThriftSerializationTrait;

  const \ThriftStructTypes::TSpec SPEC = dict[
    1 => shape(
      'var' => 'request',
      'type' => \TType::STRUCT,
      'class' => GetCardRequest::class,
    ),
  ];
  const dict<string, int> FIELDMAP = dict[
    'request' => 1,
  ];

  const type TConstructorShape = shape(
    ?'request' => ?GetCardRequest,
  );

  const int STRUCTURAL_ID = 383147069595638342;
  public ?GetCardRequest $request;

  public function __construct(?GetCardRequest $request = null)[] {
    $this->request = $request;
  }

  public static function withDefaultValues()[]: this {
    return new static();
  }

  public static function fromShape(self::TConstructorShape $shape)[]: this {
    return new static(
      Shapes::idx($shape, 'request'),
    );
  }

  public function getName()[]: string {
    return 'TestService_thrift2_args';
  }

  public static function getStructMetadata()[]: \tmeta_ThriftStruct {
    return tmeta_ThriftStruct::fromShape(
      shape(
        "name" => "test.thrift2_args",
        "fields" => vec[
          tmeta_ThriftField::fromShape(
            shape(
              "id" => 1,
              "type" => tmeta_ThriftType::fromShape(
                shape(
                  "t_struct" => tmeta_ThriftStructType::fromShape(
                    shape(
                      "name" => "test.GetCardRequest",
                    )
                  ),
                )
              ),
              "name" => "request",
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

class TestService_thrift2_result extends \ThriftSyncStructWithResult implements \IThriftStructMetadata {
  use \ThriftSerializationTrait;

  const type TResult = GetCardResponse;

  const \ThriftStructTypes::TSpec SPEC = dict[
    0 => shape(
      'var' => 'success',
      'type' => \TType::STRUCT,
      'class' => GetCardResponse::class,
    ),
    1 => shape(
      'var' => 'get_card_exception',
      'type' => \TType::STRUCT,
      'class' => GetCardException::class,
    ),
  ];
  const dict<string, int> FIELDMAP = dict[
    'success' => 0,
    'get_card_exception' => 1,
  ];

  const type TConstructorShape = shape(
    ?'success' => ?this::TResult,
    ?'get_card_exception' => ?GetCardException,
  );

  const int STRUCTURAL_ID = 8881834880055935439;
  public ?this::TResult $success;
  public ?GetCardException $get_card_exception;

  public function __construct(?this::TResult $success = null, ?GetCardException $get_card_exception = null)[] {
    $this->success = $success;
    $this->get_card_exception = $get_card_exception;
  }

  public static function withDefaultValues()[]: this {
    return new static();
  }

  public static function fromShape(self::TConstructorShape $shape)[]: this {
    return new static(
      Shapes::idx($shape, 'success'),
      Shapes::idx($shape, 'get_card_exception'),
    );
  }

  public function getName()[]: string {
    return 'TestService_thrift2_result';
  }

  public static function getStructMetadata()[]: \tmeta_ThriftStruct {
    return tmeta_ThriftStruct::fromShape(
      shape(
        "name" => "test.TestService_thrift2_result",
        "fields" => vec[
          tmeta_ThriftField::fromShape(
            shape(
              "id" => 0,
              "type" => tmeta_ThriftType::fromShape(
                shape(
                  "t_struct" => tmeta_ThriftStructType::fromShape(
                    shape(
                      "name" => "test.GetCardResponse",
                    )
                  ),
                )
              ),
              "name" => "success",
            )
          ),
          tmeta_ThriftField::fromShape(
            shape(
              "id" => 1,
              "type" => tmeta_ThriftType::fromShape(
                shape(
                  "t_struct" => tmeta_ThriftStructType::fromShape(
                    shape(
                      "name" => "test.GetCardException",
                    )
                  ),
                )
              ),
              "name" => "get_card_exception",
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

  public function checkForException(): ?\TException {
    if ($this->get_card_exception !== null) {
      return $this->get_card_exception;
    }
    return null;
  }
}

class TestServiceStaticMetadata implements \IThriftServiceStaticMetadata {
  public static function getServiceMetadata()[]: \tmeta_ThriftService {
    return tmeta_ThriftService::fromShape(
      shape(
        "name" => "test.TestService",
        "functions" => vec[
          tmeta_ThriftFunction::fromShape(
            shape(
              "name" => "thrift1",
              "return_type" => tmeta_ThriftType::fromShape(
                shape(
                  "t_struct" => tmeta_ThriftStructType::fromShape(
                    shape(
                      "name" => "test.GetNavigationResponse",
                    )
                  ),
                )
              ),
              "arguments" => vec[
                tmeta_ThriftField::fromShape(
                  shape(
                    "id" => 1,
                    "type" => tmeta_ThriftType::fromShape(
                      shape(
                        "t_struct" => tmeta_ThriftStructType::fromShape(
                          shape(
                            "name" => "test.GetNavigationRequest",
                          )
                        ),
                      )
                    ),
                    "name" => "request",
                  )
                ),
              ],
              "exceptions" => vec[
                tmeta_ThriftField::fromShape(
                  shape(
                    "id" => 1,
                    "type" => tmeta_ThriftType::fromShape(
                      shape(
                        "t_struct" => tmeta_ThriftStructType::fromShape(
                          shape(
                            "name" => "test.GetNavigationException",
                          )
                        ),
                      )
                    ),
                    "name" => "get_navigation_exception",
                  )
                ),
              ],
            )
          ),
          tmeta_ThriftFunction::fromShape(
            shape(
              "name" => "thrift2",
              "return_type" => tmeta_ThriftType::fromShape(
                shape(
                  "t_struct" => tmeta_ThriftStructType::fromShape(
                    shape(
                      "name" => "test.GetCardResponse",
                    )
                  ),
                )
              ),
              "arguments" => vec[
                tmeta_ThriftField::fromShape(
                  shape(
                    "id" => 1,
                    "type" => tmeta_ThriftType::fromShape(
                      shape(
                        "t_struct" => tmeta_ThriftStructType::fromShape(
                          shape(
                            "name" => "test.GetCardRequest",
                          )
                        ),
                      )
                    ),
                    "name" => "request",
                  )
                ),
              ],
              "exceptions" => vec[
                tmeta_ThriftField::fromShape(
                  shape(
                    "id" => 1,
                    "type" => tmeta_ThriftType::fromShape(
                      shape(
                        "t_struct" => tmeta_ThriftStructType::fromShape(
                          shape(
                            "name" => "test.GetCardException",
                          )
                        ),
                      )
                    ),
                    "name" => "get_card_exception",
                  )
                ),
              ],
            )
          ),
        ],
      )
    );
  }

  public static function getServiceMetadataResponse()[]: \tmeta_ThriftServiceMetadataResponse {
    return \tmeta_ThriftServiceMetadataResponse::fromShape(
      shape(
        'context' => \tmeta_ThriftServiceContext::fromShape(
          shape(
            'service_info' => self::getServiceMetadata(),
            'module' => \tmeta_ThriftModuleContext::fromShape(
              shape(
                'name' => 'test',
              )
            ),
          )
        ),
        'metadata' => \tmeta_ThriftMetadata::fromShape(
          shape(
            'enums' => dict[
            ],
            'structs' => dict[
              'test.GetNavigationResponse' => GetNavigationResponse::getStructMetadata(),
              'test.GetCardResponse' => GetCardResponse::getStructMetadata(),
              'test.GetNavigationRequest' => GetNavigationRequest::getStructMetadata(),
              'test.ResponseCard' => ResponseCard::getStructMetadata(),
              'test.GetCardRequest' => GetCardRequest::getStructMetadata(),
            ],
            'exceptions' => dict[
              'test.GetNavigationException' => GetNavigationException::getExceptionMetadata(),
              'test.GetCardException' => GetCardException::getExceptionMetadata(),
            ],
            'services' => dict[
            ],
          )
        ),
      )
    );
  }

  public static function getAllStructuredAnnotations()[write_props]: \TServiceAnnotations {
    return shape(
      'service' => dict[],
      'functions' => dict[
      ],
    );
  }
}
