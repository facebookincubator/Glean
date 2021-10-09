// Copyright (c) Facebook, Inc. and its affiliates.

/** Ensure this offset=0 comment is in the output
 *
 * @testname declarations/enum1
 * @type struct U
 */
struct U {
  /**
  * @testname declarations/enum1
  * @type enum InU
  */
  enum InU { UA, UB, UC };
  /**
  * @testname declarations/enum1
  * @type enum class InUClass
  */
  enum class InUClass { UCA, UCB, UCC };
};

/**
 * @testname declarations/enum1
 * @ype enum Global
 */
enum Global { GA, GB, GC };

/**
 * @testname declarations/enum1
 * @type enum class GlobalClass
 */
enum class GlobalClass { GCA, GCB, GCC };
