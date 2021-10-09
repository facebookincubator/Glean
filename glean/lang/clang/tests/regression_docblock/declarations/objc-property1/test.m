/**
 * @testname declarations/objc-property1
 * @type interface A
 */
@interface A {
  /**
   * @testname declarations/objc-property1
   * @type A::_i
   */
  int _i;
}

/**
 * @testname declarations/objc-property1
 * @type property i
 */
@property (nonatomic, assign) int i;

@end

/**
 * @testname declarations/objc-property1
 * @type implementation A
 */
@implementation A

@synthesize i = _i;

@end
