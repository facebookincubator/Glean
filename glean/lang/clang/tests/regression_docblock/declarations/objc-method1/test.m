/**
 * @testname declarations/objc-method1
 * @type interface I
 */
@interface I

/**
 * @testname declarations/objc-method1
 * @type f1
 */
- (int)f1;
/**
 * @testname declarations/objc-method1
 * @type f2:
 */
- (id)f2: (int) x;
/**
 * @testname declarations/objc-method1
 * @type f3a: f3b:
 */
- (id)f3a: (int) x f3b: (int) y;
/**
 * @testname declarations/objc-method1
 * @type f4a:
 */
- (id)f4a: (int) x : (int) y;

@end;
