@interface J
- (void)checkProtocol:(Protocol *)f;
@end

@protocol Foo
@end

void call(J *s)
{
  [s checkProtocol:@protocol(Foo)];
}
