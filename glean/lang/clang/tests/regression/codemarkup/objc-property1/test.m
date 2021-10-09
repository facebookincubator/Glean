@interface I

- (int)foo;
- (void)setFoo: (int) x;

@end;

int call(I *x)
{
  x.foo = 5;
  x.foo += 6;
  return x.foo;
}
