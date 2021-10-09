@interface I

+ (int)foo;

@end

int call(I *x)
{
  return I.foo;
}
