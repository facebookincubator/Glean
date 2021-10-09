@interface J
- (void)checkSelector:(SEL)selector;
@end

void bar(SEL selector)
{
  //no op
}

void call(J *s)
{
  // these should both get xrefs
  SEL f = @selector(foo);
  [s checkSelector:@selector(foo)];
}
