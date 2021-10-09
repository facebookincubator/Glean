@protocol Foo

- (void)setInt:(int)x setDouble:(double)y;

@end

void setFoo(id<Foo> foo) {
  [foo setInt: 1 setDouble: 2.5];
}

@interface Bar {
  id<Foo> foo;
}

@end

@implementation Bar
{
  id<Foo> foo;
}

- (void) fooBar
{
  [foo setInt: 2 setDouble: 3.5];
}

@end
