@interface A {
  int _i;
}

@property (nonatomic, assign) int i;

@end

@implementation A

@synthesize i = _i;

@end
