@interface J {
  int _i;
}

@property (nonatomic, assign) int i;

@end

@implementation J

@synthesize i = _i;

- (void)testMethod
{
  _i = 1;
}

@end
