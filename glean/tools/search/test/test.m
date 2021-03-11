@interface AA {
  int _i;
}

- (int)meth1;
- (int)meth1: (int) x sel: (int) y;

@property (nonatomic, assign) int prop1;

@end

@implementation AA

@synthesize prop1 = _i;

- (int)meth1 { return 3; };
- (int)meth1: (int) x sel: (int) y { return x; };

@end

@interface AA (CC)
- (int)meth2: (int) x;
@end

@implementation AA (CC)

- (int)meth2: (int) x { return x; }

@end
