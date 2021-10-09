@interface I

- (int)f1;
- (int)f2: (int) x;
- (int)f3a: (int) x f3b: (int) y;
- (int)f4a: (int) x : (int) y;

@end;

@implementation I

- (int)f1 {
  return 8;
}

- (int)f2: (int) x {
  return [self f1];
}

- (int)f3a: (int) x f3b: (int) y {
  return [self f2: x];
}

- (int)f4a: (int) x : (int) y {
  return [self f3a:x f3b:y];
}

@end;
