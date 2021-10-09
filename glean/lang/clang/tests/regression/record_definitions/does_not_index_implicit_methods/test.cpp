// Copyright (c) Facebook, Inc. and its affiliates.

int calcSomething() {
    // The implicit destructor for this struct
    // should NOT show up in the generated facts.
    struct __attribute__((__packed__)) A {
        int a;

        // This function SHOULD show up in the generated
        // facts
        int blah() {
            return a;
        }
    };

    struct __attribute__((__packed__)) B {
        struct A a_;
    };

    struct B b = {};
    b.a_.a = 1;

    struct __attribute__((__packed__)) ExplictMethods {
        int c;

        ExplictMethods() {
            c = 1;
        }

        ~ExplictMethods() {
            c = 0;
        }
    };


    return sizeof(b);
}
