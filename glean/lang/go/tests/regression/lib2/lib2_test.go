package lib1

import (
	"testing"
)

func TestFoo2(t *testing.T) {
	if Foo2() != 2 {
		t.Errorf("Foo2() != 2")
	}
}
