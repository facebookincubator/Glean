package lib1

import (
	"testing"
)

func TestFoo1(t *testing.T) {
	if Foo1() != 1 {
		t.Errorf("Foo1() != 1")
	}
}
