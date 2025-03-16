package cli

import (
	"fmt"
	"time"
)

type StringValue string

func (s *StringValue) Get() any {
	return string(*s)
}

func (s *StringValue) Set(val string) error {
	*s = StringValue(val)
	return nil
}

func (s *StringValue) String() string {
	return fmt.Sprintf("\"%s\"", string(*s))
}

type DurationValue time.Duration

func (d *DurationValue) Get() any {
	return time.Duration(*d)
}

func (d *DurationValue) Set(s string) error {
	v, err := time.ParseDuration(s)
	*d = DurationValue(v)
	return err
}

func (d *DurationValue) String() string {
	return (*time.Duration)(d).String()
}
