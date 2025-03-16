package cli

import (
	"fmt"
	"io"
)

var (
	// Major.Minor.Patch of the current release. Set with linker flags.
	Version = "0.0.0"
	// Date of the current commit. Set with linker flags.
	CommitDate = "1970-01-01"
	// Shortened (7 character) hash of the current commit. Set with linker flags.
	CommitHash = "0000000"
)

// Function to write the current version to an output. Overwrite to customize.
var VersionWriter = func(w io.Writer) {
	fmt.Fprintf(w, "%s (built on %s from commit %s)", Version, CommitDate, CommitHash)
}
