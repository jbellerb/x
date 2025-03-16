package cli

import (
	"fmt"
	"io"
	"strings"
	"text/tabwriter"
)

func UsageWriter(w io.Writer, cmd *Command, parents []string) {
	writeInfo(w, cmd)
	if len(cmd.Subcommands) > 0 {
		fmt.Fprint(w, "\n\nCommands:\n")
		writeSubcommands(w, cmd)
	}
	if cmd.Config != nil {
		opts := cmd.Config.Options()
		if hasEnvVars(opts) {
			fmt.Fprint(w, "\n\nEnvironment Variables:\n")
			writeEnvVars(w, opts)
		}
		if hasFlags(opts) {
			fmt.Fprint(w, "\n\nOptions:\n")
			writeFlags(w, opts)
		}
	}
	fmt.Fprint(w, "\n\nUsage:\n    ")
	writeUsage(w, cmd, parents)
	fmt.Fprint(w, "\n")
}

func writeInfo(w io.Writer, cmd *Command) {
	if cmd.LongInfo != "" {
		fmt.Fprintf(w, "%s", cmd.LongInfo)
	} else if cmd.ShortInfo != "" {
		fmt.Fprintf(w, "%s", cmd.ShortInfo)
	}
}

func writeSubcommands(w io.Writer, cmd *Command) {
	tw := tabwriter.NewWriter(w, 0, 0, 2, ' ', 0)
	for i, subcmd := range cmd.Subcommands {
		if i > 0 {
			fmt.Fprint(tw, "\n")
		}
		fmt.Fprintf(tw, "    %s\t%s", subcmd.Name, subcmd.ShortInfo)
	}
	tw.Flush()
}

func hasEnvVars(opts []Option) bool {
	for _, opt := range opts {
		if len(opt.EnvVars) > 0 {
			return true
		}
	}

	return false
}

func writeEnvVars(w io.Writer, opts []Option) {
	first := true
	for _, o := range opts {
		if len(o.EnvVars) > 0 {
			if !first {
				fmt.Fprint(w, "\n\n")
			}
			fmt.Fprintf(w, "    %s", strings.Join(o.EnvVars, ", "))
			if o.Help != "" {
				fmt.Fprintf(w, ":\n        %s", o.Help)
			}
			first = false
		}
	}
}

func hasFlags(opts []Option) bool {
	for _, opt := range opts {
		if len(opt.Flags) > 0 {
			return true
		}
	}

	return false
}

func writeFlags(w io.Writer, opts []Option) {
	first := true
	for _, o := range opts {
		if len(o.Flags) > 0 {
			if !first {
				fmt.Fprint(w, "\n\n")
			}
			fmt.Fprint(w, "    ")
			for i, flag := range o.Flags {
				if i > 0 {
					fmt.Fprint(w, ", ")
				}
				if len(flag) > 1 {
					fmt.Fprint(w, "-")
				}
				fmt.Fprintf(w, "-%s", flag)
			}

			name, usage := unquoteUsage(o.Help)
			if bf, ok := o.Value.(interface{ IsBoolFlag() bool }); !ok || !bf.IsBoolFlag() {
				if o.Default != "" {
					if err := o.Value.Set(o.Default); err != nil {
						panic(err)
					}
					if def := o.Value.String(); def != "" {
						name = def
					}
				}
				fmt.Fprintf(w, "=%s", name)
			}
			fmt.Fprintf(w, "\n        %s", usage)
			first = false
		}
	}
}

func unquoteUsage(str string) (name string, usage string) {
	usage = str
	if start := strings.IndexByte(usage, '`'); start != -1 {
		if end := strings.IndexByte(usage[start+1:], '`'); end != -1 {
			name = usage[start+1 : end]
			usage = usage[:start] + name + usage[end+1:]
			return name, usage
		}
	}

	// If the name was not found, return "...".
	name = "..."
	return name, usage
}

func writeUsage(w io.Writer, cmd *Command, parents []string) {
	for _, s := range parents {
		fmt.Fprintf(w, "%s ", s)
	}
	fmt.Fprintf(w, "%s", cmd.Name)
	if cmd.Usage != "" {
		fmt.Fprintf(w, " %s", cmd.Usage)
	}
}
