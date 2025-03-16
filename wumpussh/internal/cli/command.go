package cli

import (
	"context"
	"flag"
	"fmt"
	"os"
	"strings"
)

// A description of a configuration option.
type Option struct {
	// Pointer to the configuration value this option sets.
	Value flag.Value
	// Default value for the option.
	Default string
	// Help string to show on the usage page.
	Help string
	// Environment variables which set this option.
	EnvVars []string
	// Command line flags which set this option.
	Flags []string
	// Whether this option accepts an argument or not.
	BinaryArgument bool
}

// A configuration struct that documents its options.
type Config interface {
	// Returns the list of options for setting this config.
	Options() []Option
}

// A configuration struct that needs to be validated before use.
type ValidatedConfig interface {
	Config
	// Validates that the config is proper.
	Validate() error
}

// A command avaliable in the application.
type Command struct {
	// Name of the command.
	Name string
	// Quick usage line. Something like `executable [flags] <subcommand> [flags]`.
	Usage string
	// Brief description of the command.
	ShortInfo string
	// Longer description of the command.
	LongInfo string
	// A pointer to the configuration struct that should be parsed when running
	// this command or any of its children.
	Config Config
	// Subcommands below this command.
	Subcommands []*Command
	// Function to run if this command is called.
	Run func(ctx context.Context, args []string) error
}

var (
	cmdVersion = &Command{
		Name:      "version",
		ShortInfo: "Print the application version",
		LongInfo:  "Version prints the application version and what commit it was built from.",
		Run: func(ctx context.Context, args []string) error {
			VersionWriter(os.Stdout)
			return nil
		},
	}

	cmdHelp = &Command{
		Name:      "help",
		Usage:     "[command]",
		ShortInfo: "Provide help for any command",
		LongInfo:  "Help provides help for any command.",
	}
)

// Parse the options given by the user and then run either the given command or
// one of its subcommands.
func (cmd *Command) Execute(ctx context.Context) error {
	cmdHelp.Run = func(ctx context.Context, args []string) error {
		return helpCommand(args, cmd)
	}
	cmd.Subcommands = append(cmd.Subcommands, cmdVersion)
	cmd.Subcommands = append(cmd.Subcommands, cmdHelp)

	var err error
	args := os.Args[1:]
	parents := []string{}
	subcmd := cmd
	for {
		args, err = subcmd.parse(args, parents)
		if err != nil {
			if err == flag.ErrHelp {
				UsageWriter(os.Stdout, subcmd, parents)
				err = nil
			}
			return err
		}

		if len(args) > 0 {
			found, err := subcmd.findSubcommand(args[0])
			if err != nil {
				if subcmd.Run == nil {
					return unknownCommandError(subcmd, args[0], parents)
				}
			} else {
				args = args[1:]
				parents = append(parents, subcmd.Name)
				subcmd = found
				continue
			}
		}
		break
	}

	if subcmd.Run != nil {
		return subcmd.Run(ctx, args)
	} else {
		UsageWriter(os.Stdout, subcmd, parents)
		return nil
	}
}

func (cmd *Command) parse(args []string, parents []string) ([]string, error) {
	var b strings.Builder

	fs := flag.NewFlagSet("", flag.ContinueOnError)
	fs.SetOutput(&b)
	fs.Usage = func() {
		fmt.Fprint(fs.Output(), "usage: ")
		writeUsage(fs.Output(), cmd, parents)
	}

	if cmd.Config != nil {
		opts := cmd.Config.Options()
		for _, o := range opts {
			// Set it to the default...
			if o.Default != "" {
				o.Value.Set(o.Default)
			}

			// ...and then override with the environment variables...
			for _, k := range o.EnvVars {
				if v := os.Getenv(k); v != "" {
					o.Value.Set(v)
				}
			}

			// ...and then override with the flags.
			for _, f := range o.Flags {
				fs.Var(o.Value, f, "")
			}
		}
	}

	if err := fs.Parse(args); err != nil {
		if err != flag.ErrHelp {
			err = fmt.Errorf("%s", b.String())
		}
		return nil, err
	}

	// Validate the config if it implements `ValidatedConfig`.
	if cmd.Config != nil {
		if v, ok := cmd.Config.(ValidatedConfig); ok {
			if err := v.Validate(); err != nil {
				return nil, err
			}
		}
	}

	return fs.Args(), nil
}

func (cmd *Command) findSubcommand(name string) (*Command, error) {
	for _, subcmd := range cmd.Subcommands {
		if subcmd.Name == name {
			return subcmd, nil
		}
	}

	return nil, fmt.Errorf("unknown command \"%s\"", name)
}

func unknownCommandError(cmd *Command, subcmd string, parents []string) error {
	var b strings.Builder
	fmt.Fprintf(&b, "unknown command \"%s\" for \"", subcmd)
	for _, parent := range parents {
		fmt.Fprintf(&b, "%s ", parent)
	}
	fmt.Fprintf(&b, "%s\"", cmd.Name)
	if len(cmd.Subcommands) > 0 {
		fmt.Fprint(&b, "\n")
		writeSubcommands(&b, cmd)
	}
	return fmt.Errorf("%s", b.String())
}

func helpCommand(args []string, cmd *Command) error {
	var err error
	parents := []string{}
	subcmd := cmd
	for _, arg := range args {
		parents = append(parents, subcmd.Name)
		subcmd, err = subcmd.findSubcommand(arg)
		if err != nil {
			if !strings.HasPrefix(arg, "-") {
				return fmt.Errorf("unknown help topic: %s", arg)
			} else {
				trimmed := strings.TrimLeft(arg, "-")
				if trimmed != "h" && trimmed != "help" {
					return fmt.Errorf("flag provided but not defined: -%s", trimmed)
				}

				// Trying to match the behavior of flag's parser by allowing -h or --help
				// even after the command to lookup help for. Help is guaranteed to always
				// be the final subcommand.
				parents = []string{cmd.Name}
				subcmd = cmd.Subcommands[len(cmd.Subcommands)-1]
				break
			}
		}
	}

	UsageWriter(os.Stdout, subcmd, parents)
	return nil

}
