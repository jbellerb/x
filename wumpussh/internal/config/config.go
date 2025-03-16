package config

import (
	"fmt"
	"log/slog"
	"strconv"
	"time"

	"jae.zone/x/wumpussh/internal/cli"
)

type Config struct {
	LogLevel LogLevel
}

func (cfg *Config) Options() []cli.Option {
	return []cli.Option{
		{
			Value:   &cfg.LogLevel,
			Default: "info",
			EnvVars: []string{"WUMPUSSH_LOG"},
			Help:    "Logging verbosity level",
		},
	}
}

type ServeConfig struct {
	ShutdownGracePeriod time.Duration
	SocketTimeout       time.Duration
	ListenAddr          string
	HostPrivateEd25519  Ed25519SigningKey
}

func (cfg *ServeConfig) Options() []cli.Option {
	return []cli.Option{
		{
			Value:   (*cli.DurationValue)(&cfg.ShutdownGracePeriod),
			Default: "10s",
			EnvVars: []string{"WUMPUSSH_SHUTDOWN_GRACE_PERIOD"},
			Help:    "Time to wait for graceful shutdown before force closing",
		},
		{
			Value:   (*cli.DurationValue)(&cfg.SocketTimeout),
			Default: "10s",
			EnvVars: []string{"WUMPUSSH_SSH_SOCKET_TIMEOUT"},
			Help:    "Timeout for binding to a socket",
		},
		{
			Value:   (*cli.StringValue)(&cfg.ListenAddr),
			Default: ":22",
			EnvVars: []string{"WUMPUSSH_SSH_LISTEN_ADDR"},
			Flags:   []string{"l", "listen-addr"},
			Help:    "Address to listen for SSH connections on",
		},
		{
			Value:   &cfg.HostPrivateEd25519,
			EnvVars: []string{"WUMPUSSH_SSH_HOST_KEY_ED25519"},
			Help:    "Ed25519 private key to use as the host key for SSH sessions",
		},
	}
}

func (cfg *ServeConfig) Validate() error {
	if cfg.HostPrivateEd25519.Key == nil {
		return fmt.Errorf("no host private keys set")
	}

	return nil
}

type LogLevel struct {
	Level slog.Level
}

func (level *LogLevel) Set(s string) error {
	switch s {
	case "debug":
		level.Level = slog.LevelDebug
	case "info":
		level.Level = slog.LevelInfo
	case "warn":
		level.Level = slog.LevelWarn
	case "error":
		level.Level = slog.LevelError
	default:
		return fmt.Errorf("unknown log level: %s", s)
	}

	return nil
}

func (log *LogLevel) String() string {
	switch log.Level {
	case slog.LevelDebug:
		return "debug"
	case slog.LevelInfo:
		return "info"
	case slog.LevelWarn:
		return "warn"
	case slog.LevelError:
		return "error"
	default:
		return strconv.Itoa(int(log.Level))
	}
}
