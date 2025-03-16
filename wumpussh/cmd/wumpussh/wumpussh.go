package main

import (
	"context"
	"fmt"
	"log/slog"
	"os"
	"os/signal"
	"syscall"

	"jae.zone/x/wumpussh/internal/cli"
	"jae.zone/x/wumpussh/internal/config"
	"jae.zone/x/wumpussh/internal/server"
)

func main() {
	var cfg config.Config

	cmd := &cli.Command{
		Name:  "wumpussh",
		Usage: "[command]",
		LongInfo: `Find the Wumpus across a sprawling remote system.

  WumpuSSH is a "Hunt the Wumpus" inspired CTF challenge played over SSH.`,
		Config:      &cfg,
		Subcommands: []*cli.Command{NewCmdServe(&cfg)},
	}

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	go waitForGracefulShutdown(ctx, cancel)

	if err := cmd.Execute(ctx); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}

func NewCmdServe(rootCfg *config.Config) *cli.Command {
	var cfg config.ServeConfig

	return &cli.Command{
		Name:      "serve",
		Usage:     "[-l listen_addr]",
		ShortInfo: "Start the WumpuSSH server",
		LongInfo:  "Serve starts the WumpuSSH server on the configured listening address.",
		Config:    &cfg,

		Run: func(ctx context.Context, args []string) error {
			srv := server.NewSSHServer(&cfg)

			bindCtx, cancel := context.WithTimeout(ctx, cfg.SocketTimeout)
			defer cancel()
			l, err := server.Bind(bindCtx, cfg.ListenAddr)
			if err != nil {
				if bindCtx.Err() != context.Canceled {
					return fmt.Errorf("failed to bind to socket: %w", err)
				}
				return nil
			}

			initLogger(&rootCfg.LogLevel)

			slog.Info("Starting SSH server", "listen_addr", cfg.ListenAddr)
			server := make(chan error, 1)
			go func() { server <- srv.Serve(l) }()

			select {
			case <-ctx.Done():
				// Create a new context for the grace period. This is needed since the
				// parent context was closed when graceful shutdown started.
				ctx, cancel := context.WithTimeout(context.Background(), cfg.ShutdownGracePeriod)
				defer cancel()

				if err := srv.Shutdown(ctx); err != nil {
					slog.Warn("Shutdown did not complete in time, closing anyways", "timeout", cfg.ShutdownGracePeriod)
				}

				return nil
			case err := <-server:
				if err != nil {
					slog.Error("Socket unexpectedly closed", "error", err)
				}

				return fmt.Errorf("server unexpectedly stopped")
			}
		},
	}
}

func waitForGracefulShutdown(ctx context.Context, cancel context.CancelFunc) {
	done := make(chan os.Signal, 1)
	signal.Notify(done, os.Interrupt, syscall.SIGINT, syscall.SIGTERM)

	select {
	case sig := <-done:
		slog.Info("Starting graceful shutdown", "signal", sig)
		cancel()
	case <-ctx.Done():
		return
	}

	select {
	case sig := <-done:
		slog.Error("Aborting", "signal", sig)
		os.Exit(1)
	case <-ctx.Done():
		return
	}
}

func initLogger(level *config.LogLevel) {
	h := slog.NewJSONHandler(os.Stderr, &slog.HandlerOptions{Level: level.Level})
	slog.SetDefault(slog.New(h))
}
