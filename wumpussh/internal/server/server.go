package server

import (
	"context"
	"log/slog"
	"net"
	"sync"

	"golang.org/x/crypto/ssh"
	"jae.zone/x/wumpussh/internal/config"
)

type Server struct {
	serverConfig *ssh.ServerConfig

	mu        sync.Mutex
	listeners map[*net.Listener]struct{}
	connGroup sync.WaitGroup
}

func NewSSHServer(cfg *config.ServeConfig) *Server {
	serverConfig := &ssh.ServerConfig{NoClientAuth: true}
	if cfg.HostPrivateEd25519.Key != nil {
		serverConfig.AddHostKey(cfg.HostPrivateEd25519.Key)
	}

	return &Server{
		serverConfig: serverConfig,
		listeners:    make(map[*net.Listener]struct{}),
	}
}

func Bind(ctx context.Context, addr string) (net.Listener, error) {
	if addr == "" {
		addr = ":22"
	}

	var lc net.ListenConfig
	l, err := lc.Listen(ctx, "tcp", addr)
	if err != nil {
		return nil, err
	}

	return l, nil
}

func (srv *Server) Serve(l net.Listener) error {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	srv.trackListener(&l, true)
	defer srv.trackListener(&l, false)

	for {
		// Add an open connection before accepting to avoid dropping a connection
		// in the case Shutdown() is called between Accept() and handleConn().
		srv.connGroup.Add(1)
		conn, err := l.Accept()
		if err != nil {
			srv.connGroup.Done()
			return err
		}

		slog.Debug("Accepted new SSH connection", "client", conn.RemoteAddr())
		go srv.handleConn(ctx, conn)
	}
}

func (srv *Server) trackListener(l *net.Listener, add bool) {
	srv.mu.Lock()
	defer srv.mu.Unlock()

	if add {
		srv.listeners[l] = struct{}{}
	} else {
		delete(srv.listeners, l)
	}
}

type Conn struct {
	conn *ssh.ServerConn
	wg   sync.WaitGroup
}

// Close the connection.
func (conn *Conn) Close() error {
	return conn.conn.Close()
}

// Wait for all sessions happening over the connection to end.
func (conn *Conn) Wait() {
	conn.wg.Wait()
}

func (srv *Server) handleConn(ctx context.Context, tcpConn net.Conn) {
	defer srv.connGroup.Done()

	newConn, chans, reqs, err := ssh.NewServerConn(tcpConn, srv.serverConfig)
	if err != nil {
		slog.Error("SSH handshake failed", "error", err)
		return
	}
	conn := &Conn{conn: newConn}
	defer conn.Wait()

	go conn.handleReqs(reqs)
	for {
		select {
		case <-ctx.Done():
			conn.Close()
			return
		case newCh := <-chans:
			if newCh == nil {
				return
			}

			if newCh.ChannelType() != "session" {
				newCh.Reject(ssh.UnknownChannelType, "unknown channel type")
				continue
			}
			conn.handleSession(ctx, newCh)
		}
	}
}

// Discard all global requests.
func (conn *Conn) handleReqs(in <-chan *ssh.Request) {
	conn.wg.Add(1)
	defer conn.wg.Done()

	ssh.DiscardRequests(in)
}

// Perform a graceful shutdown by first closing all listeners, and then waiting
// for all active connections to terminate.
func (srv *Server) Shutdown(ctx context.Context) error {
	srv.mu.Lock()
	for l := range srv.listeners {
		if cerr := (*l).Close(); cerr != nil {
			slog.Error("Failed to close listener", "listener", l, "error", cerr)
		}
		delete(srv.listeners, l)
	}
	srv.mu.Unlock()

	shutdown := make(chan struct{}, 1)
	go func() {
		srv.connGroup.Wait()
		shutdown <- struct{}{}
	}()

	select {
	case <-ctx.Done():
		return ctx.Err()
	case <-shutdown:
		return nil
	}
}
