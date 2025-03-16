package server

import (
	"context"
	"encoding/binary"
	"fmt"
	"log/slog"

	"golang.org/x/crypto/ssh"
	"golang.org/x/term"
)

type Session struct {
	ch *ssh.Channel

	envTerm string
	pty     *term.Terminal
	winChan chan WindowChangeRequest
}

type WindowChangeRequest struct {
	Width  uint32
	Height uint32

	WidthPixels  uint32
	HeightPixels uint32
}

// Handle a single SSH session.
func (conn *Conn) handleSession(ctx context.Context, newCh ssh.NewChannel) {
	conn.wg.Add(1)
	defer conn.wg.Done()

	ch, reqs, err := newCh.Accept()
	if err != nil {
		slog.Error("Could not accept channel", "error", err)
		return
	}
	defer ch.Close()

	s := Session{ch: &ch}
	for {
		select {
		case <-ctx.Done():
			return
		case req := <-reqs:
			if req == nil {
				return
			}

			switch req.Type {
			case "pty-req":
				s.winChan = make(chan WindowChangeRequest, 1)
				if err := parsePtyReq(req.Payload, &s); err != nil {
					slog.Error("Failed to allocate new pty", "error", err)
					req.Reply(false, nil)
					continue
				}
				req.Reply(true, nil)
			case "shell":
				go s.Execute(ctx)
				req.Reply(true, nil)
			case "window-change":
				var w WindowChangeRequest
				if err := parseWindowChange(req.Payload, &w); err != nil {
					slog.Error("Failed to parse window change request", "error", err)
					req.Reply(false, nil)
					continue
				}
				if s.winChan != nil {
					s.winChan <- w
				}
				req.Reply(true, nil)
			default:
				slog.Debug("Unexpected session request", "type", req.Type)
				fallthrough
			// Known but unsupported requests are no-ops.
			case "agent-auth-req@openssh.com", "env", "signal", "x11-req":
				if req.WantReply {
					req.Reply(false, nil)
				}
			}
		}
	}
}

func (s *Session) Execute(ctx context.Context) error {
	if s.pty == nil {
		return fmt.Errorf("no pty allocated for session")
	}

	for {
		line, err := s.pty.ReadLine()
		if err != nil {
			return nil
		}
		fmt.Println(line)
	}
}

func parsePtyReq(in []byte, s *Session) error {
	envTerm, consumed, err := parseString(in)
	if err != nil {
		return err
	}

	var win WindowChangeRequest
	if err := parseWindowChange(in[consumed:], &win); err != nil {
		return err
	}

	// TODO: terminal modes

	s.envTerm = envTerm
	s.pty = term.NewTerminal(*s.ch, "> ")

	return nil
}

func parseString(in []byte) (str string, consumed int, err error) {
	var strLen uint32
	if _, err := binary.Decode(in, binary.BigEndian, &strLen); err != nil {
		return str, 0, nil
	}
	consumed = int(strLen) + 4

	if consumed > len(in) {
		return str, 0, fmt.Errorf("string was longer than provided payload")
	}

	str = string(in[4:consumed])
	return
}

func parseWindowChange(in []byte, w *WindowChangeRequest) error {
	_, err := binary.Decode(in, binary.BigEndian, w)

	return err
}
