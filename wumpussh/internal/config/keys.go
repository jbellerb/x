package config

import (
	"fmt"

	"golang.org/x/crypto/ssh"
)

type Ed25519SigningKey struct {
	Key ssh.Signer
}

func (key *Ed25519SigningKey) Set(s string) error {
	private, err := ssh.ParsePrivateKey([]byte(s))
	if err != nil {
		return fmt.Errorf("parse Ed25519 private key: %w", err)
	}

	key.Key, err = ssh.NewSignerWithAlgorithms(private.(ssh.AlgorithmSigner), []string{ssh.KeyAlgoED25519})
	if err != nil {
		return fmt.Errorf("expected Ed25519 private key: %w", err)
	}

	return nil
}

func (key *Ed25519SigningKey) String() string {
	if key.Key != nil {
		return "[Redacted Ed25519 Private Key]"
	}

	return ""
}
