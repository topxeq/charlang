package charlang

// Password hashing built on PBKDF2-HMAC-SM3: PBKDF2 key derivation per
// RFC 8018 with HMAC-SM3 (GB/T 32905-2016) as the pseudo-random function.
// The stored encoding is self-describing so the iteration count can be
// raised later without invalidating existing records:
//
//	$pbkdf2-sm3$<iterations>$<salt_b64url>$<dk_b64url>$

import (
	"crypto/pbkdf2"
	"crypto/rand"
	"crypto/subtle"
	"encoding/base64"
	"errors"
	"fmt"
	"strconv"
	"strings"
)

const (
	// pwHashAlgoName identifies the algorithm in the stored encoding.
	pwHashAlgoName = "pbkdf2-sm3"
	// pwHashDefaultIterations is the default PBKDF2 iteration count;
	// pass a higher iterations value to hashPassword for stronger
	// protection.
	pwHashDefaultIterations = 10000
	// pwHashSaltLen is the random salt length in bytes (128 bits).
	pwHashSaltLen = 16
	// pwHashKeyLen is the derived key length in bytes (256 bits).
	pwHashKeyLen = 32
	// pwHashFieldCount is the expected number of '$'-separated fields in
	// the stored encoding, including the two empty outer fields.
	pwHashFieldCount = 6
	// pwHashMaxVerifyIterations bounds the iteration count accepted
	// during verification, so a tampered or maliciously crafted record
	// cannot force a pathologically expensive derivation.
	pwHashMaxVerifyIterations = 10000000
	// pwHashMaxDigestLen bounds the digest length accepted during
	// verification, limiting amplification via oversized digest fields.
	pwHashMaxDigestLen = 256
)

// PBKDF2HMACSM3 derives keyLen bytes from password and salt using PBKDF2
// (RFC 8018) with HMAC-SM3 as the pseudo-random function.
func PBKDF2HMACSM3(password string, salt []byte, iterations, keyLen int) ([]byte, error) {
	return pbkdf2.Key(NewSM3, password, salt, iterations, keyLen)
}

// HashPassword hashes password with a fresh random salt and returns a
// self-describing PBKDF2-HMAC-SM3 encoding suitable for persistent
// storage. iterations <= 0 falls back to pwHashDefaultIterations.
func HashPassword(password string, iterations int) (string, error) {
	if iterations <= 0 {
		iterations = pwHashDefaultIterations
	}

	salt := make([]byte, pwHashSaltLen)
	if _, err := rand.Read(salt); err != nil {
		return "", fmt.Errorf("failed to generate salt: %w", err)
	}

	dk, err := PBKDF2HMACSM3(password, salt, iterations, pwHashKeyLen)
	if err != nil {
		return "", fmt.Errorf("failed to derive key: %w", err)
	}

	var sb strings.Builder
	sb.Grow(len(pwHashAlgoName) + 64)
	sb.WriteByte('$')
	sb.WriteString(pwHashAlgoName)
	sb.WriteByte('$')
	sb.WriteString(strconv.Itoa(iterations))
	sb.WriteByte('$')
	sb.WriteString(base64.RawURLEncoding.EncodeToString(salt))
	sb.WriteByte('$')
	sb.WriteString(base64.RawURLEncoding.EncodeToString(dk))
	sb.WriteByte('$')
	return sb.String(), nil
}

// VerifyPassword reports whether password matches an encoding produced
// by HashPassword. The digest comparison is constant-time.
func VerifyPassword(password, encoded string) (bool, error) {
	fields := strings.Split(encoded, "$")
	if len(fields) != pwHashFieldCount || fields[0] != "" || fields[len(fields)-1] != "" {
		return false, errors.New("invalid password hash format")
	}
	if fields[1] != pwHashAlgoName {
		return false, fmt.Errorf("unsupported password hash algorithm %q", fields[1])
	}

	iterations, err := strconv.Atoi(fields[2])
	if err != nil || iterations < 1 {
		return false, fmt.Errorf("invalid iteration count %q", fields[2])
	}
	if iterations > pwHashMaxVerifyIterations {
		return false, fmt.Errorf("iteration count %d exceeds limit %d", iterations, pwHashMaxVerifyIterations)
	}

	salt, err := base64.RawURLEncoding.DecodeString(fields[3])
	if err != nil {
		return false, fmt.Errorf("invalid salt encoding: %w", err)
	}

	want, err := base64.RawURLEncoding.DecodeString(fields[4])
	if err != nil {
		return false, fmt.Errorf("invalid hash encoding: %w", err)
	}
	if len(want) == 0 || len(want) > pwHashMaxDigestLen {
		return false, fmt.Errorf("invalid digest length %d", len(want))
	}

	got, err := PBKDF2HMACSM3(password, salt, iterations, len(want))
	if err != nil {
		return false, err
	}

	return subtle.ConstantTimeCompare(got, want) == 1, nil
}

// builtinHashPasswordFunc implements hashPassword(password [, iterations]).
// It returns a self-describing PBKDF2-HMAC-SM3 hash string with a fresh
// random salt; omit iterations to use the default (10000).
func builtinHashPasswordFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	password := args[0].String()

	iterations := 0
	if len(args) >= 2 {
		it, ok := args[1].(Int)
		if !ok {
			return NewCommonErrorWithPos(c, "iterations must be an integer"), nil
		}
		iterations = int(it)
	}

	rs, err := HashPassword(password, iterations)
	if err != nil {
		return NewCommonErrorWithPos(c, "%v", err), nil
	}

	return String(rs), nil
}

// builtinVerifyPasswordFunc implements verifyPassword(password, encoded).
// It returns true when password matches the stored encoding; the digest
// comparison is constant-time.
func builtinVerifyPasswordFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	ok, err := VerifyPassword(args[0].String(), args[1].String())
	if err != nil {
		return NewCommonErrorWithPos(c, "%v", err), nil
	}

	return Bool(ok), nil
}
