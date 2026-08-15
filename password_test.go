package charlang

// Tests for PBKDF2-HMAC-SM3 password hashing:
//  1. HMAC-SM3 checked against a hand-written RFC 2104 reference
//  2. PBKDF2 checked against github.com/golang/crypto/pbkdf2 (mature
//     independent implementation) across iteration counts, key lengths,
//     salts and Unicode passwords
//  3. hash/verify round trips, salt randomness, tamper detection,
//     format validation, and the script-level builtin functions

import (
	"encoding/base64"
	"bytes"
	"crypto/hmac"
	"math/rand"
	"strconv"
	"strings"
	"testing"

	xpbkdf2 "golang.org/x/crypto/pbkdf2"
)

// hmacSM3Ref is a direct RFC 2104 implementation used as an independent
// reference for crypto/hmac + NewSM3.
func hmacSM3Ref(key, msg []byte) []byte {
	const blockSize = 64
	if len(key) > blockSize {
		key = SM3Sum(key)
	}
	padded := make([]byte, blockSize)
	copy(padded, key)

	ipad := make([]byte, blockSize)
	opad := make([]byte, blockSize)
	for i := 0; i < blockSize; i++ {
		ipad[i] = padded[i] ^ 0x36
		opad[i] = padded[i] ^ 0x5c
	}

	inner := SM3Sum(append(append([]byte{}, ipad...), msg...))
	return SM3Sum(append(append([]byte{}, opad...), inner...))
}

func TestHMACSM3AgainstReference(t *testing.T) {
	rng := rand.New(rand.NewSource(11))
	longKey := make([]byte, 200) // exceeds block size, forces key hashing
	rng.Read(longKey)

	cases := []struct{ key, msg []byte }{
		{[]byte("key"), []byte("message")},
		{[]byte(""), []byte("message")},
		{[]byte("key"), []byte("")},
		{[]byte(""), []byte("")},
		{[]byte("你好密钥"), []byte("消息😀汉字")},
		{longKey, []byte("long key test")},
		{[]byte("k"), bytes.Repeat([]byte("x"), 1000)}, // multi-block message
	}

	for _, tc := range cases {
		h := hmac.New(NewSM3, tc.key)
		h.Write(tc.msg)
		got := h.Sum(nil)

		want := hmacSM3Ref(tc.key, tc.msg)
		if !bytes.Equal(got, want) {
			t.Fatalf("HMAC-SM3(key=%q, msg=%q) = %x, want %x", tc.key, tc.msg, got, want)
		}
	}
}

func TestPBKDF2HMACSM3AgainstXCrypto(t *testing.T) {
	rng := rand.New(rand.NewSource(33))
	salts := [][]byte{
		{},
		[]byte("salt"),
		make([]byte, 16),
		make([]byte, 64),
	}
	rng.Read(salts[2])
	rng.Read(salts[3])

	passwords := []string{"password", "", "p@ssw0rd!#", "密码123😀", strings.Repeat("长", 100)}

	for _, password := range passwords {
		for _, salt := range salts {
			for _, iter := range []int{1, 2, 3, 100, 1001} {
				for _, keyLen := range []int{1, 16, 32, 64, 65} {
					got, err := PBKDF2HMACSM3(password, salt, iter, keyLen)
					if err != nil {
						t.Fatalf("PBKDF2(%q, %x, %d, %d): %v", password, salt, iter, keyLen, err)
					}
					want := xpbkdf2.Key([]byte(password), salt, iter, keyLen, NewSM3)
					if !bytes.Equal(got, want) {
						t.Fatalf("PBKDF2(%q, %x, %d, %d) = %x, want %x",
							password, salt, iter, keyLen, got, want)
					}
				}
			}
		}
	}
}

func TestHashVerifyPasswordRoundTrip(t *testing.T) {
	passwords := []string{
		"correct horse battery staple",
		"",
		"p@ssw0rd!#",
		"超级密码😀한국어",
		strings.Repeat("a", 1000),
	}

	for _, pw := range passwords {
		encoded, err := HashPassword(pw, 100)
		if err != nil {
			t.Fatalf("HashPassword(%q): %v", pw, err)
		}

		if !strings.HasPrefix(encoded, "$pbkdf2-sm3$100$") {
			t.Fatalf("unexpected encoding %q", encoded)
		}

		ok, err := VerifyPassword(pw, encoded)
		if err != nil || !ok {
			t.Fatalf("VerifyPassword(correct %q) = %v, %v", pw, ok, err)
		}

		ok, err = VerifyPassword(pw+"x", encoded)
		if err != nil || ok {
			t.Fatalf("VerifyPassword(wrong %q) = %v, %v", pw, ok, err)
		}
	}
}

func TestHashPasswordFreshSalt(t *testing.T) {
	e1, err := HashPassword("same password", 10)
	if err != nil {
		t.Fatal(err)
	}
	e2, err := HashPassword("same password", 10)
	if err != nil {
		t.Fatal(err)
	}
	if e1 == e2 {
		t.Fatal("two hashes of the same password should differ (fresh salt)")
	}

	for _, e := range []string{e1, e2} {
		ok, err := VerifyPassword("same password", e)
		if err != nil || !ok {
			t.Fatalf("both encodings should verify: %v, %v", ok, err)
		}
	}
}

func TestVerifyPasswordTamperDetection(t *testing.T) {
	encoded, err := HashPassword("secret", 100)
	if err != nil {
		t.Fatal(err)
	}

	// flip one character inside the digest field
	digestStart := strings.LastIndex(encoded[:len(encoded)-1], "$")
	tampered := encoded[:digestStart+1] + "AAAA" + encoded[digestStart+5:]
	if ok, err := VerifyPassword("secret", tampered); err != nil || ok {
		t.Fatalf("tampered digest accepted: %v, %v", ok, err)
	}

	// same password, but a salt belonging to a different password must
	// not verify against this digest
	other, _ := HashPassword("different", 100)
	parts := strings.Split(encoded, "$")
	otherParts := strings.Split(other, "$")
	parts[3] = otherParts[3] // swap only the salt field
	mixed := strings.Join(parts, "$")
	if ok, err := VerifyPassword("secret", mixed); err != nil || ok {
		t.Fatalf("tampered salt accepted: %v, %v", ok, err)
	}

	// lower iteration count changes the derivation result
	lowerIters := strings.Replace(encoded, "$pbkdf2-sm3$100$", "$pbkdf2-sm3$99$", 1)
	if ok, err := VerifyPassword("secret", lowerIters); err != nil || ok {
		t.Fatalf("tampered iterations accepted: %v, %v", ok, err)
	}
}

func TestVerifyPasswordFormatErrors(t *testing.T) {
	bad := []string{
		"",
		"not-a-hash",
		"$pbkdf2-md5$100$c2FsdA$c2hhcg$",
		"$pbkdf2-sm3$abc$c2FsdA$c2hhcg$",  // non-numeric iterations
		"$pbkdf2-sm3$0$c2FsdA$c2hhcg$",    // zero iterations
		"$pbkdf2-sm3$100$!!notb64!!$c2hhcg$",
		"$pbkdf2-sm3$100$c2FsdA$!!notb64!!",
		"pbkdf2-sm3$100$c2FsdA$c2hhcg$",   // missing leading $
		"$pbkdf2-sm3$100$c2FsdA$c2hhcg",   // missing trailing $
		"$pbkdf2-sm3$100$c2FsdA$c2hhcg$$", // extra field
	}
	for _, s := range bad {
		if _, err := VerifyPassword("x", s); err == nil {
			t.Fatalf("VerifyPassword(%q) should fail with a format error", s)
		}
	}
}

// TestVerifyPasswordResourceLimits verifies that crafted records with
// excessive iteration counts or oversized digest fields are rejected
// before any expensive derivation work, while legitimately strong
// parameters still verify.
func TestVerifyPasswordResourceLimits(t *testing.T) {
	crafted := []string{
		// iteration count just above the verification limit
		"$pbkdf2-sm3$" + strconv.Itoa(pwHashMaxVerifyIterations+1) + "$c2FsdA$c2hhcg$",
		// astronomically large count (parses fine on 64-bit)
		"$pbkdf2-sm3$999999999$c2FsdA$c2hhcg$",
		// empty digest field
		"$pbkdf2-sm3$100$c2FsdA$$",
		// digest longer than the accepted bound
		"$pbkdf2-sm3$100$c2FsdA$" + base64.RawURLEncoding.EncodeToString(make([]byte, pwHashMaxDigestLen+1)) + "$",
	}
	for _, s := range crafted {
		if _, err := VerifyPassword("x", s); err == nil {
			t.Fatalf("VerifyPassword(%q) should be rejected by resource limits", s)
		}
	}

	// a strong but legitimate record must still verify
	encoded, err := HashPassword("strong params", 50000)
	if err != nil {
		t.Fatal(err)
	}
	ok, err := VerifyPassword("strong params", encoded)
	if err != nil || !ok {
		t.Fatalf("50000-iteration record should verify: %v, %v", ok, err)
	}
}

func TestHashPasswordDefaultIterations(t *testing.T) {
	// exercises the default path end to end
	encoded, err := HashPassword("default path", 0)
	if err != nil {
		t.Fatal(err)
	}
	if !strings.HasPrefix(encoded, "$pbkdf2-sm3$"+strconv.Itoa(pwHashDefaultIterations)+"$") {
		t.Fatalf("expected default iterations in %q", encoded)
	}

	ok, err := VerifyPassword("default path", encoded)
	if err != nil || !ok {
		t.Fatalf("default-iterations verify: %v, %v", ok, err)
	}
}

func TestBuiltinHashVerifyPassword(t *testing.T) {
	expectRun(t, `
		h := hashPassword("secret", 100)
		return verifyPassword("secret", h)
	`, nil, Bool(true))

	expectRun(t, `
		h := hashPassword("超级密码😀", 100)
		return verifyPassword("超级密码😀", h)
	`, nil, Bool(true))

	expectRun(t, `
		h := hashPassword("secret", 100)
		return verifyPassword("wrong", h)
	`, nil, Bool(false))

	// wrong parameter types / missing parameters surface as error objects
	expectRun(t, `return isError(hashPassword("secret", "abc"))`, nil, True)
	expectRun(t, `return isError(verifyPassword("secret"))`, nil, True)
}
