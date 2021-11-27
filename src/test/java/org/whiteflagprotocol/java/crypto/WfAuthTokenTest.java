/*
 * Whiteflag Java Library tests
 */
package org.whiteflagprotocol.java.crypto;

import org.junit.Test;
import static org.junit.Assert.*;

/* Whiteflag authentication methods */
import static org.whiteflagprotocol.java.crypto.WfAuthMethod.*;

/**
 * Whiteflag HKDF test class
 *
 * <p> This is a test class using RFC 5869 test vectors to test the
 * Whiteflag HKDF implementation.
 */
public class WfAuthTokenTest {

    @Test
    /**
     * Tests Hexadecimal String to Byte Array parser
     */
    public void testAuthToken1() {
        /* Setup */
        final String secret = "000102030405060708090a0b0c";
        final String context = "6fdb25dc394d5a437d88f15b459406ac6db8b386a49dbfc38c";
        final String verificationData = "a951cb35881ee7f78b05f8476a2193de4556455d48ffcfebcfc8938f4a37a70f";

        /* Verify */
        final WfAuthToken token = new WfAuthToken(secret);
        assertEquals("Authentication token should have the correct authentication indicator", TOKEN_PRESHARED.getIndicator(), token.authMethod.getIndicator());
        assertEquals("Authentication token should give the correct verification data", verificationData, token.getVerificationData(context));
    }
}
