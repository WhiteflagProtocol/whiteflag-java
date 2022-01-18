/*
 * Whiteflag Java Library tests
 */
package org.whiteflagprotocol.java.crypto;

import org.junit.Test;
import static org.junit.Assert.*;

import java.security.GeneralSecurityException;
import java.security.interfaces.ECPublicKey;

/**
 * Whiteflag authentication token test class
 */
public class WfECDHKeyPairTest {

    /**
     * Tests ECDH key negotiation
     */
    @Test
    public void testNegotiateKey1() throws WfCryptoException, GeneralSecurityException {
        // Repeat to encouter different coordinate lengths
        for(int i = 0; i < 10; i++) {
            /* Setup */
            WfECDHKeyPair keypair1 = new WfECDHKeyPair();
            WfECDHKeyPair keypair2 = new WfECDHKeyPair();
            byte[] pubkey1 = keypair1.getRawPublicKey();
            byte[] pubkey2 = keypair2.getRawPublicKey();

            /* Verify */
            byte[] sharedSecret1 = keypair1.negotiateKey(pubkey2);
            byte[] sharedSecret2 = keypair2.negotiateKey(pubkey1);
            assertArrayEquals("Shared secrets should be indentical", sharedSecret1, sharedSecret2);
        }
    }
    /**
     * Tests ECDH key negotiation
     */
    @Test
    public void testNegotiateKey2() throws WfCryptoException, GeneralSecurityException {
        /* Setup */
        WfECDHKeyPair keypair1 = new WfECDHKeyPair();
        WfECDHKeyPair keypair2 = new WfECDHKeyPair();
        ECPublicKey pubkey1 = keypair1.getPublicKey();
        ECPublicKey pubkey2 = keypair2.getPublicKey();

        /* Verify */
        byte[] sharedSecret1 = keypair1.negotiateKey(pubkey2);
        byte[] sharedSecret2 = keypair2.negotiateKey(pubkey1);
        assertArrayEquals("Shared secrets should be indentical", sharedSecret1, sharedSecret2);
    }
}
