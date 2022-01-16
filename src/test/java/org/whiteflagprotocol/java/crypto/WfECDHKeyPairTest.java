/*
 * Whiteflag Java Library tests
 */
package org.whiteflagprotocol.java.crypto;

import org.junit.Test;
import static org.junit.Assert.*;

import java.security.GeneralSecurityException;

/**
 * Whiteflag authentication token test class
 */
public class WfECDHKeyPairTest {

    /**
     * Tests Hexadecimal String to Byte Array parser
     */
    @Test
    public void testCreateSharedSecret1() throws GeneralSecurityException {
        /* Setup */
        WfECDHKeyPair keypair1 = new WfECDHKeyPair();
        WfECDHKeyPair keypair2 = new WfECDHKeyPair();

        byte[] pubkey1 = keypair1.getRawPublicKey();
        System.out.println("Pubkey1: " + WfCryptoUtil.convertToHexString(pubkey1));

        byte[] pubkey2 = keypair2.getRawPublicKey();
        System.out.println("Pubkey2: " + WfCryptoUtil.convertToHexString(pubkey2));

        /* Verify */
        byte[] sharedSecret1 = keypair1.getSharedKey(pubkey2);
        byte[] sharedSecret2 = keypair2.getSharedKey(pubkey1);
        assertArrayEquals("Shared secrets should be indentical", sharedSecret1, sharedSecret2);
    }
}
