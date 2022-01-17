/*
 * Whiteflag Java Library tests
 */
package org.whiteflagprotocol.java;

import org.junit.Test;
import org.whiteflagprotocol.java.crypto.WfCryptoUtil;
import org.whiteflagprotocol.java.crypto.WfECDHKeyPair;

import static org.junit.Assert.*;

import java.security.GeneralSecurityException;

/**
 * Whiteflag state test class
 */
public class WfParticipantTest {
    /**
     * Tests for correctly constructed state object
     */
    @Test
    public void testParticipantECDH() throws WfException, GeneralSecurityException {
        /* Setup */
        final String name = "organisation";
        WfParticipant participant = new WfParticipant(name);
        WfECDHKeyPair ecdhKeypair = new WfECDHKeyPair();
        final String pubkeyHexstr = WfCryptoUtil.convertToHexString(ecdhKeypair.getRawPublicKey());
        participant.setEcdhPublicKey(pubkeyHexstr);

        /* Verify */
        assertEquals("Name should be correctly assigned", name, participant.getName());
        assertArrayEquals("ECDH Public Key should be correctly stored", ecdhKeypair.getPublicKey().getEncoded(), participant.getEcdhPublicKey().getEncoded());
    }
}
