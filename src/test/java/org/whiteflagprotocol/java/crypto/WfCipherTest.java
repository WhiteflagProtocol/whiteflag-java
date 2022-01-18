/*
 * Whiteflag Java Library tests
 */
package org.whiteflagprotocol.java.crypto;

import org.junit.Test;
import static org.junit.Assert.*;

import java.security.GeneralSecurityException;
import java.security.NoSuchAlgorithmException;

import javax.security.auth.DestroyFailedException;

/* Static import of cryptographic utility functions */
import static org.whiteflagprotocol.java.crypto.WfCryptoUtil.convertToHexString;

/**
 * Whiteflag cipher test class
 */
public class WfCipherTest {

    /**
     * Tests Whiteflag encryption and decryption with pre-shared key and known test vector
     */
    @Test
    public void testCipher1() throws WfCryptoException {
        /* Setup */
        final String plaintext = "23000000000088888889111111119999999a22222222aaaaaaab33333333bbbbbbbb0983098309830983118b118b118b118b1993199319931993219b219b219b219b29a329a329a329a331ab31ab31ab31a9b1b9b1b9b1b9b1b9c1c9c1c9c1c9c1c8";
        final String ciphertext = "6d7658e7d17479677a0de95076989fcd7825b709349b143f2b17644e5cb2c8ded5c7f18d77447cf9dc2115e0c1c81d717b57fadaeedf27bfef8926448ff666d3d9a65168827c94b393974ebbe6b7f0599e184bfd1ace3569117c23ae17c5640f2f2d";
        WfEncryptionKey key = new WfEncryptionKey("32676187ba7badda85ea63a69870a7133909f1999774abb2eed251073616a6e7");
        WfCipher cipher  = WfCipher.fromKey(key);

        /* Verify */
        assertFalse("Cipher should not be ready without context and initialisation vector", cipher.isSet());
        cipher.setContext("007a0baf6f84f0fa7402ea972686e56d50b707c9b67b108866");
        assertFalse("Cipher should not be ready without initialisation vector", cipher.isSet());
        cipher.setInitVector("40aa85015d24e4601448c1ba8d7bf1aa");
        assertTrue("Cipher should be set up for encryption and decryption", cipher.isSet());
        assertEquals("The cipher should correctly encrypt the plaintext", ciphertext, cipher.encrypt(plaintext));
        assertEquals("The cipher should correctly decrypt the ciphertext", plaintext, cipher.decrypt(ciphertext));
    }

    /**
     * Tests full Whiteflag encryption scheme and decryption with negotiated key
     */
    @Test
    public void testCipher2() throws WfCryptoException, GeneralSecurityException, NoSuchAlgorithmException {
        /* Setup */
        final String context = "0011223344556677889900";

        final String plaintext1 = "aa1bb2cc3dd4ee5ff6007008009000";
        final WfECDHKeyPair keypair1 = new WfECDHKeyPair();
        final String pubkey1 = convertToHexString(keypair1.getRawPublicKey());

        final WfECDHKeyPair keypair2 = new WfECDHKeyPair();
        final String pubkey2 = convertToHexString(keypair2.getRawPublicKey());

        final WfEncryptionKey key1 = new WfEncryptionKey(pubkey2, keypair1);
        final WfCipher cipher1 = WfCipher.fromKey(key1);
        final String iv = convertToHexString(cipher1.setContext(context).setInitVector());
        final String ciphertext = cipher1.encrypt(plaintext1);

        final WfEncryptionKey key2 = new WfEncryptionKey(pubkey1, keypair2);
        final WfCipher cipher2 = WfCipher.fromKey(key2);
        final String plaintext2 = cipher2.setContext(context).setInitVector(iv).decrypt(ciphertext);

        /* Verify */
        assertEquals("The decoded ciphertext should match the original plaintext", plaintext1, plaintext2);
    }

    /**
     * Tests Whiteflag cipher w/ destroyed key
     */
    @Test
    public void testCipher3() throws WfCryptoException {
        /* Setup */
        WfEncryptionKey key = new WfEncryptionKey("32676187ba7badda85ea63a69870a7133909f1999774abb2eed251073616a6e7");
        key.destroy();

        /* Verify */
        assertTrue("Encryption key should indicate it is destroyed", key.isDestroyed());
        try {
            WfCipher cipher  = WfCipher.fromKey(key);
            fail("Expected a IllegalArgumentException to be thrown");
            assertFalse("Cipher should not be set", cipher.isSet());
        } catch (Exception e) {
            assertTrue(e instanceof IllegalArgumentException);
        }
    }

    /**
     * Tests Whiteflag encryption and decryption
     */
    @Test
    public void testCipher4() throws WfCryptoException {
        /* Setup */
        WfEncryptionKey key = new WfEncryptionKey("32676187ba7badda85ea63a69870a7133909f1999774abb2eed251073616a6e7");
        WfCipher cipher  = WfCipher.fromKey(key);

        /* Verify */
        assertFalse("Cipher should not be set", cipher.isSet());
        try {
            cipher.encrypt("a8f4b30");
            fail("Expected a IllegalStateException to be thrown");
        } catch (Exception e) {
            assertTrue(e instanceof IllegalStateException);
        }
    }

    /**
     * Tests Whiteflag encryption and decryption
     */
    @Test
    public void testCipher5() throws WfCryptoException, NoSuchAlgorithmException, DestroyFailedException {
        /* Setup */
        WfEncryptionKey key = new WfEncryptionKey("32676187ba7badda85ea63a69870a7133909f1999774abb2eed251073616a6e7");
        WfCipher cipher  = WfCipher.fromKey(key);
        cipher.setInitVector();
        cipher.destroy();

        /* Verify */
        assertTrue("Cipher should indicate it is destroyed", cipher.isDestroyed());
        assertFalse("Cipher should indicate it is not set", cipher.isSet());
        try {
            cipher.setContext("a010b020c030d040e050f060");
            cipher.encrypt("23000000000088888889111111119999999a22222222aaaaaaab33333333bbbbbbbb0983098309830983118b118b118b118b1993199319931993219b219b219b219b29a329a329a329a331ab31ab31ab31a9b1b9b1b9b1b9b1b9c1c9c1c9c1c9c1c8");
            fail("Expected a IllegalStateException to be thrown");
        } catch (Exception e) {
            assertTrue(e instanceof IllegalStateException);
        }
    }
}
