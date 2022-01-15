/*
 * Whiteflag Java Library tests
 */
package org.whiteflagprotocol.java.crypto;

import org.junit.Test;
import static org.junit.Assert.*;

/**
 * Whiteflag cipher test class
 */
public class WfCipherTest {

    @Test
    /**
     * Tests Whiteflag encryption and decryption
     */
    public void testEncryption1() throws WfCryptoException {
        /* Setup */
        final String plaintext = "23000000000088888889111111119999999a22222222aaaaaaab33333333bbbbbbbb0983098309830983118b118b118b118b1993199319931993219b219b219b219b29a329a329a329a331ab31ab31ab31a9b1b9b1b9b1b9b1b9c1c9c1c9c1c9c1c8";
        final String ciphertext = "6d7658e7d17479677a0de95076989fcd7825b709349b143f2b17644e5cb2c8ded5c7f18d77447cf9dc2115e0c1c81d717b57fadaeedf27bfef8926448ff666d3d9a65168827c94b393974ebbe6b7f0599e184bfd1ace3569117c23ae17c5640f2f2d";
        WfEncryptionKey key = new WfEncryptionKey("32676187ba7badda85ea63a69870a7133909f1999774abb2eed251073616a6e7");
        WfCipher cipher  = new WfCipher(key);
        cipher.setContext("007a0baf6f84f0fa7402ea972686e56d50b707c9b67b108866");
        cipher.setInitVector("40aa85015d24e4601448c1ba8d7bf1aa");

        /* Verify */
        assertEquals("The cipher should correctly encrypt the plaintext", ciphertext, cipher.encrypt(plaintext));
        assertEquals("The cipher should correctly decrypt the ciphertext", plaintext, cipher.decrypt(ciphertext));
    }
}
