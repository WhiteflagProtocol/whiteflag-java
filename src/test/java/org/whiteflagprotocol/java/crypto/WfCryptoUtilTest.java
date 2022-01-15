/*
 * Whiteflag Java Library tests
 */
package org.whiteflagprotocol.java.crypto;

import org.junit.Test;
import static org.junit.Assert.*;

/**
 * Whiteflag crypto util test class
 *
 * <p> This test class uses RFC 5869 test vectors to test the
 * Whiteflag HKDF implementation.
 */
public class WfCryptoUtilTest {

    /**
     * Tests Hexadecimal String to Byte Array parser
     */
    @Test
    public void testConvertToByteArray1() {
        /* Setup */
        final String inputHexString = "000102030405060708090a0b0c";

        /* Verify */
        final byte[] byteArray = WfCryptoUtil.convertToByteArray(inputHexString);
        final String outputHexString = WfCryptoUtil.convertToHexString(byteArray);
        assertEquals("Input and output hexadecimal strings should be equal ", inputHexString, outputHexString);
    }

    /**
     * Tests Hexadecimal String to Byte Array parser
     */
    @Test
    public void testConvertToByteArray2() {
        /* Setup */
        final String inputHexString = "f0f1f2f3f4f5f6f7f8f9";

        /* Verify */
        final byte[] byteArray = WfCryptoUtil.convertToByteArray(inputHexString);
        assertEquals("Length of hexadecimal string and byte array should be equal", (inputHexString.length() /2), byteArray.length);

        final String outputHexString = WfCryptoUtil.convertToHexString(byteArray);
        assertEquals("Input and output hexadecimal strings should be equal ", inputHexString, outputHexString);
    }


    /**
     * Tests Hexadecimal String to Byte Array parser
     */
    @Test
    public void testZeroise1() {
        /* Setup */
        final byte[] byteArray = WfCryptoUtil.convertToByteArray("f0f1f2f3f4f5f6f7f8f9");
        final byte[] zero = new byte[byteArray.length];

        /* Verify */
        WfCryptoUtil.zeroise(byteArray);
        assertArrayEquals("Input and output hexadecimal strings should be equal ", zero, byteArray);
    }

    /**
     * Tests RFC 5869 A.1 Test Case 1
     */
    @Test
    public void testHKDF1() {
        /* Setup */
        final byte[] IKM = WfCryptoUtil.convertToByteArray("0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b");
        final byte[] SALT = WfCryptoUtil.convertToByteArray("000102030405060708090a0b0c");
        final byte[] INFO = WfCryptoUtil.convertToByteArray("f0f1f2f3f4f5f6f7f8f9");
        final byte[] PRK = WfCryptoUtil.convertToByteArray("077709362c2e32df0ddc3f0dc47bba6390b6c73bb50f9c3122ec844ad7c2b3e5");
        final byte[] OKM = WfCryptoUtil.convertToByteArray("3cb25f25faacd57a90434f64d0362f2a2d2d0a90cf1a5a4c5db02d56ecc4c5bf34007208d5b887185865");
        final int KEYLENGTH = 42;

        /* Verify */
        final byte[] prk = WfCryptoUtil.hkdfExtract(IKM, SALT);
        assertArrayEquals("Should pass RFC 5869 A.1 Test Case 1 Extract", PRK, prk);
        final byte[] okm = WfCryptoUtil.hkdfExpand(prk, INFO, KEYLENGTH);
        assertArrayEquals("Should pass RFC 5869 A.1 Test Case 1 Expand", OKM, okm);
    }

    /**
     * Tests RFC 5869 A.3 Test Case 2
     */
    @Test
    public void testHKDF2() {
        /* Setup */
        final byte[] IKM = WfCryptoUtil.convertToByteArray("000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f404142434445464748494a4b4c4d4e4f");
        final byte[] SALT = WfCryptoUtil.convertToByteArray("606162636465666768696a6b6c6d6e6f707172737475767778797a7b7c7d7e7f808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9fa0a1a2a3a4a5a6a7a8a9aaabacadaeaf");
        final byte[] INFO = WfCryptoUtil.convertToByteArray("b0b1b2b3b4b5b6b7b8b9babbbcbdbebfc0c1c2c3c4c5c6c7c8c9cacbcccdcecfd0d1d2d3d4d5d6d7d8d9dadbdcdddedfe0e1e2e3e4e5e6e7e8e9eaebecedeeeff0f1f2f3f4f5f6f7f8f9fafbfcfdfeff");
        final byte[] PRK = WfCryptoUtil.convertToByteArray("06a6b88c5853361a06104c9ceb35b45cef760014904671014a193f40c15fc244");
        final byte[] OKM = WfCryptoUtil.convertToByteArray("b11e398dc80327a1c8e7f78c596a49344f012eda2d4efad8a050cc4c19afa97c59045a99cac7827271cb41c65e590e09da3275600c2f09b8367793a9aca3db71cc30c58179ec3e87c14c01d5c1f3434f1d87");
        final int KEYLENGTH = 82;

        /* Verify */
        final byte[] prk = WfCryptoUtil.hkdfExtract(IKM, SALT);
        assertArrayEquals("Should pass RFC 5869 A.2 Test Case 2 Extract", PRK, prk);
        final byte[] okm = WfCryptoUtil.hkdfExpand(prk, INFO, KEYLENGTH);
        assertArrayEquals("Should pass RFC 5869 A.2 Test Case 2 Expand", OKM, okm);
    }
}
