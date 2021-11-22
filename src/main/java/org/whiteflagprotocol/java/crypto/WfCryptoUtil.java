/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.crypto;

import java.nio.ByteBuffer;
import java.security.NoSuchAlgorithmException;
import java.security.InvalidKeyException;

import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;

/**
 * Whiteflag cryptographic utility class
 *
 * <p> This is a non-instantiatable utility class that performs
 * cryptographic support functions.
 */
public class WfCryptoUtil {

    /* PROPERTIES */

    /* Constants */
    public static final String HKDF_HASHALG ="HMACSHA256";
    public static final int HEXRADIX = 16;

    /* CONSTRUCTOR */

    /** 
     * Prevents the utility class to be instantiated
     */
    private WfCryptoUtil() {
        throw new IllegalStateException("Cannot instantiate Whiteflag HKDF utility class");
    }

    /* PUBLIC METHODS */

    /**
     * Parses a hexadecimal string to a byte array used in crypto functions
     * @param hexString the hexadecimal string
     * @return a byte array
     */
    public static final byte[] parseHexString(final String hexString) {
        final int length = hexString.length();
        byte[] byteArray = new byte[length / 2];
        for (int i = 0; i < length; i += 2) {
            byteArray[i / 2] = (byte) ((Character.digit(hexString.charAt(i), HEXRADIX) << 4)
                                      + Character.digit(hexString.charAt(i + 1), HEXRADIX));
        }
        return byteArray;
    }

    /**
     * Converts a byte array to a hexadecimal string
     * @param byteArray the byte array
     * @return a hexadecimal string
     */
    public static final String toHexString(final byte[] byteArray) {
        StringBuffer hexBuffer = new StringBuffer();
        for (int i = 0; i < byteArray.length; i++) {
            char[] hexDigits = new char[2];
            hexDigits[0] = Character.forDigit((byteArray[i] >> 4) & 0xF, HEXRADIX);
            hexDigits[1] = Character.forDigit((byteArray[i] & 0xF), HEXRADIX);
            hexBuffer.append(new String(hexDigits));
        }
        return hexBuffer.toString();
    }

    /**
     * Performs HKDF key and token derivation for Whiteflag
     * 
     * <p> The HKDF function as defined in RFC 5869 to derive the tokens and
     * encryption keys used for Whiteflag. This function performs the full
     * HKDF expand and extract.
     * 
     * @wfver v1-draft.6
     * @wfref 5.2.3 Key and Token Derivation
     * 
     * @param ikm the input key material
     * @param salt the cryptographic salt
     * @param info context and application specific information
     * @param keyLength the output key length in bytes
     * @return the output key material, i.e. the generated key
     */
    public static final byte[] hkdf(final byte[] ikm, final byte[] salt, final byte[] info, final int keyLength) {
        /*
         * Step 1. HKDF-Extract(salt, IKM) -> PRK
         * Step 2. HKDF-Expand(PRK, info, L) -> OKM
         */
        return hkdfExpand(hkdfExtract(salt, ikm), info, keyLength);
    };

    /**
     * Performs RFC 5869 HKDF Step 1: extract
     * @param ikm the input key material
     * @param salt the cryptographic salt
     * @return an intermediate pseudo random key
     */
    protected static final byte[] hkdfExtract(final byte[] ikm, final byte[] salt) {
        return getHMAC(salt).doFinal(ikm);
    }

    /**
     * Performs RFC 5869 HKDF Step 2: expand
     * @param prk the psudo random key
     * @param info context and application specific information
     * @param keyLength the output key length in bytes
     * @return the output key material
     */
    protected static final byte[] hkdfExpand(final byte[] prk, final byte[] info, final int keyLength) {
        // Prepare output
        ByteBuffer okm = ByteBuffer.allocate(keyLength);
        int remainder = keyLength;

        // Prepare hashing function
        Mac HMAC = getHMAC(prk);
        byte[] T = new byte[0];
        final int N = (int) Math.ceil((double) keyLength / (double) HMAC.getMacLength());

        // Interations to calculate okm
        for (int i = 1; i <= N; i++) {
            // Concatinate and hash previous hash T, info and counter i
            HMAC.update(T);
            HMAC.update(info);
            HMAC.update((byte) i);
            T = HMAC.doFinal();

            // Add hash to (remainder of) okm buffer
            final int length = Math.min(remainder, T.length);
            okm.put(T, 0, length);
            remainder -= length;
        }
        return okm.array();
    }

    /* PRIVATE METHODS */

    /**
     * Creates a HMAC object initialised with the provide key
     * @param key the key to initialize the HMAC object
     * @return an initialised HMAC object
     */
    private static final Mac getHMAC(byte[] key) {
        Mac HMAC;
        try {
            HMAC = Mac.getInstance(HKDF_HASHALG);
            HMAC.init(new SecretKeySpec(key, HKDF_HASHALG));
        } catch(NoSuchAlgorithmException e) {
            throw new IllegalArgumentException("Wrong hash algorithm in HKDF function: " + e.getMessage());
        } catch(InvalidKeyException e) {
            throw new IllegalArgumentException("Invalid keying material in HKDF function: " + e.getMessage());
        }
        return HMAC;
    }
}
