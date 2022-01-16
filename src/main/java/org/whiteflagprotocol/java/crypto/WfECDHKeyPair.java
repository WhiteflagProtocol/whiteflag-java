/*
 * Whiteflag Java Library
 */
package org.whiteflagprotocol.java.crypto;

import java.math.BigInteger;
import java.security.Security;
import java.security.KeyFactory;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.GeneralSecurityException;
import java.security.interfaces.ECPrivateKey;
import java.security.interfaces.ECPublicKey;

import javax.crypto.KeyAgreement;

import org.bouncycastle.jce.provider.BouncyCastleProvider;
import org.bouncycastle.jce.ECNamedCurveTable;
import org.bouncycastle.jce.spec.ECNamedCurveParameterSpec;
import org.bouncycastle.jce.spec.ECPrivateKeySpec;
import org.bouncycastle.jce.spec.ECPublicKeySpec;
import org.bouncycastle.math.ec.ECCurve;
import org.bouncycastle.math.ec.ECPoint;

/**
 * Whiteflag ECDH Key Pair class
 *
 * <p> This class represents an Elleptic Curve Diffie-Hellmann key pair
 * used by Whiteflag for cryptographic key negotiation. The elliptic curve
 * parameters that must be used for Whiteflag are defined by the
 * brainpoolP256r1 curve as specified in RFC 5639. Public keys are shared
 * as raw 264-bit compressed public ECDH keys.
 * 
 * @wfver v1-draft.6
 * @wfref 5.2.2 Key Agreement
 * 
 * @since 1.1
 */
public class WfECDHKeyPair {

    /* STATIC CLAUSE */
    static {
        /* Load cryptographic provider */
        Security.addProvider(new BouncyCastleProvider());
    }

    /* PROPERTIES */

    /* Constants */
    public static final String CURVENAME = "brainpoolP256R1";
    private static final String ALGORITHM = "ECDH";
    private static final String PROVIDER = "BC";
    private static final int PUBKEYLENGTH = 33;

    /* ECDH Parameters */
    private static final ECNamedCurveParameterSpec ecParamSpec = ECNamedCurveTable.getParameterSpec(CURVENAME);
    private static final ECCurve curve = ecParamSpec.getCurve();

    /* Main key pair properties */
    private KeyPair keypair;

    /* CONSTRUCTOR */

    /**
     * Constructs a new Whiteflag ECDH key pair
     */
    public WfECDHKeyPair() throws GeneralSecurityException {
        this.keypair = createKeyPair();
    }

    /* PUBLIC METHODS */

    /**
     * Returns the public key of this key pair
     * @return the {@java.security.interfaces.ECPublicKey} object
     */
    public ECPublicKey getPublicKey() {
        return (ECPublicKey) keypair.getPublic();
    }

    /**
     * Returns the raw public key of the ECDH key pair
     * @return a byte array with the raw 264-bit compressed public ECDH key
     */
    public final byte[] getRawPublicKey() {
        return compressPublicKey(getPublicKey());
    }

    /**
     * Calculates the shared secret with an originator
     * @param rawPublicKey the originator's raw 264-bit compressed public ECDH key
     * @return a byte array with the shared secret
     * @throws GeneralSecurityException if the raw key or any of the parameters is invalid
     */
    public final byte[] getSharedKey(final byte[] rawPublicKey) throws GeneralSecurityException {
		KeyAgreement ka = KeyAgreement.getInstance(ALGORITHM, PROVIDER);
		ka.init(keypair.getPrivate());
		ka.doPhase(createPublicKey(rawPublicKey), true);
		return ka.generateSecret();
    }

    /* PUBLIC STATIUC METHODS */

    /**
     * Creates a new random ECDH key pair with the curve specified for Whiteflag key negotiation
     * @return a {@link java.security.KeyPair} object with the new key pair
     * @throws GeneralSecurityException
     */
    public static KeyPair createKeyPair() throws GeneralSecurityException {
        KeyPairGenerator kpg = KeyPairGenerator.getInstance(ALGORITHM, PROVIDER);
        kpg.initialize(ecParamSpec);
        return kpg.generateKeyPair();
    }

    /**
     * Creates an ECDH key pair from an existing private key with the curve specified for Whiteflag key negotiation
     * @param ecPrivateKey {@link java.security.ECPrivateKey} object with the private key
     * @return a {@link java.security.KeyPair} object with the new key pair
     * @throws GeneralSecurityException
     */
    public static KeyPair createKeyPair(ECPrivateKey ecPrivateKey) throws GeneralSecurityException {
        KeyFactory kf = KeyFactory.getInstance(ALGORITHM, PROVIDER);
        ECPoint point = ecParamSpec.getG().multiply(ecPrivateKey.getS());
        ECPublicKeySpec ecPubkeySpec = getPublicKeySpec(point.getEncoded(false));
        return new KeyPair((ECPublicKey) kf.generatePublic(ecPubkeySpec), ecPrivateKey);
    }

    /**
     * Compresses an ECDH public key
     * @param rawPublicKey
     * @return a byte array with the raw 264-bit compressed public ECDH key
     */
    public static byte[] compressPublicKey(ECPublicKey ecPublicKey) {
        // Get coordinates of public key
        final BigInteger y = ecPublicKey.getW().getAffineY();
        final BigInteger x = ecPublicKey.getW().getAffineX();

        // Set first byte of compressed key
        byte[] compressedPubkey = new byte[PUBKEYLENGTH];
        if (y.testBit(0)) compressedPubkey[0] = 0x03;   // y is odd
        else compressedPubkey[0] = 0x02;                // y is even

        // Copy x-coordinate and return compressed key
        byte[] xBytes = x.toByteArray();
        final int startByte = compressedPubkey.length - xBytes.length;
        System.arraycopy(xBytes, 0, compressedPubkey, startByte, xBytes.length);
        return compressedPubkey;
    }

    /**
     * Creates an ECDH public key object from a byte array
     * @param rawPublicKey a byte array with the raw 264-bit compressed public ECDH key
     * @return a {@link java.security.interface.ECPublicKey} object
     * @throws GeneralSecurityException if the raw key or any of the parameters is invalid
     */
	public static ECPublicKey createPublicKey(byte[] rawPublicKey) throws GeneralSecurityException {
        KeyFactory kf = KeyFactory.getInstance(ALGORITHM, PROVIDER);
        ECPublicKeySpec ecPubkeySpec = getPublicKeySpec(rawPublicKey);
        return (ECPublicKey) kf.generatePublic(ecPubkeySpec);
	}

    /**
     * Creates an ECDH private key object from a byte array
     * @param rawPrivateKey a byte array with the raw private ECDH key
     * @return a {@link java.security.interface.ECPrivateKey} object 
     * @throws GeneralSecurityException if the raw key or any of the parameters is invalid
     */
    public static ECPrivateKey createPrivateKey(byte[] rawPrivateKey) throws GeneralSecurityException {
		KeyFactory kf = KeyFactory.getInstance(ALGORITHM, PROVIDER);
		ECPrivateKeySpec ecPrivkeySpec = new ECPrivateKeySpec(new BigInteger(rawPrivateKey), ecParamSpec);
		return (ECPrivateKey) kf.generatePrivate(ecPrivkeySpec);
	}

    /* PRIVATE STATIC METHODS */

    /**
     * Calculates the point on the curve and returns public key specification
     * @param coordinates a byte array with the ASN.1 encoded coordinates
     * @return the {@linkl  } with the public key specification
     */
    public static ECPublicKeySpec getPublicKeySpec(byte[] coordinates) {
        ECPoint point = curve.decodePoint(coordinates);
        return new ECPublicKeySpec(point, ecParamSpec);
    }
}
